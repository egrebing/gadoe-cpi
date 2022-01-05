################################
# Analyze GA CPI Data - Update for 2022 CPI and FTE Data
# Last Modified: 1.3.2022
################################

#Update the calculation of percentiles to NOT include the charters
#Add the new CPI and FTE data

##### Set up packages, custom functions, and read in the raw data #####

library(tidyverse)
library(openxlsx)
library(data.table)
library(tictoc)
options(scipen = 999)

#Function to return the colors associated with a data; input data frame and the variable names
ReturnColorScale <- function(df, var = c("name", "value", "color_var"), 
                             colors = c("#005287", "#3e9ad4", "#f26922"), limits = c(-1.5, 1.5)) {
  input.df <- df %>%
    select(name = !!sym(var[1]), value = !!sym(var[2]), color_var = !!(var[3]))
  
  plot.data <- ggplot(input.df, aes(x = value, y = as.factor(name), color = color_var, fill = color_var, 
                                    color_val = color_var, name = name)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    scale_color_gradientn(colors = colors,
                          limits = limits) +
    scale_fill_gradientn(colors = rev(colors),
                         limits = limits) +
    theme_bw()
  
  output <- plot.data %>%
    ggplot_build() 
  
  as.data.frame(output$data) %>%
    select(name, value = colour_val, color_fwd = colour, color_rev = fill)
}

setwd("C:/Users/egrebing/Box/Grebing - Comp Center/Georgia/CPI Data - July 2021 Request/Data from GaDOE")

#Function to add ratio variables
AddRatioVariables <- function(df) {
  df %>%
    mutate(FTE_STUDENT_STAFF_RATIO = ifelse(FTE_POSITIONS == 0,
                                            NA,
                                            Student_FTE_Count / FTE_POSITIONS),
           FTE_RATIO_LABEL = ifelse(is.na(FTE_STUDENT_STAFF_RATIO), 
                                    "No Staff with Job Code",
                                    paste0("'", as.character(round(FTE_STUDENT_STAFF_RATIO, 0)), ":1")),
           Denom_Grades = gsub("STUDENT_COUNT_", "", Denom_Variable),
           Denom_Grades = gsub("_", "-", Denom_Grades),
           Denom_Grades = paste0("Grades ", Denom_Grades),
           STAFF_FTE_PER_1000 = FTE_POSITIONS*1000 / Student_FTE_Count,
           STAFF_FTE_PER_1000_LABEL = str_trim(paste(ifelse(STAFF_FTE_PER_1000 > 1, 
                                                            format(round(STAFF_FTE_PER_1000, 1), nsmall = 1), 
                                                            format(round(STAFF_FTE_PER_1000, 2), nsmall = 2)),
                                                     "staff per 1,000 students in grades", gsub("^'", "", Denom_Grades), sep = " ")))
}

#Load raw CPI data received from GADoE
cpi.17.21 <- read.xlsx("serve-cpi-data-output-2017-2021.xlsx") %>%
  mutate(ST_LEAID = paste0("GA-", SYSTEM_ID))

cpi.22 <- read.xlsx("serve-cpi-data-output-fy2022.xlsx") %>%
  mutate(ST_LEAID = paste0("GA-", SYSTEM_ID))

table(names(cpi.17.21)==names(cpi.22))

cpi <- bind_rows(cpi.17.21, cpi.22)

rm(cpi.17.21, cpi.22)

#Load the CCD LEA Universe Data for RC6
ccd.rc6 <- fread("../../../NCES CCD/CCD LEA Universe 2019-20/RC6 CCD 2019-20 LEAs.csv",
                    stringsAsFactors = F)

#Filter the CCD data to only GA LEAs
ccd.lea.ga <- ccd.rc6 %>%
  filter(MSTATE == "GA") 

#EMG 1.3.22 - Add GA-7830636 (new charter LEA)
ccd.lea.addition <- ccd.lea.ga %>%
  head(1) %>%
  mutate_all(~NA) %>%
  mutate(ST_LEAID = "GA-7830636")

ccd.lea.ga <- ccd.lea.ga %>%
  bind_rows(ccd.lea.addition) %>%
  distinct()

#Filter CPI data to only include LEAs in the CCD data; removes LEAs no longer in existence
cpi <- cpi %>%
  filter(ST_LEAID %in% ccd.lea.ga$ST_LEAID)

#Get GA locale codes and coordinates from the CCD data
locale.data.ga <- ccd.lea.ga %>%
  select(ST_LEAID, GEOID = LEAID, LOCALE, LAT, LON)

#Get the county codes for each GA LEA
ga.county.lookup <- ccd.lea.ga %>%
  select(ST_LEAID, NMCNTY, LEA_TYPE_TEXT)

#Load the self-created crosswalk of RESA to county
ga.resa.lookup <- read.xlsx("../CPI Files for PowerBI/GA County to RESA Crosswalk.xlsx")

ga.lea.resa <- ga.county.lookup %>%
  left_join(ga.resa.lookup, by = "NMCNTY") 

#ga.lea.resa.regular is only regular public school districts
ga.lea.resa.regular <- ga.lea.resa %>%
  filter(grepl("Regular", LEA_TYPE_TEXT))

rm(ga.county.lookup, ga.resa.lookup)

#Load information from data dictionary on the individual job code definitions
job.codes.single <- fread("../CPI Files for PowerBI/GA Job Codes Denominators Single.csv", stringsAsFactors = F) %>%
  mutate(JOB_CODE = as.character(JOB_CODE),
         JOB_CODE = ifelse(nchar(JOB_CODE) == 2, paste0("0", JOB_CODE), JOB_CODE)) %>%
  mutate(Title = str_trim(Title),
         CODE_TITLE_COMB = str_trim(CODE_TITLE_COMB)) %>%
  as.data.frame()

#Load self-created cluster job codings
job.codes.clustered <- fread("../CPI Files for PowerBI/GA Job Codes Lookup Clustered.csv", stringsAsFactors = F) %>%
  mutate(JOB_CODE = as.character(JOB_CODE),
         JOB_CODE = ifelse(nchar(JOB_CODE) == 2, paste0("0", JOB_CODE), JOB_CODE)) %>%
  mutate(Title = str_trim(Title),
         Cluster_Group = str_trim(Cluster_Group),
         JOB_CODE = str_trim(JOB_CODE)) %>%
  as.data.frame()

##### Create the Student FTE File #####
RepairFTEFile <- function(df) {
  names(df) <- df[3,]
  names(df) <- gsub("PK-12 Enrollment", "PK-12 Student Count", names(df))
  df[-c(1:3),]
}

#Load each year's student FTE file
fte.2017 <- read.xlsx("fte2017-1_enroll-system-state-grade.xlsx") %>%
  RepairFTEFile()

fte.2018 <- read.xlsx("fte2018-1_enroll-system-state-grade.xlsx") %>%
  RepairFTEFile()

fte.2019 <- read.xlsx("fte2019-1_enroll-system-state-grade.xlsx") %>%
  RepairFTEFile()

fte.2020 <- read.xlsx("fte2020-1_enroll-system-state-grade.xlsx") %>%
  RepairFTEFile()

fte.2021 <- read.xlsx("fte2021-1_enroll-system-state-grade.xlsx") %>%
  RepairFTEFile()

fte.2022 <- read.xlsx("fte2022-1_enroll-system-state-grade.xlsx") %>%
  RepairFTEFile()

#Combine all student FTE files from 2017-2021
fte.all <- bind_rows(fte.2017, fte.2018, fte.2019, fte.2020, fte.2021, fte.2022) %>%
  mutate(ST_LEAID = paste0("GA-", SYSTEM_ID)) %>%
  select(-SYSTEM_ID, -FISCAL_COUNT) %>%
  select(ST_LEAID, FISCAL_YEAR:SYSTEM_NAME, 
         STUDENT_COUNT_PK_12 = `PK-12 Student Count`, everything()) %>%
  mutate_at(vars(STUDENT_COUNT_PK_12:GR12), as.numeric) %>%
  mutate(STUDENT_COUNT_KK_05 = rowSums(across(KK:GR05)),
         STUDENT_COUNT_PK_05 = rowSums(across(PK:GR05)),
         STUDENT_COUNT_01_03 = rowSums(across(GR01:GR03)),
         STUDENT_COUNT_04_05 = rowSums(across(GR04:GR05)),
         STUDENT_COUNT_06_08 = rowSums(across(GR06:GR08)),
         STUDENT_COUNT_09_12 = rowSums(across(GR09:GR12)),
         STUDENT_COUNT_KK_12 = rowSums(across(KK:GR12)),
         STUDENT_COUNT_PK_08 = rowSums(across(PK:GR08)),
         STUDENT_COUNT_06_12 = rowSums(across(GR06:GR12))) %>%
  select(-SYSTEM_NAME)

str(fte.all)
rm(fte.2017, fte.2018, fte.2019, fte.2020, fte.2021, fte.2022)

#Create a long file with all student FTE data by year
student.fte.all.long <- fte.all %>%
  pivot_longer(cols = -(ST_LEAID:FISCAL_YEAR), 
               names_to = "Denom_Variable",
               values_to = "Student_FTE_Count")

setwd("C:/Users/egrebing/Box/Grebing - Comp Center/Georgia/CPI Data - July 2021 Request/CPI Files for PowerBI")

fwrite(fte.all, "Student FTE Data 2017-2022.csv", row.names = F, na = "")
fwrite(ccd.lea.ga, "GA 2019-20 CCD LEAs.csv", row.names = F, na = "")

##### Create LEA-Level Summary Files #####
names(fte.all)

#Create data frame with all needed student FTE denominators
fte.all.totals <- fte.all %>%
  select(ST_LEAID, FISCAL_YEAR, starts_with("STUDENT_COUNT"))

#Create data frame with the student FTE counts by RESA
fte.all.resa.long <- student.fte.all.long %>%
  left_join(ga.lea.resa, by = "ST_LEAID") %>%
  filter(!is.na(RESA_NAME)) %>%
  group_by(FISCAL_YEAR, RESA_NUM, RESA_NAME, Denom_Variable) %>%
  summarise(Student_FTE_Count = sum(Student_FTE_Count, na.rm = T))

#Determine the name used in the data from the most recent year to ensure all units have consistent
#names across the years.
system.names.latest.year <- cpi %>%
  group_by(ST_LEAID) %>%
  filter(FISCAL_YEAR == max(FISCAL_YEAR)) %>%
  ungroup() %>%
  select(ST_LEAID, SYSTEM_NAME) %>%
  distinct() %>%
  mutate(SYSTEM_NAME_SIMPLE = gsub("State Charter Schools- |State Charter Schools II- |State Schools- ", "", SYSTEM_NAME))

#Put all data together in a single file
cpi.out <- cpi %>%
  select(-SYSTEM_NAME) %>%
  left_join(system.names.latest.year, by = "ST_LEAID") %>%
  left_join(job.codes.single, by = c("JOB_CODE")) %>%
  left_join(fte.all.totals, by = c("ST_LEAID", "FISCAL_YEAR")) %>%
  left_join(ccd.lea.ga, by = "ST_LEAID")

#Get the total CPI head count (each year, job code, ST_LEAID); include zeros
cpi.head.ct <- cpi %>%
  select(FISCAL_YEAR, ST_LEAID, JOB_CODE, HEAD_CT) %>%
  group_by(FISCAL_YEAR, ST_LEAID, JOB_CODE) %>%
  summarise(HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = JOB_CODE, values_from = HEAD_CT,
              values_fill = list(HEAD_CT = 0)) %>%
  pivot_longer(cols = -(FISCAL_YEAR:ST_LEAID), names_to = "JOB_CODE", values_to = "HEAD_CT") %>%
  arrange(FISCAL_YEAR, ST_LEAID, JOB_CODE) 

#Check head count data
cpi.head.ct %>%
  count(ST_LEAID, FISCAL_YEAR) %>%
  pivot_wider(names_from = FISCAL_YEAR, values_from = n) %>%
  print(n = Inf)

#Reshape CPI to insert zeros for missing values and join head count values
cpi.fte <- cpi %>%
  select(FISCAL_YEAR, ST_LEAID, JOB_CODE, FTE_POSITIONS) %>%
  group_by(FISCAL_YEAR, ST_LEAID, JOB_CODE) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = JOB_CODE, values_from = FTE_POSITIONS,
              values_fill = list(FTE_POSITIONS = 0)) %>%
  pivot_longer(cols = -(FISCAL_YEAR:ST_LEAID), names_to = "JOB_CODE", values_to = "FTE_POSITIONS") %>%
  left_join(cpi.head.ct, by = c("FISCAL_YEAR", "ST_LEAID", "JOB_CODE")) %>%
  arrange(FISCAL_YEAR, ST_LEAID, JOB_CODE) 

#Check staff FTE data
cpi.fte %>%
  count(ST_LEAID, FISCAL_YEAR) %>%
  pivot_wider(names_from = FISCAL_YEAR, values_from = n) %>%
  print(n = Inf)

#Join job code descriptions and the student FTE counts to the CPI data
cpi.fte.joined <- cpi.fte %>%
  left_join(system.names.latest.year, by = "ST_LEAID") %>%
  left_join(ga.lea.resa, by = "ST_LEAID") %>%
  left_join(job.codes.single, by = "JOB_CODE") %>%
  left_join(student.fte.all.long, by = c("ST_LEAID", "FISCAL_YEAR", "Denom_Variable")) %>%
  AddRatioVariables() %>%
  left_join(locale.data.ga, by = "ST_LEAID")

#Write to file
fwrite(cpi.fte.joined, paste0("LEA-Level CPI Data.csv"),
          row.names = F, na = "")

##### Create Clustered Job Code Files #####

rm(ccd.lea.ga, ccd.rc6, cpi.head.ct, cpi.out)

#Create clusters of CPI data
cpi.clusters <- cpi.fte %>%
  left_join(job.codes.clustered, by = "JOB_CODE") %>%
  filter(Cluster_Group != "") %>%
  group_by(ST_LEAID, FISCAL_YEAR, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(student.fte.all.long, by = c("ST_LEAID", "FISCAL_YEAR", 
                                 "Denom_Variable" = "Denom_Variable")) %>%
  AddRatioVariables()

#Add additional clusters based on observed data trends
cpi.clusters.ii <- cpi.fte %>%
  left_join(job.codes.clustered, by = "JOB_CODE") %>%
  filter(Cluster_Group != "") %>%
  mutate(Cluster_Group = ifelse(grepl("K-5|Early Inter", Cluster_Group), 
                                   "Grade K-5 or Early Intervention Teacher",
                                   Cluster_Group),
         Cluster_Codes = ifelse(grepl("K-5 or Early", Cluster_Group),
                                "085, 100-105, 131-133",
                                Cluster_Codes),
         Cluster_Group = ifelse(grepl("Social|Psych|Nurse", Cluster_Group), 
                                "School Health (Nurse, Psychologist, Social Worker)",
                                Cluster_Group),
         Cluster_Codes = ifelse(grepl("Social|Psych|Nurse", Cluster_Group),
                                "404, 405, 407, 409",
                                Cluster_Codes)) %>%
  group_by(ST_LEAID, FISCAL_YEAR, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(student.fte.all.long, by = c("ST_LEAID", "FISCAL_YEAR", 
                                 "Denom_Variable" = "Denom_Variable")) %>%
  AddRatioVariables()

cpi.clusters.out <- cpi.clusters %>%
  bind_rows(cpi.clusters.ii) %>%
  distinct() %>%
  mutate(Combined_Title = paste(Cluster_Codes, Cluster_Group, sep = " - ")) %>%
  arrange(ST_LEAID, FISCAL_YEAR, Combined_Title) %>%
  left_join(system.names.latest.year, by = "ST_LEAID")

cpi.clusters.out %>%
  count(Cluster_Group, Cluster_Codes)

rm(cpi.clusters, cpi.clusters.ii)

#Write to File
fwrite(cpi.clusters.out, paste0("LEA-Level CPI Data Clustered Groups ", format(Sys.time(), "%b %d %Y"), ".csv"), row.names = F, na = "")

##### Create separate fact and dimension tables for Power BI #####
cpi.fact <- cpi.fte.joined %>%
  select(FISCAL_YEAR:HEAD_CT, GEOID, Denom_Variable, Denom_Grades, Student_FTE_Count) %>%
  mutate(Student_FTE_Count = ifelse(is.na(Student_FTE_Count), 0, Student_FTE_Count)) %>%
  distinct() 

cpi.cluster.fact <- cpi.clusters.out %>%
  select(ST_LEAID:Cluster_Group, FTE_POSITIONS:Student_FTE_Count, STAFF_FTE_PER_1000) %>%
  distinct() %>%
  mutate(Student_FTE_Count = ifelse(is.na(Student_FTE_Count), 0, Student_FTE_Count)) %>%
  filter(!is.na(STAFF_FTE_PER_1000)) %>%
  filter(STAFF_FTE_PER_1000 != Inf) %>%
  group_by(FISCAL_YEAR, Cluster_Group) %>%
  mutate(STAFF_FTE_Z_SCORE = scale(STAFF_FTE_PER_1000)) %>%
  ungroup()

#EMG 8.21 - Add color scale values calculated by custom function
cpi.cluster.colors <- cpi.cluster.fact %>%
  unite(name, ST_LEAID, FISCAL_YEAR, Cluster_Group) %>%
  select(name, value = STAFF_FTE_Z_SCORE, color_var = STAFF_FTE_Z_SCORE) %>%
  ReturnColorScale(colors = c("#9E3D22", "#FFFFFF", "#2B5C8A"), limits = c(-1, 1)) %>%
  separate(name, into = c("ST_LEAID", "FISCAL_YEAR", "Cluster_Group"), sep = "_") %>%
  mutate(color_fwd = gsub("grey50", "#EEEEEE", color_fwd),
         color_rev = gsub("grey50", "#EEEEEE", color_rev))

##### STATE-LEVEL AND RESA-LEVEL ANALYSIS #####

#Create a state-level summary file for staff and students
state.summary.staff <- cpi %>%
  left_join(job.codes.single[,grep("JOB_CODE|Denom", 
                                   names(job.codes.single))], by = "JOB_CODE") %>%
  group_by(FISCAL_YEAR, JOB_CODE, JOB_TITLE, Denom_Variable) %>%
  summarise(HEAD_CT = sum(HEAD_CT),
            FTE_POSITIONS = sum(FTE_POSITIONS))

state.summary.students <- student.fte.all.long %>%
  filter(ST_LEAID == "GA-999") %>%
  select(-ST_LEAID)

names(state.summary.staff)
names(state.summary.students)

state.summary.cpi <- state.summary.staff %>%
  left_join(state.summary.students, by = c("FISCAL_YEAR", "Denom_Variable")) %>%
  AddRatioVariables()

#Job Cluster Analysis
state.cluster.i <- state.summary.cpi %>%
  select(-Denom_Variable) %>%
  left_join(job.codes.clustered, by = "JOB_CODE") %>%
  filter(Cluster_Group != "") %>%
  mutate(Cluster_Group = ifelse(grepl("K-5|Early Inter", Cluster_Group), 
                                "Grade K-5 or Early Intervention Teacher",
                                Cluster_Group),
         Cluster_Codes = ifelse(grepl("K-5 or Early", Cluster_Group),
                                "085, 100-105, 131-133",
                                Cluster_Codes),
         Cluster_Group = ifelse(grepl("Social|Psych|Nurse", Cluster_Group), 
                                "School Health (Nurse, Psychologist, Social Worker)",
                                Cluster_Group),
         Cluster_Codes = ifelse(grepl("Social|Psych|Nurse", Cluster_Group),
                                "404, 405, 407, 409",
                                Cluster_Codes)) %>%
  group_by(FISCAL_YEAR, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(state.summary.students, by = c("FISCAL_YEAR", "Denom_Variable")) %>%
  AddRatioVariables()

state.cluster.ii <- state.summary.cpi %>%
  select(-Denom_Variable) %>%
  left_join(job.codes.clustered, by = "JOB_CODE") %>%
  filter(Cluster_Group != "") %>%
  group_by(FISCAL_YEAR, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(state.summary.students, by = c("FISCAL_YEAR", "Denom_Variable")) %>%
  AddRatioVariables()

state.cluster <- bind_rows(state.cluster.i, state.cluster.ii) %>%
  distinct() %>%
  arrange(Cluster_Codes, FISCAL_YEAR)

fwrite(state.cluster, "State-Level CPI Data 2017-2022.csv", row.names = F)

## Create RESA-Level Summary File ##
resa.summary.staff <- cpi %>%
  left_join(job.codes.single[,grep("JOB_CODE|Denom", 
                                   names(job.codes.single))], by = "JOB_CODE") %>%
  inner_join(ga.lea.resa, by = "ST_LEAID") %>%
  group_by(FISCAL_YEAR, RESA_NAME, RESA_NUM, JOB_CODE, JOB_TITLE, Denom_Variable) %>%
  summarise(HEAD_CT = sum(HEAD_CT),
            FTE_POSITIONS = sum(FTE_POSITIONS)) %>%
  ungroup()

resa.summary.students <- student.fte.all.long %>%
  inner_join(ga.lea.resa, by = "ST_LEAID") %>%
  group_by(FISCAL_YEAR, RESA_NAME, RESA_NUM, Denom_Variable) %>%
  summarise(Student_FTE_Count = sum(Student_FTE_Count)) %>%
  ungroup()

names(resa.summary.staff)
names(resa.summary.students)

resa.summary.cpi <- resa.summary.staff %>%
  left_join(resa.summary.students, by = c("RESA_NAME", "RESA_NUM",
                                          "FISCAL_YEAR", "Denom_Variable")) %>%
  AddRatioVariables()

#Job Cluster Analysis
resa.cluster.i <- resa.summary.cpi %>%
  select(-Denom_Variable) %>%
  left_join(job.codes.clustered, by = "JOB_CODE") %>%
  filter(Cluster_Group != "") %>%
  mutate(Cluster_Group = ifelse(grepl("K-5|Early Inter", Cluster_Group), 
                                "Grade K-5 or Early Intervention Teacher",
                                Cluster_Group),
         Cluster_Codes = ifelse(grepl("K-5 or Early", Cluster_Group),
                                "085, 100-105, 131-133",
                                Cluster_Codes),
         Cluster_Group = ifelse(grepl("Social|Psych|Nurse", Cluster_Group), 
                                "School Health (Nurse, Psychologist, Social Worker)",
                                Cluster_Group),
         Cluster_Codes = ifelse(grepl("Social|Psych|Nurse", Cluster_Group),
                                "404, 405, 407, 409",
                                Cluster_Codes)) %>%
  group_by(FISCAL_YEAR, RESA_NAME, RESA_NUM, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(resa.summary.students, by = c("FISCAL_YEAR", "Denom_Variable",
                                          "RESA_NAME", "RESA_NUM")) %>%
  AddRatioVariables()

resa.cluster.ii <- resa.summary.cpi %>%
  select(-Denom_Variable) %>%
  left_join(job.codes.clustered, by = "JOB_CODE") %>%
  filter(Cluster_Group != "") %>%
  group_by(FISCAL_YEAR, RESA_NAME, RESA_NUM, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(resa.summary.students, by = c("FISCAL_YEAR", "Denom_Variable",
                                          "RESA_NAME", "RESA_NUM")) %>%
  AddRatioVariables()

resa.cluster <- bind_rows(resa.cluster.i, resa.cluster.ii) %>%
  distinct() %>%
  arrange(Cluster_Codes, FISCAL_YEAR)

resa.cluster %>%
  count(RESA_NAME)

#Calculate SD for each cluster group from LEA-level data
head(cpi.cluster.fact)

cpi.cluster.mean.sd <- cpi.cluster.fact %>%
  group_by(FISCAL_YEAR, Cluster_Group) %>%
  summarise(STATE_STAFF_FTE_PER_1000_MEAN = mean(STAFF_FTE_PER_1000, na.rm = T),
            STATE_STAFF_FTE_PER_1000_SD = sd(STAFF_FTE_PER_1000, na.rm = T))

cpi.cluster.pctile <- cpi.cluster.fact %>%
  
  #EMG 12.10 - Only calculate percentiles for regular LEAs
  filter(ST_LEAID %in% ga.lea.resa.regular$ST_LEAID) %>%
  
  select(ST_LEAID, FISCAL_YEAR, Cluster_Group, STAFF_FTE_PER_1000) %>%
  group_by(FISCAL_YEAR, Cluster_Group) %>%
  mutate(STAFF_FTE_PER_1000_PCTILE = round((ecdf(STAFF_FTE_PER_1000)(STAFF_FTE_PER_1000)-0.0045)*100, 0),
         STAFF_FTE_PER_1000_PCTILE = ifelse(STAFF_FTE_PER_1000_PCTILE == 100, 99, STAFF_FTE_PER_1000_PCTILE),
         STAFF_FTE_PER_1000_PCTILE = ifelse(STAFF_FTE_PER_1000_PCTILE == 0, 1, STAFF_FTE_PER_1000_PCTILE)) %>%
  ungroup() %>%
  
  #EMG 10.29 - Change to 1st Percentile if there are no staff
  mutate(STAFF_FTE_PER_1000_PCTILE = ifelse(STAFF_FTE_PER_1000 == 0, 1, STAFF_FTE_PER_1000_PCTILE)) %>%
  
  mutate(PCTILE_FORMAT = case_when(
    STAFF_FTE_PER_1000_PCTILE %% 10 == 1 ~ paste0(STAFF_FTE_PER_1000_PCTILE, "st"),
    STAFF_FTE_PER_1000_PCTILE %% 10 == 2 ~ paste0(STAFF_FTE_PER_1000_PCTILE, "nd"),
    STAFF_FTE_PER_1000_PCTILE %% 10 == 3 ~ paste0(STAFF_FTE_PER_1000_PCTILE, "rd"),
    TRUE ~ paste0(STAFF_FTE_PER_1000_PCTILE, "th")),
    PCTILE_FORMAT = ifelse(STAFF_FTE_PER_1000_PCTILE >= 10 &
                             STAFF_FTE_PER_1000_PCTILE <= 19, 
                           gsub("st|nd|rd", "th", PCTILE_FORMAT),
                           PCTILE_FORMAT),
    PCTILE_FORMAT = paste0("(", PCTILE_FORMAT, " pctile)"),
    PCTILE_FORMAT = ifelse(STAFF_FTE_PER_1000 == 0, "", PCTILE_FORMAT)) %>%
  select(-STAFF_FTE_PER_1000) %>%
  arrange(ST_LEAID, Cluster_Group) %>%
  left_join(cpi.cluster.mean.sd, by = c("FISCAL_YEAR", "Cluster_Group"))

#Create State- and RESA-level tables to merge with the LEA CPI clusters
state.cluster.to.merge <- state.cluster %>%
  select(FISCAL_YEAR, Cluster_Group, 
         STATE_FTE_POSITIONS = FTE_POSITIONS,
         STATE_HEAD_CT = HEAD_CT,
         STATE_Student_FTE_Count = Student_FTE_Count,
         STATE_FTE_STUDENT_STAFF_RATIO = FTE_STUDENT_STAFF_RATIO,
         STATE_STAFF_FTE_PER_1000 = STAFF_FTE_PER_1000)

resa.cluster.to.merge <- resa.cluster %>%
  select(FISCAL_YEAR, RESA_NAME, Cluster_Group, 
         RESA_FTE_POSITIONS = FTE_POSITIONS,
         RESA_HEAD_CT = HEAD_CT,
         RESA_Student_FTE_Count = Student_FTE_Count,
         RESA_FTE_STUDENT_STAFF_RATIO = FTE_STUDENT_STAFF_RATIO,
         RESA_STAFF_FTE_PER_1000 = STAFF_FTE_PER_1000) %>%
  left_join(state.cluster.to.merge, by = c("FISCAL_YEAR", "Cluster_Group"))

fwrite(resa.cluster, "RESA-Level CPI Data 2017-2022.csv", row.names = F)

#EMG 12.7 - Add EDS and Title I measures
#Pull EDS from latest GOSA file
eds.pct <- fread("Enrollment_by_Subgroups_Programs_2021_Dec062021.csv", stringsAsFactors = F) %>%
  filter(DETAIL_LVL_DESC == "District") %>%
  mutate(ENRL_EDS_PCT_FY21 = ENROLL_PERCENT_ED / 100,
         ST_LEAID = paste0("GA-", SCHOOL_DSTRCT_CD)) %>%
  select(ST_LEAID, ENROLL_PERCENT_ED)

#Create the output cluster fact table that contains LEA, RESA, and State Level measures
cpi.cluster.fact.out <- cpi.cluster.fact %>%
  left_join(cpi.cluster.pctile, by = c("ST_LEAID", "FISCAL_YEAR", "Cluster_Group")) %>%
  left_join(ga.lea.resa, by = "ST_LEAID") %>%
  left_join(cpi.cluster.colors, by = c("ST_LEAID", "FISCAL_YEAR", "Cluster_Group")) %>%
  left_join(resa.cluster.to.merge, by = c("FISCAL_YEAR", "RESA_NAME", "Cluster_Group")) %>%
  mutate(LEA_FTE_STUDENT_STAFF_RATIO = round(1/(STAFF_FTE_PER_1000/1000), 1),
         LEA_RATIO_FORMAT = str_trim(paste0(format(round(LEA_FTE_STUDENT_STAFF_RATIO, 0), big.mark = ","), ":1")),
         RESA_RATIO_FORMAT = str_trim(paste0(format(round(RESA_FTE_STUDENT_STAFF_RATIO, 0), big.mark = ","), ":1")),
         STATE_RATIO_FORMAT = str_trim(paste0(format(round(STATE_FTE_STUDENT_STAFF_RATIO, 0), big.mark = ","), ":1")),
         LEA_RATIO_FORMAT = ifelse(LEA_RATIO_FORMAT == "Inf:1", "No Staff", LEA_RATIO_FORMAT))

#Calculate the median LEA clusters
state.median.lea.cluster <- cpi.cluster.fact.out %>%
  group_by(FISCAL_YEAR, Cluster_Group) %>%
  summarise(STATE_LEA_MEDIAN_RATIO = median(LEA_FTE_STUDENT_STAFF_RATIO)) %>%
  ungroup() %>%
  mutate(STATE_LEA_MEDIAN_RATIO_FORMAT = str_trim(paste0(format(round(STATE_LEA_MEDIAN_RATIO, 0), big.mark = ","), ":1")))

#Merge state median data with cpi.cluster.fact.out
cpi.cluster.fact.out <- cpi.cluster.fact.out %>%
  left_join(state.median.lea.cluster, by = c("FISCAL_YEAR", "Cluster_Group")) %>%
  mutate(LEA_TYPE_SHORT = case_when(
    grepl("charter", LEA_TYPE_TEXT) ~ "Charter",
    grepl("Regular", LEA_TYPE_TEXT) ~ "Regular LEA",
    grepl("State agency", LEA_TYPE_TEXT) ~ "State Agency",
    TRUE ~ as.character(LEA_TYPE_TEXT)
  )) %>%
  select(-LEA_TYPE_TEXT)

#Create state-level variables for visuals
cpi.cluster.state.summary.rows <- cpi.cluster.fact.out %>%
  filter(FISCAL_YEAR == 2022 & ST_LEAID == "GA-601") %>%
  mutate(ST_LEAID = "GA-00",
         RESA_FTE_POSITIONS = STATE_FTE_POSITIONS,
         RESA_HEAD_CT = STATE_HEAD_CT,
         RESA_Student_FTE_Count = STATE_Student_FTE_Count,
         RESA_FTE_STUDENT_STAFF_RATIO = STATE_FTE_STUDENT_STAFF_RATIO,
         RESA_STAFF_FTE_PER_1000 = STATE_STAFF_FTE_PER_1000,
         LEA_RATIO_FORMAT = STATE_LEA_MEDIAN_RATIO_FORMAT,
         RESA_RATIO_FORMAT = STATE_LEA_MEDIAN_RATIO_FORMAT) %>%
  mutate_at(vars(FTE_POSITIONS:color_rev), ~NA) %>%
  mutate(RESA_NAME = "All Georgia RESAs")

cpi.cluster.fact.out <- bind_rows(cpi.cluster.state.summary.rows, cpi.cluster.fact.out)
  
cpi.cluster.dim <- cpi.clusters.out %>%
  select(Cluster_Group:Denom_Variable, Denom_Grades) %>%
  distinct() %>%
  mutate(CLUSTER_CODE_COMB = paste0(Cluster_Group, " (", Cluster_Codes, ")"))

location.dim <- cpi.fte.joined %>%
  select(ST_LEAID, GEOID, SYSTEM_NAME:RESA_NAME, LOCALE:LON) %>%
  distinct() 

job.codes.dim <- job.codes.clustered %>%
  mutate(CODE_TITLE_COMB = paste0(JOB_CODE, " - ", Title))

#EMG 9.12.21: Add blank rows to the location dimension table (one for each RESA)
blank.location.rows <- location.dim %>%
  group_by(RESA_NAME) %>%
  sample_n(1) %>%
  ungroup() %>%
  mutate_at(vars(SYSTEM_NAME:NMCNTY, LOCALE:LON), ~NA) %>%
  mutate(SYSTEM_NAME = "(All LEAs)",
         SYSTEM_NAME_SIMPLE = "(All LEAs)",
         NMCNTY = "(All Counties)",
         GEOID = GEOID + 13, #Create a 'garbage' GEOID that doesn't match to any shape in the geographic file
         ST_LEAID = paste0("GA-0", rownames(.)))

blank.overall.row <- blank.location.rows %>%
  head(1) %>%
  mutate(GEOID = 1300000,
         ST_LEAID = "GA-00",
         RESA_NUM = NA,
         RESA_NAME = "All Georgia RESAs")

#Add rows to display "All RESAs" or "All LEAs" when no filters are applied
location.dim.update <- blank.overall.row %>%
  bind_rows(blank.location.rows, location.dim) %>%
  mutate(RESA_NAME_SHORT = gsub(" RESA", "", RESA_NAME),
         RESA_NAME_SHORT = gsub("Georgia", "GA", RESA_NAME_SHORT))

#Add column to the CPI dimension table to have a short cluster name
cpi.cluster.short.names <- fread("CPI Cluster Short Names.csv", stringsAsFactors = F) %>%
  select(CLUSTER_CODE_COMB, contains("Cluster_Group_Short"), Index)

cpi.cluster.dim.out <- cpi.cluster.dim %>%
  left_join(cpi.cluster.short.names, by = "CLUSTER_CODE_COMB") %>%
  arrange(Index)

#Create Output Files with All Years FY17-FY22
fwrite(location.dim.update, "CPI Location Dimension Table Incl Charters.csv", row.names = F, na = "")
fwrite(cpi.fact, "CPI Fact Table Incl Charters.csv", row.names = F, na = "")
fwrite(cpi.cluster.fact.out, "CPI Clustered Fact Table Incl Charters.csv", row.names = F, na = "")
fwrite(cpi.cluster.dim.out, "CPI Cluster Dimension Table Incl Charters.csv", row.names = F, na = "")
fwrite(job.codes.dim, "CPI Job Code Dimension Table Incl Charters.csv", row.names = F, na = "")

## Create a separate set of output fact tables with only FY22 data ##
cpi.fact.fy22 <- cpi.fact %>%
  filter(FISCAL_YEAR == "2022")

#Create a LEA index and RESA Index for sorting
resa.index <- cpi.cluster.fact.out %>%
  select(RESA_NAME) %>%
  distinct() %>%
  mutate(RESA_Index = seq(nrow(.)))

lea.index <- cpi.cluster.fact.out %>%
  select(ST_LEAID, RESA_NAME) %>%
  distinct() %>%
  mutate(LEA_Index = seq(nrow(.))) %>%
  left_join(resa.index, by = "RESA_NAME") %>%
  select(-RESA_NAME)

#Create a percent change data set
str(cpi.cluster.fact.out)

cpi.change <- cpi.cluster.fact.out %>%
  select(ST_LEAID, FISCAL_YEAR, Cluster_Group, FTE_POSITIONS, Student_FTE_Count) %>%
  filter(FISCAL_YEAR %in% c(2017, 2021, 2022)) %>% #EMG 1.3.22: Updated to include 2022
  arrange(ST_LEAID, Cluster_Group, FISCAL_YEAR) %>%
  pivot_longer(cols = FTE_POSITIONS:Student_FTE_Count) %>%
  unite(temp, name, FISCAL_YEAR) %>%
  pivot_wider(names_from = temp, values_from = value) 

#Get attributes from CCD - includes # of schools and statistical area
ga.sch.ccd <- fread("C:/Users/egrebing/Box/Grebing - Comp Center/NCES CCD/CCD School Universe 2019-20/RC6 CCD 2019-20 Schools.csv", stringsAsFactors = F) %>%
  mutate(ST_LEAID = substr(ST_SCHID, 1, 6)) %>%
  filter(grepl("GA", ST_SCHID)) %>%
  count(ST_LEAID, TITLEI_STATUS_TEXT) %>%
  mutate(TITLEI_STATUS_TEXT_COL = case_when(
    grepl("Not a", TITLEI_STATUS_TEXT) ~ "NOT_T1",
    grepl("schoolwide", TITLEI_STATUS_TEXT) ~ "T1_SCHOOLWIDE",
    grepl("targeted", TITLEI_STATUS_TEXT) ~ "T1_TGT_ASSIST",
    grepl("", TITLEI_STATUS_TEXT) ~ "T1_BLANK",
    TRUE ~ "T1_BLANK"
  )) %>%
  select(-TITLEI_STATUS_TEXT) %>%
  filter(TITLEI_STATUS_TEXT_COL != "T1_BLANK") %>%
  pivot_wider(names_from = TITLEI_STATUS_TEXT_COL, 
              values_from = n,
              values_fill = list(n = 0)) %>%
  arrange(ST_LEAID)

ga.lea.ccd <- fread("C:/Users/egrebing/Box/Grebing - Comp Center/NCES CCD/CCD LEA Universe 2019-20/RC6 CCD 2019-20 LEAs.csv", stringsAsFactors = F) %>%
  filter(MSTATE == "GA") %>%
  select(ST_LEAID, NMCBSA, NMCSA, OPERATIONAL_SCHOOLS) %>%
  left_join(ga.sch.ccd, by = "ST_LEAID") %>%
  rowwise() %>%
  mutate(PCT_TITLE_I_SCHOOLS = 1 - (NOT_T1 / (NOT_T1 + T1_SCHOOLWIDE + T1_TGT_ASSIST)),
         NUM_TITLE_I_SCHOOLS = T1_SCHOOLWIDE + T1_TGT_ASSIST)

cpi.cluster.fact.fy22 <- cpi.cluster.fact.out %>%
  filter(FISCAL_YEAR == "2022") %>%
  select(ST_LEAID, Cluster_Group:STAFF_FTE_PER_1000, STAFF_FTE_PER_1000_PCTILE:PCTILE_FORMAT,
         RESA_FTE_POSITIONS, RESA_Student_FTE_Count, RESA_STAFF_FTE_PER_1000,
         STATE_FTE_POSITIONS, STATE_STAFF_FTE_PER_1000, contains("RATIO_FORMAT")) %>%
  mutate(Row_Index = seq(nrow(.))) %>%
  left_join(lea.index, by = "ST_LEAID") %>%
  left_join(cpi.change, by = c("ST_LEAID", "Cluster_Group")) %>%
  left_join(eds.pct, by = "ST_LEAID") %>%
  left_join(ga.lea.ccd, by = "ST_LEAID")

cpi.cluster.fact.fy22 %>%
  count(RESA_Index, Cluster_Group, RESA_RATIO_FORMAT)

fwrite(cpi.fact.fy22, "CPI Fact Table Incl Charters FY22.csv", row.names = F, na = "")
fwrite(cpi.cluster.fact.fy22, "CPI Clustered Fact Table Incl Charters FY22.csv", row.names = F, na = "")
fwrite(cpi.change, "CPI Change Incl Charters FY17 to FY22.csv", row.names = F, na = "")

#####################
#Create summary data sets by RESA and for the whole state
resa.summary <- cpi.fte.joined %>%
  filter(!is.na(RESA_NAME)) %>%
  group_by(FISCAL_YEAR, RESA_NAME, RESA_NUM, JOB_CODE, Title, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T),
            MEAN_LEA_STAFF_FTE_PER_1000 = mean(STAFF_FTE_PER_1000, na.rm = T),
            MEDIAN_LEA_STAFF_FTE_PER_1000 = median(STAFF_FTE_PER_1000, na.rm = T),
            SD_LEA_STAFF_FTE_PER_1000 = sd(STAFF_FTE_PER_1000, na.rm = T),
            LEA_COUNT = n()) %>%
  ungroup() %>%
  arrange(FISCAL_YEAR, RESA_NUM, JOB_CODE) 

#Calculate summary student FTE
fte.resa.summary.long <- student.fte.all.long %>%
  left_join(ga.lea.resa, by = "ST_LEAID") %>%
  filter(!is.na(RESA_NAME)) %>%
  group_by(FISCAL_YEAR, Denom_Variable, RESA_NUM, RESA_NAME) %>%
  summarise(Student_FTE_Count = sum(Student_FTE_Count, na.rm = T)) %>%
  ungroup()

#Combine single position counts with the student FTE
resa.summary.single <- resa.summary %>%
  left_join(fte.resa.summary.long, by = c("FISCAL_YEAR", "Denom_Variable", "RESA_NUM", "RESA_NAME")) %>%
  AddRatioVariables() %>%
  group_by(FISCAL_YEAR, JOB_CODE) %>%
  mutate(Z_STAFF_FTE_PER_1000 = scale(STAFF_FTE_PER_1000)) %>%
  arrange(FISCAL_YEAR, JOB_CODE, FTE_STUDENT_STAFF_RATIO)
  
#Combine student FTE with the staff FTE data in clusters
resa.cluster.summary <- cpi.clusters.out %>%
  left_join(ga.lea.resa, by = "ST_LEAID") %>%
  filter(!is.na(RESA_NAME)) %>%
  group_by(FISCAL_YEAR, RESA_NAME, RESA_NUM, Cluster_Group, Cluster_Codes, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            HEAD_CT = sum(HEAD_CT, na.rm = T),
            LEA_COUNT = n()) %>%
  ungroup() %>%
  arrange(FISCAL_YEAR, RESA_NUM, Cluster_Group)

resa.summary.clusters <- resa.cluster.summary %>%
  filter(Cluster_Group != "") %>%
  group_by(RESA_NUM, RESA_NAME, FISCAL_YEAR, Cluster_Group, Denom_Variable) %>%
  summarise(FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T)) %>%
  ungroup() %>%
  left_join(fte.resa.summary.long, 
            by = c("FISCAL_YEAR", "Denom_Variable" = "Denom_Variable", "RESA_NUM", "RESA_NAME")) %>%
  AddRatioVariables() %>%
  arrange(FISCAL_YEAR, Cluster_Group, FTE_STUDENT_STAFF_RATIO) %>%
  mutate(Cluster_Year_Title = paste0(Cluster_Group, " Staff-Student Ratio in ", FISCAL_YEAR))

fwrite(resa.summary.clusters, paste0("CPI Data Summarized by Job Cluster and RESA ", format(Sys.time(), "%b %d %Y"), ".csv"),
          row.names = F, na = "")

#####
## Calculate the summary stats for the clusters and the RESA FTEs
state.fte.long <- fte.all.resa.long %>%
  group_by(FISCAL_YEAR, Denom_Variable) %>%
  summarise(State_Student_FTE_Count = sum(Student_FTE_Count, na.rm = T)) %>%
  ungroup()

state.cluster.summary.stats <- resa.cluster.summary %>%
  group_by(FISCAL_YEAR, Cluster_Group, Denom_Variable) %>%
  summarise(STATE_FTE_POSITIONS = sum(FTE_POSITIONS, na.rm = T),
            STATE_HEAD_CT = sum(HEAD_CT, na.rm = T)) %>%
  ungroup() %>%
  left_join(state.fte.long, by = c("FISCAL_YEAR", "Denom_Variable" = "Denom_Variable")) %>%
  mutate(STATE_FTE_STUDENT_STAFF_RATIO = ifelse(STATE_FTE_POSITIONS == 0,
                                          NA,
                                          State_Student_FTE_Count / STATE_FTE_POSITIONS),
         STATE_FTE_RATIO_LABEL = ifelse(is.na(STATE_FTE_STUDENT_STAFF_RATIO), 
                                  "No Staff with Job Code",
                                  paste0("'", as.character(round(STATE_FTE_STUDENT_STAFF_RATIO, 0)), ":1")))

resa.summary.clusters.out <- resa.summary.clusters %>%
  left_join(state.cluster.summary.stats, by = c("FISCAL_YEAR", "Cluster_Group", "Denom_Variable"))
