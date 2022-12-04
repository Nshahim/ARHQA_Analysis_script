### Data Analysis
# Load required packages -----------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(writexl)) install.packages("writexl")
if(!require(glue)) install.packages("glue")
source("R/Analysis_double_disagg.R") # custom function for anlaysis
`%notin%` <- Negate(`%in%`)

# Read data ------------------------------------------------------------------------------
convert_to_na <- c("NA", "N/A", "-", " ", 9999, 7777, 8888)
data_path <- "input/raw_data/" # data path
# file.edit("R/read_data.R")
source("R/read_data.R")

# Re-code Data -------------------------------------------------------------------------------------
# file.edit("R/recode.R")
source("R/recode.R")

# Read analysis plans ------------------------------------------------------------------------------
a1_t1_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_A1_T1.xlsx")
a1_t2_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_A1_T2.xlsx")
a1_t3_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_A1_T3.xlsx")
a1_t4_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_A1_T4.xlsx")
a2_t1_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_A2_T1.xlsx")
a2_t2_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_A2_T2.xlsx")


# Analysis -----------------------------------------------------------------------------------------
## A1_T1
a1_t1_analysis <- rbind(
  analysis_func(df = a1_t1_Human_resource, 
                ap = filter(a1_t1_ap, sheet=="HF_Human_Resources"), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t1_staff_info, 
                ap = filter(a1_t1_ap, sheet=="Staff_Info"), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t1_staff_notin_jawza, 
                ap = filter(a1_t1_ap, sheet=="Staff_not_in_the_jawza1400_r..."), 
                multi_response_sep = " & ")
)
a1_t1_analysis <- a1_t1_analysis %>% 
  filter(Denominator != 0 & Disaggregation_level != 0)


## A1_T2 
a1_t2_analysis <- rbind(
  analysis_func(df = a1_t2_infra, 
                ap = filter(a1_t2_ap, sheet=="Infrastructure_Group"), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t2_medicine, 
                ap = filter(a1_t2_ap, sheet=="Medicine"), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t2_common_med, 
                ap = filter(a1_t2_ap, sheet=="Common_Medicine_Group"), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t2_medicine_equip, 
                ap = filter(a1_t2_ap, sheet=="Medicine_Or_Equipment_Infras..."), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t2_equipment, 
                ap = filter(a1_t2_ap, sheet=="Equipment"), 
                multi_response_sep = " & ")
)
a1_t2_analysis <- a1_t2_analysis %>% 
  filter(Denominator != 0) %>% 
  filter(!(Question == "Was_there_a_shortage_of_this_medicine_since_August_15_2021" & 
             Response %in% c("I don't know", "No"))) # Get only Health Facilities that had shortage

## A1_T3
a1_t3_analysis <- rbind(
  analysis_func(df = a1_t3_data, 
                ap = filter(a1_t3_ap, sheet=="data"), 
                multi_response_sep = " & "),
  analysis_func(df = a1_t3_salary, 
                ap = filter(a1_t3_ap, sheet=="Salary"),
                multi_response_sep = " & ")
)
a1_t3_analysis <- a1_t3_analysis %>% 
  filter(Denominator != 0) %>% unique() %>%
  group_by(Disaggregation_level, Question, repeat_for_label) %>% 
  mutate(Result = case_when(
    !is.na(repeat_for) ~ round((Result/sum(Result))*100, 1)
  ),
  Denominator = case_when(
    !is.na(repeat_for) ~ sum(Result),
    TRUE ~ as.numeric(Denominator)
  ), .after=Count) %>% ungroup()

## A1_T4
a1_t4_analysis <- analysis_func(df = a1_t4_data, ap = a1_t4_ap, multi_response_sep = " & ") %>% 
  arrange(Disaggregation)
a1_t4_analysis <- a1_t4_analysis %>% 
  filter(Denominator != 0)

## A2_T1
a2_t1_analysis <- rbind(
  analysis_func(df = a2_t1_rep_service, 
                ap = filter(a2_t1_ap, sheet=="rep_service"), 
                multi_response_sep = " & "),
  analysis_func(df = a2_t1_hmis_service, 
                ap = filter(a2_t1_ap, sheet=="number_of_services_HMIS"), 
                multi_response_sep = " & "),
  analysis_func(df = a2_t1_health_service, 
                ap = filter(a2_t1_ap, sheet=="number_of_services_Health_Re..."), 
                multi_response_sep = " & "))
# Calculating percentage for double disaggregations
a2_t1_analysis <- a2_t1_analysis %>% 
  filter(Denominator != 0) %>% unique() %>%
  group_by(Disaggregation_level, Question, repeat_for_label) %>% 
  mutate(Percentage = case_when(
    !is.na(repeat_for) ~ round((Result/sum(Result))*100, 1)
  ),
  Denominator = case_when(
    !is.na(repeat_for) ~ sum(Result),
    TRUE ~ as.numeric(Denominator)
  ), .after=Count) %>% ungroup()

## A2_T2
a2_t2_analysis <- analysis_func(df = a2_t2_data, ap = a2_t2_ap, multi_response_sep = " & ") %>% 
  arrange(Disaggregation)
a1_t3_analysis <- a1_t3_analysis %>% 
  filter(Denominator != 0)

# Manual Analysis ----------------------------------------------------------------------------------
# file.edit("R/manual_calculations.R")
source("R/manual_calculations.R")

# Export lists -------------------------------------------------------------------------------------
overview_list <- list(
  unique_service_providers=unique_service_providers,
  unique_health_facilty_type=unique_health_facilty_type,
  unique_health_facilty_type_SP=unique_health_facilty_type_SP,
  type_gender_of_respondents=type_gender_of_respondents,
  resp_type_merged=resp_type_merged,
  surveyor_gender=surveyor_gender,
  surveyor_enum_gender=surveyor_enum_gender,
  number_of_Site_Visits=number_of_Site_Visits,
  respondents_and_surveyors=respondents_and_surveyors
)

a1_t1_list <- list(
  a1_t1_analysis=a1_t1_analysis,
  HF_meet_recommended_staffing=HF_meeting_recommended_staffing,
  HF_meet_recommended_staffing_SP=HF_meeting_recommended_staffing_SP
)
a1_t2_list <- list(
  a1_t2_analysis=a1_t2_analysis,
  medicine_tables=medicine_tables,
  available_equipments_SP=available_equipments_SP,
  available_equipments_HF=available_equipments_HF,
  available_equipments_Prov=available_equipments_Prov
)
a1_t3_list <- list(
  a1_t3_analysis=a1_t3_analysis,
  salary_change_SP=average_salary_change_SP,
  salary_change_HF=average_salary_change_HF,
  salary_change_prov=average_salary_change_prov
)

a2_t2_list <- list(
  Overview=overview_of_patient_respondent,
  person_interviewed_patient_SP=person_interviewed_patient_SP,
  person_interviewed_patient_HF=person_interviewed_patient_HF,
  person_interviewed_patient_prov=person_interviewed_patient_prov,
  a2_t2_analysis=a2_t2_analysis
)


# export results -------------------------------------------------------------------------
if (!file.exists(glue::glue("output/analysis"))) {
  dir.create(glue::glue("output/analysis"), showWarnings = TRUE, recursive = TRUE)
  cat("created 'output/analysis' folder")
} else {
  cat("The 'output/analysis' folder already exists")
}

# Long
writexl::write_xlsx(overview_list, "output/analysis/RHQA_Overview_Analysis.xlsx", format_headers = F) # Overview
writexl::write_xlsx(HMIS_Verification_list, "output/analysis/RHQA_Verification_Analysis.xlsx", format_headers = F) # Verification
writexl::write_xlsx(a1_t1_list, "output/analysis/a1_t1_analysis.xlsx", format_headers = F) # A1_T1
writexl::write_xlsx(a1_t2_list, "output/analysis/a1_t2_analysis.xlsx", format_headers = F) # A1_T2
writexl::write_xlsx(a1_t3_list, "output/analysis/a1_t3_analysis.xlsx", format_headers = F) # A1_T3
writexl::write_xlsx(a1_t4_analysis, "output/analysis/a1_t4_analysis.xlsx", format_headers = F) # A1_T4
writexl::write_xlsx(a2_t1_analysis, "output/analysis/a2_t1_analysis.xlsx", format_headers = F) # A2_T1
writexl::write_xlsx(a2_t2_list, "output/analysis/a2_t2_analysis.xlsx", format_headers = F) # A2_T1


