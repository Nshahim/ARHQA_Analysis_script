# RHQAHMISVerificationAnalysis ---------------------------------------------------------------------
## 01_SAMPLE
SAMPLE_01 <- a2_t1_data %>%
  group_by(Province, SP_Name, HF_Type) %>%
  summarize(Freq = length(unique(HF_Code))) %>% 
  group_by(Province) %>% 
  mutate(Perc=round((Freq/sum(Freq))*100), Total = sum(Freq)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "HF_Type", values_from = c("Freq", "Perc"), names_vary = "slowest")

## Annex1_HF
patients_stats <- a2_t2_data %>% 
  group_by(Site_Visit_ID, Province, HF_Code, HF_Type, SP_Name, Type_of_service) %>% 
  summarize(selected_cases = n(),
            households_identified=length(Did_you_locate_the_household_of_the_patient[Did_you_locate_the_household_of_the_patient %in% "Yes"]),
            client_presence=length(Is_this_the_actual_patient[Is_this_the_actual_patient %in% "Yes"]),
            date_verified=length(Did_you_visit_the_health_facility_to_receive_the_mentioned_service[Did_you_visit_the_health_facility_to_receive_the_mentioned_service %in% "Yes"]),	
            service_verified=length(Did_you_received_the_mentioned_health_service[Did_you_received_the_mentioned_health_service %in% "Yes"]),
            satisfaction=length(How_satisfied_were_you_with_the_service[How_satisfied_were_you_with_the_service %in% c("Very satisfied", "Moderately satisfied")]),
            paid_for_anything=length(Did_you_have_to_pay_for_anything_at_the_facility[Did_you_have_to_pay_for_anything_at_the_facility %in% "Yes"]),
            Consent_to_interview=length(Consent[Consent %in% "Yes"]),
            accuracy_index = service_verified/Consent_to_interview) %>% select(-Consent_to_interview) %>% ungroup()
# HMIS Report
HMIS_HF_stats <- full_join(
  a2_t1_hmis_service %>%
    group_by(Site_Visit_ID, Province, HF_Code, HF_Type, SP_Name, Type_of_service) %>% 
    summarize(HMIS_Report= sum(Number_of_visits_recorded_for_the_selected_indicator_in_the_HMIS_MIAR_HMIR_for_the_month_of_Asad1401)) %>% 
    ungroup(),
  a2_t1_health_service %>% 
    group_by(Site_Visit_ID, Province, HF_Code, HF_Type, SP_Name, Type_of_service) %>% 
    summarize(HF_Register= sum(Number_of_visits_recorded_for_the_selected_indicator_in_the_health_register_for_the_month_of_Asad1401)) %>% 
    ungroup()
  ) %>% 
  mutate(consistency_index = case_when(
    HMIS_Report > HF_Register ~ HF_Register/HMIS_Report,
    TRUE ~ HMIS_Report/HF_Register
  ))

annex1_HF <- patients_stats %>% 
  full_join(HMIS_HF_stats) %>% 
  relocate(HMIS_Report:HF_Register, .after = Type_of_service) %>% 
  relocate(consistency_index, .before = accuracy_index) %>% 
  mutate(HMIS_composite_score = accuracy_index*consistency_index,
         HMIS_verified_cases = round(HMIS_Report*HMIS_composite_score)) %>% 
  mutate(across(c(consistency_index:HMIS_composite_score), function(x) x = round(x*100)))

## ANNEX2_PROV
annex2_Prov <- annex1_HF %>% 
  group_by(Province, Type_of_service) %>% 
  summarize(across(c(HMIS_Report:paid_for_anything, HMIS_verified_cases), function(x) x = sum(x, na.rm = T)),
            across(consistency_index:HMIS_composite_score, function(x) x = round(mean(x, na.rm=T)))) %>% 
  ungroup() %>% 
  relocate(HMIS_verified_cases, .after = HMIS_composite_score)

## ANNEX3_SP
annex3_SP <- annex1_HF %>% 
  group_by(SP_Name, Type_of_service) %>% 
  summarize(across(c(HMIS_Report:paid_for_anything, HMIS_verified_cases), function(x) x = sum(x, na.rm = T)),
            across(consistency_index:HMIS_composite_score, function(x) x = round(mean(x, na.rm=T)))) %>% 
  ungroup() %>% 
  relocate(HMIS_verified_cases, .after = HMIS_composite_score)

## 02_HMIS_STD
HMIS_STD <- annex1_HF %>% 
  group_by(Province) %>% 
  summarize(consistency_index = round(mean(consistency_index, na.rm=T)),
            accuracy_index = round(mean(accuracy_index, na.rm=T)),
            HMIS_composite_score = round(mean(HMIS_composite_score, na.rm=T))) %>% 
  ungroup()

## 02_HMIS_STD_SP
HMIS_STD_SP <- annex1_HF %>% 
  group_by(SP_Name) %>% 
  summarize(consistency_index = round(mean(consistency_index, na.rm=T)),
            accuracy_index = round(mean(accuracy_index, na.rm=T)),
            HMIS_composite_score = round(mean(HMIS_composite_score, na.rm=T))) %>% 
  ungroup()

## 03_HMIS_MA
HMIS_MA <- annex1_HF %>% 
  group_by(Province) %>% 
  summarize(# calculating Confidence interval values for Consistency and Accuracy indexes
            n = n(), CI_xbar = mean(consistency_index, na.rm=T), AI_xbar = mean(accuracy_index, na.rm=T),
            CI_std = sd(consistency_index, na.rm=T), AI_std = sd(accuracy_index, na.rm = T),
            CI_margin = qt(0.975, df=n-1)*CI_std/sqrt(n), AI_margin = qt(0.975, df=n-1)*AI_std/sqrt(n),
            # Consistency Index
            Consistency_Index = round(mean(consistency_index, na.rm=T)),
            Consistency_lower_limit = round(CI_xbar-CI_margin),
            Consistency_upper_limit = round(CI_xbar+CI_margin),
            # Accuracy Index
            Accuracy_Index = round(mean(accuracy_index, na.rm=T)),
            Accuracy_lower_limit = round(AI_xbar-AI_margin),
            Accuracy_upper_limit = round(AI_xbar+AI_margin),
            HMIS_composite_score = round(mean(HMIS_composite_score, na.rm=T))) %>% 
  ungroup() %>% select(-c(n:AI_margin))

## 03_HMIS_MA_SP
HMIS_MA_SP <- annex1_HF %>% 
  group_by(SP_Name) %>% 
  summarize(# calculating Confidence interval values for Consistency and Accuracy indexes
    n = n(), CI_xbar = mean(consistency_index, na.rm=T), AI_xbar = mean(accuracy_index, na.rm=T),
    CI_std = sd(consistency_index, na.rm=T), AI_std = sd(accuracy_index, na.rm = T),
    CI_margin = qt(0.975, df=n-1)*CI_std/sqrt(n), AI_margin = qt(0.975, df=n-1)*AI_std/sqrt(n),
    # Consistency Index
    Consistency_Index = round(mean(consistency_index, na.rm=T)),
    Consistency_lower_limit = round(CI_xbar-CI_margin),
    Consistency_upper_limit = round(CI_xbar+CI_margin),
    # Accuracy Index
    Accuracy_Index = round(mean(accuracy_index, na.rm=T)),
    Accuracy_lower_limit = round(AI_xbar-AI_margin),
    Accuracy_upper_limit = round(AI_xbar+AI_margin),
    HMIS_composite_score = round(mean(HMIS_composite_score, na.rm=T))) %>% 
  ungroup() %>% select(-c(n:AI_margin))

## 04_HMIS_PROV_COMPILED
HMIS_PROV_Compiled <- annex1_HF %>% 
  group_by(Province, Type_of_service) %>% 
  summarize(HMIS_Reported_Cases = sum(HMIS_Report, na.rm = T),
            HMIS_Verified_Cases = sum(HMIS_verified_cases, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Type_of_service) %>% 
  mutate(Total_Reported_Cases = sum(HMIS_Reported_Cases),
         Total_Verified_Cases = sum(HMIS_Verified_Cases)) %>% ungroup()

# 04_HMIS_SP_COMPILED
HMIS_SP_Compiled <- annex1_HF %>% 
  group_by(SP_Name, Type_of_service) %>% 
  summarize(HMIS_Reported_Cases = sum(HMIS_Report, na.rm = T),
            HMIS_Verified_Cases = sum(HMIS_verified_cases, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Type_of_service) %>% 
  mutate(Total_Reported_Cases = sum(HMIS_Reported_Cases),
         Total_Verified_Cases = sum(HMIS_Verified_Cases)) %>% ungroup()

HMIS_Verification_list = list(
  "01_SAMPLE"=SAMPLE_01,
  "02_HMIS_STD"=HMIS_STD,
  "02_HMIS_STD_SP"=HMIS_STD_SP,
  "03_HMIS_MA"= HMIS_MA, # CI formula needs double check
  "03_HMIS_MA_SP"= HMIS_MA_SP, # CI formula needs double check
  "04_HMIS_PROV_COMPILED"=HMIS_PROV_Compiled,
  "04_HMIS_SP_COMPILED"=HMIS_SP_Compiled,
  "ANNEX1_HF"=annex1_HF,
  "ANNEX2_PROV"=annex2_Prov,
  "ANNEX3_SP"=annex3_SP
)


# RHQAIndividualInterviewFrequencyPercentageTables -------------------------------------------------
# Salary change percentage
average_salary_change_SP <- a1_t3_data %>% 
  group_by(SP_Name, Changes_In_Salary_Amount, Gender_Of_Interviewee) %>% 
  summarize(average = round(mean(salary_change_percentage, na.rm=TRUE),1)) %>% 
  pivot_wider(names_from = c("Changes_In_Salary_Amount", "Gender_Of_Interviewee"), values_from = "average")
average_salary_change_HF <- a1_t3_data %>% 
  group_by(HF_Type_Database, Changes_In_Salary_Amount, Gender_Of_Interviewee) %>% 
  summarize(average = round(mean(salary_change_percentage, na.rm=TRUE),1)) %>% 
  pivot_wider(names_from = c("Changes_In_Salary_Amount", "Gender_Of_Interviewee"), values_from = "average")
average_salary_change_prov <- a1_t3_data %>% 
  group_by(Province, Changes_In_Salary_Amount, Gender_Of_Interviewee) %>% 
  summarize(average = round(mean(salary_change_percentage, na.rm=TRUE),1)) %>% 
  pivot_wider(names_from = c("Changes_In_Salary_Amount", "Gender_Of_Interviewee"), values_from = "average")

# Medicine Calculation -----------------------------------------------------------------------------
# ** Add the % in Stock column and work on the 50% tables
## Medicine Tables calculation (hint: separete tables for each medicine)
medicine_tables <- rbind(
  a1_t2_medicine %>% 
    group_by(Medicine_Name_Value, SP_Name) %>% 
    summarize(mean = round(mean(How_Many_Days_Out_Of_Stock_In_Jawza1400, na.rm=T),1)) %>% 
    ungroup() %>% 
    mutate(column = "Number of days out of stock in June 2021"),
  a1_t2_medicine %>% 
    group_by(Medicine_Name_Value, SP_Name) %>% 
    summarize(mean = round(mean(How_Many_Days_Out_Of_Stock_In_Asad1401, na.rm=T),1)) %>% 
    ungroup() %>% 
    mutate(column = "Number of days out of stock in last reporting perion"),
  a1_t2_medicine %>% 
    group_by(Medicine_Name_Value, SP_Name) %>% 
    summarize(mean = round(mean(Number_of_months_since_August_15_for_which_medicine_witnessed_shortage, na.rm=T),1)) %>% 
    ungroup() %>% 
    mutate(column = "shortage since August 2021 in months")
) %>% 
  pivot_wider(names_from = "column", values_from = "mean") 
# # View
# medicine_tables %>% 
#   filter(Medicine_Name_Value == "ORS Sachets") %>% View

# % in stock and expired	
a1_t2_medicine %>% 
  filter(Is_medicine_available_in_stock_in_the_day_of_observation == "Yes" & Total_number_of_expired_medicine_samples > 0) %>% 
  count(Medicine_Name_Value, SP_Name) %>% 
  group_by(SP_Name) %>% 
  mutate(perc = round((n/sum(n))*100, 1)) %>%
  filter(Medicine_Name_Value == "Chlorhexidine, Gel, tube 20g (7.1%)")

# # of and % of health facilities with more than 50% of medicines on checklist available by service provider and health facility type									
a1_t2_medicine %>% 
  # filter(Is_medicine_available_in_stock_in_the_day_of_observation == "Yes") %>% 
  count(HF_Type, SP_Name, HF_Code, Is_medicine_available_in_stock_in_the_day_of_observation) %>% 
  filter(Is_medicine_available_in_stock_in_the_day_of_observation == "Yes" & n > 19) %>% 
  count(HF)
  
## Equipment Tables
# # of and % of health facilities with more than 50% of equipment on checklist available by service provider								
available_equipments_SP <- a1_t2_equipment %>% 
  group_by(HF_Code) %>% 
  filter(length(Is_equipment_available_in_this_facility == "Yes") > 17) %>% 
  ungroup() %>% 
  group_by(SP_Name) %>% 
  summarize(total = length(unique(HF_Code)),
            Perc = round((total/sum(total))*100, 1))
available_equipments_HF <- a1_t2_equipment %>% 
  group_by(HF_Code) %>% 
  filter(length(Is_equipment_available_in_this_facility == "Yes") > 17) %>% 
  ungroup() %>% 
  group_by(HF_Type) %>% 
  summarize(total = length(unique(HF_Code)),
            Perc = round((total/sum(total))*100, 1))
available_equipments_Prov <- a1_t2_equipment %>% 
  group_by(HF_Code) %>% 
  filter(length(Is_equipment_available_in_this_facility == "Yes") > 17) %>% 
  ungroup() %>% 
  group_by(Province) %>% 
  summarize(total = length(unique(HF_Code)),
            Perc = round((total/sum(total))*100, 1))

# RHQAPatientVerificationFrequencyPercentageTables -------------------------------------------------
patient_stats <- a2_t2_data %>% 
  count(col=Did_you_locate_the_household_of_the_patient)

overview_of_patient_respondent <- data.frame(
  Tool = "Patient Verification",
  "Number of households traced" = patient_stats$n %>% sum(),
  "Number of households located" = 	patient_stats$n[patient_stats$col == "Yes"],
  "Number of households not located" = patient_stats$n[patient_stats$col == "No"],
  "Number of households located with someone home" = length(a2_t2_data$Is_Anyone_Home[a2_t2_data$Is_Anyone_Home %in% "Yes"]),
  "Number of households located with the patient home" = length(a2_t2_data$Is_this_the_actual_patient[a2_t2_data$Is_this_the_actual_patient %in% "Yes"]),
  "Number of households located with the patient not home but available for interview"= length(a2_t2_data$Is_the_actual_patient_available_for_interview[a2_t2_data$Is_the_actual_patient_available_for_interview %in% "Yes"]),
  "Number of patients who consented to be interviewed" = length(a2_t2_data$Consent[a2_t2_data$Consent %in% "Yes"])
)

# # and % of households where person interviewed is patient
patient_interview <- a2_t2_data %>%
  filter(!is.na(Is_this_the_actual_patient)) %>% 
  mutate(patient_being_interviewed = case_when(
    Is_this_the_actual_patient %in% "Yes" ~ "Yes",
    Is_this_the_actual_patient %in% "No" & Is_the_actual_patient_available_for_interview %in% "Yes" ~ "Yes",
    TRUE ~ "No"
  ))
person_interviewed_patient_SP <- patient_interview %>%
  count(SP_Name, patient_being_interviewed, name="Freq") %>% 
  group_by(SP_Name) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "patient_being_interviewed", values_from = c("Freq", "Perc"), names_vary = "slowest")
person_interviewed_patient_HF <- patient_interview %>%
  count(HF_Type, patient_being_interviewed, name="Freq") %>% 
  group_by(HF_Type) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "patient_being_interviewed", values_from = c("Freq", "Perc"), names_vary = "slowest")
person_interviewed_patient_prov <- patient_interview %>%
  count(Province, patient_being_interviewed, name="Freq") %>% 
  group_by(Province) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "patient_being_interviewed", values_from = c("Freq", "Perc"), names_vary = "slowest")

# RHQAPersonnelAssessmentFrequencyPercentageTables -------------------------------------------------
## ** calculate % of female workers in HFs
## % of health facilities that meet the recommended staffing for BPHS facilities by service provider August 2022
HF_meeting_recommended_staffing <- a1_t1_Human_resource %>% 
  mutate(meets_recommended_staffing = case_when(
    # confirm which condition to use
    # staff_constraint == How_many_staff_for_the_selected_type_did_this_facility_have ~ "Yes",
    is.na(Please_specify_the_reason_why_the_number_of_staff_are_less_than_indicated_in_the_BPHS_manual) ~ "Yes",
    TRUE ~ "No"
  )) %>% 
  count(Health_Facility_Staff, HF_Type, meets_recommended_staffing, name="Freq") %>%
  group_by(Health_Facility_Staff, HF_Type) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% 
  ungroup() 
# # check
# a1_t1_Human_resource %>% 
#   filter(staff_constraint != How_many_staff_for_the_selected_type_did_this_facility_have & 
#            is.na(Please_specify_the_reason_why_the_number_of_staff_are_less_than_indicated_in_the_BPHS_manual))  %>% View()

HF_meeting_recommended_staffing_SP <- a1_t1_Human_resource %>% 
  mutate(meets_recommended_staffing = case_when(
    is.na(Please_specify_the_reason_why_the_number_of_staff_are_less_than_indicated_in_the_BPHS_manual) ~ "Yes",
    TRUE ~ "No"
  )) %>% 
  count(Health_Facility_Staff, SP_Name, meets_recommended_staffing, name="Freq") %>%
  group_by(Health_Facility_Staff, SP_Name) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% 
  ungroup() 


# % of health facilities with available female health care workers by province in August 2022							
female_health_workers <- a1_t1_staff_info %>% 
  filter(Gender_Of_Interviewee == "Female") %>%
  count(Province, HF_Type, Health_Facility_Staff)

female_health_workers <- a1_t1_staff_info %>% 
  filter(Gender_Of_Interviewee == "Female") %>%
  group_by(Province, HF_Type, Health_Facility_Staff) %>% 
  summarize(total = sum(Current_How_many_staff_for_the_selected_type_did_this_facility_have))


# RHQAOverviewFrequencyPercentageTables ------------------------------------------------------------
## *** calculate the values for this table "Total # and % of respondents by gender and interviewee respondent type"
unique_service_providers <- a1_t1_data %>% 
  group_by(Province, SP_Name) %>% 
  summarize(Freq = length(unique(SP_Name))) %>% 
  group_by(Province) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "SP_Name", values_from = c("Freq", "Perc"), names_vary = "slowest")

unique_health_facilty_type <- a1_t1_data %>% 
  group_by(Province, HF_Type) %>% 
  summarize(Freq = length(unique(HF_Code))) %>% 
  group_by(Province) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "HF_Type", values_from = c("Freq", "Perc"), names_vary = "slowest")

unique_health_facilty_type_SP <- a1_t1_data %>% 
  group_by(SP_Name, HF_Type) %>% 
  summarize(Freq = length(unique(HF_Code))) %>% 
  group_by(SP_Name) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "HF_Type", values_from = c("Freq", "Perc"), names_vary = "slowest")

# Gender and Interview Respondent Type
number_of_resp_gender <- rbind(
  a2_t1_rep_service %>% 
    select(Interviewee_Respondent_Type, Gender_Of_Interviewee) %>% 
    mutate(Dataset = "HMIS Verification"),
  a1_t1_data %>% 
    select(Interviewee_Respondent_Type, Gender_Of_Interviewee) %>% 
    mutate(Dataset = "Personnel Assessment"),
  a1_t2_medicine_equip %>% 
    filter(!is.na(Gender_Of_Interviewee)) %>% 
    select(Interviewee_Respondent_Type, Gender_Of_Interviewee) %>% 
    mutate(Dataset = "Medicine, Infrastructure"),
  a1_t3_data %>% 
    filter(!is.na(Gender_Of_Interviewee)) %>% 
    select(Interviewee_Respondent_Type=respodent_position_sample, Gender_Of_Interviewee) %>% 
    mutate(Dataset = "Individual Interview"),
  a1_t4_data %>% 
    select(Interviewee_Respondent_Type, Gender_Of_Interviewee) %>% 
    mutate(Dataset = "Waste Management"),
  a2_t2_data %>% 
    select(Interviewee_Respondent_Type, Gender_Of_Interviewee) %>% 
    mutate(Dataset = "Patient Verification")
) 

type_gender_of_respondents <- number_of_resp_gender %>% 
  count(Dataset, Interviewee_Respondent_Type, Gender_Of_Interviewee, name = "Freq") %>% 
  group_by(Dataset, Interviewee_Respondent_Type) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "Gender_Of_Interviewee", values_from = c("Freq", "Perc"), names_vary = "slowest")
 
resp_type_merged <- rbind(
  a2_t1_rep_service %>% 
    select(Interviewee_Respondent_Type) %>% 
    mutate(Dataset = "HMIS Verification"),
  a1_t1_data %>% 
    select(Interviewee_Respondent_Type) %>% 
    mutate(Dataset = "Personnel Assessment"),
  a1_t2_medicine_equip %>% 
    filter(!is.na(Gender_Of_Interviewee)) %>% 
    select(Interviewee_Respondent_Type) %>% 
    mutate(Dataset = "Medicine, Infrastructure"),
  a1_t3_data %>% 
    filter(!is.na(Gender_Of_Interviewee)) %>% 
    select(Interviewee_Respondent_Type=respodent_position_sample) %>% 
    mutate(Dataset = "Individual Interview"),
  a1_t4_data %>% 
    select(Interviewee_Respondent_Type) %>% 
    mutate(Dataset = "Waste Management"),
  a2_t2_data %>% 
    select(Interviewee_Respondent_Type) %>% 
    mutate(Dataset = "Patient Verification")
) %>%
  count(Dataset, Interviewee_Respondent_Type, name="Freq") %>% 
  group_by(Dataset) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "Interviewee_Respondent_Type", values_from = c("Freq", "Perc"), names_vary = "slowest")

surveyor_gender_merged <- rbind(
  a2_t1_data %>% 
    select(Surveyor_Id, Surveyor_Gender) %>% 
    mutate(Dataset = "HMIS Verification"),
  a1_t1_data %>% 
    select(Surveyor_Id, Surveyor_Gender) %>% 
    mutate(Dataset = "Personnel Assessment"),
  a1_t2_data %>% 
    select(Surveyor_Id, Surveyor_Gender) %>% 
    mutate(Dataset = "Medicine, Infrastructure"),
  a1_t3_data %>% 
    select(Surveyor_Id, Surveyor_Gender) %>% 
    mutate(Dataset = "Individual Interview"),
  a1_t4_data %>% 
    select(Surveyor_Id, Surveyor_Gender) %>% 
    mutate(Dataset = "Waste Management"),
  a2_t2_data %>% 
    select(Surveyor_Id, Surveyor_Gender) %>% 
    mutate(Dataset = "Patient Verification")
) %>% unique() %>% filter(!is.na(Surveyor_Gender)) %>% 
  mutate(Surveyor_Gender = str_to_title(Surveyor_Gender))

surveyor_gender <- surveyor_gender_merged %>% 
  count(Dataset, Surveyor_Gender, name="Freq") %>% 
  group_by(Dataset) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "Surveyor_Gender", values_from = c("Freq", "Perc"), names_vary = "slowest")

surveyor_enum_gender <- surveyor_gender_merged %>% 
  rename(Surveyor_Type=Dataset) %>% 
  mutate(Surveyor_Type = case_when(
    Surveyor_Type == "Individual Interview" ~ "Call Centre Agent",
    TRUE ~ "Enumerator" 
  )) %>% unique() %>% 
  count(Surveyor_Type, Surveyor_Gender, name="Freq") %>% 
  group_by(Surveyor_Type) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "Surveyor_Gender", values_from = c("Freq", "Perc"), names_vary = "slowest")

site_visits_merged <- rbind(
  a2_t1_data %>% 
    select(Site_Visit_ID, Province) %>% 
    mutate(Dataset = "HMIS Verification"),
  a1_t1_data %>% 
    select(Site_Visit_ID, Province) %>% 
    mutate(Dataset = "Personnel Assessment"),
  a1_t2_data %>% 
    select(Site_Visit_ID, Province) %>% 
    mutate(Dataset = "Medicine, Infrastructure"),
  a1_t3_data %>% 
    select(Site_Visit_ID, Province) %>% 
    mutate(Dataset = "Individual Interview"),
  a1_t4_data %>% 
    select(Site_Visit_ID, Province) %>% 
    mutate(Dataset = "Waste Management"),
  a2_t2_data %>% 
    select(Site_Visit_ID, Province) %>% 
    mutate(Dataset = "Patient Verification")
) 
number_of_Site_Visits <- site_visits_merged %>% 
  group_by(Dataset, Province, name="Freq") %>% 
  summarize(Freq = length(unique(Site_Visit_ID))) %>% 
  group_by(Dataset) %>% 
  mutate(Perc = paste0(round((Freq/sum(Freq))*100, 1), "%")) %>% ungroup() %>% 
  pivot_wider(names_from = "Province", values_from = c("Freq", "Perc"), names_vary = "slowest")

respondents_and_surveyors <- cbind(
  # Site Visits
  site_visits_merged %>% 
    group_by(Dataset) %>% 
    summarize(Site_Visits = length(unique(Site_Visit_ID))) %>% ungroup(),
  # Respondents
  number_of_resp_gender %>% 
    count(Dataset, Gender_Of_Interviewee, name = "Freq") %>% 
    group_by(Dataset) %>% 
    mutate(Type = "Number of respondents", Total = sum(Freq)) %>% ungroup() %>% 
    pivot_wider(names_from = "Gender_Of_Interviewee", values_from = "Freq") %>% 
    select(Type, Female, Male, Total),
  # Surveyors
  surveyor_gender_merged %>% 
    count(Dataset, Surveyor_Gender, name="Freq") %>% 
    group_by(Dataset) %>% 
    mutate(Type = "Number of Surveyors", Total = sum(Freq)) %>% ungroup() %>% 
    pivot_wider(names_from = "Surveyor_Gender", values_from = "Freq") %>% 
    select(Type, Female, Male, Total)
)

# Rm extra objects --------
rm(patient_stats)

# Testing -----------------------------------


