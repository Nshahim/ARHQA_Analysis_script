## A1_T1
a1_t1_data <- a1_t1_data %>% mutate(SP_Name=toupper(SP_Name))
a1_t1_SP_name <- a1_t1_data %>% select(Gender_Of_Interviewee, SP_Name, KEY)
a1_t1_Human_resource <- a1_t1_Human_resource %>% 
  left_join(a1_t1_SP_name, by="KEY")
a1_t1_staff_info <- a1_t1_staff_info %>% 
  left_join(a1_t1_SP_name, by="KEY")
a1_t1_staff_notin_jawza <- a1_t1_staff_notin_jawza %>% 
  left_join(a1_t1_SP_name, by="KEY")

## A1_T2
a1_t2_data <- a1_t2_data %>% mutate(SP_Name=toupper(SP_Name))
a1_t2_SP_name <- a1_t2_data %>% select(SP_Name, HF_Code, KEY)
a1_t2_infra <- a1_t2_infra %>% 
  left_join(a1_t2_SP_name, by="KEY")
a1_t2_medicine <- a1_t2_medicine %>% 
  left_join(a1_t2_SP_name, by="KEY")
a1_t2_common_med <- a1_t2_common_med %>% 
  left_join(a1_t2_SP_name, by="KEY")
a1_t2_medicine_equip <- a1_t2_medicine_equip %>% 
  left_join(a1_t2_SP_name, by="KEY")
a1_t2_equipment <- a1_t2_equipment %>% 
  left_join(a1_t2_SP_name, by="KEY")

## A1_T3
service_providers <- a1_t1_data %>% mutate(SP_Name=toupper(SP_Name)) %>% 
  select(Site_Visit_ID, SP_Name) %>% unique()
# Filter Complete interviews and join SP_Name
a1_t3_data <- a1_t3_data %>% 
  filter(phone_response_short %in% "Complete") %>% 
  left_join(service_providers, by="Site_Visit_ID")

# Join Salary increase and decrease
a1_t3_data <- a1_t3_data %>% 
  mutate(salary_change_percentage = case_when(
    !is.na(Decrease_Percentage) ~ paste0("-", str_replace_all(Decrease_Percentage, " %", "")),
    TRUE ~ str_replace_all(Increase_Percentage, " %", "")
  ), salary_change_percentage = as.numeric(salary_change_percentage)) %>% 
  relocate(salary_change_percentage, .after = Changes_In_Salary_Amount)
# Salary sheet
a1_t3_salary <- a1_t3_salary %>% 
  left_join(a1_t3_data %>% select(SP_Name, KEY), by="KEY") 

## A1_T4
a1_t4_data <- a1_t4_data %>% mutate(SP_Name= case_when(
  SP_Name == "OHMP" ~ "OHPM",
  TRUE ~ toupper(SP_Name)
))

## A2_T1
a2_t1_SP_name <- a2_t1_data %>% select(SP_Name, HF_Code, KEY)
a2_t1_rep_service <- a2_t1_rep_service %>%  
  left_join(a2_t1_SP_name, by="KEY")
a2_t1_hmis_service <- a2_t1_hmis_service %>% 
  left_join(a2_t1_SP_name, by="KEY")
a2_t1_health_service <- a2_t1_health_service %>% 
  left_join(a2_t1_SP_name, by="KEY")

## A2_T2
# Fixing province
a2_t2_data <- a2_t2_data %>% 
  mutate(Province = case_when(
    Province == "kunar" ~ "Kunar",
    TRUE ~ Province
  ))

# Remove extra objects -----------------------------------------------------------------------------
rm(a1_t2_SP_name, a2_t1_SP_name, service_providers)