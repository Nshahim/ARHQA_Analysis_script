# pull file names --------------------------------------------------
files <- list.files(data_path, pattern = ".xls")

for (data_name in files) {
  # A1_T1
  if(str_detect(data_name, "A1_T1")){
    a1_t1 <- data_name
  }
  # A1_T2
  if(str_detect(data_name, "A1_T2")){
    a1_t2 <- data_name
  }
  # A1_T3 
  if(str_detect(data_name, "A1_T3")){
    a1_t3 <- data_name
  }
  # A1_T4
  if(str_detect(data_name, "A1_T4")){
    a1_t4 <- data_name
  }
  # A2_T1
  if(str_detect(data_name, "A2_T1")){
    a2_t1 <- data_name
  }
  # A2_T2
  if(str_detect(data_name, "A2_T2")){
    a2_t2 <- data_name
  }
}

# Read data --------------------------------------------------
## A1_T1
if (exists("a1_t1")) {
  a1_t1_data <- read_excel(glue("{data_path}{a1_t1}"), sheet = "data", guess_max = 100000, na = convert_to_na)
  a1_t1_Human_resource <- read_excel(glue("{data_path}{a1_t1}"), sheet = "HF_Human_Resources", guess_max = 100000, na = convert_to_na)
  a1_t1_staff_info <- read_excel(glue("{data_path}{a1_t1}"), sheet = "Staff_Info", guess_max = 100000, na = convert_to_na)
  a1_t1_staff_notin_jawza <- read_excel(glue("{data_path}{a1_t1}"), sheet = "Staff_not_in_the_jawza1400_r...", guess_max = 100000, na = convert_to_na)
  a1_t1_new_staff_info <- read_excel(glue("{data_path}{a1_t1}"), sheet = "New_Staff_Infor", guess_max = 100000, na = convert_to_na)
}

## A1_T2
if (exists("a1_t2")) {
  a1_t2_data <- read_excel(glue("{data_path}{a1_t2}"), sheet = "data", guess_max = 100000, na = convert_to_na)
  a1_t2_medicine_equip <- read_excel(glue("{data_path}{a1_t2}"), sheet = "Medicine_Or_Equipment_Infras...", guess_max = 100000, na = convert_to_na)
  a1_t2_infra <- read_excel(glue("{data_path}{a1_t2}"), sheet = "Infrastructure_Group", guess_max = 100000, na = convert_to_na)
  a1_t2_medicine <- read_excel(glue("{data_path}{a1_t2}"), sheet = "Medicine", guess_max = 100000, na = convert_to_na)
  a1_t2_common_med <- read_excel(glue("{data_path}{a1_t2}"), sheet = "Common_Medicine_Group", guess_max = 100000, na = convert_to_na)
  a1_t2_equipment <- read_excel(glue("{data_path}{a1_t2}"), sheet = "Equipment", guess_max = 100000, na = convert_to_na)
}

## A1_T3
if (exists("a1_t3")) {
  a1_t3_data <- read_excel(glue("{data_path}{a1_t3}"), sheet = "data", guess_max = 100000, na = convert_to_na)
  a1_t3_salary <- read_excel(glue("{data_path}{a1_t3}"), sheet = "Salary", guess_max = 100000, na = convert_to_na)
}

## A1_T4
if (exists("a1_t4")) {
  a1_t4_data <- read_excel(glue("{data_path}{a1_t4}"), sheet = "data", guess_max = 100000, na = convert_to_na)
  a1_t4_training_photos <- read_excel(glue("{data_path}{a1_t4}"), sheet = "Photos_of_Training_Curriculum", guess_max = 100000, na = convert_to_na)
  a1_t4_relev_area_photos <- read_excel(glue("{data_path}{a1_t4}"), sheet = "Photos_of_the_relevant_areas", guess_max = 100000, na = convert_to_na)
}

## A2_T1
if (exists("a2_t1")) {
  a2_t1_data <- read_excel(glue("{data_path}{a2_t1}"), sheet = "data", guess_max = 100000, na = convert_to_na)
  a2_t1_rep_service <- read_excel(glue("{data_path}{a2_t1}"), sheet = "rep_service", guess_max = 100000, na = convert_to_na)
  a2_t1_hmis_service <- read_excel(glue("{data_path}{a2_t1}"), sheet = "number_of_services_HMIS", guess_max = 100000, na = convert_to_na)
  a2_t1_patient_sample <- read_excel(glue("{data_path}{a2_t1}"), sheet = "Patient_Sample", guess_max = 100000, na = convert_to_na)
  a2_t1_health_service <- read_excel(glue("{data_path}{a2_t1}"), sheet = "number_of_services_Health_Re...", guess_max = 100000, na = convert_to_na)
}

## A2_T2
if (exists("a2_t2")) {
  a2_t2_data <- read_excel(glue("{data_path}{a2_t2}"), sheet = "data", guess_max = 100000, na = convert_to_na)
}

# remove extra objects --------------------------------------------------
rm(convert_to_na, data_name, data_path, files, a1_t1, a1_t2, a1_t3, a1_t4, a2_t1, a2_t2)
