#%%bash
#dx download "alldata_clean_10.csv"
#dx download "european_standard.csv"

#Generate data frame for the european standard population, available https://www.opendata.nhs.scot/dataset/standard-populations/resource/29ce4cda-a831-40f4-af24-636196e05c1a?inner_span=True

esp <- data.frame(AgeESP = c(
               "0-4 years", "0-4 years",
               "5-9 years", "5-9 years",
               "10-14 years", "10-14 years",
               "15-19 years", "15-19 years",
               "20-24 years", "20-24 years",
               "25-29 years", "25-29 years",
               "30-34 years", "30-34 years",
               "35-39 years", "35-39 years",
               "40-44 years", "40-44 years",
               "45-49 years", "45-49 years",
               "50-54 years", "50-54 years",
               "55-59 years", "55-59 years",
               "60-64 years", "60-64 years",
               "65-69 years", "65-69 years",
               "70-74 years", "70-74 years",
               "75-79 years", "75-79 years",
               "80-84 years", "80-84 years",
               "85plus years", "85plus years"
             ),
              SexESP = rep(c("Male", "Female"), times = 18),
             n = c(5000, 5000, 
                   5500, 5500, 
                   5500, 5500, 
                   5500, 5500, 
                   6000, 6000, 
                   6000, 6000, 
                   6500, 6500, 
                   7000, 7000, 
                   7000, 7000,
                   7000, 7000, 
                   7000, 7000, 
                   6500, 6500, 
                   6000, 6000, 
                   5500, 5500, 
                   5000, 5000, 
                   4000, 4000, 
                   2500, 2500, 
                   2500, 2500))

write.csv(esp, "european_standard_modified.csv", row.names = FALSE)






###### Create age- and sex- standardised prevalences #######

library(dplyr)


alldata<-read.csv("alldata_clean.csv")
esp <- read.csv("european_standard_modified.csv")


# Remove missing data from mooddisorder
alldata <- alldata[!alldata$mooddisorder %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% c('',NA, TRUE), ] # This group is removed from all further analysis as they do not fit in any group

# Label blanks as NA
alldata[alldata == ""] <- NA

write.csv(alldata, "alldata_prev.csv")




alldata_prev <- read.csv("alldata_prev.csv")

# Mutate "age" so categories are the same as the esp
alldata_prev <- 
  alldata_prev %>%    
  filter (age >= 18 & age <= 100) %>%
  mutate(age = case_when(
    age <  5 ~   "0-4 years",
    age >=  5 & age < 10 ~   "5-9 years", 
    age >= 10 & age < 15 ~ "10-14 years",
    age >= 15 & age < 20 ~ "15-19 years", 
    age >= 20 & age < 25 ~ "20-24 years", 
    age >= 25 & age < 30 ~ "25-29 years", 
    age >= 30 & age < 35 ~ "30-34 years", 
    age >= 35 & age < 40 ~ "35-39 years", 
    age >= 40 & age < 45 ~ "40-44 years",
    age >= 45 & age < 50 ~ "45-49 years", 
    age >= 50 & age < 55 ~ "50-54 years",
    age >= 55 & age < 60 ~ "55-59 years", 
    age >= 60 & age < 65 ~ "60-64 years", 
    age >= 65 & age < 70 ~ "65-69 years", 
    age >= 70 & age < 75 ~ "70-74 years", 
    age >= 75 & age < 80 ~ "75-79 years", 
    age >= 80 & age < 85 ~ "80-84 years",
    age >= 85            ~ "85plus years"))


### Create a variable called MULTIMORBIDITY ###
#Relabel 0 as FALSE and 1 as TRUE
angina <- ifelse(alldata_prev$angina == FALSE, 0, 1)
stroke <- ifelse(alldata_prev$stroke == FALSE, 0, 1)
mi <- ifelse(alldata_prev$mi == FALSE, 0, 1)
chf <- ifelse (alldata_prev$chf == FALSE, 0, 1)
cad <- ifelse(alldata_prev$cad == FALSE, 0, 1)
hypertension <- ifelse(alldata_prev$hypertension == FALSE, 0, 1)
hypercholesterolaemia <- ifelse(alldata_prev$hypercholesterolaemia == FALSE, 0, 1)
obesity <- ifelse(alldata_prev$obese == FALSE, 0, 1)

#Add all diagnoses together so that any participant ticking more than one category will be given a number >1
alldata_prev$multimorbidity_add <- (angina+stroke+mi+chf+cad+hypertension+hypercholesterolaemia+obesity)

#Create multimorbidity variable
alldata_prev$multimorbidity <- ifelse(alldata_prev$multimorbidity_add > 1, "Multiple cardiometabolic comorbidities", "One or no cardiometabolic disorders")

alldata_prev$multisystem_comorbidity <- grepl("Multiple cardiometabolic comorbidities", alldata_prev$multimorbidity, ignore.case = T)
alldata_prev$no_comorbidity <- grepl("One or no cardiometabolic disorders", alldata_prev$multimorbidity, ignore.case = T)

alldata_prev$multimorbidity <- ifelse(alldata_prev$multisystem_comorbidity == T,
                                      TRUE, FALSE)



# Filter for affective disorders
bip_data <- alldata_prev %>% filter(mooddisorder == "Bipolar")
dep_data <- alldata_prev %>% filter(mooddisorder == "Depression")
anx_data <- alldata_prev %>% filter(mooddisorder == "Anxiety")
con_data <- alldata_prev %>% filter(mooddisorder == "Comparison")


# Create a function that calculates age and sex standardised prevalence for total cvd
get_std_any <- function(alldata_prev, esp) {
  
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(cvd == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}

# Create a function that calculates age and sex standardised prevalence for angina
get_std_angina <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(angina == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}



# Create a function that calculates age and sex standardised prevalence for cad
get_std_cad <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(cad == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}





# Create a function that calculates age and sex standardised prevalence for mi
get_std_mi <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(mi == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}



# Create a function that calculates age and sex standardised prevalence for chf
get_std_chf <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(chf == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}




# Create a function that calculates age and sex standardised prevalence for stroke
get_std_stroke <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(stroke == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}





# Create a function that calculates age and sex standardised prevalence for hypertension
get_std_hypertension <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(hypertension == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}



# Create a function that calculates age and sex standardised prevalence for hypercholesterolaemia
get_std_hypercholesterolaemia <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(hypercholesterolaemia == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}

                  



# Create a function that calculates age and sex standardised prevalence for t2dm
get_std_t2dm <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(t2dm == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}




# Create a function that calculates age and sex standardised prevalence for obese
get_std_obese <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(obese == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}




# Create a function that calculates age and sex standardised prevalence for multimorbidity
get_std_multimorbidity <- function(alldata_prev, esp) {
  std_df <- alldata_prev %>%
    group_by(age, sex) %>%
    summarise(
      cases = sum(multimorbidity == TRUE, na.rm = TRUE),
      population = n(),
      .groups = "drop"
    ) %>%
    inner_join(esp, by = c("age" = "AgeESP", "sex" = "SexESP")) %>%
    mutate(
      rate = cases / population,
      weighted_rate = rate * n,
      var_weighted_rate = ((rate * (1 - rate)) / population) * (n^2)
    )
  
  std_summary <- std_df %>%
    group_by(sex) %>%
    summarise(
      Ncases = sum(cases),
      Ntotal = sum(population),
      standardised_prevalence = sum(weighted_rate) / sum(n),
      var_total = sum(var_weighted_rate),
      se = sqrt(var_total) / sum(n),
      .groups = "drop"
    ) %>%
    mutate(
      ci_lower = standardised_prevalence - 1.96 * se,
      ci_upper = standardised_prevalence + 1.96 * se,
      standardised_prevalence_percent = standardised_prevalence * 100,
      ci_lower_percent = ci_lower * 100,
      ci_upper_percent = ci_upper * 100
    )
  
  return(std_summary)
}



std_bip_any <- get_std_any(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Any cardiometabolic disorder")
std_dep_any <- get_std_any(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Any cardiometabolic disorder")
std_anx_any <- get_std_any(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Any cardiometabolic disorder")
std_con_any <- get_std_any(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Any cardiometabolic disorder") 
std_bip_angina <- get_std_angina(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Angina")
std_dep_angina <- get_std_angina(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Angina")
std_anx_angina <- get_std_angina(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Angina")
std_con_angina <- get_std_angina(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Angina")
std_bip_cad <- get_std_cad(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Coronary artery disease")
std_dep_cad <- get_std_cad(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Coronary artery disease")
std_anx_cad <- get_std_cad(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Coronary artery disease")
std_con_cad <- get_std_cad(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Coronary artery disease")
std_bip_mi <- get_std_mi(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Myocardial infarction")
std_dep_mi <- get_std_mi(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Myocardial infarction")
std_anx_mi <- get_std_mi(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Myocardial infarction")
std_con_mi <- get_std_mi(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Myocardial infarction")
std_bip_chf <- get_std_chf(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Congestive heart failure")
std_dep_chf <- get_std_chf(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Congestive heart failure")
std_anx_chf <- get_std_chf(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Congestive heart failure")
std_con_chf <- get_std_chf(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Congestive heart failure")
std_bip_stroke <- get_std_stroke(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Stroke")
std_dep_stroke <- get_std_stroke(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Stroke")
std_anx_stroke <- get_std_stroke(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Stroke")
std_con_stroke <- get_std_stroke(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Stroke")
std_bip_hypertension <- get_std_hypertension(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Hypertension")
std_dep_hypertension <- get_std_hypertension(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Hypertension")
std_anx_hypertension <- get_std_hypertension(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Hypertension")
std_con_hypertension <- get_std_hypertension(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Hypertension")
std_bip_hypercholesterolaemia <- get_std_hypercholesterolaemia(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Hypercholesterolaemia")
std_dep_hypercholesterolaemia <- get_std_hypercholesterolaemia(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Hypercholesterolaemia")
std_anx_hypercholesterolaemia <- get_std_hypercholesterolaemia(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Hypercholesterolaemia")
std_con_hypercholesterolaemia <- get_std_hypercholesterolaemia(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Hypercholesterolaemia")
std_bip_t2dm <- get_std_t2dm(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Type 2 diabetes")
std_dep_t2dm <- get_std_t2dm(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Type 2 diabetes")
std_anx_t2dm <- get_std_t2dm(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Type 2 diabetes")
std_con_t2dm <- get_std_t2dm(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Type 2 diabetes")
std_bip_obese<- get_std_obese(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Obesity (BMI >30)")
std_dep_obese <- get_std_obese(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Obesity (BMI >30)")
std_anx_obese <- get_std_obese(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Obesity (BMI >30)")
std_con_obese <- get_std_obese(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Obesity (BMI >30)")
std_bip_multimorbidity <- get_std_multimorbidity(bip_data, esp) %>% mutate(mooddisorder = "Bipolar") %>% mutate(disorder = "Cardiometabolic multimorbidity")
std_dep_multimorbidity <- get_std_multimorbidity(dep_data, esp) %>% mutate(mooddisorder = "Depression") %>% mutate(disorder = "Cardiometabolic multimorbidity")
std_anx_multimorbidity <- get_std_multimorbidity(anx_data, esp) %>% mutate(mooddisorder = "Anxiety") %>% mutate(disorder = "Cardiometabolic multimorbidity")
std_con_multimorbidity <- get_std_multimorbidity(con_data, esp) %>% mutate(mooddisorder = "Comparison") %>% mutate(disorder = "Cardiometabolic multimorbidity")


combined_results <- bind_rows(
  std_bip_any, std_dep_any, std_anx_any, std_con_any,
  std_bip_angina, std_dep_angina, std_anx_angina, std_con_angina,
  std_bip_cad, std_dep_cad, std_anx_cad, std_con_cad,
  std_bip_mi, std_dep_mi, std_anx_mi, std_con_mi,
  std_bip_chf, std_dep_chf, std_anx_chf, std_con_chf,
  std_bip_stroke, std_dep_stroke, std_anx_stroke, std_con_stroke,
  std_bip_hypertension, std_dep_hypertension, std_anx_hypertension, std_con_hypertension,
  std_bip_hypercholesterolaemia, std_dep_hypercholesterolaemia, std_anx_hypercholesterolaemia, std_con_hypercholesterolaemia,
  std_bip_t2dm, std_dep_t2dm, std_anx_t2dm, std_con_t2dm,
  std_bip_obese, std_dep_obese, std_anx_obese, std_con_obese,
  std_bip_multimorbidity, std_dep_multimorbidity, std_anx_multimorbidity, std_con_multimorbidity
)

prev <- as.data.frame(combined_results)
prev_simple <- prev %>%
  select(-standardised_prevalence, -var_total, -se, -ci_lower, -ci_upper)
prev_simple <- as.matrix(prev_simple)

write.csv(prev_simple, "prev_age_sex_standardised_grouped_by_sex.csv")


