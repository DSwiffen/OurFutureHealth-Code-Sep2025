#%%bash
#dx download "v_03_dataset/all_outcomes_participant.csv"
#dx download "v_03_dataset/all_outcomes_questionnaire.csv"


library(dplyr)

### Missing data counts for total sample and sample used in analysis

all_participant <- read.csv("all_outcomes_participant.csv")
all_questionnaire <- read.csv("all_outcomes_questionnaire.csv")
alldata <- merge(all_participant, all_questionnaire, by = "pid")

## Rename columns
names(alldata)[names(alldata) == "diag_2_m"] <- "diag_sys"
names(alldata)[names(alldata) == "diag_cvd_1_m"] <- "diag_cvd"
names(alldata)[names(alldata) == "diag_endocr_1_m"] <- "diag_endo"
names(alldata)[names(alldata) == "smoke_status_2_1"] <- "smoke_status"
names(alldata)[names(alldata) == "alcohol_curr_1_1"] <- "alcohol_curr"
names(alldata)[names(alldata) == "activity_mod_mins_2_1"] <- "activity_mod"
names(alldata)[names(alldata) == "activity_vig_mins_2_1"] <- "activity_vig"
names(alldata)[names(alldata) == "sleep_chronotype_1_1"] <- "sleep_chronotype"
names(alldata)[names(alldata) == "demog_height_1_1"] <- "height"
names(alldata)[names(alldata) == "demog_weight_1_1"] <- "weight"
names(alldata)[names(alldata) == "demog_sex_2_1"] <- "sex"
names(alldata)[names(alldata) == "demog_ethnicity_1_1"] <- "ethnicity"
names(alldata)[names(alldata) == "consent_year"] <- "consent_year"
names(alldata)[names(alldata) == "birth_year"] <- "birth_year"
names(alldata)[names(alldata) == "edu_qual_1_m"] <- "edu_qual"
names(alldata)[names(alldata) == "housing_income_1_1"] <- "housing_income"
names(alldata)[names(alldata) == "diag_psych_1_m"] <- "diag_psych"
names(alldata)[names(alldata) == "diag_psych_anx_1_m"] <- "diag_psych_anx"
names(alldata)[names(alldata) == "diag_psych_depr_1_m"] <- "diag_psych_depr"
names(alldata)[names(alldata) == "medicat_psych_1_m"] <- "psych_meds"
names(alldata)[names(alldata) == "activity_type_1_m"] <-"activity_type"
names(alldata)[names(alldata) == "smoke_tobacco_type_1_m"] <- "tobacco_even_once"
names(alldata)[names(alldata) == "smoke_reg_1_m"] <- "smoke_reg"

# Convert consent_year and birth_year variables to R-friendly year format
alldata$consent_year <- format(as.Date(strptime(alldata$consent_year, format = "%Y")), "%Y")
alldata$consent_year <- as.numeric(alldata$consent_year)
alldata$birth_year <- format(as.Date(strptime(alldata$birth_year, format = "%Y")), "%Y")
alldata$birth_year <- as.numeric(alldata$birth_year)


## Calculate age at point of consent from consent year and birth year
alldata$age <- alldata$consent_year - alldata$birth_year
alldata <- alldata[,c(1,ncol(alldata),2:(ncol(alldata)-1))] # bringing age to front of dataframe

#Create new columns for endocrine diagnosis
# TRUE if diag_endo contains the strings "Type 1 diabetes", "Type 2 diabetes",
alldata$t2dm <- grepl("Type 2 diabetes", alldata$diag_endo, ignore.case = T)

#Calculate BMI
alldata$height <- alldata$height / 100 # convert to meters
alldata$bmi <- alldata$weight/alldata$height^2
# Categorise BMI output
alldata$underweight <- ifelse(alldata$bmi <18.5, TRUE, FALSE)
alldata$normal_weight <- ifelse((alldata$bmi >= 18.5 & alldata$bmi < 25), TRUE, FALSE)
alldata$overweight <- ifelse((alldata$bmi >= 25 & alldata$bmi <30), TRUE, FALSE)
alldata$obese <- ifelse(alldata$bmi >=30, TRUE, FALSE)

#Create new columns for cardiovascular diagnosis
# TRUE if diag_cvd contains the strings "Stroke", "Coronary artery", "Congestive heart failure", "High cholesterol", "Heart attack", "Chest pain", "High blood pressure"
alldata$stroke<- grepl("Stroke", alldata$diag_cvd, ignore.case = T)
alldata$cad <- grepl("Coronary artery", alldata$diag_cvd, ignore.case = T)
alldata$chf<- grepl("Congestive heart failure", alldata$diag_cvd, ignore.case = T)
alldata$hypercholesterolaemia<- grepl("High cholesterol", alldata$diag_cvd, ignore.case = T)
alldata$mi <- grepl("Heart attack", alldata$diag_cvd, ignore.case = T)
alldata$angina <- grepl("Chest pain", alldata$diag_cvd, ignore.case = T)
alldata$hypertension <- grepl("High blood pressure", alldata$diag_cvd, ignore.case = T)

alldata$cvd <- ifelse(alldata$t2dm == T |
                        alldata$stroke == T |
                        alldata$cad == T |
                        alldata$chf == T |
                        alldata$hypercholesterolaemia == T |
                        alldata$mi == T |
                        alldata$angina == T |
                        alldata$hypertension == T |
                        alldata$obese == T,
                      TRUE, FALSE)

### Create a variable called MULTIMORBIDITY ###
#Relabel 0 as FALSE and 1 as TRUE
angina <- ifelse(alldata$angina == FALSE, 0, 1)
stroke <- ifelse(alldata$stroke == FALSE, 0, 1)
mi <- ifelse(alldata$mi == FALSE, 0, 1)
chf <- ifelse (alldata$chf == FALSE, 0, 1)
cad <- ifelse(alldata$cad == FALSE, 0, 1)
hypertension <- ifelse(alldata$hypertension == FALSE, 0, 1)
hypercholesterolaemia <- ifelse(alldata$hypercholesterolaemia == FALSE, 0, 1)
obesity <- ifelse(alldata$obese == FALSE, 0, 1)

#Add all diagnoses together so that any participant ticking more than one category will be given a number >1
alldata$multimorbidity_add <- (angina+stroke+mi+chf+cad+hypertension+hypercholesterolaemia+obesity)

#Create multimorbidity variable
alldata$multimorbidity <- ifelse(alldata$multimorbidity_add > 1, "Multiple cardiometabolic comorbidities", "One or no cardiometabolic disorders")

alldata$multisystem_comorbidity <- grepl("Multiple cardiometabolic comorbidities", alldata$multimorbidity, ignore.case = T)
alldata$no_comorbidity <- grepl("One or no cardiometabolic disorders", alldata$multimorbidity, ignore.case = T)

alldata$multimorbidity <- ifelse(alldata$multisystem_comorbidity == T,
                                 TRUE, FALSE)



total_sample <- nrow(alldata)

m01 <- alldata[alldata$diag_sys %in% '', ] # Checks to see how many blanks there are in that variable
analysed_sample_missing <- nrow(m01)

alldata <- alldata[!alldata$diag_sys %in% '', ] #Removes the blanks
analysed_sample <- nrow(alldata)



## Create new columns for mood disorders and "case" group
alldata$dep <- grepl("Depression", alldata$diag_psych, ignore.case = T)
alldata$pmdd <- grepl("Premenstrual dysphoric disorder", alldata$diag_psych, ignore.case = T)
alldata$bipolar <- grepl("Bipolar disorder", alldata$diag_psych, ignore.case = T)
alldata$anx <- grepl("Anxiety", alldata$diag_psych, ignore.case = T)
alldata$ocd <- grepl("Obsessive Compulsive Disorder", alldata$diag_psych, ignore.case = T)
alldata$ptsd <- grepl("Post Traumatic Stress Disorder", alldata$diag_psych, ignore.case = T)
alldata$anxiety <- ifelse(alldata$anx == T |
                            alldata$ocd == T |
                            alldata$ptsd == T,
                          TRUE, FALSE) 
alldata$depression <- ifelse(alldata$dep == T |
                               alldata$pmdd == T,
                             TRUE, FALSE) #
## Create new columns for psych_disorders and missing data from systems diagnosis
alldata$psych_disorders <- grepl("Mental health conditions", alldata$diag_sys, ignore.case = T)
alldata$do_not_know_diag <- grepl("Do not know", alldata$diag_sys, ignore.case = T)
alldata$prefer_not_to_answer_diag <- grepl("Prefer not to answer", alldata$diag_sys, ignore.case = T)
alldata$none_of_the_above_diag <- grepl("None of the above", alldata$diag_sys, ignore.case = T)
alldata$other_not_listed_diag <- grepl("Other not listed", alldata$diag_sys, ignore.case = T)
#Create new columns for psychiatric medications
alldata$antidepressant_all <- grepl("antidepressant", alldata$psych_meds, ignore.case = T)
alldata$antipsychotic_all <- grepl("antipsychotic", alldata$psych_meds, ignore.case = T)
alldata$lithium <- grepl("lithium", alldata$psych_meds, ignore.case = T)
alldata$valproic_acid <- grepl("valproic acid", alldata$psych_meds, ignore.case = T)
alldata$other_mood_stabiliser <- grepl("other mood stabilising medication", alldata$psych_meds, ignore.case = T)
alldata$pregabalin <- grepl("pregabalin", alldata$psych_meds, ignore.case = T)
alldata$sleeping_pill <- grepl("sleeping pills", alldata$psych_meds, ignore.case = T)
alldata$benzodiazepine <- grepl("benzodiazepine", alldata$psych_meds, ignore.case = T)
alldata$beta_blocker <- grepl("blocker", alldata$psych_meds, ignore.case = T)
alldata$do_not_know_meds <- grepl("do not know", alldata$psych_meds, ignore.case = T)
alldata$prefer_not_to_answer_meds <- grepl("prefer not to answer", alldata$psych_meds, ignore.case = T)
alldata$none_of_the_above_meds <- grepl("none of the above", alldata$psych_meds, ignore.case = T)
alldata$other_not_listed_meds <- grepl("other not listed", alldata$psych_meds, ignore.case = T)

alldata$psych_meds_new <- ifelse(alldata$antidepressant_all == FALSE &
                                   alldata$antipsychotic_all == FALSE &
                                   alldata$lithium == FALSE &
                                   alldata$valproic_acid == FALSE &
                                   alldata$other_mood_stabiliser == FALSE &
                                   alldata$pregabalin == FALSE &
                                   alldata$sleeping_pill == FALSE &
                                   alldata$benzodiazepine == FALSE &
                                   alldata$beta_blocker == FALSE &
                                   alldata$do_not_know_meds == FALSE &
                                   alldata$prefer_not_to_answer_meds == FALSE &
                                   alldata$none_of_the_above_meds == FALSE & 
                                   alldata$other_not_listed_meds == FALSE,
                                 FALSE, TRUE)

alldata$control <- ifelse ((alldata$psych_disorders == FALSE  & 
                              alldata$do_not_know_diag == FALSE &
                              alldata$prefer_not_to_answer_diag == FALSE  &  
                              alldata$other_not_listed_diag == FALSE) |
                             alldata$none_of_the_above_diag == TRUE, 
                           TRUE, FALSE)

# Create a group that identifies ppts who claimed not to have mental illness but who were prescribed regular psychotropics for their mental health
alldata$psych_meds_con <- ifelse (alldata$psych_meds_new == TRUE &
                                    alldata$control == TRUE,
                                  TRUE, FALSE)


# Vectorise the groups into a single variable including all mood disorders in a hierarchy and control group
# Missing data has already been removed so the following variables are already binary as "True" or "False"
alldata$mooddisorder[alldata$bipolar == TRUE] <- 'Bipolar'
alldata$mooddisorder[alldata$depression == TRUE & alldata$bipolar == FALSE] <- 'Depression'
alldata$mooddisorder[alldata$anxiety == TRUE & alldata$depression == FALSE & alldata$bipolar == FALSE] <- 'Anxiety'
alldata$mooddisorder[alldata$control == TRUE] <- 'Comparison'



### Missing data counts for baseline characteristics estimates
#Create data frame that can be reused
write.csv(alldata, "alldata_02.csv")

#reread alldata
alldata <- read.csv("alldata_02.csv")

# Remove missing data from mooddisorder
alldata <- alldata[!alldata$mooddisorder %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% c('',NA, TRUE), ] # This group is removed from all further analysis as they do not fit in any group


total_for_baseline_characteristics <- nrow(alldata)

total_for_baseline_characteristics_by_group <- table(alldata$mooddisorder)

total_for_baseline_characteristics_missing_02 <- total_sample - total_for_baseline_characteristics


### Missing data counts for prevalence estimates
write.csv(alldata, "alldata_03.csv")

alldata <- read.csv("alldata_03.csv")

# Define exclusion values
age_exclude <- c('', NA, 'Prefer not to answer', 'Other not listed', 'Do not know')
sex_exclude <- c('', NA, 'Prefer not to answer', 'Other not listed', 'Do not know')

# Identify rows with excluded age or sex values
excluded_cases <- alldata[alldata$age %in% age_exclude | alldata$sex %in% sex_exclude, ]

total_for_prevalence_missing_by_group <- table(excluded_cases$mooddisorder)



alldata <- read.csv("alldata_03.csv")

alldata <- alldata[!alldata$age %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
alldata <- alldata[!alldata$sex %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]

total_for_prevalence <- nrow(alldata)

total_for_prevalence_by_group <- table(alldata$mooddisorder)

total_for_prevalence_missing_02 <- total_for_baseline_characteristics - total_for_prevalence



###Missing data counts for logreg
# Label blanks as NA
alldata[alldata == ""] <- NA


alldata$mooddisorder <- as.factor(alldata$mooddisorder)


#Vectorise sex
alldata$sex[alldata$sex == "Female"] <- 'Female'
alldata$sex[alldata$sex == "Male" | alldata$sex == "Intersex"] <- 'Not female'


# Vectorise ethicity 
alldata$bangladeshi <- grepl("Bangladeshi", alldata$ethnicity, ignore.case = T) 
alldata$indian <- grepl("indian", alldata$ethnicity, ignore.case = T) 
alldata$pakistani <- grepl("pakistani", alldata$ethnicity, ignore.case = T) 
alldata$other_asian <- grepl("Any other Asian", alldata$ethnicity, ignore.case = T) 
alldata$african <- grepl("african", alldata$ethnicity, ignore.case = T) 
alldata$caribbean <- grepl("caribbean", alldata$ethnicity, ignore.case = T) 
alldata$other_black <- grepl("Any other Black", alldata$ethnicity, ignore.case = T) 
alldata$white_asian <- grepl("White and Asian", alldata$ethnicity, ignore.case = T) 
alldata$white_african <- grepl("White and Black African", alldata$ethnicity, ignore.case = T) 
alldata$white_caribbean <- grepl("White and Black Caribbean", alldata$ethnicity, ignore.case = T) 
alldata$other_mixed <- grepl("Any other mixed multiple ethnic background", alldata$ethnicity, ignore.case = T) 
alldata$other <- grepl("Other", alldata$ethnicity) 
alldata$chinese <- grepl("chinese", alldata$ethnicity, ignore.case = T) 
alldata$arab <- grepl("arab", alldata$ethnicity, ignore.case = T) 
alldata$white_british <- grepl("English", alldata$ethnicity, ignore.case = T) 
alldata$white_gypsy <- grepl("Gypsy", alldata$ethnicity, ignore.case = T) 
alldata$white_irish <- grepl("Irish", alldata$ethnicity, ignore.case = T) 
alldata$white_polish <- grepl("Polish", alldata$ethnicity, ignore.case = T) 
alldata$white_other <- grepl("Any other white background", alldata$ethnicity, ignore.case = T) 

alldata$ethnicity[alldata$white_british == TRUE | alldata$white_gypsy == TRUE | alldata$white_irish == TRUE | alldata$white_polish == TRUE | alldata$white_other == TRUE] <- 'White'
alldata$ethnicity[alldata$bangladeshi == TRUE | alldata$indian == TRUE | alldata$pakistani == TRUE] <- 'South Asian'
alldata$ethnicity[alldata$arab == TRUE |alldata$other_asian == TRUE | alldata$chinese == TRUE |alldata$other == TRUE] <- 'Any other ethnic background'
alldata$ethnicity[alldata$african == TRUE | alldata$caribbean == TRUE | alldata$other_black == TRUE] <- 'Black'
alldata$ethnicity[alldata$white_asian == TRUE | alldata$white_african == TRUE | alldata$white_caribbean == TRUE | alldata$other_mixed == TRUE] <-'Mixed or multiple heritage ethnic background'


#Create vector called "degree"
alldata$alevels <- grepl("A levels", alldata$edu_qual, ignore.case = T)
alldata$cse <- grepl("CSEs or equivalent", alldata$edu_qual, ignore.case = T)
alldata$nvq <- grepl("NVQ or HND or HNC or equivalent", alldata$edu_qual, ignore.case = T)
alldata$profqual <- grepl("Other professional qualifications eg: nursing, teaching", alldata$edu_qual, ignore.case = T)
alldata$gcse <- grepl("gcses", alldata$edu_qual, ignore.case = T)
alldata$degree_01 <- grepl("College or University degree", alldata$edu_qual, ignore.case = T)
alldata$none_edu <- grepl("None of the above", alldata$edu_qual, ignore.case = T)

alldata$degree <- ifelse(alldata$degree_01 == T,
                         TRUE, FALSE)

# Vectorise household income
alldata$less_than_eighteen <- grepl("Less than", alldata$housing_income, ignore.case = T)
alldata$eighteen_to_thirtyone <- grepl("18,000 to", alldata$housing_income, ignore.case = T)
alldata$thirtyone_to_fiftytwo <- grepl("31,000 to", alldata$housing_income, ignore.case = T)
alldata$fiftytwo_to_onehundred <- grepl("52,000 to", alldata$housing_income, ignore.case = T)
alldata$greater_than_onehundred <- grepl("Greater than", alldata$housing_income, ignore.case = T)

alldata$housing_income[alldata$less_than_eighteen == TRUE] <- 'Less than £18,000'
alldata$housing_income[alldata$eighteen_to_thirtyone == TRUE] <- '£18,000 to £30,999'
alldata$housing_income[alldata$thirtyone_to_fiftytwo == TRUE] <- '£31,000 to £51,999'
alldata$housing_income[alldata$fiftytwo_to_onehundred == TRUE] <- '£52,000 to £100,000'
alldata$housing_income[alldata$greater_than_onehundred == TRUE] <- 'More than £100,000'


#Vectorise work_status
alldata$employed <- grepl("In paid employment or self-employed", alldata$work_status, ignore.case = T)
alldata$retired <- grepl("Retired", alldata$work_status, ignore.case = T)
alldata$looking_after_home <- grepl("Looking after home", alldata$work_status, ignore.case = T)
alldata$sickness_disability <- grepl("Unable to work because of sickness or disability", alldata$work_status, ignore.case = T)
alldata$voluntary_work <- grepl("Doing unpaid or voluntary work", alldata$work_status, ignore.case = T)
alldata$student <- grepl("Full or part-time student", alldata$work_status, ignore.case = T)
alldata$paid_leave <- grepl("On paid leave", alldata$work_status, ignore.case = T)
alldata$unpaid_carer <- grepl("Unpaid carer", alldata$work_status, ignore.case = T)

alldata$work_status <- ifelse(alldata$employed == T |
                                alldata$student == T |
                                alldata$paid_leave == T,
                              "Currently in employment or education", "Not currently in employment or education")


#Vectorise smoking
alldata$never_smoker <- grepl("I have not used any of these tobacco products", alldata$tobacco_even_once, ignore.case = T)
alldata$prefer_not_to_answer_tobacco <- grepl("prefer not to answer", alldata$tobacco_even_once, ignore.case = T)
alldata$cigarettes <- grepl("cigarettes", alldata$smoke_reg, ignore.case = T)
alldata$cigars <- grepl("cigars", alldata$smoke_reg, ignore.case = T)
alldata$vapes <- grepl("vaped", alldata$smoke_reg, ignore.case = T)
alldata$pipe <- grepl("tobacco pipe", alldata$smoke_reg, ignore.case = T)
alldata$shisha <- grepl("shisha", alldata$smoke_reg, ignore.case = T)
alldata$chewing_tobacco <- grepl("chewing tobacco", alldata$smoke_reg, ignore.case = T)
alldata$prefer_not_to_answer_smoking <- grepl("prefer not to answer", alldata$smoke_reg, ignore.case = T)
alldata$not_regular_smoking <- grepl("I have not used any of these tobacco products", alldata$smoke_reg, ignore.case = T)

alldata$smoking[alldata$never_smoker == TRUE | alldata$not_regular_smoking == TRUE | alldata$vapes == TRUE | alldata$chewing_tobacco == TRUE | alldata$shisha == TRUE] <- 'Never regularly smoked cigarettes, cigars or tobacco pipes'
alldata$smoking[alldata$cigarettes == TRUE | alldata$cigars == TRUE | alldata$pipe == TRUE] <- 'History of regular cigarette, cigar or tobacco pipe smoking'
alldata$smoking[alldata$prefer_not_to_answer_smoking == TRUE | alldata$prefer_not_to_answer_tobacco == TRUE] <- 'Prefer not to answer'



# Vectorise alcohol_curr
alldata$alcohol_curr[alldata$alcohol_curr == "Daily or almost daily" | alldata$alcohol_curr == "Three or four times a week"]  <- 'Drinks alcohol more frequently than once or twice per week'
alldata$alcohol_curr[alldata$alcohol_curr == "Never"] <- 'Never drinks alcohol' 
alldata$alcohol_curr[alldata$alcohol_curr == "One to three times a month" | alldata$alcohol_curr == "Special occasions only"| alldata$alcohol_curr == "Once or twice a week"] <- 'Drinks alcohol once or twice a week or less frequently'


#Vectorise chronotype
alldata$sleep_chronotype[alldata$sleep_chronotype == "Definitely an \'evening\' person" | alldata$sleep_chronotype == "More an 'evening' than a 'morning' person"] <- 'Evening person'
alldata$sleep_chronotype[alldata$sleep_chronotype == "Definitely a \'morning\' person" | alldata$sleep_chronotype == "More a 'morning' than 'evening' person"] <- 'Morning person'



#Vectorise activity_type
alldata$walking <- grepl("walking for pleasure", alldata$activity_type, ignore.case = T)
alldata$other_exercises <- grepl("other exercises", alldata$activity_type, ignore.case = T)
alldata$strenuous_sports <- grepl("strenuous sports", alldata$activity_type, ignore.case = T)
alldata$light_diy <- grepl("light diy", alldata$activity_type, ignore.case = T)
alldata$heavy_diy <- grepl("heavy diy", alldata$activity_type, ignore.case = T)
alldata$no_activity <- grepl("none of the above", alldata$activity_type, ignore.case = T)
alldata$prefer_not_to_answer_activity <- grepl("prefer not to answer", alldata$activity_type, ignore.case = T)

alldata$activity_type[alldata$no_activity == TRUE] <- 'No physical activity'
alldata$activity_type[alldata$light_diy == TRUE | alldata$walking == TRUE | alldata$heavy_diy == TRUE | alldata$other_exercises == TRUE] <- 'Low to medium level physical activity' 
alldata$activity_type[alldata$strenuous_sports == TRUE] <- 'High level physical activity' 


## Clean up coding of relevant sociodemographic covariates
alldata[alldata == ""] <- NA
alldata$age[alldata$age==""] <- NA
alldata$ethnicity[alldata$ethnicity=="Prefer not to answer"] <- NA
alldata$sex[alldata$sex=="Prefer not to answer"] <- NA
alldata$housing_income[alldata$housing_income=="Prefer not to answer"] <- NA
alldata$housing_income[alldata$housing_income=="Do not know"] <- NA
alldata$smoking[alldata$smoking=="Prefer not to answer"] <- NA
alldata$alcohol_curr[alldata$alcohol_curr=="Prefer not to answer"] <- NA
alldata$alcohol_curr[alldata$alcohol_curr=="Do not know"] <- NA
alldata$activity_type[alldata$activity_type=="Prefer not to answer"] <- NA
alldata$activity_type[alldata$activity_type==""] <- NA
alldata$sleep_chronotype[alldata$sleep_chronotype=="Prefer not to answer"] <- NA
alldata$sleep_chronotype[alldata$sleep_chronotype=="Do not know"] <- NA
alldata$degree[alldata$degree ==""] <- NA
alldata$angina[alldata$angina ==""] <- NA
alldata$cad[alldata$cad ==""] <-NA
alldata$mi[alldata$mi ==""] <- NA
alldata$chf[alldata$chf ==""] <-NA
alldata$stroke[alldata$stroke ==""] <- NA
alldata$hypertension[alldata$hypertension ==""] <-NA
alldata$hypercholesterolaemia[alldata$hypercholesterolaemia ==""] <- NA
alldata$t2dm[alldata$t2dm == ""] <- NA
alldata$obese[alldata$obese == ""] <- NA

write.csv(alldata, "alldata_04.csv")

##### ISOLATE INCOMPLETE CASES ONLY ######
#Remove complete cases. This creates a sample that should contain cases that are removed for the complete cases only logistic regression. Outputs can then be compared between complete and incomplete cases
alldata <- alldata[!complete.cases(alldata[, c("age", "ethnicity", "sex", "housing_income", "smoking", "alcohol_curr", "activity_type", "sleep_chronotype", "degree", "mooddisorder", "angina", "cad", "mi", "chf", "stroke", "hypertension", "hypercholesterolaemia", "t2dm", "obese")]), ]

total_for_logreg_missing_by_group <- table(alldata$mooddisorder)

alldata <- read.csv("alldata_04.csv")


##### ISOLATE COMPLETE CASES ONLY ######
alldata <- alldata[complete.cases(alldata[, c("age", "ethnicity", "sex", "housing_income", "smoking", "alcohol_curr", "activity_type", "sleep_chronotype", "degree", "mooddisorder", "angina", "cad", "mi", "chf", "stroke", "hypertension", "hypercholesterolaemia", "t2dm", "obese")]), ]

total_for_logreg <- nrow(alldata)

total_for_logreg_by_group <- table(alldata$mooddisorder)

total_for_logreg_missing_02 <- total_for_prevalence - total_for_logreg




missing_total <- rbind(total_sample, analysed_sample, analysed_sample_missing, total_for_baseline_characteristics, total_for_baseline_characteristics_missing_02, total_for_prevalence, total_for_prevalence_missing_02, total_for_logreg, total_for_logreg_missing_02)
missing_by_group <- rbind(total_for_baseline_characteristics_by_group, total_for_prevalence_by_group, total_for_prevalence_missing_by_group, total_for_logreg_by_group, total_for_logreg_missing_by_group)

write.csv(missing_total, "missing_total.csv")
write.csv(missing_by_group, "missing_by_group.csv")


