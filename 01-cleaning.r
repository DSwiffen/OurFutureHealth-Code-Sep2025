# Read csv
all_participant <- read.csv("all_outcomes_participant.csv")
all_questionnaire <- read.csv("all_outcomes_questionnaire.csv")

#Merge data
alldata <- merge(all_participant, all_questionnaire, by = "pid")

alldata <- alldata[!alldata$diag_2_m %in% '', ] # Removes blanks and creates a new variable "alldata"

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



## Create new columns for mood disorders and "case" group
# TRUE if diag_psych contains the strings "Anxiety", "Bipolar disorder", or "Depression"
#This will include all participants who have endorsed at least one mood disorder
alldata$dep <- grepl("Depression", alldata$diag_psych, ignore.case = T)
alldata$pmdd <- grepl("Premenstrual dysphoric disorder", alldata$diag_psych, ignore.case = T)
alldata$bipolar <- grepl("Bipolar disorder", alldata$diag_psych, ignore.case = T)
alldata$anx <- grepl("Anxiety", alldata$diag_psych, ignore.case = T)
alldata$ocd <- grepl("Obsessive Compulsive Disorder", alldata$diag_psych, ignore.case = T)
alldata$ptsd <- grepl("Post Traumatic Stress Disorder", alldata$diag_psych, ignore.case = T)
alldata$anxiety <- ifelse(alldata$anx == T |
                            alldata$ocd == T |
                            alldata$ptsd == T,
                          TRUE, FALSE) #Creating a single "anxiety" group containing ocd and ptsd
# This group will include any individual who answered true to ANY of "anxiety", "ocd", or "ptsd"; there will be people in this group who ticked more than one
alldata$depression <- ifelse(alldata$dep == T |
                               alldata$pmdd == T,
                             TRUE, FALSE) # Creating a single "depression" group
# This group will include any individual who answered true to ANY of "depression" or "pmdd"; there will be people in this group who ticked more than one


## Create new columns for psych_disorders and missing data from systems diagnosis
alldata$psych_disorders <- grepl("Mental health conditions", alldata$diag_sys, ignore.case = T)
alldata$do_not_know_diag <- grepl("Do not know", alldata$diag_sys, ignore.case = T)
alldata$prefer_not_to_answer_diag <- grepl("Prefer not to answer", alldata$diag_sys, ignore.case = T)
alldata$none_of_the_above_diag <- grepl("None of the above", alldata$diag_sys, ignore.case = T)
alldata$other_not_listed_diag <- grepl("Other not listed", alldata$diag_sys, ignore.case = T)


#Create new columns for psychiatric medications
#TRUE if medicat_psych_1_M contains strings "antidepressant", "antipsychotic", "lithium", "valproic acid", "other mood stabilising medication"
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

### Non mutually exclusive variables requiring prevalence outputs relabelled to include meaningful outcomes ###

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



#Create .csv file from clean data
write.csv(alldata, "alldata_clean.csv", row.names = F)
