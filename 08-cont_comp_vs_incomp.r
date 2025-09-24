############################### COMPLETE CASES ##################################

library(dplyr)
library(tidyr)

rm(list = ls())

## Switch to R kernel
alldata <- read.csv("alldata_clean.csv")

# Remove missing data from mooddisorder
alldata <- alldata[!alldata$mooddisorder %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% c('',NA, TRUE), ] # This group is removed from all further analysis as they do not fit in any group

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


##### CODE FOR CLEAN CASES ONLY ######
#Remove cases that have NA for any variable. This creates a "clean" sample that should contain no missing data points that we can run an unadjusted logistic regression on
complete <- alldata[complete.cases(alldata[, c("age", "ethnicity", "sex", "housing_income", "smoking", "alcohol_curr", "activity_type", "sleep_chronotype", "degree", "mooddisorder", "angina", "cad", "mi", "chf", "stroke", "hypertension", "hypercholesterolaemia", "t2dm", "obese")]), ]


write.csv(complete, "alldata_cat_comp.csv")

alldata <- read.csv("alldata_cat_comp.csv")

alldata$age <- as.numeric(alldata$age)

#Create an output for age including the number of observations per quartile
op1 <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(age_n = sum(!is.na(alldata$age)), age_n_by_group = sum(!is.na(age)),  
            age_Q1 = quantile(age, probs = c(0.25),na.rm=TRUE), age_median = median(age, na.rm=TRUE),  age_Q3 = quantile(age, probs = c(0.75), na.rm=TRUE),
            n_Q1 = sum(age == age_Q1, na.rm = TRUE), n_median = sum(age == age_median, na.rm = TRUE), n_Q3 = sum(age == age_Q3, na.rm = TRUE)
  )

write.csv(op1, "cont_comp_data.csv")




############################### INCOMPLETE CASES ##################################

library(dplyr)
library(tidyr)

rm(list = ls())

## Switch to R kernel
alldata <- read.csv("alldata_clean.csv")

# Remove missing data from mooddisorder
alldata <- alldata[!alldata$mooddisorder %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% c('',NA, TRUE), ] # This group is removed from all further analysis as they do not fit in any group

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

alldata$housing_income[alldata$less_than_eighteen == TRUE | alldata$eighteen_to_thirtyone == TRUE] <- 'Less than £31,000'
alldata$housing_income[alldata$fiftytwo_to_onehundred == TRUE | alldata$greater_than_onehundred == TRUE | alldata$thirtyone_to_fiftytwo == TRUE] <- 'More than £31,000'


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


##### CODE FOR INCOMPLETE CASES ONLY ######
#Remove complete cases. This creates a sample that should contain cases that are removed for the complete cases only logistic regression. Outputs can then be compared between complete and incomplete cases
incomplete <- alldata[!complete.cases(alldata[, c("age", "ethnicity", "sex", "housing_income", "smoking", "alcohol_curr", "activity_type", "sleep_chronotype", "degree", "mooddisorder", "angina", "cad", "mi", "chf", "stroke", "hypertension", "hypercholesterolaemia", "t2dm", "obese")]), ]


write.csv(incomplete, "alldata_cat_incomp.csv")

alldata_01 <- read.csv("alldata_cat_incomp.csv")

alldata <- alldata_01[!alldata_01$age %in% c('',NA, "Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Remove missing data

#Ensure age is numeric
alldata$age <- as.numeric(alldata$age)

#Create an output for age including the number of observations per quartile
op1 <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(age_n = sum(!is.na(alldata$age)), age_n_by_group = sum(!is.na(age)),  
            age_Q1 = quantile(age, probs = c(0.25),na.rm=TRUE), age_median = median(age, na.rm=TRUE),  age_Q3 = quantile(age, probs = c(0.75), na.rm=TRUE),
            n_Q1 = sum(age == age_Q1, na.rm = TRUE), n_median = sum(age == age_median, na.rm = TRUE), n_Q3 = sum(age == age_Q3, na.rm = TRUE)
  )

write.csv(op1, "cont_incomp_data.csv")



### Export to DNANexus ###
# Switch to Python kernel
# %%bash
# dx upload "cont_comp_data.csv"
# dx upload "cont_incomp_data.csv"