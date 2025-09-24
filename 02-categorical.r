## Switch to R kernel
alldata <- read.csv("alldata_clean.csv")

library(dplyr)

# Remove missing data from mooddisorder
alldata <- alldata[!alldata$mooddisorder %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% c('',NA, TRUE), ] # This group is removed from all further analysis as they do not fit in any group

# Label blanks as NA
alldata[alldata == ""] <- NA

## Convert categorical variables to factors
factorcols <- c(
  "sex", "ethnicity",
  "diag_endo",
  "diag_psych",  "diag_cvd", "smoke_reg",
  "tobacco_even_once",
  "alcohol_curr", "sleep_chronotype", 
  "housing_income", "psych_meds",
  "activity_type"
)

alldata[factorcols] <- lapply(alldata[factorcols], as.factor)
rm(factorcols)

#Create new vectors to allow for comparing each affective disorder group with comparison
alldata$bi_con[alldata$bipolar == TRUE] <- "Bipolar"
alldata$bi_con[alldata$control == TRUE] <- "Comparison"

alldata$dep_con[alldata$depression == TRUE] <- "Depression"
alldata$dep_con[alldata$control == TRUE] <- "Comparison"

alldata$anx_con[alldata$anxiety == TRUE] <- "Anxiety"
alldata$anx_con[alldata$control == TRUE] <- "Comparison"


write.csv(alldata, "alldata_cat.csv")



### Outcomes for non-mutually exclusive variables need to be recoded and data frames created out of each categorical variable ###


### SEX ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$sex %in% c('',NA, "Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Checks to see how many data are missing in that variable
missing_sex_total <- nrow(alldata_01)
missing_sex_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_sex <- nrow(alldata_02)
prop_missing_sex_total <- (missing_sex_total / total_n_missing_sex) *100

alldata <- alldata_02[!alldata_02$sex %in% c('',NA, "Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Remove missing data

## Create new columns for sex
alldata$sex[alldata$sex == "Female"] <- 'Female'
alldata$sex[alldata$sex == "Male" | alldata$sex == "Intersex"] <- 'Not female'


# Count the sample for the variable
n_sex <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(sex_n = sum(!is.na(alldata$sex)), sex_n_by_group = sum(!is.na(sex)))
df1 <- t(n_sex)

# Create a crosstab with mooddisorder and sex
sex_xtabs <- table(alldata$mooddisorder, alldata$sex)
sex_t <- t(sex_xtabs)
df2 <- sex_t

pc2 <- round(100*prop.table(df2,2),2) 

sex_chi_tab_bi <- table(alldata$bi_con, alldata$sex)
sex_chi_tab_dep <- table(alldata$dep_con, alldata$sex)
sex_chi_tab_anx <- table(alldata$anx_con, alldata$sex)

sex_chi_bi <- chisq.test(sex_chi_tab_bi)
sex_chi_dep <- chisq.test(sex_chi_tab_dep)
sex_chi_anx <- chisq.test(sex_chi_tab_anx)

# Extract p-values
sex_p_values <- c(sex_chi_bi$p.value, sex_chi_dep$p.value, sex_chi_anx$p.value)

# Adjust for multiple comparisons
sex_p_adj <- p.adjust(sex_p_values, method = "bonferroni")

# Create a named vector for clarity
names(sex_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")



### ETHNICITY ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$ethnicity %in% c('',NA,"Prefer not to answer","Do not know"), ] # Checks to see how many data are missing in that variable
missing_ethnicity_total <- nrow(alldata_01)
missing_ethnicity_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_ethnicity <- nrow(alldata_02)
prop_missing_ethnicity_total <- (missing_ethnicity_total / total_n_missing_ethnicity) *100

alldata <- alldata_02[!alldata_02$ethnicity %in% c('',NA,"Prefer not to answer","Do not know"), ] # Remove missing data

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


n_eth <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(ethnicity_n = sum(!is.na(alldata$ethnicity)), ethnicity_n_by_group = sum(!is.na(ethnicity)))
df3 <- t(n_eth)

ethnicity_xtabs <- table(alldata$mooddisorder, alldata$ethnicity)
ethnicity_t <- t(ethnicity_xtabs)
df4 <- ethnicity_t

pc4 <- round(100*prop.table(df4,2),2) 

#chi_ethnicity <- chisq.test(ethnicity_xtabs)

ethnicity_chi_tab_bi <- table(alldata$bi_con, alldata$ethnicity)
ethnicity_chi_tab_dep <- table(alldata$dep_con, alldata$ethnicity)
ethnicity_chi_tab_anx <- table(alldata$anx_con, alldata$ethnicity)

ethnicity_chi_bi <- chisq.test(ethnicity_chi_tab_bi)
ethnicity_chi_dep <- chisq.test(ethnicity_chi_tab_dep)
ethnicity_chi_anx <- chisq.test(ethnicity_chi_tab_anx)

# Extract p-values
ethnicity_p_values <- c(ethnicity_chi_bi$p.value, ethnicity_chi_dep$p.value, ethnicity_chi_anx$p.value)

# Adjust for multiple comparisons
ethnicity_p_adj <- p.adjust(ethnicity_p_values, method = "bonferroni")

# Create a named vector for clarity
names(ethnicity_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")



### PAST OR CURRENT REGULAR SMOKING ###


alldata_03 <- read.csv("alldata_cat.csv")

smoke_t <- ifelse(alldata_03$tobacco_even_once == "Prefer not to answer" |
                    alldata_03$smoke_reg == "Prefer not to answer" ,
                  TRUE, FALSE)
missing_smoke_reg_total <- table(smoke_t) #This is provided in a separate csv file - TRUE answers are the numbers missing
smoke_g <-table(alldata_03$mooddisorder, smoke_t)
missing_smoke_reg_by_group <- t(smoke_g) # TRUE answers are the numbers missing


alldata_04 <- read.csv("alldata_cat.csv")

total_n_missing_smoke_reg <- nrow(alldata_04)
prop_missing_smoke_total <- (missing_smoke_reg_total / total_n_missing_smoke_reg) *100 # The TRUE percentage refers to the proportion missing

alldata <- alldata_04


#Create a new "smoking" column, including never smokers from "tobacco_even_once"
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

alldata <- alldata[!alldata$smoking %in% c('',NA,"Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Remove missing data


n_smr <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(smoking_n = sum(!is.na(alldata$smoking)), smoking_n_by_group = sum(!is.na(smoking)))
df5 <- t(n_smr)

smoking_xtabs <- table(alldata$mooddisorder, alldata$smoking)
smoking_t <- t(smoking_xtabs)
df6 <- smoking_t

pc6 <- round(100*prop.table(df6,2),2) 


smoking_chi_tab_bi <- table(alldata$bi_con, alldata$smoking)
smoking_chi_tab_dep <- table(alldata$dep_con, alldata$smoking)
smoking_chi_tab_anx <- table(alldata$anx_con, alldata$smoking)

smoking_chi_bi <- chisq.test(smoking_chi_tab_bi)
smoking_chi_dep <- chisq.test(smoking_chi_tab_dep)
smoking_chi_anx <- chisq.test(smoking_chi_tab_anx)

# Extract p-values
smoking_p_values <- c(smoking_chi_bi$p.value, smoking_chi_dep$p.value, smoking_chi_anx$p.value)

# Adjust for multiple comparisons
smoking_p_adj <- p.adjust(smoking_p_values, method = "bonferroni")

# Create a named vector for clarity
names(smoking_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")






### ALCOHOL CURRENT ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$alcohol_curr %in% c('',NA,"Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Checks to see how many data are missing in that variable
missing_alcohol_curr_total <- nrow(alldata_01)
missing_alcohol_curr_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_alcohol_curr <- nrow(alldata_02)
prop_missing_alcohol_curr_total <- (missing_alcohol_curr_total / total_n_missing_alcohol_curr) *100

alldata <- alldata_02[!alldata_02$alcohol_curr %in% c('',NA,"Prefer not to answer", "None of the above","Do not know","Other not listed"), ] # Remove missing data

alldata$alcohol_curr[alldata$alcohol_curr == "Daily or almost daily" | alldata$alcohol_curr == "Three or four times a week"]  <- 'Drinks alcohol more frequently than once or twice per week'
alldata$alcohol_curr[alldata$alcohol_curr == "Never"] <- 'Never drinks alcohol' 
alldata$alcohol_curr[alldata$alcohol_curr == "One to three times a month" | alldata$alcohol_curr == "Special occasions only"| alldata$alcohol_curr == "Once or twice a week"] <- 'Drinks alcohol once or twice a week or less frequently'


n_alc <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(alcohol_curr_n = sum(!is.na(alldata$alcohol_curr)), alcohol_curr_n_by_group = sum(!is.na(alcohol_curr)))
df7 <- t(n_alc)

alcohol_curr_xtabs <- table(alldata$mooddisorder, alldata$alcohol_curr)
alcohol_curr_t <- t(alcohol_curr_xtabs)
df8 <- alcohol_curr_t

pc8 <- round(100*prop.table(df8,2),2) 

alcohol_curr_chi_tab_bi <- table(alldata$bi_con, alldata$alcohol_curr)
alcohol_curr_chi_tab_dep <- table(alldata$dep_con, alldata$alcohol_curr)
alcohol_curr_chi_tab_anx <- table(alldata$anx_con, alldata$alcohol_curr)

alcohol_curr_chi_bi <- chisq.test(alcohol_curr_chi_tab_bi)
alcohol_curr_chi_dep <-chisq.test(alcohol_curr_chi_tab_dep)
alcohol_curr_chi_anx <-chisq.test(alcohol_curr_chi_tab_anx)

# Extract p-values
alcohol_curr_p_values <- c(alcohol_curr_chi_bi$p.value, alcohol_curr_chi_dep$p.value, alcohol_curr_chi_anx$p.value)

# Adjust for multiple comparisons
alcohol_curr_p_adj <- p.adjust(alcohol_curr_p_values, method = "bonferroni")

# Create a named vector for clarity
names(alcohol_curr_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")





### EDUCATIONAL QUALIFICATIONS ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$edu_qual %in% c('',NA,"Prefer not to answer","Do not know","Other not listed"), ] # Checks to see how many data are missing in that variable
missing_edu_qual_total <- nrow(alldata_01)
missing_edu_qual_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_edu_qual <- nrow(alldata_02)
prop_missing_edu_qual_total <- (missing_edu_qual_total / total_n_missing_edu_qual) *100

alldata <- alldata_02[!alldata_02$edu_qual %in% c('',NA,"Prefer not to answer","Do not know","Other not listed"), ] # Remove missing data

## Create new columns for educational qualifications
# TRUE if edu_qual contains the strings "College or University degree"
alldata$degree_01 <- grepl("College or University degree", alldata$edu_qual, ignore.case = T)

alldata$degree <- ifelse(alldata$degree_01 == T,
                         TRUE, FALSE)

n_edq <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(degree_n = sum(!is.na(alldata$degree)), edu_qual_n_by_group = sum(!is.na(degree)))
df9 <- t(n_edq)

degree_xtabs <- table(alldata$mooddisorder, alldata$degree)
df10 <- t(degree_xtabs)

pc10 <- round(100*prop.table(df10,2),2) 

degree_chi_tab_bi <- table(alldata$bi_con, alldata$degree)
degree_chi_tab_dep <- table(alldata$dep_con, alldata$degree)
degree_chi_tab_anx <- table(alldata$anx_con, alldata$degree)

degree_chi_bi <- chisq.test(degree_chi_tab_bi)
degree_chi_dep <-chisq.test(degree_chi_tab_dep)
degree_chi_anx <-chisq.test(degree_chi_tab_anx)

# Extract p-values
degree_p_values <- c(degree_chi_bi$p.value, degree_chi_dep$p.value, degree_chi_anx$p.value)

# Adjust for multiple comparisons
degree_p_adj <- p.adjust(degree_p_values, method = "bonferroni")

# Create a named vector for clarity
names(degree_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")



### ACTIVITY ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$activity_type %in% c('',NA,"Prefer not to answer","Do not know","Other not listed"), ] # Checks to see how many data are missing in that variable
missing_activity_type_total <- nrow(alldata_01)
missing_activity_type_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_activity_type <- nrow(alldata_02)
prop_missing_activity_type_total <- (missing_activity_type_total / total_n_missing_activity_type) *100

alldata <- alldata_02[!alldata_02$activity_type %in% c('',NA,"Prefer not to answer","Do not know","Other not listed"), ] # Remove missing data

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

n_act <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(activity_n = sum(!is.na(alldata$activity_type)), activity_n_by_group = sum(!is.na(activity_type)))
df11 <- t(n_act)

activity_type_xtabs <- table(alldata$mooddisorder, alldata$activity_type)
df12 <- t(activity_type_xtabs)

pc12 <- round(100*prop.table(df12,2),2) 

activity_type_chi_tab_bi <- table(alldata$bi_con, alldata$activity_type)
activity_type_chi_tab_dep <- table(alldata$dep_con, alldata$activity_type)
activity_type_chi_tab_anx <- table(alldata$anx_con, alldata$activity_type)

activity_type_chi_bi <- chisq.test(activity_type_chi_tab_bi)
activity_type_chi_dep <-chisq.test(activity_type_chi_tab_dep)
activity_type_chi_anx <-chisq.test(activity_type_chi_tab_anx)

# Extract p-values
activity_type_p_values <- c(activity_type_chi_bi$p.value, activity_type_chi_dep$p.value, activity_type_chi_anx$p.value)

# Adjust for multiple comparisons
activity_type_p_adj <- p.adjust(activity_type_p_values, method = "bonferroni")

# Create a named vector for clarity
names(activity_type_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")




### HOUSEHOLD INCOME ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$housing_income %in% c('',NA,"Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Checks to see how many data are missing in that variable
missing_housing_income_total <- nrow(alldata_01)
missing_housing_income_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_housing_income <- nrow(alldata_02)
prop_missing_housing_income_total <- (missing_housing_income_total / total_n_missing_housing_income) *100

alldata <- alldata_02[!alldata_02$housing_income %in% c('',NA,"Prefer not to answer", "None of the above","Do not know","Other not listed"), ] # Remove missing data

# Create new columns to generate a binary variable for those earning <£52,000 and those earning more
alldata$less_than_eighteen <- grepl("Less than", alldata$housing_income, ignore.case = T)
alldata$eighteen_to_thirtyone <- grepl("18,000 to", alldata$housing_income, ignore.case = T)
alldata$thirtyone_to_fiftytwo <- grepl("31,000 to", alldata$housing_income, ignore.case = T)
alldata$fiftytwo_to_onehundred <- grepl("52,000 to", alldata$housing_income, ignore.case = T)
alldata$greater_than_onehundred <- grepl("Greater than", alldata$housing_income, ignore.case = T)

#alldata$housing_income[alldata$less_than_eighteen == TRUE | alldata$eighteen_to_thirtyone == TRUE] <- 'Less than £31,000'
#alldata$housing_income[alldata$fiftytwo_to_onehundred == TRUE | alldata$greater_than_onehundred == TRUE | alldata$thirtyone_to_fiftytwo == TRUE] <- 'More than £31,000'

alldata$housing_income[alldata$less_than_eighteen == TRUE] <- 'Less than £18,000'
alldata$housing_income[alldata$eighteen_to_thirtyone == TRUE] <- '£18,000 to £30,999'
alldata$housing_income[alldata$thirtyone_to_fiftytwo == TRUE] <- '£31,000 to £51,999'
alldata$housing_income[alldata$fiftytwo_to_onehundred == TRUE] <- '£52,000 to £100,000'
alldata$housing_income[alldata$greater_than_onehundred == TRUE] <- 'More than £100,000'

n_inc <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(housing_income_n = sum(!is.na(alldata$housing_income)), housing_income_n_by_group = sum(!is.na(housing_income)))
df13 <- t(n_inc)

housing_income_xtabs <- table(alldata$mooddisorder, alldata$housing_income)
housing_income_t <- t(housing_income_xtabs)
df14 <- housing_income_t

pc14 <- round(100*prop.table(df14,2),2) 

housing_income_chi_tab_bi <- table(alldata$bi_con, alldata$housing_income)
housing_income_chi_tab_dep <- table(alldata$dep_con, alldata$housing_income)
housing_income_chi_tab_anx <- table(alldata$anx_con, alldata$housing_income)

housing_income_chi_bi <- chisq.test(housing_income_chi_tab_bi)
housing_income_chi_dep <-chisq.test(housing_income_chi_tab_dep)
housing_income_chi_anx <-chisq.test(housing_income_chi_tab_anx)

# Extract p-values
housing_income_p_values <- c(housing_income_chi_bi$p.value, housing_income_chi_dep$p.value, housing_income_chi_anx$p.value)

# Adjust for multiple comparisons
housing_income_p_adj <- p.adjust(housing_income_p_values, method = "bonferroni")

# Create a named vector for clarity
names(housing_income_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")



### SLEEP CHRONOTYPE ###
alldata_01 <- read.csv("alldata_cat.csv")

alldata_01 <- alldata_01[alldata_01$sleep_chronotype %in% c('',NA,"Prefer not to answer","None of the above","Do not know","Other not listed"), ] # Checks to see how many data are missing in that variable
missing_sleep_chronotype_total <- nrow(alldata_01)
missing_sleep_chronotype_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cat.csv")

total_n_missing_sleep_chronotype <- nrow(alldata_02)
prop_missing_sleep_chronotype_total <- (missing_sleep_chronotype_total / total_n_missing_sleep_chronotype) *100

alldata <- alldata_02[!alldata_02$sleep_chronotype %in% c('',NA,"Prefer not to answer", "None of the above","Do not know","Other not listed"), ] # Remove missing data

alldata$sleep_chronotype[alldata$sleep_chronotype == "Definitely an \'evening\' person" | alldata$sleep_chronotype == "More an 'evening' than a 'morning' person"] <- '"Evening" person'
alldata$sleep_chronotype[alldata$sleep_chronotype == "Definitely a \'morning\' person" | alldata$sleep_chronotype == "More a 'morning' than 'evening' person"] <- '"Morning" person'


n_chr <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(sleep_chronotype_n = sum(!is.na(alldata$sleep_chronotype)), sleep_chronotype_n_by_group = sum(!is.na(sleep_chronotype)))
df15 <- t(n_chr)

sleep_chronotype_xtabs <- table(alldata$mooddisorder, alldata$sleep_chronotype)
sleep_chronotype_t <- t(sleep_chronotype_xtabs)
df16 <- sleep_chronotype_t

pc16 <- round(100*prop.table(df16,2),2) 


sleep_chronotype_chi_tab_bi <- table(alldata$bi_con, alldata$sleep_chronotype)
sleep_chronotype_chi_tab_dep <- table(alldata$dep_con, alldata$sleep_chronotype)
sleep_chronotype_chi_tab_anx <- table(alldata$anx_con, alldata$sleep_chronotype)

sleep_chronotype_chi_bi <- chisq.test(sleep_chronotype_chi_tab_bi)
sleep_chronotype_chi_dep <-chisq.test(sleep_chronotype_chi_tab_dep)
sleep_chronotype_chi_anx <-chisq.test(sleep_chronotype_chi_tab_anx)

# Extract p-values
sleep_chronotype_p_values <- c(sleep_chronotype_chi_bi$p.value, sleep_chronotype_chi_dep$p.value, sleep_chronotype_chi_anx$p.value)

# Adjust for multiple comparisons
sleep_chronotype_p_adj <- p.adjust(sleep_chronotype_p_values, method = "bonferroni")

# Create a named vector for clarity
names(sleep_chronotype_p_adj) <- c("Bipolar vs Comparison", "Depression vs Comparison", "Anxiety vs Comparison")




#Combine 
categorical_data_count <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15, df16)
categorical_data_pc <- rbind(pc2, pc4, pc6, pc8, pc10, pc12, pc14, pc16)
missing_categorical_data <- rbind(missing_sex_total, missing_sex_by_group, 
                                  missing_ethnicity_total, missing_ethnicity_by_group,
                                  missing_alcohol_curr_total, missing_alcohol_curr_by_group, 
                                  missing_edu_qual_total, missing_edu_qual_by_group, 
                                  missing_activity_type_total, missing_activity_type_by_group,
                                  missing_housing_income_total, missing_housing_income_by_group, 
                                  missing_sleep_chronotype_total, missing_sleep_chronotype_by_group)
prop_missing_categorical_data <- rbind(prop_missing_sex_total, prop_missing_ethnicity_total, prop_missing_smoke_total,
                                       prop_missing_alcohol_curr_total, prop_missing_edu_qual_total, prop_missing_activity_type_total, 
                                       prop_missing_housing_income_total, prop_missing_sleep_chronotype_total)
chi_square_by_group <- rbind(sex_chi_bi, sex_chi_dep, sex_chi_anx, 
                             ethnicity_chi_bi, ethnicity_chi_dep, ethnicity_chi_anx, 
                             smoking_chi_bi, smoking_chi_dep,smoking_chi_anx,
                             alcohol_curr_chi_bi, alcohol_curr_chi_dep, alcohol_curr_chi_anx, 
                             degree_chi_bi, degree_chi_dep, degree_chi_anx, 
                             activity_type_chi_bi, activity_type_chi_dep, activity_type_chi_anx, 
                             housing_income_chi_bi, housing_income_chi_dep, housing_income_chi_anx, 
                             sleep_chronotype_chi_bi, sleep_chronotype_chi_dep, sleep_chronotype_chi_anx)
chi_corrected_p_values <- rbind(sex_p_adj, ethnicity_p_adj, smoking_p_adj, alcohol_curr_p_adj, degree_p_adj, activity_type_p_adj, 
                                housing_income_p_adj, sleep_chronotype_p_adj)
missing_smoking <- cbind(missing_smoke_reg_total,missing_smoke_reg_by_group)


#isolate all observed values
chi <- as.data.frame(chi_square_by_group)
total_observed <- as.matrix(chi$observed)
colnames(total_observed) <- ("observed")

#remove columns not used
chi <- as.data.frame(chi_square_by_group)
chi_square_simple <- chi %>%
  select(-observed, -expected, -residuals, -stdres)
chi_square_simple <- as.matrix(chi_square_simple)




#Write .csv files

write.csv(categorical_data_count, file = "categorical_data_count.csv")
write.csv(categorical_data_pc, file = "categorical_data_percentage.csv")
write.csv(missing_categorical_data,file = "categorical_missing_data.csv")
write.csv(prop_missing_categorical_data, file = "categorical_missing_proportions.csv")
write.csv(chi_square_simple, file = "categorical_chi_square_simplified.csv")
write.csv(chi_square_by_group, file = "categorical_chi_square.csv")
write.csv(total_observed, file = "categorical_chi_square_observed.csv")
write.csv(chi_corrected_p_values, file = "categorical_chi_corrected_p_values.csv")
write.csv(missing_smoking, file = "categorical_missing_smoking.csv")


### Export to DNANexus ###
# Switch to Python kernel
# %%bash
# dx upload "categorical_data_count.csv"
# dx upload "categorical_data_percentage.csv"
# dx upload "categorical_missing_data.csv"
# dx upload "categorical_missing_proportions.csv"
# dx upload "categorical_chi_square_simplified.csv"
# dx upload "categorical_chi_corrected_p_values.csv"