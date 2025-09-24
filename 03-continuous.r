## Launch dplyr
library(dplyr)


alldata <- read.csv("alldata_clean.csv")


# Remove missing data from mooddisorder
alldata <- alldata[!alldata$mooddisorder %in% c('',NA, 'Prefer not to answer', 'Other not listed', 'Do not know'), ]
# Remove ppts who claim not to have a mental illness but are taking psychotropic medications
alldata <- alldata[!alldata$psych_meds_con %in% c('',NA, TRUE), ] # This group is removed from all further analysis as they do not fit in any group


#Save new data frame
write.csv(alldata, "alldata_cont.csv")


### AGE ###
alldata_01 <- read.csv("alldata_cont.csv")

# Count the number of missing data in the variable (total and by group)
alldata_01 <- alldata_01[alldata_01$age %in% c('', "Prefer not to answer", "None of the above", "Do not know", "Other not listed") | is.na(alldata_01$age), ]
missing_age_total <- nrow(alldata_01)
missing_age_by_group <- table(alldata_01$mooddisorder)

alldata_02 <- read.csv("alldata_cont.csv")

#Calculate the proportions of missing data
total_n <- nrow(alldata_02) 
prop_missing_age_total <- (missing_age_total / total_n) * 100 

#Remove missing data points
alldata <- alldata_02[!alldata_02$age %in% c('', NA, "Prefer not to answer", "None of the above", "Do not know", "Other not listed"), ] # Remove missing data

#Ensure age is numeric
alldata$age <- as.numeric(alldata$age)

#Check for statistically significant differences across the four groups
anova_age_01 <- aov(age ~ mooddisorder, data = alldata)
anova_age <- summary(anova_age_01)
tukey_age <- TukeyHSD(anova_age_01)
tukey_anova <- c(tukey_age)

#Create an output for age including the number of observations per quartile
op1 <- alldata %>%
  group_by(mooddisorder) %>%
  summarise(age_n = sum(!is.na(alldata$age)), age_n_by_group = sum(!is.na(age)),  
            age_Q1 = quantile(age, probs = c(0.25),na.rm=TRUE), age_median = median(age, na.rm=TRUE),  age_Q3 = quantile(age, probs = c(0.75), na.rm=TRUE),
            n_Q1 = sum(age == age_Q1, na.rm = TRUE), n_median = sum(age == age_median, na.rm = TRUE), n_Q3 = sum(age == age_Q3, na.rm = TRUE)
  )

missing_age_data <- rbind(missing_age_total, missing_age_by_group)

write.csv(op1, "continuous_data.csv")
write.csv(missing_age_data, "continuous_missing_data.csv")
write.csv(tukey_anova, "continuous_anova_tukey.csv")



### Export to DNANexus ###
# Switch to Python kernel
# %%bash
# dx upload "continuous_data.csv"
# dx upload "continuous_missing_data.csv"
# dx upload "continuous_anova_tukey.csv"