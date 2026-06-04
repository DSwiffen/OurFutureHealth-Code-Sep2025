#############################################################################################################
############################ SENSITIVITY ANALYSES FOR PEER REVIEW ###########################################
#############################################################################################################

##### To run after 01.cleaning.r #####

#Read csv
alldata <- read.csv("alldata_clean.csv")

#Load libraries
library(dplyr)
library(tidyr)

## remove mooddisorder variable
alldata2 <- alldata %>%
  select(-mooddisorder)
# Create columns that are TRUE is there is mooddisorder comorbidity
alldata_filtered1 <- alldata2 %>%
  mutate(bip_dep = ifelse(bipolar == TRUE & depression == TRUE, TRUE, FALSE)) %>%
  mutate(bip_anx = ifelse(bipolar == TRUE & anxiety == TRUE, TRUE, FALSE)) %>%
  mutate(dep_anx = ifelse(depression == TRUE & anxiety == TRUE, TRUE, FALSE)) %>%
  mutate(bip_dep_anx = ifelse(bipolar == TRUE & depression == TRUE & anxiety == TRUE, TRUE, FALSE))
## Create a variable that is TRUE for participants with more than one mooddisorder diagnosis
alldata_filtered2 <- alldata_filtered1 %>%
  mutate(mooddis_comorb = ifelse(bip_dep == TRUE | bip_anx == TRUE | dep_anx == TRUE | bip_dep_anx == TRUE, TRUE, FALSE))
# Filter out these participants 
alldata <- alldata_filtered2 %>%
  filter(mooddis_comorb == FALSE)
## Create groups called moodisorder with mutually exclusive participants
alldata <- alldata %>%
  pivot_longer(cols = c(bipolar, depression, anxiety, control),
               names_to = "mooddisorder",
               values_to = "present") %>%
  filter(present == TRUE) %>%
  mutate(mooddisorder = recode(mooddisorder,
                               "control" = "comparison"))
#Create .csv file from clean data with overlapping mooddisorder group
write.csv(alldata, "alldata_exclusive.csv", row.names = F)
