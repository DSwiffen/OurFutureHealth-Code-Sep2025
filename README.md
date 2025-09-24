# OurFutureHealth-Code-Sep2025
Analysis code for a study assessing cardiometabolic comorbidity in affective disorders using data from the Our Future Health cohort. 

PROJECT SUMMARY

The goal of this project was to understand the association between cardiometabolic disorders and affective disorders in the Our Future Health cohort. To realise this, individuals with bipolar disorder, depressive disorders and anxiety disorders were compared to those with no history of mental health problems and both the prevalence and odds ratios of cardiometabolic disorders were calculated in each group and compared. Odds ratios were adjusted for a range of relevant sociodemographic, lifestyle and health-related covariates to quantify association of cardiometabolic comorbidity in affective disorders.  

ASSOCIATED PUBLICATION

Submitted.

CONTACT

For any questions, please contact Dr Duncan Swiffen (dswiffen@ed.ac.uk).

DATA DICTIONARY

A full description of variables used in the analysis can be found in the Our Future Health data dictionary, at this link: https://research.ourfuturehealth.org.uk/data-and-cohort/

GENERAL APPROACH

Three mutually-exclusive affective disorder groups - bipolar disorder, depressive disorders and anxiety disorders- are created and baseline sociodemographic, lifestyle and health-related factors are compared with a comparison group made up of participants with no mental health problems. Standardised total prevalence and sex-stratified prevalence of any cardiometabolic disorder, individual cardiometabolic disorders and cardiometabolic comorbidity are calculated within each group. Odds ratios are calculated via logistic regression models adjusted for relevant covariates. Further analysis is completed to establish the extent and the impact of missing data on the results and conclusions. 

DESCRIPTION OF SCRIPTS

Scripts are described below in the order in which they are run.

01-cleaning.r cleans the raw data imported from Our Future Health into a format that is usable for the intended analysis. This involves merging two .csv files that were provided from the DNANexus Table Exporter tool, reassigning the columns names, creating an “age” column out of year of consent and year of birth, a single combined mooddisorder column containing four affective disorder groups, and a cardiometabolic disorder column containing all cardiometabolic disorders and obesity, which was taken from BMI calculated from height and weight data. 

02-categorical.r contains the cleaning and analysis (creating count and percentage data, missing data counts and percentages, and comparisons using Chi-squared test with Bonferroni correction) for categorical variables, namely sex, ethnicity, smoking status, current alcohol use, chronotype, activity levels, educational attainment and household income.

03-continuous.r contains the cleaning and analysis (creating median and upper and lower quartile values, missing data counts and percentages, and comparisons using ANOVA with Tukey post-hoc test) for continuous variables, namely age. 

04-prev_stand_by_sex.r contains code that creates a data frame for the European Standard Population, modified by combining the two oldest age categories. The script then includes data cleaning, grouping of participants by ESP age categories, and creation of a multimorbidity category. Age- and sex-standardised prevalence with 95% confidence intervals are estimated for each cardiometabolic disorder using the ESP. This code groups the data by sex to generate sex-stratified output. 

05-prev_stand_all.r includes data cleaning, grouping of participants by ESP age categories, and creation of a multimorbidity category, followed by estimation of age- and sex-standardised prevalence with 95% confidence intervals for each cardiometabolic disorder. Estimates are for the whole sample as this code does not group by sex.

06-logreg.r contains data cleaning and logistic regression analysis. This code generates odds ratios with 95% confidence intervals of each cardiometabolic disorder in the bipolar, depressive disorders and anxiety disorders groups compared to the comparison group. The three logistic regression models are i) unadjusted, ii) adjusted for age and sex, and iii) fully adjusted for all included demographic variables. 

07-missing_data.r contains repeated code from the other scripts, but with an emphasis on determining the total counts of data and the number of missing data points at each stage of analysis. The code is used to calculate the total sample, sample used in analysis, sample used to compare baseline characteristics,  sample used to estimate prevalence and the sample used in logistic regression, along with the number of missing data points between each stage of the analysis and the total sample at each stage, separated by affective disorder group. 

08-cont_comp_vs_incomp.r contains code that initially cleans and creates composite factors for the variables of interest, and then separates out the total sample into “complete” cases and “incomplete” cases (i.e. cases that have no missing data and those that are missing data in at least one of the included variables). Outputs for the continuous variables (in this case, only age) are then calculated for the complete and incomplete cases separately. 

09-cat_comp_vs_incomp.r contains code that initially cleans and creates composite factors for the variables of interest, and then separates out the total sample into “complete” cases and “incomplete” cases. Outputs for the categorical variables are then calculated for the complete and incomplete cases separately.
