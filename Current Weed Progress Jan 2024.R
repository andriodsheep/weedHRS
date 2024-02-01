library(haven) 
library(stringr)
library(tidyverse)
library(table1)
library(Hmisc)

####DATA MERGING AND CLEANING----

#Weed Data from 2020 Module 10
weed2020 <- read_sas("module10_data.sas7bdat")
names(weed2020)
#keep only the necessary columns
weed2020 <- weed2020[, c("HHID", "PN", "RV655")]
#drop rows that are NAs for weed question
weed2020 <- weed2020[!is.na(weed2020$RV655), ]
#combine HHID and PN
weed2020 <- weed2020 %>%
  mutate(HHIDPN = as.numeric(paste0(HHID, PN))) %>%
  select(-c(HHID, PN))  

#operationalized weed variable
weed2020 <- weed2020 %>%
  mutate(mj_use_mod10 = case_when(
    RV655 == 1 ~ 5,
    RV655 %in% 2:5 ~ 1,
    RV655 %in% 8:9 ~ NA_real_,
    TRUE ~ RV655
  ))

base_data<-read.csv("q6_data.csv")
names(base_data)

cannabis_module<-read_sas("weed.sas7bdat")
# create HHIDPN variable in cannabis data for join
# remove leading 0 from HHID, then attach HHID and PN
cannabis_module_clean <- cannabis_module %>%
  mutate(HHID=as.numeric(str_remove(HHID, "^0+")),
         HHIDPN = as.numeric(paste0(HHID,PN))) %>%
  select(-c(HHID,PN))

#merge the 3 datasets
combined_weed_cannabis <- full_join(weed2020, cannabis_module_clean, by = "HHIDPN")
q6_data_complete <- left_join(combined_weed_cannabis, base_data, by = "HHIDPN")


#drop unnecessary id columns
q6_data_complete <- q6_data_complete %>%
  select(-X, -HHID, -PN, -RAHHIDPN)

# double check join worked as intended
# all HHIDPNs in cannabis data should have a match in the base data file - sum should be 1661
sum(cannabis_module_clean$HHIDPN %in% base_data$HHIDPN)
sum(cannabis_module_clean$HHIDPN %in% q6_data_complete$HHIDPN)

# response breakdown should be identical between these tables
table(cannabis_module_clean$QV402)
table(q6_data_complete$QV402)
table(q6_data_complete$mj_use_mod10)

#filter out HHIDPN that are not in the weed modules
weed_merge_clean <- q6_data_complete %>%
  filter(!is.na(QV402) | !is.na(mj_use_mod10))

table(q6_data_complete$QV402)
table(weed_merge_clean$QV402)
table(q6_data_complete$mj_use_mod10)
table(weed_merge_clean$mj_use_mod10)

#Write Another CSV file that contains only weed HHIDPN----
write.csv(weed_merge_clean, "weed_merged")
merged_data <- read.csv("weed_merged")

#drop all unused variables
q6_clean = subset(merged_data, select = c(X, HHIDPN, RAGENDER, R14AGEY_B, R15AGEY_B, RAEDUC, H14INPOV, H15INPOV, 
                                          QC128, QC129, QC130, RC128, RC129, RC130, 
                                          R14SMOKEV, R14SMOKEN, R15SMOKEV, R15SMOKEN, 
                                          R14CANCR, R15CANCR, QN365, RN365, RARACEM, RAHISPAN,
                                          R14CESD, R15CESD, NLB041A, NLB041B, NLB041C, NLB041D, NLB041E,
                                          QLB035C1, QLB035C2, QLB035C3, QLB035C4, QLB035C5,
                                          RLB035C1, RLB035C2, RLB035C3, RLB035C4, RLB035C5, QV402, QV403, mj_use_mod10))

#rename remaining variables
q6_clean <- q6_clean %>%
  rename("hh_id" = "HHIDPN",
         "sex" = "RAGENDER",
         "age_18" = "R14AGEY_B",
         "age_20" = "R15AGEY_B",
         "educ" = "RAEDUC",
         "hh_poverty_18" = "H14INPOV",
         "hh_poverty_20" = "H15INPOV",
         "alc_ever_18" = "QC128",
         "alc_daywk_18" = "QC129",
         "alc_drinkday_18" = "QC130",
         "alc_ever_20" = "RC128",
         "alc_daywk_20" = "RC129",
         "alc_drinkday_20" = "RC130",
         "tob_ever_18" = "R14SMOKEV",
         "tob_cur_18" = "R14SMOKEN",
         "tob_ever_20" = "R15SMOKEV",
         "tob_cur_20" = "R15SMOKEN",
         "cancer_18" = "R14CANCR",
         "cancer_20" = "R15CANCR",
         "antidep_18" = "QN365",
         "antidep_20" = "RN365",
         "race" = "RARACEM",
         "ethnicity" = "RAHISPAN",
         "cesd_18" = "R14CESD",
         "cesd_20" = "R15CESD",
         "bai_fearworst_20" = "RLB035C1",
         "bai_nervous_20" = "RLB035C2",
         "bai_tremble_20" = "RLB035C3",
         "bai_feardie_20" = "RLB035C4",
         "bai_faint_20" = "RLB035C5",
         "bai_fearworst_18" = "QLB035C1",
         "bai_nervous_18" = "QLB035C2",
         "bai_tremble_18" = "QLB035C3",
         "bai_feardie_18" = "QLB035C4",
         "bai_faint_18" = "QLB035C5",
         "bai_fearworst_12" = "NLB041A",
         "bai_nervous_12" = "NLB041B",
         "bai_tremble_12" = "NLB041C",
         "bai_feardie_12" = "NLB041D",
         "bai_faint_12" = "NLB041E",
         "mj_ever_18" = "QV402",
         "mj_pastyr_18" = "QV403")


####DERIVATIONS AND VARIABLE LABELING

###Outcomes

##Marijuana Use----
#Marijuana Use, combined 2018 and 2020
table(q6_clean$mj_use_mod10)
sum(is.na(q6_clean$mj_use_mod10))

q6_clean <- q6_clean %>%
  mutate(mj_use_combined = case_when(
    mj_pastyr_18 == 1 | mj_use_mod10 == 1 ~ "Use in Past Year",
    mj_pastyr_18 == 5 | mj_use_mod10 == 5 | mj_ever_18 == 5 ~ "Not Used in Past Year",
    TRUE ~ "Other/Unknown"
  ))

table(q6_clean$mj_use_combined)

q6_clean <- q6_clean %>%
  mutate(
    mj_year = ifelse(!is.na(mj_use_mod10), "2020", "2018")
  )
table(q6_clean$mj_year)

q6_clean <- q6_clean %>%
  mutate(
    mj_year_f = factor(if_else(mj_year == 2020, 1, 0), 
                       levels = c(0, 1), 
                       labels = c("2018", "2020"))
  )


###Exposures

##Depression (CESD)----
q6_clean$cesd <- ifelse(q6_clean$mj_year == 2018, q6_clean$cesd_18,
                        ifelse(q6_clean$mj_year == 2020, q6_clean$cesd_20, NA))
table(q6_clean$cesd)

#CESD greater than 4 is depression
q6_clean <- q6_clean %>%
  mutate(
    depress_4 = factor(if_else(cesd >= 4, 1, 0), 
                       levels = c(0, 1), 
                       labels = c("Not Depressed", "Depressed"))
  )
table(q6_clean$depress_4)

##Anxiety (BAI)----
#total BAI (2020, 2018, 2012) - Nathan
q6_clean <- q6_clean %>%
  mutate(
    bai_total_20 = bai_fearworst_20 + bai_nervous_20 + bai_tremble_20 + bai_feardie_20 + bai_faint_20
  )
q6_clean <- q6_clean %>%
  mutate(
    bai_total_18 = bai_fearworst_18 + bai_nervous_18 + bai_tremble_18 + bai_feardie_18 + bai_faint_18
  )
q6_clean <- q6_clean %>%
  mutate(
    bai_total_12 = bai_fearworst_12 + bai_nervous_12 + bai_tremble_12 + bai_feardie_12 + bai_faint_12
  )
table(q6_clean$bai_total_12)
table(q6_clean$bai_total_18)
table(q6_clean$bai_total_20)

#total BAI for whichever year has data for 2018 outcome
q6_clean <- q6_clean %>%
  mutate(
    bai_total_18_nonmiss = case_when(
      bai_total_18 > 0 ~ bai_total_18,
      bai_total_12 > 0 ~ bai_total_12
    )
  )

#total BAI for whichever year has data for 2020 outcome
q6_clean <- q6_clean %>%
  mutate(
    bai_total_20_nonmiss = case_when(
      bai_total_20 > 0 ~ bai_total_20,
      bai_total_18 > 0 ~ bai_total_18,
      bai_total_12 > 0 ~ bai_total_12
    )
  )
table(q6_clean$bai_total_18_nonmiss)
table(q6_clean$bai_total_20_nonmiss)


#identifier of which year is being used for BAI
q6_clean <- q6_clean %>%
  mutate(
    bai_year = case_when(
      bai_total_20 > 0 ~ 2020,
      bai_total_18 > 0 ~ 2018,
      bai_total_12 > 0 ~ 2012
    )
  )

table(q6_clean$bai_year)

#BAI for regressions
q6_clean$bai_total <- ifelse(q6_clean$mj_year == 2018, q6_clean$bai_total_18_nonmiss,
                             ifelse(q6_clean$mj_year == 2020, q6_clean$bai_total_20_nonmiss, NA))
table(q6_clean$bai_total)


#Should we include a column for BAI >= 12?
q6_clean <- q6_clean %>%
  mutate(
    anx_12 = factor(if_else(bai_total >= 12, 1, 0),
                    levels = c(0,1),
                    labels = c("Not Anxious", "Anxious"))
  )
table(q6_clean$anx_12)


#Anxiety & Depression Composite----

# q6_clean <- q6_clean %>%
#   mutate(anx_depr = case_when(
#     cesd < 4 & bai_total < 12 ~ 1,
#     cesd < 4 & bai_total >= 12 ~ 2,
#     cesd >= 4 & bai_total < 12 ~ 3,
#     cesd >= 4 & bai_total >= 12 ~ 4
#   )) %>%
#   mutate(anx_depr = factor(anx_depr, levels = c(1, 2, 3, 4), labels = c("None", "Anxiety", "Depression", "Both Anxiety and Depression")))
# 
# table(q6_clean$anx_depr)

q6_clean <- q6_clean %>%
  mutate(anx_depr = case_when(
    depress_4 == "Not Depressed" & (anx_12 == "Not Anxious" | is.na(anx_12)) ~ 1,
    depress_4 == "Depressed" & (anx_12 == "Not Anxious" | is.na(anx_12)) ~ 2,
    depress_4 == "Depressed" & anx_12 == "Anxious" ~ 3
    )) %>%
  mutate(anx_depr = factor(anx_depr, levels = c(1, 2, 3), labels = c("None", "Depression", "Depression and Anxiety")))
table(q6_clean$anx_depr)


###Confounders

##Sex----
q6_clean$sex <- factor(q6_clean$sex)
q6_clean$sex <- factor(q6_clean$sex, levels = c(1, 2), labels = c("Male", "Female"))
table(q6_clean$sex)

##Age Groups----
#define 2018 vs 2020
q6_clean$age <- ifelse(q6_clean$mj_year == 2018, q6_clean$age_18,
                       ifelse(q6_clean$mj_year == 2020, q6_clean$age_20, NA))

#create groups
q6_clean$age_group <- cut(
  q6_clean$age,
  breaks = c(49, 59, 69, 79, Inf),
  labels = c("50-59", "60-69", "70-79", "80+"),
  right = FALSE
)

q6_clean$age_group <- factor(q6_clean$age_group)

table(q6_clean$age_group)

##Education Levels----
q6_clean <- q6_clean %>%
  mutate(educ = case_when(
    educ %in% c(2, 3) ~ 2,  # Combine categories 2 and 3
    TRUE ~ educ             # Keep other categories as they are
  )) %>%
  mutate(educ_cat = factor(educ, labels = c("Less than High School", "High School/GED", "Some College", "College+")))

q6_clean$educ_cat <- factor(q6_clean$educ_cat)
table(q6_clean$educ_cat)

##Poverty Status----
#income categories, hh poverty
q6_clean$hh_poverty <- ifelse(q6_clean$mj_year == 2018, q6_clean$hh_poverty_18,
                              ifelse(q6_clean$mj_year == 2020, q6_clean$hh_poverty_20, NA))

q6_clean$hh_poverty[is.na(q6_clean$hh_poverty)] <- 0

q6_clean$hh_poverty <- factor(
  q6_clean$hh_poverty,
  levels = c(0, 1),
  labels = c("Above poverty threshold", "Below poverty threshold")
)

table(q6_clean$hh_poverty)


##Alcohol Use----
#2018
q6_clean$alc_18 <- with(q6_clean, ifelse(alc_ever_18 == 3, "Never drinkers",
                                         ifelse(is.na(alc_daywk_18) | alc_daywk_18 == 0, "Former drinkers",
                                                ifelse((sex == 2 & (alc_daywk_18 * alc_drinkday_18) >= 8) | 
                                                         (sex == 1 & (alc_daywk_18 * alc_drinkday_18) >= 15), 
                                                       "Heavy drinkers",
                                                       "Low to moderate drinkers"))))



#2020
q6_clean$alc_20 <- with(q6_clean, ifelse(alc_ever_20 == 3, "Never drinkers",
                                         ifelse(is.na(alc_daywk_20) | alc_daywk_20 == 0, "Former drinkers",
                                                ifelse((sex == 2 & (alc_daywk_20 * alc_drinkday_20) >= 8) | 
                                                         (sex == 1 & (alc_daywk_20 * alc_drinkday_20) >= 15), 
                                                       "Heavy drinkers",
                                                       "Low to moderate drinkers"))))
#alcohol use for regressions
q6_clean$Alcohol_Use <- ifelse(q6_clean$mj_year == 2018, q6_clean$alc_18,
                               ifelse(q6_clean$mj_year == 2020, q6_clean$alc_20, NA))

q6_clean$Alcohol_Use <- factor(q6_clean$Alcohol_Use, 
                               levels = c("Never drinkers", "Former drinkers", "Low to moderate drinkers", "Heavy drinkers"))


q6_clean$Alcohol_Use <- factor(q6_clean$Alcohol_Use)

table(q6_clean$Alcohol_Use)

##Tobacco Use----
#2018
q6_clean <- q6_clean %>%
  mutate(tob_18 = case_when(
    q6_clean$tob_cur_18 == 1 ~ 3,
    q6_clean$tob_cur_18 == 0 & q6_clean$tob_ever_18 == 1 ~ 2,
    q6_clean$tob_cur_18 == 0 & q6_clean$tob_ever_18 == 0 ~ 1
  ))


table(q6_clean$tob_18)

#2020
q6_clean <- q6_clean %>%
  mutate(tob_20 = case_when(
    q6_clean$tob_cur_20 == 1 ~ 3,
    q6_clean$tob_cur_20 == 0 & q6_clean$tob_ever_20 == 1 ~ 2,
    q6_clean$tob_cur_20 == 0 & q6_clean$tob_ever_20 == 0 ~ 1
  ))

table(q6_clean$tob_20)

#tobacco for regressions
q6_clean$tob <- ifelse(q6_clean$mj_year == 2018, q6_clean$tob_18,
                       ifelse(q6_clean$mj_year == 2020, q6_clean$tob_20, NA))


q6_clean$tob <- factor(q6_clean$tob, 
                       levels = c(1, 2, 3), 
                       labels = c("Non-smokers", 
                                  "Former smokers", 
                                  "Current smokers"))

table(q6_clean$tob)

##Cancer----
q6_clean$cancer_18 <- factor(q6_clean$cancer_18)
q6_clean$cancer_20 <- factor(q6_clean$cancer_20)
q6_clean$cancer <- ifelse(q6_clean$mj_year == 2018, q6_clean$cancer_18,
                          ifelse(q6_clean$mj_year == 2020, q6_clean$cancer_20, NA))

q6_clean$cancer <- factor(q6_clean$cancer, 
                       levels = c(1, 2), 
                       labels = c("No cancer", 
                                  "Cancer"))


table(q6_clean$cancer)



###Effect Modifiers
##Race & Ethnicity----
q6_clean$race_eth = ifelse(q6_clean$ethnicity == 1, 3,  # Hispanic
                           ifelse(q6_clean$race == 1, 1,  # White
                                  ifelse(q6_clean$race == 2, 2,  # Black
                                         ifelse(q6_clean$race == 3, 4, NA))))  # Other or Missing


q6_clean$race_eth <- factor(q6_clean$race_eth)
q6_clean$race_eth <- factor(q6_clean$race_eth, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("White non-hispanic", 
                                       "Black non-hispanic", 
                                       "Hispanic", 
                                       "Other non-hispanic"))
table(q6_clean$race_eth)


##Antidepressant Use----

#2018
# Replace -8 with NA
q6_clean$antidep_18[q6_clean$antidep_18 == -8] <- NA

#2020
# Replace -8 with NA
q6_clean$antidep_20[q6_clean$antidep_20 == -8] <- NA

table(q6_clean$antidep_18)
table(q6_clean$antidep_20)


#antidepressant use for regressions
q6_clean$antidep <- ifelse(q6_clean$mj_year == 2018, q6_clean$antidep_18,
                           ifelse(q6_clean$mj_year == 2020, q6_clean$antidep_20, "Yes"))

q6_clean$antidep[is.na(q6_clean$antidep)] <- 5
q6_clean$antidep[q6_clean$antidep != 1] <- 5  # set anything other than 1 to 5

#factor with levels 1 and 5, and labels yes or no
q6_clean$antidep <- factor(q6_clean$antidep, levels = c(1, 5), labels = c("Yes", "No"))

table(q6_clean$antidep)


# TABLE CREATION ----

#Dropping NAs from variables in Table 1
q6_clean <- q6_clean[q6_clean$age >= 50, ] # age
q6_clean <- q6_clean[complete.cases(q6_clean[c("race_eth", "educ_cat", "hh_poverty", "tob", "cancer")]), ] #race and education


#Summary Statistics----
# # Set custom labels for variables OVERRALL
# label(q6_clean$bai_total) <- "Anxiety Score (BAI Total)*"
# label(q6_clean$cesd) <- "Depression Score (CES-D)**"
# label(q6_clean$sex) <- "Sex"
# label(q6_clean$race_eth) <- "Race/Ethnicity"
# label(q6_clean$antidep) <- "Antidepressant Use"
# label(q6_clean$age_group) <- "Age Group"
# label(q6_clean$educ_cat) <- "Education Level"
# label(q6_clean$hh_poverty) <- "Household Poverty Threshold"
# # label(q6_clean$simp_marstat) <- "Marital Status"
# label(q6_clean$Alcohol_Use) <- "Alcohol Use"
# label(q6_clean$tob) <- "Tobacco Use"
# label(q6_clean$cancer) <- "Cancer Status"
# # label(q6_clean$chronic) <- "Chronic Condition"
# label(q6_clean$mj_use_combined) <- "Marijuana use in the past year"
# 
# # create the table with the labels
# # table1_data <- table1(~ bai_total + cesd + sex + race_eth + antidep + age_group + educ_cat + 
# #                         hh_poverty + simp_marstat + Alcohol_Use + tob, 
# #                       data = q6_clean,
# #                       title = "Table 1: Overall Characteristics of Population"
# # )
# table1_data <- table1(~ bai_total + cesd + sex + race_eth + antidep + age_group + educ_cat + 
#                         hh_poverty + Alcohol_Use + tob, 
#                       data = q6_clean,
#                       title = "Table 1: Overall Characteristics of Population"
# )
# 
# print(table1_data)

# Table stratified by if use weed in past year PROBLEM IS CAN'T DO BECAUSE OF MISSING VALUES?----
# Create a new dataset without missing values in mj_pastyr
# new_dataset <- q6_clean[!is.na(q6_clean$mj_use_past_year), ]
# 
# label(new_dataset$bai_total) <- "Anxiety Score (BAI Total)*"
# label(new_dataset$cesd) <- "Depression Score (CES-D)**"
# label(new_dataset$sex) <- "Sex"
# label(new_dataset$race_eth) <- "Race/Ethnicity"
# label(new_dataset$antidep) <- "Antidepressant Use"
# label(new_dataset$region) <- "Geographic Region"
# label(new_dataset$age_group) <- "Age Group"
# label(new_dataset$educ_cat) <- "Education Level"
# label(new_dataset$hh_poverty) <- "Household Poverty Threshold"
# label(new_dataset$simp_marstat) <- "Marital Status"
# label(new_dataset$Alcohol_Use) <- "Alcohol Use"
# label(new_dataset$tob) <- "Tobacco Use"
# label(new_dataset$cancer) <- "Cancer Status"
# label(new_dataset$chronic) <- "Chronic Condition"
# label(new_dataset$anx_depr) <- "Anxiety and/or Depression"
# label(new_dataset$mj_use_combined) <- "Marijuana use in the past year"
# 
# table1(
#   ~ bai_total + cesd + anx_depr + sex + race_eth + age_group + educ_cat + 
#     hh_poverty + simp_marstat + Alcohol_Use + tob | mj_use_past_year,
#   data = new_dataset,
#   caption = "Table 1: Characteristics of Population (marijuana vs. non-marijuana user in the past year)"
# )

# By Anxiety and Depression ----
q6_clean_labels <- q6_clean[!is.na(q6_clean$anx_depr), ]

label(q6_clean_labels$sex) <- "Sex"
label(q6_clean_labels$race_eth) <- "Race/Ethnicity"
label(q6_clean_labels$antidep) <- "Antidepressant Use"
label(q6_clean_labels$age_group) <- "Age Group"
label(q6_clean_labels$educ_cat) <- "Education Level"
label(q6_clean_labels$hh_poverty) <- "Household Poverty Threshold"
# label(q6_clean_labels$simp_marstat) <- "Marital Status"
label(q6_clean_labels$Alcohol_Use) <- "Alcohol Use"
label(q6_clean_labels$tob) <- "Tobacco Use"
label(q6_clean_labels$cancer) <- "Cancer Status"
# label(q6_clean_labels$chronic) <- "Chronic Condition"
label(q6_clean_labels$anx_depr) <- "Anxiety and/or Depression"
label(q6_clean_labels$antidep) <- "Antidepressant Use"
label(q6_clean_labels$mj_use_combined) <- "Marijuana use in the past year"

# Creating Table 1 using q6_clean_labels
# table1(
#   ~ sex + race_eth + age_group + educ_cat + 
#     hh_poverty + simp_marstat + Alcohol_Use + tob | anx_depr,
#   data = q6_clean_labels,
#   caption = "Table 1: Characteristics of Population by Anxiety and/or Depression"
# )


table1(
  ~ sex + race_eth + age_group + educ_cat + 
    hh_poverty + Alcohol_Use + tob + cancer + antidep | anx_depr,
  data = q6_clean_labels,
  caption = "Table 1: Characteristics of Population by Anxiety and/or Depression"
)



# By CESD only ----
new_dataset1 <- q6_clean[!is.na(q6_clean$cesd), ]
new_dataset1$cesd <- as.factor(new_dataset1$cesd)

label(new_dataset1$sex) <- "Sex"
label(new_dataset1$race_eth) <- "Race/Ethnicity"
label(new_dataset1$antidep) <- "Antidepressant Use"
label(new_dataset1$age_group) <- "Age Group"
label(new_dataset1$educ_cat) <- "Education Level"
label(new_dataset1$hh_poverty) <- "Poverty Status"
label(new_dataset1$Alcohol_Use) <- "Alcohol Use"
label(new_dataset1$tob) <- "Tobacco Use"
label(new_dataset1$cancer) <- "Cancer Status"
label(new_dataset1$anx_depr) <- "Anxiety and/or Depression"
label(new_dataset1$mj_use_combined) <- "Marijuana use in the past year"
label(new_dataset1$cesd) <- "Depression Score (CES-D)"

# Creating Table 1 using new_dataset1
table1(
  ~ sex + race_eth + age_group + educ_cat +
    hh_income_category + simp_marstat + Alcohol_Use + tob | cesd,
  data = new_dataset1,
  caption = "Table 1: Characteristics of Population by Depression Score (CES-D)"
)

# By CESD only Binary ----
new_dataset1 <- q6_clean[!is.na(q6_clean$depress_4), ]
new_dataset1$depress_4 <- as.factor(new_dataset1$depress_4)

label(new_dataset1$sex) <- "Sex"
label(new_dataset1$race_eth) <- "Race/Ethnicity"
label(new_dataset1$antidep) <- "Antidepressant Use"
label(new_dataset1$age_group) <- "Age Group"
label(new_dataset1$educ_cat) <- "Education Level"
label(new_dataset1$hh_poverty) <- "Household Poverty Threshold"
# label(new_dataset1$simp_marstat) <- "Marital Status"
label(new_dataset1$Alcohol_Use) <- "Alcohol Use"
label(new_dataset1$tob) <- "Tobacco Use"
label(new_dataset1$cancer) <- "Cancer Status"
# label(new_dataset1$chronic) <- "Chronic Condition"
label(new_dataset1$anx_depr) <- "Anxiety and/or Depression"
label(new_dataset1$mj_use_combined) <- "Marijuana use in the past year"
# label(new_dataset1$cesd) <- "Depression Score (CES-D)"

# Creating Table 1 using new_dataset1
table1(
  ~ antidep + sex + race_eth + age_group + educ_cat +
    hh_poverty + Alcohol_Use + tob | depress_4,
  data = new_dataset1,
  caption = "Table 1: Characteristics of Population by Depression Score (CES-D)"
)


# Regression Models ####


## unadjusted association with anxiety and depression ####
# need to categorize exposure/outcome as numbers
df_anx_depr <- q6_clean_labels %>%
  mutate(
    anx_depr_num = case_when(
      anx_depr == "None" ~ 0,
      anx_depr == "Depression" ~ 1,
      anx_depr == "Depression and Anxiety" ~ 2
      ),
    mj_use_combined_num = case_when(
      mj_use_combined == "Not Used in Past Year" ~ 0,
      mj_use_combined == "Use in Past Year" ~ 1
      )
    )

# factoring exposure and outcome
df_anx_depr <- df_anx_depr %>%
  mutate(
    anx_depr_num = relevel(factor(anx_depr_num), ref = "0"),
    mj_use_combined_num <- relevel(factor(mj_use_combined_num), ref = "0")
  )

glm.crude <- glm(mj_use_combined_num ~ anx_depr_num, 
                 data = df_anx_depr, 
                 family = binomial())

summary(glm.crude)
glm.crude$coefficients
confint(glm.crude)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude$coefficients)
exp(confint(glm.crude))

table(df_anx_depr$anx_depr, df_anx_depr$mj_use_combined)


# unadjusted association - depression only - Nathan####
#need to categorize exposure/outcome as numbers
df_depr <- new_dataset1 %>%
  mutate(
    depr_num = case_when(
      depress_4 == "Not Depressed" ~ 0,
      depress_4 == "Depressed" ~ 1,
    ),
    mj_use_combined_num = case_when(
      mj_use_combined == "Not Used in Past Year" ~ 0,
      mj_use_combined == "Use in Past Year" ~ 1
    )
  )

# # factoring exposure and outcome
df_depr <- df_depr %>%
  mutate(
    depr_num = relevel(factor(depr_num), ref = "0"),
    mj_use_combined_num <- relevel(factor(mj_use_combined_num), ref = "0")
  )

glm.crude <- glm(mj_use_combined_num ~ depr_num,
                 data = df_depr,
                 family = binomial())

summary(glm.crude)
glm.crude$coefficients
confint(glm.crude)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude$coefficients)
exp(confint(glm.crude))

## adjusted association with anxiety and depression ####
# need to categorize covariates as numeric
df_anx_depr <- df_anx_depr %>%
  mutate(
    sex_num = case_when(
      sex == "Male" ~ 0,
      sex == "Female" ~ 1
    ),
    age_group_num = case_when(
      age_group == "50-59" ~ 0,
      age_group == "60-69" ~ 1,
      age_group == "70-79" ~ 2,
      age_group == "80+" ~ 3,
    ),
    educ_cat_num = case_when(
      educ_cat == "Less than High School" ~ 0,
      educ_cat == "High School/GED" ~ 1,
      educ_cat == "Some College" ~ 2,
      educ_cat == "College+" ~ 3
    ),
    hh_poverty_num = case_when(
      hh_poverty == "Above poverty threshold" ~ 0, 
      hh_poverty == "Below poverty threshold" ~ 1
    ),
    Alcohol_Use_num = case_when(
      Alcohol_Use == "Never drinkers" ~ 0,
      Alcohol_Use == "Former drinkers" ~ 1,
      Alcohol_Use == "Low to moderate drinkers" ~ 2,
      Alcohol_Use == "Heavy drinkers" ~ 3
    ),
    tob_num = case_when(
      tob == "Non-smokers" ~ 0,
      tob == "Former smokers" ~ 1,
      tob == "Current smokers" ~ 2,
    ),
    cancer_num = case_when(
      cancer == "No cancer" ~ 0,
      cancer == "Cancer" ~ 1
    ),
    antidep_num = case_when(
      antidep == "No" ~ 0,
      antidep == "Yes" ~ 1
    ),
    mj_year_num = case_when(
      mj_year == 2018 ~ 0,
      mj_year == 2020 ~ 1
    ),
    race_eth_num = case_when(
      race_eth == "White non-hispanic" ~ 0,
      race_eth == "Black non-hispanic" ~ 1,
      race_eth == "Hispanic" ~ 2,
      race_eth == "Other non-hispanic" ~ 3
    )
  )

# checking coding worked
table(df_anx_depr$sex_num, exclude = NULL)
table(df_anx_depr$age_group_num, exclude = NULL)
table(df_anx_depr$educ_cat_num, exclude = NULL)
table(df_anx_depr$hh_poverty_num, exclude = NULL)
table(df_anx_depr$Alcohol_Use_num, exclude = NULL)
table(df_anx_depr$tob_num, exclude = NULL)
table(df_anx_depr$cancer_num, exclude = NULL)
table(df_anx_depr$mj_year_num, exclude = NULL)
table(df_anx_depr$antidep_num, exclude = NULL)


# factoring covariates
df_anx_depr <- df_anx_depr %>%
  mutate(
    sex_num = relevel(factor(sex_num), ref = "0"),
    age_group_num = relevel(factor(age_group_num), ref = "2"),
    educ_cat_num = relevel(factor(educ_cat_num), ref = "1"),
    hh_poverty_num = relevel(factor(hh_poverty_num), ref = "0"),
    Alcohol_Use_num = relevel(factor(Alcohol_Use_num), ref = "0"),
    tob_num = relevel(factor(tob_num), ref = "0"),
    cancer_num = relevel(factor(cancer_num), ref = "0"),
    mj_year_num = relevel(factor(mj_year_num), ref = "0"),
    antidep_num = relevel(factor(antidep_num), ref = "0")
    )


glm.adjusted <- glm(mj_use_combined_num ~ anx_depr_num + 
                      sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num + cancer_num + mj_year_num, 
                    data = df_anx_depr, 
                    family = binomial())

summary(glm.adjusted)
glm.adjusted$coefficients
confint(glm.adjusted)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted$coefficients)
exp(confint(glm.adjusted))



## adjusted association with depression only ####
# need to categorize covariates as numeric
df_depr <- df_depr %>%
  mutate(
    sex_num = case_when(
      sex == "Male" ~ 0,
      sex == "Female" ~ 1
    ),
    age_group_num = case_when(
      age_group == "50-59" ~ 0,
      age_group == "60-69" ~ 1,
      age_group == "70-79" ~ 2,
      age_group == "80+" ~ 3,
    ),
    educ_cat_num = case_when(
      educ_cat == "Less than High School" ~ 0,
      educ_cat == "High School/GED" ~ 1,
      educ_cat == "Some College" ~ 2,
      educ_cat == "College+" ~ 3
    ),
    hh_poverty_num = case_when(
      hh_poverty == "Above poverty threshold" ~ 0, 
      hh_poverty == "Below poverty threshold" ~ 1
    ),
    Alcohol_Use_num = case_when(
      Alcohol_Use == "Never drinkers" ~ 0,
      Alcohol_Use == "Former drinkers" ~ 1,
      Alcohol_Use == "Low to moderate drinkers" ~ 2,
      Alcohol_Use == "Heavy drinkers" ~ 3
    ),
    tob_num = case_when(
      tob == "Non-smokers" ~ 0,
      tob == "Former smokers" ~ 1,
      tob == "Current smokers" ~ 2,
    ),
    cancer_num = case_when(
      cancer == "No cancer" ~ 0,
      cancer == "Cancer" ~ 1
    ),
    antidep_num = case_when(
      antidep == "No" ~ 0,
      antidep == "Yes" ~ 1
    ),
    mj_year_num = case_when(
      mj_year == 2018 ~ 0,
      mj_year == 2020 ~ 1
    ),
    race_eth_num = case_when(
      race_eth == "White non-hispanic" ~ 0,
      race_eth == "Black non-hispanic" ~ 1,
      race_eth == "Hispanic" ~ 2,
      race_eth == "Other non-hispanic" ~ 3
    )
  )

# checking coding worked
table(df_depr$sex_num, exclude = NULL)
table(df_depr$age_group_num, exclude = NULL)
table(df_depr$educ_cat_num, exclude = NULL)
table(df_depr$hh_poverty_num, exclude = NULL)
table(df_depr$Alcohol_Use_num, exclude = NULL)
table(df_depr$tob_num, exclude = NULL)
table(df_depr$antidep_num, exclude = NULL)
table(df_depr$cancer_num, exclude = NULL)
table(df_depr$mj_year_num, exclude = NULL)
table(df_depr$race_eth_num, exclude = NULL)

# factoring covariates
df_depr <- df_depr %>%
  mutate(
    sex_num = relevel(factor(sex_num), ref = "0"),
    age_group_num = relevel(factor(age_group_num), ref = "2"),
    educ_cat_num = relevel(factor(educ_cat_num), ref = "1"),
    hh_poverty_num = relevel(factor(hh_poverty_num), ref = "0"),
    Alcohol_Use_num = relevel(factor(Alcohol_Use_num), ref = "0"),
    tob_num = relevel(factor(tob_num), ref = "0"),
    antidep_num = relevel(factor(antidep_num), ref = "0"),
    cancer_num = relevel(factor(cancer_num), ref = "0"),
    mj_year_num = relevel(factor(mj_year_num), ref = "0"),
    race_eth_num = relevel(factor(race_eth_num), ref = "0")
  )


##testing for interaction
glm.adjusted <- glm(mj_use_combined_num ~ depr_num + depr_num*antidep_num +
                      sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num, 
                    data = df_depr, 
                    family = binomial())


summary(glm.adjusted)
glm.adjusted$coefficients
confint(glm.adjusted)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted$coefficients)
exp(confint(glm.adjusted))

##no antidepressants unadjusted regression
df_depr_noantidep <- subset(df_depr, df_depr$antidep_num == 0)

glm.crude_noantidep <- glm(mj_use_combined_num ~ depr_num, 
                              data = df_depr_noantidep, 
                              family = binomial())


summary(glm.crude_noantidep)
glm.crude_noantidep$coefficients
confint(glm.crude_noantidep)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude_noantidep$coefficients)
exp(confint(glm.crude_noantidep))

table(df_depr_noantidep$depr_num, df_depr_noantidep$mj_use_combined)

##no antidepressants adjusted regression
df_depr_noantidep <- subset(df_depr, df_depr$antidep_num == 0)

glm.adjusted_noantidep <- glm(mj_use_combined_num ~ depr_num + 
                      sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num + cancer_num + mj_year_num, 
                    data = df_depr_noantidep, 
                    family = binomial())


summary(glm.adjusted_noantidep)
glm.adjusted_noantidep$coefficients
confint(glm.adjusted_noantidep)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted_noantidep$coefficients)
exp(confint(glm.adjusted_noantidep))


##antidepressants unadjusted regression
df_depr_antidep <- subset(df_depr, df_depr$antidep_num == 1)

glm.crude_antidep <- glm(mj_use_combined_num ~ depr_num, 
                                    data = df_depr_antidep, 
                                    family = binomial())


summary(glm.crude_antidep)
glm.crude_antidep$coefficients
confint(glm.crude_antidep)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude_antidep$coefficients)
exp(confint(glm.crude_antidep))

table(df_depr_antidep$depr_num, df_depr_antidep$mj_use_combined)

##antidepressants adjusted regression
glm.adjusted_antidep <- glm(mj_use_combined_num ~ depr_num + 
                                sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num + cancer_num + mj_year_num, 
                              data = df_depr_antidep, 
                              family = binomial())


summary(glm.adjusted_antidep)
glm.adjusted_antidep$coefficients
confint(glm.adjusted_antidep)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted_antidep$coefficients)
exp(confint(glm.adjusted_antidep))


##white non-hispanic unadjusted regression
df_depr_white <- subset(df_depr, df_depr$race_eth_num == 0)

glm.crude_white <- glm(mj_use_combined_num ~ depr_num, 
                         data = df_depr_white, 
                         family = binomial())


summary(glm.crude_white)
glm.crude_white$coefficients
confint(glm.crude_white)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude_white$coefficients)
exp(confint(glm.crude_white))

table(df_depr_white$depr_num, df_depr_white$mj_use_combined)

##white non-hispanic adjusted regression
glm.adjusted_white <- glm(mj_use_combined_num ~ depr_num + 
                              sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num + cancer_num + mj_year_num, 
                            data = df_depr_white, 
                            family = binomial())


summary(glm.adjusted_white)
glm.adjusted_white$coefficients
confint(glm.adjusted_white)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted_white$coefficients)
exp(confint(glm.adjusted_white))

##black non-hispanic unadjusted regression
df_depr_black <- subset(df_depr, df_depr$race_eth_num == 1)

glm.crude_black <- glm(mj_use_combined_num ~ depr_num, 
                       data = df_depr_black, 
                       family = binomial())


summary(glm.crude_black)
glm.crude_black$coefficients
confint(glm.crude_black)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude_black$coefficients)
exp(confint(glm.crude_black))

table(df_depr_black$depr_num, df_depr_black$mj_use_combined)


##black non-hispanic adjusted regression
glm.adjusted_black <- glm(mj_use_combined_num ~ depr_num + 
                            sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num + cancer_num + mj_year_num, 
                          data = df_depr_black, 
                          family = binomial())


summary(glm.adjusted_black)
glm.adjusted_black$coefficients
confint(glm.adjusted_black)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted_black$coefficients)
exp(confint(glm.adjusted_black))

##Hispanic unadjusted regression
df_depr_hispanic <- subset(df_depr, df_depr$race_eth_num == 2)

glm.crude_hispanic <- glm(mj_use_combined_num ~ depr_num, 
                       data = df_depr_hispanic, 
                       family = binomial())


summary(glm.crude_hispanic)
glm.crude_hispanic$coefficients
confint(glm.crude_hispanic)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude_hispanic$coefficients)
exp(confint(glm.crude_hispanic))

table(df_depr_hispanic$depr_num, df_depr_hispanic$mj_use_combined)

##Hispanic adjusted regression
glm.adjusted_hispanic <- glm(mj_use_combined_num ~ depr_num + 
                            sex_num + age_group_num + educ_cat_num + hh_poverty_num + Alcohol_Use_num + tob_num + cancer_num + mj_year_num, 
                          data = df_depr_hispanic, 
                          family = binomial())


summary(glm.adjusted_hispanic)
glm.adjusted_hispanic$coefficients
confint(glm.adjusted_hispanic)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted_hispanic$coefficients)
exp(confint(glm.adjusted_hispanic))

