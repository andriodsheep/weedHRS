library(haven) 
library(stringr)
library(tidyverse)
library(table1)
library(Hmisc)
#Merging ----
base_data<-read.csv("q6_data.csv")
names(base_data)

cannabis_module<-read_sas("weed.sas7bdat")

# create HHIDPN variable in cannabis data for join
# remove leading 0 from HHID, then attach HHID and PN
cannabis_module_clean <- cannabis_module %>%
  mutate(HHID=as.numeric(str_remove(HHID, "^0+")),
         HHIDPN = as.numeric(paste0(HHID,PN))) %>%
  select(-c(HHID,PN))

#merge the 2 datasets
q6_data_complete<-left_join(base_data,cannabis_module_clean)

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

# write out file - replace file path with your own
write.csv(q6_data_complete, "weed_merge.csv")

#read in the merged dataset
weed_merge <- read.csv("weed_merge.csv")

#filter out HHIDPN that are not in the weed module
weed_merged <- weed_merge %>%
  filter(HHIDPN %in% cannabis_module_clean$HHIDPN)
table(cannabis_module_clean$QV402)
table(weed_merged$QV402)

#Write Another CSV file that contains only weed HHIDPN
write.csv(weed_merged, "weed_merged")

#Nathan's Code Cleaning----
merged_data <- read.csv("weed_merged")

#drop all observations where mj questions not answered
merged_data_NA <- merged_data %>% drop_na(QV402)

#drop all unused variables
q6_clean = subset(merged_data_NA, select = c(X, HHIDPN, H14HHRES, R14MSTAT, H14ITOT, H14INPOV, RAGENDER, R14CANCR, RAEDUC, 
                                             QV413, QN365, R14AGEY_B, R14CENREG, R14CENDIV, RARACEM, RAHISPAN, R14CESD, 
                                             QV412, QV402, QV403, QV409, QV410, QV411, QV751, QC128, QC129, QC130, QC131, 
                                             QC116, QC117, QC118, QLB035C1, QLB035C2, QLB035C3, QLB035C4, QLB035C5, 
                                             NLB041A, NLB041B, NLB041C, NLB041D, NLB041E))

#rename remaining variables
q6_clean <- q6_clean %>%
  rename("hh_id" = "HHIDPN",
         "hh_num" = "H14HHRES",
         "marstat" = "R14MSTAT",
         "hh_income" = "H14ITOT",
         "hh_poverty" = "H14INPOV",
         "sex" = "RAGENDER",
         "cancer" = "R14CANCR",
         "educ" = "RAEDUC",
         "mj_rx" = "QV413",
         "antidep" = "QN365",
         "age" = "R14AGEY_B", 
         "region"  = "R14CENREG",
         "division" = "R14CENDIV",
         "race" = "RARACEM",
         "ethnicity" = "RAHISPAN",
         "cesd" = "R14CESD",
         "mj_health" = "QV412",
         "mj_ever" = "QV402",
         "mj_pastyr" = "QV403",
         "mj_freq_max" = "QV409",
         "mj_freq_cur" = "QV410",
         "mj_nosmoke" = "QV411",
         "chronic" = "QV751",
         "alc_ever" = "QC128",
         "alc_daywk" = "QC129",
         "alc_drinkday" = "QC130",
         "alc_binge" = "QC131",
         "tob_ever" = "QC116",
         "tob_cur" = "QC117",
         "tob_cigday" = "QC118",
         "bai_fearworst_18" = "QLB035C1",
         "bai_nervous_18" = "QLB035C2",
         "bai_tremble_18" = "QLB035C3",
         "bai_feardie_18" = "QLB035C4",
         "bai_faint_18" = "QLB035C5",
         "bai_fearworst_12" = "NLB041A",
         "bai_nervous_12" = "NLB041B",
         "bai_tremble_12" = "NLB041C",
         "bai_feardie_12" = "NLB041D",
         "bai_faint_12" = "NLB041E")


#Age Groups----
q6_clean$age_group <- cut(
  q6_clean$age,
  breaks = c(49, 59, 69, 79, 89, Inf),
  labels = c("50-59", "60-69", "70-79", "80-89", "90+"),
  right = FALSE
)
table(q6_clean$age_group)


#Smoking Not Using----
q6_clean$Tobacco_Use <- with(q6_clean, ifelse(tob_ever == 5, "Never",
                                              ifelse(tob_ever == 1 & tob_cur == 5 | tob_cur == 5, "Former",
                                                     ifelse(tob_cur == 1 & tob_cigday < 25 | tob_cigday < 25, "Light",
                                                            ifelse(tob_cur == 1 & tob_cigday >= 25 | tob_cigday >= 25, "Heavy", NA)))))

q6_clean$Tobacco_Use <- factor(q6_clean$Tobacco_Use, 
                               levels = c("Never", "Former", "Light", "Heavy"))
table(q6_clean$Tobacco_Use)


#Nathan Smoking----
q6_clean <- q6_clean %>%
  mutate(tob = case_when(
    q6_clean$tob_cur == 1 & q6_clean$tob_cigday > 13 ~ 4,
    q6_clean$tob_cur == 1 & q6_clean$tob_cigday <= 13 ~ 3,
    q6_clean$tob_cur == 5 & q6_clean$tob_ever == 1 ~ 2,
    q6_clean$tob_cur == 5 | q6_clean$tob_ever == 5 ~ 1
  ))
table(q6_clean$tob)

q6_clean$tob <- factor(q6_clean$tob)
q6_clean$tob <- factor(q6_clean$tob, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("Non-smokers", 
                                  "Former smokers", 
                                  "Light smokers (1-13 cigarettes/day)", 
                                  "Heavy smokers (>13 cigarettes)"))

table(q6_clean$tob)


#Race----
q6_clean$race_eth = ifelse(q6_clean$ethnicity == 1, 3,  # Hispanic
                           ifelse(q6_clean$race == 1, 1,  # White
                                  ifelse(q6_clean$race == 2, 2,  # Black
                                         ifelse(q6_clean$race == 3, 4, NA))))  # Other or Missing
table(q6_clean$race_eth)


#BAI Score----
#total BAI (2018 & 2012) - Nathan
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

#total BAI for whichever year has data
q6_clean <- q6_clean %>%
  mutate(
    bai_total = case_when(
      bai_total_12 > 0 ~ bai_total_12,
      bai_total_18 > 0 ~ bai_total_18
    )
  )
table(q6_clean$bai_total)

#identifier of which year is being used for BAI
q6_clean <- q6_clean %>%
  mutate(
    bai_year = case_when(
      bai_total_12 > 0 ~ 2012,
      bai_total_18 > 0 ~ 2018
    )
  )

table(q6_clean$bai_year)

#Should we include a column for BAI >= 12?
q6_clean <- q6_clean %>%
  mutate(
    bai_12 = if_else(bai_total >= 12, 1, 0)
  )
table(q6_clean$bai_12)


#anxiety and depression composite variable - Nathan----
q6_clean <- q6_clean %>%
  mutate(anx_depr = case_when(
    cesd < 4 & bai_total < 12 ~ 1,
    cesd < 4 & bai_total >= 12 ~ 2,
    cesd >= 4 & bai_total < 12 ~ 3,
    cesd >= 4 & bai_total >= 12 ~ 4
  )) %>%
  mutate(anx_depr = factor(anx_depr, levels = c(1, 2, 3, 4), labels = c("None", "Anxiety", "Depression", "Both Anxiety and Depression")))

table(q6_clean$anx_depr)


table(q6_clean$cesd) #is CESD continuous?
table(q6_clean$antidep) # -8 is no response, make NA


#CESD greater than 4 is depression
q6_clean <- q6_clean %>%
  mutate(
    depress_4 = factor(if_else(cesd >= 4, 1, 0), 
                       levels = c(0, 1), 
                       labels = c("Not Depressed", "Depressed"))
  )
table(q6_clean$depress_4)


#Alcohol Use----
q6_clean$Alcohol_Use <- with(q6_clean, ifelse(alc_ever == 3, "Never drinkers",
                                              ifelse(is.na(alc_daywk) | alc_daywk == 0, "Former drinkers",
                                                     ifelse((sex == 2 & (alc_daywk * alc_drinkday) >= 8) | 
                                                              (sex == 1 & (alc_daywk * alc_drinkday) >= 15), 
                                                            "Heavy drinkers",
                                                            "Low to moderate drinkers"))))

q6_clean$Alcohol_Use <- factor(q6_clean$Alcohol_Use, 
                               levels = c("Never drinkers", "Former drinkers", "Low to moderate drinkers", "Heavy drinkers"))

table(q6_clean$Alcohol_Use)


#Education Levels----
q6_clean <- q6_clean %>%
  mutate(educ = case_when(
    educ %in% c(2, 3) ~ 2,  # Combine categories 2 and 3
    TRUE ~ educ             # Keep other categories as they are
  )) %>%
  mutate(educ_cat = factor(educ, labels = c("Less than High School", "High School/GED", "Some College", "College+")))
table(q6_clean$educ_cat)

#Look at all frequency tables
lapply(q6_clean, table)
names(q6_clean)

#label the categorical variables
#SEX
q6_clean$sex <- factor(q6_clean$sex)
q6_clean$sex <- factor(q6_clean$sex, levels = c(1, 2), labels = c("Male", "Female"))

#RACE/ETHNICITY
q6_clean$race_eth <- factor(q6_clean$race_eth)
q6_clean$race_eth <- factor(q6_clean$race_eth, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("White non-hispanic", 
                                       "Black non-hispanic", 
                                       "Hispanic", 
                                       "Other non-hispanic"))

#ANTIDEP USE
# Replace -8 with NA
q6_clean$antidep[q6_clean$antidep == -8] <- NA
#factor with levels 1 and 5, and labels "Yes" and "No"
q6_clean$antidep <- factor(q6_clean$antidep, levels = c(1, 5), labels = c("Yes", "No"))

#age group ALREADY DONE
q6_clean$age_group <- factor(q6_clean$age_group)

#education ALREADY DONE
q6_clean$educ_cat <- factor(q6_clean$educ_cat)

#MARITAL STATUS
# create a new column with simplified marital status categories
q6_clean$simp_marstat <- factor(
  case_when(
    q6_clean$marstat %in% c(1,3) ~ "Married",
    q6_clean$marstat %in% c(2, 4, 5, 6) ~ "Separated/divorced",
    q6_clean$marstat %in% c(7) ~ "Widowed",
    q6_clean$marstat %in% c(8) ~ "Never married"
  )
)

#ALCOHOL USE ALREADY DONE
q6_clean$Alcohol_Use <- factor(q6_clean$Alcohol_Use)


#Look at data dictionary CAN'T FIND ON HRS
q6_clean$cancer <- factor(q6_clean$cancer)

#chronic condition ALL NAs?
q6_clean$chronic <- factor(q6_clean$chronic)
table(q6_clean$chronic)

#IF USE IN THE PAST YEAR
q6_clean <- q6_clean %>%
  mutate(mj_use_past_year = case_when(
    q6_clean$mj_pastyr == 1 ~ "Use in Past Year",
    q6_clean$mj_ever == 5 | q6_clean$mj_pastyr == 5 ~"Not used in Past Year",
  ))
table(q6_clean$mj_use_past_year)

#income categories, hh poverty
table(q6_clean$hh_poverty)
q6_clean$hh_poverty <- factor(
  q6_clean$hh_poverty,
  levels = c(0, 1),
  labels = c("Above poverty threshold", "Below poverty threshold")
)

#Dropping NAs from variables in Table 1
q6_clean <- q6_clean[q6_clean$age >= 50, ] # age

# tob, race, education, maritial status, poverty
q6_clean <- q6_clean[complete.cases(q6_clean[c("tob", "race_eth", "educ_cat", "simp_marstat", "hh_poverty", "antidep")]), ]


#Summary Statistics----
# Set custom labels for variables OVERRALL
label(q6_clean$bai_total) <- "Anxiety Score (BAI Total)*"
label(q6_clean$cesd) <- "Depression Score (CES-D)**"
label(q6_clean$sex) <- "Sex"
label(q6_clean$race_eth) <- "Race/Ethnicity"
label(q6_clean$antidep) <- "Antidepressant Use"
label(q6_clean$age_group) <- "Age Group"
label(q6_clean$educ_cat) <- "Education Level"
label(q6_clean$hh_poverty) <- "Household Poverty Threshold"
label(q6_clean$simp_marstat) <- "Marital Status"
label(q6_clean$Alcohol_Use) <- "Alcohol Use"
label(q6_clean$tob) <- "Tobacco Use"
label(q6_clean$cancer) <- "Cancer Status"
label(q6_clean$chronic) <- "Chronic Condition"
label(q6_clean$mj_pastyr) <- "Marijuana use in the past year"

# create the table with the labels
table1_data <- table1(~ bai_total + cesd + sex + race_eth + antidep + age_group + educ_cat + 
                        hh_poverty + simp_marstat + Alcohol_Use + tob, 
                      data = q6_clean,
                      title = "Table 1: Overall Characteristics of Population"
)
print(table1_data)

# Table stratified by if use weed in past year PROBLEM IS CAN'T DO BECAUSE OF MISSING VALUES?----
# Create a new dataset without missing values in mj_pastyr
new_dataset <- q6_clean[!is.na(q6_clean$mj_use_past_year), ]

label(new_dataset$bai_total) <- "Anxiety Score (BAI Total)*"
label(new_dataset$cesd) <- "Depression Score (CES-D)**"
label(new_dataset$sex) <- "Sex"
label(new_dataset$race_eth) <- "Race/Ethnicity"
label(new_dataset$antidep) <- "Antidepressant Use"
label(new_dataset$region) <- "Geographic Region"
label(new_dataset$age_group) <- "Age Group"
label(new_dataset$educ_cat) <- "Education Level"
label(new_dataset$hh_poverty) <- "Household Poverty Threshold"
label(new_dataset$simp_marstat) <- "Marital Status"
label(new_dataset$Alcohol_Use) <- "Alcohol Use"
label(new_dataset$tob) <- "Tobacco Use"
label(new_dataset$cancer) <- "Cancer Status"
label(new_dataset$chronic) <- "Chronic Condition"
label(new_dataset$anx_depr) <- "Anxiety and/or Depression"
label(new_dataset$mj_use_past_year) <- "Marijuana use in the past year"

table1(
  ~ bai_total + cesd + anx_depr + sex + race_eth + age_group + educ_cat + 
    hh_poverty + simp_marstat + Alcohol_Use + tob | mj_use_past_year,
  data = new_dataset,
  caption = "Table 1: Characteristics of Population (marijuana vs. non-marijuana user in the past year)"
)

# By Anxiety and Depression ----
new_dataset1 <- q6_clean[!is.na(q6_clean$anx_depr), ]
label(new_dataset1$sex) <- "Sex"
label(new_dataset1$race_eth) <- "Race/Ethnicity"
label(new_dataset1$antidep) <- "Antidepressant Use"
label(new_dataset1$age_group) <- "Age Group"
label(new_dataset1$educ_cat) <- "Education Level"
label(new_dataset1$hh_poverty) <- "Household Poverty Threshold"
label(new_dataset1$simp_marstat) <- "Marital Status"
label(new_dataset1$Alcohol_Use) <- "Alcohol Use"
label(new_dataset1$tob) <- "Tobacco Use"
label(new_dataset1$cancer) <- "Cancer Status"
label(new_dataset1$chronic) <- "Chronic Condition"
label(new_dataset1$anx_depr) <- "Anxiety and/or Depression"
label(new_dataset1$mj_use_past_year) <- "Marijuana use in the past year"

# Creating Table 1 using new_dataset1
table1(
  ~ sex + race_eth + age_group + educ_cat + 
    hh_poverty + simp_marstat + Alcohol_Use + tob | anx_depr,
  data = new_dataset1,
  caption = "Table 1: Characteristics of Population by Anxiety and/or Depression"
)

# By CESD only ----
new_dataset1 <- q6_clean[!is.na(q6_clean$cesd), ]
new_dataset1$cesd <- as.factor(new_dataset1$cesd)

label(new_dataset1$sex) <- "Sex"
label(new_dataset1$race_eth) <- "Race/Ethnicity"
label(new_dataset1$antidep) <- "Antidepressant Use"
label(new_dataset1$region) <- "Geographic Region"
label(new_dataset1$age_group) <- "Age Group"
label(new_dataset1$educ_cat) <- "Education Level"
label(new_dataset1$hh_income_category) <- "Household Income"
label(new_dataset1$simp_marstat) <- "Marital Status"
label(new_dataset1$Alcohol_Use) <- "Alcohol Use"
label(new_dataset1$tob) <- "Tobacco Use"
label(new_dataset1$cancer) <- "Cancer Status"
label(new_dataset1$chronic) <- "Chronic Condition"
label(new_dataset1$anx_depr) <- "Anxiety and/or Depression"
label(new_dataset1$mj_use_past_year) <- "Marijuana use in the past year"
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
label(new_dataset1$simp_marstat) <- "Marital Status"
label(new_dataset1$Alcohol_Use) <- "Alcohol Use"
label(new_dataset1$tob) <- "Tobacco Use"
label(new_dataset1$cancer) <- "Cancer Status"
label(new_dataset1$chronic) <- "Chronic Condition"
label(new_dataset1$anx_depr) <- "Anxiety and/or Depression"
label(new_dataset1$mj_use_past_year) <- "Marijuana use in the past year"
label(new_dataset1$cesd) <- "Depression Score (CES-D)"

# Creating Table 1 using new_dataset1
table1(
  ~ antidep + sex + race_eth + age_group + educ_cat + 
    hh_poverty + simp_marstat + Alcohol_Use + tob | depress_4,
  data = new_dataset1,
  caption = "Table 1: Characteristics of Population by Depression Score (CES-D)"
)


# Brian's Code ####

## crude association with anxiety and depression ####
# need to categorize exposure/outcome as numbers
df_anx_depr <- new_dataset1 %>%
  mutate(
    anx_depr_num = case_when(
      anx_depr == "None" ~ 0,
      anx_depr == "Anxiety" ~ 1,
      anx_depr == "Depression" ~ 2,
      anx_depr == "Both Anxiety and Depression" ~ 3
      ),
    mj_use_past_year_num = case_when(
      mj_use_past_year == "Not used in Past Year" ~ 0,
      mj_use_past_year == "Use in Past Year" ~ 1
      )
    )
#df_anx_depr$anx_depr_num <- relevel(factor(anx_depr_num), ref = "0")
# df_anx_depr$mj_use_past_year_num <- relevel(factor(mj_use_past_year_num),ref="0")

##unadjusted association - depression only - Nathan##
# need to categorize exposure/outcome as numbers
df_depr <- new_dataset %>%
  mutate(
    depr_num = case_when(
      depress_4 == "Not Depressed" ~ 0,
      depress_4 == "Depressed" ~ 1,
    ),
    mj_use_past_year_num = case_when(
      mj_use_past_year == "Not used in Past Year" ~ 0,
      mj_use_past_year == "Use in Past Year" ~ 1
    )
  )

# factoring exposure and outcome
df_depr <- df_depr %>%
  mutate(
    depr_num = relevel(factor(depr_num), ref = "0"),
    mj_use_past_year_num <- relevel(factor(mj_use_past_year_num), ref = "0")
  )

glm.crude <- glm(mj_use_past_year_num ~ depr_num, 
                 data = df_depr, 
                 family = binomial())

summary(glm.crude)
glm.crude$coefficients
confint(glm.crude)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude$coefficients)
exp(confint(glm.crude))

# factoring exposure and outcome
df_anx_depr <- df_anx_depr %>%
  mutate(
    anx_depr_num = relevel(factor(anx_depr_num), ref = "0"),
    mj_use_past_year_num <- relevel(factor(mj_use_past_year_num), ref = "0")
  )

glm.crude <- glm(mj_use_past_year_num ~ anx_depr_num, 
                 data = df_anx_depr, 
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
      age_group == "50-54 yrs" ~ 0,
      age_group == "55-59 yrs" ~ 1,
      age_group == "60-64 yrs" ~ 2,
      age_group == "65-69 yrs" ~ 3,
      age_group == "70-74 yrs" ~ 4,
      age_group == "75-79 yrs" ~ 5,
      age_group == "80-84 yrs" ~ 6,
      age_group == "85+ yrs" ~ 7
    ),
    educ_cat_num = case_when(
      educ_cat == "Less than High School" ~ 0,
      educ_cat == "High School/GED" ~ 1,
      educ_cat == "Some College" ~ 2,
      educ_cat == "College+" ~ 3
    ),
    hh_income_category_num = case_when(
      hh_income_category == "<19K" ~ 0,
      hh_income_category == "19K-39,999" ~ 1,
      hh_income_category == "40K-79,999" ~ 2,
      hh_income_category == ">= 80K" ~ 3
    ),
    simp_marstat_num = case_when(
      simp_marstat == "Married" ~ 0,
      simp_marstat == "Never married" ~ 1,
      simp_marstat == "Separated/divorced" ~ 2,
      simp_marstat == "Widowed" ~ 3
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
      tob == "Light smokers (1-13 cigarettes/day)" ~ 2,
      tob == "Heavy smokers (>13 cigarettes)" ~ 3
    )
  )

# checking coding worked
table(df_anx_depr$sex_num)
table(df_anx_depr$age_group_num)
table(df_anx_depr$educ_cat_num)
table(df_anx_depr$hh_income_category_num)
table(df_anx_depr$simp_marstat_num)
table(df_anx_depr$Alcohol_Use_num)
table(df_anx_depr$tob_num)

# factoring covariates
df_anx_depr <- df_anx_depr %>%
  mutate(
    sex_num = relevel(factor(sex_num), ref = "0"),
    age_group_num <- relevel(factor(age_group_num), ref = "3"),
    educ_cat_num <- relevel(factor(educ_cat_num), ref = "3"),
    hh_income_category_num <- relevel(factor(hh_income_category_num), ref = "2"),
    simp_marstat_num <- relevel(factor(simp_marstat_num), ref = "0"),
    Alcohol_Use_num <- relevel(factor(Alcohol_Use_num), ref = "0"),
    tob_num <- relevel(factor(tob_num), ref = "0")
  )


glm.adjusted <- glm(mj_use_past_year_num ~ anx_depr_num + 
                      sex_num + age_group_num + educ_cat_num + hh_income_category_num + simp_marstat_num + Alcohol_Use_num + tob_num, 
                    data = df_anx_depr, 
                    family = binomial())

summary(glm.adjusted)
glm.adjusted$coefficients
confint(glm.adjusted)
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted$coefficients)
exp(confint(glm.adjusted))


## updated table 1 ####
table1(
  ~ sex + race_eth + age_group + educ_cat + 
    hh_income_category + simp_marstat + Alcohol_Use + tob | anx_depr,
  data = df_anx_depr,
  caption = "Table 1: Characteristics of Population by Anxiety and/or Depression"
)


