library(haven) 
library(stringr)
library(tidyverse)
library(table1)
library(Hmisc)
#Merging ----
base_data<-read.csv("q6_data.csv")

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
write.csv(q6_data_complete, "weed_merge")

#read in the merged dataset
weed_merge <- read.csv("weed_merge")

#filter out HHIDPN that are not in the weed module
weed_merged <- weed_merge %>%
  filter(HHIDPN %in% cannabis_module_clean$HHIDPN)
table(cannabis_module_clean$QV402)
table(weed$QV402)

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
                                             QC116, QC117, QC118, QLB035C1, QLB035C2, QLB035C3, QLB035C4, QLB035C5))

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
         "bai_fearworst" = "QLB035C1",
         "bai_nervous" = "QLB035C2",
         "bai_tremble" = "QLB035C3",
         "bai_feardie" = "QLB035C4",
         "bai_faint" = "QLB035C5")

#Operationalizing the Variables----
q6_clean <- read_csv("q6_clean")

#Age Groups
q6_clean$age_group <- cut(q6_clean$age,
                      breaks = c(49, 54, 59, 64, 69, 74, 79, 84, Inf),
                      labels = c("50-54 yrs", "55-59 yrs", "60-64 yrs", "65-69 yrs", 
                                 "70-74 yrs", "75-79 yrs", "80-84 yrs", "85+ yrs"),
                      right = FALSE)
table(q6_clean$age_group)


#Smoking
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

q6_clean$tob <- factor(q6_clean$tob)
q6_clean$tob <- factor(q6_clean$tob, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("Non-smokers", 
                                  "Former smokers", 
                                  "Light smokers (1-13 cigarettes/day)", 
                                  "Heavy smokers (>13 cigarettes)"))
table(q6_clean$tob)


#Race
table(q6_clean$race)

q6_clean$race_eth = ifelse(q6_clean$ethnicity == 1, 3,  # Hispanic
                           ifelse(q6_clean$race == 1, 1,  # White
                                  ifelse(q6_clean$race == 2, 2,  # Black
                                         ifelse(q6_clean$race == 3, 4, NA))))  # Other or Missing
table(q6_clean$race_eth)




#BAI Score 
#total BAI - Nathan
q6_clean <- q6_clean %>%
  mutate(
    bai_total = bai_fearworst + bai_nervous + bai_tremble + bai_feardie + bai_faint
  )
table(q6_clean$bai_total)


#anxiety and depression composite variable - Nathan
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

#Alcohol Use
table(q6_clean$alc_ever)
q6_clean$Alcohol_Use <- with(q6_clean, ifelse(alc_ever == 3 | alc_ever == 5, "Never drinkers",
                                              ifelse(alc_drinkday == 0| alc_daywk == 0, "Former drinkers",
                                                     ifelse((sex == 2 & (alc_daywk * alc_drinkday) >= 8) | 
                                                              (sex == 1 & (alc_daywk * alc_drinkday) >= 15), 
                                                            "Heavy drinkers",
                                                            "Low to moderate drinkers"))))

q6_clean$Alcohol_Use <- factor(q6_clean$Alcohol_Use, 
                               levels = c("Never drinkers", "Former drinkers", "Low to moderate drinkers", "Heavy drinkers"))

table(q6_clean$Alcohol_Use)

#Education Levels
q6_clean <- q6_clean %>%
  mutate(educ = case_when(
    educ %in% c(2, 3) ~ 2,  # Combine categories 2 and 3
    TRUE ~ educ             # Keep other categories as they are
  )) %>%
  mutate(educ_cat = factor(educ, labels = c(" Less than High School", "High School/GED", "Some College", "College+")))
table(q6_clean$educ_cat)

#Look at all frequency tables
lapply(q6_clean, table)
names(q6_clean)

#Label the categorical variables
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

#GEO REGION
q6_clean$region <- factor(q6_clean$region)
q6_clean$region <- factor(q6_clean$region, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Midwest", "Northeast", "South", "West", "Other"))


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


#Look at data dictionary CAN'T FIND ON HRS
q6_clean$cancer <- factor(q6_clean$cancer)

#chronic condition ALL NAs
q6_clean$chronic <- factor(q6_clean$chronic)
table(q6_clean$chronic)

#IF USE IN THE PAST YEAR
q6_clean <- q6_clean %>%
  mutate(mj_use_past_year = case_when(
    q6_clean$mj_pastyr == 1 ~ "Use in Past Year",
    q6_clean$mj_ever == 5 | q6_clean$mj_pastyr == 5 ~"Not used in Past Year",
  ))
table(q6_clean$mj_use_past_year)

#Income categories
q6_clean <- q6_clean %>%
  mutate(hh_income_category = factor(case_when(
    hh_income < 19000 ~ "<19K",
    hh_income >= 19000 & hh_income < 40000 ~ "19K-39,999",
    hh_income >= 40000 & hh_income < 80000 ~ "40K-79,999",
    hh_income >= 80000 ~ ">= 80K"
  ), levels = c("<19K", "19K-39,999", "40K-79,999", ">= 80K")))

table(q6_clean$hh_income_category)


# Subset data for rows that are NOT NA for anx_depr variable (Start of Brian's Code)----
df_anx_depr <- q6_clean[!is.na(q6_clean$anx_depr), ]

df_anx_depr 

# updated table 1 
label(df_anx_depr$sex) <- "Sex"
label(df_anx_depr$race_eth) <- "Race/Ethnicity"
label(df_anx_depr$antidep) <- "Antidepressant Use"
label(df_anx_depr$region) <- "Geographic Region"
label(df_anx_depr$age_group) <- "Age Group"
label(df_anx_depr$educ_cat) <- "Education Level"
label(df_anx_depr$hh_income_category) <- "Household Income"
label(df_anx_depr$simp_marstat) <- "Marital Status"
label(df_anx_depr$Alcohol_Use) <- "Alcohol Use"
label(df_anx_depr$tob) <- "Tobacco Use"
label(df_anx_depr$cancer) <- "Cancer Status"
label(df_anx_depr$chronic) <- "Chronic Condition"
label(df_anx_depr$anx_depr) <- "Anxiety and/or Depression"
label(df_anx_depr$mj_use_past_year) <- "Marijuana use in the past year"

table1(
  ~ sex + race_eth + age_group + educ_cat + 
    hh_income_category + simp_marstat + Alcohol_Use + tob | anx_depr,
  data = df_anx_depr,
  caption = "Table 1: Characteristics of Population by Anxiety and/or Depression"
)


#THINGS TO ASK BERNE/LAUREN Clean the dataset more? ----
df_anx_depr$tob_ever # tons of NAs here
table(df_anx_depr$tob_cur)
table(df_anx_depr$tob_cigday)

table(df_anx_depr$age) # found people below 50 #q6_clean <- q6_clean[q6_clean$age >= 50] ????

# Brian's Code crude association with anxiety and depression ----

# need to categorize exposure/outcome as numbers
# came from this part of the code before -> new_dataset1 <- q6_clean[!is.na(q6_clean$anx_depr), ]

df_anx_depr <- df_anx_depr %>%
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

# factoring exposure and outcome
df_anx_depr <- df_anx_depr %>%
  mutate(
    anx_depr_num = relevel(factor(anx_depr_num), ref = "0"),
    mj_use_past_year_num = relevel(factor(mj_use_past_year_num), ref = "0")
  )

class(df_anx_depr$anx_depr_num)

glm.crude <- glm(mj_use_past_year_num ~ anx_depr_num, 
                 data = df_anx_depr, 
                 family = binomial)

summary(glm.crude)
glm.crude$coefficients
confint(glm.crude)
# To get the effect, we need to exponentiate the coefficients
exp(glm.crude$coefficients)
exp(confint(glm.crude))

#ANOVA
glm.null <- glm(mj_use_past_year_num ~ 1, 
                 data = df_anx_depr, 
                 family = binomial())

summary(glm.null)
anova(glm.null, glm.crude, test = "Chisq")

anova(glm.null, glm.adjusted, test = "Chisq")

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

# factoring covariates and setting the reference
df_anx_depr <- df_anx_depr %>%
  mutate(
    sex_num = relevel(factor(sex_num), ref = "0"),
    age_group_num = relevel(factor(age_group_num), ref = "3"),
    educ_cat_num = relevel(factor(educ_cat_num), ref = "3"),
    hh_income_category_num = relevel(factor(hh_income_category_num), ref = "2"),
    simp_marstat_num = relevel(factor(simp_marstat_num), ref = "0"),
    Alcohol_Use_num = relevel(factor(Alcohol_Use_num), ref = "1"),
    tob_num = relevel(factor(tob_num), ref = "0")
  )


glm.adjusted <- glm(mj_use_past_year_num ~ anx_depr_num + 
                      sex_num + age_group_num + educ_cat_num + hh_income_category_num + simp_marstat_num + Alcohol_Use_num + tob_num, 
                    data = df_anx_depr, 
                    family = binomial)

summary(glm.adjusted)
glm.adjusted$coefficients
confint(glm.adjusted) #glm.fit: fitted probabilities numerically 0 or 1 occurred
# To get the effect, we need to exponentiate the coefficients
exp(glm.adjusted$coefficients)
exp(confint(glm.adjusted))

#Lasso, Ridge, Stepwise?






