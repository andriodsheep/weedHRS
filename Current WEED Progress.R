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

#Age Groups----
q6_clean$age_group <- cut(q6_clean$age,
                      breaks = c(49, 54, 59, 64, 69, 74, 79, 84, Inf),
                      labels = c("50-54 yrs", "55-59 yrs", "60-64 yrs", "65-69 yrs", 
                                 "70-74 yrs", "75-79 yrs", "80-84 yrs", "85+ yrs"),
                      right = FALSE)
table(q6_clean$age_group)


#Smoking----
q6_clean$Tobacco_Use <- with(q6_clean, ifelse(tob_ever == 5, "Never",
                                              ifelse(tob_ever == 1 & tob_cur == 5, "Former",
                                                     ifelse(tob_cur == 1 & tob_cigday < 25, "Light",
                                                            ifelse(tob_cur == 1 & tob_cigday >= 25, "Heavy", NA)))))

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




#Race----
q6_clean$race_eth = ifelse(q6_clean$ethnicity == 1, 3,  # Hispanic
                           ifelse(q6_clean$race == 1, 1,  # White
                                  ifelse(q6_clean$race == 2, 2,  # Black
                                         ifelse(q6_clean$race == 3, 4, NA))))  # Other or Missing
table(q6_clean$race_eth)





#BAI Score----
#total BAI - Nathan
q6_clean <- q6_clean %>%
  mutate(
    bai_total = bai_fearworst + bai_nervous + bai_tremble + bai_feardie + bai_faint
  )
table(q6_clean$bai_total)

#Should we include a column for BAI >= 12?


#anxiety and depression composite variable - Nathan----
q6_clean <- q6_clean %>%
  mutate(anx_depr = case_when(
    q6_clean$cesd < 4 & q6_clean$bai_total < 12 ~ 1,
    q6_clean$cesd < 4 & q6_clean$bai_total >= 12 ~ 2,
    q6_clean$cesd >= 4 & q6_clean$bai_total < 12 ~ 3,
    q6_clean$cesd >= 4 & q6_clean$bai_total >= 12 ~ 4
  ))

table(q6_clean$cesd) #is CESD continuous?
table(q6_clean$antidep) # -8 is no response, make NA


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
  mutate(educ_cat = factor(educ, labels = c(" Less than High School", "High School/GED", "Some College", "College+")))
table(q6_clean$educ_cat)

#Look at all frequency tables
lapply(q6_clean, table)


#Table 1----
table(q6_clean$mj_pastyr)

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

#GEO REGION
q6_clean$region <- factor(q6_clean$region)
q6_clean$region <- factor(q6_clean$region, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Midwest", "Northeast", "South", "West", "Other"))

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

#TOBACCO USE NATHAN'S
q6_clean$tob <- factor(q6_clean$tob)
q6_clean$tob <- factor(q6_clean$tob, 
                       levels = c(1, 2, 3, 4), 
                       labels = c("Non-smokers", 
                                  "Former smokers", 
                                  "Light smokers (1-13 cigarettes/day)", 
                                  "Heavy smokers (>13 cigarettes)"))

#Look at data dictionary CAN'T FIND ON HRS
q6_clean$cancer <- factor(q6_clean$cancer)

#chronic condition ALL NAs?
q6_clean$chronic <- factor(q6_clean$chronic)
table(q6_clean$chronic)

#IF USE WEED IN PAST YEAR
q6_clean$mj_pastyr <- factor(q6_clean$mj_pastyr, 
                          levels = c(1,5), 
                          labels = c("Yes", "No"))
table(q6_clean$mj_pastyr)



#Summary Statistics
# Set custom labels for variables
label(q6_clean$bai_total) <- "Anxiety Score (BAI Total)*"
label(q6_clean$cesd) <- "Depression Score (CES-D)**"
label(q6_clean$sex) <- "Sex"
label(q6_clean$race_eth) <- "Race/Ethnicity"
label(q6_clean$antidep) <- "Antidepressant Use"
label(q6_clean$region) <- "Geographic Region"
label(q6_clean$age_group) <- "Age Group"
label(q6_clean$educ_cat) <- "Education Level"
label(q6_clean$hh_income) <- "Household Income"
label(q6_clean$simp_marstat) <- "Marital Status"
label(q6_clean$Alcohol_Use) <- "Alcohol Use"
label(q6_clean$tob) <- "Tobacco Use"
label(q6_clean$cancer) <- "Cancer Status"
label(q6_clean$chronic) <- "Chronic Condition"
label(q6_clean$mj_pastyr) <- "Marijuana use in the past year"

# Create the table with the new labels
table1_data <- table1(~ bai_total + cesd + sex + race_eth + antidep + region + age_group + educ_cat + 
                        hh_income + simp_marstat + Alcohol_Use + tob, 
                      data = q6_clean)
print(table1_data)


# Table stratified by if use weed in past year PROBLEM IS CAN'T DO BECAUSE OF MISSING VALUES?
# Create a new dataset without missing values in mj_pastyr
new_dataset <- q6_clean[!is.na(q6_clean$mj_pastyr), ]

label(new_dataset$bai_total) <- "Anxiety Score (BAI Total)*"
label(new_dataset$cesd) <- "Depression Score (CES-D)**"
label(new_dataset$sex) <- "Sex"
label(new_dataset$race_eth) <- "Race/Ethnicity"
label(new_dataset$antidep) <- "Antidepressant Use"
label(new_dataset$region) <- "Geographic Region"
label(new_dataset$age_group) <- "Age Group"
label(new_dataset$educ_cat) <- "Education Level"
label(new_dataset$hh_income) <- "Household Income"
label(new_dataset$simp_marstat) <- "Marital Status"
label(new_dataset$Alcohol_Use) <- "Alcohol Use"
label(new_dataset$tob) <- "Tobacco Use"
label(new_dataset$cancer) <- "Cancer Status"
label(new_dataset$chronic) <- "Chronic Condition"
label(new_dataset$mj_pastyr) <- "Marijuana use in the past year"

table1(
  ~ bai_total + cesd + sex + race_eth + antidep + region + age_group + educ_cat + 
    hh_income + simp_marstat + Alcohol_Use + tob | mj_pastyr,
  data = new_dataset
)



