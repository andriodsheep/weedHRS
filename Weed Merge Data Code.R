library(haven) 
library(stringr)
library(tidyverse)

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

# response breakdown should be identical between these tables
table(cannabis_module_clean$QV402)
table(q6_data_complete$QV402)

# write out file - replace file path with your own
write.csv("weed_data")

#read in the merged dataset
weed <- read.csv("weed_data")

#filter out HHIDPN that are not in the weed module
weed <- weed %>%
  filter(HHIDPN %in% cannabis_module_clean$HHIDPN)

table(cannabis_module_clean$QV402)
table(weed$QV402)



