library(tidycensus)   # Interface with a select number of the US Census Bureau’s data APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.
library(tidyverse)    # Collection of R packages designed for data science
library(readxl)       # Get data out of Excel and into R

d <- paste(getwd(),"/Output/TableID B21001 - Sex by Age by Veteran Status",sep="")
dir.create(d, showWarnings = FALSE)

## State Output ##

B21001_CA <- get_acs(
  geography = "state",
  state = "California",
  survey = "acs5",
  year = 2019,
  variables = c("Male" =  "B21001_005", "Female" = "B21001_023"),
  summary_var = "B21001_002") %>%
  mutate(prop = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  select(geoid = GEOID, state = NAME, sex = variable, estimate, moe, summary_est, summary_moe, prop, prop_moe) %>%
  write.csv(paste(d,"/California.csv", sep=""))


## CBSA Ouptut ##

dir.create(paste(d,"/CBSA/",sep=""), showWarnings = FALSE)

# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cbsa <- read_excel("./Data/cbsa.xls") %>%
  filter(fips_state_code == "06") %>%
  distinct()

B21001_cbsa <- get_acs(
  geography = "cbsa",
  survey = "acs5",
  year = 2019,
  variables = c("Male" =  "B21001_005", "Female" = "B21001_023"),
  summary_var = "B21001_002") %>%
  mutate(prop = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  filter(GEOID %in% cbsa$cbsa_code) %>%
  group_by(NAME) %>%
  select(geoid = GEOID, cbsa = NAME, sex = variable, estimate, moe, summary_est, summary_moe, prop, prop_moe) %>%
  arrange(cbsa)


cbsa_list <- unique(B21001_cbsa$cbsa)

for (active_cbsa in cbsa_list) {
  
  file_cbsa <- paste(d,"/CBSA/",active_cbsa,".csv",sep="")  
  
  B21001_cbsa %>%
    filter(cbsa == active_cbsa) %>%
    write.csv(file_cbsa)
  
  print(file_cbsa)
  
}  


## County Output ##

dir.create(paste(d,"/County/",sep=""), showWarnings = FALSE)

B21001_county <- get_acs(
  geography = "county",
  state = "California",
  survey = "acs5",
  year = 2019,
  variables = c("Male" =  "B21001_005", "Female" = "B21001_023"),
  summary_var = "B21001_002") %>%
  mutate(NAME = str_replace(NAME," County, California",""),
         prop = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  group_by(NAME) %>%
  select(geoid = GEOID, county = NAME, sex = variable, estimate, moe, summary_est, summary_moe, prop, prop_moe) %>%
  arrange(county)

county_list <- unique(B21001_county$county)

for (active_county in county_list) {
  
  file_county <- paste(d,"/County/",active_county,".csv",sep="")  
  
  B21001_county %>%
    filter(county == active_county) %>%
    write.csv(file_county)
  
  print(file_county)
  
  
}  


