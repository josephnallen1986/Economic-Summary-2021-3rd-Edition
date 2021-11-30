library(tidycensus)   # Interface with a select number of the US Census Bureau’s data APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.
library(tidyverse)    # Collection of R packages designed for data science
library(readxl)       # Get data out of Excel and into R

d <- paste(getwd(),"/Output/TableID B21003 - Veteran Status By Educational Attainment",sep="")
dir.create(d, showWarnings = FALSE)

## State Output ##

B21003_CA <- get_acs(
  geography = "state",
  state = "California",
  variables = c("Less than high school graduate" = "B21003_003", 
                "High school graduate (includes equivalency)" = "B21003_004", 
                "Some college or associate's degree" = "B21003_005", 
                "Bachelor's degree or higher" = "B21003_006"),
  survey = "acs5",
  year = 2019,
  summary_var = "B21003_002") %>%
  mutate(prop = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  select(geoid = GEOID, state = NAME, education = variable, estimate, moe, summary_est, summary_moe, prop, prop_moe) %>%
  write.csv(paste(d,"/California.csv", sep=""))  


## CBSA Output ##

dir.create(paste(d,"/CBSA/",sep=""), showWarnings = FALSE)

# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cbsa <- read_excel("./Data/cbsa.xls") %>%
  filter(fips_state_code == "06") %>%
  distinct()

B21003_cbsa <- get_acs(
  geography = "cbsa",
  variables = c("Less than high school graduate" = "B21003_003", 
                "High school graduate (includes equivalency)" = "B21003_004", 
                "Some college or associate's degree" = "B21003_005", 
                "Bachelor's degree or higher" = "B21003_006"),
  survey = "acs5",
  year = 2019,
  summary_var = "B21003_002") %>%
  filter(GEOID %in% cbsa$cbsa_code) %>%
  mutate(prop = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  filter(GEOID %in% cbsa$cbsa_code) %>%
  group_by(NAME) %>%
  select(geoid = GEOID, cbsa = NAME, education = variable, estimate, moe, summary_est, summary_moe, prop, prop_moe) %>%
  arrange(cbsa)

cbsa_list <- unique(B21003_cbsa$cbsa)

for (active_cbsa in cbsa_list) {
  
  file_cbsa <- paste(d,"/CBSA/",active_cbsa,".csv",sep="")  
  
  B21003_cbsa %>%
    filter(cbsa == active_cbsa) %>%
    write.csv(file_cbsa)
  
  print(file_cbsa)
  
}  

## County Output ##

dir.create(paste(d,"/County/",sep=""), showWarnings = FALSE)

B21003_county <- get_acs(
  geography = "county",
  state = "California",
  variables = c("Less than high school graduate" = "B21003_003", 
                "High school graduate (includes equivalency)" = "B21003_004", 
                "Some college or associate's degree" = "B21003_005", 
                "Bachelor's degree or higher" = "B21003_006"),
  survey = "acs5",
  year = 2019,
  summary_var = "B21003_002") %>%
  mutate(NAME = str_replace(NAME," County, California",""),
         prop = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe)) %>%
  group_by(NAME) %>%
  select(geoid = GEOID, county = NAME, education = variable, estimate, moe, summary_est, summary_moe, prop, prop_moe) %>%
  arrange(county)

county_list <- unique(B21003_county$county)

for (active_county in county_list) {

  file_county <- paste(d,"/County/",active_county,".csv",sep="")  
  
  B21003_county %>%
    filter(county == active_county) %>%
    write.csv(file_county)
  
  
  print(file_county)  
}  

