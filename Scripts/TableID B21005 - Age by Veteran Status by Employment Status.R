library(tidycensus)   # Interface with a select number of the US Census Bureau’s data APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.
library(tidyverse)    # Collection of R packages designed for data science
library(readxl)       # Get data out of Excel and into R

d <- paste(getwd(),"/Output/TableID B21005 - Age by Veteran Status by Employment Status",sep="")
dir.create(d, showWarnings = FALSE)

## State Output ##

B21005_CA <- get_acs(
  geography = "state",
  state = "California",
  variables = c("18-34 (Employed)" = "B21005_005",
                "18-34 (Unemployed)" = "B21005_006",
                "35-54 (Employed)" = "B21005_016",
                "35-54 (Unmployed)" = "B21005_017",
                "55-64 (Employed)" = "B21005_027",
                "55-64 (Unemployed)" = "B21005_028"),
  survey = "acs5",
  year = 2019) %>%
  select(geoid = GEOID, state = NAME, age_group = variable, estimate, moe) %>%
  write.csv(paste(d,"/California.csv", sep=""))

## CBSA Output ##

dir.create(paste(d,"/CBSA/",sep=""), showWarnings = FALSE)

# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cbsa <- read_excel("./Data/cbsa.xls") %>%
  filter(fips_state_code == "06") %>%
  distinct()

B21005_cbsa <- get_acs(
  geography = "cbsa",
  variables = c("18-34 (Employed)" = "B21005_005",
                "18-34 (Unemployed)" = "B21005_006",
                "35-54 (Employed)" = "B21005_016",
                "35-54 (Unmployed)" = "B21005_017",
                "55-64 (Employed)" = "B21005_027",
                "55-64 (Unemployed)" = "B21005_028"),
  survey = "acs5",
  year = 2019) %>%
  filter(GEOID %in% cbsa$cbsa_code) %>%
  group_by(NAME) %>%
  select(geoid = GEOID, cbsa = NAME, age_group = variable, estimate, moe) %>%
  arrange(cbsa)

cbsa_list <- unique(B21005_cbsa$cbsa)

for (active_cbsa in cbsa_list) {
  
  file_cbsa <- paste(d,"/CBSA/",active_cbsa,".csv",sep="")  
  
  B21005_cbsa %>%
    filter(cbsa == active_cbsa) %>%
    write.csv(file_cbsa)
  
  print(file_cbsa)
  
}   

## County Output ##

dir.create(paste(d,"/County/",sep=""), showWarnings = FALSE)

B21005_county <- get_acs(
  geography = "county",
  state = "California",
  variables = c("18-34 (Employed)" = "B21005_005",
                "18-34 (Unemployed)" = "B21005_006",
                "35-54 (Employed)" = "B21005_016",
                "35-54 (Unmployed)" = "B21005_017",
                "55-64 (Employed)" = "B21005_027",
                "55-64 (Unemployed)" = "B21005_028"),
  survey = "acs5",
  year = 2019) %>%
  group_by(NAME) %>%
  mutate(NAME = str_replace(NAME," County, California","")) %>%
  select(geoid = GEOID, county = NAME, age_group = variable, estimate, moe) %>%
  arrange(county)

county_list <- unique(B21005_county$county)

for (active_county in county_list) {
  
  file_county <- paste(d,"/County/",active_county,".csv",sep="")  
  
  B21005_county %>%
    filter(county == active_county) %>%
    write.csv(file_county)
  
  
  print(file_county)  
}  

