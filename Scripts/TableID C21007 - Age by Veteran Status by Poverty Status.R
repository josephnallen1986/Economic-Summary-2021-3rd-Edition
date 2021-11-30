library(tidycensus)   # Interface with a select number of the US Census Bureau’s data APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.
library(tidyverse)    # Collection of R packages designed for data science
library(readxl)       # Get data out of Excel and into R

d <- paste(getwd(),"/Output/TableID C21007 - Age by Veteran Status by Poverty Status",sep="")
dir.create(d, showWarnings = FALSE)

places <- read.table(file = "https://www2.census.gov/geo/docs/reference/codes/files/st06_ca_places.txt",
                     header = FALSE,
                     text = txt,
                     colClasses = "character",
                     sep = "|") %>%
  select(V4, V7) %>%
  distinct()

colnames(places) <- c("place","county")

places <- places %>%
  inner_join(cbsa, by=c("county"="county"))

## State Output ##

C21007_CA <- get_acs(
  geography = "place",
  variables = c("18-64" = "C21007_003", "65+" = "C21007_018"),
  survey = "acs5",
  state = "CA",
  year = 2019)%>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  inner_join(places, by=c("NAME" = "place")) %>%
  select(county, place = NAME, age_group = variable, estimate, moe) %>%
  arrange(county, place)

C21007_CA_top <- C21007_CA %>%
  group_by(place) %>%
  summarize(total_vet_est = 
              estimate[age_group == "18-64"] +
              estimate[age_group == "65+"]) %>%
  arrange(desc(total_vet_est)) %>%
  slice_max(order_by = total_vet_est, n = 10)

C21007_CA %>%
  inner_join(C21007_CA_top, by=c("place" = "place")) %>%
  arrange(desc(total_vet_est),place) %>%
  select(place, age_group, estimate, moe) %>%
  write.csv(paste(d,"/California.csv", sep=""))

## CBSA Output ##

dir.create(paste(d,"/CBSA/",sep=""), showWarnings = FALSE)

# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cbsa <- read_excel("./Data/cbsa.xls") %>%
  filter(fips_state_code == "06") %>%
  distinct()

C21007_cbsa <- get_acs(
  geography = "place",
  variables = c("18-64" = "C21007_003", "65+" = "C21007_018"),
  survey = "acs5",
  state = "CA",
  year = 2019)%>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  inner_join(places, by=c("NAME" = "place")) %>%
  select(cbsa = tidycensus, place = NAME, age_group = variable, estimate, moe) %>%
  arrange(cbsa, place)

C21007_cbsa_top <- C21007_cbsa %>%
  group_by(cbsa, place) %>%
  summarize(total_vet_est = 
              estimate[age_group == "18-64"] +
              estimate[age_group == "65+"]) %>%
  ungroup() %>%
  arrange(desc(total_vet_est)) %>%
  group_by(cbsa) %>%
  slice_max(order_by = total_vet_est, n = 10)

cbsa_list <- unique(C21007_cbsa$cbsa)

for (active_cbsa in cbsa_list) {
  
  file_cbsa <- paste(d,"/CBSA/",active_cbsa,".csv",sep="")  
  
  C21007_cbsa %>%
    filter(cbsa == active_cbsa) %>%
    inner_join(C21007_cbsa_top, by=c("cbsa"="cbsa","place"="place")) %>%
    arrange(desc(total_vet_est),cbsa,place) %>%
    select(cbsa, place, age_group, estimate, moe) %>%
    write.csv(file_cbsa)
  
  print(file_cbsa)
  
}   

## County Output ##

dir.create(paste(d,"/County/",sep=""), showWarnings = FALSE)

C21007_county <- get_acs(
  geography = "place",
  variables = c("18-64" = "C21007_003", "65+" = "C21007_018"),
  survey = "acs5",
  state = "CA",
  year = 2019)%>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  inner_join(places, by=c("NAME" = "place")) %>%
  mutate(county = str_replace_all(county, " County", "")) %>%
  select(county, place = NAME, age_group = variable, estimate, moe) %>%
  arrange(county, place)

C21007_county_top <- C21007_county %>%
  group_by(county, place) %>%
  summarize(total_vet_est = 
              estimate[age_group == "18-64"] +
              estimate[age_group == "65+"]) %>%
  ungroup() %>%
  arrange(desc(total_vet_est)) %>%
  group_by(county) %>%
  slice_max(order_by = total_vet_est, n = 10)

county_list <- unique(C21007_county$county)

for (active_county in county_list) {

  file_county <- paste(d,"/County/",active_county,".csv",sep="")  
  
  C21007_county %>%
    filter(county == active_county) %>%
    inner_join(C21007_county_top, by=c("county"="county","place"="place")) %>%
    arrange(desc(total_vet_est),county,place) %>%
    select(county, place, age_group, estimate, moe) %>%
    write.csv(file_county)
  
  print(file_county)
  
}  
