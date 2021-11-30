library(tidycensus)   # Interface with a select number of the US Census Bureau’s data APIs and return tidyverse-ready data frames, optionally with simple feature geometry included.
library(tidyverse)    # Collection of R packages designed for data science
library(readxl)       # Get data out of Excel and into R


d <- paste(getwd(),"/Output/TableID S2101 - Veteran Status",sep="")
dir.create(d, showWarnings = FALSE)


## State Output ##

S2101_CA <- get_acs(
  geography = "place",
  variables = c("S2101_C03_009", "S2101_C03_010", "S2101_C03_011", 
                "S2101_C03_012", "S2101_C03_013"),
  survey = "acs5",
  state = "CA",
  year = 2019)%>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  inner_join(places, by=c("NAME" = "place")) %>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  group_by(NAME) %>%
  summarize("18_to_64" = estimate[variable == "S2101_C03_009"] +
              estimate[variable == "S2101_C03_010"] +
              estimate[variable == "S2101_C03_011"],
            "65_plus" = estimate[variable == "S2101_C03_012"] +
              estimate[variable == "S2101_C03_013"],
            "total_vets" = estimate[variable == "S2101_C03_009"] +
              estimate[variable == "S2101_C03_010"] +
              estimate[variable == "S2101_C03_011"] + 
              estimate[variable == "S2101_C03_012"] +
              estimate[variable == "S2101_C03_013"]) %>%
  inner_join(places, by=c("NAME" = "place"),) %>%
  select(place = NAME, "18_to_64", "65_plus", "total_vets") %>%
  arrange(place) %>%
  ungroup() %>%
  arrange(desc(total_vets)) %>%
  slice_max(order_by = total_vets, n = 10) %>%
  write.csv(paste(d,"/California.csv", sep=""))

## CBSA Output ##

dir.create(paste(d,"/CBSA/",sep=""), showWarnings = FALSE)

# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cbsa <- read_excel("./data/cbsa.xls") %>%
  filter(fips_state_code == "06") %>%
  distinct()

places <- places %>%
  inner_join(cbsa, by=c("county"="county"))

S2101_cbsa <- get_acs(
  geography = "place",
  variables = c("S2101_C03_009", "S2101_C03_010", "S2101_C03_011", 
                "S2101_C03_012", "S2101_C03_013"),
  survey = "acs5",
  state = "CA",
  year = 2019)%>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  group_by(NAME) %>%
  summarize("18_to_64" = estimate[variable == "S2101_C03_009"] +
              estimate[variable == "S2101_C03_010"] +
              estimate[variable == "S2101_C03_011"],
            "65_plus" = estimate[variable == "S2101_C03_012"] +
              estimate[variable == "S2101_C03_013"],
            "total_vets" = estimate[variable == "S2101_C03_009"] +
              estimate[variable == "S2101_C03_010"] +
              estimate[variable == "S2101_C03_011"] + 
              estimate[variable == "S2101_C03_012"] +
              estimate[variable == "S2101_C03_013"]) %>%
  inner_join(places, by=c("NAME" = "place")) %>%
  select(cbsa = tidycensus, place = NAME, "18_to_64", "65_plus", "total_vets") %>%
  arrange(cbsa, place) %>%
  ungroup() %>%
  arrange(desc(total_vets)) %>%
  group_by(cbsa) %>%
  slice_max(order_by = total_vets, n = 10)

cbsa_list <- unique(S2101_cbsa$cbsa)

for (active_cbsa in cbsa_list) {
  
  pop_fil <- paste(d,"/CBSA/",active_cbsa,".csv",sep="")  
  
  S2101_cbsa %>%
    filter(cbsa == active_cbsa) %>%
    write.csv(pop_fil)
  
  print(pop_fil)
  
}    


## County Output ##

dir.create(paste(d,"/County/",sep=""), showWarnings = FALSE)

places <- read.table(file = "https://www2.census.gov/geo/docs/reference/codes/files/st06_ca_places.txt",
                     header = FALSE,
                     text = txt,
                     colClasses = "character",
                     sep = "|") %>%
  select(V4, V7) %>%
  distinct()

colnames(places) <- c("place","county")

S2101_County <- get_acs(
  geography = "place",
  variables = c("S2101_C03_009", "S2101_C03_010", "S2101_C03_011", 
                "S2101_C03_012", "S2101_C03_013"),
  survey = "acs5",
  state = "CA",
  year = 2019)%>%
  mutate(NAME = str_replace_all(NAME, ", California", "")) %>%
  group_by(NAME) %>%
  summarize("18_to_64" = estimate[variable == "S2101_C03_009"] +
              estimate[variable == "S2101_C03_010"] +
              estimate[variable == "S2101_C03_011"],
            "65_plus" = estimate[variable == "S2101_C03_012"] +
              estimate[variable == "S2101_C03_013"],
            "total_vets" = estimate[variable == "S2101_C03_009"] +
              estimate[variable == "S2101_C03_010"] +
              estimate[variable == "S2101_C03_011"] + 
              estimate[variable == "S2101_C03_012"] +
              estimate[variable == "S2101_C03_013"]) %>%
  inner_join(places, by=c("NAME" = "place"),) %>%
  mutate(county = str_replace_all(county, " County", "")) %>%
  select(county, place = NAME, "18_to_64", "65_plus", "total_vets") %>%
  arrange(county, place) %>%
  ungroup() %>%
  arrange(desc(total_vets)) %>%
  group_by(county) %>%
  slice_max(order_by = total_vets, n = 10)

county_list <- unique(S2101_County$county)

for (active_county in county_list) {
  
  pop_fil <- paste(d,"/County/",active_county,".csv",sep="")  

  S2101_County %>%
    filter(county == active_county) %>%
    write.csv(pop_fil)
  
  print(pop_fil)
  
}    



