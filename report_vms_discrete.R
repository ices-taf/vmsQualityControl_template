
library(icesTAF)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)

options(dplyr.summarise.inform = FALSE)

mkdir("report")

vms <- fread("data/vms.csv")

#### character data summaries by year

## Years & number of records for which data has been submitted:
vms %>%
  count(year) %>%
  write.taf(file = "records_per_year.csv", dir = "report")

## Distribution of records by month:
vms %>%
  count(month, year) %>%
  write.taf("records_per_month.csv", dir = "report")

## Frequency of vessel length categories by year:
vms %>%
  count(vessel_length_category, year) %>%
  write.taf("records_per_vessel_length_cat.csv", dir = "report")

## Frequency of gear codes by year:
vms %>%
  count(gear_code, year) %>%
  write.taf("records_per_gear_code.csv", dir = "report")

## Number of unique DCF Level 6 codes by year:
vms %>%
  group_by(year) %>%
  summarise(n = length(unique(LE_MET_level6))) %>%
  write.taf("unique_level6_code_by_year.csv", dir = "report")


## Top 5 DCF Level 6 codes by year:
top5Met6 <-
  vms %>%
  count(LE_MET_level6) %>%
  arrange(desc(n)) %>%
  select(LE_MET_level6) %>%
  head(5) %>%
  `[[`(1)

vms %>%
  filter(LE_MET_level6 %in% top5Met6) %>%
  count(LE_MET_level6, year) %>%
  write.taf("records_per_metier_level_6.csv", dir = "report")
