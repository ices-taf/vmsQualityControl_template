library(icesTAF)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)

source("utilities_report.R")

mkdir("report")

vms <- fread("data/vms.csv")


#### time series data summaries

## Landings by gear by year:
vms %>%
  group_by(year, gear_code) %>%
  summarise(
    y = sum(totweight, na.rm = TRUE)
  ) %>%
  write.taf("summary_of_totalwieight.csv", dir = "report")


## Mean landing per kW fishing hours by year:
vms %>%
  group_by(year, gear_code) %>%
  summarise(
    y = median(totweight / kw_fishinghours, na.rm = TRUE)
  ) %>%
  write.taf("summary_of_median_weight_per_effort.csv", dir = "report")


## Value:
vms %>%
  group_by(year, gear_code) %>%
  summarise(
    y = sum(totvalue, na.rm = TRUE)
  ) %>%
  write.taf("summary_of_totvalue.csv", dir = "report")


## value per effort:
vms %>%
  group_by(year, gear_code) %>%
  summarise(
    y = median(totvalue / kw_fishinghours, na.rm = TRUE)
  ) %>%
  write.taf("summary_of_median_value_per_hour.csv", dir = "report")

##  Average price:
vms %>%
  group_by(year, gear_code) %>%
  summarise(
    y = median(totvalue / totweight, na.rm = TRUE)
  ) %>%
  write.taf("summary_of_median_value_per_weight.csv", dir = "report")
