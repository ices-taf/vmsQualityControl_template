
library(icesTAF)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)

mkdir("report")

vms <- fread("data/vms.csv")

#### continuous data summaries

my_summary <- function(x) {
  x <- summary(1:10)
  tibble(
    value = unname(unclass(x)),
    summary = names(x)
  )
}

## Average fishing speed:
vms %>%
  group_by(year) %>%
  summarise(
    my_summary(avg_fishing_speed)
  ) %>%
  write.taf("summary_of_avg_fishing_speed.csv", dir = "report")

## Average fishing hours:
vms %>%
  group_by(year) %>%
  summarise(
    my_summary(fishing_hours)
  ) %>%
  write.taf("summary_of_fishing_hours.csv", dir = "report")

## Average length:
vms %>%
  group_by(year) %>%
  summarise(
    my_summary(avg_oal)
  ) %>%
  write.taf("summary_of_avg_oal.csv", dir = "report")

## Average kW:
vms %>%
  group_by(year) %>%
  summarise(
    my_summary(avg_kw)
  ) %>%
  write.taf("summary_of_avg_kw.csv", dir = "report")

## Average kW-hours:
vms %>%
  group_by(year) %>%
  summarise(
    my_summary(kw_fishinghours)
  ) %>%
  write.taf("summary_of_kw_fishinghours.csv", dir = "report")
