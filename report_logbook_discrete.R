
library(icesTAF)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)

options(dplyr.summarise.inform = FALSE)

mkdir("report")

logbook <- fread("data/logbook.csv")

#### character data summaries by year

## Years & number of records for which data has been submitted:
logbook %>%
  count(year) %>%
  write.taf(file = "logbook_records_per_year.csv", dir = "report")


## Records assigned to invalid Statistical Rectangles
logbook %>%
  group_by(year, ICES_rectangle) %>%
  summarise(n = sum(is.na(SI_LONG))) %>%
  filter(n > 0) %>%
  write.taf(file = "logbook_invalid_statrecs.csv", dir = "report")

## Distribution of records by month:
logbook %>%
  count(month, year) %>%
  write.taf("logbook_records_per_month.csv", dir = "report")

## Frequency of vessel length categories by year:
logbook %>%
  count(vessel_length_category, year) %>%
  write.taf("logbook_records_per_vessel_length_cat.csv", dir = "report")

## Frequency of gear codes by year:
logbook %>%
  count(gear_code, year) %>%
  write.taf("logbook_records_per_gear_code.csv", dir = "report")

## Frequency of VMS enabled category
logbook %>%
  count(vms_enabled, vessel_length_category, year) %>%
  write.taf("logbook_vms_enabled_per_length_category.csv", dir = "report")


## Number of unique DCF Level 6 codes by year:
logbook %>%
  group_by(year) %>%
  summarise(n = length(unique(LE_MET_level6))) %>%
  write.taf("logbook_unique_level6_code_by_year.csv", dir = "report")


## Top 5 DCF Level 6 codes by year:
top5Met6 <-
  logbook %>%
  count(LE_MET_level6) %>%
  arrange(desc(n)) %>%
  select(LE_MET_level6) %>%
  head(5) %>%
  `[[`(1)

logbook %>%
  filter(LE_MET_level6 %in% top5Met6) %>%
  count(LE_MET_level6, year) %>%
  write.taf("logbook_records_per_metier_level_6.csv", dir = "report")





if (FALSE) {

## Distribution of number of unique vessels per stat-square:

try(
  barplot(
    prop.table(
      table(
        factor(ICES_LE$UniqueVessels)
      )
    ),
    main = "unnagregated",
    ylab = "Proportion of c-squares with given number of unique vessels",
    xlab = "number of unique vessels"
  )
)


tmp <- ICES_LE %>%
  dplyr::group_by(year, ICES_rectangle) %>%
  dplyr::summarise(
    new_count =
      ifelse(
        any(UniqueVessels > 2),
        3,
        length(unique(unlist(strsplit(AnonVessels, ";"))))
      )
  ) %>%
  dplyr::mutate(
    new_count = ifelse(new_count >= 3, "3+", paste(new_count))
  )

try(
  barplot(
    prop.table(
      table(factor(tmp$new_count))
    ),
    main = "aggregated to year level",
    ylab = "Proportion of stat-squares with given number of unique vessels",
    xlab = "number of unique vessels"
  )
)

tmp <- ICES_LE %>%
  dplyr::group_by(ICES_rectangle) %>%
  dplyr::summarise(
    new_count =
      ifelse(
        any(UniqueVessels > 2),
        3,
        length(unique(unlist(strsplit(AnonVessels, ";"))))
      )
  ) %>%
  dplyr::mutate(
    new_count = ifelse(new_count >= 3, "3+", paste(new_count))
  )

try(
  barplot(
    prop.table(
      table(factor(tmp$new_count))
    ),
    main = "fully aggregated",
    ylab = "Proportion of stat-squares with given number of unique vessels",
    xlab = "number of unique vessels"
  )
)

}
