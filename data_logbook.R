
library(icesTAF)
library(data.table)
library(vmstools)
library(dplyr)

mkdir("data")

logbook <-
  fread(
    taf.data.path("vms-data", "logbook.csv")
  )

numerics <-
  c(
    "fishing_days", "kw_fishing_days", "totweight", "totvalue"
  )

characters <-
  c(
    "recordtype", "country", "ICES_rectangle", "vessel_length_category",
    "gear_code", "LE_MET_level6", "vms_enabled", "AnonVessels"
  )

integers <-
  c(
    "year", "month", "UniqueVessels"
  )

logbook[, (numerics) := lapply(.SD, as.numeric), .SDcols = numerics]
logbook[, (characters) := lapply(.SD, as.character), .SDcols = characters]
logbook[, (integers) := lapply(.SD, as.integer), .SDcols = integers]

logbook <-
  cbind(
    logbook,
    ICESrectangle2LonLat(logbook$ICES_rectangle, midpoint = TRUE)
  )


logbook <-
  logbook %>%
  mutate(
    wkt =
      paste0(
        "POLYGON((",
        SI_LONG + 0.025, " ", SI_LATI + 0.025, ", ",
        SI_LONG - 0.025, " ", SI_LATI + 0.025, ", ",
        SI_LONG - 0.025, " ", SI_LATI - 0.025, ", ",
        SI_LONG + 0.025, " ", SI_LATI - 0.025, ", ",
        SI_LONG + 0.025, " ", SI_LATI + 0.025, "))"
      )
  )

write.taf(logbook, dir = "data", quote = TRUE)
