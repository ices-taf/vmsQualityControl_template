
library(icesTAF)
library(data.table)
library(vmstools)
library(dplyr)

mkdir("data")

vms <-
  fread(
    taf.data.path("vms-data", "vms.csv")
  )

numerics <-
  c(
    "avg_fishing_speed", "fishing_hours", "avg_oal", "avg_kw",
    "kw_fishinghours", "totweight", "totvalue",
    "ICES_avg_fishing_speed", "avg_gearWidth"
  )

characters <-
  c(
    "recordtype", "country", "c_square", "vessel_length_category",
    "gear_code", "LE_MET_level6", "AnonVessels"
  )

integers <-
  c(
    "year", "month", "UniqueVessels"
  )

vms[, (numerics) := lapply(.SD, as.numeric), .SDcols = numerics]
vms[, (characters) := lapply(.SD, as.character), .SDcols = characters]
vms[, (integers) := lapply(.SD, as.integer), .SDcols = integers]

vms <-
  cbind(
    vms,
    CSquare2LonLat(vms$c_square, degrees = 0.05)
  )

vms <-
  vms %>%
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

write.taf(vms, dir = "data", quote = TRUE)
