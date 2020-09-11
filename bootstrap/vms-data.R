#' download VMS and logbook data
#'
#' direct connection with DB do download VMS data for the required
#' country
#'
#' @name vms-data
#' @format csv files
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Restricted
#' @tafSource script

# packages
library(RODBC)
library(icesTAF)
library(jsonlite)

# settings
config <- read_json(taf.data.path("config.json"), simplifyVector = TRUE)

# connect to DB
conn <- odbcDriverConnect(connection = config$db_connection)

msg("downloading LE data for ... ", config$country)

# set up sql command
sqlq <- sprintf("SELECT * FROM dbo._ICES_VMS_Datacall_LE WHERE country = '%s' order by year, ICES_rectangle, gear_code, LE_MET_level6, month, vessel_length_category, fishing_days, vms_enabled", config$country)

# fetch
logbook <- sqlQuery(conn, sqlq)

# save to file
write.taf(logbook)

msg("downloading VMS data for ... ", config$country)

# set up sql command
sqlq <- sprintf("SELECT * FROM [dbo].[_ICES_VMS_Datacall_VMS] WHERE country = '%s' order by year, c_square, gear_code, LE_MET_level6, month, vessel_length_category, fishing_hours, avg_fishing_speed", config$country)

# fetch
vms <- sqlQuery(conn, sqlq)
# save to file
write.taf(vms)

# disconnect
odbcClose(conn)
