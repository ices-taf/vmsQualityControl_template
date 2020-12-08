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
library(icesTAF)
taf.library(icesVMS)
library(jsonlite)

# NOTE TO USER: check a token is present
# icesConnect::ices_token()

# settings
config <- read_json(taf.data.path("config.json"), simplifyVector = TRUE)

# download logbook from web service
msg("downloading LE data for ... ", config$country)
logbook <- get_logbook(country = config$country, year = 2009:(config$year-1)) # add data call year

# save to file
write.taf(logbook)

# download VMS from web service
msg("downloading VMS data for ... ", config$country)
vms <- get_vms(country = config$country, year = 2009:(config$year-1), datacall = config$year)

# save to file
write.taf(vms)
