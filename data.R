## Preprocess data, write TAF data tables

## Before: not
## After:

# packages
library(RODBC)
library(icesTAF)

# create directories
mkdir("data")

# settings
dbConnection <- 'Driver={SQL Server};Server=SQL06;Database=VMS;Trusted_Connection=yes'


# connect to DB
conn <- odbcDriverConnect(connection = dbConnection)

# get submitted countries
country <- 'GBR'

msg("downloading LE data for ... ", country, "\n")

# set up sql command
sqlq <- sprintf("SELECT * FROM dbo._2017_ICES_VMS_Datacall_LE WHERE country = '%s'", country)
fname <- paste0("data/ICES_LE_", country, ".csv")

# fetch
out <- sqlQuery(conn, sqlq)
# save to file
write.taf(out, fname)


msg("downloading VMS data for ... ", country, "\n")

# set up sql command
sqlq <- sprintf("SELECT * FROM [dbo].[_2017_ICES_VMS_Datacall_VMS] WHERE country = '%s'", country)
fname <- paste0("data/ICES_VE_", country, ".csv")

# fetch
out <- sqlQuery(conn, sqlq)
# save to file
write.taf(out, fname)

# disconnect
odbcClose(conn)
