
library(icesTAF)
library(data.table)
library(ggplot2)
library(sf)
library(dplyr)

source("utilities_report.R")

mkdir("report")

vms <- fread("data/vms.csv")


#### spatial stuff


## Spatial extent of data submitted by year:
coverage <- unique(vms[, c("SI_LONG", "SI_LATI", "year", "wkt")])

spatial_extent_by_year <-
  cbind(
    as.matrix(
      aggregate(
        coverage$SI_LONG,
        by = list(coverage$year),
        FUN = range
      )
    ),
    as.matrix(
      aggregate(
        coverage$SI_LATI,
        by = list(coverage$year),
        FUN = range
      )[, -1]
    )
  )
colnames(spatial_extent_by_year) <- c("Year", "min lon", "max lon", "min lat", "max lat")
write.taf(spatial_extent_by_year, dir = "report")

## Area for which data has been submitted:
coverage$wkt <- sf::st_as_sfc(coverage$wkt)
coverage <- sf::st_sf(coverage, sf_column_name = "wkt", crs = 4326)

coverage <-
  coverage %>%
  group_by(year) %>%
  summarise(
    cover = sf::st_union(wkt)
  )

st_write(coverage, "report/coverage.geojson")


## Spatial extend of 3 most dominant gears:
# get 3 main gears
gear_table <-
  vms %>%
  group_by(gear_code) %>%
  summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE)) %>%
  arrange(desc(fishing_hours)) %>%
  head(3)

top3gears <- gear_table$gear_code

# legend calculations
groups <- function(x) {
  steps <- ceiling(max(x, na.rm = TRUE) / 250)
  breaks <- unique(c(-1, 0, steps * c(1, 2.5, 5, 10, 25, 50, 100, 250)))
  legval <- paste(breaks[-length(breaks)], "<=", breaks[-1L])
  legval[1] <- "0"
  ids <- as.numeric(cut(x, breaks = breaks))
  factor(legval[ids], levels = legval)
}

gears_coverage <-
  vms %>%
  filter(gear_code %in% top3gears) %>%
  group_by(year, gear_code, wkt) %>%
  summarise(
    fishing_days = sum(fishing_hours, na.rm = TRUE) / 24
  ) %>%
  group_by(year, gear_code, wkt) %>%
  summarise(
    fishing_days = groups(fishing_days)
  ) %>%
  mutate(
    wkt = st_as_sfc(wkt)
  ) %>%
  group_by(year, gear_code, fishing_days) %>%
  summarise(
    wkt = sf::st_union(wkt)
  ) %>%
  mutate(
    fillColour = as.integer(fishing_days)
  ) %>%
  st_sf(sf_column_name = "wkt", crs = 4326)

st_write(gears_coverage, "report/gears_coverage.geojson")




if (FALSE) {

## Spatial distribution of effort by year:
coordGrd <- unique(ICES_VE[,c("SI_LONG","SI_LATI","year","c_square")])
# make fortified DF for csquares
polVMS <- make_polVMS(coordGrd)
polVMS$year     <- rep(coordGrd$year, each = 5)
polVMS$c_square <- rep(coordGrd$c_square, each = 5)

# aggregate fishing days
dat <- with(ICES_VE,
            aggregate(fishing_hours/24,
                 by = list(year = year, c_square = c_square),
                 FUN = sum, na.rm = TRUE))
dat <- dplyr::rename(dat, fishing_days = x)

# legend calculations
steps <- ceiling(max(dat$fishing_days, na.rm = TRUE)/250)
breaks <- unique(c(-1, 0, steps * c(1, 2.5, 5, 10, 25, 50, 100, 250)))
legval <- paste(breaks[-length(breaks)], "<=", breaks[-1L])
legval[1] <- "0"

palette <- c("white", RColorBrewer::brewer.pal(length(breaks)-2, "YlOrRd"))
dat$colgrp <- as.numeric(cut(dat$fishing_days, breaks = breaks))
dat$cols <- palette[dat$colgrp]

# join dat onto polVMS (much faster than left_join or merge)
# make joining keys
a_by <- apply(dat[c("year","c_square")], 1, paste, collapse = ".")
b_by <- apply(polVMS[c("year","c_square")], 1, paste, collapse = ".")
ib_by <- as.integer(factor(b_by, levels = a_by))
polVMS$cols <- dat$cols[ib_by]

# do plots
spatialplot(polVMS) +
  scale_fill_manual(values = rev(palette), labels = rev(legval)) +
  guides(fill = guide_legend(title = "Days@Sea")) +
  facet_wrap(~ year, ncol = 2) +
  theme_icesqc(legend.position = "top")


## Spatial difference of effort `r tempBound[1]`:`r (tempBound[2]-1)` vs `r tempBound[2]`
base <- with(ICES_VE,
             aggregate(fishing_hours,
                       by = list(c_square = c_square, year = year),
                       FUN = sum, na.rm = TRUE))
base <- dplyr::rename(base, fishing_hours = x)

# calculate total fishing hours for recent year
recent <- base[base$year == tempBound[2],]

# calculate median of the total fishing hours per square for historical years
base <- with(base[base$year < tempBound[2],],
             aggregate(fishing_hours,
                       by = list(c_square = c_square),
                       FUN = median, na.rm = TRUE))
base <- dplyr::rename(base, fishing_hours_median = x)

# join
dat2plot <- dplyr::full_join(base,
                             recent[,c("c_square","fishing_hours")])

# set NAs to zero
dat2plot$fishing_hours_median[is.na(dat2plot$fishing_hours_median)] <- 0
dat2plot$fishing_hours[is.na(dat2plot$fishing_hours)] <- 0

# calculate ratio (with exceptions for zeros)
dat2plot$ratio <- 1/with(dat2plot, pmax(fishing_hours, 1e-9) / pmax(fishing_hours_median, 1e-9))

# add back in lat and long
dat2plot <- cbind(dat2plot,
                  vmstools::CSquare2LonLat(dat2plot$c_square, degrees = 0.05))

# make 'fortified' data frame
polVMS <- make_polVMS(dat2plot)
polVMS$c_square <- rep(dat2plot$c_square, each = 5)

# set up legend
# This is not what the legend says.... there is no +/-5% break!
breaks <- c(-Inf, 1/2, 1/1.5, 1/1.25, 1/1.05, 1, 1.05, 1.25, 1.5, 2, Inf)
legval <- c("historic >>","historic> +100%","historic> +50%","historic> +25%",
            "+/-5%",
            "recent> +5%","recent> +25%","recent> +50%","recent> +100%","recent >>")
palette <- RColorBrewer::brewer.pal(length(breaks)-1,"RdYlBu")  #colour for fishing intensity
dat2plot$colgrp <- as.numeric(cut(dat2plot$ratio, breaks = breaks))
dat2plot$cols <- palette[dat2plot$colgrp]

# join dat onto polVMS (much faster than left_join or merge)
# make joining keys
a_by <- dat2plot$c_square
b_by <- polVMS$c_square
ib_by <- as.integer(factor(b_by, levels = a_by))
polVMS$cols <- dat2plot$cols[ib_by]
# NOTE dark blue here just means that nothing was observered before, and now there is something...
# - could be 1 hour of fishing
grps <- sort(unique(dat2plot$colgrp))
spatialplot(polVMS) +
  guides(fill = guide_legend(title = "Days@Sea")) +
  scale_fill_manual(values = rev(palette)[grps], labels = legval[grps]) +
  theme_icesqc(legend.position = "right")


## Spatial difference of effort `r tempBound[2]-1` vs `r tempBound[2]`

base <- with(ICES_VE,
             aggregate(fishing_hours,
                       by = list(c_square = c_square, year = year),
                       FUN = sum, na.rm = TRUE))
base <- dplyr::rename(base, fishing_hours = x)

# calculate total fishing hours for recent year
recent <- base[base$year == tempBound[2],]

# previous year
base <- base[base$year == tempBound[2]-1,]
base <- dplyr::rename(base, fishing_hours_median = fishing_hours)

# join
dat2plot <- dplyr::full_join(base,
                             recent[,c("c_square","fishing_hours")])

# set NAs to zero
dat2plot$fishing_hours_median[is.na(dat2plot$fishing_hours_median)] <- 0
dat2plot$fishing_hours[is.na(dat2plot$fishing_hours)] <- 0

# calculate ratio (with exceptions for zeros)
dat2plot$ratio <- 1/with(dat2plot, pmax(fishing_hours, 1e-9) / pmax(fishing_hours_median, 1e-9))

# add back in lat and long
dat2plot <- cbind(dat2plot,
                  vmstools::CSquare2LonLat(dat2plot$c_square, degrees = 0.05))

# make 'fortified' data frame
polVMS <- make_polVMS(dat2plot)
polVMS$c_square <- rep(dat2plot$c_square, each = 5)

# set up legend
# This is not what the legend says.... there is no +/-5% break!
breaks <- c(-Inf, 1/2, 1/1.5, 1/1.25, 1/1.05, 1, 1.05, 1.25, 1.5, 2, Inf)
legval <- c("historic >>","historic> +100%","historic> +50%","historic> +25%",
            "+/-5%",
            "recent> +5%","recent> +25%","recent> +50%","recent> +100%","recent >>")
palette <- RColorBrewer::brewer.pal(length(breaks)-1,"RdYlBu")  #colour for fishing intensity
dat2plot$colgrp <- as.numeric(cut(dat2plot$ratio, breaks = breaks))
dat2plot$cols <- palette[dat2plot$colgrp]

# join dat onto polVMS (much faster than left_join or merge)
# make joining keys
a_by <- dat2plot$c_square
b_by <- polVMS$c_square
ib_by <- as.integer(factor(b_by, levels = a_by))
polVMS$cols <- dat2plot$cols[ib_by]
# NOTE dark blue here just means that nothing was observered before, and now there is something...
# - could be 1 hour of fishing
grps <- sort(unique(dat2plot$colgrp))
spatialplot(polVMS) +
  guides(fill = guide_legend(title = "Days@Sea")) +
  scale_fill_manual(values = rev(palette)[grps], labels = legval[grps]) +
  theme_icesqc(legend.position = "right")




}


if (FALSE) {
  ## Distribution of number of unique vessels per c-square:

  try(
    barplot(
      prop.table(
        table(
          factor(ICES_VE$UniqueVessels)
        )
      ),
      main = "unnagregated",
      ylab = "Proportion of c-squares with given number of unique vessels",
      xlab = "number of unique vessels"
    )
  )

  tmp <- ICES_VE %>%
    dplyr::group_by(year, c_square) %>%
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
      ylab = "Proportion of c-squares with given number of unique vessels",
      xlab = "number of unique vessels"
    )
  )

  tmp <- ICES_VE %>%
    dplyr::group_by(c_square) %>%
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
      ylab = "Proportion of c-squares with given number of unique vessels",
      xlab = "number of unique vessels"
    )
  )
}
