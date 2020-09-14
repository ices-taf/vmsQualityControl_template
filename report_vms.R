
library(icesTAF)
taf.library(vmstools)
library(htmlTable)

source("utilities_report.R")

mkdir("report")

load("bootstrap/data/eurPols.Rdata")
polLand <- fortify(eurPols)

ICES_VE <-
  read.table(
    taf.data.path("vms-data", "vms.csv"),
    sep = ",", header = TRUE,
    stringsAsFactors = FALSE, na.strings = "NULL",
    colClasses = c(
      "character", "character",
      "character", "character", "numeric", "numeric",
      "character", "character", "character", "character",
      "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "character"
    )
  )

country <- unique(ICES_VE$country)
ICES_VE <-
  cbind(
    ICES_VE,
    CSquare2LonLat(ICES_VE$c_square, degrees = 0.05)
  )

spatBound <-
  list(
    xrange = range(ICES_VE$SI_LONG, na.rm = TRUE),
    yrange = range(ICES_VE$SI_LATI, na.rm = TRUE)
  )
spatCore <-
  list(
    xrange = quantile(ICES_VE$SI_LONG, c(0.025, 0.975)),
    yrange = quantile(ICES_VE$SI_LATI, c(0.025, 0.975))
  )
tempBound <- range(ICES_VE$year, na.rm = TRUE)



# Quality of the VMS data from `r country`

## Years & number of records for which data has been submitted:
records_per_year <-
  data.frame(
    table(Year = ICES_VE$year)
  )
write.taf(records_per_year, dir = "report")

ggplot(records_per_year) +
  geom_bar(aes(x = Year, y = Freq), stat = "identity") +
  xlab("Year") +
  ylab("Number of records") +
  theme_icesqc()
ggsave("records_per_year.png", path = "report")


## Records assigned to invalid Statistical Rectangles
if (any(is.na(ICES_VE$SI_LONG))) {


  table(`ICES Rectangle` = ICES_VE$ICES_rectangle[is.na(ICES_VE$SI_LONG)],
              Year = ICES_VE$year[is.na(ICES_VE$SI_LONG)])
} else {
  x <- "There were no invalid Statistical Rectangles reported"
  attr(x, "format") <- "markdown"
  attr(x, "class") <- "knit_asis"
  x
}


if (FALSE) {

## Distribution of records by month:
kable(table(ICES_VE$month, ICES_VE$year), booktabs = TRUE)

qplot(factor(year),data=ICES_VE, geom="bar",fill=factor(month)) +
  xlab("Years") + ylab ("Count") + scale_fill_grey(guide=guide_legend(title="Month")) +
  theme_icesqc()

## Spatial extent of data submitted by year:
dat2tab <- as.matrix(aggregate(ICES_VE$SI_LONG,by=list(ICES_VE$year),FUN=range))
dat2tab <- cbind(dat2tab,as.matrix(aggregate(ICES_VE$SI_LATI,by=list(ICES_VE$year),FUN=range)[,-1]))
colnames(dat2tab) <- c("Year", "min lon", "max lon", "min lat", "max lat")
kable(dat2tab, booktabs = TRUE)

## Area for which data has been submitted:
coordGrd <- unique(ICES_VE[, c("SI_LONG", "SI_LATI", "year")])
data_coverage(coordGrd, spatBound, res = 0.5)

data_coverage(coordGrd, spatCore, res = 0.05)

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



## Frequency of vessel length categories by year:
kable(table(ICES_VE$vessel_length_category,ICES_VE$year), booktabs = TRUE)

qplot(factor(year), data = ICES_VE, geom = "bar", fill = factor(vessel_length_category)) +
  xlab("Years") + ylab ("Count") +
  scale_fill_grey(guide = guide_legend(title = "Length category")) +
  theme_icesqc(legend.position = "right")

## Frequency of gear codes by year:
kable(table(ICES_VE$gear_code,ICES_VE$year), booktabs = TRUE)

qplot(factor(year), data = ICES_VE, geom = "bar", fill = factor(gear_code)) +
  xlab("Years") + ylab ("Count") +
  scale_fill_grey(guide = guide_legend(title = "Gear code")) +
  theme_icesqc(legend.position = "right")

## Spatial extend of 3 most dominant gears:

# get 3 main gears
gear_table <- with(ICES_VE, aggregate(fishing_hours, list(gear_code = gear_code), sum, na.rm = TRUE))
gear_table <- gear_table[order(gear_table$x, decreasing = TRUE),]
top3gears <- gear_table$gear_code[1:pmin(3, nrow(gear_table))]

# split by gear
idx <- which(ICES_VE$gear_code %in% top3gears)
coordGrd <- unique(ICES_VE[idx, c("SI_LONG", "SI_LATI", "year", "c_square", "gear_code")])

# create a fortied polygon of csquares : 0.39 secs
polVMS <- make_polVMS(coordGrd)
polVMS$year <- rep(coordGrd$year, each = 5)
polVMS$c_square <- rep(coordGrd$c_square,each=5)
polVMS$gear_code<- rep(coordGrd$gear_code,each=5)

# aggregate fishing days
dat <- with(ICES_VE[idx,],
            aggregate(fishing_hours/24,
                 by = list(year = year, c_square = c_square, gear_code = gear_code),
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
a_by <- apply(dat[c("year","c_square","gear_code")], 1, paste, collapse = ".")
b_by <- apply(polVMS[c("year","c_square","gear_code")], 1, paste, collapse = ".")
ib_by <- as.integer(factor(b_by, levels = a_by))
polVMS$cols <- dat$cols[ib_by]

# do plots
for (gear in top3gears) {
  p <-
    spatialplot(polVMS[polVMS$gear_code == gear,]) +
    scale_fill_manual(values = rev(palette), labels = rev(legval)) +
    guides(fill = guide_legend(title = paste("Days@Sea", gear))) +
    facet_wrap(~ year, ncol = 2) +
    theme_icesqc(legend.position = "top")
  print(p)
}




## Number of unique DCF Level 6 codes by year:

kable(count(unique(ICES_VE[,c("LE_MET_level6","year")]),"year"), booktabs = TRUE)


## Top 5 DCF Level 6 codes by year:

top5Met6 <- names(sort(table(ICES_VE$LE_MET_level6), decreasing = TRUE))
top5Met6 <- top5Met6[1:pmin(5, length(top5Met6))]
kable(table(ICES_VE$LE_MET_level6, ICES_VE$year)[top5Met6,,drop=FALSE], booktabs = TRUE)

qplot(factor(year),
      data = ICES_VE[ICES_VE$LE_MET_level6 %in% top5Met6,],
      geom = "bar", fill = factor(LE_MET_level6)) +
  xlab("Years") + ylab ("Count") +
  scale_fill_grey(guide = guide_legend(title = "Level 6")) +
  theme_icesqc(legend.position = "right")



## Average fishing speed:

kable(do.call(rbind, tapply(ICES_VE$avg_fishing_speed, ICES_VE$year, summary)), booktabs = TRUE)

ggplot(ICES_VE, aes(x = avg_fishing_speed)) +
  geom_histogram() +
  xlab("Average fishing speed") + ylab ("Count") +
  facet_wrap( ~ year, ncol = 2) +
  theme_icesqc()

## Average fishing hours:
kable(do.call(rbind, tapply(ICES_VE$fishing_hours, ICES_VE$year, summary)), booktabs = TRUE)

ggplot(ICES_VE, aes(x = fishing_hours)) +
  geom_histogram() +
  xlab("Average fishing hours") + ylab("Count") +
  facet_wrap( ~ year, ncol = 2) +
  theme_icesqc()



## Average length:

kable(do.call(rbind, tapply(ICES_VE$avg_oal, ICES_VE$year, summary)), booktabs = TRUE)


ggplot(ICES_VE, aes(x = avg_oal))+
  geom_histogram() +
  xlab("Average vessel length") + ylab("Count") +
  facet_wrap( ~ year, ncol = 2) +
  theme_icesqc()


## Average kW:
kable(do.call(rbind, tapply(ICES_VE$avg_kw, ICES_VE$year, summary)), booktabs = TRUE)

ggplot(ICES_VE, aes(x = avg_kw))+
  geom_histogram() +
  xlab("Average kW") + ylab ("Count") +
  facet_wrap( ~ year, ncol = 2) +
  theme_icesqc()



## Average kW-hours:

kable(do.call(rbind, tapply(ICES_VE$kw_fishinghours, ICES_VE$year, summary)), booktabs = TRUE)

ggplot(ICES_VE, aes(x = kw_fishinghours))+
  geom_histogram() +
  xlab("Average kW-hours") + ylab("Count") +
  facet_wrap(~ year, ncol = 2) +
  theme_icesqc()


## Landings by gear by year:
ps <- gear_splits(ICES_VE$totweight, data = ICES_VE, "kg landed", year_groups = 2, gear_groups = 4, func = sum)
ps$table
for (p in ps$plots) print(p)


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


## Mean landing per kW fishing hours by year:
ps <- gear_splits(with(ICES_VE, totweight/kw_fishinghours), data = ICES_VE, "kg/kWh", gear_groups = 4, func = median)
ps$table
for (p in ps$plots) print(p)


## Value by gear by year:
ps <- gear_splits(ICES_VE$totvalue, data = ICES_VE, "EUR landed", gear_groups = 4, func = sum)
ps$table
for (p in ps$plots) print(p)


## Median value per KW fishing hours by year:
ps <- gear_splits(with(ICES_VE, totvalue/kw_fishinghours), data = ICES_VE, "EUR/kWh", gear_groups = 4, func = median)
ps$table
for (p in ps$plots) print(p)


##  Average price:

ps <- gear_splits(with(ICES_VE, totvalue/totweight), data = ICES_VE, "Mean price (EUR/kg)", gear_groups = 4, func = median)
ps$table
for (p in ps$plots) print(p)

}