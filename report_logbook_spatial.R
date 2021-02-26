
if (FALSE) {

## Area extent of data submitted by year:

dat2tab <- as.matrix(aggregate(ICES_LE$SI_LONG, by = list(year = ICES_LE$year), FUN = range, na.rm = TRUE))
dat2tab <- cbind(dat2tab, as.matrix(aggregate(ICES_LE$SI_LATI, by = list(ICES_LE$year), FUN = range, na.rm = TRUE)[, -1]))
colnames(dat2tab) <- c("Year", "min lon", "max lon", "min lat", "max lat")
kable(dat2tab, booktabs = TRUE)


## Area for which data has been submitted:

coordGrd <- unique(ICES_LE[, c("SI_LONG", "SI_LATI", "year")])
data_coverage(coordGrd, spatBoundLog, res = 1)


data_coverage(coordGrd, spatCore, res = 1)





## Spatial distribution of effort by year:
coordGrd        <- unique(ICES_LE[,c("SI_LONG","SI_LATI","year","ICES_rectangle")])

polRect  <- make_polVMS(coordGrd, resolution = 1)
polRect$year    <- rep(coordGrd$year,each=5)
polRect$ICES_rectangle <- rep(coordGrd$ICES_rectangle, each = 5)

#TIDY ---
dat             <- aggregate(ICES_LE$fishing_days,by=list(ICES_LE$year,ICES_LE$ICES_rectangle),FUN=sum,na.rm=T)

steps               <- ceiling(max(dat$x,na.rm=T)/250)
cutbreaksval        <- unique(c(-1,0,steps*c(1,2.5,5,10,25,50,100,250)))
legval              <- outer(ac(cutbreaksval),ac(cutbreaksval),function(x,y){return(paste(x,"<=",y))})
legval              <- c("0",diag(legval[-1,-c(1,2)]))
palette <- c("white", brewer.pal(length(cutbreaksval)-2,"YlOrRd"))
cols <- palette[cut(dat$x,breaks=cutbreaksval)]
cols                <- cbind(cols,id=1:length(cols),ICES_rectangle=dat$Group.2,year=dat$Group.1)
polRect              <- merge(polRect,cols,by=c("ICES_rectangle","year"))
# ----

spatialplot(polRect) +
  guides(fill = guide_legend(title = "Days@Sea")) +
  scale_fill_manual(values = rev(palette), labels = rev(legval)) +
  facet_wrap(~ year, ncol = 2) +
  theme_icesqc(legend.position = "top")


## Spatial difference of effort `r tempBound[1]`:`r (tempBound[2]-1)` vs `r tempBound[2]`

base <- with(ICES_LE,
             aggregate(fishing_days,
                       by = list(ICES_rectangle = ICES_rectangle, year = year),
                       FUN = sum, na.rm = TRUE))
base <- dplyr::rename(base, fishing_days = x)

# calculate total fishing hours for recent year
recent <- base[base$year == tempBound[2],]

# calculate median of the total fishing hours per square for historical years
base <- with(base[base$year < tempBound[2],],
             aggregate(fishing_days,
                       by = list(ICES_rectangle = ICES_rectangle),
                       FUN = median, na.rm = TRUE))
base <- dplyr::rename(base, fishing_days_median = x)

# join
dat2plot <- dplyr::full_join(base,
                             recent[,c("ICES_rectangle","fishing_days")])

# set NAs to zero
dat2plot$fishing_days_median[is.na(dat2plot$fishing_days_median)] <- 0
dat2plot$fishing_days[is.na(dat2plot$fishing_days)] <- 0

# calculate ratio (with exceptions for zeros)
dat2plot$ratio <- 1/with(dat2plot, pmax(fishing_days, 1e-9) / pmax(fishing_days_median, 1e-9))

# add back in lat and long
dat2plot <- cbind(dat2plot,
                  vmstools::ICESrectangle2LonLat(dat2plot$ICES_rectangle, midpoint = TRUE))

# make 'fortified' data frame
polRect <- make_polVMS(dat2plot, resolution = 1)
polRect$ICES_rectangle <- rep(dat2plot$ICES_rectangle, each = 5)

## tidy ---
breaks <- rev(c(1e-10,0.5,2/3,0.8,0.952381,1,1.05,1.25,1.5,2,1e10))
legval <- c("historic >>","historic> +100%","historic> +50%","historic> +25%",
            "+/-5%",
            "recent> +5%","recent> +25%","recent> +50%","recent> +100%","recent >>")
palette <- brewer.pal(length(cutbreaksval)-1,"RdYlBu")
colgrp <- as.numeric(cut(dat2plot$ratio, breaks = breaks))
cols <- cbind(cols = palette[colgrp], ICES_rectangle = dat2plot$ICES_rectangle)
polRect <- merge(polRect, cols, by=c("ICES_rectangle"))
# ---

spatialplot(polRect) +
    guides(fill=guide_legend(title="Days@Sea")) +
    scale_fill_manual(values = rev(palette), labels = legval) +
    theme_icesqc(legend.position = "right")


## Spatial difference of effort `r tempBound[2]-1` vs `r tempBound[2]`

base <- with(ICES_LE,
             aggregate(fishing_days,
                       by = list(ICES_rectangle = ICES_rectangle, year = year),
                       FUN = sum, na.rm = TRUE))
base <- dplyr::rename(base, fishing_days = x)

# calculate total fishing hours for recent year
recent <- base[base$year == tempBound[2],]

# previous year
base <- base[base$year == tempBound[2]-1,]
base <- dplyr::rename(base, fishing_days_median = fishing_days)

# join
dat2plot <- dplyr::full_join(base,
                             recent[,c("ICES_rectangle","fishing_days")])

# set NAs to zero
dat2plot$fishing_days_median[is.na(dat2plot$fishing_days_median)] <- 0
dat2plot$fishing_days[is.na(dat2plot$fishing_days)] <- 0

# calculate ratio (with exceptions for zeros)
dat2plot$ratio <- 1/with(dat2plot, pmax(fishing_days, 1e-9) / pmax(fishing_days_median, 1e-9))

# add back in lat and long
dat2plot <- cbind(dat2plot,
                  vmstools::ICESrectangle2LonLat(dat2plot$ICES_rectangle, midpoint = TRUE))

# make 'fortified' data frame
polRect <- make_polVMS(dat2plot, resolution = 1)
polRect$ICES_rectangle <- rep(dat2plot$ICES_rectangle, each = 5)

## tidy ---
breaks <- rev(c(1e-10,0.5,2/3,0.8,0.952381,1,1.05,1.25,1.5,2,1e10))
legval <- c("historic >>","historic> +100%","historic> +50%","historic> +25%",
            "+/-5%",
            "recent> +5%","recent> +25%","recent> +50%","recent> +100%","recent >>")
palette <- brewer.pal(length(cutbreaksval)-1,"RdYlBu")
colgrp <- as.numeric(cut(dat2plot$ratio, breaks = breaks))
cols <- cbind(cols = palette[colgrp], ICES_rectangle = dat2plot$ICES_rectangle)
polRect <- merge(polRect, cols, by=c("ICES_rectangle"))
# ---

spatialplot(polRect) +
    guides(fill=guide_legend(title="Days@Sea")) +
    scale_fill_manual(values = rev(palette), labels = legval) +
    theme_icesqc(legend.position = "right")




}