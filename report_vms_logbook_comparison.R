if (FALSE) {
  ## Comparison of Metier level 6 reporting between logbook and VMS

ledat <- with(ICES_LE, table(LE_MET_level6, year))
vedat <- with(ICES_VE, table(LE_MET_level6, year))

dat2tab <-
  rbind(
    cbind(as.data.frame.table(ledat), data = "LE (records)"),
    cbind(as.data.frame.table(vedat), data = "VE (pings)")
  )

tab <- with(dat2tab, tapply(Freq, list(LE_MET_level6, data, year), sum))
tab[tab == 0] <- NA

for (i in dimnames(tab)[[3]]) {
  x <- tab[, , i]
  x <- x[apply(!is.na(x), 1, any), ]
  cat(kable(cbind(x, year = i), booktabs = TRUE), sep = "\n")
  cat("\n")
}

}