
if (FALSE) {
## Landings by gear by year:

ps <- gear_splits(ICES_LE$totweight, data = ICES_LE, "kg landed", gear_groups = 4, func = sum)
ps$table
for (p in ps$plots) print(p)


## Relationship fishing days and total weight

ggplot(
  ICES_LE[ICES_LE$year == tempBoundLog[2], ],
  aes(x = fishing_days, y = totweight)
) +
  geom_point() +
  facet_wrap(~gear_code, ncol = 3, scale = "free") +
  xlab("Fishing days") +
  ylab("Total weight") +
  theme_icesqc()


## Mean landing per KW fishing day by year:


ps <- gear_splits(with(ICES_LE, totweight / kw_fishing_days), data = ICES_LE, "kg landed", gear_groups = 4, func = median)
ps$table
for (p in ps$plots) print(p)


## Value by gear by year:

ps <- gear_splits(with(ICES_LE, totvalue), data = ICES_LE, "EUR landed", gear_groups = 4, func = median)
ps$table
for (p in ps$plots) print(p)


## Mean value per KW fishing day by year:


ps <- gear_splits(with(ICES_LE, totvalue / kw_fishing_days), data = ICES_LE, "EUR/kWh", gear_groups = 4, func = median)
ps$table
for (p in ps$plots) print(p)



##  Average price:

ps <- gear_splits(with(ICES_LE, totvalue / totweight), data = ICES_LE, "Mean price (EUR/kg)", gear_groups = 4, func = median)
cat(ps$table)
for (p in ps$plots) print(p)

}