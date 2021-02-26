


if (FALSE) {


## Average fishing days:

ggplot(ICES_LE, aes(x = fishing_days))+
  geom_histogram() +
  xlab("Average fishing days") + ylab("Count") +
  facet_wrap(~ year, ncol = 2) +
  theme_icesqc()


## Average KW-hours:

ggplot(ICES_LE, aes(x = kw_fishing_days)) +
  geom_histogram() +
  xlab("Average KW-days") + ylab("Count") +
  facet_wrap(~ year, ncol = 2) +
  theme_icesqc()

}
