# vms_quality_control
Generate a quality control report

to run follow these steps:
1. create or copy annex 1 and annex 2 data files to the `data` folder.  This can be automated in the `data.R`script
2. run 
``` r
source("report.R")
```

## requirements

From CRAN
* rmarkdown
* icesTAF
* plyr
* ggplot2
* vmstools
* RColorBrewer
* doBy
* reshape2
* knitr
* kableExtra

from GitHub
* [nielshintzen/vmstools/vmstools](https://github.com/nielshintzen/vmstools)

### expected file format: CSV (comma separated)

with the following headings:

LE format:
```r
recordtype	country	year	month	ICES_rectangle	gear_code	LE_MET_level6	fishing_days	vessel_length_category	vms_enabled	kw_fishing_days	totweight	totvalue
```
VE format:
```r
recordtype	country	year	month	c_square	vessel_length_category	gear_code	LE_MET_level6	avg_fishing_speed	fishing_hours	avg_oal	avg_kw	kw_fishinghours	totweight	totvalue	ICES_avg_fishing_speed	avg_gearWidth
```
please use `NA` where data is not available
