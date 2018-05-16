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
