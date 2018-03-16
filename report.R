## Prepare plots/tables for report

## Before:
## After:

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

# packages
library(icesTAF)
library(rmarkdown)

# create report directory
mkdir("report")

# run template
render("report.Rmd")

# clean up
cp(report.pdf, "report", move = TRUE)
unlink("report.tex")
unlink("report_files", recursive = TRUE)

# done!
