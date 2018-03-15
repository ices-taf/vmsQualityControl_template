## Prepare plots/tables for report

## Before:
## After:

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

# packages
library(icesTAF)
library(rmarkdown)

# source utility functions
source("utils.R")

# read and parse template file
qc <- readLines("report.Rmd")
loc1 <- grep("<!-- QCTEMPLATE: header -->", qc)
loc2 <- grep("<!-- QCTEMPLATE: data -->", qc)
loc3 <- grep("<!-- QCTEMPLATE: body -->", qc)
qc <- list(yaml = qc[1:(loc1-1)],
           header = qc[loc1:(loc2-1)],
           data = qc[loc2:(loc3-1)],
           body = qc[loc3:length(qc)])

# create report directory
mkdir("report")

# setup file name
fname <- paste0("report_QC", format(Sys.time(), "_%Y-%m-%d_%b-%Y"),".Rmd")

# fillin and write template
cat(makeQCRmd(qc), sep = "\n", file = fname)

# run template
render(fname, output_dir = "report")

# done!
