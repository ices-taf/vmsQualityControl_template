

# install vmstools from github
#devtools::install_github("nielshintzen/vmstools/vmstools")

# libraries
library(rmarkdown)
library(icesTAF)


# create report directory
mkdir("report")

# set up params
report_params <- list(LE_filename = "data/LE.csv",
               VE_filename = "data/VE.csv",
               country = "test")

# run report
render("report.Rmd", params = report_params)

# copy to report folder
cp("report.pdf", paste0("report/QC_", report_params$country, format(Sys.time(), "_%Y-%m-%d_%b-%Y"),".pdf"), move = TRUE)
