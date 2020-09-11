# libraries
library(rmarkdown)
library(icesTAF)
library(jsonlite)

# create report directory
mkdir("report")

# utiities
source("utilities_qc.R")

# settings
config <- read_json(taf.data.path("config.json"), simplifyVector = TRUE)

msg("Running QC for ... ", config$country)

# fillin and write template
makeQCRmd(config$country, taf.data.path("vms-data"), template = "report_QC_template.Rmd")

# render Rmd
ret <- try(render("report.Rmd", clean = FALSE, output_format = latex_document()))
if (inherits(ret, "try-error")) {
  msg("FAILED - ", country)
}

# compile pdf
x <- shell(paste("pdflatex -halt-on-error", ret))

if (x == 0) {
  # copy report file
  cp("report.pdf", "report", move = TRUE)
}

# copy Rmd file
cp("report.Rmd", "report", move = TRUE)

# copy disclaimer into report folder
# cp("bootstrap/data/Disclaimer.txt", "report")
