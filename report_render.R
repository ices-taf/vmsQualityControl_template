# libraries
library(rmarkdown)
library(icesTAF)

# create report directory
mkdir("report")

# render Rmd
ret <- try(render("report.Rmd"))

try(cp("report.html", "report", move = TRUE))

# copy disclaimer into report folder
# cp("bootstrap/data/Disclaimer.txt", "report")
