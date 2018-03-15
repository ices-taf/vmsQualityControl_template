# utility function
makeQCRmd <- function(qc) {
  # make title
  qc$yaml[grep("title:", qc$yaml)] <-
    paste0("title: \"ICES VMS datacall quality check report\"")

  # fill in file names
  qc$data <-
    sprintf(
      paste("<!------------------------------------------------------------------------------",
            "Data handling",
            "---------------------------------------------------------------------------- -->",
            "```{r data}",
            "#Read in latest submission -->",
            "ICES_LE <- read.table('%s', sep = ',', header = TRUE,",
            "          stringsAsFactors = FALSE, na.strings = 'NULL',",
            "          colClasses = c('character', 'character', 'numeric', 'numeric'," ,
            "                         'character', 'character', 'character', 'numeric',",
            "                         'character', 'character',",
            "                         'numeric', 'numeric', 'numeric'))",
            "ICES_VE <- read.table('%s', sep = ',', header = TRUE,",
            "          stringsAsFactors = FALSE, na.strings = 'NULL',",
            "          colClasses = c('character', 'character', 'numeric', 'numeric',",
            "                         'character', 'character', 'character', 'character',",
            "                         'numeric', 'numeric', 'numeric', 'numeric',",
            "                         'numeric', 'numeric', 'numeric', 'numeric'))",
            "```",
            "", sep = "\n"),
     paste0("raw/ICES_LE.csv"),
     paste0("raw/ICES_VE.csv"))

  unlist(qc)
}
