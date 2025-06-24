## Setup ==========================================================
suppressPackageStartupMessages({
    # library(JAPackage)
    library(mNIRS)
    # library(tidyverse)
})

options(digits = 5, digits.secs = 3, scipen = 3,
        dplyr.summarise.inform = FALSE,
        tibble.print_min = 20)

# camcorder::gg_record(
#   # dir = "ggplots",
#   width = 220,
#   height = 220*2/3,
#   dpi = 300,
#   units = "mm",
#   device = "png",
#   bg = "white")
# camcorder::gg_stop_recording()
#
## BUILD =======================================
# library(devtools)
#
# usethis::use_mit_license()
#
# use_r("read_data")
# load_all()
#
# exists("read_data", where = globalenv(), inherits = FALSE)
#
# tools::showNonASCIIfile("C:/R-Projects/mNIRS/R/process_data.R")
# document()
# ?read_data
# install()
# use_readme_rmd()
devtools::build_readme()

# usethis::use_import_from("tidyr", "any_of")
devtools::check()

