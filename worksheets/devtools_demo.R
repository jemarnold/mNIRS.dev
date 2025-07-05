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

# usethis::use_package_doc()
# usethis::use_github_links()
# usethis::use_tidy_description()
usethis::use_package("rlang")
usethis::use_package("purrr", type = "suggest")

usethis::use_import_from("stats", c("na.omit"))

usethis::use_vignette("processing-mNIRS-data.qmd")

usethis::use_test("replace_fixed_values")
devtools::load_all()
devtools::test()

## Building R packages with devtools and usethis | RStudio ================
## https://www.youtube.com/watch?v=EpTkT6Rkgbs
MVP minimum viable package
- metadata (DESCRIPTION)
- source code function.R
- roxygen comments in function.R
- NAMESPACE for imported and exported functions
- tests
- other (files, data, tutorials, vignettes)

The Whole Game
library(devtools)
library(usethis)

1. usethis::create_package()
or
1. create new working directory for R package
- delete namespace, build from scratch

2. usethis::use_r("fun_name")
- create minimal R function

3. devtools::load_all()
- load all imported & exported functions to test

4. insert roxygen2 comment (ctrl+alt+shift+R)
- @title @description @param @details @return @export @examples
- @importFrom (specific functions) @import (whole package)

5. usethis::use_package("ggplot2", type = "Imports")
- add external package as dependency
- usethis::use_version()

6. devtools::document() ctrl+shift+d

7. devtools::install() ctrl+shift+b

8. devtools::check()
- check early & often

9. usethis::use_git() / ::usegithub()
- happygitwithr.com by Jenny Bryan
- committ early & often

10. usethis::use_mit_license()

11. usethis::use_test()
- for open file
- devtools::load_all() then devtools::test() for all function tests
- devtools::check() will run tests

12. devtools::build_site()
- pkgdown site

13. remotes::install_github("jemarnold/mNIRS")
