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


# usethis::use_package_doc()
# usethis::use_github_links()
# usethis::use_tidy_description()
usethis::use_package("rlang")
usethis::use_package("purrr", type = "suggest")

usethis::use_import_from("stats",
                         c("as.formula", "coef", "fitted", "formula",
                           "median", "na.exclude", "nls", "predict", "setNames",
                           "sortedXyData", "update"))
usethis::use_import_from("rlang", c("as_name"))

usethis::use_vignette("processing-mNIRS-data.qmd")
devtools::build_rmd("vignettes/processing-mNIRS-data.qmd")

usethis::use_test("replace_invalid_values")
devtools::load_all()
devtools::test()
devtools::check()

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

12. usethis::use_github_action("pkgdown")
- usethis::use_pkgdown_github_pages()
- NEED TO REMOVE `docs` FROM GITIGNORE
- locally build devtools::build_site()
- pkgdown site
- (pkgdown::clean_site() to remove)

13. remotes::install_github("jemarnold/mNIRS")

#
## Claude.ai ===============================================
## Here's the workflow for creating a public repository from your private
## development package:
## 1. Prepare Your Package Structure
# Ensure your package follows standard structure
usethis::use_package_doc()
usethis::use_mit_license()  # or appropriate licence
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_cran_comments()

## 2. Clean and Document
# Update documentation
devtools::document()

# Check package
devtools::check()

# Build vignettes if any
devtools::build_vignettes()

## 3. Create Public Repository
# Create new public repo (will prompt for GitHub details)
usethis::use_github(private = FALSE)

# Or if you want to create manually on GitHub first:
usethis::use_git_remote("origin", "https://github.com/yourusername/packagename.git")

## 4. Set Up GitHub Actions
# Standard R package checks
# usethis::use_github_actions_check() ## DOESNT EXIST

# Test coverage
# usethis::use_github_actions_test_coverage()
usethis::use_github_action()

# pkgdown site deployment
usethis::use_pkgdown_github_pages()

## 5. Configure pkgdown
# Initialise pkgdown
# usethis::use_pkgdown()

# Build site locally to test
pkgdown::build_site()

# Configure _pkgdown.yml as needed

## 6. Set Up Development vs Release Workflow
# Use development version numbering
usethis::use_dev_version()

# Set up lifecycle badges
usethis::use_lifecycle()

## 7. Final Steps
# Check everything works
devtools::check()

# Build and push
devtools::build()

## The key is keeping your development work in a private repo and only pushing stable,
## documented code to the public repository. Use semantic versioning (0.0.0.9000 for
## dev, 0.1.0 for first release) to distinguish versions.
