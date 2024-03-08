# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_from_rscripts() |>
  purrr::map(usethis::use_package)
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "database") # Name of the module
golem::add_module(name = "ENV") # Name of the module
golem::add_module(name = "upload_timeseries") # Name of the module
golem::add_module(name = "upload_metadata") # Name of the module
golem::add_module(name = "upload_shapefile") # Name of the module
golem::add_module(name = "upload_raster") # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("database")

golem::add_utils("connect_database")
golem::add_utils("render_sidebar")
golem::add_utils("connect_postgres")
golem::add_utils("connect_sqlite")


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package

# Language
apptextfiles <- list.files(pattern = "\\.csv$", path = "inst/app/language/")
apptext <- lapply(file.path("inst/app/language/", apptextfiles), \(x) read.csv(x, header = FALSE)[, 1])
names(apptext) <- tools::file_path_sans_ext(apptextfiles)
usethis::use_data(apptext, internal = TRUE, overwrite = TRUE)

# Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("PCApp")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# R Buildignore
usethis::use_build_ignore(c("shinyproxy-3.0.2.jar", "application.yml", ".env"))

# GitHub Actions
usethis::use_github_action("docker")

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
