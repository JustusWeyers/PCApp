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
golem::add_module(name = "home") # Name of the module
golem::add_module(name = "database") # Name of the module
golem::add_module(name = "ENV") # Name of the module
golem::add_module(name = "import") # Name of the module
golem::add_module(name = "selection") # Name of the module
golem::add_module(name = "PCA") # Name of the module
golem::add_module(name = "export") # Name of the module
golem::add_module(name = "general") # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("database")
golem::add_fct("data")

golem::add_fct("classDatabase")
golem::add_fct("classPostgreSQL")
golem::add_fct("classSQLite")
golem::add_fct("classGroup")
golem::add_fct("classData")
golem::add_fct("classTableData")
golem::add_fct("classTimeseries")
golem::add_fct("classMetadata")
golem::add_fct("classGeospatialData")
golem::add_fct("classVectorData")
golem::add_fct("classRasterData")
golem::add_utils("connect_database")
golem::add_utils("render_sidebar")
golem::add_utils("connect_postgres")
golem::add_utils("connect_sqlite")
golem::add_utils("s4todataframe")
golem::add_utils("boxcolor")
golem::add_utils("optional_fun_param")
golem::add_utils("return_matching_UI_element")
golem::add_utils("instantiatePostgreSQL")
golem::add_utils("instantiateSQLite")
golem::add_utils("recreateDataObjects")
golem::add_utils("random_address")
golem::add_utils("setdifflist")
golem::add_utils("insert_timeseries")
golem::add_utils("datareading")
golem::add_utils("join_timeseries_with_metadata")
golem::add_utils("fetch_metadata")
golem::add_utils("read_shapefile")
golem::add_utils("autocorrelation")
golem::add_utils("plots")



## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package

# Language

languages <- list.dirs(path = "inst/app/language/", full.names = FALSE, recursive = FALSE)

apptext = lapply(languages, function(l) {
  files = list.files(path = paste0("inst/app/language/", l))
  unlist(lapply(files, function(f) {
    fn = paste0("inst/app/language/", l, "/",  f)
    if (endsWith(fn, ".csv")) {
      w = unlist(read.csv(fn, col.names = FALSE, header = FALSE))
      names(w) <- unlist(read.csv("inst/app/language/en/en.csv", header = FALSE))
      return(w)
    }
    if (endsWith(fn, ".txt")) {
      return(stats::setNames(readChar(fn, file.info(fn)$size), f))
    }
  }))
})
names(apptext) <- languages

sampledata = read.csv("inst/app/sampledata/sampledata_weekly.csv")

# Default database access values
acc = c(
  host = "localhost",
  superuser = "user",
  user = "user",
  superpassword = "mysecretpassword",
  password = "mysecretpassword",
  port = "5432",
  dbname = "mydb"
)

internal = list("apptext" = apptext, "acc" = acc, "sampledata" = sampledata)
usethis::use_data(internal, internal = TRUE, overwrite = TRUE)

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

# Create a summary readme for the testthat sub directory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# R Buildignore
usethis::use_build_ignore(c("shinyproxy-3.0.2.jar", "application.yml", ".env",
                            "application.properties"))

# GitHub Actions
usethis::use_github_action("docker")

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
