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
golem::add_module(name = "PCA-PCA") # Name of the module
golem::add_module(name = "PCA-Pairing_of_component_loadings") # Name of the module
golem::add_module(name = "PCA-Component_loadings") # Name of the module
golem::add_module(name = "PCA-Combinations_of_principal_components") # Name of the module
golem::add_module(name = "PCA-Linear_regression") # Name of the module
golem::add_module(name = "PCA-Correlations") # Name of the module


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
golem::add_utils("home")
golem::add_utils("import")
golem::add_utils("selection")
golem::add_utils("pca")
golem::add_utils("export")
golem::add_utils("database")
golem::add_fct("classPlotPanel")

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

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_combine() |>
  rmapshaper::ms_simplify(keep = 0.03) |>
  sf::st_as_sf()

internal = list("apptext" = apptext, "acc" = acc, "sampledata" = sampledata,
                "world" = world)
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
