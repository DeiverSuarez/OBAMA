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
## Add one line by package you want to add as dependency
#usethis::use_package( "thinkr" ) 
usethis::use_package( "reticulate" )
usethis::use_package( "shiny" )
usethis::use_package( "venn" )
usethis::use_package( "plotly" )
usethis::use_package( "ggplot2" )
usethis::use_package( "DT" )
usethis::use_package( "vroom" )
usethis::use_package("shinycssloaders")   
############### MST ####################
usethis::use_package("Biobase") 
usethis::use_package("GEOquery") 
usethis::use_package("tidyr")  
usethis::use_package("flexclust") 
usethis::use_package("dplyr") 
usethis::use_package("tidyr")
usethis::use_package("tibble")  
usethis::use_package("corrplot")
usethis::use_package("readr") 
usethis::use_package("optrees")
usethis::use_package("DataCombine") #
usethis::use_package("networkD3")
############### OFP ####################
usethis::use_package("RColorBrewer")


usethis::use_pipe()
## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "Individuals" ) # Name of the module
golem::add_module( name = "Individual_by_Sex" ) # Name of the module
golem::add_module( name = "Meta_analysis" ) # Name of the module
############# MST ####################
golem::add_module(name = "MST_one_condition")
golem::add_module(name = "MST_Meta_analysis")
############# OGF ####################
golem::add_module(name = "OGF_Optimal_Group_Formation")



## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct("mco_one_diseases")
golem::add_fct("mco_one_diseases_3PM")
golem::add_fct("mco_two_meta_analysis")
golem::add_fct("mco_three_meta_analysis")
golem::add_fct("mco_four_meta_analysis")
golem::add_fct("mco_five_meta_analysis")
golem::add_utils("MCO_two_meta_analysis")
golem::add_utils("MCO_three_meta_analysis")
golem::add_utils("MCO_four_meta_analysis")
golem::add_utils("MCO_five_meta_analysis")
golem::add_utils( "data_build" )
golem::add_utils( "data_build2" )
golem::add_utils( "PMs" )
golem::add_utils( "PMs2" )
golem::add_utils( "PMs_M" )
golem::add_utils("load_file")
golem::add_utils( "MCO_one_disease" )
golem::add_utils( "MCO_one_disease_3PM" ) 
golem::add_utils("data_partition")
golem::add_utils("Venn")

############# MST ####################
golem::add_fct("MST_one_disease")
golem::add_fct("MST_two_disease")
golem::add_fct("MST_Three_disease")
golem::add_fct("MST_four_disease")
golem::add_fct("MST_five_disease")
golem::add_utils("MST_diagram")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "dataExample", open = FALSE ) 
## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("OBAMA")
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

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")