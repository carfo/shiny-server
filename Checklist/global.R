# load queries and helpers
source("functions.R")
# load libraries
load_lib(c("shiny","DT","highcharter","data.table","DBI","RMySQL","digest","lubridate","config",
           "shinyWidgets","shinythemes","RSQLite","dplyr"))
# load config
#config<-config::get(file = "Sicurezza/config.yml")
