# load queries and helpers
source("functions.R")
# load libraries
# load_lib(c("shiny","DT","data.table","DBI","RSQLite","digest","lubridate","config",
#           "shinyWidgets","shinythemes","dplyr","pool","stringr","stringi"))
library(shiny)
library(DT)
library(data.table)
library(DBI)
library(RSQLite)
library(digest)
library(lubridate)
library(config)
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(pool)
library(stringr)
library(stringi)
# load config
#config<-config::get(file = "Sicurezza/config.yml")
#credentials_file<-"www/users.dat"
#keys_file<-"Security/key.dat"
db_file<-"./Data/gestione.db"
bk_file <- paste0("./Data/gestione",Sys.Date(),".db")
if(!file.exists(db_file)){
  #crea db
}

file.copy(from = db_file, to = bk_file)
