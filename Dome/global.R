# load queries and helpers
source("functions.R")
# load libraries
load_lib(c("shiny","DT","data.table","DBI","RSQLite","digest","lubridate","config",
           "shinyWidgets","shinythemes","dplyr","pool","stringr","stringi"))
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

pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = db_file
)
