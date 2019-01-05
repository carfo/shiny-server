##HELPERS#############################################################################
# load or install the necessary packages
load_lib <- function(package, update=F) {
  if(update)
    update.packages(ask=FALSE,checkBuilt=TRUE)
  for(i in package){
    if(!require(i,character.only=T)){
      install.packages(i)
      library(i,character.only=T)
    }
  }
}

saveSQLite <- function(data, name){
  path <- dplyr:::db_location(filename=paste0(name, ".sqlite"))
  src <- dbConnect(RSQLite::SQLite(), path)
  if (!file.exists(path)) {
    message("Caching db at ", path)
    dbWriteTable(src, name, data)
  } else {
    dbWriteTable(src, name, data, append=T)
  }
  return (src)
}

updatecheck <- function(session=NULL, input, selected, db=NULL) {
  if(!is.null(db)){
    return(NULL)
  }else{
    updatePrettyCheckboxGroup(session,inputId = input[1],selected = NA)
    updateCheckboxGroupButtons(session,inputId = input[2],selected = FALSE)
    updateAwesomeCheckboxGroup(session,inputId = input[3],selected = FALSE)
  }
}

##### SQLlite####
# crea il db
create.db <- function(table,database){
  require(RSQLite)
  require(DBI)
  if((file.exists(database))){
    sprintf("Database found!")
    return(FALSE)
  }
  con <- dbConnect(RSQLite::SQLite(), database)

  for (i in unique(table$name)) {
    dt<-data.frame(rep(NA,length(table[name==i,field])))
    names(dt)<-table[name==i,field]
    dbWriteTable(con, dbQuoteIdentifier(con,i), dt[0,])
  }
  dbDisconnect(con)
}
# leggi tabelle da db
read.db <- function(table,database){
  require(data.table)
  require(RSQLite)
  require(DBI)
  if(!(file.exists(database))){
    sprintf("Database not found!")
    return(FALSE)
  }
  con <- dbConnect(RSQLite::SQLite(), database)
  if(!(dbExistsTable(con, dbQuoteIdentifier(con,table)))){
    sprintf("Table not found!")
    dbDisconnect(con)
    return(FALSE)
  }
  tb<-data.table(dbReadTable(con, dbQuoteIdentifier(con,table)))

  dbDisconnect(con)
  return(tb)
}

# aggiunta dati al db
append.db <- function(table_name,table,database){
  require(data.table)
  require(RSQLite)
  require(DBI)
  # if(!(file.exists(database))){
  #   sprintf("Database not found!")
  #   #create.db(table_name,database = database)
  #   return(FALSE)
  # }
  con <- dbConnect(RSQLite::SQLite(), database)

  dbWriteTable(con, dbQuoteIdentifier(con,table_name), table, append = TRUE)

  dbDisconnect(con)
  return(TRUE)
}
