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
