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

#read encrypted data
read.aes <- function(filename,key) {
  require(digest)
  dat <- readBin(filename,"raw",n=1000)
  aes <- AES(key,mode="ECB")
  raw <- aes$decrypt(dat, raw=TRUE)
  txt <- rawToChar(raw[raw>0])
  read.csv(text=txt, stringsAsFactors = F)
}

#Place of birth, residence and domiciliation descriptions
desc_comuni<-function(dat,comuni){
  setkey(dat,luogonasc)
  dat<-comuni[,.(cod_unito,descrizion,sigla_prov)][dat]
  setnames(dat,"descrizion","Luogo nascita")
  setnames(dat,"sigla_prov","Provincia nascita")
  dat[,cod_unito:=NULL]

  setkey(dat,luogoresid)
  dat<-comuni[,.(cod_unito,descrizion,sigla_prov)][dat]
  setnames(dat,"descrizion","Luogo residenza")
  setnames(dat,"sigla_prov","Provincia residenza")
  setnames(dat,"indirizzores","indirizzo residenza")
  dat[,cod_unito:=NULL]

  setkey(dat,luogodomic)
  dat<-comuni[,.(cod_unito,descrizion,sigla_prov)][dat]
  setnames(dat,"descrizion","Luogo domicilio")
  setnames(dat,"sigla_prov","Provincia domicilio")
  setnames(dat,"indirizzodomic","indirizzo domicilio")
  dat[,cod_unito:=NULL]
  setkey(dat,codpaz)
  return(dat)
}

# The CKD-EPI equation, expressed as a single equation, is:
#
#   GFR = 141 * min(Scr/κ,1)^α * max(Scr/κ, 1)^-1.209 * 0.993^Age * 1.018 [if female] * 1.159 [if black]
#
# Scr is serum creatinine (mg/dL), κ is 0.7 for females and 0.9 for males, α is -0.329 for females and -0.411 for
# males,
# min indicates the minimum of Scr/κ or 1, and max indicates the maximum of Scr/κ or 1.
ckd.epi<-function(creatinin,age,sex){
  den<-ifelse(sex=="M",0.9,0.7)
  alpha<-ifelse(sex=="M",-0.411,-0.329)
  mul<-ifelse(sex=="M",1,1.018)
  gfr<-141*(min(creatinin/den,1)^alpha)*(max(creatinin/den,1)^-1.209)*(0.993^age)*mul
  return(gfr)
}

active_patients <- function(dbconfig){
  sed<-query_sedute(dbconfig = dbconfig)
  sel<-unique(sed[dstipotrattprg!="PLASMAFERESI"&is.na(datadecesso),.N,by=.(codpaz,cognome,nome,
                                                                            datanasc)][N>20,.(codpaz,cognome,nome,datanasc)])
  sel[,trattamento:="Extracorporeo"]
  sel<-sel[sed[,.SD[which.max(attacco)],by=codpaz][,.(codpaz,dscentro)],on="codpaz",nomatch=0]
  setnames(sel,"dscentro","sede")
  pd<-query_PD(dbconfig = dbconfig)
  pd[[1]][,trattamento:="Peritoneale"]
  pd[[1]][,sede:="Domiciliare"]
  pd[[2]][,trattamento:="Peritoneale"]
  pd[[2]][,sede:="Domiciliare"]
  sel<-rbind(sel,unique(pd[[1]][is.na(datadecesso),.(codpaz,cognome,nome,datanasc,trattamento,sede)]),
             unique(pd[[2]][is.na(datadecesso),.(codpaz,cognome,nome,datanasc,trattamento,sede)]))
  return(sel)
}

therap <- function(patients,treatment,dbconfig) {
  e<-query_prodiEmo(patients=patients[treatment%like%"Extracorporeo"], dbconfig = dbconfig)
  e<-melt(e,id.vars = c("codpaz","validodal"),measure.vars = c("Farmaco1","Farmaco2","Farmaco3","Farmaco4"))
  setnames(e,c("validodal","value"),c("data","farmaco"))
  p<-query_terapia(pat=patients[treatment%like%"Peritoneale"], dbconfig = dbconfig)
  setnames(p,c("validadal","dsfarm"),c("data","farmaco"))
  return(rbind(e[,.(codpaz,data,farmaco)],p[,.(codpaz,data,farmaco)]))
}

#Connect to DB
connect <- function(drv,dbname,host=NULL,port=NULL,user=NULL,password=NULL,key=NULL) {
  #require(RSQLite)
  require(DBI)
  if((is.null(user)|is.null(password))){
    if(!is.null(key)){
      credentials <- read.aes(filename = "Sicurezza/credentials.txt",key = key)
      user<-credentials$login
      password<-credentials$password
      rm(credentials)
    }else
      return(FALSE)
  }

  return(dbConnect(drv = eval(parse(text=drv)),
                   dbname=dbname,
                   host=host,
                   port=port,
                   user=user,
                   password=password
  ))
}

##QUERY###############################################################################
###Query towns
query_comuni <- function(dbconfig) {
  require(data.table)
  require(DBI)
  #require(RMySQL)
  #credentials <- read.aes(filename = "credentials.txt",key = key)
  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)
  rs <- dbSendQuery(con, "SELECT *
                    FROM comuni")
  comuni_db<-data.table(dbFetch(rs,n=-1))
  dbClearResult(rs)
  dbDisconnect(con)
  setkey(comuni_db,cod_unito)
  return(comuni_db)
}

###Pharmacy
query_farmaci <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,MAX(trpdomh.id_trpdomh),MAX(trpdomh.validadal)
                FROM anagr RIGHT JOIN trpdomh ON (trpdomh.codpaz=anagr.codpaz)
                WHERE trpdomh.stato='I' and validadal<='",as.Date(end)+days(1),"' and anagr.codpaz IN (",
                paste(unique(patients),sep=",",collapse=", "),")
                GROUP BY codpaz")

  rs <- dbSendQuery(con, query)
  prescrizioni<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  setnames(prescrizioni,c(5,6),c("id_trpdomh","validadal"))
  query<-paste0("SELECT trpdomh.id_trpdomh,trpdomh.validadal,medici.nome,trpdomd.posologia,farmaci.dsfarm,tbfarmacibd.atcliv5
                FROM trpdomh RIGHT JOIN medici ON (trpdomh.prescrittore=medici.matricola)
                LEFT JOIN trpdomd ON (trpdomh.id_trpdomh=trpdomd.id_trpdomh)
                LEFT JOIN farmaci ON (trpdomd.codfarm=farmaci.codfarm)
                LEFT JOIN tbfarmacibd ON (farmaci.caic=tbfarmacibd.caic)
                WHERE trpdomh.id_trpdomh in (",paste(prescrizioni$id_trpdomh,sep=",",collapse=", "),")")
  rs <- dbSendQuery(con, query)
  farmaci<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  setkey(prescrizioni,id_trpdomh,validadal)
  setkey(farmaci,id_trpdomh,validadal)
  farmaci<-prescrizioni[farmaci]
  farmaci[,datanasc:=as.Date(datanasc)]
  farmaci[,validadal:=as.Date(validadal)]
  #setnames(sedute,c("t016xttipomembrana","t017xtsuperficiemembrana"),c("tipo_membrana","sup_membrana"))
  #print(paste("Estrazione effettuata ",sedute[,.N]))
  return(farmaci)
}

#Pharm
query_terapia <- function(pat,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,trpdomd.*,trpdomh.*,farmaci.dsfarm,farmaci.codfarm
                FROM anagr RIGHT JOIN trpdomh ON (trpdomh.codpaz=anagr.codpaz)
                LEFT JOIN trpdomd ON (trpdomh.id_trpdomh=trpdomd.id_trpdomh)
                LEFT JOIN farmaci ON (trpdomd.codfarm=farmaci.codfarm)
                WHERE trpdomh.stato='I' and anagr.codpaz IN ( ", paste(unique(pat),sep=",",collapse=", ")," )")

  rs <- dbSendQuery(con, query)
  terapia<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  terapia[,datanasc:=as.Date(datanasc)]
  terapia[,validadal:=as.Date(validadal)]
  return(terapia)
}

###Laboratory
query_esami <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc, anagr.sesso,labor1.*,labor2.*
                FROM anagr RIGHT JOIN labor1 ON (labor1.codpaz=anagr.codpaz)
                LEFT JOIN labor2 ON (labor1.id_labor=labor2.id_labor)
                WHERE labor1.stato='I' and labor1.datalab>='",as.Date(start),"' and labor1.datalab<='",as.Date(end)+days(1),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),")")

  rs <- dbSendQuery(con, query)
  esami<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  esami[,grep("codpaz",names(esami))[2]:=NULL]
  esami[,grep("id_labor",names(esami))[2]:=NULL]
  esami[,datanasc:=as.Date(datanasc)]
  esami[,datalab:=as.Date(datalab)]
  for (i in grep("p",tolower(names(esami))))
    set(esami,which(esami[[i]]==0L),i,NA_real_)
  setnames(esami,names(esami),tolower(names(esami)))

  setnames(esami,names(esami)[startsWith(names(esami),"p1")],
           paste0(names(esami)[startsWith(names(esami),"p1")],"_emocromo"))
  setnames(esami,names(esami)[startsWith(names(esami),"p3")],
           paste0(names(esami)[startsWith(names(esami),"p3")],"_urine"))
  setnames(esami,names(esami)[startsWith(names(esami),"p4")],
           paste0(names(esami)[startsWith(names(esami),"p4")],"_emogas"))

  setnames(esami,names(esami)[startsWith(names(esami),"p")],
           substring(sapply(strsplit(names(esami)[startsWith(names(esami),"p")], "^p"),"[",2),first = 2))
  esami[,sesso:=ifelse(sesso==1,"M","F")]
  esami[,age:=as.integer(round((datalab-datanasc)/365))]
  esami[,eGFR:=round(ckd.epi(creatinin=creatinina_emocromo,age=as.integer(round((datalab-datanasc)/365)),sex=sesso),
                     2),by=id_labor]
  esami[,CKD:=factor(ifelse(eGFR>0&eGFR<15,6,
                            ifelse(eGFR>=15&eGFR<30,5,
                                   ifelse(eGFR>=30&eGFR<45,4,
                                          ifelse(eGFR>=45&eGFR<60,3,
                                                 ifelse(eGFR>=60&eGFR<90,2,
                                                        ifelse(eGFR>=90&eGFR<200,1,0)))))),
                     levels = c(0,1,2,3,4,5,6),labels = c("Valore incorretto","CKD 1","CKD 2","CKD 3a","CKD 3b","CKD 4",
                                                          "CKD 5"),ordered = T)]

  setkey(esami,id_labor,codpaz)
  return(esami)
}

#Creatinine and eGFR
query_creatinine <- function(start=NULL,end=Sys.Date(),patients,dbconfig) {
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz, anagr.cognome, anagr.nome, anagr.datanasc, anagr.sesso, labor1.P1CREATININA,
                labor1.P1CREAT_CLEARANCE, labor1.datalab, labor1.stato, labor2.P3CREATININA, labor2.P3VOLUME, labor1.id_labor
                FROM labor2 INNER JOIN (labor1 INNER JOIN anagr ON labor1.codpaz = anagr.codpaz) ON labor2.id_labor = labor1.id_labor
                WHERE anagr.codpaz IN (", paste(unique(patients),sep=",",collapse=", "),") AND labor1.datalab Between '",
                start,"' And '",end,"' AND (labor1.stato)='I' AND (labor1.P1CREATININA)<>0;")
  rs <- dbSendQuery(con, query)
  lab <- data.table(dbFetch(rs, -1))
  dbClearResult(rs)

  dbDisconnect(con)
  lab[, datanasc := as.Date(datanasc)]
  lab[, datalab:=as.Date(datalab)]
  setnames(lab, tolower(names(lab)))
  lab <-lab[, .SD, .SDcols = grep("^p", tolower(names(lab)),
                                  invert = T, value = T)][lab[,lapply(.SD, function(x) {
                                    ifelse(is.numeric(x) & x == 0, NA_real_, x)
                                  }),
                                  .SDcols =
                                    tolower(names(lab)) %like% "^p" |
                                    tolower(names(lab)) %like% "id_labor"],
                                  on = "id_labor"]
  lab[,datanasc:=as.Date(datanasc)]

  setnames(lab,names(lab)[startsWith(names(lab),"p")],
           substring(sapply(strsplit(names(lab)[startsWith(names(lab),"p")], "^p"),"[",2),first = 2))

  lab[,sesso:=ifelse(sesso==1,"M","F")]
  lab[,age:=as.integer(round((datalab-datanasc)/365))]
  lab[,eGFR:=ckd.epi(creatinin=creatinina,age=as.integer(round((datalab-datanasc)/365)),sex=sesso),by=.(codpaz,datalab)]
  lab[,CKD:=factor(ifelse(eGFR>0&eGFR<15,6,
                          ifelse(eGFR>=15&eGFR<30,5,
                                 ifelse(eGFR>=30&eGFR<45,4,
                                        ifelse(eGFR>=45&eGFR<60,3,
                                               ifelse(eGFR>=60&eGFR<90,2,
                                                      ifelse(eGFR>=90&eGFR<200,1,0)))))),
                   levels = c(0,1,2,3,4,5,6),labels = c("Valore incorretto","CKD 1","CKD 2","CKD 3a","CKD 3b","CKD 4",
                                                        "CKD 5"),ordered = T)]
  return(lab[,tail(.SD,1),by=.(codpaz,datalab)])
}

#Medical history
query_anamnesi <- function(pat,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,app.*
                FROM anagr RIGHT JOIN app ON (app.codpaz=anagr.codpaz)
                WHERE app.stato='I' and anagr.codpaz IN (", paste(unique(pat),sep=",",collapse=", "),")")

  rs <- dbSendQuery(con, query)
  anamnesi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  anamnesi[,datanasc:=as.Date(datanasc)]
  anamnesi[,data:=as.Date(data)]
  return(anamnesi)
}

#Hospitalizations
query_ricoveri <- function(pat,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,ricovero.*
                FROM anagr RIGHT JOIN ricovero ON (ricovero.codpaz=anagr.codpaz)
                WHERE ricovero.stato='I' and anagr.codpaz IN (", paste(unique(pat),sep=",",collapse=", "),")")

  rs <- dbSendQuery(con, query)
  ricoveri<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  ricoveri[,datanasc:=as.Date(datanasc)]
  ricoveri[,ingresso:=as.Date(ingresso)]
  ricoveri[,uscita:=as.Date(uscita)]
  ricoveri[,datainh:=as.Date(datainh)]
  return(ricoveri)
}

#Anagraphics data (!!!!diabete!!!! 0302250E)
query_anagrafica <- function(patients=NULL,dbconfig) {
  require(DBI)
  require(data.table)
  #require(RMySQL)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT anagr.*,t007titolostudio.t007xttitolostudio AS titolo,statociv.descrizion AS statoc,
                nazional.descrizion AS naz
                FROM anagr LEFT JOIN t007titolostudio ON (anagr.titstud=t007titolostudio.t007idtitolostudio)
                LEFT JOIN statociv ON (anagr.statociv=statociv.codice)
                LEFT JOIN nazional ON (anagr.nazionalita=nazional.cod_nazion)
                WHERE anagr.codpaz>0")

  if(!is.null(patients))
    query<-paste0(query," AND anagr.codpaz IN (", paste(unique(patients),sep=",",collapse=", "),")")

  rs <- dbSendQuery(con, query)
  anagrafica<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  anagrafica[,datanasc:=as.Date(datanasc)]
  anagrafica<-desc_comuni(anagrafica,query_comuni(dbconfig))
  anagrafica[,sesso:=factor(sesso,levels=c(1,2),labels = c("m","f"))]
  com<-query_comorbidita(patients=anagrafica$codpaz,dbconfig=dbconfig)
  anagrafica<-com[anagrafica,on=.(codpaz,cognome,nome,datanasc),nomatch=NA]
  anagrafica[,diabete:=tolower(comorbBreve)%like%"diabete"]
  return(anagrafica)
}

# Comorbidity
query_comorbidita <- function(patients,dbconfig) {
  require(DBI)
  require(data.table)
  #require(RMySQL)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,comorb.dtrilev,
                t021eventi.t021descbreve as comorbBreve, t021eventi.t021descestesa as comorbEstesa
                FROM anagr LEFT JOIN comorb ON (anagr.codpaz=comorb.codpaz)
                LEFT JOIN t021eventi ON (comorb.codcomorb=t021eventi.t021idevento)
                WHERE comorb.codpaz IN (", paste(unique(patients),sep=",",collapse=", "),")
                AND comorb.stato='I'")

  rs <- dbSendQuery(con, query)
  comorb<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  comorb[,datanasc:=as.Date(datanasc)]
  comorb[,dtrilev:=as.Date(dtrilev)]
  return(comorb)
}

#Ambulatory visit
query_visita <- function(start=NULL,end=Sys.Date(),pat,agenda=NULL,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(12)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,medici.matricola,
                medici.nome AS nomemedico,visamb.*,erogat.dserogat,medici.cfis
                FROM anagr RIGHT JOIN visamb ON (visamb.codpaz=anagr.codpaz)
                LEFT JOIN medici ON (medici.matricola=visamb.medico)
                LEFT JOIN erogat on (visamb.iderogat=erogat.iderogat)
                WHERE visamb.datavis>='",start,"' and visamb.datavis<='",end,"' and visamb.stato='I' and anagr.codpaz IN (",
                paste(unique(pat),sep=",",collapse=", "),")")

  if(!is.null(agenda))
    query<-paste0(query," and erogat.iderogat in (",paste(unique(agenda),sep=",",collapse=", "),")")

  rs <- dbSendQuery(con, query)
  visite<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  visite[,datanasc:=as.Date(datanasc)]
  visite[,datavis:=as.Date(datavis)]
  return(visite)
}

#Book
query_agenda <- function(start=NULL,end=Sys.Date(),agenda=NULL,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(12)

  # aggiungere controllo su stato
  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,agenda.*,erogat.*
                FROM anagr RIGHT JOIN agenda ON (anagr.codpaz=agenda.cart) LEFT JOIN erogat ON (agenda.iderogat=erogat.iderogat)
                WHERE agenda.giorno>='",start,"' and agenda.giorno<='",end,"' and agenda.cart<>4610 and agenda.cart<>0 and ",
                "agenda.cart<>11294 and agenda.cart<>12930 and agenda.cart<>17519 and agenda.cart<>13710")
  if(!is.null(agenda))
    query<-paste0(query," and erogat.iderogat in (",paste(unique(agenda),sep=",",collapse=", "),")")

  rs <- dbSendQuery(con, query)
  visite<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  visite[,datanasc:=as.Date(datanasc, format = "%Y-%m-%d")]
  visite[,giorno:=as.Date(giorno, format = "%Y-%m-%d")]
  return(visite)
}

#Signaled patients
query_segnalati <- function(start=NULL,end=Sys.Date(),dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  if(is.null(start))
    start<-end %m-% months(12)

  ariel<-unique(query_agenda(start=start,end=end,agenda=30,dbconfig=dbconfig)$codpaz)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,medici.matricola,
                medici.nome AS nomemedico,visamb.*,erogat.dserogat,medici.cfis,visamb.*,erogat.dserogat
                FROM anagr RIGHT JOIN visamb ON (visamb.codpaz=anagr.codpaz)
                LEFT JOIN medici ON (medici.matricola=visamb.medico)
                LEFT JOIN erogat on (visamb.iderogat=erogat.iderogat)
                WHERE visamb.datavis>='",start,"' and visamb.datavis<='",end,"' and visamb.stato='I' and anagr.codpaz NOT IN (",
                paste(unique(ariel),sep=",",collapse=", "),") and LOWER(visamb.referto) LIKE '% ariel %'")

  rs <- dbSendQuery(con, query)
  visite<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)

  dbDisconnect(con)
  visite[,datanasc:=as.Date(datanasc)]
  visite[,datavis:=as.Date(datavis)]

}

#Vascular access
query_accessi <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,accvash.*,accvasd.*,
                tbsvarie.Valore as Tipo
                FROM anagr RIGHT JOIN accvash ON (accvash.codpaz=anagr.codpaz)
                LEFT JOIN accvasd ON (accvash.id_avh=accvasd.id_avh)
                LEFT JOIN tbsvarie ON (accvash.tipoav=tbsvarie.IDvarie)
                WHERE tbsvarie.Chiave='TIPOAV' and accvash.data>='",as.Date(start),"'
                and accvash.data<='",as.Date(end),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),") and accvash.stato='I'
                and accvasd.stato='I'")

  rs <- dbSendQuery(con, query)
  accessi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  accessi[,datanasc:=as.Date(datanasc)]
  accessi[,data:=as.Date(data)]
  return(accessi)
}

#Peritoneal access
query_accessiP <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,accperh.*,accperd.*,
                tbsvarie.Valore as Tipo
                FROM anagr RIGHT JOIN accperh ON (accperh.codpaz=anagr.codpaz)
                LEFT JOIN accperd ON (accperh.id_aph=accperd.id_aph)
                LEFT JOIN tbsvarie ON (accperh.tipoap=tbsvarie.IDvarie)
                WHERE tbsvarie.Chiave='TIPOAP' and accperh.data>='",as.Date(start),"'
                and accperh.data<='",as.Date(end),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),") and accperh.stato='I'
                and accperd.stato='I'")

  rs <- dbSendQuery(con, query)
  accessi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  accessi[,datanasc:=as.Date(datanasc)]
  accessi[,data:=as.Date(data)]
  return(accessi)
}

#Exams
query_esamiStrum <- function(start=NULL,end=Sys.Date(),patients,dbconfig) {
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,esastru.*,
                tbesstru.dsesastru as Tipo
                FROM anagr RIGHT JOIN esastru ON (esastru.codpaz=anagr.codpaz)
                LEFT JOIN tbesstru ON (esastru.tipoesam=tbesstru.idesastru)
                WHERE esastru.dtesam>='",as.Date(start),"'
                and esastru.dtesam<='",as.Date(end),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),") and esastru.stato='I'")

  rs <- dbSendQuery(con, query)
  strum<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  strum[,datanasc:=as.Date(datanasc)]
  strum[,dtesam:=as.Date(dtesam)]
  return(strum)
}

#Consults
query_consulenza <- function(start=NULL,end=Sys.Date(),patients,dbconfig) {
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,consulenze.*,
                tbconsu.dsconsu as Tipo
                FROM anagr RIGHT JOIN consulenze ON (consulenze.codpaz=anagr.codpaz)
                LEFT JOIN tbconsu ON (consulenze.tipoconsul=tbconsu.idconsu)
                WHERE consulenze.dtesam>='",as.Date(start),"'
                and consulenze.dtesam<='",as.Date(end),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),") and consulenze.stato='I'")

  rs <- dbSendQuery(con, query)
  consulenze<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  consulenze[,datanasc:=as.Date(datanasc)]
  consulenze[,dtesam:=as.Date(dtesam)]
  return(consulenze)
}

#Hemodialysis program
query_prodiEmo <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,diaprghd.*,
                tipotrattprg.dstipotrattprg as Tipo,
                terappostsed.dsterappostsed as Farmaco1,
                terappostsed2.dsterappostsed as Farmaco2,
                terappostsed3.dsterappostsed as Farmaco3,
                terappostsed4.dsterappostsed as Farmaco4
                FROM anagr RIGHT JOIN diaprghd ON (diaprghd.codpaz=anagr.codpaz)
                LEFT JOIN tipotrattprg ON (diaprghd.tipotratt=tipotrattprg.idtipotrattprg)
                LEFT JOIN terappostsed ON (diaprghd.terapiapost1farm=terappostsed.idterappostsed)
                LEFT JOIN terappostsed AS terappostsed2 ON (diaprghd.terapiapost2farm=terappostsed2.idterappostsed)
                LEFT JOIN terappostsed AS terappostsed3 ON (diaprghd.terapiapost3farm=terappostsed3.idterappostsed)
                LEFT JOIN terappostsed AS terappostsed4 ON (diaprghd.terapiapost4farm=terappostsed4.idterappostsed)
                WHERE diaprghd.validodal>='",as.Date(start),"'
                and diaprghd.validodal<='",as.Date(end),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),") and diaprghd.stato='I'")

  rs <- dbSendQuery(con, query)
  prodi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  prodi[,datanasc:=as.Date(datanasc)]
  prodi[,validodal:=as.Date(validodal)]
  return(prodi)
}

#CAPD program
query_prodiCAPD <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,diacapd.*,
                sacchecapd.dssacca as Sacca1, sacchecapd.formula as formula1, sacchecapd.soluzione as soluzione1,
                sacchecapd2.dssacca as Sacca2, sacchecapd2.formula as formula2, sacchecapd2.soluzione as soluzione2,
                sacchecapd3.dssacca as Sacca3, sacchecapd3.formula as formula3, sacchecapd3.soluzione as soluzione3,
                sacchecapd4.dssacca as Sacca4, sacchecapd4.formula as formula4, sacchecapd4.soluzione as soluzione4,
                sacchecapd5.dssacca as Sacca5, sacchecapd5.formula as formula5, sacchecapd5.soluzione as soluzione5
                FROM anagr RIGHT JOIN diacapd ON (diacapd.codpaz=anagr.codpaz)
                LEFT JOIN sacchecapd ON (diacapd.sacca1=sacchecapd.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd2 ON (diacapd.sacca2=sacchecapd2.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd3 ON (diacapd.sacca3=sacchecapd3.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd4 ON (diacapd.sacca4=sacchecapd4.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd5 ON (diacapd.sacca5=sacchecapd5.idsacca)
                WHERE diacapd.validodal>='",as.Date(start),"'
                and diacapd.validodal<='",as.Date(end),
                "' and anagr.codpaz IN (",paste(patients,sep=",",collapse=", "),") and diacapd.stato='I'")

  rs <- dbSendQuery(con, query)
  prodi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  prodi[,datanasc:=as.Date(datanasc)]
  prodi[,validodal:=as.Date(validodal)]
  return(prodi)
}

#APD program
query_prodiAPD <- function(start=NULL,end=Sys.Date(),patients,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,diaapd.*,
                tbsvarie.Valore as Tipo
                FROM anagr RIGHT JOIN diaapd ON (diaapd.codpaz=anagr.codpaz)
                LEFT JOIN tbsvarie ON (diaapd.tipoapd=tbsvarie.IDvarie)
                WHERE diaapd.validodal>='",as.Date(start),"'and diaapd.validodal<='",as.Date(end),
                "' and tbsvarie.Chiave='TIPOAPD' and anagr.codpaz IN (",
                paste(patients,sep=",",collapse=", "),") and diaapd.stato='I'")

  rs <- dbSendQuery(con, query)
  prodi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  prodi[,datanasc:=as.Date(datanasc)]
  prodi[,validodal:=as.Date(validodal)]
  return(prodi)
}

#Hemodialysis treatments
query_emo <- function(start=NULL, end=Sys.Date(), patients, dbconfig) {
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.cognome,anagr.nome,anagr.datanasc,anagr.codfisc, anagr.luogonasc,
                anagr.luogoresid,anagr.indirizzores, anagr.luogodomic, anagr.indirizzodomic, anagr.telefono1,
                anagr.telefono2,diaseduta.*,
                tipotrattprg.dstipotrattprg as tipoTrattamento, centri.dscentro as centro, filtri.*,
                t016tipomembrana.t016xttipomembrana as tipoMembrana,
                t017superficiemembrana.t017xtsuperficiemembrana as supMembrana
                FROM anagr RIGHT JOIN diaseduta ON (diaseduta.codpaz=anagr.codpaz)
                LEFT JOIN tipotrattprg ON (diaseduta.tipotratt=tipotrattprg.idtipotrattprg)
                LEFT JOIN filtri ON (diaseduta.idfiltro=filtri.idfiltro)
                LEFT JOIN t016tipomembrana ON (filtri.compmembra=t016tipomembrana.t016idtipomembrana)
                LEFT JOIN t017superficiemembrana ON
                (filtri.supmembra=t017superficiemembrana.t017idsuperficiemembrana)
                LEFT JOIN centri ON (diaseduta.idcentro = centri.idcentro)
                WHERE diaseduta.attacco>='",as.Date(start),"' and diaseduta.attacco<='",as.Date(end)+days(1),
                "' and anagr.codpaz IN (",
                paste(patients,sep=",",collapse=", "),") and diaseduta.stato='I'")

  rs <- dbSendQuery(con, query)
  sedute<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  sedute<-desc_comuni(sedute,query_comuni(dbconfig = dbconfig))
  sedute[,datanasc:=as.Date(datanasc)]
  #  sedute[,ora.attacco:=]
  #  sedute[,attacco:=as.Date(attacco)]
  #  sedute[,turno:=]
  return(sedute)
}

#All hemodialysis sessions
query_sedute<-function(start=NULL, end=Sys.Date(), dbconfig){
  require(data.table)
  require(DBI)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)
  query<-paste("SELECT diaseduta.codpaz, diaseduta.attacco, diaseduta.id_seduta, diaseduta.salareale,
               centri.dscentro,anagr.cognome,anagr.nome, anagr.datanasc, anagr.datadecesso, anagr.codfisc, anagr.luogonasc,
               anagr.luogoresid,anagr.indirizzores, anagr.luogodomic, anagr.indirizzodomic, anagr.telefono1,
               anagr.telefono2, diaseduta.pzricoverato, diaseduta.dialdacat, filtri.dsfiltro,
               tipotrattprg.dstipotrattprg
               FROM ((diaseduta LEFT JOIN tipotrattprg ON diaseduta.tipotratt = tipotrattprg.idtipotrattprg)
               LEFT JOIN filtri ON diaseduta.idfiltro = filtri.idfiltro
               LEFT JOIN centri ON diaseduta.idcentro = centri.idcentro)
               LEFT JOIN anagr ON diaseduta.codpaz = anagr.codpaz
               WHERE diaseduta.attacco>='",as.Date(start),"' AND diaseduta.attacco<='",
               as.Date(end)+days(1),"' AND diaseduta.stato='I'",sep="")

  rs <- dbSendQuery(con, query)
  sedute<-data.table(dbFetch(rs,n=-1))
  dbClearResult(rs)
  dbDisconnect(con)
  #sedute[,datanasc:=as.Date(datanasc,format="%d/%m/%Y")]
  sedute[,datanasc:=as.Date(datanasc,format="%Y-%m-%d")]
  #sedute[,attacco:=as.Date(attacco,format="%d/%m/%Y")]
  sedute[,attacco:=as.Date(attacco,format="%Y-%m-%d")]
  setkey(sedute,codpaz,attacco)
  sedute<-(unique(sedute, by = key(sedute)))
  setnames(sedute,"codfisc","codice fiscale")
  #setnames(sedute,"datanasc","data nascita")
  sedute<-desc_comuni(sedute,query_comuni(dbconfig = dbconfig))
  return(sedute)
}

#CAPD program
query_PD <- function(start=NULL,end=Sys.Date(),dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)

  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc, anagr.datadecesso, anagr.codfisc,
                diacapd.*,
                sacchecapd.dssacca as Sacca1, sacchecapd.formula as formula1, sacchecapd.soluzione as soluzione1,
                sacchecapd2.dssacca as Sacca2, sacchecapd2.formula as formula2, sacchecapd2.soluzione as soluzione2,
                sacchecapd3.dssacca as Sacca3, sacchecapd3.formula as formula3, sacchecapd3.soluzione as soluzione3,
                sacchecapd4.dssacca as Sacca4, sacchecapd4.formula as formula4, sacchecapd4.soluzione as soluzione4,
                sacchecapd5.dssacca as Sacca5, sacchecapd5.formula as formula5, sacchecapd5.soluzione as soluzione5
                FROM anagr RIGHT JOIN diacapd ON (diacapd.codpaz=anagr.codpaz)
                LEFT JOIN sacchecapd ON (diacapd.sacca1=sacchecapd.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd2 ON (diacapd.sacca2=sacchecapd2.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd3 ON (diacapd.sacca3=sacchecapd3.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd4 ON (diacapd.sacca4=sacchecapd4.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd5 ON (diacapd.sacca5=sacchecapd5.idsacca)
                WHERE diacapd.validodal>='",as.Date(start),"'
                and diacapd.validodal<='",as.Date(end),
                "' and diacapd.stato='I'")

  rs <- dbSendQuery(con, query)
  prodi<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)


  query<-paste0("SELECT anagr.codpaz,anagr.cognome,anagr.nome,anagr.datanasc,anagr.datadecesso, anagr.codfisc,
                diaapd.*,
                tbsvarie.Valore as Tipo, sacchecapd.dssacca as Sacca1, sacchecapd.formula as formula1, sacchecapd.soluzione as soluzione1,
                sacchecapd2.dssacca as Sacca2, sacchecapd2.formula as formula2, sacchecapd2.soluzione as soluzione2,
                sacchecapd3.dssacca as Sacca3, sacchecapd3.formula as formula3, sacchecapd3.soluzione as soluzione3,
                sacchecapd4.dssacca as Sacca4, sacchecapd4.formula as formula4, sacchecapd4.soluzione as soluzione4,
                sacchecapd5.dssacca as Sacca5, sacchecapd5.formula as formula5, sacchecapd5.soluzione as soluzione5
                FROM anagr RIGHT JOIN diaapd ON (diaapd.codpaz=anagr.codpaz)
                LEFT JOIN tbsvarie ON (diaapd.tipoapd=tbsvarie.IDvarie)
                LEFT JOIN sacchecapd ON (diaapd.sacca1=sacchecapd.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd2 ON (diaapd.sacca2=sacchecapd2.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd3 ON (diaapd.sacca3=sacchecapd3.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd4 ON (diaapd.sacca4=sacchecapd4.idsacca)
                LEFT JOIN sacchecapd AS sacchecapd5 ON (diaapd.sacca5=sacchecapd5.idsacca)
                WHERE diaapd.validodal>='",as.Date(start),"'and diaapd.validodal<='",as.Date(end),
                "' and tbsvarie.Chiave='TIPOAPD' and diaapd.stato='I'")

  rs <- dbSendQuery(con, query)
  prodi_a<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  prodi_a[,datanasc:=as.Date(datanasc)]
  prodi_a[,validodal:=as.Date(validodal)]

  prodi[,datanasc:=as.Date(datanasc)]
  prodi[,validodal:=as.Date(validodal)]

  return(list(prodi,prodi_a))
}

#Dialysis
query_periodi <- function(start=NULL,end=Sys.Date(),dbconfig) {
  require(data.table)
  require(DBI)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start))
    start<-end %m-% months(3)
  query<-paste("SELECT diaperiodi.codpaz, diaperiodi.dtcarico, diaperiodi.dtscarico,
               centri_1.dscentro as centroCarico, centri_2.dscentro as centroScarico,
               motcarico.dsmotcarico,motscarico.dsmotscarico,
               t027insuccessotx.t027xtinsuccesso,tipotratt.dstipotratt,
               anagr.cognome,anagr.nome, anagr.datanasc, anagr.datadecesso, anagr.codfisc,
               diaperiodi.dtin,diaperiodi.dted
               FROM ((diaperiodi LEFT JOIN tipotratt ON diaperiodi.pertipotra = tipotratt.idtipotratt)
               LEFT JOIN motcarico ON diaperiodi.motcarico = motcarico.idmotcarico
               LEFT JOIN motscarico ON diaperiodi.motscarico = motscarico.idmotscarico
               LEFT JOIN centri AS centri_1 ON diaperiodi.centrocar = centri_1.idcentro
               LEFT JOIN centri AS centri_2 ON diaperiodi.centroscar = centri_2.idcentro
               LEFT JOIN t027insuccessotx ON diaperiodi.causainsuc = t027insuccessotx.t027idinsuccesso)
               LEFT JOIN anagr ON diaperiodi.codpaz = anagr.codpaz
               WHERE diaperiodi.dtcarico>='",as.Date(start),"' AND diaperiodi.dtcarico<='",
               as.Date(end)+days(1),"' AND diaperiodi.stato='I'",sep="")

  rs <- dbSendQuery(con, query)
  periodi<-data.table(dbFetch(rs,n=-1))
  dbClearResult(rs)
  dbDisconnect(con)

  periodi[,datanasc:=as.Date(datanasc)]
  periodi[,dtcarico:=as.Date(dtcarico)]
  periodi[,dtscarico:=as.Date(dtscarico)]
  return(periodi)
}

#Personal
query_personale <- function(dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT medici.nome, medici.siglaturno, medici.matricola
                FROM medici
                WHERE medici.ATTIVO='S'")

  rs <- dbSendQuery(con, query)
  personale<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)

  return(personale)
}

#Personal
query_turnisti <- function(dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT plavpers.nome, plavpers.sigla, plavpers.matricola
                FROM plavpers
                WHERE plavpers.stato='I'")

  rs <- dbSendQuery(con, query)
  personale<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)

  return(personale)
}

#Activity
query_attivita <- function(dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  query<-paste0("SELECT plavatt.dsattiv, plavatt.orariodef
                FROM plavatt
                WHERE plavatt.stato='I'")

  rs <- dbSendQuery(con, query)
  personale<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)

  return(personale)
}

#Holydays
query_assenze <- function(start=NULL,end=NULL,idpers=NULL,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start)&!is.null(end))
    start<-end %m-% months(3)

  query<-paste0("SELECT plavpers.nome, plavpers.sigla, plavasse.dt1, plavasse.dt2, plavasse.motivoasse, plavasse.dtin,
                plavasse.dted
                FROM plavpers LEFT JOIN plavasse ON (plavpers.idplavpers=plavasse.idplavpers)
                WHERE plavasse.stato='I' AND plavpers.stato='I'")
  if(!is.null(idpers)){
    idpers<-gsub("'","''",idpers)
    query<-paste0(query," AND plavpers.nome='",idpers,"'")
  }
  if(!is.null(start))
    query<-paste0(query," AND plavasse.dt1>='",as.Date(start),"' AND plavasse.dt1<='",as.Date(end),"'")

  rs <- dbSendQuery(con, query)
  assenze<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  assenze[,dt1:=as.Date(dt1)]
  assenze[,dt2:=as.Date(dt2)]
  assenze[,dtin:=as.Date(dtin)]
  assenze[,dted:=as.Date(dted)]
  return(assenze)
}

#Workplan
query_piano <- function(start=NULL,end=NULL,idpers=NULL,idact=NULL,dbconfig){
  require(DBI)
  require(data.table)
  #require(RMySQL)
  require(lubridate)

  con <- connect(drv = dbconfig$drv, dbname = dbconfig$database, host = dbconfig$server, port = dbconfig$port,
                 key = dbconfig$key)

  if(is.null(start)&!is.null(end))
    start<-end %m-% months(3)

  query<-paste0("SELECT plavpers.nome, plavpers.sigla, plavskema.dtplav, plavatt.dsattiv, plavatt.orariodef
                FROM plavatt LEFT OUTER JOIN plavskema ON (plavatt.idplavatt=plavskema.idplavatt)
                RIGHT JOIN plavpers ON (plavpers.idplavpers=plavskema.idplavpers)
                WHERE plavskema.stato='I' AND plavpers.stato='I' AND plavatt.stato='I'")
  if(!is.null(idpers)){
    idpers<-gsub("'","''",idpers)
    query<-paste0(query," AND plavpers.nome='",idpers,"'")
  }
  if(!is.null(idact))
    query<-paste0(query," AND plavatt.idplavatt=",idact)
  if(!is.null(start))
    query<-paste0(query," AND plavskema.dtplav>='",as.Date(start),"' AND plavskema.dtplav<='",as.Date(end),"'")

  rs <- dbSendQuery(con, query)
  schema<-data.table(dbFetch(rs,-1))
  dbClearResult(rs)
  dbDisconnect(con)
  schema[,dtplav:=as.Date(dtplav)]

  return(schema)
}
