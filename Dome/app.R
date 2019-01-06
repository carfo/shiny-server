source("global.R")
library(shinydashboard)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "navy"

##### SIDEBAR ####
sidebar <- dashboardSidebar(
  #sidebarSearchForm(label = "Cerca un cliente", "searchClientText", "searchClientButton"),
  #sidebarSearchForm(label = "Cerca un ordine", "searchOrderText", "searchOrderButton"),
  actionButton(inputId = "new_client", label = "Nuovo Cliente", icon = icon("address-card")),
  actionButton(inputId = "new_order", label = "Nuovo Ordine", icon = icon("cart-plus")),
  sidebarSearchForm(label = "Cerca un Serial Number", "searchSNText", "searchSNButton"),
  sidebarMenu(
    fileInput("clienti", "Elenco clienti",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    selectInput(
      inputId = "nome",
      label = "Cliente:",
      choices = ""
    )
    #,
    #bookmarkButton(label = "Salva tutte cose", title="Non si sa mai")
  )
)
##### BODY ####
body <- dashboardBody(
  tags$head(
    tags$style(HTML('
                    .content-wrapper {
                    background-color: #e6e6e6;
                    }
                    .skin-blue .main-header .navbar {
                    background-color: #1a1a1a;
                    }
                    .skin-blue .main-header .navbar .sidebar-toggle:hover {
                    background-color: #808080;
                    }
                    .skin-blue .main-header .logo{
                    font-family: "Georgia", Times, "Times New Roman", serif;
                    font-weight: bold;
                    font-size: 24px;
                    background-color: #1a1a1a;
                    }
                    .skin-blue .main-header .logo:hover {
                    background-color: #808080;
                    }


                    .box-header .box-title {
                    font-weight: bold;
                    }
                    .box{
                    background-color: transparent;
                    border-color: transparent;
                    }')
               )
    ),
  fluidRow(
    box(
      title = "Ordini", width = NULL,
      solidHeader = TRUE,
      dataTableOutput("ordini")
    ),
    box(
      title = "Pompe", width = NULL,
      solidHeader = TRUE,
      dataTableOutput("pompe"),
      actionBttn(inputId = "new_shipment", label = "Spedisci", style = "minimal", color = "primary")
    )
  ),
  # Boxes with solid headers
  fluidRow(
    box(
      title = "Prodotti da inserire", width = NULL,
      solidHeader = TRUE,
      textAreaInput(inputId="newSN", label="", value = NULL, width = "100%",
                    height = NULL, cols = NULL, rows = 6, placeholder = "Nuovi codici",
                    resize = NULL)
    )
  ),
  fluidRow(
    column(width=12, offset=5,
       actionBttn(inputId = "associa", label = "Associa a ordine", style = "minimal", color = "primary")
    )
  )
    )
##### MESSAGES ####
messages <- dropdownMenu(type = "messages",
                         messageItem(
                           from = "Sales Dept",
                           message = "Sales are steady this month."
                         ),
                         messageItem(
                           from = "New User",
                           message = "How do I register?",
                           icon = icon("question"),
                           time = "13:45"
                         ),
                         messageItem(
                           from = "Support",
                           message = "The new server is ready.",
                           icon = icon("life-ring"),
                           time = "2014-12-01"
                         )
)
##### NOTIFICATIONS ####
notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
                              notificationItem(
                                text = "5 new users today",
                                icon("users")
                              ),
                              notificationItem(
                                text = "12 items delivered",
                                icon("truck"),
                                status = "success"
                              ),
                              notificationItem(
                                text = "Server load at 86%",
                                icon = icon("exclamation-triangle"),
                                status = "warning"
                              )
)
##### TASKS ####
tasks <- dropdownMenu(type = "tasks", badgeStatus = "success",
                      taskItem(value = 90, color = "green",
                               "Documentation"
                      ),
                      taskItem(value = 17, color = "aqua",
                               "Project X"
                      ),
                      taskItem(value = 75, color = "yellow",
                               "Server deployment"
                      ),
                      taskItem(value = 80, color = "red",
                               "Overall project"
                      )
)
##### HEADER ####
header <- dashboardHeader(
  title = "Gestionale"
  #,
#  messages,
#  notifications,
#  tasks
)
##### UI ####
ui<-function(req) {
  dashboardPage(header, sidebar, body)
}
##### SERVER ####
server <- function(session, input, output) {
  ##### DATA ####
  #clientFile<-"data.csv"


  db<-reactiveValues(clientData = data.table(dbReadTable(pool,"Clienti")),
                     orderData = data.table(dbReadTable(pool,"Ordini")),
                     pumpData = data.table(dbReadTable(pool,"Pompe")),
                     shipData = data.table(dbReadTable(pool,"Spedizioni")),
                     osData = data.table(dbReadTable(pool,"Ordine_spedizione")),
                     psData = data.table(dbReadTable(pool,"Pompa_Spedizione")))
  isolate(db$orderData[,Data:=as.Date(Data, origin = "1970-01-01")])
  isolate(db$shipData[,Data:=as.Date(Data, origin = "1970-01-01")])

  rv <- reactiveValues(
    new_pump = FALSE,
    new_order = FALSE,
    new_client = FALSE,
    new_ship = FALSE
  )
  ##### INSERIMENTO NUOVO CLIENTE ####
  clientModal <- function(failed = FALSE) {
    modalDialog(
      useSweetAlert(),
      textInput("cod_cliente", "Codice",
                placeholder = 'Inserisci il codice cliente'
      ),
      textInput("nome_cliente", "Nome",
                placeholder = 'Inserisci il nome cliente'
      ),
      textInput("indirizzo_cliente", "Indirizzo",
                placeholder = "Inserisci l'indirizzo del cliente"
      ),
      textInput("telefono_cliente", "Numero Telefono",
                placeholder = "Inserisci il numero di telefono del cliente"
      ),
      textInput("email_cliente", "Indirizzo email",
                placeholder = "Inserisci l'indirizzo email del cliente"
      ),
      span('Inserisci le informazioni del cliente'),
      if (failed)
        div(tags$b("Codice cliente non valido", style = "color: red;")),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("insert_client", "OK")
      )
    )
  }

  observeEvent(input$insert_client, {
    # Check that data object exists and is data frame.
    if (!is.null(input$cod_cliente) && nzchar(input$cod_cliente) &&
        NROW(db$clientData[Codice.cliente==input$cod_cliente])==0) {
      print(input$cod_cliente)
      new_client<-data.table(ID = max(db$clientData$ID,na.rm=T)+1,
                             Codice.cliente = input$cod_cliente,
                             Nome = input$nome_cliente,
                             Indirizzo = input$indirizzo_cliente,
                             Telefono = input$telefono_cliente,
                             email = input$email_cliente)
      db$clientData<-rbindlist(list(db$clientData,new_client))
      # inserire nel db
      dbWriteTable(pool,"Clienti",db$clientData)

      rv$new_client<-TRUE
      sendSweetAlert(
        session = session, title = "Success !!", text = "All in order", type = "success"
      )
      removeModal()
    } else {
      showModal(clientModal(failed = TRUE))
      # sendSweetAlert(
      #   session = session, title = "Error !!", text = "It's broken...", type = "error"
      # )
    }
  })

  observeEvent(input$new_client, {
    showModal(clientModal())
  })

  ##### INSERIMENTO NUOVO ORDINE ####
  orderModal <- function(failed = FALSE,client=NULL) {
    choices<-db$clientData$Codice.cliente
    names(choices)<-db$clientData$Nome
    modalDialog(
      useSweetAlert(),
      textInput("cod_ordine", "Codice",
                placeholder = 'Inserisci il codice ordine'
      ),
      pickerInput("ord_cliente", "Cliente",choices = choices,multiple = FALSE,
                  options = list(`live-search` = TRUE,size = 5,
                                 title = "Scegli il cliente",
                                 style="btn-primary")
      ),
      dateInput("data_ordine","Data dell'ordine",value = Sys.Date(),format="dd-mm-yyyy",
                language="it"),
      numericInput("quant_ordine", "Quantità ordine",value=0),
      span("Inserisci i dati dell'ordine",
           ""),
      if (failed)
        div(tags$b("Ordine non valido", style = "color: red;")),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("insert_order", "OK")
      )
    )
  }

  observeEvent(input$insert_order, {
    # Check that data object exists.
    if (!is.null(input$cod_ordine) && nzchar(input$cod_ordine) && input$ord_cliente!="") {
      #print(paste0(input$ord_cliente,input$cod_ordine))
      new_order<-data.table(ID= max(db$orderData$ID,na.rm=T)+1,
                            Codice_ordine = input$cod_ordine,
                            Data = input$data_ordine,
                            Q = input$quant_ordine,
                            ID_cliente = input$ord_cliente)
      db$orderData<-rbindlist(list(db$orderData,new_order))
      # inserire nel db
      dbWriteTable(pool,"Ordini",db$orderData)

      rv$new_order<-TRUE
      sendSweetAlert(
        session = session, title = "Success !!", text = "All in order", type = "success"
      )
      removeModal()
    } else {
      showModal(orderModal(failed = TRUE))
      # sendSweetAlert(
      #   session = session, title = "Error !!", text = "It's broken...", type = "error"
      # )
    }
  })

  observeEvent(input$new_order, {
    showModal(orderModal())
  })

  ##### INSERIMENTO NUOVO TIPO POMPA ####
  # pumpModal <- function(failed = FALSE) {
  #   modalDialog(
  #     useSweetAlert(),
  #     textInput("cod_modello", "Modello",
  #               placeholder = 'Inserisci il modello'
  #     ),
  #     textAreaInput("caratteristiche_pompa", "Caratteristiche",
  #                   placeholder = 'Inserisci le caratteristiche del prodotto',rows = 6
  #     ),
  #     span("Inserisci un modello di pompa",
  #          ""),
  #     if (failed)
  #       div(tags$b("Modello non valido", style = "color: red;")),
  #
  #     footer = tagList(
  #       modalButton("Cancel"),
  #       actionButton("insert_modello", "OK")
  #     )
  #   )
  # }
  #
  # observeEvent(input$insert_modello, {
  #   # Check that data object exists and is data frame.
  #   if (!is.null(input$cod_modello) && nzchar(input$cod_modello)) {
  #     print(input$cod_modello)
  #     # inserire nel db
  #     sendSweetAlert(
  #       session = session, title = "Success !!", text = "All in order", type = "success"
  #     )
  #     removeModal()
  #   } else {
  #     showModal(pumpModal(failed = TRUE))
  #     sendSweetAlert(
  #       session = session, title = "Error !!", text = "It's broken...", type = "error"
  #     )
  #   }
  # })
  #
  # observeEvent(input$new_pump, {
  #   showModal(pumpModal())
  # })

  ##### INSERIMENTO NUOVA SPEDIZIONE ####
  shipModal <- function(failed = FALSE) {
    modalDialog(
      useSweetAlert(),
      textInput("cod_spedizione", "DDT",
                placeholder = 'Inserisci il DDT'
      ),
      dateInput("data_spedizione","Data della spedizione",value = Sys.Date(),format="dd-mm-yyyy",language="it"),
      span("Inserisci una nuova spedizione",
           ""),
      if (failed)
        div(tags$b("Spedizione non valida", style = "color: red;")),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("insert_shipment", "OK")
      )
    )
  }

  observeEvent(input$insert_shipment, {
    req(input$pompe_rows_selected)
    # Check that data object exists.
    if (!is.null(input$cod_spedizione) && nzchar(input$cod_spedizione)
        ##aggiunta controlli pompe selezionate
    ) {
      pompeAssegnate <- db$orderData[ID_cliente==input$nome][input$ordini_rows_selected][
        db$pumpData,on=c("Codice_ordine"="ID_ordine"),nomatch=0]
      setnames(pompeAssegnate,c("i.ID","Data"),c("ID_pompa","Data.Ordine"))
      pompeSpedite <-db$shipData[db$psData[pompeAssegnate,on=c("ID_pompa")],on=c("ID"="ID_spedizione")]
      setnames(pompeSpedite,c("Data"),c("Data.Spedizione"))
      if(pompeSpedite[input$pompe_rows_selected][!is.na(DDT)]){
        sendSweetAlert(
          session = session, title = "Errore !!", text = "Le pompe risultano già spedite", type = "error"
        )
      }
      else{
        new_shipment <-data.table(ID = max(db$shipData$ID,na.rm=T)+1,
                                  DDT = input$cod_spedizione,
                                  Data = input$data_spedizione)
        new_ps <-data.table(ID_pompa = pompeSpedite[input$pompe_rows_selected]$ID_pompa,
                            ID_spedizione = rep(max(db$shipData$ID,na.rm=T)+1,length(input$pompe_rows_selected)))
        new_os <-data.table(ID_ordine = pompeSpedite[input$pompe_rows_selected]$Codice_ordine[1],
                            ID_spedizione = max(db$shipData$ID,na.rm=T)+1)
        print(new_os)
        # associazione psData e osData
        db$shipData<-rbindlist(list(db$shipData,new_shipment))
        db$psData<-rbindlist(list(db$psData,new_ps))
        db$osData<-rbindlist(list(db$osData,new_os))
        # inserire nel db
        dbWriteTable(pool,"Ordine_spedizione", db$osData, overwrite=T)
        dbWriteTable(pool,"Pompa_Spedizione", db$psData, overwrite=T)
        dbWriteTable(pool,"Spedizione", db$shipData, overwrite=T)
        rv$new_ship<-TRUE
        sendSweetAlert(
          session = session, title = "Success !!", text = "All in order", type = "success"
        )
      }
      removeModal()
    } else {
      showModal(shipModal(failed = TRUE))
      # sendSweetAlert(
      #   session = session, title = "Error !!", text = "It's broken...", type = "error"
      # )
    }
  })

  observeEvent(input$new_shipment, {
    showModal(shipModal())
  })

  ##### CONTROLLO APP####
  #Elenco clienti
  observe({
    choices<-db$clientData$Codice.cliente
    names(choices)<-db$clientData$Nome
    updateSelectInput(session, "nome",
                      choices = choices
    )
  })
  #Ordini del cliente
  observeEvent(c(
    input$nome,
    rv$new_order),
    {
      pompeAssegnate <- db$orderData[ID_cliente==input$nome][
        db$pumpData,on=c("Codice_ordine"="ID_ordine"),nomatch=0]
      setnames(pompeAssegnate,c("i.ID","Data"),c("ID_pompa","Data.Ordine"))
      pompeSpedite <-db$shipData[db$psData[pompeAssegnate,on=c("ID_pompa")],on=c("ID"="ID_spedizione")]
      setnames(pompeSpedite,c("Data"),c("Data.Spedizione"))
      ordini<-db$orderData[ID_cliente==input$nome,.SD,.SDcols=c("ID","Codice_ordine","Data","Q")][
        pompeSpedite[!is.na(Data.Spedizione),.(Spedite=.N),by=Codice_ordine], on="Codice_ordine"]
      ordini[,Spedite:=Spedite/Q]
      setorder(ordini,"ID")
      output$ordini <- renderDT(formatStyle(datatable(ordini, options = list(columnDefs= list(list(visible=FALSE,
                                                                                                   targets=c(1,5))))),
                                            target="row","Spedite",
                                            backgroundColor = styleInterval(1, c("#EF3B2C","#74C476"))))
      output$pompe <- renderDT(NULL)
  })
  #Pompe dell'ordine e stato
  observeEvent(c(
    input$ordini_rows_selected,
    rv$new_pump,
    rv$new_ship),
    {
      req(input$ordini_rows_selected)
      pompeAssegnate <- db$orderData[ID_cliente==input$nome][input$ordini_rows_selected][
        db$pumpData,on=c("Codice_ordine"="ID_ordine"),nomatch=0]
      setnames(pompeAssegnate,c("i.ID","Data"),c("ID_pompa","Data.Ordine"))
      pompeSpedite <-db$shipData[db$psData[pompeAssegnate,on=c("ID_pompa")],on=c("ID"="ID_spedizione")]
      setnames(pompeSpedite,c("Data"),c("Data.Spedizione"))
      pompeSpedite[,Spedite:=as.numeric(!is.na(Data.Spedizione))]
      output$pompe <- renderDT(if(length(input$ordini_rows_selected))
        formatStyle(datatable(pompeSpedite[,.(Serial.Number,DDT,Data.Spedizione,Spedite)],
                              extensions = 'Scroller', options = list(deferRender = TRUE,
                                                                      scrollY = 200,
                                                                      scroller = TRUE,
                                                                      columnDefs = list(list(visible=FALSE,
                                                                                             targets=c(4))))),
                    "Spedite", target = "row",
                    backgroundColor = styleEqual(c(0, 1), c("#EF3B2C","#74C476"))))

      if(rv$new_pump)
        isolate(rv$new_pump<-FALSE)

      if(rv$new_ship)
        isolate(rv$new_ship<-FALSE)
  })

  #Aggiunta pompe
  observeEvent(input$newSN,{
    req(input$newSN)
    sn<-unique(stri_split_lines1(input$newSN))
    if(NROW(db$pumpData[Serial.Number%in%sn])>0)
      sendSweetAlert(
        session = session, title = "Error !!", text = "Pompa già in db", type = "error"
      )
  })
  #Aggiunta pompe a ordine
  observeEvent(input$associa,{
    req(input$newSN)
    sn<-unique(stri_split_lines1(input$newSN))
    if(NROW(db$pumpData[Serial.Number%in%sn])>0)
      sendSweetAlert(
        session = session, title = "Errore!!", text = "Alcune pompe sono già in db", type = "error"
      )
    else{
      if(length(input$ordini_rows_selected)<1||length(input$ordini_rows_selected)>1)
        sendSweetAlert(
          session = session, title = "Attenzione!!", text = "Seleziona un ordine", type = "warning"
        )
      else{
        new_pump <- data.table(ID=max(db$pumpData$ID,na.rm=T)+1:length(sn),
                              Serial.Number=sn,
                              ID_Tipo=rep("NA",length(sn)),
                              ID_ordine=rep(db$orderData[ID_cliente==input$nome][
                                input$ordini_rows_selected]$Codice_ordine,
                                            length(sn)))
        db$pumpData<-rbindlist(list(db$pumpData,new_pump))
        dbWriteTable(pool,"Pompe",db$pumpData, overwrite=T)
        rv$new_pump<-TRUE
      }
    }
  })
  # Search
  searchModal <- function(failed = FALSE) {
    modalDialog(
      dataTableOutput("searchDT"),
      if (failed)
        div(tags$b("Codice non trovato", style = "color: red;")),
      footer = tagList(
        modalButton("Ok")
      )
    )
  }

  observeEvent(input$searchSNButton,{
    req(input$searchSNText)
    print(input$searchSNText)
    if(NROW(db$pumpData[Serial.Number%like%trimws(input$searchSNText)])>0){
      output$searchDT<-renderDT(datatable(
        db$pumpData[Serial.Number%like%trimws(input$searchSNText)]))
      showModal(searchModal())
    }else{
      output$searchDT<-renderDT(NULL)
      showModal(searchModal(failed = T))
    }

  })
  ##### CLOSURE####
  session$onSessionEnded(function(){
    #rm(config,pos=.GlobalEnv)
    stopApp()})
}
##### APP ####
enableBookmarking(store = "server")
shinyApp(ui, server)
