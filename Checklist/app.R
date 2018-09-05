library(shinydashboard)
source("global.R")

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "navy"

##### SIDEBAR ####
sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Cerca un candidato", "searchText", "searchButton"),
  sidebarMenu(
    fileInput("partecipanti", "Elenco partecipanti",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    selectInput(
      inputId = "nome",
      label = "Cognome e nome:",
      choices = ""
    ),
    dateInput("nascita", "Data di nascita:",
              value = Sys.Date()%m-%years(25),format = "dd-mm-yyyy"),
    radioGroupButtons(
      inputId = "sex",
      label = "Sesso",
      choices = c("M", "F"),
      individual = TRUE,
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square"),
        no = tags$i(class = "fa fa-square-o")
      )
    ),
    selectInput(
      inputId = "professione",
      label = "Professione:",
      choices = c("","Infermiere","Specializzando","Medico Specializzato")
    )
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
      title = "1-Rescuer Adult BLS Skills Evaluation", width = NULL,
      solidHeader = TRUE,
      awesomeCheckboxGroup(
        inputId = "firstBLSp",
        label = "During this first phase, evaluate the first rescuer's ability to initiate BLS and deliver high-quality CPR for 5 cycles.",
        c("ASSESSES Checks for response and for no breathing or no normal breathing, only gasping (at least 5 seconds but no more than 10 seconds)",
          "ACTIVATES emergency response system",
          "Checks for PULSE (no more than 10 seconds)",
          "Correct compression HAND PLACEMENT",
          "ADEQUATE RATE At least 100/min (ie, delivers each set of 30 chest compressions in 18 seconds or less)",
          "ADEQUATE DEPTH Delivers compressions at least 2 inches in depth (at least 23 out of 30)",
          "ALLOWS COMPLETE CHEST RECOIL (at least 23 out of 30)",
          "MINIMIZES INTERRUPTIONS Gives 2 breaths with pocket mask in less than 10 seconds"),
        width = "100%",
        status = "success"
      )
    )
  ),

  # Boxes with solid headers
  fluidRow(
    box(
      title = "Second Rescuer AED Skills Evaluation and SWITCH", width = NULL,
      solidHeader = TRUE,
      checkboxGroupInput(
        inputId = "secondBLSp",
        label = "During this next phase, evaluate the second rescuer's ability to use the AED and both rescuers' abilities to switch roles.",
        choices = c("DURING FIFTH SET OF COMPRESSIONS: Second rescuer arrives with AED and bag-mask device, turns on AED, and applies pads",
                    "First rescuer continues compressions while second rescuer turns on AED and applies pads",
                    "Second rescuer clears victim, allowing AED to analyze-RESCUERS SWITCH",
                    "If AED indicates a shockable rhythm, second rescuer clears victim again and delivers shock"),
        inline=FALSE, width="100%"
      )
    )
  ),

  # Solid backgrounds
  fluidRow(
    box(
      title = "First Rescuer Bag-Mask Ventilation", width = NULL,
      solidHeader = TRUE,
      awesomeCheckboxGroup(
        inputId = "firstBAG",
        label = "During this next phase, evaluate the first rescuer's ability to give breaths with a bag-mask.",
        choices = c("SECOND RESCUER gives 30 compressions immediately after shock delivery (for 2 cycles)",
                    "FIRST RESCUER successfully delivers 2 breaths with bag-mask (for 2 cycles)"),
        width = "100%",
        status = "success"
      )
    )
  ),
  fluidRow(
    HTML("<center><h3>AFTER 2 CYCLES, STOP THE EVALUATION</h3></center>"),
    textOutput("txt")
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
  title = "Skills Testing Sheet",
  messages,
  notifications,
  tasks
)
##### UI ####
ui<-dashboardPage(header, sidebar, body)
##### SERVER ####
server <- function(session, input, output) {
  partecipantFile<-"data.csv"

  partecipantData <- reactiveFileReader(1000, session, partecipantFile,
                                        read.csv,as.is=T)

  observe({
    choices<-partecipantData()$CF
    names(choices)<-paste(partecipantData()$Cognome,partecipantData()$Nome)
    updateSelectInput(session, "nome",
                      choices = choices
    )
  })

  observeEvent(input$nome,{
    updateDateInput(session, "nascita",
                    value = as.Date(partecipantData()[partecipantData()$CF==input$nome,
                                                      ]$Data.di.nascita,
                                    format="%d-%m-%y"))
    updateRadioGroupButtons(session, "sex",
                            selected = partecipantData()[partecipantData()$CF==input$nome,
                                                         ]$Sesso)
    updateAwesomeCheckboxGroup(session,inputId = "firstBLSp",selected = character(0))
    updateCheckboxGroupInput(session,inputId = "secondBLSp",selected = character(0))
    updateAwesomeCheckboxGroup(session,inputId = "firstBAG",selected = character(0))
  })

  output$txt <- renderText({
    firstBLS <- paste(input$firstBLSp, collapse = ", \n")
    paste("Your assessment ", firstBLS, collapse = "\n")
  })

  session$onSessionEnded(function(){
    #rm(config,pos=.GlobalEnv)
    stopApp()})
}
##### APP ####
shinyApp(ui, server)
