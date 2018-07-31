library(shiny)


# Define UI
shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  tags$head(tags$style(
    HTML(
      "body {
      background-color: #fff8e8;
      font-size: 20px;
      }"
    )
  )),
  # background-image: url('*.jpg');
  # background-position: center;
  # background-repeat: no-repeat;
  # background-attachment: fixed;
  # background-size: cover;
  #img(src='human body.jpg', align = "right"),

  # Application title
  titlePanel("1- and 2-Rescuer Adult BLS With AED"),
  tags$h2("Skills Testing Sheet"),
    fluidRow(
      column(2,
                    tags$h3("Partecipanti"),
                    fileInput("partecipanti", "Elenco partecipanti",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    ),
                    #w,h,a,sex,hct,perc_i,c_iv,b_flow,f_factor,km,dest,prod
                    tags$h4(tags$strong("Anagrafica")),
                    br(),
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
    ),
    column(9,
      # checkboxGroupInput(inputId = "firstBLS",
      #                      label = "1-Rescuer Adult BLS Skills Evaluation\n During this first phase, evaluate the first rescuer’s ability to initiate BLS and deliver high-quality CPR for 5 cycles.",
      #                      choices = c("ASSESSES: Checks for response and for no breathing or no normal breathing, only gasping (at least 5 seconds but no more than 10 seconds)",
      #                                      "ACTIVATES emergency response system",
      #                                      "Checks for PULSE (no more than 10 seconds)",
      #                                      "Correct compression HAND PLACEMENT",
      #                                      "ADEQUATE RATE: At least 100/min (ie, delivers each set of 30 chest compressions in 18 seconds or less)",
      #                                      "ADEQUATE DEPTH: Delivers compressions at least 2 inches in depth (at least 23 out of 30)",
      #                                      "ALLOWS COMPLETE CHEST RECOIL (at least 23 out of 30)",
      #                                      "MINIMIZES INTERRUPTIONS: Gives 2 breaths with pocket mask in less than 10 seconds"
      #                                      ), inline = F),
      tags$h3("1-Rescuer Adult BLS Skills Evaluation"),
      prettyCheckboxGroup(inputId = "firstBLSp",
                         label = "During this first phase, evaluate the first rescuer’s ability to initiate BLS and deliver high-quality CPR for 5 cycles.",
                         choices = c("ASSESSES: Checks for response and for no breathing or no normal breathing, only gasping (at least 5 seconds but no more than 10 seconds)",
                                     "ACTIVATES emergency response system",
                                     "Checks for PULSE (no more than 10 seconds)",
                                     "Correct compression HAND PLACEMENT",
                                     "ADEQUATE RATE: At least 100/min (ie, delivers each set of 30 chest compressions in 18 seconds or less)",
                                     "ADEQUATE DEPTH: Delivers compressions at least 2 inches in depth (at least 23 out of 30)",
                                     "ALLOWS COMPLETE CHEST RECOIL (at least 23 out of 30)",
                                     "MINIMIZES INTERRUPTIONS: Gives 2 breaths with pocket mask in less than 10 seconds"),
                         status = "success", bigger=T, outline=T, icon = icon("check"),
                         width = "100%"
                         ),
      tags$h3("Second Rescuer AED Skills Evaluation and SWITCH"),
      # prettyCheckboxGroup(inputId = "secondBLSp",
      #                     label = "During this next phase, evaluate the second rescuer’s ability to use the AED and both rescuers’ abilities to switch roles.",
      #                     choices = c("DURING FIFTH SET OF COMPRESSIONS: Second rescuer arrives with AED and bag-mask device, turns on AED, and applies pads",
      #                                 "First rescuer continues compressions while second rescuer turns on AED and applies pads",
      #                                 "Second rescuer clears victim, allowing AED to analyze—RESCUERS SWITCH",
      #                                 "If AED indicates a shockable rhythm, second rescuer clears victim again and delivers shock"),
      #                     status = "success", bigger=T, outline=T, icon = icon("check"),
      #                     width = "100%"
      # ),
      checkboxGroupButtons(
        inputId = "secondBLSp",
        label = "During this next phase, evaluate the second rescuer’s ability to use the AED and both rescuers’ abilities to switch roles.",
        choices = c("DURING FIFTH SET OF COMPRESSIONS: Second rescuer arrives with AED and bag-mask device, turns on AED, and applies pads",
                    "First rescuer continues compressions while second rescuer turns on AED and applies pads",
                    "Second rescuer clears victim, allowing AED to analyze—RESCUERS SWITCH",
                    "If AED indicates a shockable rhythm, second rescuer clears victim again and delivers shock"),
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon")),
          direction="vertical", individual=TRUE, width="100%", size="lg", status="primary"
      ),
      tags$h3("First Rescuer Bag-Mask Ventilation"),
      awesomeCheckboxGroup(
        inputId = "firstBAG",
        label = "During this next phase, evaluate the first rescuer’s ability to give breaths with a bag-mask.",
        choices = c("SECOND RESCUER gives 30 compressions immediately after shock delivery (for 2 cycles)",
                    "FIRST RESCUER successfully delivers 2 breaths with bag-mask (for 2 cycles)"),
        width = "100%",
        status = "success"
      ),
      HTML("<center><h3>AFTER 2 CYCLES, STOP THE EVALUATION</h3></center>"),
      textOutput("txt")
    ))
))
