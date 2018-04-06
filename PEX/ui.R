# Define UI for application
shinyUI(
  fluidPage(theme=shinytheme("sandstone"),
                  tags$head(
                    tags$style(
                      HTML("body {
                           background-image: url('human body.jpg');
                           background-position: center;
                           background-repeat: no-repeat;
                           background-attachment: fixed;
                           background-size: cover;
                           background-color: #fff8e8;
                           }
                           .dropdown-menu {
                           background-color: #fff2d8;
                           }"))),
                           #img(src='human body.jpg', align = "right"),

  # Application title
  titlePanel("Simulazione Plasmaferesi"),

  mainPanel(
    dropdownButton(
      tags$h3("Parametri"),
      #w,h,a,sex,hct,perc_i,c_iv,b_flow,f_factor,km,dest,prod
      tabsetPanel(
        tabPanel("Paziente",
                 br(),
                 sliderInput(inputId="height", label = "Altezza [cm]", min = 125, max = 225, value = 170, round = T),
                 sliderInput(inputId="weight", label = "Peso [kg]", min = 25, max = 150, value = 70, round = T),
                 sliderInput(inputId="age", label = "Età [anni]", min = 18, max = 100, value = 30, round = T),
                 radioGroupButtons(inputId = "sex",
                                   label = "Sesso", choices = c("M","F"), individual = TRUE,
                                   checkIcon = list(yes = tags$i(class = "fa fa-check-square"),
                                                    no = tags$i(class = "fa fa-square-o"))),
                 sliderInput(inputId="hct", label = "Ematocrito [%]", min = 10, max = 60, value = 45, round = T),
                 strong(textOutput(outputId="v_plasm"))),
        tabPanel("Sostanza",
                 br(),
                 selectInput(inputId = "substance",label = "Sostanza da rimuovere:", choices = ""),
                 numericInput(inputId="c_in",label = "Concentrazione iniziale [mg/ml]", value = 100),
                 numericInput(inputId="m_w",label = "Peso molecolare [kDa]", value = 950),
                 sliderInput(inputId="perc_i",label = "Percentuale presenza intravascolare [%]", value = 50, round = T,
                              min = 0, max = 100,animate=T)),
        tabPanel("Trattamento",
                 br(),
                 sliderInput(inputId="blood", label = "Portata ematica [ml/min]", min = 100, max = 150, value = 150,
                             round = T),
                 sliderInput(inputId="filtration", label = "Frazione filtrazione [%]", min = 5, max = 20, value = 15,
                             round = T),
                 sliderInput(inputId="v_exc", label = "Volume scambio [ml]", min = 500, max = 8000, value = 2500,
                             step = 500),
                 strong(textOutput(outputId="exc_plasm")),strong(textOutput(outputId="tr_time")))
      ),
      circle = TRUE, icon = icon("gear"), width = "400px",
      tooltip = tooltipOptions(title = "Inserisci i parametri")
    ),
    fluidRow(
      column(width = 9,
             br(),
             highchartOutput(outputId = "conc_plot", width = "100%", height = "100%"),
             br(),
             highchartOutput(outputId = "rem_plot", width = "100%"))
    )
  )
  )
)
