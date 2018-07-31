# Define server logic
shinyServer(function(input, output, session) {
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
  })

  output$txt <- renderText({
    firstBLS <- paste(input$firstBLSp, collapse = ", \n")
    paste("Your assessment ", firstBLS, collapse = "\n")
  })

  # output$image <- renderImage({
  #   # Get width and height of image output
  #   width  <- session$clientData$output_image_width
  #   height <- session$clientData$output_image_height
  #
  #   # Return a list containing information about the image
  #   list(
  #     src = "www/acls.gif",
  #     contentType = "image/gif",
  #     width = width,
  #     height = height,
  #     alt = "This is alternate text"
  #   )
  # }, deleteFile = FALSE)
  #
  # output$image_clickinfo <- renderPrint({
  #   cat("Click:\n")
  #   str(input$image_click)
  # })
  #
  # output$image_hoverinfo <- renderPrint({
  #   cat("Hover (throttled):\n")
  #   str(input$image_hover)
  # })
  #
  # output$image_brushinfo <- renderPrint({
  #   cat("Brush (debounced):\n")
  #   str(input$image_brush)
  # })

  session$onSessionEnded(function(){
    #rm(config,pos=.GlobalEnv)
    stopApp()})
})
