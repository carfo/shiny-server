library(shiny)
library(shinyDND)
library(shinyjqui)
library(data.table)
items<-data.table(read.csv(file = "test.csv",header = F,as.is = T))
group<-"gruppo a"
ui <- fluidPage(
  titlePanel("Club Imprese famigliari"),
  sidebarLayout(
    # Sidebar with a slider input
    sidebarPanel(
      tags$style(type = "text/css",".dragelement {font-size: large};"),
      h2("Fattori individuati dal Gruppo 1"),
      dragSetUI("div1", textval = as.list(items[V2==group])$V1),
      width = 3
    ),
    mainPanel(tags$style(type = "text/css",".dropelement {background-image: url('background.png');
                         height:600px; width:850px}"),
              tags$style(type = "text/css",".footer {background-color: #D2232A}"),
              jqui_resizable(dropUI("div2", row_n = 40, col_n = 20),
                             options = list(minWidth= 850, minHeight = 600, maxWidth= 850, maxHeight = 600))
              #verbatimTextOutput("foo"),
    )
  ),
  div(class = "footer",
      br(),
      tags$img(src="logo.png"),
      br())
)

server = shinyServer(function(input, output,session) {
  session$onSessionEnded(function(){
    #rm(config,pos=.GlobalEnv)
    stopApp()})
})

# Run the application
shinyApp(ui = ui, server = server)
