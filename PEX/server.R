# Define server logic
shinyServer(function(input, output, session) {
  plasma_p<-reactiveFileReader(intervalMillis = 1000, filePath = "look up.csv", readFunc = read.csv2,
                               stringsAsFactors=F, session = session)

  observeEvent(plasma_p(),{
    ch<-c("",plasma_p()$Substance)
    names(ch)<-c("Manuale",plasma_p()$Substance)
    updateSelectInput(inputId = "substance", session = session,
                      choices = ch)
  })

  treatment<-reactive({
    treat<-param(w = input$weight, h = input$height, a = input$age, sex = input$sex, hct = input$hct,
                 perc_i = input$perc_i/100, c_iv = input$c_in)
    treat[,':='(b_flow = input$blood, f_factor = input$filtration/100, km = 1/input$m_w, prod = 27, dest = 20,
                t_pex = t_time(ve = input$v_exc,b_flow = input$blood,f_factor = input$filtration/100),
                v_exc = input$v_exc)]
    for(t in c(2:(48*60))){
      temp<-pex(t = t, v_exc = treat$v_exc[t-1], iv = treat$iv[t-1], b_flow = treat$b_flow[t-1],
                f_factor = treat$f_factor[t-1], ev = treat$ev[t-1], c_iv_prec = treat$c_iv[t-1],
                m_iv_prec = treat$m_iv[t-1], c_ev_prec = treat$c_ev[t-1], m_ev_prec = treat$m_ev[t-1],
                km = treat$km[t-1], dest = treat$dest[t-1], prod = treat$prod[t-1],
                perc_i = input$perc_i/100, mw = input$m_w)
      l <- list(treat,temp)
      treat<-rbindlist(l, use.names=TRUE, fill=TRUE)
      rm(l,temp)
    }
    treat
  })

  output$conc_plot<-renderHighchart(highchart() %>%
                                      hc_chart(type = "line",zoomType = "x", panning=T, panKey="shift") %>%
                                      hc_title(text = "Andamento concentrazioni PEX") %>%
                                      hc_xAxis(title = list(text = "Tempo [min]"),
                                               plotBands = list(
                                                 list(from = 0, to = treatment()$t_pex[1],
                                                      color = "rgba(100, 0, 0, 0.1)",
                                                      label = list(text = "PEX"))),
                                               plotLines = list(
                                                 list(label = list(text = "24h"),
                                                      color = "rgba(100, 0, 0, 0.5)",
                                                      width = 2,
                                                      value = 24*60),
                                                 list(label = list(text = "48h"),
                                                      color = "rgba(100, 0, 0, 0.5)",
                                                      width = 2,
                                                      value = 48*60))) %>%
                                      hc_yAxis(title = list(text = "Concentrazione [mg/ml]")) %>%
                                      hc_add_series(data =treatment()$c_iv,
                                                    name = "Concentrazione Intravascolare") %>%
                                      hc_add_series(data =treatment()$c_ev,
                                                    name = "Concentrazione Extravascolare") %>%
                                      hc_tooltip(valueDecimals= 1, valueSuffix=' mg/ml') %>%
                                      hc_exporting(enabled = TRUE) %>%
                                      hc_add_theme(hc_theme_merge(
                                        hc_theme_538(),
                                        hc_theme(
                                          chart = list(
                                            backgroundColor = "transparent"
                                          ))
                                      ))
                                    )

  output$rem_plot<-renderHighchart(highchart() %>%
                                     hc_chart(type = "column") %>%
                                     hc_title(text = "Rimozione percentuale") %>%
                                     hc_xAxis(categories = c("Fine PEX","48 h")) %>%
                                     hc_yAxis(title = list(text = "Percentuale [%]")) %>%
                                     hc_add_series(data =
                                                     c(100*((treatment()$m_iv[1]+treatment()$m_ev[1])-(treatment()$m_iv[treatment()$t_pex[1]]+treatment()$m_ev[treatment()$t_pex[1]]))/(treatment()$m_iv[1]+treatment()$m_ev[1]),
                                                   100*((treatment()$m_iv[1]+treatment()$m_ev[1])-(last(treatment()$m_iv)+last(treatment()$m_ev)))/(treatment()$m_iv[1]+treatment()$m_ev[1])),
                                                   name = "% rispetto alla massa totale")%>%
                                     hc_tooltip(valueDecimals= 0, valueSuffix='%')%>%
                                     hc_add_theme(hc_theme_merge(
                                       hc_theme_538(),
                                       hc_theme(
                                         colors = c('#68cdff','#FF2700','#FF2700'),
                                         chart = list(
                                           backgroundColor = "transparent"
                                         ))
                                     ))
                                   )

  output$v_plasm<-renderText({
    paste("Volume plasmatico stimato:", epv(weight = input$weight, hct = input$hct))
  })

  output$exc_plasm<-renderText({
    paste("Rapporto volume scambio / volume plasmatico: ",
          round(input$v_exc/(epv(weight = input$weight, hct = input$hct)*1000),1))
  })

  output$tr_time<-renderText({
    paste("Durata del trattamento:", t_time(input$v_exc,input$blood,input$filtration/100), "minuti")
  })

  observeEvent(input$substance,{
    if(input$substance!=""){
      updateNumericInput(session = session, inputId = "m_w",
                       value = plasma_p()[plasma_p()$Substance==input$substance,]$MW)
      # updateNumericInput(session = session, inputId = "c_in",
      #                    value = plasma_p[Substance==input$substance]$P.concentration)
      updateSliderInput(session = session, inputId = "perc_i",
                       value = plasma_p()[plasma_p()$Substance==input$substance,]$Intravascular)
    }
  })

  session$onSessionEnded(function(){
    rm(config,pos=.GlobalEnv)
    stopApp()})
})