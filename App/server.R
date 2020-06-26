# Aplicacion empresas

shinyServer(function(input, output, session) {
  # 
  # ######## PRIMERA VENTANA ===========================================================================

  data_f1 <- eventReactive(input$go,{
    data_f1 <- bd_afiliados %>%
      dplyr::filter(if (input$xsegmento != "Total") segmento_poblacional == input$xsegmento else TRUE,
                    if (input$xcategoria != "Total") categoria == input$xcategoria else TRUE)
    })

  # con_piramide <- eventReactive(input$go,{
  #   con_piramide <- consulta_piramide %>%
  #     filter(if (input$xsegmento != "Total") Segmento_poblacional == input$xsegmento else TRUE,
  #            if (input$xcategoria != "Total") categoria == input$xcategoria else TRUE,)
  # })

  # info_afil_tot
  output$info_afil_emp <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(input$xsegmento,format="s"),
      subtitle = "Segmento Individual",
      icon = icon("users",lib="font-awesome"),
      color = "blue"
    )
  })

  output$info_afil_emp2 <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(input$xcategoria,format="s"),
      subtitle = "Categoria",
      icon = icon("users",lib="font-awesome"),
      color = "blue"
    )
  })

  output$info_personas_prin <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(length(data_f1$id_persona),digits = 0, format = "d", big.mark=","),
      subtitle = "Afiliados",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$info_empresas_fil <- renderValueBox({
    data_f1<-data_f1()
    valueBox(
      value = formatC(length(unique(data_f1$id_empresa)),digits = 0, format = "d", big.mark=","),
      subtitle = "Empresas",
      icon = icon("home"),
      color = "blue"
    )
  })

  output$plot_pira_glob <- renderPlotly({
    aux1 <- data_f1() %>%
      dplyr::filter(!is.na(Edad)) %>%
      mutate(edad_agru =  cut(Edad, breaks = c(0,10,20,30,40,50,60,80,90,100,110,120,130))) %>%
      group_by(edad_agru, Genero) %>%
      summarise(clientes=n_distinct(id_persona)) %>%
      mutate(con_clientes = ifelse(test = Genero == "M",yes = -clientes, no = clientes)) %>%
      dplyr::filter(!is.na(edad_agru))

    m <- list(l = 0,r = 0,b = 50,t = 50, pad = 0)
    f1 <- list(family = "Arial, sans-serif",size = 14,color = "darkgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "darkgrey")

    aux1 %>%
      plot_ly(x= ~con_clientes, y=~edad_agru,color=~Genero) %>%
      add_bars(orientation = 'h', hoverinfo = 'text', text = ~comma(clientes), textposition = "outside", textfont = list(color = 'darkgrey', size = 10)) %>%
      layout(margin = m,
             bargap = 0.1,
             barmode = 'overlay',
             title = 'Piramide Poblacional',
             xaxis = list(title='Afiliados', titlefont = f1, tickfont = f2, zeroline = FALSE, showline = TRUE, showgrid = FALSE, showticklabels = FALSE,
                          range = c(-round(max(abs(aux1$con_clientes))*1.4, digits = 0), round(max(abs(aux1$con_clientes))*1.4, digits = 0))),
             yaxis = list(title='Edad Agru', titlefont = f1, tickfont = f2),
             font = list(color = 'darkgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>% 
      config(displayModeBar = F)

  })

  output$plot1_glob <- renderPlotly({
    data_plot <- data_f1() %>%
      dplyr::select(id_persona,segmento_poblacional) %>%
      group_by(segmento_poblacional) %>%
      summarise(Conteo = n())

    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(214,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")

    p1 <- plot_ly(data_plot, labels = ~segmento_poblacional, values = ~Conteo, type = 'pie',hole = 0,alpha = 0.9,
                  textinfo = 'text+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste('Conteo:', comma(Conteo)),
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Segmento',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             font = list(color = 'darkgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  output$plot2_glob <- renderPlotly({
    data_plot <- data_f1() %>%
      dplyr::select(id_persona,categoria) %>%
      group_by(categoria) %>%
      summarise(Conteo = n())
    
    m <- list(l = 0,r = 0,b = 100,t = 100, pad = 0)
    colors <- c('rgb(333,133,133)', 'rgb(111,103,167)', 'rgb(222,104,87)', 'rgb(200,147,203)')
    f1 <- list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
    f2 <- list(family = "Old Standard TT, serif",size = 14,color = "lightgrey")

    p1 <- plot_ly(data_plot, labels =~ categoria, values = ~Conteo, type = 'pie',hole = 0,
                  textinfo = 'text+percent',
                  insidetextfont = list(color = '#FFFFFF'),
                  hoverinfo = 'text',
                  text = ~paste('Conteo:', comma(Conteo)),
                  showlegend = T,
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 2))) %>%
      layout(margin = m,
             title = 'Participación por Categoria',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, titlefont = f1, tickfont = f2),
             legend = list(x = 0.8, y = 0.5),
             font = list(color = 'darkgrey'),
             paper_bgcolor='transparent',
             plot_bgcolor='transparent') %>%
      config(displayModeBar = F)
    p1
  })

  output$conteo_famisanar_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,filial_famisanar)
    valueBox(
      value = paste(formatC(100*sum(data_f1$filial_famisanar, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$filial_famisanar, na.rm = T)),")",sep = ""),
      subtitle = "Famisanar",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_ips_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,filial_ips_colsubsidio, filial_famisanar)
    valueBox(
      value = paste(formatC(100*sum(data_f1$filial_ips_colsubsidio, na.rm = T)/sum(data_f1$filial_famisanar, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$filial_ips_colsubsidio, na.rm = T)),")",sep = ""),
      subtitle = "IPS Colsubsidio",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_famisanar_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,filial_pac_famisanar,filial_famisanar)
    valueBox(
      value = paste(formatC(100*sum(data_f1$filial_pac_famisanar)/sum(data_f1$filial_famisanar),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$filial_pac_famisanar)),")",sep = ""),
      subtitle = "PAC Famisanar",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_sura_eps_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,filial_suramericana)
    valueBox(
      value = paste(formatC(100*sum(data_f1$filial_suramericana, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$filial_suramericana, na.rm = T)),")",sep = ""),
      subtitle = "Suramericana",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$conteo_pac_sura_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,filial_pac_suramericana,filial_suramericana)
    valueBox(
      value = paste(formatC(100*sum(data_f1$filial_pac_suramericana, na.rm = T)/sum(data_f1$filial_suramericana, na.rm = T),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$filial_pac_suramericana, na.rm = T)),")",sep = ""),
      subtitle = "Pac Suramericana",
      icon = icon("briefcase"),
      color = "purple"
    )
  })

  output$pro_salario_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Salario)
    valueBox(
      value = paste("$ ",formatC(mean(data_f1$Salario, na.rm = T),digits = 0, format = "d", big.mark=","), sep = " "),
      subtitle = "Promedio Salario",
      icon = icon("credit-card"),
      color = "olive"
    )
  })

  output$pro_edad_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Edad)
    valueBox(
      value = formatC(mean(data_f1$Edad, na.rm = T),digits = 0, format = "d", big.mark=","),
      subtitle = "Promedio Edad",
      icon = icon("user"),
      color = "olive"
    )
  })

  output$conteo_vivienda_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Compra_vivienda)
    valueBox(
      value = formatC(sum(data_f1$Compra_vivienda, na.rm = T), digits = 0, format = "f", big.mark=","),
      subtitle = "Proyecto de Vivienda (Total)",
      icon = icon("home"),
      color = "olive"
    )
  })

  output$conteo_educacion_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Educacion)
    valueBox(
      value = formatC(sum(data_f1$Educacion, na.rm = T),digits = 0, format = "f", big.mark=","),
      subtitle = "Educacion (Niños en colegios -Total)",
      icon = icon("graduation-cap"),
      color = "olive"
    )
  })

  output$conteo_consumo_credito_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Consumo_credito)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Consumo_credito, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Consumo_credito, na.rm = T)),")",sep = ""),
      subtitle = "Crédito Consumo (Total)",
      icon = icon("credit-card"),
      color = "teal"
    )
  })

  output$conteo_cupo_credito_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Cupo_credito)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Cupo_credito == "ACTIVO", na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Cupo_credito == "ACTIVO",na.rm = T)),")",sep = ""),
      subtitle = "Crédito Cupo",
      icon = icon("credit-card"),
      color = "teal"
    )
  })

  output$conteo_uso_mes_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,uso_mes)
    valueBox(
      value = paste(formatC(100*sum(data_f1$uso_mes, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$uso_mes, na.rm = T)),")",sep = ""),
      subtitle = "Uso Mes",
      icon = icon("ok", lib = "glyphicon"),
      color = "teal"
    )
  })

  # output$conteo_ryt_glob <- renderValueBox({
  #   data_f1<-data_f1()
  #   valueBox(
  #     value = paste(formatC(100*sum(data_f1$RyT)/sum(data_f1$Afiliados),
  #                           digits = 1, format = "f", big.mark=","),"%", " (",sum(data_f1$RyT),")",sep = ""),
  #     subtitle = "Recreacion y Turismo",
  #     icon = icon("rocket"),
  #     color = "teal"
  #   )
  # })
  
  output$conteo_salud_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Salud)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Salud, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Salud, na.rm = T)),")",sep = ""),
      subtitle = "Salud",
      icon = icon("briefcase"),
      color = "teal"
    )
  })

  output$conteo_supermercado_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Supermercados)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Supermercados,na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Supermercados, na.rm = T)),")",sep = ""),
      subtitle = "Supermercado",
      icon = icon("cart-plus"),
      color = "teal"
    )
  })

  output$conteo_drogueria_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Medicamentos) 
    valueBox(
      value = paste(formatC(100*sum(data_f1$Medicamentos, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Medicamentos, na.rm = T)),")",sep = ""),
      subtitle = "Droguerias",
      icon = icon("plus"),
      color = "teal"
    )
  })

  output$conteo_kit_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,kit_redimido,kit_derecho)
    valueBox(
      value = paste(formatC(100*sum(data_f1$kit_redimido, na.rm = T)/sum(data_f1$kit_derecho, na.rm = T),digits = 1, format = "f", big.mark=","),
                    "%", " (",comma(sum(data_f1$kit_redimido, na.rm = T)),"/",comma(sum(data_f1$kit_derecho, na.rm = T)),")",sep = ""),
      subtitle = "Kit Escolar",
      icon = icon("book"),
      color = "maroon"
    )
  })

  output$conteo_monetarias_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,cuota_derecho,cuota_redimida)
    valueBox(
      value = paste(formatC(100*sum(data_f1$cuota_redimida)/sum(data_f1$cuota_derecho),digits = 1, format = "f", big.mark=","),
                    "%", " (",comma(sum(data_f1$cuota_redimida,na.rm=T)),"/",comma(sum(data_f1$cuota_derecho,na.rm=T)),")",sep = ""),
      subtitle = "Cuotas Monetarias",
      icon = icon("play"),
      color = "maroon"
    )
  })

  output$conteo_lonchera_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Bono_redimido,Bono_derecho)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Bono_redimido, na.rm = T)/sum(data_f1$Bono_derecho, na.rm = T),digits = 1, format = "f", big.mark=","),
                    "%", " (",comma(sum(data_f1$Bono_redimido, na.rm = T)),"/",comma(sum(data_f1$Bono_derecho, na.rm = T)),")",sep = ""),
      subtitle = "Bono Lonchera",
      icon = icon("star"),
      color = "maroon"
    )
  })
  
  output$conteo_data_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Habeas_data)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Habeas_data, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Habeas_data, na.rm = T)),")",sep = ""),
      subtitle = "Habeas Data",
      icon = icon("database"),
      color = "purple"
    )
  })

  # RyT
  output$conteo_ryt_club_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Club)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Club, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Club, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Club",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_hoteles_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Hotel)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Hotel, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Hotel, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Hoteles",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_piscilago_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Piscilago)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Piscilago, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Piscilago, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Piscilago",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_ryt_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,RyT)
    valueBox(
      value = paste(formatC(100*sum(data_f1$RyT, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$RyT, na.rm = T)),")",sep = ""),
      subtitle = "Recreacion y Turismo - Global",
      icon = icon("rocket"),
      color = "teal"
    )
  })

  output$conteo_subsidio_vivienda_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,Subsidio_asignado)
    valueBox(
      value = paste(formatC(100*sum(data_f1$Subsidio_asignado, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$Subsidio_asignado, na.rm = T)),")",sep = ""),
      subtitle = "Subsidio Vivienda (Total)",
      icon = icon("hotel"),
      color = "purple"
    )
  })

  output$conteo_hoteles_glob <- renderValueBox({
      data_f1<-data_f1() %>% 
        select(id_persona,pros_hotel)
      valueBox(
        value = paste(formatC(100*sum(data_f1$pros_hotel, na.rm = T)/length(data_f1$id_persona),
                              digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pros_hotel, na.rm = T)),")",sep = ""),
        subtitle = "Prospecto Hoteles",
        icon = icon("bicycle",lib="font-awesome"),
        color = "purple"
      )
    })

  output$conteo_piscilago_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,pros_pisi)
    valueBox(
      value = paste(formatC(100*sum(data_f1$pros_pisi, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pros_pisi, na.rm = T)),")",sep = ""),
      subtitle = "Prospecto Piscilago",
      icon = icon("bicycle",lib="font-awesome"),
      color = "purple"
    )
  })

  output$conteo_club_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,pros_club)
    valueBox(
      value = paste(formatC(100*sum(data_f1$pros_club, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$pros_club, na.rm = T)),")",sep = ""),
      subtitle = "Prospecto Clubes",
      icon = icon("bicycle",lib="font-awesome"),
      color = "purple"
    )
  })

  output$pre_aprobado_hipotecario_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,preaprobado_hipo)
    valueBox(
      value = paste(formatC(100*sum(data_f1$preaprobado_hipo, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$preaprobado_hipo, na.rm = T)),")",sep = ""),
      subtitle = "Pre Aprobado Hipotecario",
      icon = icon("eraser"),
      color = "olive"
    )
  })

  output$pre_aprobado_cupo_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,preaprobado_cupo)
    valueBox(
      value = paste(formatC(100*sum(data_f1$preaprobado_cupo, na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$preaprobado_cupo, na.rm = T)),")",sep = ""),
      subtitle = "Pre Aprobado Cupo",
      icon = icon("eraser"),
      color = "olive"
    )
  })
  
  output$conteo_cuad_a_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,CuadranteViv2)
    valueBox(
      value = paste(formatC(100*sum(data_f1$CuadranteViv2 == "A3", na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$CuadranteViv == "A", na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante A: En caso de tener Crédito hipotecario sin desembolsar,  sin tener otorgado necesariamente subsidio de vivienda",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_cuad_a1_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,CuadranteViv2)
    valueBox(
      value = paste(formatC(100*sum(data_f1$CuadranteViv2 == "A1", na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$CuadranteViv == "A1", na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante A1: Con subsidio asignado con vigencia  menor a 6 meses",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_cuad_a2_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,CuadranteViv2)
    valueBox(
      value = paste(formatC(100*sum(data_f1$CuadranteViv2 == "A2", na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$CuadranteViv == "A2", na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante A2: Con subsidio asignado con vigencia  mayor de  6 meses",
      icon = icon("home"),
      color = "teal"
    )
  })

  output$conteo_cuad_b_glob <- renderValueBox({
    data_f1<-data_f1() %>% 
      select(id_persona,CuadranteViv2)
    valueBox(
      value = paste(formatC(100*sum(data_f1$CuadranteViv2 == "B1", na.rm = T)/length(data_f1$id_persona),
                            digits = 1, format = "f", big.mark=","),"%", " (",comma(sum(data_f1$CuadranteViv == "B1", na.rm = T)),")",sep = ""),
      subtitle = "Cuadrante B: Afiliados con interés de compra (Basado en encuestas y diagnóstico de necesidades)",
      icon = icon("home"),
      color = "teal"
    )
  })
})



