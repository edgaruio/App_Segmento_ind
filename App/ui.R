
dashboardPage(skin = "blue",
              dashboardHeader(title = "Segmento Individual"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE,br(),
                            tags$img(src = "Logo.png", height=40, width=200, align="center"),
                            tags$hr(),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            tags$hr(),
                            menuItem("GLOBAL", tabName = "global", icon = icon("area-chart"),
                                     menuSubItem("Check",tabName = "global",icon = icon("check-circle")),
                                     selectInput("xsegmento", label = "Segmento Poblacional:",choices = name_segmento,
                                                 selected = "Total", multiple = F),
                                     selectInput("xcategoria", label = "Categoria:",choices = name_categoria,
                                                 selected = "Total", multiple = F),
                                     actionButton("go", label = "Go")),
                            tags$hr()
                            )),
              dashboardBody(
                shinyDashboardThemes(
                  theme = "purple_gradient"
                ),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Aplicacion Segmento Individual"),
                                   br(),br(),
                                   h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta de información para afiliados"),
                                   br(),
                                   h3("La siguiente pestaña muestra informacion global por Categoria y Segmento."),
                                   br(),
                                   h3("Nota: Al cambiar la pestaña debe dar click en 'Check' para cambiar el panel principal,
                                      luego hacer click en 'Go' para realizar la consulta."),
                                   br(),
                                   h4("Fecha de actualización: 25/06/2020")
                            ),
                            column(1)
                          )
                  ),
                  tabItem(tabName = "global",
                          fluidPage(
                            title = "DB",
                            fluidRow(
                              box(title = "Consolidado Información (Corte Mayo)",width=12,status="primary",solidHeader=TRUE,collapsible=FALSE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("info_afil_emp",width = 3),
                                    valueBoxOutput("info_afil_emp2",width = 3),
                                    valueBoxOutput("info_personas_prin",width = 3),
                                    valueBoxOutput("info_empresas_fil",width = 3)
                                  ),
                                  fluidRow(
                                    column(width = 4,
                                           withLoader(plotlyOutput("plot_pira_glob", height = 500), type = "html", loader = "loader5")),
                                    column(width = 4,
                                           withLoader(plotlyOutput("plot1_glob", height = 500), type = "html", loader = "loader5")),
                                    column(width = 4,
                                           withLoader(plotlyOutput("plot2_glob", height = 500), type = "html", loader = "loader5"))
                                    )
                                  )
                            ),
                            fluidRow(
                              box(title = "Gestion de Afiliados Cobertura",width=12,status="primary",solidHeader=TRUE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_famisanar_glob",width = 3),
                                    valueBoxOutput("conteo_pac_famisanar_glob",width = 2),
                                    valueBoxOutput("conteo_sura_eps_glob",width = 3),
                                    valueBoxOutput("conteo_pac_sura_glob",width = 2),
                                    valueBoxOutput("conteo_ips_glob",width = 2)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("pro_salario_glob",width = 3),
                                    valueBoxOutput("pro_edad_glob",width = 3),
                                    valueBoxOutput("conteo_vivienda_glob",width = 3),
                                    valueBoxOutput("conteo_educacion_glob",width = 3)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_consumo_credito_glob",width = 2),
                                    valueBoxOutput("conteo_cupo_credito_glob",width = 2),
                                    valueBoxOutput("conteo_uso_mes_glob",width = 2),
                                    valueBoxOutput("conteo_salud_glob",width = 2),
                                    valueBoxOutput("conteo_supermercado_glob",width = 2),
                                    valueBoxOutput("conteo_drogueria_glob",width = 2)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_ryt_hoteles_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_club_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_piscilago_glob",width = 3),
                                    valueBoxOutput("conteo_ryt_glob",width = 3)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Gestion de Derechos",width=12,status="primary",solidHeader=TRUE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_kit_glob",width = 4),
                                    valueBoxOutput("conteo_monetarias_glob",width = 4),
                                    valueBoxOutput("conteo_lonchera_glob",width = 4)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_data_glob",width = 6),
                                    # valueBoxOutput("conteo_subsidio_glob",width = 4),
                                    valueBoxOutput("conteo_subsidio_vivienda_glob",width = 6)
                                  )
                              )
                            ),
                            fluidRow(
                              box(title = "Prospectos (Total)",width=12,status="primary",solidHeader=TRUE,collapsible=TRUE,collapsed = TRUE,
                                  fluidRow(
                                    valueBoxOutput("conteo_hoteles_glob",width = 4),
                                    valueBoxOutput("conteo_piscilago_glob",width = 4),
                                    valueBoxOutput("conteo_club_glob",width = 4)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("pre_aprobado_hipotecario_glob",width = 6),
                                    valueBoxOutput("pre_aprobado_cupo_glob",width = 6)
                                  ),
                                  fluidRow(
                                    valueBoxOutput("conteo_cuad_a_glob",width = 3),
                                    valueBoxOutput("conteo_cuad_a1_glob",width = 3),
                                    valueBoxOutput("conteo_cuad_a2_glob",width = 3),
                                    valueBoxOutput("conteo_cuad_b_glob",width = 3)
                                  )
                              )
                            ))
                  )
                )
              )
)

