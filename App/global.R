# Cargamos librerias
# options(scipen = 999)
rm(list = ls())
library(shiny); library(ggplot2); library(dplyr); library(plotly)
library(shinydashboard);library(DT); library(shinyjs); library(reshape)
library(data.table); library(tidyr); library(stringr); library(shinydashboardPlus)
library(Hmisc); library(shinythemes); library(dashboardthemes)
library(scales); library(shinycustomloader)

bd_afiliados <- readRDS("Data/bd_afiliados12_23062020.rds") %>% 
  mutate(Genero = toupper(Genero)) %>% 
  mutate(CuadranteViv2 = as.character(ifelse(is.na(CuadranteViv), NA, as.character(CuadranteViv)))) %>% 
  data.frame()
str(bd_afiliados)
sapply(bd_afiliados, function(x) sum(is.na(x)))

name_segmento <- c("Total","Alto","Joven","Medio","Básico")
name_categoria <- c("Total","A","B","C")
sum(bd_afiliados$RyT)

