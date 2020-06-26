# Bases de datos requeridas
# Consulta seg Individual
# Bono lonchera
# Kit Escolar 
# Preaprobados
rm(list = ls())
library(RODBC); library(data.table); library(dplyr); library(tidyr); library(lubridate)
mes_actual <- as.Date("2019-05-01")
### Datos ====

conn_consulta_seg_ind <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/APP_Segmento_Ind/Data/Originales/Consulta_BD_SegInd.accdb")
subset(sqlTables(conn_consulta_seg_ind), tableType = "SYSTEM TABLE")
consulta_seg_ind <- sqlFetch(conn_consulta_seg_ind, "Consulta_Seg_Ind")
str(consulta_seg_ind)
df_pros_vivi <- sqlFetch(conn_consulta_seg_ind, "Cuadrantes_Vivienda")
str(df_pros_vivi)
odbcClose(conn_consulta_seg_ind)

consulta_seg_ind <- consulta_seg_ind %>% 
  mutate(id_empresa = as.character(id_empresa),
         id_persona = as.character(id_persona))
str(consulta_seg_ind)

# info_prospectos vivienda
df_pros_vivi <- df_pros_vivi %>% 
  mutate(id_persona = as.character(id_persona))
str(df_pros_vivi)

### Bono Lonchera ====
conn_bono <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/BonoLonchera.accdb")
subset(sqlTables(conn_bono), TABLE_TYPE == "TABLE") 
bono <- sqlFetch(conn_bono, "BonoLochera") 
str(bono)
odbcClose(conn_bono)

df_bono <- bono %>% 
  mutate(fecha = as.Date.character(paste(AÑO_REDENCION, MES, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha >= mes_actual & fecha <= mes_actual + 365) %>% 
  select(id_persona,REDIMIO) %>% 
  mutate(id_persona = as.character(id_persona),
         Bono_derecho = 1,
         Bono_redimido = ifelse(is.na(REDIMIO), 0, 1)) %>% 
  select(-REDIMIO) %>% 
  group_by(id_persona) %>% 
  summarise(Bono_derecho = ifelse(sum(Bono_derecho)>=1,1,0),
            Bono_redimido = ifelse(sum(Bono_redimido)>=1,1,0))
str(df_bono)
table(duplicated(df_bono$id_persona))
sum(df_bono$Bono_redimido)
rm(bono)

### Kit Escolar ====
conn_kit_2020 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//Bogak08beimrodc/bi/Proteccion_Social/Kit_Escolar_2020.accdb")
subset(sqlTables(conn_kit_2020), TABLE_TYPE == "TABLE") 
kit_2020 <- sqlFetch(conn_kit_2020, "base_kit_escolar_2020") 
str(kit_2020)
odbcClose(conn_kit_2020)
table(kit_2020$nuevo_estado_kit)

df_kit <- kit_2020 %>% 
  select(id_persona,nuevo_estado_kit) %>% 
  group_by(id_persona,nuevo_estado_kit) %>% 
  summarise(c_kit = n()) %>% 
  spread(nuevo_estado_kit,c_kit, fill = 0) %>% 
  data.frame() %>% 
  mutate(n_kit_derecho = ACTIVO + ANULADO + REDIMIDO) %>% 
  dplyr::rename(kit_redimido=REDIMIDO) %>% 
  mutate(id_persona = as.character(id_persona),
         kit_redimido = ifelse(kit_redimido == 0, 0, 1),
         kit_derecho = ifelse(n_kit_derecho == 0, 0, 1)) %>% 
  select(id_persona,kit_derecho,kit_redimido,n_kit_derecho)
str(df_kit)
rm(kit_2020)

### Preaprobados ====
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Consumo/Credito/Base_Preaprobados.accdb")
sqlTables(channel)

tb_producto <- sqlQuery( 
  channel , 
  paste ("select * from Tb_Productos_Preaprobados"),
  as.is=T) %>% 
  data.frame() %>% 
  mutate(Codigo_Subproducto = as.character(Codigo_Subproducto))
str(tb_producto)

df_preaprobados <- sqlQuery( 
  channel , 
  paste ("select * from Preaprobados"),
  as.is=T) %>% 
  data.frame() %>% 
  select(nid_persona = NUMERO.DE.IDENTIFICACION, subproducto = SUBPRODUCTO, fecha_vigencia = FECHA.DE.VIGENCIA) %>% 
  mutate(subproducto = as.character(subproducto),
         fecha_vigencia = gsub(" 00:00:00", "", fecha_vigencia, fixed = T),
         marca = 1) %>% 
  mutate(fecha_vigencia = as.Date.character(fecha_vigencia, format = "%Y-%m-%d")) %>% 
  filter(fecha_vigencia >= mes_actual + 335) %>% 
  left_join(tb_producto, by = c("subproducto"="Codigo_Subproducto")) %>% 
  filter(Subproducto %in% c("CUPO","HIPOTECARIO")) %>% 
  mutate(id_persona = paste0("CC",nid_persona)) %>% 
  select(id_persona,Subproducto,marca) %>% 
  group_by(id_persona,Subproducto) %>% 
  summarise(marca = sum(marca, na.rm = T)) %>% 
  ungroup() %>% 
  spread(Subproducto, marca, fill = 0) %>% 
  mutate(preaprobado_cupo = ifelse(CUPO >=1, 1, 0),
         preaprobado_hipo = ifelse(HIPOTECARIO >=1, 1, 0)) %>% 
  select(id_persona,preaprobado_cupo,preaprobado_hipo) %>% 
  data.frame()
str(df_preaprobados)

odbcCloseAll()

### cuota derecho ====
con_giro <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Proteccion_Social/GiroCuotaMonetaria2018.accdb")
sqlTables(con_giro)

df_derecho_cm <- sqlQuery(
  con_giro , 
  paste ("select * from giro_cuotamonetaria"),
  as.is=T) %>% 
  data.frame() %>% 
  mutate(año = as.numeric(año),
         mes = as.numeric(mes),
         fecha = as.Date.character(paste(año, mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha >= mes_actual) %>% 
  select(id_persona) %>% 
  distinct() %>% 
  mutate(cuota_derecho = 1)
str(df_derecho_cm)

odbcCloseAll()

### cuota redimida ====
# Persona tarjeta multiservicios
con_tms <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Persona/Persona_Tarjeta_Multiservicios.accdb")
sqlTables(con_tms)

df_persona_tms <- sqlQuery(
  con_tms, 
  paste ("select id_persona, numero_tarjeta_multiservicios from afiliado_tarjeta_multiservicios"),
  as.is=T) %>% 
  data.frame()
str(df_persona_tms)

odbcCloseAll()

con_redencion20191 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Consumo/Convenios/ConsumoConvenios2019_1.accdb")
sqlTables(con_redencion20191)

redencion_tms20191 <- sqlQuery(
  con_redencion20191, 
  paste ("select * from ConsumoConvenios"),
  as.is=T) %>% 
  data.frame() %>% 
  select(TARJETA, FECHA.TX, BOLSILLO, COD.RTA, TIPO.DE.MSJ, VR.TX) %>% 
  mutate(FECHA.TX = as.Date.character(paste(substr(FECHA.TX, 1, 4), substr(FECHA.TX, 5, 6), substr(FECHA.TX, 7, 8), sep = "-"), 
                                      format = "%Y-%m-%d")) %>% 
  filter(FECHA.TX >= mes_actual) %>% 
  filter(BOLSILLO == "1") %>% 
  filter(COD.RTA == "0") %>% 
  filter(TIPO.DE.MSJ == "200") %>% 
  dplyr::select(TARJETA, VR.TX) %>% 
  group_by(TARJETA) %>% 
  summarise(VR.TX = sum(VR.TX, na.rm = T),
            n_cm = n()) %>% 
  data.frame()
str(redencion_tms20191)
odbcCloseAll()


con_redencion20192 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//bogak08beimrodc/bi/Consumo/Convenios/ConsumoConvenios2019_2.accdb")
sqlTables(con_redencion20192)

redencion_tms20192 <- sqlQuery(
  con_redencion20192, 
  paste ("select * from ConsumoConvenios"),
  as.is=T) %>% 
  data.frame() %>% 
  select(TARJETA, FECHA.TX, BOLSILLO, COD.RTA, TIPO.DE.MSJ, VR.TX) %>% 
  mutate(FECHA.TX = as.Date.character(paste(substr(FECHA.TX, 1, 4), substr(FECHA.TX, 5, 6), substr(FECHA.TX, 7, 8), sep = "-"), 
                                      format = "%Y-%m-%d")) %>% 
  filter(FECHA.TX >= mes_actual) %>% 
  filter(BOLSILLO == "1") %>% 
  filter(COD.RTA == "0") %>% 
  filter(TIPO.DE.MSJ == "200") %>% 
  dplyr::select(TARJETA, VR.TX) %>% 
  group_by(TARJETA) %>% 
  summarise(VR.TX = sum(VR.TX, na.rm = T),
            n_cm = n()) %>% 
  data.frame()
str(redencion_tms20192)
odbcCloseAll()

redencion_tms2019 <- bind_rows(redencion_tms20191,redencion_tms20192) %>% 
  group_by(TARJETA) %>% 
  summarise(valor_cm = sum(VR.TX, na.rm = T),
            n_cm = sum(n_cm, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(df_persona_tms, by = c("TARJETA"="numero_tarjeta_multiservicios")) %>%
  mutate(cuota_redimida = 1) %>% 
  select(id_persona, valor_cm, cuota_redimida) %>% 
  data.frame()
str(redencion_tms2019)
rm(redencion_tms20191, redencion_tms20192)


# Para 2020
ruta <- "//Bogak08beimrodc/bi/Consumo/Convenios/"
lista <- list.files(path = ruta, pattern = "accdb") %>% 
  data.frame() %>% 
  filter(grepl(., pattern = "ConsumoConvenios_")) %>% 
  as.matrix() %>% as.vector()

redencion_tms2020 <- data.frame()
for (i in 1:length(lista)) {
  driver = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",ruta,lista[i])
  con_redencion <- odbcDriverConnect(driver)
  sqlTables(con_redencion)
  aux <- sqlQuery(
    con_redencion, 
    paste ("select * from Consolidado_mes"),
    as.is=T) %>% 
    data.frame() %>% 
    filter(codigo_bolsillo == 1) %>% 
    filter(codigo_respuesta == "0") %>% 
    filter(codigo_tipo_mensaje == 200) %>% 
    select(tarjeta, valor_transaccion) %>% 
    data.frame()
  odbcCloseAll()
  redencion_tms2020 <- bind_rows(redencion_tms2020,aux)
}
str(redencion_tms2020)
str(aux)

redencion_tms2020 <- redencion_tms2020 %>% 
  left_join(df_persona_tms, by = c("tarjeta"="numero_tarjeta_multiservicios")) %>%
  group_by(id_persona) %>% 
  summarise(valor_cm = sum(valor_transaccion, na.rm = T),
            n_cm = n()) %>% 
  ungroup() %>% 
  mutate(cuota_redimida = 1) %>% 
  select(id_persona, valor_cm, cuota_redimida)


str(redencion_tms2020)
df_redimida_cm <- bind_rows(redencion_tms2019, redencion_tms2020) %>% 
  group_by(id_persona) %>% 
  summarise(valor_cm = sum(valor_cm, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(cuota_redimida = 1)
str(df_redimida_cm)
rm(aux, df_persona_tms, redencion_tms2019, redencion_tms2020)

df_cuotas <- df_derecho_cm %>% 
  left_join(df_redimida_cm, by = c("id_persona"))
str(df_cuotas)
rm(df_derecho_cm,df_redimida_cm)

### Prospectos ====
prospectos <- readRDS("Data/Originales/Abril2020/bd_ryt_prospectos_27052020.rds") %>% 
  select(id_persona,Frec.pronos.hoteles,Frec.pronos.club,Frec.pronos.pisi) %>% 
  mutate(pros_hotel = ifelse(Frec.pronos.hoteles >= 11, 1, 0),
         pros_club = ifelse(Frec.pronos.club >= 3.5, 1, 0),
         pros_pisi = ifelse(Frec.pronos.pisi >= 1.12, 1, 0)) %>% 
  select(-c(Frec.pronos.hoteles,Frec.pronos.club,Frec.pronos.pisi)) %>%
  distinct()
table(prospectos$pros_hotel)
table(prospectos$pros_club)
table(prospectos$pros_pisi)
str(prospectos)

#### CONSUMO INDIVIDUAL ====================
# \\Bogak08beimrodc\bi\Analitica\ConsumosRDS\3 - Marzo
consumo_individual <- readRDS("//Bogak08beimrodc/bi/Analitica/ConsumosRDS/5 - Mayo/consumo_individual_Mayo2020.rds") %>% 
  data.frame() %>% 
  mutate(segmento_poblacional = iconv(segmento_poblacional,to="ASCII//TRANSLIT"),
         servicio = iconv(servicio,to="ASCII//TRANSLIT"),
         fecha = as.Date.character(paste(anno,mes,"01",sep="-"), format = "%Y-%m-%d")) %>% 
  filter(fecha >= mes_actual & fecha <= mes_actual + 365)
str(consumo_individual)
table(consumo_individual$segmento_poblacional)
table(consumo_individual$anio_mes)

consumo_individual_ryt <- consumo_individual %>%
  filter(ues == "RyT")
str(consumo_individual_ryt)
table(consumo_individual$servicio)

# CC1019078197  
consumo_ryt_ind1 <- consumo_individual_ryt %>% 
  select(id_empresa,id_persona,servicio) %>% 
  group_by(id_persona, servicio) %>% 
  summarise(n_id_persona = n()) %>% 
  spread(key = servicio, value = n_id_persona, fill = 0) %>% 
  mutate(Club = `Club Bellavista` + `Club Calle 195` + `Club El Cubo` + `Club La Colina`,
         Hotel = `Hotel Alcaravan` + `Hotel Bosques De Athan` + `Hotel Penalisa` + `Hotel Colonial` + `Hotel Lanceros` + `Hoteles Paipa`
  ) %>% 
  select(-c(`Club Bellavista`,`Club Calle 195`,`Club El Cubo`,`Club La Colina`,
            `Hotel Alcaravan`,`Hotel Bosques De Athan`,`Hotel Penalisa`, `Hotel Colonial`, `Hotel Lanceros`, `Hoteles Paipa`,
            `Programas Deportivos`,`Programas Recreativos`)) %>% 
  mutate(Piscilago = ifelse(Piscilago >= 1, 1, 0),
         Club = ifelse(Club >= 1, 1, 0), 
         Hotel = ifelse(Hotel >= 1, 1, 0)) %>% 
  data.frame()
str(consumo_ryt_ind1)

consumo_ryt_ind2 <- consumo_individual_ryt %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona, ues) %>% 
  summarise(RyT = n()) %>% 
  select(-ues) %>% 
  mutate(RyT = ifelse(RyT >= 1, 1, 0)) %>% 
  data.frame()
str(consumo_ryt_ind2)

consumo_ryt_ind <- left_join(consumo_ryt_ind1,consumo_ryt_ind2)
str(consumo_ryt_ind)
rm(consumo_ryt_ind1,consumo_ryt_ind2)

estados_tarjeta <-fread("Data/Originales/Cobranzas/EstadosTarjeta.csv") %>% 
  data.frame()
str(estados_tarjeta)

cupo_credito <- fread("Data/Originales/Cobranzas/cobranzas_mayo_2020.csv", encoding = 'UTF-8') %>% 
  data.frame() %>% 
  select(1,2,31,Desc.Bloqueo) %>% 
  filter(Tipo.Identificación %in% c(2,3,4)) %>% 
  mutate(tipo_id = ifelse(Tipo.Identificación == 2, "CC",
                          ifelse(Tipo.Identificación == 3, "CE", "TI"))) %>% 
  mutate(id_persona = paste0(tipo_id,Nro.Identificación)) %>% 
  left_join(estados_tarjeta, by = c("Estado.Tarjeta"="estadotarjeta")) %>% 
  select(c(id_persona,ESTADOCUPO)) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  data.frame() %>% 
  dplyr::rename(Cupo_credito=ESTADOCUPO)
str(cupo_credito)
table(duplicated(cupo_credito$id_persona))

consumo_credito <- consumo_individual %>% 
  filter(servicio %in% c("Libranza","No Libranza")) %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(Consumo_credito = 1)
str(consumo_credito)

# educacion <- consumo_individual %>% 
#   filter(ues == "Educacion") %>% 
#   select(id_empresa,id_persona,ues) %>% 
#   group_by(id_persona) %>% 
#   summarise(Educacion = 1)
# str(educacion)

conn_educacion <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=D:/A COLSUBSIDIO/APP_Segmento_Ind/Data/Originales/Consumo_Educacion.accdb")
subset(sqlTables(conn_educacion), tableType = "SYSTEM TABLE")
consumo_educacion <- sqlFetch(conn_educacion, "Consulta_educacion")
str(consumo_educacion)
odbcClose(conn_educacion)

educacion <- consumo_educacion %>% 
  mutate_all(as.character) %>% 
  select(id_persona = noDocumentoRes, id_alumno = noDocumentoAlu, mes, año) %>% 
  mutate(id_persona = paste0("CC",id_persona),
         fecha = as.Date.character(paste(año, mes, "01", sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha >= max(fecha, na.rm = T)) %>% 
  group_by(id_persona) %>% 
  summarise(Educacion = n_distinct(id_alumno))
rm(consumo_educacion)

medicamentos <- consumo_individual %>% 
  filter(servicio == "Medicamentos") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(Medicamentos = 1)
str(medicamentos)

supermercados <- consumo_individual %>% 
  filter(servicio == "Supermercados") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(Supermercados = 1)
str(supermercados)

salud <- consumo_individual %>% 
  filter(ues == "Salud") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(Salud = 1)
str(salud)

compra_vivienda <- consumo_individual %>% 
  filter(servicio == "Fecha Entrega") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(Compra_vivienda = 1)
str(compra_vivienda)

subsidio_asignado <- consumo_individual %>% 
  filter(servicio == "Subsidio Asignado") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(Subsidio_asignado = 1)
str(subsidio_asignado)

subsidio_entregado <- consumo_individual %>% 
  filter(servicio == "Subsidio Entregado") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(subsidio_entregado = 1)
str(subsidio_entregado)

uso_tms <- consumo_individual %>% 
  filter(servicio == "Credito convenios") %>% 
  select(id_empresa,id_persona,ues) %>% 
  group_by(id_persona) %>% 
  summarise(uso_mes = 1)
str(subsidio_entregado)

# Union de tablas
str(consulta_seg_ind)
str(df_preaprobados)
bd_afiliados <- consulta_seg_ind %>% 
  # filter(marca_afiliado_unico == "X") %>% 
  left_join(df_bono, by = "id_persona") %>% 
  left_join(df_kit, by = "id_persona") %>% 
  left_join(df_cuotas, by = "id_persona") %>% 
  left_join(df_preaprobados, by = "id_persona") %>% 
  left_join(consumo_ryt_ind, by = "id_persona") %>% 
  left_join(cupo_credito, by = "id_persona") %>% 
  left_join(consumo_credito, by = "id_persona") %>% 
  left_join(educacion, by = "id_persona") %>% 
  left_join(medicamentos, by = "id_persona") %>% 
  left_join(supermercados, by = "id_persona") %>% 
  left_join(salud, by = "id_persona") %>% 
  left_join(compra_vivienda, by = "id_persona") %>% 
  left_join(subsidio_asignado, by = "id_persona") %>% 
  left_join(subsidio_entregado, by = "id_persona") %>% 
  left_join(uso_tms, by = "id_persona") %>%
  left_join(prospectos, by = "id_persona") %>% 
  left_join(df_pros_vivi, by = "id_persona") %>% 
  mutate(Habeas_data = ifelse(toupper(autorizacion)== "SI", 1, 0)) %>% 
  data.frame()
str(bd_afiliados)

bd_afiliados[,c(34:47,49:60,62)][is.na(bd_afiliados[,c(34:47,49:60,62)])] <- 0


# library(purrr)
saveRDS(bd_afiliados, file = "App/Data/bd_afiliados12_23062020.rds")
saveRDS(bd_afiliados, file = "Data/Originales/Mayo2020/bd_afiliados12_23062020.rds")
str(bd_afiliados)
table(duplicated(bd_afiliados$id_persona))
sum(bd_afiliados$pre_aprobado_cupo)

