# 1. Lectura de fitxers 

memory.size(max=160685)

#
# Directori Font     ==============================  
gc()
rm(list=ls())

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


####    DIRECTORI DE TREBALL              
#### setwd en directori de treball 


# 0. Inicialització de parametres  -----------------------------

# N test mostra a seleccionar  (Nmostra=Inf)

# Nmostra=Inf  # Seria tota la mostra
Nmostra=Inf

# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg_precav.xls"

# fitxer conductor variables
fitxer_conductor_variables<-"variables_precav.xls"


# write.xlsx(CATALEG,file="cataleg.xlsx")
CATALEG<-readxl::read_excel(fitxer_conductor_cataleg,col_types = "text")


# Funcions lectura de fitxers
LLEGIR.PACIENTS<<-function(n=Nmostra) {
  readRDS("dades/sidiap_precav" %>% here::here("ECV_CAT_entregable_pacients_20190517_101801.rds")) %>% as_tibble() %>% head(n)}

LLEGIR.PROBLEMES<<-function(n=Nmostra) {
  readRDS("dades/sidiap_precav" %>% here::here("ECV_CAT_entregable_problemes_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.CMBDH<<-function(n=Nmostra) {
  readRDS("dades/sidiap_precav" %>% here::here("ECV_CAT_entregable_cmbd_dx_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.padris<<-function(n=Nmostra) {
  readRDS("dades/sidiap_precav" %>% here::here("ECV_CAT_entregable_cmbd_dx_padris_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.PROC<<-function(n=Nmostra) {
  readRDS("dades/sidiap_precav" %>% here::here("ECV_CAT_entregable_cmbd_px_padris_20181123_172533.rds"))%>% as_tibble() %>% head(n)}

LLEGIR.TABAC<<-function(n=Nmostra) {
  readRDS("dades/sidiap_precav" %>% here::here("ECV_CAT_entregable_tabaquisme_20181123_172533.rds"))%>% as_tibble() %>% head(n) }

# Llegit pacients i problemes de salut
library(dplyr)

#  Llegir PACIENTS, I PROBLEMES DE SALUT
PACIENTS<-Inf %>% LLEGIR.PACIENTS()
PROBLEMES<-Nmostra %>% LLEGIR.PROBLEMES()

# Seleccionar tots els pacients que han estat DM durant periode

# 1. Extreure T2DM (exposed) Agrego problemes amb límit data máxima (31/12/2016) 

#### Fusiono agregador (+agr) 
dadesDM_exposats<-PROBLEMES %>% 
  agregar_problemes_agr(bd.dindex="20161231",agregador ="DM",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="DG_DM") %>%  # agrego problemes de salut
  select(idp,dtevent="DM",codindex=cod)

dades_exclos<-PROBLEMES %>% 
  agregar_problemes_agr(bd.dindex="20161231",agregador ="EXCLUDE",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="exclude") %>%  # agrego problemes de salut
  select(idp,dtevent="DM",codindex=cod)

# filtro dadesDM_exposats (Elimino dades excloses)
dadesDM_exposats<-dadesDM_exposats %>% anti_join(dades_exclos,by="idp")





