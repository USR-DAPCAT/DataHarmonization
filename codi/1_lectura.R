# 1. Lectura de fitxers 
# memory.size(max=160685)
memory.limit()

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
fitxer_conductor_cataleg<-"Spain_codes.xls"

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

# Criteris d'Inclusió 1  (Any naixament)
PACIENTS<-PACIENTS %>% 
  filter (dnaix<19720101)     # Excluye Nacidos a partir del 1972

# Seleccionar tots els pacients que han estat DM durant periode

# 1. Extreure T2DM (exposed) Agrego problemes amb límit data máxima (31/12/2016) 

#### Fusiono agregador (+agr) 
dadesDM_exposats<-PROBLEMES %>% 
  agregar_problemes_agr(bd.dindex="20161231",agregador ="exposed",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="exposed") %>%  # agrego problemes de salut
  select(idp,dtevent="exposed",codindex=cod)

dades_exclos<-PROBLEMES %>% 
  agregar_problemes_agr(bd.dindex="20161231",agregador ="exclude",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="exclude") %>%  # agrego problemes de salut
  select(idp,dtevent="exclude",codindex=cod)

# filtro dadesDM_exposats (Elimino dades excloses)
dadesDM_exposats<-dadesDM_exposats %>% anti_join(dades_exclos,by="idp")


# 2.	From this cohort (T2C), remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; this is the exposed cohort (EC).
bd_dtindex<-dadesDM_exposats %>% select(idp,dataindex=dtevent) %>% mutate(dataindex=as.Date(dataindex,origin="1970-01-01"))
historic_problemes<-PROBLEMES %>% mutate(dat=as.Date(as.character(dat),format="%Y%m%d")) %>% select(idp,cod,dat)

dades_exclos2<-historic_problemes %>% 
  agregar_problemes_agr(bd.dindex=bd_dtindex,agregador ="prevalent",dt.agregadors=CATALEG,finestra.dies=c(-Inf,0),prefix="",camp_agregador="prevalent") 

dadesDM_exposats<-dadesDM_exposats %>% anti_join(dades_exclos2,by="idp")

# 3.	From the entire database (ED), remove patients with any codes in sheet “non-exposed pool” before the end of the study (31/12/2018), to get the candidate non-exposed patients (CNE).

## Eliminar de població (PACIENTS) dades_exclos

PACIENTS<-PACIENTS %>% anti_join(dades_exclos,by="idp")


# 4.	Exact matching the exposed cohort (EC) to the candidate non-exposed patients (CNE) with a ratio 1:10 (EC:CNE) 
# by year of birth (+/-1year), sex, and practice, without replacement (each candidate non-exposed patient can be only matched once). 
# This is the matched cohort (MC), and the index date is the same as the matched exposed patient.
# Juntar casos a PACIENTS
PACIENTS<-PACIENTS %>% left_join(dadesDM_exposats,by="idp") %>% mutate(grup=ifelse(is.na(dtevent),0,1))


# Preparar matching i setriskmatching #
dt_matching<-PACIENTS %>% select(idp,idup,dnaix,sexe,grup,dtevent,sortida)

#   5.2.1 Generar data de sortida (Data event / Data de censura)     -----------------
## dtindex_case 
dt_matching<-dt_matching %>% mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament i grups cada 10 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 


#### Parametres d'aparellament
llistaPS=c("sexe","any_naix","idup")
num_controls<-5
llavor<-125
set.seed(llavor)

# 5.4.1. Aplicar algoritme   -----------
dades_match<-heaven::riskSetMatch(ptid="idp"                                # Unique patient identifier
                                  ,event="grup"                # 0=Control, 1=case
                                  ,terms=llistaPS   # terms c("n1","n2",...) - list of vairables to match by
                                  ,dat=dt_matching             # dataset with all variables
                                  ,Ncontrols=num_controls       # number of controls to provide
                                  ,oldevent="oldevent"          # To distinguish cases used as controls
                                  ,caseid="caseid"              # variable to group cases and controls (case-ptid)
                                  ,reuseCases=F                 # T og F or NULL - can a case be a control prior to being a case?
                                  ,reuseControls=F              # T or F or NULL - can controls be reused?
                                  ,caseIndex="dtindex_case"       # Integer or date, date where controls must be prior
                                  ,controlIndex="dtindex_control" # controlIndex - Index date for controls
                                  ,NoIndex=FALSE                # If T ignore index
                                  ,cores=1                      # Number of cores to use, default 1
                                  ,dateterms=NULL               # character list of date variables
)

# Report matchreport -------------
heaven::matchReport(dades_match, id="idp",case="grup",caseid="caseid")

# Número de controls per conjunt a risk  ------------
dades_match[,numControls:=.N,by=caseid]
dades_match<- dades_match %>% mutate(numControls=numControls-1)


# 
bdades_index<-dades_match %>% 
  select(idp,dataindex=dtindex_case)%>% 
  mutate (dataindex=as.Date(dataindex,origin = "1970-01-01")) %>%
  as_tibble()
dades_match<-dades_match %>% rename(dtindex=dtindex_case) %>% mutate (dtindex=as.Date(dtindex,origin = "1970-01-01"))

dades_match<-dades_match %>% as_tibble()


gc()
# Verificació de Matching aprox -----------------------
table(dades_match$numControls,dades_match$grup) 
descrTable(formula_vector(llistaPS,y="grup"),data=dades_match)
# extreure_OR("event~sexe+any_naix",dades=dades_match,conditional = T,str

# any_dg i filtrar per any
dades_match<-dades_match %>% mutate(yearindex=lubridate::year(dtindex)) %>% select(-c(idup,dnaix,sexe,sortida)) %>% 
  filter(yearindex>=2006)

# filtrar per Pacients 
PACIENTS_MATCH<-dades_match %>% left_join(select(PACIENTS,-c(grup,dtevent)), by="idp")

table(PACIENTS_MATCH$yearindex,PACIENTS_MATCH$grup)
descrTable(formula_vector(llistaPS,y="grup"),data=dades_match)

# Filtrar per edat=35-100 anys en data index
library(lubridate)
PACIENTS_MATCH<-PACIENTS_MATCH %>% 
  mutate(edat = year(as.period(interval(start = ymd(dnaix), end = dtindex))))
PACIENTS_MATCH<-PACIENTS_MATCH %>% filter(edat>=35 & edat<=100)



