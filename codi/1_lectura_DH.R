# Lectura de fitxers --------------------

#--------------------------------#



#install.packages("remotes")
#remotes::install_github("tagteam/heaven")
#--------------------------------#
#library(heaven)
#heaven::riskSetMatch

# Per installar llibreria heaven::riskSetMatch

library("githubinstall")
githubinstall("heaven",ref="964bbbd",force=T) # "2018.8.9"




#--------------------------------##--------------------------------#
#Hola, et comento 
#He estat investigant també, és el que dius tu, 
#van fer dos llibreries més incidenceMatch() etc, i en les noves versions han eliminat la funció riskSetMatch()
#Ho he provat amb les meves dades i les coses que canvien són:
# 1. Els noms dels parámetres canvien, i han alguns arguments opocionals .....com la llavor ,etc..., 
# 2. Va molt més lent ,( no sé perqué)
# 3. S'ha de vigilar amb els noms dels arguments dels paràmetres ja que si són els mateixos que el data.table que et genera, peta (p.e. el nom <event> no es pot posar com a paràmetre)
# 4. No et genera la data index pels grups a risc per tant després l'has d'assignar als grups a risc (casos i controls)
# 4. M'ha trobat uns quants més controls que l'anterior funció però ha tardat molt més. , nose si es per la llavor o que...
# 5. De totes maneres si vols utilitzar la versió antiga pots descarregar la llibreria amb el commit 964bbbd
# Installar versió de github 964bbbd (2018.08.09)

#githubinstall("heaven",ref="964bbbd",force=T) 

#Qualsevol dubte em truques
#Fins aviat 
#--------------------------------##--------------------------------#
#install.packages("heaven")
#--------------------------------------------------------------------------#
#library("heaven")
#--------------------------------------------------------------------------#
#githubinstall("heaven",ref="964bbbd",force=T)
#--------------------------------------------------------------------------#
# 1. Lectura de fitxers 
#memory.size(max=160685)
memory.limit()
#
# Directori Font     ==============================  

#--------------------------------------------------------------------------#
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)
#--------------------------------------------------------------------------#
#--------------------------------#
#testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320
#--------------------------------#

# generar_mostra_fitxers()  ----------------------

# Llegir tots els fitxers RDS dins d'un directori i generar una mostra aleatoria i salvar-lo en un directori "mostra"


#i        [cmbdh_diagnostics] mult
LLEGIR.cmbdh_diagnostics_padris<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds")) %>% as_tibble()
variable.names(LLEGIR.cmbdh_diagnostics_padris)
#[1] "idp"    "cod"    "dat"    "dx_pos" "dalta"  "calta"  "agr" 
#----------------------------------------------#
#ii       [dianostics]  mult
LLEGIR.diagnostics<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_diagnostics_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.diagnostics)
#[1] "idp"    "cod"    "dat"    "dbaixa" "agr"   
#----------------------------------------------#

#iii      [farmacs_facturats] mult
LLEGIR.farmacs_facturat<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_farmacs_facturats_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_facturat)
#[1]"idp" "cod" "dat" "agr" "env"
#----------------------------------------------#
#iv       [farmacs_prescrits] mult
LLEGIR.farmacs_prescrits<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_farmacs_prescrits_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_prescrits)
#[1] "idp"    "cod"    "dat"    "dbaixa" "ics"    "ap"     "agr"
#----------------------------------------------#

#v        [població] unic
LLEGIR.poblacio<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_poblacio_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.poblacio)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#----------------------------------------------#
#vi       [tabaquisme] unic
LLEGIR.tabaquisme<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_tabaquisme_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.tabaquisme)
#[1] "idp"    "val"    "dat"    "dbaixa"
#----------------------------------------------#

#vii      [analitiques] mult
LLEGIR.variables_analitiques<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_analitiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_analitiques)
#[1] "idp" "cod" "dat" "val" "agr"
#----------------------------------------------#
#viii     [variables_cliíniques] mult
LLEGIR.variables_cliniques<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_cliniques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_cliniques)
#[1] "idp" "cod" "dat" "val" "agr"
#----------------------------------------------#

#ix       [variables geo_sanitàries] unic
LLEGIR.variables_geo_sanitaries<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_geo_sanitaries_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_geo_sanitaries)
#[1] "idp"     "idup"    "idabs"   "idrs"    "iddap"   "idambit"  
#----------------------------------------------#  
#x        [variables socioeconòmiques] unic
LLEGIR.variables_socioeconomiques<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_socioeconomiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_socioeconomiques)
#[1] "idp"       "qmedea"    "ruralitat"

                                                  #####################
                                                  #Cohort define steps#
                                                  #####################

#1.	  From the entire database (ED), extract T2DM cohort with any codes in sheet “exposed”; 
#     then remove patients with any codes in sheet “exclude” before the end of the study (31/12/2018); 
#     use the first appearance diagnosis code of T2DM as the index date for exposed patient cohort (T2C).
#     --------------------------------------------------------------------------------------------------------
#     De tota la base de dades (ED), extreu la cohort T2DM amb els codis del full "exposat"; 
#     després elimineu els pacients amb els codis del full "excloure" abans de finalitzar l'estudi (31/12/2018);
#     utilitzeu el codi de diagnòstic de primera aparició de T2DM com a data índex de la cohort del pacient exposada (T2C). 
#     --------------------------------------------------------------------------------------------------------
#2.	  From this cohort (T2C), 
#     remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; 
#     this is the exposed cohort (EC).
#     --------------------------------------------------------------------------------------------------------
#     D’aquesta cohort (T2C), traieu els pacients amb els codis del full “prevalent” o “càncer” 
#     que apareixin abans de la data de l’índex; es tracta de la cohort exposada (CE). 
#     --------------------------------------------------------------------------------------------------------
#3.	  From the entire database (ED), 
#     remove patients with any codes in sheet “non-exposed pool” before the end of the study (31/12/2018), 
#     to get the candidate non-exposed patients (CNE).
#     --------------------------------------------------------------------------------------------------------
#     De tota la base de dades (ED), elimineu els pacients amb els codis del full "piscina no exposada" 
#     abans de finalitzar l'estudi (31/12/2018) per obtenir els pacients candidats no exposats (CNE). 
#     --------------------------------------------------------------------------------------------------------
#4.	  Exact matching the exposed cohort (EC) to the candidate non-exposed patients (CNE) with a ratio 1:10 (EC:CNE)
#     by year of birth (+/-1year), sex, and practice, 
#     without replacement (each candidate non-exposed patient can be only matched once). 
#     This is the matched cohort (MC), and the index date is the same as the matched exposed patient.
#     --------------------------------------------------------------------------------------------------------
#     Correspondre exactament a la cohort exposada (CE)
#     als pacients candidats no exposats (CNE) amb una proporció 1:10 (EC: CNE) 
#     per any de naixement (+/- 1 any), sexe i pràctica, sense substitució 
#     ( cada pacient candidat no exposat només es pot combinar una vegada). Aquesta és la cohort coincident (MC)
#     i la data de l’índex és la mateixa que el pacient exposat igualat. 
#     --------------------------------------------------------------------------------------------------------
#5.	  From the matched cohort (MC), remove patients died before the index date; 
#     then remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; 
#     then keep a randomly selected 5 matched non-exposed patients 
#     (good to set a seed to make the random selection replicable). This is the final non-exposed cohort (FNE).
#     --------------------------------------------------------------------------------------------------------
#     De la cohort aparellada (MC), elimineu els pacients morts abans de la data de l’índex; 
#     a continuació, elimineu els pacients amb qualsevol codi en el full "prevalent" o "càncer" 
#     que aparegui abans de la data de l'índex; a continuació, 
#     mantingueu a 5 pacients no exposats seleccionats aleatòriament
#     (és bo establir una llavor perquè la selecció aleatòria sigui replicable). 
#     Es tracta de la cohort final no exposada (FNE).
#     --------------------------------------------------------------------------------------------------------
#6.	  The final study cohort is the combination of the exposed cohort (EC) and the final non-exposed cohort (FNE).
#     --------------------------------------------------------------------------------------------------------
#     La cohort d'estudi final és la combinació de la cohorte exposada (EC) i la cohort final no exposada (FNE)
#     --------------------------------------------------------------------------------------------------------



##############################################################################################################################
# Generar data index -----------
dt_diagnostics<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

dt_diagnostics_global<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))


#data minima[]#
dt_cataleg<-read_excel("Spain_codes.xls") %>% select(cod,agr,exposed)

dt_diagnostics<-dt_diagnostics %>% left_join(dt_cataleg,by="cod") %>% filter(exposed=="exposed") 


#-----#
DINDEX<-dt_diagnostics%>% group_by(idp)%>%summarise(data_index=min(dat,na.rm = TRUE))%>%ungroup()
DINDEX
##############################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una base de dades dels exposat( TOTS ELS DELS DINDEX!!), amb ANY DE NAIXAMENT+SEXE]
#[[població]] 
# C_EXPOSATS<-DINDEX%>%left_join(LLEGIR.poblacio,by="idp")%>%mutate(Edat=as.numeric(lubridate::ymd(data_index)-lubridate::ymd(dnaix))/365.25 )
C_EXPOSATS<-DINDEX%>%left_join(LLEGIR.poblacio,by="idp")
C_EXPOSATS<-C_EXPOSATS%>%filter(entrada<=20181231) #Excluits entrada després de (31/12/2018)
variable.names(C_EXPOSATS)
#[1] "idp"        "data_index" "sexe"       "dnaix"      "entrada"    "sortida"    "situacio"   "Edat"  
#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una altre base de dades , que seran els No exposats,tots menys els exposats!!]
C_NO_EXPOSATS<-LLEGIR.poblacio %>% anti_join(C_EXPOSATS,by="idp")
C_NO_EXPOSATS<-C_NO_EXPOSATS%>%filter(entrada<=20181231) #Excluits entrada després de (31/12/2018)
variable.names(C_NO_EXPOSATS)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#-----------------------------------------------------------------------------------------------------------------------#


# Fusionar base de dades en dues : 

dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))

# Preparar matching i setriskmatching #
dt_matching<-dt_matching %>% transmute(idp,dnaix,sexe,grup,dtevent=data_index,sortida)

#   5.2.1 Generar data de sortida (Data event / Data de censura)     -----------------
## dtindex_case 
dt_matching<-dt_matching %>% mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament i grups cada 10 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 


#### Parametres d'aparellament
llistaPS=c("sexe","any_naix")
num_controls<-10
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


# Verificació d'aparellament per edad + sexe 
descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
descrTable(grup~dnaix+any_naix+sexe,data=dades_match)


# Selecciono
dt_index_match<-dades_match %>% transmute(idp,caseid,grup,dnaix,sexe,dtindex=dtindex_case,numControls) %>% as_tibble()


# Agregar problemes de salut utilitzant com a referencia la data index nova

bd_index<-dt_index_match %>% transmute(idp,dtindex=lubridate::as_date(dtindex))

dt_agregada_agr<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),bd.dindex = bd_index,dt.agregadors=select(dt_cataleg,cod,agr))


# Filtrar per exclusions (Eliminar prevalents i cancer)
dt_index_match<-dt_index_match %>% left_join(select(dt_agregada_agr,-dtindex),by="idp") %>% 
  filter(is.na(DG.prevalent) & is.na(DG.cancer))


# Selecciona 5 random 1:5 

table(dt_index_match$numControls,dt_index_match$grup)







































































##############################################################################################################################
# (demà fer-ho!)
#[ DESPRÉS FARÉ UN MATCHED COHORT [ 1 Exposició: 10 No Exposició], ANY DE NAIXAMENT (+/- 1 ANY), SEXE]
##############################################################################################################################



##############################################################################################################################
#[ De la coohrt aparellada, eliminarem els pacients Morts abans de la data Index,eliminem els pacients amb qualsevol
#  codi en el full "prevalent" o "Cancer"]
##############################################################################################################################



#[12.12.2019]--> fet!!! acabar-ho!!!!








#-----#
#mirar si a la data del dia Index  tenen 35 anys o mes!, tenir un minim un any d'història clínica prèvia
#abans de de dia index ?

# MATCH amb els controls!

#Introducció

#Al present document tractarem de traduir el protocol original del projecte a l’estructura actual del SIDIAP,
#convertint-se aquest en el manual fet servir pels Data Managers del SIDIAP durant l’extracció de les dades. 
#És un document INTERN del SIDIAP.


#Objectius del projecte


#   Objectius principals  #
###########################
#-----------------------------------------------------------------------------------------------------------------------------#
#●	Avaluar les tendències al llarg del temps en mortalitat  de totes les causes entre el 20060101 i
#   la captura més recent del SIDIAP (20181231) en persones amb recent diabetis tipus 2 
#   ien comparació amb una població control  sense diabetis.
#-----------------------------------------------------------------------------------------------------------------------------#
#●	Avaluar les diferències i les proporcions de les taxes de mortalitat de totes les causes en persones amb diabetis tipus 2
#   i sense diabetis entre el 20060101 i la captura més recent del SIDIAP (20181231). 
#-----------------------------------------------------------------------------------------------------------------------------#
#   Objectius secundaris  #
###########################
#-----------------------------------------------------------------------------------------------------------------------------#
#●	Avaluar les tendències al llarg del temps en la mortalitat cardiovascula r entre 20060101
#   i la captura més recent del SIDIAP en persones amb diabetis tipus 2 i sense diabetis.
#-----------------------------------------------------------------------------------------------------------------------------#
#●	Avaluar les diferències i les proporcions 
#   de les taxes de mortalitat per causes determinades en persones amb diabetis tipus 2 i sense diabetis entre el 20060101
#   i la captura més recent del SIDIAP.
#-----------------------------------------------------------------------------------------------------------------------------#
#●	Comparar les tendències de les taxes de mortalitat i de ràtio entre els diferents països.
#-----------------------------------------------------------------------------------------------------------------------------#
#●	Avaluar les tendències del temps en condicions renals cardiometabòliques. 
#-----------------------------------------------------------------------------------------------------------------------------#

# 20060101  la captura més recent del SIDIAP (20181231)
#[1.1.2006---31.12.2018] 


# data.index [] 
#[La data del primer diagnòstic de DM2 passarà a ser la data d’índex]: [[DINDEX]]





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



#[]

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

library("tagteam")










#--------------------------------#
#db <-read_dta("db.dta")
#nrow(db)
#--------------------------------#


######all-cause mortality in DM####
db1 <-Lexis(entry = list(period  = yearin,
                         age     = agein),
            exit  = list(period  = outm),
            exit.status = acm,
            id    = patid,
            data  = subset(db, DM == 1))

dbs1 <- splitMulti(db1, age = seq(35,100,1), period= seq(1998,2018,1))

a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(period+lex.dur,(1:5-0.5)/5))

r1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(period, knots = p.kn)*gender,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs1)

age          <- c(35:100)
period       <- seq(1998,2018,0.1)
gender       <- c(1:2)
nd           <- expand.grid(age, period, gender)
colnames(nd) <- c("age","period","gender")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r1, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM       <- cbind(nd,p1, out="acm")



#####cardio-renal-mortality in DM#####
db1 <-Lexis(entry = list(period  = yearin,
                         age  = agein),
            exit = list(period  = outm),
            exit.status = crm,
            id = patid,
            data = subset(db, DM == 1))

dbs1  <-splitMulti(db1, age = seq(35,100,1), period= seq(1998,2018,1))

a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(period+lex.dur,(1:5-0.5)/5))

r1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(period, knots = p.kn)*gender,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs1)

age          <- c(35:100)
period       <- seq(1998,2018,0.1)
gender       <- c(1:2)
nd           <- expand.grid(age, period, gender)
colnames(nd) <- c("age","period","gender")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r1, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
crm_DM       <- cbind(nd,p1, out="crm")

res_DM <-rbind(acm_DM,crm_DM)
res_DM <-cbind(res_DM, rateD=exp(res_DM$es_d), rateD_lb=exp(res_DM$lb_d), rateD_ub=exp(res_DM$ub_d))
write.dta(res_DM, file="res_DM.dta")



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



