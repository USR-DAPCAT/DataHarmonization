#
#
#Hola Ray
#Que tal? Nos faltan unas variables de data harmonization ,me las puedes pasar por favor? Adjunto los dos docs , que tenemos que enviar a los de UK
#Muchas gracias
#B
#


#-----------#
#Median follow-up, (IQR, years)
#-----------#
#Median age at diagnosis (IQR, years)
#-----------#
#Median year of entry, (IQR, year)
#-----------#


#-----------#
#Diagnosis age group
#<40
#40-50
#50-60
#60-70
#>70
#-----------#
#number of patients by year of diagnosis
#2006
#2007
#2008
#2009
#2010
#2011
#2012
#2013
#2014
#2015
#2016
#2017
#2018
#-----------#
#BMI- most recent around index date
#<15
#15-25
#25-30
#>30
#-----------#


#-----------#
#[04.02.2020]
#[03.02.2020]
#[31.01.2020]
#[30.01.2020]
#[29.01.2020]
#[28.01.2020]
#[27.01.2020]
#[24.01.2020]
#[23.01.2020]
#[22.01.2020]
#[21.01.2020]
#[20.01.2020]
#[17.01.2020]
#[16.01.2020]
#[15.01.2020]
#[14.01.2020]
#[13.01.2020]
#[10.01.2020]
#[13.01.2020]
#[31.12.2019]
#[30.12.2019]
#[27.12.2019]
#[24.12.2019]
#[20.12.2019]
#[19.12.2019]
#[17.12.2019]
#-----------#


# s'ha de fer :

#-----------------
# !!! Lexis.lines 
# !!! RR GLOBAL!
# splines
#-----------------


# Lectura de fitxers --------------------




#install.packages("remotes")
#remotes::install_github("tagteam/heaven")
#--------------------------------#
#library(heaven)
#heaven::riskSetMatch

# Per installar llibreria heaven::riskSetMatch

#noooo

#library("githubinstall")
#githubinstall("heaven",ref="964bbbd",force=T) # "2018.8.9"



#Ray Puig <raypuig@gmail.com>
#  dc., 11 de des. 16:27 (fa 7 dies)
#per a mi

#https://github.com/RayPuig


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

#--------------------------------------------------------------------------#
#library("githubinstall")
#githubinstall("heaven",ref="964bbbd",force=T)
#--------------------------------------------------------------------------#
# S'ha de treure :[00LOCK-heaven]
#ERROR: failed to lock directory 'C:/Program Files/R/R-3.6.1/library' for modifying
#Try removing 'C:/Program Files/R/R-3.6.1/library/00LOCK-heaven'
#Error: Failed to install 'heaven' from GitHub:
#--------------------------------------------------------------------------#

# 1. Lectura de fitxers 
#memory.size(max=160685)
memory.limit()
#
library(splines)
library(stats)
library(graphics)
library("LexisPlotR")
library(Epi)
library(lubridate)
#
# Directori Font     ==============================  

library("LexisPlotR")
library("Epi")
library("lubridate")
# library("xlsx")


# Parametres  --------------------------

# Si mostra = T llegeix MOSTRA (Altri llegeix població)

mostra<-T
if (mostra) directori_dades<-"dades/sidiap/test" else directori_dades<-"dades/sidiap"
directori_dades


#--------------------------------------------------------------------------#
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)
#--------------------------------------------------------------------------#
#--------------------------------#
#testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320
#--------------------------------#

# generar_mostra_fitxers()  ----------------------

# Llegir tots els fitxers RDS dins d'un directori i generar una mostra aleatoria i salvar-lo en un directori "mostra"

#LLEGIR.cmbdh_diagnostics_padris<-readRDS("dades/SIDIAP/test" ) without 

#i        [cmbdh_diagnostics] mult

list.files(directori_dades)

LLEGIR.cmbdh_diagnostics_padris<-readRDS(directori_dades%>% here::here("DAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds")) %>% as_tibble()
variable.names(LLEGIR.cmbdh_diagnostics_padris)
#[1] "idp"    "cod"    "dat"    "dx_pos" "dalta"  "calta"  "agr" 
#----------------------------------------------#
#ii       [dianostics]  mult
LLEGIR.diagnostics<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_diagnostics_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.diagnostics)
#[1] "idp"    "cod"    "dat"    "dbaixa" "agr"   
#----------------------------------------------#

#iii      [farmacs_facturats] mult
LLEGIR.farmacs_facturat<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_farmacs_facturats_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_facturat)
#[1]"idp" "cod" "dat" "agr" "env"
#----------------------------------------------#
#iv       [farmacs_prescrits] mult
LLEGIR.farmacs_prescrits<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_farmacs_prescrits_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_prescrits)
#[1] "idp"    "cod"    "dat"    "dbaixa" "ics"    "ap"     "agr"
#----------------------------------------------#

#v        [població] unic
LLEGIR.poblacio<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_poblacio_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.poblacio)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#----------------------------------------------#
#vi       [tabaquisme] unic
LLEGIR.tabaquisme<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_tabaquisme_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.tabaquisme)
#[1] "idp"    "val"    "dat"    "dbaixa"
#----------------------------------------------#

#vii      [analitiques] mult
LLEGIR.variables_analitiques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_analitiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_analitiques)
#[1] "idp" "cod" "dat" "val" "agr"
#----------------------------------------------#
#viii     [variables_cliíniques] mult
LLEGIR.variables_cliniques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_cliniques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_cliniques)
#[1] "idp" "cod" "dat" "val" "agr"
#----------------------------------------------#

#ix       [variables geo_sanitàries] unic
LLEGIR.variables_geo_sanitaries<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_geo_sanitaries_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_geo_sanitaries)
#[1] "idp"     "idup"    "idabs"   "idrs"    "iddap"   "idambit"  



#----------------------------------------------#  
#x        [variables socioeconòmiques] unic
LLEGIR.variables_socioeconomiques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_socioeconomiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_socioeconomiques)
#[1] "idp"       "qmedea"    "ruralitat"

#----------------------------------------------#  
#xi        [Catàleg]
LLEGIR.variables_Cataleg<-readRDS("dades/SIDIAP/test" %>% here::here("DAPCRMM_entregable_cataleg_20190930_093320.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_Cataleg)
#[1]  "domini" "cod"    "des"    "agr"   
#----------------------------------------------#  

#table(LLEGIR.variables_Cataleg$agr)
#----------------------------------------------#  
#       ALFAGLUC   ALT_GLUC   BIGUANIDAS    CAC     CANCER     CKDEPI       cLDL  COMB_GLUC 
#1          6         18          4          1        520          1          1         38 
#----------------------------------------------#  
#cT        CVD       DPP4    GLICADA   GLINIDES       GLP1        IMC  INSULINAS         KD 
#1        576         12          1          4         22          1         66        210 
#----------------------------------------------#  
#MET      SGLT2      SULFO      TIAZO 
#62         14         28          6 
#----------------------------------------------#  
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

#----------------------------------------------#
#i        [cmbdh_diagnostics]               mult
#LLEGIR.cmbdh_diagnostics_padris.
#----------------------------------------------#
#ii       [dianostics]                      mult
#LLEGIR.diagnostics 
#----------------------------------------------#

##################################################################################################
# Generar data index -----------
dt_diagnostics<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

dt_diagnostics_global<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))
##################################################################################################
dt_cataleg<-read_excel("Spain_codes.xls") %>% select(cod,agr,exposed)
######################################
NUM_POBLACIO<-length(LLEGIR.poblacio$idp)
#----------------------------------------------------------------------------


# FILTRE C.INCLUSIÓ   --------------- 


######################################
#
#i)  FILTRE_1 : exposed=="exposed"
dt_diagnostics<-dt_diagnostics %>% left_join(dt_cataleg,by="cod") %>% filter(exposed=="exposed") 
#
#ii) FILTRE_2 : Filtrem la base de dades 2004-2018 *[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
# canvi a 2006!
dt_diagnostics<-dt_diagnostics%>% filter(dat>=20060101  & dat<=20181231)
#
######################################
gc()

#busquem la DATA ÍNDEX!:[]

#data minima[data Índex!!!]#
#-----#
DINDEX<-dt_diagnostics%>% group_by(idp)%>%summarise(data_index=min(dat,na.rm = TRUE))%>%ungroup()
#DINDEX


##############################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una base de dades dels exposat( TOTS ELS DELS DINDEX!!), amb ANY DE NAIXAMENT+SEXE]
#[[població]] 
# C_EXPOSATS<-DINDEX%>%left_join(LLEGIR.poblacio,by="idp")%>%mutate(Edat=as.numeric(lubridate::ymd(data_index)-lubridate::ymd(dnaix))/365.25 )
C_EXPOSATS<-DINDEX%>%left_join(LLEGIR.poblacio,by="idp")
#C_EXPOSATS<-C_EXPOSATS%>%filter(entrada<=20181231) #Excluits entrada després de (31/12/2018)
variable.names(C_EXPOSATS)
C_EXPOSATS_num<-length(C_EXPOSATS$idp)
#C_EXPOSATS_num

#[1] "idp"        "data_index" "sexe"       "dnaix"      "entrada"    "sortida"    "situacio"   "Edat"  

#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una altre base de dades , que seran els No exposats,tots menys els exposats!!]
#[#Excluits entrada:abans(01/01/2004) i  després de (31/12/2018)]
# canvi a 2006!
C_NO_EXPOSATS<-LLEGIR.poblacio%>%filter(entrada<=20181231)%>%anti_join(C_EXPOSATS,by="idp")
C_NO_EXPOSATS<-C_NO_EXPOSATS 
variable.names(C_NO_EXPOSATS)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#-----------------------------------------------------------------------------------------------------------------------#
#min(LLEGIR.poblacio$entrada)-->[01.01.2006]
#max(LLEGIR.poblacio$entrada)-->[19.12.2018]
C_NO_EXPOSATS_num<-length(C_NO_EXPOSATS$idp)
#C_NO_EXPOSATS_num


#iii) FILTRE_3 : Els  EXPOSATS a DIABETIS TIPUS2 amb DIAiNDEX que tinguin <35 anys l'any 1/1/2006, quedaran FORA!, per tant hagin nascut l'any <=1971.
C_EXPOSATS<-C_EXPOSATS%>%filter(dnaix<=19710101)
C_EXPOSATS2_num<-length(C_EXPOSATS$idp)
#C_EXPOSATS2_num



# PREPARACIÓ ------------------




# Fusionar base de dades en dues : 

dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))

# Preparar matching i setriskmatching #
dt_matching<-dt_matching %>% transmute(idp,dnaix,sexe,grup,dtevent=data_index,sortida) %>%left_join(LLEGIR.variables_geo_sanitaries,by="idp")
#dt_matching

#   5.2.1 Generar data de sortida (Data event / Data de censura)     -----------------
## dtindex_case 
dt_matching<-dt_matching %>% mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament i grups cada 10 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 


#birth (+/-1year), sex, and practice, 

#### Parametres d'aparellament
llistaPS=c("sexe","any_naix","iddap")
num_controls<-10
llavor<-125
set.seed(llavor)

gc()

#5188815





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


# Número de controls per conjunt a risk  ------------
heaven::matchReport(dades_match, id="idp",case="grup",caseid="caseid")

# Número de controls per conjunt a risk  ------------
dades_match[,numControls:=.N,by=caseid]
dades_match<- dades_match %>% mutate(numControls=numControls-1)

#table(dades_match$numControls,dades_match$grup)

# Verificació d'aparellament per edad + sexe 

#descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
#descrTable(grup~dnaix+any_naix+sexe,data=dades_match)
#---------------------------------------------------------------------------------------------#
# ---------------------------------------------------------------------------------------------#
# Preparo dt_index_match per
  
dt_index_match<-dades_match %>% transmute(idp,iddap,caseid,grup,dnaix,sexe,dtindex=dtindex_case,numControls)%>%as_tibble()
#
bd_index<-dt_index_match %>% transmute(idp,dtindex=lubridate::as_date(dtindex))
# Agrego problemes de Salut
dt_agregada_agr<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                   bd.dindex = bd_index,
                                   dt.agregadors=select(dt_cataleg,cod,agr))


#iV) FILTRE_4:  Si un dels elements del grup [caseid] és prevalent o càncer, tot el [caseid] anirà a FORA!!!
#                Filtrar per exclusions (Eliminar prevalents i cancer)

dt_index_match <-dt_index_match %>% 
  left_join(select(dt_agregada_agr,-dtindex),by="idp") %>%
  mutate(can_prev=ifelse(!is.na(DG.prevalent) | !is.na(DG.cancer),0,1 ))%>%
  group_by(caseid)%>%mutate(sel=min(can_prev)) %>% ungroup() %>%
  filter(sel==1) %>% select(-sel)



# Seleccionar com a molt 5 No exposats ---------
#---------------------------------------------------------------------------------------------#
dt_index_match<-dt_index_match%>%group_by(caseid)%>%mutate(idp2 = row_number())%>%ungroup()
dt_index_match<-dt_index_match%>%mutate(idp2=ifelse(grup==1,0,idp2))
#---------------------------------------------------------------------------------------------#
dt_index_match<-dt_index_match%>%filter(idp2<=5)%>%as_tibble()
#---------------------------------------------------------------------------------------------#
dt_index_match<-dt_index_match %>% group_by(caseid) %>% mutate(numControls=n()-1) %>% ungroup() %>% select(-idp2)

#---------------------------------------------------------------------------------------------#

# Eliminem pajaros que notenen controls o casos
dt_index_match<-dt_index_match %>% filter(numControls>=1)

# Verifiquem num controls x cas 
table(dt_index_match$numControls,dt_index_match$grup)

# Verificació d'aparellament per edad + sexe 
#descrTable(grup~dnaix+sexe,data=dt_index_match)
#descrTable(grup~dnaix+sexe,data=dt_matching)

# Agregar resta d'historics  -----------------


#i    LLEGIR.variables_analitiques
#ii   LLEGIR.variables_cliniques
#iii  LLEGIR.tabaquisme
#iv   LLEGIR.farmacs_prescrits
#v    LLEGIR.farmacs_facturat
#vi   LLEGIR.variables_socioeconomiques





# Agregar variables --------------
dt_variables<-LLEGIR.variables_analitiques %>% bind_rows(LLEGIR.variables_cliniques) %>% 
  transmute(idp,cod=agr,dat,val)
dt_temp<-dt_index_match %>% transmute(idp,dtindex=lubridate::as_date(dtindex))
dtagr_variables<-agregar_analitiques(dt=dt_variables,bd.dindex=dt_temp,finestra.dies = c(-365,0))





# Agregar tabac --------------
LLEGIR.tabaquisme<-LLEGIR.tabaquisme %>% transmute(idp,cod="tabac",dat,val)
dtagr_tabac<-agregar_analitiques(dt=LLEGIR.tabaquisme,bd.dindex=dt_temp,finestra.dies = c(-Inf,0))


# ----------------------------------------------------------#
# Prescripcions / Facturació pendent de CODIS / AGREGADORS 
# ----------------------------------------------------------#


# ----------------------------------------------------------#
#iv   LLEGIR.farmacs_prescrits
# Prescripcion de CODIS / AGREGADORS 
LLEGIR.farmacs_prescrits<-LLEGIR.farmacs_prescrits %>% transmute(idp,cod,dat,dbaixa)
#
dtagr_prescrip<-agregar_prescripcions(
  dt=LLEGIR.farmacs_prescrits,
  bd.dindex=dt_temp,
  dt.agregadors=select(dt_cataleg,cod,agr),
  prefix="FP.",
  finestra.dies=c(-45,+45),
  camp_agregador="agr",
  agregar_data=F)
#FP.ALFAGLUC 
#FP.ALT_GLUC
#FP.BIGUANIDAS
#FP.COMB_GLUC
#FP.DPP4       
#FP.GLINIDES  
#FP.GLP1       
#FP.INSULINAS
#FP.SULFO     

#???
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.ALFAGLUC=case_when(FP.ALFAGLUC>=1 ~ 1,TRUE~0))

dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.ALT_GLUC=case_when(FP.ALT_GLUC>=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.BIGUANIDAS=case_when(FP.BIGUANIDAS>=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.COMB_GLUC=case_when(FP.COMB_GLUC>=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.DPP4 =case_when(FP.DPP4 >=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.GLINIDES=case_when(FP.GLINIDES>=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.GLP1=case_when(FP.GLP1>=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.INSULINAS=case_when(FP.INSULINAS>=1 ~ 1,TRUE~0))
dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.SULFO=case_when(FP.SULFO  >=1 ~ 1,TRUE~0))
# ----------------------------------------------------------#



# ----------------------------------------------------------#
# v    LLEGIR.farmacs_facturat
# Facturació pendent de CODIS / AGREGADORS 
LLEGIR.farmacs_facturat<-LLEGIR.farmacs_facturat %>% transmute(idp,cod,dat,env)
#
dtagr_facturat<-agregar_facturacio(
  dt=LLEGIR.farmacs_facturat,
  finestra.dies=c(-365,0),
  dt.agregadors=select(dt_cataleg,cod,agr),
  prefix="FF.",
  camp_agregador="agr",
  agregar_data=F)
#FF.ALFAGLUC 
#FF.ALT_GLUC 
#FF.BIGUANIDAS
#FF.COMB_GLUC
#FF.DPP4      
#FF.GLINIDES  
#FF.GLP1   
#FF.INSULINAS 
#FF.SGLT2     
#FF.SULFO    
#FF.TIAZO 
dtagr_facturat<-dtagr_facturat%>%mutate(FF.ALFAGLUC=case_when(FF.ALFAGLUC>=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.ALT_GLUC =case_when(FF.ALT_GLUC >=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.BIGUANIDAS=case_when(FF.BIGUANIDAS>=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.COMB_GLUC=case_when(FF.COMB_GLUC>=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.DPP4  =case_when(FF.DPP4  >=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.GLINIDES=case_when(FF.GLINIDES>=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.GLP1 =case_when(FF.GLP1 >=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.INSULINAS =case_when(FF.INSULINAS>=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.SGLT2  =case_when(FF.SGLT2 >=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.SULFO  =case_when(FF.SULFO  >=1 ~ 1,TRUE~0))
dtagr_facturat<-dtagr_facturat%>%mutate(FF.TIAZO  =case_when(FF.TIAZO  >=1 ~ 1,TRUE~0))
# ----------------------------------------------------------#



  

#i    LLEGIR.variables_analitiques
#ii   LLEGIR.variables_cliniques
#iii  LLEGIR.tabaquisme
#iv   LLEGIR.farmacs_prescrits
#v    LLEGIR.farmacs_facturat


#dt_variables             :#i    LLEGIR.variables_analitiques+#ii   LLEGIR.variables_cliniques
#LLEGIR.tabaquisme        :#iii  LLEGIR.tabaquisme
#LLEGIR.farmacs_prescrits :#iv   LLEGIR.farmacs_prescrits
#LLEGIR.farmacs_facturat  :#v    LLEGIR.farmacs_facturat

#i,ii                            dtagr_variables
#iii                             dtagr_tabac
#iv                              dtagr_prescrip
#v                               dtagr_facturat

#dt_index_match 
#LLEGIR.poblacio
dtagr_variables  <-dtagr_variables   %>%select(-dtindex )
dtagr_tabac      <-dtagr_tabac       %>%select(-dtindex )
dtagr_prescrip   <-dtagr_prescrip    %>%select(-dtindex )
dtagr_facturat   <-dtagr_facturat    %>%select(-dtindex )

#     [LLEGIR.variables_socioeconomiques]
dt_sociodemo<-LLEGIR.variables_socioeconomiques


# Unió de totes les agregacions ----------------

dt_total<-dt_index_match %>% 
  left_join(select(LLEGIR.poblacio,idp,sortida,situacio))%>% 
    left_join(dtagr_variables,by="idp")%>%
      left_join(dtagr_tabac,by="idp")%>%
        left_join(dtagr_prescrip,by="idp")%>%
          left_join(dtagr_facturat ,by="idp")%>%
                left_join(dt_sociodemo ,by="idp")
  

#canvis :[]
  
dt_total2<-dt_total%>%mutate(dtindex=as_date(dtindex))
dt_total2<-dt_total2 %>% mutate(any_index=lubridate::year(lubridate::as_date(dtindex)))
dt_total2<-dt_total2 %>% mutate(agein=(as_date(dtindex)-ymd(dnaix))/365.25)
dt_total2<-dt_total2 %>% mutate(exitus=if_else(situacio=="D",1,0))
dt_total2<-dt_total2 %>% mutate(temps_FU=ymd(sortida)-as_date(dtindex))
dt_total2<-dt_total2 %>% mutate(agein2=as.numeric(as_date(dtindex)-ymd(dnaix))/365.25)
dt_total2$dnaix<-as.character(dt_total2$dnaix)
dt_total2$any_index2<-as.character(dt_total2$any_index)

#recodificació:
dt_total2<-dt_total2%>%mutate(agein3.cat6=case_when(agein2<40~ 1,
                                                    agein2>=40 & agein2<50 ~ 2,  
                                                    agein2>=50 & agein2<60 ~ 3,
                                                    agein2>=60 & agein2<70 ~ 4,
                                                    agein2>=70  ~ 5))

dt_total2_num<-length(dt_total2$grup)


#dt_total2$agein2

#variable.names(dt_total2)


#[1] "idp"           "iddap"         "caseid"        "grup"          "dnaix"         "sexe"         
#[7] "dtindex"       "numControls"   "DG.cancer"     "DG.DM2"        "DG.exclude"    "DG.outcome"   
#[13] "DG.prevalent"  "can_prev"      "sortida"       "situacio"      "CAC.valor"     "CKDEPI.valor" 
#[19] "cLDL.valor"    "cT.valor"      "GLICADA.valor" "IMC.valor"     "CAC.dies"      "CKDEPI.dies"  
#[25] "cLDL.dies"     "cT.dies"       "GLICADA.dies"  "IMC.dies"      "tabac.valor"   "tabac.dies"   
#[31] "FP.ALFAGLUC"   "FP.ALT_GLUC"   "FP.BIGUANIDAS" "FP.COMB_GLUC"  "FP.DPP4"       "FP.GLINIDES"  
#[37] "FP.GLP1"       "FP.INSULINAS"  "FP.SULFO"      "FF.ALFAGLUC"   "FF.ALT_GLUC"   "FF.BIGUANIDAS"
#[43] "FF.COMB_GLUC"  "FF.DPP4"       "FF.GLINIDES"   "FF.GLP1"       "FF.INSULINAS"  "FF.SGLT2"     
#[49] "FF.SULFO"      "FF.TIAZO"      "qmedea"        "ruralitat"     "any_index"     "agein"        
#[55] "exitus"        "temps_FU"     

#dt_total2$qmedea
#dt_total2$ruralitat



#dt_total2_num
#table(dt_total2$grup)


# ANALISIS  --------------

#https://rstudio-pubs-static.s3.amazonaws.com/369387_b8a63ee7e039483e896cb91f442bc72f.html

#-------------------------------------------------------#
#                                   A  N A L I S I S    #
#-------------------------------------------------------#



#This is a retrospective cohort study. 
#All people diagnosed with diabetes 
#between 01/01/2006 and the latest specific capture 31/12/2018
#entrada>=20060101  & entrada<=20181231



#i) [[Flowchart]]



#ii)
#--------------------------------------#
# D E S C R I P C I Ó :
#--------------------------------------#

#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.SULFO=case_when(FP.SULFO  >=1 ~ 1,TRUE~0))
#dades_1000<-dades_1000%>%mutate(CVD_CHF=case_when((DG.CHF=="Yes" | DG.MCV=="Yes")~ "Yes",TRUE~"No" ))
#x & y	x AND y

----------------------------------------------------------------#
conductor_variables<-"conductor_DataHarmonization.xls"
#------------------------------------------------------------------#
dt_total2<-recodificar(dt_total2,taulavariables = conductor_variables,"recode",missings = T)
variable.names(dt_total2)
#dt_total2$agein2
#------------------------------------------------------------------#


#apliquem esl conductor!:[ull!]
dt_total4<-dt_total2



#------------------------------------------------------------------#
dt_total4<-convertir_dates(d=dt_total4,taulavariables=conductor_variables)
dt_total4<-etiquetar_valors(dt=dt_total4,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
dt_total4<-etiquetar(d=dt_total4,taulavariables=conductor_variables)
#------------------------------------------------------------------#

variable.names(dt_total4)

#dt_recod  <-dt_total4%>%select(IMC.valor.cat5 )

#ActualitzarConductor(d=dt_recod,taulavariables="conductor_DataHarmonization.xls")


# mirar-ho per MEDIAN [[temps_FU;agein;any_index]]

#variable.names(dt_total2)

#i
#***********************************************************************#
formula_taula00<-formula_compare("taula00",y="grup",taulavariables = conductor_variables)

T00<-descrTable(formula_taula00,method = c(IMC.valor=2,temps_FU = 2,agein=2,any_index=2),data=dt_total4,max.xlev = 100, show.p.overall=FALSE)
#***********************************************************************#
T00
#***********************************************************************#


 

#***********************************************************************#
# FLOWCHART.
#***********************************************************************#

#



pob=NUM_POBLACIO
#pob=c(5188815)

pob_lab=c("Mortalidad en personas Diabéticas Tipo 2 en comparación en Población Control[MUESTRA]")



  

#C_EXPOSATS_num
#C_NO_EXPOSATS_num

#C_EXPOSATS2_num
#C_NO_EXPOSATS2_num=C_NO_EXPOSATS_num
#table(dt_total2$grup)
C_EXPOSATS_MATCHED_num<-dt_total2%>%filter(grup==1)
C_EXPOSATS_MATCHED_num<-length(C_EXPOSATS_MATCHED_num$idp)
C_NO_EXPOSATS_MATCHED_num<-dt_total2%>%filter(grup==0)
C_NO_EXPOSATS_MATCHED_num<-length(C_NO_EXPOSATS_MATCHED_num$idp)

#pob1=c(838,251)
#pob2=c(9162,550)
#exc1=c(805)
#exc2=c(805)

pob1=c(C_EXPOSATS_num,C_EXPOSATS_MATCHED_num)
pob2=c(C_NO_EXPOSATS_num,C_NO_EXPOSATS_MATCHED_num)

pob_lab1=c("Población Diabética íncidente anterior del Matched [01/01/2006 - 31.12.2018]","Matched:[Sexo-Año de Nacimiento-Iddap] 1:5")
pob_lab2=c("Población No Diabética íncidente anterior del Matched [01/01/2006 - 31.12.2018]","Matched:[Sexo-Año de Nacimiento-Iddap] 1:5")


exc1=c(C_EXPOSATS2_num)
exc_lab1=c('Edad>=35 años')

exc2=c(C_NO_EXPOSATS_num)
exc_lab2=c('Edad>=35 años')


colors=c('grey','orange')
forma=c('box','box')

#***********************************************************************#
flowchart<-diagramaFlowchart(
      grups=2,
      pob=pob,
      pob_lab=pob_lab,
      pob1=pob1,
      pob_lab1=pob_lab1,
      exc1=exc1,
      exc_lab1=exc_lab1,
      pob2=pob2,
      pob_lab2=pob_lab2,
      exc2=exc2,
      exc_lab2=exc_lab2,
      colors=colors,
      forma=forma)
#***********************************************************************#
flowchart
#***********************************************************************#



#iii)
#Tasa de incidencia o densidad de incidencia

#(DI). 
#Se calcula como el cociente entre el número de casos nuevos 
#de una enfermedad ocurridos durante el periodo de seguimiento 
#y la suma de todos los tiempos individuales de observación: 

# 
#dt_total2$temps_FU
#dt_total2$exitus
#dt_total2$grup
# 

#table(dt_total2$situacio,dt_total2$exitus)
#dt_total2$situacio
#dt_total2$situacio=="Difunto"
#dt_total4$grup

rescox<-coxph(Surv(temps_FU,exitus)~grup,data=dt_total2)    

N=rescox$n
EVENTS=rescox$nevent
coef=summary(rescox)$coef[1,1]
HR=summary(rescox)$coef[1,2]
IC951=summary(rescox)$conf.int[1,3]
IC952=summary(rescox)$conf.int[1,4]
se.coef=summary(rescox)$coef[1,3]
p=summary(rescox)$coef[1,5]


dt_total2$temps_FU<-as.numeric(dt_total2$temps_FU)



taula_events<-dt_total2 %>% group_by(grup,sexe) %>% summarise(
  events=sum(exitus), 
  N=n(),
  PTdies=sum(temps_FU),
  PTanys=(sum(temps_FU)/365.25) ,
  taxa=(events/PTanys)*10000) %>% 
  ungroup()


#taula_events
conductor_variables<-"conductor_DataHarmonization.xls"
taula_events<-etiquetar_valors(dt=taula_events,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")

#-------------------------------------------------------#
taula_events2<-dt_total2 %>% group_by(grup) %>% summarise(
  events=sum(exitus), 
  N=n(),
  PTdies=sum(temps_FU),
  PTanys=(sum(temps_FU)/365.25) ,
  taxa=(events/PTanys)*10000) %>% 
  ungroup()
#-------------------------------------------------------#
conductor_variables<-"conductor_DataHarmonization.xls"
taula_events2<-etiquetar_valors(dt=taula_events2,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
#-------------------------------------------------------#




###################
#[Taula de TAXES!]#
###################
taula_events
taula_events2


#taula_events2
#kable(taula_events, digits = 2, caption="Tasa de Moratlidad por cada 1000 perosnas-año")




#iv)
###################################################################################
# Survival 
fit<- survfit(Surv(temps_FU, exitus) ~ grup, data = dt_total2)
library("survminer")
survminer::ggsurvplot(fit,data = dt_total2)
figura1<-survminer::ggsurvplot(fit,data = dt_total2)
figura1



#
###################################################################################
#Part descriptiva  inicial!!!
#mirar LEXIS!!!
#::: preparció LEXIS:[]
#variable.names(dt_total)
#--------------------------------------#
# "idp"           
# "iddap"         
# "caseid"        
# "grup"          
# "dnaix"         
# "sexe"         
# "dtindex"       
# "numControls"   
# "DG.cancer"     
# "DG.DM2"        
# "DG.exclude"    
# "DG.outcome"   
# "DG.prevalent"  
# "can_prev"      
# "sortida"       
# "situacio"      
# "CAC.valor"     
# "CKDEPI.valor" 
# "cLDL.valor"    
# "cT.valor"      
# "GLICADA.valor" 
# "IMC.valor"     
# "CAC.dies"      
# "CKDEPI.dies"  
# "cLDL.dies"     
# "cT.dies"       
# "GLICADA.dies"  
# "IMC.dies"      
# "FP.ALFAGLUC"   
# "FP.ALT_GLUC"  
# "FP.BIGUANIDAS" 
# "FP.COMB_GLUC"  
# "FP.DPP4"       
# "FP.GLINIDES"   
# "FP.GLP1"       
# "FP.INSULINAS" 
# "FP.SULFO"      
# "FF.ALFAGLUC"   
# "FF.ALT_GLUC"   
# "FF.BIGUANIDAS" 
# "FF.COMB_GLUC"  
# "FF.DPP4"      
# "FF.GLINIDES"   
# "FF.GLP1"       
# "FF.INSULINAS"  
# "FF.SGLT2"      
# "FF.SULFO"      
# "FF.TIAZO"     
# "any_index"     
# "agein"         
# "exitus"        
# "temps_FU"     
#--------------------------------------#
#summary(dt_total2)
#--------------------------------------#

#Prova!
#--------------------------------------#
#dt_total$iddap
#dt_total$caseid  
#dt_total$grup
#dt_total$sexe
#dt_total$numControls
#dt_total$DG.cancer     
#dt_total$DG.DM2        
#dt_total$DG.exclude    
#dt_total$DG.outcome   
#dt_total$DG.prevalent  
#dt_total$can_prev      
#dt_total$sortida
#dt_total$dnaix
#dt_total$situacio
#--------------------------------------#
#dt_total$CAC.valor     
#dt_total$CKDEPI.valor 
#dt_total$cLDL.valor    
#dt_total$cT.valor      
#dt_total$GLICADA.valor 
#dt_total$IMC.valor     
#dt_total$CAC.dies      
#dt_total$CKDEPI.dies  
#dt_total$cLDL.dies
#dt_total$cT.dies       
#dt_total$GLICADA.dies  
#--------------------------------------#
#dt_total$FP.ALFAGLUC 
#dt_total$FP.ALT_GLUC
#dt_total$FP.BIGUANIDAS
#dt_total$FP.COMB_GLUC
#dt_total$FP.DPP4       
#dt_total$FP.GLINIDES  
#dt_total$FP.GLP1       
#dt_total$FP.INSULINAS
#dt_total$FP.SULFO     
#--------------------------------------#
#dt_total$FF.ALFAGLUC 
#dt_total$FF.ALT_GLUC 
#dt_total$FF.BIGUANIDAS
#dt_total$FF.COMB_GLUC
#dt_total$FF.DPP4      
#dt_total$FF.GLINIDES  
#dt_total$FF.GLP1   
#dt_total$FF.INSULINAS 
#dt_total$FF.SGLT2     
#dt_total$FF.SULFO    
#dt_total$FF.TIAZO 
#--------------------------------------#
#dt_total$exitus        
#--------------------------------------#


#v)

#Preparació LEXIS:

###################################################################################
dt_total2<-dt_total2 %>%mutate(dtindex=as_date(dtindex))
dt_total2<-dt_total2 %>%mutate(dnaix=as.Date(as.character(dnaix),    format="%Y%m%d"))
dt_total2<-dt_total2 %>%mutate(sortida=as.Date(as.character(sortida),format = "%Y%m%d"))
###################################################################################



dt_total3<-dt_total2%>%select(idp,dtindex,dnaix,sortida,exitus,grup,caseid)
###################################################################################
dt_total3<-dt_total3%>%mutate(birth =dnaix)
dt_total3<-dt_total3%>%mutate(entry =dtindex)
dt_total3<-dt_total3%>%mutate(exit =sortida)
dt_total3<-dt_total3%>%mutate(fail =exitus)
###################################################################################
dt_total3<-dt_total3%>%select(idp,birth,entry,exit,fail,grup,caseid)
###################################################################################
###################################################################################
#dt_total$dtindex
#dt_total$dnaix
#dt_total$sortida
dt_total3<-structure( dt_total3,class = "data.frame")
dt_total3 <- cal.yr(dt_total3, format="%y-%m-%d", wh=2:4 )
###################################################################################
###################################################################################
dt_total3_grup0<-dt_total3 %>% filter(grup==0)
dt_total3_grup1<-dt_total3 %>% filter(grup==1)
###################################################################################
#Com funciona!:
#####################################################################
dt_total3_10cases<-dt_total3[1:150,]
dt_total3_10cases_grup0<-dt_total3_grup0[1:150,]
dt_total3_10cases_grup1<-dt_total3_grup1[1:150,]
#####################################################################
#dt_total3_10cases
#dt_total3_10cases_grup0
#dt_total3_10cases_grup1
#####################################################################



                                                #############
                                                #L  E X I S #
                                                #############



#####################################################################
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_total3_b<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =     dt_total3)

#LEXIS_dt_total3_b
#####################################################################
#Lexis.diagram()
#plot(Lexis( entry = list( per=entry ),
#            exit = list( per=exit, age=exit-birth ),
#            exit.status = fail,
#            data = dt_total3))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
#plot(LEXIS_dt_total3_b, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2021), ylim=c(30,100), lwd=1, las=1 )
#points(LEXIS_dt_total3_b, pch=c(NA,16)[LEXIS_dt_total3_b$fail+1] )
########################################################################




#grup1 (DIABETIS)
#####################################################################
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_total3_b_grup1<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =     dt_total3_grup1 )
#LEXIS_dt_total3_b_grup1
#####################################################################
#Lexis.diagram()
#plot(Lexis( entry = list( per=entry ),
#            exit = list( per=exit, age=exit-birth ),
#            exit.status = fail,
#            data = dt_total3_grup1))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]

plot(LEXIS_dt_total3_b_grup1, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2019), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_total3_b_grup1, pch=c(NA,16)[LEXIS_dt_total3_b_grup1$fail+1] )

#GARVAR-HO!!!!!
#figura2a.png



#grup0 (NO DIABTEIS)
#####################################################################
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_total3_b_grup0<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =    dt_total3_grup0 )
#LEXIS_dt_total3_b_grup0
#####################################################################
#Lexis.diagram()
#plot(Lexis( entry = list( per=entry ),
#            exit = list( per=exit, age=exit-birth ),
#            exit.status = fail,
#            data = dt_total3_grup0))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
plot(LEXIS_dt_total3_b_grup0, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2019), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_total3_b_grup0, pch=c(NA,16)[LEXIS_dt_total3_b_grup0$fail+1] )

#GARVAR-HO!!!!!
#figura2b.png



#####################################################################
#grup0_10 cases  [grup==0] (NO DIABETIS)
#####################################################################
# Define a Lexis object with timescales calendar time and age
#LEXIS_dt_total3_b_grup0_10cases<- Lexis( 
#  entry        =     list(per=entry),
#  exit         =     list(per=exit,age=exit-birth ),
#  exit.status  =     fail,
#  data         =    dt_total3_10cases_grup0 )
#
#LEXIS_dt_total3_b_grup0_10cases
#####################################################################
#Lexis.diagram()
#plot(Lexis( entry = list( per=entry ),
#            exit = list( per=exit, age=exit-birth ),
#            exit.status = fail,
#            data = dt_total3_10cases_grup0))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
#plot(LEXIS_dt_total3_b_grup0_10cases, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2021), ylim=c(30,100), lwd=1, las=1 )
#points(LEXIS_dt_total3_b_grup0_10cases, pch=c(NA,16)[LEXIS_dt_total3_b_grup0_10cases$fail+1] )
#####################################################################
#####################################################################
#grup1_10 cases [grup==1] (DIABETIS)
#####################################################################
# Define a Lexis object with timescales calendar time and age
#LEXIS_dt_total3_b_grup1_10cases<- Lexis( 
#  entry        =     list(per=entry),
#  exit         =     list(per=exit,age=exit-birth ),
#  exit.status  =     fail,
#  data         =     dt_total3_10cases_grup1 )
#
#LEXIS_dt_total3_b_grup1_10cases
#####################################################################
#Lexis.diagram()
#plot(Lexis( entry = list( per=entry ),
#            exit = list( per=exit, age=exit-birth ),
#            exit.status = fail,
#            data = dt_total3_10cases_grup1))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
#plot(LEXIS_dt_total3_b_grup1_10cases, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2021), ylim=c(30,100), lwd=1, las=1 )
#points(LEXIS_dt_total3_b_grup1_10cases, pch=c(NA,16)[LEXIS_dt_total3_b_grup1_10cases$fail+1] )



#vi)



                  #--------------------------------------------------#
                  # M O D E L     P O I S S O N   G L M   GLOBAL     #
                  #--------------------------------------------------#

##########################
#dbs1 <- popEpi::splitMulti(LEXIS_dt_total2_b, age = seq(35,100,1), per= seq(1998,2018,1))
#dbs1
#a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
#p.kn <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
##########################
# 
#dbs1<-dbs1 %>% left_join(select(dt_total,idp,sexe)) %>% mutate(gender=if_else(sexe=="H",1,0))
#r1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*gender,
#          family = poisson,
#          offset = log(lex.dur),
#          data   = dbs1)
#summary(r1)
# Genera una matriu amb dades i fa les prediccions segons el model ajustat
#age          <- c(35:100)
#period       <- seq(1998,2018,0.1)
#gender       <- c(0:1)
#nd           <- expand.grid(age, period, gender)
#colnames(nd) <- c("age","per","gender")
#nd           <- cbind(nd, lex.dur=1000)
#p1           <- ci.pred(r1, newdata = nd, Exp = FALSE)
#colnames(p1) <- c("es_d", "lb_d", "ub_d")
#acm_DM       <- cbind(nd,p1, out="acm")
#res_DM_TOTAL <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
#--------------------------------------------------#

#http://bendixcarstensen.com/SDC/EPJmort/MortT2.pdf


#%>%mutate(gender=if_else(sexe=="H",1,0)

          
       
#----------------------------------------------------------------------------------------------------#
#Raw mortality by calendar year
#----------------------------------------------------------------------------------------------------#
S1 <- splitLexis(LEXIS_dt_total3_b, time.scale="per", breaks=2004+seq(0,14,1) )
S1<-S1%>%left_join(select(dt_total,idp,grup))%>%mutate(grup2=(if_else(grup==1,"Diabético","No Diabético")))   
#summary( S1 )
#----------------------------------------------------------------------------------------------------#
TAULA<-xtabs( cbind(Y=lex.dur,D=(lex.Xst==1),rate=lex.dur) ~I(floor(per))+grup,data=S1 )
dnam <- dimnames(TAULA)
dnam[[3]] <- c("Y","D","rate")
YDrate <- array( NA, dimnames=dnam, dim=sapply(dnam,length) )
YDrate[,,1:3] <- TAULA
#----------------------------------------------------------------------------------------------------#
YDrate[,,3] <- YDrate[,,2]/YDrate[,,1]*1000
#----------------------------------------------------------------------------------------------------#
YDrate[,,3]
table_rate<-round(ftable( YDrate, row.vars=1 ), 1 ) 
table_rate<-table_rate %>% as.data.frame() %>% unite(kk,"grup","Var3") %>% spread(kk,"Freq") 
#
#----------------------------------------------------------------------------------------------------#
variable.names(table_rate)
#----------------------------------------------------------------------------------------------------#
colnames(table_rate)[1] <- "AÑO"
colnames(table_rate)[2] <- "Mort_No_Diabet"
colnames(table_rate)[3] <- "Tasa_de_Mort_1000_Personas_Año_No_Diabet"
colnames(table_rate)[4] <- "Per_Año_No_Diabet"
colnames(table_rate)[5] <- "Mort_Diabet"
colnames(table_rate)[6] <- "Tasa_de_Mort_1000_Personas_Año_Diabet"
colnames(table_rate)[7] <- "Per_Año_Diabet"
#----------------------------------------------------------------------------------------------------#
#table_rate$Tasa_de_Mortalidad_1000_Personas_Año_Diabeticos
#table_rate$Tasa_de_Mortalidad_1000_Personas_Año_No_Diabeticos
#----------------------------------------------------------------------------------------------------#
# ordenar..
variable.names(table_rate)
table_rate<- table_rate%>%select(AÑO,
                                 Mort_Diabet,
                                 Per_Año_Diabet,
                                 Tasa_de_Mort_1000_Personas_Año_Diabet,
                                 Mort_No_Diabet,
                                 Per_Año_No_Diabet,
                                 Tasa_de_Mort_1000_Personas_Año_No_Diabet)%>% tibble() 
#----------------------------------------------------------------------------------------------------#
table_rate


#GARFICA GRAVAR-HO!!!

#----------------------------------------------------------------------------------------------------#
#...[plot!]
#----------------------------------------------------------------#
matplot(as.numeric(dimnames(YDrate)[[1]]), 
         log="y",
         las=1,
         xlab="Periodo",
         ylab="Tasa de Mortalidad Bruta por cada 1000 personas-año ,por Periodo ",
         YDrate[,,"rate"], type="l",
         lty=1, lwd=3,
         col=c("black","blue"),
         xlim=c(2006,2018))
#----------------------------------------------------------------#
#table_rate$Mortalidad_Diabeticos
#table_rate$Mortalidad_No_Diabeticos
#----------------------------------------------------------------#







#----------------------------------------------------------------#  
#table_rate
#taula_events2
#----------------------------------------------------------------#  








                  #----------------------------------------------------------------#
                  # M O D E L     P O I S S O N   G L M:Moratlitat=Diabetis*Periodo            #
                  #----------------------------------------------------------------#

#--------------------------------------------------------#
dbs00 <- popEpi::splitMulti(LEXIS_dt_total3_b, per= seq(2006,2018,1))
dbs00<-dbs00 %>%left_join(select(dt_total,idp,grup))
p.kn0 <- with(subset(dbs00, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#--------------------------------------------------------#



#MODEL POISSON PERIODO +GRUP!
r00<- glm((lex.Xst==1)~Ns(per, knots = p.kn0)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs00 )
#--------------------------------------------------------#
figura00_TOTAL<-summary(r00)
figura00_TOTAL
#--------------------------------------------------------#





#--------------------------------------------------------#
#NO DIABÈTICS!
#--------------------------------------------------------#
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd00$per,ci.pred(r00, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2004-2018  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
#--------------------------------------------------------#
#DIABÈTICS!
#--------------------------------------------------------#
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd01$per,ci.pred(r00, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2004-2018  Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
#--------------------------------------------------------#


#--------------------------------------------------------#
#COMPROVACIO: EDAT!
#--------------------------------------------------------#
#  "sexe",
#  "any_naix",
#   "iddap"            [(MATCHING!)]
#--------------------------------------------------------#




#--------------------------------------------------------#
dbs01 <- popEpi::splitMulti(LEXIS_dt_total3_b, age= seq(35,100,1))
dbs01<-dbs01 %>%left_join(select(dt_total,idp,grup))
#--------------------------------------------------------#
a.kn <- with(subset(dbs01, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
#--------------------------------------------------------#

#MODEL POISSON AGE*GRUP!

r01<- glm((lex.Xst==1)~Ns( age, knots=a.kn)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs01 )
#--------------------------------------------------------#
figura00_TOTAL2<-summary(r01)
figura00_TOTAL2



#NO DIABÈTICS!
#--------------------------------------------------------#
nd00<- data.frame(age=35:100,grup=0,lex.dur=1000)
#--------------------------------------#
#ci.pred(r0)
matplot( nd00$age,ci.pred(r01, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2004-2018  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Edad", 
         las=1, 
         ylim=c(1,1000) )
rug( a.kn , lwd=2 )
#DIABÈTICS!
#--------------------------------------------------------#
nd00<- data.frame(age=35:100,grup=1,lex.dur=1000)
#--------------------------------------#
#ci.pred(r0)
matplot( nd00$age,ci.pred(r01, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2004-2018  N Diabéticos cada  1000 Personas-año ", 
         xlab="Edad", 
         las=1, 
         ylim=c(1,1000) )
rug( a.kn , lwd=2 )






######
#dbs1#
######



#M O D E L    P R O D U C T E , M O D E L   S U M A

#--------------------------------------------------------#
dbs1 <- popEpi::splitMulti(LEXIS_dt_total3_b, age = seq(35,100,1), per= seq(2006,2018,1))
dbs1<-dbs1%>%left_join(select(dt_total,idp,grup))
#--------------------------------------------------------#
a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#--------------------------------------------------------#



#M O D E L    P R O D U C T E#[DIABETIS:(AGE*PER*GRUP)]
#--------------------------------------#
r02 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs1 )
figura02_TOTAL2<-summary(r02)
figura02_TOTAL2



#M O D E L   S U M A#[DIABETIS:(AGE+PER+GRUP)]
#--------------------------------------#
r03 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)+Ns(per, knots = p.kn)+grup,
           family = poisson,
           offset = log(lex.dur),
           data   = dbs1 )
figura02_TOTAL3<-summary(r03)
figura02_TOTAL3
#-------------------------------------------------------------------------------------------#




#--------------------------------------------------------#
#NO DIABÈTICS! producte mitjana:65 anys
#--------------------------------------------------------#
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000,age=65)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd00$per,ci.pred(r02, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 65 años Edad  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
#--------------------------------------------------------#
#DIABÈTICS! producte mitjana:65 anys
#--------------------------------------------------------#
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000,age=65)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd01$per,ci.pred(r02, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 65 años   Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
#--------------------------------------------------------#










#--------------------------------------------------------#
#NO DIABÈTICS! aditiu mitjana:65 anys
#--------------------------------------------------------#
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000,age=65)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd00$per,ci.pred(r03, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 65 años Edad  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
#--------------------------------------------------------#
#DIABÈTICS! aditiu  mitjana:65 anys
#--------------------------------------------------------#
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000,age=65)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd01$per,ci.pred(r03, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 65 años   Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
#--------------------------------------------------------#






                         
#-------------------------------------------------------------------------------------------#
# figura02_TOTAL2
# figura02_TOTAL3
# Genera una matriu amb dades i fa les prediccions segons el model ajustat
age          <- c(35:100)
period       <- seq(2006,2018,1)
grup         <- c(0:1)
nd           <- expand.grid(age, period,grup)
colnames(nd) <- c("age","per","grup")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r02, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM       <- cbind(nd,p1, out="acm")
#--------------------------------------------------------#
res_MORTALITY_PRODUCTE <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
#--------------------------------------------------------#
# write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.csv")
#--------------------------------------------------------#


# Genera una matriu amb dades i fa les prediccions segons el model ajustat
age          <- c(35:100)
period       <- seq(2006,2018,1)
grup         <- c(0:1)
nd           <- expand.grid(age, period,grup)
colnames(nd) <- c("age","per","grup")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r03, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM       <- cbind(nd,p1, out="acm")
#--------------------------------------------------------#
res_MORTALITY_SUMA <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
#--------------------------------------------------------#
write.csv2(res_MORTALITY_SUMA, file="res_MORTALITY_SUMA.csv")
#--------------------------------------------------------#



#####
#COX#
#####
##############################################################################
#r1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*gender,
#          family = poisson(),
#          offset = log(lex.dur),
#data   = dbs1)
##############################################################################
#LEXIS_dt_total3_b
COX1<-LEXIS_dt_total3_b%>% left_join(select(dt_total,idp,sexe)) %>% mutate(gender=if_else(sexe=="H",1,0))
#-----------------------------------------------#
cox_lexis_model <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+sexe+age+ cluster(caseid),data =  COX1)
#cox_lexis_model
#-----------------------------------------------#
cox_lexis_ratios <- cbind(HR = exp(coef(cox_lexis_model)), exp(confint(cox_lexis_model)))
#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out <- summary(cox_lexis_model)
cox_lexis_out <- cbind(cox_lexis_ratios,cox_lexis_out$coefficients)
#And we put everything in a nice table
#cox_lexis_out
#-----------------------------------------------#
km <- survfit(Surv(lex.dur,lex.Xst)~factor(grup),data =  COX1)

##Fit the data
##The initial statement Surv(lex.dur,lex.Xst)
#says that the duration of follow-up 
#is in lex.dur and the outcome variable in lex.Xst (as outlined above). 

#We want to stratify by grup

#-----------------------------------------------#
figura5<-ggsurvplot(km,conf.int=TRUE, legend.labs=c("No Diabético", "Diabètico"),data =  LEXIS_dt_total3_b)
#-----------------------------------------------#





#-----------------------------------------------#
cox_lexis_model2 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup),data =  COX1)
#cox_lexis_model2
cox_lexis_ratios2 <- cbind(HR = exp(coef(cox_lexis_model2)), exp(confint(cox_lexis_model2)))
cox_lexis_out2 <- summary(cox_lexis_model2)
cox_lexis_out2 <- cbind(cox_lexis_ratios2,cox_lexis_out2$coefficients)
#cox_lexis_out2
#-----------------------------------------------#


## model with robust SE via clustering
#m2 <- coxph(Surv(time1, time2, mortality) ~ age + sex + transplant + cluster(ID), 
#            data = dat)
## summary of the model
#summary(m2)

#-----------------------------------------------#
cox_lexis_model3 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+ cluster(caseid),data =  COX1)
#cox_lexis_model3
#-----------------------------------------------#
cox_lexis_ratios3 <- cbind(HR = exp(coef(cox_lexis_model3)), exp(confint(cox_lexis_model3)))
#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out3 <- summary(cox_lexis_model3)
cox_lexis_out3 <- cbind(cox_lexis_ratios3,cox_lexis_out3$coefficients)
#And we put everything in a nice table
#cox_lexis_out3
#-----------------------------------------------#

#-----------------------------------------------#
#cox_lexis_out
#cox_lexis_out2
#cox_lexis_out3
#-----------------------------------------------#

#https://rpubs.com/aniuxa/socdem1
#https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/







save(flowchart,
     T00,
     taula_events,
     taula_events2,
     table_rate,
     figura1,
     figura00_TOTAL,
     figura00_TOTAL2,
     figura02_TOTAL2,
     figura02_TOTAL3,
     cox_lexis_out,
     cox_lexis_out2,
     cox_lexis_out3,
     file="DataHarmonization.Rdata")


#http://www.sthda.com/english/wiki/cox-proportional-hazards-model
















