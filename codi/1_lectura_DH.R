#-----------#
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

#
# Directori Font     ==============================  

library("LexisPlotR")
library(Epi)

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







######################################
#
#i)  FILTRE_1 : exposed=="exposed"
dt_diagnostics<-dt_diagnostics %>% left_join(dt_cataleg,by="cod") %>% filter(exposed=="exposed") 
#
#ii) FILTRE_2 : Filtrem la base de dades 2004-2018 *[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
dt_diagnostics<-dt_diagnostics%>% filter(dat>=20040101  & dat<=20181231)
#
######################################


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





#[1] "idp"        "data_index" "sexe"       "dnaix"      "entrada"    "sortida"    "situacio"   "Edat"  
#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una altre base de dades , que seran els No exposats,tots menys els exposats!!]
#[#Excluits entrada:abans(01/01/2004) i  després de (31/12/2018)]
C_NO_EXPOSATS<-LLEGIR.poblacio%>%filter(entrada>=20040101  & entrada<=20181231)%>%anti_join(C_EXPOSATS,by="idp")
C_NO_EXPOSATS<-C_NO_EXPOSATS 
variable.names(C_NO_EXPOSATS)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#-----------------------------------------------------------------------------------------------------------------------#
#min(LLEGIR.poblacio$entrada)-->[01.01.2006]
#max(LLEGIR.poblacio$entrada)-->[19.12.2018]


#iii) FILTRE_3 : Els  EXPOSATS a DIABETIS TIPUS2 amb DIAiNDEX que tinguin <35 anys l'any 1/1/2006, quedaran FORA!, per tant hagin nascut l'any <=1971.
C_EXPOSATS<-C_EXPOSATS%>%filter(dnaix<=19710101)




# Fusionar base de dades en dues : 

dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))

# Preparar matching i setriskmatching #
dt_matching<-dt_matching %>% transmute(idp,dnaix,sexe,grup,dtevent=data_index,sortida) %>%left_join(LLEGIR.variables_geo_sanitaries,by="idp")
dt_matching

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

table(dades_match$numControls,dades_match$grup)

# Verificació d'aparellament per edad + sexe 

descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
descrTable(grup~dnaix+any_naix+sexe,data=dades_match)
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
descrTable(grup~dnaix+sexe,data=dt_index_match)
descrTable(grup~dnaix+sexe,data=dt_matching)

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

dt_index_match 
LLEGIR.poblacio
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
  
  
dt_total2<-dt_total%>%mutate(dtindex=as_date(dtindex))

dt_total2<-dt_total2 %>% mutate(any_index=lubridate::year(lubridate::as_date(dtindex)))
dt_total2<-dt_total2 %>% mutate(agein=(as_date(dtindex)-ymd(dnaix))/365.25)
dt_total2<-dt_total2 %>% mutate(exitus=if_else(situacio=="D",1,0))
dt_total2<-dt_total2 %>% mutate(temps_FU=ymd(sortida)-as_date(dtindex))


variable.names(dt_total2)


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


# PREPARACIÓ ------------------

library(lubridate)




#dt_total


#dt_total$temps_FU

# FILTRE C.INCLUSIÓ   --------------- 


# ANALISIS  --------------

#https://rstudio-pubs-static.s3.amazonaws.com/369387_b8a63ee7e039483e896cb91f442bc72f.html



#This is a retrospective cohort study. 
#All people diagnosed with diabetes 
#between 01/01/2006 and the latest specific capture 31/12/2018
#entrada>=20060101  & entrada<=20181231




###################################################################################
# Survival 

fit<- survfit(Surv(temps_FU, exitus) ~ grup, data = dt_total2)
library("survminer")
survminer::ggsurvplot(fit,data = dt_total2)
figura1<-survminer::ggsurvplot(fit,data = dt_total2)
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




#preparació LEXIS:

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
dt_total3_10cases
dt_total3_10cases_grup0
dt_total3_10cases_grup1
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

LEXIS_dt_total3_b
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
LEXIS_dt_total3_b_grup1
#####################################################################
#Lexis.diagram()
plot(Lexis( entry = list( per=entry ),
            exit = list( per=exit, age=exit-birth ),
            exit.status = fail,
            data = dt_total3_grup1))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
plot(LEXIS_dt_total3_b_grup1, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2021), ylim=c(30,100), lwd=1, las=1 )
points(LEXIS_dt_total3_b_grup1, pch=c(NA,16)[LEXIS_dt_total3_b_grup1$fail+1] )


#figura2a.png



#grup0 (NO DIABTEIS)
#####################################################################
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_total3_b_grup0<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =    dt_total3_grup0 )
LEXIS_dt_total3_b_grup0
#####################################################################
#Lexis.diagram()
plot(Lexis( entry = list( per=entry ),
            exit = list( per=exit, age=exit-birth ),
            exit.status = fail,
            data = dt_total3_grup0))
#####################################################################
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
plot(LEXIS_dt_total3_b_grup0, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2004,2021), ylim=c(30,100), lwd=1, las=1 )
points(LEXIS_dt_total3_b_grup0, pch=c(NA,16)[LEXIS_dt_total3_b_grup0$fail+1] )


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















                  #-----------------------------------------------------#
                  # M O D E L     P O I S S O N   G L M:      DIABETIS  #
                  #-----------------------------------------------------#
#--------------------------------------------------------#
#[DIABETIS: Causes de Mortalitat]  :  LEXIS_dt_total2_b_grup1
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
#--------------------------------------------------------#
  
##########################
dbs1 <- popEpi::splitMulti( LEXIS_dt_total3_b_grup1, age = seq(35,100,1), per= seq(2004,2018,1))
dbs1
a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
##########################
#--------------------------------------------------------# 
dbs1<-dbs1 %>% left_join(select(dt_total,idp,sexe)) %>% mutate(gender=if_else(sexe=="H",1,0))
#--------------------------------------------------------#

#dbs1$lex.dur

r1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*gender,
          family = poisson(),
          offset = log(lex.dur),
          data   = dbs1)
#--------------------------------------------------------#
dbs1$lex.Xst
#--------------------------------------------------------#

#--------------------------------------------------------#
summary(r1)
# Genera una matriu amb dades i fa les prediccions segons el model ajustat
age          <- c(35:100)
period       <- seq(2004,2018,1)
gender       <- c(0:1)
nd           <- expand.grid(age, period, gender)
colnames(nd) <- c("age","per","gender")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r1, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM       <- cbind(nd,p1, out="acm")
#--------------------------------------------------------#
res_DM_DIBAETIS <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
#################
#res_DM_DIBAETIS
################
figura3<-summary(r1)
library(xlsx)
#--------------------------------------------------------#
write.xlsx(res_DM_DIBAETIS, file="MORATLITY_DIBAETIS.xlsx")
#--------------------------------------------------------#



# Journal of Statistical Software
# January 2011, Volume 38, Issue 6. http://www.jstatsoft.org/
#sing Lexis Objects for Multi-State Models in R



                #-----------------------------------------------------#
                # M O D E L     P O I S S O N   G L M: NO   DIABETIS  #
                #-----------------------------------------------------#
#--------------------------------------------------------#
#[DIABETIS: Causes de Mortalitat]  :  LEXIS_dt_total2_b_grup0
#[S'HA DE CANVIAR A 2006-2018, quan tinguem tota la base de dades!, així convergirà]
#--------------------------------------------------------#



##########################
dbs1 <- popEpi::splitMulti( LEXIS_dt_total3_b_grup0, age = seq(35,100,1), per= seq(2004,2018,1))
dbs1
a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
##########################
#--------------------------------------------------------# 
dbs1<-dbs1 %>% left_join(select(dt_total,idp,sexe)) %>% mutate(gender=if_else(sexe=="H",1,0))
#--------------------------------------------------------#
r2 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*gender,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs1)
#--------------------------------------------------------#
summary(r2)
figura4<-summary(r2)
#--------------------------------------------------------#
# Genera una matriu amb dades i fa les prediccions segons el model ajustat
age          <- c(35:100)
period       <- seq(2004,2018,1)
gender       <- c(0:1)
nd           <- expand.grid(age, period, gender)
colnames(nd) <- c("age","per","gender")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r2, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM       <- cbind(nd,p1, out="acm")
#--------------------------------------------------------#
res_DM_NO_DIBAETIS <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
#################
#res_DM_NO_DIBAETIS
################
library(xlsx)
#--------------------------------------------------------#
write.xlsx(res_DM_NO_DIBAETIS, file="MORATLITY_NO_DIBAETIS.xlsx")
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
cox_lexis_model <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+sexe+age,data =  COX1)
cox_lexis_model
#-----------------------------------------------#
cox_lexis_ratios <- cbind(HR = exp(coef(cox_lexis_model)), exp(confint(cox_lexis_model)))
#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out <- summary(cox_lexis_model)
cox_lexis_out <- cbind(cox_lexis_ratios,cox_lexis_out$coefficients)
#And we put everything in a nice table
cox_lexis_out
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
cox_lexis_model2
#-----------------------------------------------#
cox_lexis_ratios2 <- cbind(HR = exp(coef(cox_lexis_model2)), exp(confint(cox_lexis_model2)))
#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out2 <- summary(cox_lexis_model2)
cox_lexis_out2 <- cbind(cox_lexis_ratios2,cox_lexis_out3$coefficients)
#And we put everything in a nice table
cox_lexis_out2
#-----------------------------------------------#

#-----------------------------------------------#
cox_lexis_model3 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+factor(caseid),data =  COX1)
cox_lexis_model3
#-----------------------------------------------#
cox_lexis_ratios3 <- cbind(HR = exp(coef(cox_lexis_model3)), exp(confint(cox_lexis_model3)))
#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out3 <- summary(cox_lexis_model3)
cox_lexis_out3 <- cbind(cox_lexis_ratios3,cox_lexis_out3$coefficients)
#And we put everything in a nice table
cox_lexis_out3
#-----------------------------------------------#

#-----------------------------------------------#
cox_lexis_out
cox_lexis_out2
cox_lexis_out3
#-----------------------------------------------#


#             EXEMPLE:[]








                                            #########
                                            # LEXIS #
                                            #########

#https://rpubs.com/aniuxa/socdem1


#--------------------------------------------------------------------------------------#
# Exemple 2
#--------------------------------------------------------------------------------------#

# NOT RUN {
# A small bogus cohort
xcoh <- structure( list( id = c("A", "B", "C"),
                         birth = c("14/07/1952", "01/04/1954", "10/06/1987"),
                         entry = c("04/08/1965", "08/09/1972", "23/12/1991"),
                         exit = c("27/06/1997", "23/05/1995", "24/07/1998"),
                         fail = c(1, 0, 1) ),
                   .Names = c("id", "birth", "entry", "exit", "fail"),
                   row.names = c("1", "2", "3"),
                   class = "data.frame" )

xcoh

#####################################################################
xcoh$id
xcoh$birth 
xcoh$entry
xcoh$exit
xcoh$fail

#####################################################################
#Convert the character dates into numerical variables (fractional years)

xcoh <- cal.yr( xcoh, format="%d/%m/%Y", wh=2:4 )

#
#See how it looks

xcoh
str( xcoh )

#xcoh$id
#xcoh$birth 
#xcoh$entry
#xcoh$exit
#xcoh$fail

#####################################################################
# Define a Lexis object with timescales calendar time and age
Lcoh <- Lexis( entry = list(per=entry ),
               exit = list( per=exit,
                            age=exit-birth ),
               exit.status = fail,
               data = xcoh )

xcoh
# Using character states may have undesired effects:
xcoh$Fail <- c("Dead","Well","Dead")
xcoh$Fail

Lexis( entry = list( per=entry ),
       exit = list( per=exit, age=exit-birth ),
       exit.status = Fail,
       data = xcoh )
Lexis.diagram()

plot(Lexis( entry = list( per=entry ),
            exit = list( per=exit, age=exit-birth ),
            exit.status = fail,
            data = xcoh ))

#####################################################################


#https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/











#--------------------------------------#
# D E S C R I P C I Ó :
#--------------------------------------#


#canviar

#------------------------------------------------------------------#
conductor_variables<-"conductor_DataHarmonization.xls"
#------------------------------------------------------------------#





#------------------------------------------------------------------#
dt_total4<-convertir_dates(d=dt_total2,taulavariables=conductor_variables)
dt_total4<-etiquetar_valors(dt=dt_total4,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
dt_total4<-etiquetar(d=dt_total4,taulavariables=conductor_variables)
#------------------------------------------------------------------#

#variable.names(dt_total2)

#i
#***********************************************************************#
formula_taula00<-formula_compare("taula00",y="grup",taulavariables = conductor_variables)

T00<-descrTable(formula_taula00,method = 1,data=dt_total4,max.xlev = 100, show.p.overall=FALSE)
#***********************************************************************#
T00
#***********************************************************************#



save(T00,figura1,figura3,figura4,cox_lexis_out,cox_lexis_out2,cox_lexis_out3,file="DataHarmonization.Rdata")


#http://www.sthda.com/english/wiki/cox-proportional-hazards-model

