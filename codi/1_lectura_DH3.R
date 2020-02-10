############################################
# D  A T A     H A R M O N I Z A T I O N   #
############################################


# ●	Tenir 35 anys o més a DINDEX.
# ●	Tenir mínim un any d’història clínica prèvia abans de DINDEX.


# DATES dels fitxers --------------------        
#-----------#
#[10.02.2020]
#[07.02.2020]
#[06.02.2020]
#[05.02.2020]
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




# Lectura de fitxers --------------------



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

# generar_mostra_fitxers()  ----------------------

# Llegir tots els fitxers RDS dins d'un directori i generar una mostra aleatoria i salvar-lo en un directori "mostra"

# LLEGIR.cmbdh_diagnostics_padris<-readRDS("dades/SIDIAP/test" ) without 

#i        [cmbdh_diagnostics] mult

# list.files(directori_dades)

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

#[COM HO FAREM:]

#     [PART I]
#----------------------------------------------#
#0)   [De la B.D POBLACIO ho restringim a DAT<=31.12.2018]                                      :filtre0
#i)   [De les dues B.D DIAGNOSTICS ---> AGAFEM DIABETIS[EXPOSED a l'excel(Spain_codes)]         :filtre1
#ii)  [Dels DIABETIS escollits nomes agafem els que han entrat a  1.1.2006 --- 31.12.2018]      :filtre2 
#iii) [agafem la data mínima , serà la DATINDEX]
#iv)  [De la B.D POBLACIÓ , separem aquells que tinguin DIABETIS ]
#v)   [DOS GRUPS: 
#               EXPOSATS:   DIABETICS  
#               NO EXPOSATS:NO DIABETIS
#vi)  [DELS DOS GRUPS, 1.1.2006,no poden tenir mes de 100 anys, 31.12.2018, no menys de 35 anys]:filtre3
#vii) [ #### Parametres d'aparellament: llistaPS=c("sexe","any_naix","iddap")]
#viii)[MATCHING 1:10]
#----------------------------------------------#

#     [PART II]
#----------------------------------------------#
#ix)  [Tenim la DATINDEX pels 2 grups EXPOSATS(Diabetis), i NO EXPOSATS(No Diabetis)]
#x)   [Eliminen TOTS aquells que hagin tingut abans  DATINDEX;[CANCER,CVD,KD,MET]   ]           :filtre4
#xi)  [Si un dels grups 1_CAS/10_Controls hi ha un pacient eliminat , TOT EL GRUP ELIMINAT]     :filtre5
#xii) [Eliminem aleatoriament aquells controls superiors a 5][1:5]                              :filtre6
#xiii)[Eliminem aquells individus que la DATINDEX, tinguin menys de 35 anys i mes de 100 anys ] :filtre7
#xiv) [Eliminem aquells individus que la DATINDEX ,tinguin un any o menys Història Clínica]     :filtre8
#xv)  [Aquell EXPOSAT sense No Exposats, també serà ELIMINAT!]                                  :filtre9
#----------------------------------------------#





                                                  #####################
                                                  #Cohort define steps#
                                                  #####################

#1.	  From the entire database (ED), extract T2DM cohort with any codes in sheet “exposed”; 
#     then remove patients with any codes in sheet “exclude” before the end of the study (31/12/2018); 
#     use the first appearance diagnosis code of T2DM as the index date for exposed patient cohort (T2C).

#1.   De tota la base de dades (ED), extreu la cohort T2DM amb els codis del full "exposat"; 
#     després elimineu els pacients amb els codis del full "excloure" abans de finalitzar l'estudi (31/12/2018);
#     utilitzeu el codi de diagnòstic de primera aparició de T2DM com a data índex de la cohort del pacient exposada (T2C). 


#2.	  From this cohort (T2C), 
#     remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; 
#     this is the exposed cohort (EC).
#
#     D’aquesta cohort (T2C), traieu els pacients amb els codis del full “prevalent” o “càncer” 
#     que apareixin abans de la data de l’índex; es tracta de la cohort exposada (CE). 
#

#3.	  From the entire database (ED), 
#     remove patients with any codes in sheet “non-exposed pool” before the end of the study (31/12/2018), 
#     to get the candidate non-exposed patients (CNE).
#
#     De tota la base de dades (ED), elimineu els pacients amb els codis del full "piscina no exposada" 
#     abans de finalitzar l'estudi (31/12/2018) per obtenir els pacients candidats no exposats (CNE). 
#
#4.	  Exact matching the exposed cohort (EC) to the candidate non-exposed patients (CNE) with a ratio 1:10 (EC:CNE)
#     by year of birth (+/-1year), sex, and practice, 
#     without replacement (each candidate non-exposed patient can be only matched once). 
#     This is the matched cohort (MC), and the index date is the same as the matched exposed patient.
#
#     Correspondre exactament a la cohort exposada (CE)
#     als pacients candidats no exposats (CNE) amb una proporció 1:10 (EC: CNE) 
#     per any de naixement (+/- 1 any), sexe i pràctica, sense substitució 
#     ( cada pacient candidat no exposat només es pot combinar una vegada). Aquesta és la cohort coincident (MC)
#     i la data de l’índex és la mateixa que el pacient exposat igualat. 
#
#5.	  From the matched cohort (MC), remove patients died before the index date; 
#     then remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; 
#     then keep a randomly selected 5 matched non-exposed patients 
#     (good to set a seed to make the random selection replicable). This is the final non-exposed cohort (FNE).
#--------------------------------------------------------------------------------------------------------
#     De la cohort aparellada (MC), elimineu els pacients morts abans de la data de l’índex; 
#     a continuació, elimineu els pacients amb qualsevol codi en el full "prevalent" o "càncer" 
#     que aparegui abans de la data de l'índex; a continuació, 
#     mantingueu a 5 pacients no exposats seleccionats aleatòriament
#     (és bo establir una llavor perquè la selecció aleatòria sigui replicable). 
#     Es tracta de la cohort final no exposada (FNE).
#--------------------------------------------------------------------------------------------------------
#6.	  The final study cohort is the combination of the exposed cohort (EC) and the final non-exposed cohort (FNE).
#--------------------------------------------------------------------------------------------------------
#     La cohort d'estudi final és la combinació de la cohorte exposada (EC) i la cohort final no exposada (FNE)
#--------------------------------------------------------------------------------------------------------

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
#    [De les dues B.D DIAGNOSTICS ---> AGAFEM DIABETIS[EXPOSED a l'excel(Spain_codes)]                    :filtre1
######################################


dt_diagnostics<-dt_diagnostics %>% left_join(dt_cataleg,by="cod") %>% filter(exposed=="exposed") 


######################################
#ii) FILTRE_2 : Filtrem la base de dades 2006-2018 
#   [Dels DIABETIS escollits nomes agafem els que han entrat a  1.1.2006 --- 31.12.2018]                  :filtre2 
dt_diagnostics<-dt_diagnostics%>% filter(dat>=20060101  & dat<=20181231)
######################################


gc()

#busquem la DATA ÍNDEX!:[]

######################################
#iii) [agafem la data mínima , serà la DATINDEX]
######################################

#data minima[data Índex!!!]#
DINDEX<-dt_diagnostics%>% group_by(idp)%>%summarise(data_index=min(dat,na.rm = TRUE))%>%ungroup()
#DINDEX


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

#0)  De la B.D POBLACIO ho restringim a DAT<=31.12.2018]                           :filtre0
C_NO_EXPOSATS<-LLEGIR.poblacio%>%filter(entrada<=20181231)%>%anti_join(C_EXPOSATS,by="idp")

variable.names(C_NO_EXPOSATS)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#-----------------------------------------------------------------------------------------------------------------------#
#min(LLEGIR.poblacio$entrada)-->[01.01.2006]
#max(LLEGIR.poblacio$entrada)-->[19.12.2018]
C_NO_EXPOSATS_num<-length(C_NO_EXPOSATS$idp)
#C_NO_EXPOSATS_num




# PREPARACIÓ ------------------

######################################
#iv)  [De la B.D POBLACIÓ , separem aquells que tinguin DIABETIS ]
######################################

######################################
#v)   [DOS GRUPS: 
#               EXPOSATS:   DIABETICS  
#               NO EXPOSATS:NO DIABETIS
######################################
# Fusionar base de dades en dues : 
dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))


######################################
#vi)  [DELS DOS GRUPS, 1.1.2006,no poden tenir mes de 100 anys, 31.12.2018, no menys de 35 anys]           :filtre3
######################################

#     dt_matching$dnaix>19840101 # posteriors a 84    (Massa Joves)
#     dt_matching$dnaix<19060101 # posteriors al 1906 (Massa grans)

Nexclosos_naixament<-dt_matching %>% filter(dt_matching$dnaix>19840101 | dt_matching$dnaix<19060101) %>% count()

dt_matching<-dt_matching%>%filter(dnaix<=19840101 & dnaix>=19060101)

# C_EXPOSATS2_num<-length(C_EXPOSATS$idp)


# Preparar matching i setriskmatching #
dt_matching<-dt_matching %>% transmute(idp,dnaix,sexe,grup,dtevent=data_index,entrada,sortida) %>%
  left_join(LLEGIR.variables_geo_sanitaries,by="idp")
#dt_matching



#   5.2.1 Generar data de sortida (Data event / Data de censura)     -----------------
## dtindex_case 
dt_matching<-dt_matching %>% mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament i grups cada 10 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 

######################################
#vii) [ #### Parametres d'aparellament: llistaPS=c("sexe","any_naix","iddap")]
######################################

#birth (+/-1year), sex, and practice, 

#### Parametres d'aparellament
llistaPS=c("sexe","any_naix","iddap")
num_controls<-10
llavor<-125
set.seed(llavor)

gc()

#5188815

table(dt_matching$grup)



######################################
#viii)					[MATCHING 1:10]
######################################

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

# ---------------------------------------------------------------------------------------------#
table(dades_match$grup,dades_match$numControls)
# ---------------------------------------------------------------------------------------------#

# Verificació d'aparellament per edad + sexe 

#descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
#descrTable(grup~dnaix+any_naix+sexe,data=dades_match)



# ---------------------------------------------------------------------------------------------#
# Preparo dt_index_match per

#[10.2.2020]







######################################
#ix)  [Tenim la DATINDEX pels 2 grups EXPOSATS(Diabetis), i NO EXPOSATS(No Diabetis)]
######################################
  
dt_index_match<-dades_match %>% transmute(idp,iddap,caseid,grup,dnaix,sexe,dtindex=dtindex_case,numControls,entrada)%>%as_tibble()
#
bd_index<-dt_index_match %>% transmute(idp,dtindex=lubridate::as_date(dtindex))
# Agrego problemes de Salut



######################################
#x)   [Eliminen TOTS aquells que hagin tingut abans  DATINDEX;[CANCER,CVD,KD,MET]   ]                   
######################################

#FILTRES DE ANTECEDENTS
#[FILTRE1A : Antacedents de DG.Cancer,CVD,KD,MET =1]
#[FILTRE1B : Aquells a on algun element del Grup(caseid) tingui Antecedents! ]


dt_agregada_agr<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                   bd.dindex = bd_index,
                                   dt.agregadors=select(dt_cataleg,cod,agr))

dt_index_match <-dt_index_match %>% 
  left_join(select(dt_agregada_agr,-dtindex),by="idp") %>% 
  mutate(exc_cancer=ifelse(is.na(DG.cancer),0,1),
         exc_prev_CVD=ifelse(is.na(DG.prevalent_CVD),0,1),
         exc_prev_KD=ifelse(is.na(DG.prevalent_KD),0,1),
         exc_prev_MET=ifelse(is.na(DG.prevalent_MET),0,1))

dt_index_match<-dt_index_match%>%mutate(FILTRE_EXC_1A=ifelse(exc_cancer==1  
                                                        | exc_prev_CVD==1 
                                                        | exc_prev_KD==1
                                                        | exc_prev_MET==1 ,1,0)) 
dt_index_match<-dt_index_match %>% group_by(caseid) %>% mutate(FILTRE_EXC_1B=max(FILTRE_EXC_1A))%>% ungroup()
#

######################################
#xi)  Filtre EDAT edat. 
# Apliquem filtre :no agafarem edats superiors a 100 anys a dtindex , ni inferiors de 35 anys.
######################################

dt_index_match<-dt_index_match %>% 
  mutate(edat_dtindex=(as_date(dtindex)-ymd(dnaix))/365.25, 
         FILTRE_EXC_2A=ifelse(edat_dtindex>100 | edat_dtindex<35,1,0))

dt_index_match<-dt_index_match %>% group_by(caseid) %>% mutate(FILTRE_EXC_2B=max(FILTRE_EXC_2A))%>% ungroup()



######################################
#xii)  Filtre Entrada Clínicia. 
# 4. data entrada > 1 any 
dt_index_match<-dt_index_match %>% mutate( FILTRE_EXC_3A=ifelse(as_date(dtindex) - ymd(entrada)<365,1,0))
dt_index_match<-dt_index_match %>% group_by(caseid) %>% mutate(FILTRE_EXC_3B=max(FILTRE_EXC_3A))%>% ungroup()


######################################
# xiii). FILTRE DE RANDOM 1:5]
#[Eliminem aleatoriament aquells controls superiors a 5][1:5]             
# Seleccionar com a molt 5 No exposats ---------
#---------------------------------------------------------------------------------------------#
dt_index_match<-dt_index_match%>%group_by(caseid)%>%mutate(idp2 = row_number())%>%ungroup()
dt_index_match<-dt_index_match%>%mutate(idp2=ifelse(grup==1,0,idp2))
#---------------------------------------------------------------------------------------------#
dt_index_match<-dt_index_match%>%mutate(FILTRE_EXC_4A=ifelse(idp2>5,1,0))

# dt_index_match<-dt_index_match %>% group_by(caseid) %>% mutate(FILTRE_EXC_4B=max(FILTRE_EXC_4A))%>% ungroup()
#---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#
#Filtre_Normal!!!
dt_index_match<-dt_index_match%>%mutate(FILTRE_FINAL=ifelse(FILTRE_EXC_1A==0 & 
                                                            FILTRE_EXC_2A==0 &
                                                            FILTRE_EXC_3A==0 &
                                                            FILTRE_EXC_4A==0 ,1,0)) 
#---------------------------------------------------------------------------------------------#
#Filtre_Apestats!!!
dt_index_match<-dt_index_match%>%mutate(FILTRE_FINAL2=ifelse(FILTRE_EXC_1B==0 & 
                                                              FILTRE_EXC_2B==0 &
                                                              FILTRE_EXC_3B==0
                                                             ,1,0)) 

#---------------------------------------------------------------------------------------------#

criteris_exclusio_diagrama(dt_index_match,
                           "conductor_exclusions2A.xls",
                           criteris = "exclusio",grups="grup",
                           etiquetes="lab_exclusio",
                           ordre="ordre",sequencial = T,
                           pob_lab=c("Població generació:1906-1984","Mostra aparellada final"))

criteris_exclusio_diagrama(dt_index_match,
                           "conductor_exclusions2B.xls",
                           criteris = "exclusio",grups="grup",
                           etiquetes="lab_exclusio",
                           ordre="ordre",sequencial = T,
                           pob_lab=c("Població generació Apestada:1906-1984","Mostra aparellada final"))






# Apliquem filtre1 
dt_index_match<-dt_index_match %>% filter(FILTRE_FINAL==1)


# Apliquem filtre2 
#dt_index_match<-dt_index_match %>% filter(FILTRE_FINAL2==1)


######################################
#xiv)  [Aquell EXPOSAT sense No Exposats, també serà ELIMINAT!]                                             
######################################
# Pajaracos sense control eliminats ()

dt_temp<-dt_index_match %>%
  mutate (grup2=ifelse(grup==0,2,1))%>%
    group_by(caseid) %>%
     mutate(cas_control=max(grup2))%>% 
        ungroup()%>% 
          select(idp,cas_control)

dt_index_match<-dt_index_match %>% left_join(dt_temp,by="idp") %>% 
  mutate(FILTRE_EXC_5A=ifelse(cas_control==1,1,0)) 



#---------------------------------------------------------------------------------------------#
# Apliquem filtre2 [Aquell EXPOSAT sense No Exposats, també serà ELIMINAT!]
dt_index_match<-dt_index_match %>% filter(FILTRE_EXC_5A==0)
#---------------------------------------------------------------------------------------------#

dt_index_match<-dt_index_match%>%
  group_by(caseid)%>%
    mutate(num_controls2= row_number())%>%ungroup()

dt_index_match<-dt_index_match%>%mutate(num_controls2=ifelse(grup==1,0,num_controls2))



#abans
# ---------------------------------------------------------------------------------------------#
table(dades_match$grup,dades_match$numControls)
# ---------------------------------------------------------------------------------------------#

#després filtres!
# ---------------------------------------------------------------------------------------------#
table(dt_index_match$grup,dt_index_match$num_controls2)
# ---------------------------------------------------------------------------------------------#
table(dt_index_match$grup)
# 

# Verificació d'aparellament per edad + sexe 
#descrTable(grup~dnaix+sexe,data=dt_index_match)
#descrTable(grup~dnaix+sexe,data=dt_matching)

#variable.names(dt_index_match)

#OBSERVAR:[]--> 

   
#"FILTRE_EXC_1A"    
#"FILTRE_EXC_1B"
#"FILTRE_EXC_2A"    
#"FILTRE_EXC_2B"   
#"FILTRE_EXC_3A"    
#"FILTRE_EXC_3B"  
#"FILTRE_EXC_4A"    
#"FILTRE_EXC_4B"   
#"FILTRE_FINAL"
#FILTRE_EXC_5A"    
#"num_controls2" 

#criteris_exclusio_diagrama(dt_index_match,
#                           "conductor_exclusions.xls",
#                           criteris = "exclusio",grups="grup",
#                           etiquetes="lab_exclusio",
#                           ordre="ordre",sequencial = T,
#                           pob_lab=c("Població generació:1906-1984","Mostra aparellada final"))


# funciona!!!


#criteris_exclusio_diagrama





















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
#??????????????????????????????????????????????????????????????????????? ULL
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.ALFAGLUC=case_when(FP.ALFAGLUC>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.ALT_GLUC=case_when(FP.ALT_GLUC>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.BIGUANIDAS=case_when(FP.BIGUANIDAS>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.COMB_GLUC=case_when(FP.COMB_GLUC>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.DPP4 =case_when(FP.DPP4 >=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.GLINIDES=case_when(FP.GLINIDES>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.GLP1=case_when(FP.GLP1>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.INSULINAS=case_when(FP.INSULINAS>=1 ~ 1,TRUE~0))
#dtagr_prescrip<-dtagr_prescrip%>%mutate(FP.SULFO=case_when(FP.SULFO  >=1 ~ 1,TRUE~0))
# ----------------------------------------------------------#
dtagr_prescrip<-mutate_at(dtagr_prescrip, vars( starts_with("FP.") ), funs( if_else(.==0  | is.na(.)  ,0,1)))

#ULL HEM DE MIRAR ELS FÀRMACS PEL CONDUCTOR!


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
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.ALFAGLUC=case_when(FF.ALFAGLUC>=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.ALT_GLUC =case_when(FF.ALT_GLUC >=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.BIGUANIDAS=case_when(FF.BIGUANIDAS>=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.COMB_GLUC=case_when(FF.COMB_GLUC>=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.DPP4  =case_when(FF.DPP4  >=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.GLINIDES=case_when(FF.GLINIDES>=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.GLP1 =case_when(FF.GLP1 >=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.INSULINAS =case_when(FF.INSULINAS>=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.SGLT2  =case_when(FF.SGLT2 >=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.SULFO  =case_when(FF.SULFO  >=1 ~ 1,TRUE~0))
#dtagr_facturat<-dtagr_facturat%>%mutate(FF.TIAZO  =case_when(FF.TIAZO  >=1 ~ 1,TRUE~0))
dtagr_facturat<-mutate_at(dtagr_facturat, vars( starts_with("FF.") ), funs( if_else(.==0  | is.na(.)  ,0,1)))

#ULL HEM DE MIRAR ELS FÀRMACS PEL CONDUCTOR!



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

#Fem la Taula Final:[PLana]


dt_plana<-dt_index_match %>% 
  left_join(select(LLEGIR.poblacio,idp,sortida,situacio))%>% 
    left_join(dtagr_variables,by="idp")%>%
      left_join(dtagr_tabac,by="idp")%>%
        left_join(dtagr_prescrip,by="idp")%>%
          left_join(dtagr_facturat ,by="idp")%>%
                left_join(dt_sociodemo ,by="idp")
  

#canvis :[]
  
dt_plana<-dt_plana%>%mutate(dtindex=as_date(dtindex))
dt_plana<-dt_plana %>% mutate(any_index=lubridate::year(lubridate::as_date(dtindex)))
dt_plana<-dt_plana %>% mutate(agein=(as_date(dtindex)-ymd(dnaix))/365.25)
dt_plana<-dt_plana %>% mutate(exitus=if_else(situacio=="D",1,0))
dt_plana<-dt_plana %>% mutate(temps_FU=ymd(sortida)-as_date(dtindex))
dt_plana<-dt_plana %>% mutate(temps_FU2=(ymd(sortida)-as_date(dtindex))/365.25)
dt_plana<-dt_plana %>% mutate(agein2=as.numeric(as_date(dtindex)-ymd(dnaix))/365.25)
dt_plana$dnaix<-as.character(dt_plana$dnaix)
dt_plana$any_index2<-as.character(dt_plana$any_index)

#recodificació1:
dt_plana<-dt_plana%>%mutate(agein3.cat6=case_when(  agein2<40~ 1,
                                                    agein2>=40 & agein2<50 ~ 2,  
                                                    agein2>=50 & agein2<60 ~ 3,
                                                    agein2>=60 & agein2<70 ~ 4,
                                                    agein2>=70  ~ 5))


#recodificació2:
dt_plana<-dt_plana%>%mutate(IMC.valor2=case_when(   IMC.valor   <15~ 1,
                                                    IMC.valor   >=15 & IMC.valor   <25 ~ 2,  
                                                    IMC.valor   >=25 & IMC.valor   <30 ~ 3,
                                                    IMC.valor   >=30  ~ 4))







# ANALISIS  --------------

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

#----------------------------------------------------------------#
conductor_variables<-"conductor_DataHarmonization.xls"
#------------------------------------------------------------------#
dt_plana<-recodificar(dt_plana,taulavariables = conductor_variables,"recode",missings = T)
variable.names(dt_plana)
#dt_total2$agein2
#------------------------------------------------------------------#

#variable.names(dt_plana)


#apliquem esl conductor!:[ull!]
dt_plana2<-dt_plana



#------------------------------------------------------------------#
dt_plana2<-convertir_dates(d=dt_plana2,taulavariables=conductor_variables)
dt_plana2<-etiquetar_valors(dt=dt_plana2,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
dt_plana2<-etiquetar(d=dt_plana2,taulavariables=conductor_variables)
#------------------------------------------------------------------#





#i
#-------------------------------------------------------#
formula_taula00<-formula_compare("taula00",y="grup",taulavariables = conductor_variables)

T00<-descrTable(formula_taula00,
                method = c(IMC.valor=2,temps_FU = 2,temps_FU2 = 2,agein=2,any_index=2),
                data=dt_plana2,
                max.xlev = 100, 
                show.p.overall=FALSE,
                show.n = T,
                hide.no="No")
                
#-------------------------------------------------------#
T00
#-------------------------------------------------------#


 

#-------------------------------------------------------#
# FLOWCHART.
#-------------------------------------------------------#

#paràmetres per fer el Flowchart!:
#-------------------------------------------------------#
pob=NUM_POBLACIO
pob_lab=c("Mortalidad en personas Diabéticas Tipo 2 en comparación en Población Control[MUESTRA]")
C_EXPOSATS_MATCHED_num<-dt_plana%>%filter(grup==1)
C_EXPOSATS_MATCHED_num<-length(C_EXPOSATS_MATCHED_num$idp)
C_NO_EXPOSATS_MATCHED_num<-dt_plana%>%filter(grup==0)
C_NO_EXPOSATS_MATCHED_num<-length(C_NO_EXPOSATS_MATCHED_num$idp)
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
dt_plana_num<-length(dt_plana$grup)
#-------------------------------------------------------##
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
#-------------------------------------------------#
flowchart
#-------------------------------------------------#


#iii)
#Tasa de Mortalitat amb  Kaplan-Meier [diabetic/ no-diabètics]



rescox<-coxph(Surv(temps_FU,exitus)~grup,data=dt_plana)    

#--------------------------------------------------#
N=rescox$n
EVENTS=rescox$nevent
coef=summary(rescox)$coef[1,1]
HR=summary(rescox)$coef[1,2]
IC951=summary(rescox)$conf.int[1,3]
IC952=summary(rescox)$conf.int[1,4]
se.coef=summary(rescox)$coef[1,3]
p=summary(rescox)$coef[1,5]
#--------------------------------------------------#
dt_plana$temps_FU<-as.numeric(dt_plana$temps_FU)
#--------------------------------------------------#
taula_events<-dt_plana %>% group_by(grup,sexe) %>% summarise(
  events=sum(exitus), 
  N=n(),
  PTdies=sum(temps_FU),
  PTanys=(sum(temps_FU)/365.25) ,
  taxa=(events/PTanys)*1000) %>% 
  ungroup()
#--------------------------------------------------#
#taula_events
conductor_variables<-"conductor_DataHarmonization.xls"
taula_events<-etiquetar_valors(dt=taula_events,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
#-------------------------------------------------------#
taula_events2<-dt_plana%>% group_by(grup) %>% summarise(
  events=sum(exitus), 
  N=n(),
  PTdies=sum(temps_FU),
  PTanys=(sum(temps_FU)/365.25) ,
  taxa=(events/PTanys)*1000) %>% 
  ungroup()
#-------------------------------------------------------#
conductor_variables<-"conductor_DataHarmonization.xls"
taula_events2<-etiquetar_valors(dt=taula_events2,variables_factors=conductor_variables,fulla="etiquetes",camp_etiqueta="etiqueta2")
#-------------------------------------------------------#
#[Taula de TAXES!]#
taula_events
taula_events2
#-------------------------------------------------------#



#iv) #GRAFIC  Tasa de Mortalitat amb  Kaplan-Meier [diabetic/ no-diabètics]
 

#-------------------------------------------------------#
# Survival 
fit<- survfit(Surv(temps_FU, exitus) ~ grup, data = dt_plana)
library("survminer")
survminer::ggsurvplot(fit,data = dt_plana)
figura1<-survminer::ggsurvplot(fit,data =dt_plana)
figura1
#-------------------------------------------------------#







#-------------------------------------------------------
#v)


#############
#L  E X I S #
#############

##Part descriptiva  inicial!!!
#mirar LEXIS!!!
#::: preparció LEXIS:[]
#variable.names(dt_total)


#Preparació LEXIS:
#-------------------------------------------------------------------------------------
dt_plana<-dt_plana%>%mutate(dtindex=as_date(dtindex))
dt_plana<-dt_plana%>%mutate(dnaix=as.Date(as.character(dnaix),    format="%Y%m%d"))
dt_plana<-dt_plana %>%mutate(sortida=as.Date(as.character(sortida),format = "%Y%m%d"))
#-------------------------------------------------------------------------------------

dt_plana_Lex<-dt_plana%>%select(idp,dtindex,dnaix,sortida,exitus,grup,caseid)
#-------------------------------------------------------------------------------------
dt_plana_Lex<-dt_plana_Lex%>%mutate(birth =dnaix)
dt_plana_Lex<-dt_plana_Lex%>%mutate(entry =dtindex)
dt_plana_Lex<-dt_plana_Lex%>%mutate(exit =sortida)
dt_plana_Lex<-dt_plana_Lex%>%mutate(fail =exitus)
#-------------------------------------------------------------------------------------
dt_plana_Lex<-dt_plana_Lex%>%select(idp,birth,entry,exit,fail,grup,caseid)
#-------------------------------------------------------------------------------------
dt_plana_Lex<-structure(dt_plana_Lex,class = "data.frame")
dt_plana_Lex <- cal.yr(dt_plana_Lex, format="%y-%m-%d", wh=2:4 )
#-------------------------------------------------------------------------------------
dt_plana_Lex_grup0<-dt_plana_Lex %>% filter(grup==0)
dt_plana_Lex_grup1<-dt_plana_Lex %>% filter(grup==1)

                                               
#-------------------------------------------------------------------------------------
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana_Lex<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =  dt_plana_Lex )
#-------------------------------------------------------------------------------------
#grup1 (DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana_Lex_grup1<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =     dt_plana_Lex_grup1 )
#gravem la figura!
png('figura2a.png')
plot(LEXIS_dt_plana_Lex_grup1, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2006,2018), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_plana_Lex_grup1, pch=c(NA,16)[LEXIS_dt_plana_Lex_grup1$fail+1] )
dev.off()
#-------------------------------------------------------------------------------------
#grup0 (NO DIABTEIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana_Lex_grup0<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =    dt_plana_Lex_grup0 )
png('figura2b.png')
plot(LEXIS_dt_plana_Lex_grup0, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2006,2018), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_plana_Lex_grup0, pch=c(NA,16)[LEXIS_dt_plana_Lex_grup0$fail+1] )
dev.off()
#-------------------------------------------------------------------------------------



#vi)
#TAXA BRUTA! 
#-----------------------------------------------
#Tasa_de_Mort_1000_Personas_Año_Diabetico
#Tasa_de_Mort_1000_Personas_Año_No_Diabetico
#-----------------------------------------------

#----------------------------------------------------------------------------------------------------#
#Raw mortality by calendar year
#----------------------------------------------------------------------------------------------------#
S1 <- splitLexis(LEXIS_dt_plana_Lex, time.scale="per", breaks=2006+seq(0,14,1) )
S1<-S1%>%left_join(select(dt_plana,idp,grup))%>%mutate(grup2=(if_else(grup==1,"Diabético","No Diabético")))   
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
#table_rate



#[grafica7.png]:-> Taxa Bruta

png("grafica7.png")
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

dev.off()


#vii) 

#####
#COX#
#####

#TASAS CON EL MODELO DE COX. 


COX1<-LEXIS_dt_plana_Lex%>% left_join(select(dt_plana,idp,sexe)) %>% mutate(gender=if_else(sexe=="H",1,0))
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



#-----------------------------------------------#
figura5<-ggsurvplot(km,conf.int=TRUE, legend.labs=c("No Diabético", "Diabètico"),data =  LEXIS_dt_plana_Lex)
#-----------------------------------------------#





#-----------------------------------------------#
cox_lexis_model2 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup),data =  COX1)


cox_lexis_ratios2 <- cbind(HR = exp(coef(cox_lexis_model2)), exp(confint(cox_lexis_model2)))
cox_lexis_out2 <- summary(cox_lexis_model2)
cox_lexis_out2 <- cbind(cox_lexis_ratios2,cox_lexis_out2$coefficients)
#cox_lexis_out2
#-----------------------------------------------#



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











#VIII

                      #--------------------------------------------------#
                      # M O D E L     P O I S S O N   G L M   GLOBAL     #
                      #--------------------------------------------------#


# A partir dels Models Poisson, aplicarem les gràfiques suavitzades de les nostres Taxes , 
# en funcio del periòde o de l'edat!.


                  #-------------------------------------------------------------------#
                  # M O D E L     P O I S S O N   G L M:Moratlitat=Periodo*Diabetis   #
                  #-------------------------------------------------------------------#

#--------------------------------------------------------#
dbs00 <- popEpi::splitMulti(LEXIS_dt_plana_Lex, per= seq(2006,2018,1))
dbs00<-dbs00 %>%left_join(select(dt_plana,idp,grup))
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
#grafica1.png
#--------------------------------------------------------#
#NO DIABÈTICS!
#--------------------------------------------------------#
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000)
#--------------------------------------------------------#
#ci.pred(r0)

png("grafica1.png")
matplot( nd00$per,ci.pred(r00, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
#--------------------------------------------------------#
dev.off()
#grafica2.png

#DIABÈTICS!
#--------------------------------------------------------#
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000)
#--------------------------------------------------------#
#ci.pred(r0)

png("grafica2.png")
matplot( nd01$per,ci.pred(r00, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018  Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
#--------------------------------------------------------#
dev.off()






          #-------------------------------------------------------------------#
          # M O D E L     P O I S S O N   G L M:Moratlitat=AGE*Diabetis       #
          #-------------------------------------------------------------------#

#MODEL POISSON AGE*GRUP!
#--------------------------------------------------------#
dbs01 <- popEpi::splitMulti(LEXIS_dt_plana_Lex, age= seq(35,100,1))
dbs01<-dbs01 %>%left_join(select(dt_total,idp,grup))
#--------------------------------------------------------#
a.kn <- with(subset(dbs01, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
#--------------------------------------------------------#

r01<- glm((lex.Xst==1)~Ns( age, knots=a.kn)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs01 )
#--------------------------------------------------------#
figura00_TOTAL2<-summary(r01)
figura00_TOTAL2


#grafica3.png
#NO DIABÈTICS!
#--------------------------------------------------------#
nd00<- data.frame(age=35:100,grup=0,lex.dur=1000)
#--------------------------------------#
#ci.pred(r0)

png("grafica3.png")
matplot( nd00$age,ci.pred(r01, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Edad", 
         las=1, 
         ylim=c(1,1000) )
rug( a.kn , lwd=2 )
dev.off()

#grafica4.png
#DIABÈTICS!
#--------------------------------------------------------#
nd00<- data.frame(age=35:100,grup=1,lex.dur=1000)
#--------------------------------------#
#ci.pred(r0)

png("grafica4.png")
matplot( nd00$age,ci.pred(r01, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018  N Diabéticos cada  1000 Personas-año ", 
         xlab="Edad", 
         las=1, 
         ylim=c(1,1000) )
rug( a.kn , lwd=2 )
dev.off()







        #---------------------------------------------------------------------------#
        # M O D E L     P O I S S O N   G L M:Moratlitat=AGE*PERIODO*Diabetis       #
        #---------------------------------------------------------------------------#
#-----------------#
mean(dt_plana$agein2)
#-----------------#

#Edat mitjana de la Població que estudiem: [54.55175 --> 55 anys]

#--------------------------------------------------------#
dbs1 <- popEpi::splitMulti(LEXIS_dt_plana_Lex, age = seq(35,100,1), per= seq(2006,2018,1))
dbs1<-dbs1%>%left_join(select(dt_plana,idp,grup))
#--------------------------------------------------------#
a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#--------------------------------------------------------#
#--------------------------------------#
r02 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs1 )
figura02_TOTAL2<-summary(r02)
figura02_TOTAL2



#grafica5.png
#--------------------------------------------------------#
#NO DIABÈTICS! MODEL producte mitjana:55 anys
#--------------------------------------------------------#
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000,age=55)
#--------------------------------------------------------#
#ci.pred(r0)

png("grafica5.png")
matplot( nd00$per,ci.pred(r02, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 55 años Edad  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
#--------------------------------------------------------#
dev.off()
#grafica6.png
#DIABÈTICS! MODEL producte mitjana:55 anys
#--------------------------------------------------------#
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000,age=55)
#--------------------------------------------------------#
#ci.pred(r0)
png("grafica6.png")
matplot( nd01$per,ci.pred(r02, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 55 años   Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
#--------------------------------------------------------#
dev.off()




        #---------------------------------------------------------------------------#
        # M O D E L     P O I S S O N   G L M:Moratlitat=AGE+PERIODO+Diabetis       #
        #---------------------------------------------------------------------------#



#--------------------------------------#
r03 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)+Ns(per, knots = p.kn)+grup,
           family = poisson,
           offset = log(lex.dur),
           data   = dbs1 )
figura02_TOTAL3<-summary(r03)
figura02_TOTAL3
#-------------------------------------------------------------------------------------------#


#--------------------------------------------------------#
#NO DIABÈTICS! aditiu MODEL mitjana:55 anys
#--------------------------------------------------------#
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000,age=55)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd00$per,ci.pred(r03, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 55 años Edad  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
#--------------------------------------------------------#
#DIABÈTICS! aditiu MODEL  mitjana:55 anys
#--------------------------------------------------------#
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000,age=55)
#--------------------------------------------------------#
#ci.pred(r0)
matplot( nd01$per,ci.pred(r03, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ 55 años   Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
#--------------------------------------------------------#





#[prediccions taxes producte triple!]
                         
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




#https://rpubs.com/aniuxa/socdem1
#https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/
#http://www.sthda.com/english/wiki/cox-proportional-hazards-model
#https://rstudio-pubs-static.s3.amazonaws.com/369387_b8a63ee7e039483e896cb91f442bc72f.html
#http://bendixcarstensen.com/SDC/EPJmort/MortT2.pdf

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








# S'ha de mirar la mitjana d'edat!, i tornar-ho a fer! i REFER DataHarmonization!









