############################################
# D  A T A     H A R M O N I Z A T I O N   #
############################################

#[11.01.2020]

# Lectura de fitxers --------------------

# 1. Lectura de fitxers 

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

#--------------------------------------------------------------------------#
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)
#--------------------------------------------------------------------------#

# Parametres  --------------------------

# Si mostra = T llegeix MOSTRA (Altri llegeix població)

mostra<-F
if (mostra) directori_dades<-"dades/sidiap/test" else directori_dades<-"dades/sidiap"


conductor<-"conductor_DataHarmonization.xls"

# Llegir fitxers --------
LLEGIR.cmbdh_diagnostics_padris<-readRDS(directori_dades%>% here::here("DAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds")) %>% as_tibble()
variable.names(LLEGIR.cmbdh_diagnostics_padris)

#ii       [dianostics]  mult
LLEGIR.diagnostics<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_diagnostics_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.diagnostics)

#iii      [farmacs_facturats] mult
LLEGIR.farmacs_facturat<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_farmacs_facturats_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_facturat)

#iv       [farmacs_prescrits] mult
LLEGIR.farmacs_prescrits<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_farmacs_prescrits_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_prescrits)

#v        [població] unic
LLEGIR.poblacio<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_poblacio_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.poblacio)

#vi       [tabaquisme] unic
LLEGIR.tabaquisme<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_tabaquisme_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.tabaquisme)

#vii      [analitiques] mult
LLEGIR.variables_analitiques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_analitiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_analitiques)

#viii     [variables_cliíniques] mult
LLEGIR.variables_cliniques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_cliniques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_cliniques)

#ix       [variables geo_sanitàries] unic
LLEGIR.variables_geo_sanitaries<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_geo_sanitaries_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_geo_sanitaries)

#x        [variables socioeconòmiques] unic
LLEGIR.variables_socioeconomiques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_socioeconomiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_socioeconomiques)

#xi        [Catàleg]
LLEGIR.variables_Cataleg<-readRDS("dades/SIDIAP/test" %>% here::here("DAPCRMM_entregable_cataleg_20190930_093320.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_Cataleg)


# Generar data index -----------
dt_diagnostics<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

dt_diagnostics_global<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

# Llegeixo cataleg 
dt_cataleg<-read_excel("Spain_codes.xls") %>% select(cod,agr,exposed)

# Guardo N mostra
NUM_POBLACIO<-length(LLEGIR.poblacio$idp)

# Captura de casos (DM incidents entre 2006-2018)   --------------- 

# [De les dues B.D DIAGNOSTICS ---> AGAFEM DIABETIS[EXPOSED a l'excel(Spain_codes)]                  

dt_diagnostics<-select(dt_diagnostics,-agr) %>% 
  left_join(dt_cataleg,by="cod") %>% 
  filter(exposed=="exposed") %>% 
  group_by(idp)%>%mutate(data_index=min(dat,na.rm = TRUE))%>% slice(1) %>% ungroup()%>% 
  filter(data_index>=20060101  & data_index<=20181231) 
  

# Capturos primera data index (Primer diagnostic de DM entre 2006-2018)
DINDEX<-dt_diagnostics %>% select(idp,data_index)

# Afegeixo info de població sobre exposats 
C_EXPOSATS<-DINDEX %>% left_join(LLEGIR.poblacio,by="idp") %>% filter(entrada<=20181231)

# Guardo numero
C_EXPOSATS_num<-length(C_EXPOSATS$idp)

# No exposats ----------------------

# Tots els No DM exposats 
C_NO_EXPOSATS<-LLEGIR.poblacio %>% filter(entrada<=20181231) %>% anti_join(C_EXPOSATS,by="idp")

# Guardo numero 
C_NO_EXPOSATS_num<-length(C_NO_EXPOSATS$idp)


# Formateig PRE matching  ------------------
# Fusionar base de dades en dues : 
dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))





# Agregar Diagnostics en data 20051231
dt_problemes_2005<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                   bd.dindex = "20051231",
                                   dt.agregadors=select(dt_cataleg,cod,agr),
                                   finestra.dies=c(-Inf,0),prefix = "DG05.") 

# Fusiono diagnostics agregats a 2005 i recodifico 0/1
dt_matching<-dt_matching %>% left_join(dt_problemes_2005) %>% 
  select(-dtindex) %>% 
  mutate_at(vars(starts_with("DG05.") ),funs(ifelse(is.na(.),0,1))) 


# Genero filtres inicials  -------------------

# Excloc difunts anteriors a 20060101 
dt_matching<-dt_matching %>% filter(!(situacio=="D" & sortida <20060101)) 

# Filtre_pre matching 1: DM prevalents (Falta confirmar mes eliminacions?) ---------------------
dt_matching<-dt_matching %>% mutate(exclusio1_DMprev=ifelse(DG05.DM2==0 & DG05.exclude==0,0,1)) 

# Filtre_pre matching 2: per generacions :# posteriors a 84   (Massa Joves) / anteriors al 1906 (Massa grans)   -------------
dt_matching<-dt_matching %>% mutate(exclusio2_generacio=if_else(dnaix>19840101 | dnaix<19060101,1,0)) 

Nexclosos_naixament<-dt_matching %>% filter(dt_matching$dnaix>19840101 | dt_matching$dnaix<19060101) %>% count()





# Generar flow_chart Prematching i aplicar criteris exclusions ------------
flow_chart1<-criteris_exclusio_diagrama(dt=dt_matching,
                                        taulavariables=conductor,
                                        criteris = "exc_pre",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample pre matching"))


# Aplicar filtres 
dt_matching_pre<-dt_matching
dt_matching<-criteris_exclusio(dt_matching,taulavariables=conductor,criteris="exc_pre")


# Preparar matching i setriskmatching  ----------------------------
dt_matching<-dt_matching %>% transmute(idp,dnaix,sexe,grup,dtevent=data_index,entrada,sortida) %>%
  left_join(LLEGIR.variables_geo_sanitaries,by="idp")

# Generar data de sortida (Data event / Data de censura)     -----------------
dt_matching<-dt_matching %>% mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament i grups cada 10 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 

#vii [Parametres d'aparellament: llistaPS=c("sexe","any_naix","iddap")]  ---------------------

#### Parametres d'aparellament
llistaPS=c("sexe","any_naix","iddap")
num_controls<-10
llavor<-125
set.seed(llavor)
gc()

#viii)					[MATCHING 1:10]

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

gc()

# Número de controls per conjunt a risk  ------------
heaven::matchReport(dades_match, id="idp",case="grup",caseid="caseid")

# Número de controls per conjunt a risk  ------------
dades_match[,numControls:=.N,by=caseid]
dades_match<- dades_match %>% mutate(numControls=numControls-1)

table(dades_match$grup,dades_match$numControls)

# Verificació d'aparellament per edad + sexe 
# descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
# descrTable(grup~dnaix+any_naix+sexe,data=dades_match)

# Flowchart 2 pre-post matching ----------------------
dt_matching_pre<-dt_matching_pre %>% 
  left_join(transmute(dades_match,idp,exclusio3_match=0),by="idp") %>% 
  mutate(exclusio3_match=ifelse(is.na(exclusio3_match),1,0)) 


# Generar flow_chart Prematching i aplicar criteris exclusions ------------
flow_chart2<-criteris_exclusio_diagrama(dt=dt_matching_pre,
                                        taulavariables=conductor,
                                        criteris = "exc_pre2",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample pre matching"))


# Agregar base de dades aparellada i generar filtres d'exclusion POST MATCHING --------------------

flow_chart1
flow_chart2


dt_index_match<-dades_match %>% transmute(idp,iddap,caseid,grup,dnaix,sexe,dtindex=dtindex_case,numControls,entrada)%>%as_tibble()
#
bd_index<-dt_index_match %>% transmute(idp,dtindex=lubridate::as_date(dtindex))

# AgregaR  problemes de Salut -----------------

#x)   [Eliminen TOTS aquells que hagin tingut abans  DATINDEX;[CANCER,CVD,KD,MET]   ]                   

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


#xi) Filtre EDAT edat. ------------------

# [Apliquem filtre :no agafarem edats superiors a 100 anys a dtindex , ni inferiors de 35 anys]

dt_index_match<-dt_index_match %>% 
  mutate(edat_dtindex=(as_date(dtindex)-ymd(dnaix))/365.25, 
         exc_edat=ifelse(edat_dtindex>100 | edat_dtindex<35,1,0))

#xii)  Filtre Entrada Clínicia. ------
# 4. data entrada > 1 any 
dt_index_match<-dt_index_match %>% mutate(exc_antiguitat=ifelse(as_date(dtindex) - ymd(entrada)<365,1,0))


# xiii). FILTRE DE RANDOM 1:5] -----------
#[Eliminem aleatoriament aquells controls superiors a 5][1:5]             

dt_index_match<-dt_index_match%>%group_by(caseid)%>%mutate(idp2 = row_number()) %>% 
  ungroup() %>% 
  mutate(idp2=ifelse(grup==1,0,idp2)) %>%
  mutate(exc_random=ifelse(idp2>5,1,0))


# Generar exclusions apestats -------------
dt_index_match<-dt_index_match%>%mutate(exc_apestat=ifelse(exc_cancer==1  
                                                           | exc_prev_CVD==1 
                                                           | exc_prev_KD==1
                                                           | exc_prev_MET==1
                                                           | exc_edat==1 
                                                           | exc_antiguitat==1
                                                             ,1,0))

dt_index_match<-dt_index_match %>% group_by(caseid) %>% mutate(exc_apestat=max(exc_apestat))%>% ungroup()

# Si es apestat però te la patologia no es apestat -----

dt_index_match<-dt_index_match %>% mutate(exc_apestat=ifelse(exc_cancer==1  
                                                             | exc_prev_CVD==1 
                                                             | exc_prev_KD==1
                                                             | exc_prev_MET==1
                                                             | exc_edat==1 
                                                             | exc_antiguitat==1 
                                                             ,0,exc_apestat)) 



#xiv)  Casos sense controls 

# Pajarracos sense control eliminats 
dt_index_match<-dt_index_match %>% mutate(exc_0controls=ifelse(numControls==0,1,0))


# Generar flow_chart Post_matching i aplicar criteris exclusions ------------

flow_chart3<-criteris_exclusio_diagrama(dt=dt_index_match,
                                        taulavariables=conductor,
                                        criteris = "exc_post",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample post matching"))


flow_chart3



dt_post_matching<-criteris_exclusio(dt_index_match,taulavariables=conductor,criteris="exc_post")


# Actualitzar numero de controls per grup ------------- 

dt_post_matching<-dt_post_matching %>% group_by(caseid) %>% mutate(numControls=max(idp2)) %>% ungroup()


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
dt_temp<-dt_post_matching %>% transmute(idp,dtindex=lubridate::as_date(dtindex))
dtagr_variables<-agregar_analitiques(dt=dt_variables,bd.dindex=dt_temp,finestra.dies = c(-365,0))


# Agregar tabac --------------
LLEGIR.tabaquisme<-LLEGIR.tabaquisme %>% transmute(idp,cod="tabac",dat,val)
dtagr_tabac<-agregar_analitiques(dt=LLEGIR.tabaquisme,bd.dindex=dt_temp,finestra.dies = c(-Inf,0))

# Prescripcions / Facturació pendent de CODIS / AGREGADORS  -------------------

#iv   LLEGIR.farmacs_prescrits  ------------
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


#ULL HEM DE MIRAR ELS FÀRMACS PEL CONDUCTOR!

# v    LLEGIR.farmacs_facturat ------------------------
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



#ULL HEM DE MIRAR ELS FÀRMACS PEL CONDUCTOR!

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


dt_plana<-dt_post_matching %>% 
  left_join(select(LLEGIR.poblacio,idp,sortida,situacio))%>% 
    left_join(dtagr_variables,by="idp")%>%
      left_join(dtagr_tabac,by="idp")%>%
        left_join(dtagr_prescrip,by="idp")%>%
          left_join(dtagr_facturat ,by="idp")%>%
                left_join(dt_sociodemo ,by="idp")
  

# RECODES / CALCULS      -----------------------


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



# Farmacs 
dt_plana<-mutate_at(dt_plana, vars( starts_with("FF.") ), funs( if_else(.==0  | is.na(.)  ,0,1)))
dt_plana<-mutate_at(dt_plana, vars( starts_with("FP.") ), funs( if_else(.==0  | is.na(.)  ,0,1)))



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
                hide.no="No",simplify=F)



                
#-------------------------------------------------------#
T00
#-------------------------------------------------------#



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

save(dt_plana,
     flow_chart1,
     flow_chart2,
     flow_chart3,
     T00,
     taula_events,
     taula_events2,
     table_rate,
     figura1,
     figura00_TOTAL,
     cox_lexis_out,
     cox_lexis_out2,
     cox_lexis_out3,
     file="DataHarmonization.Rdata")








# S'ha de mirar la mitjana d'edat!, i tornar-ho a fer! i REFER DataHarmonization!









