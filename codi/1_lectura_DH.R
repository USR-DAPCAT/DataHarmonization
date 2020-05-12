#####################
# 12.05.2020
# Lectura de fitxers 

# rm(list = ls())


# libreries i Funcions  ------------------------------
library("dplyr")
library("magrittr")
library("mschart")
library("officer")
library("stats")
library("lubridate")
library("here")


# library("pander")
# library("devtools")
#devtools::install_version("igraph", version = "1.2.4.2")
#devtools::install_version("DiagrammeR", version = "1.0.1")
#githubinstall("heaven",ref="964bbbd",force=T) # "2018.8.9"


# Parametres  --------------------------
# Si no existeix el parametres conductuals
if (exists("parametres_conductuals")==FALSE) {
  rm(list = ls())
  mostra<-T
  conductor<-"conductor_DataHarmonization.xlsx"
  }

# Descarregar funcions github -
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# mostra<-F
if (mostra) directori_dades<-"dades/sidiap/test" else directori_dades<-"dades/sidiap"

# Crear directoris test i output
dir.create(file.path("output_test"))
dir.create(file.path("output"))
dir.create(file.path("images"))
dir.create(file.path("images_test"))
dir.create(file.path("dades_test"))


# Llegir parametrs  --------
source("codi/funcio_parametre.R")
parametres<-parametres_directori(mostra)
dir_dades<-parametres$dir_dades
dir_output<-parametres$dir_output

# Llegir fitxers --------

source("codi/global_lectura.R")


# Exclusions d'inici   --------------------------------------------
# Agrego a 2005 per excloure a inici
dt_problemes_2005<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                     bd.dindex = "20051231",
                                     dt.agregadors=select(dt_cataleg,cod,agr),
                                     finestra.dies=c(-Inf,0),prefix = "DG05.") 
# Exclosos d'inici per Problemes de salut
exclosos_inici<-dt_problemes_2005 %>% 
  filter(DG05.cancer>0 | DG05.DM2>0 | DG05.prevalent_CVD>0 | DG05.prevalent_KD>0 | DG05.prevalent_MET>0) %>% select(idp)

# Excloure de tots els historics de pacients exclosos 2005 (Antijoin amb pacients exclosos_inici)

llistaNomenada(LLEGIR.poblacio,
               LLEGIR.tabaquisme,
               LLEGIR.variables_geo_sanitaries,
               LLEGIR.variables_analitiques,
               LLEGIR.variables_cliniques,
               LLEGIR.diagnostics,
               LLEGIR.cmbdh_diagnostics_padris,
               LLEGIR.farmacs_facturat,
               LLEGIR.farmacs_prescrits,
               LLEGIR.variables_socioeconomiques) %>% 
  map(~anti_join(.x,exclosos_inici,by="idp")) %>% 
  list2env(globalenv())


# Ecloure morts anterior a 2006
LLEGIR.poblacio<-LLEGIR.poblacio %>% filter(!(situacio=="D" & sortida<20051231))


# Fusiono dt_diagnostics (E-CAP + hospital )
dt_diagnostics<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

# Copia
dt_diagnostics_global<-dt_diagnostics


# Generar data index  (Primer Diagnostic registrat de DM / primera prescripció/facturacio AD) -----------

dtagr_prescrip_DIABET<-agregar_prescripcions(
  dt=LLEGIR.farmacs_prescrits,
  dt.agregadors=select(cataleg_antidiab,cod,agr),
  prefix="FP.",
  bd.dindex=20181231,
  finestra.dies=c(-Inf,0),
  camp_agregador="agr",
  agregar_data=T) %>% 
  transmute(idp,data_index=data.to.string(FP.AD) %>% as.numeric())

# dtagr_prescrip_DIABET
#min(dtagr_prescrip_DIABET$ data_index)

# Facturació pendent de CODIS / AGREGADORS 
LLEGIR.farmacs_facturat<-LLEGIR.farmacs_facturat %>% transmute(idp,cod,dat,env)
#
dtagr_facturat_DIABET<-agregar_facturacio(
  dt=LLEGIR.farmacs_facturat,
  bd.dindex="20181231",
  finestra.dies=c(-Inf,0),
  dt.agregadors=select(cataleg_antidiab,cod,agr),
  prefix="FF.",
  camp_agregador="agr",
  agregar_data=T) %>% 
  transmute(idp,data_index=data.to.string(FF.AD) %>% as.numeric())

# Primer diagnostic DM
dt_diagnostics<-select(dt_diagnostics,-agr) %>% 
  left_join(dt_cataleg,by="cod") %>% 
  filter(exposed=="exposed") %>% 
  group_by(idp)%>%mutate(data_index=min(dat,na.rm = TRUE))%>% slice(1) %>% ungroup() %>% 
  transmute(idp,data_index)

# Fusiono les tres i agafo data minima !!! PER TENIR LA DATA INDEX!!!
dt_diagnostics<-
  dtagr_prescrip_DIABET %>% 
  rbind(dtagr_facturat_DIABET) %>% 
  rbind(dt_diagnostics) %>% group_by(idp) %>% 
  summarise(data_index=min(data_index))


DINDEX<-dt_diagnostics %>% 
  mutate(DM_pre2005=ifelse(data_index<20060101,1,0)) # Filtre per data d'entrada dels Casos (DM prevalents)

# Construeixo el Grup Cohort dels EXPOSATS:
# Afegeixo info de població sobre exposats 
C_EXPOSATS<-DINDEX %>% left_join(LLEGIR.poblacio,by="idp") 

bd_index<-C_EXPOSATS %>% transmute(idp,dtindex=(data_index))


# Agrego a 2018
dt_problemes_2018<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                     bd.dindex = "20181231",
                                     dt.agregadors=select(dt_cataleg,cod,agr),
                                     finestra.dies=c(-Inf,0),prefix = "DG18.") 


#the candidate non-exposed patients (CNE) 2018
dt_problemes_POOL_2018<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                          bd.dindex = "20181231",
                                          dt.agregadors=select(dt_non_exposed_pool,cod,agr),
                                          finestra.dies=c(-Inf,0),prefix = "DG18.") 


# Fusiono diagnostics agregats a 2018 i recodifico 0/1
C_EXPOSATS<-C_EXPOSATS%>% left_join(dt_problemes_2018,by="idp") %>% 
  select(-dtindex)

C_EXPOSATS<-C_EXPOSATS %>% 
  mutate_at(vars(starts_with("DG18.") ),funs(ifelse(is.na(.),0,1))) 

# [Filtre 1]: DM EXCLUENTS abans del 31.12.2018
C_EXPOSATS<-C_EXPOSATS %>% mutate(exclusio1_prev_2018=ifelse(DG18.exclude==0,0,1)) %>% 
  select(idp,data_index,DM_pre2005,sexe,dnaix,entrada,sortida,situacio,any_entrada, exclusio1_prev_2018)

# Agrego en data index 
dt_agregada_agr1<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),
                                    bd.dindex = bd_index,
                                    dt.agregadors=select(dt_cataleg,cod,agr),
                                    finestra.dies = c(-Inf,0),prefix = "DG.") 

# filtres dels prevalents i cancer abans de la data Index pels Exposats!
C_EXPOSATS<-C_EXPOSATS %>% 
  left_join(dt_agregada_agr1,by="idp") %>% select(-dtindex) %>% 
  mutate_at(vars(starts_with("DG.") ),funs(ifelse(is.na(.),0,1))) 

# Genero EDAT en data index i criteri d'inclusio en data index
C_EXPOSATS<-C_EXPOSATS %>% mutate(
  edat_dtindex=(ymd(data_index)-ymd(dnaix))/365.25, 
  exc_edat=ifelse(edat_dtindex>100 | edat_dtindex<35,1,0))


# NO EXPOSATS   -------------------------------
# Tots els No DM exposats 
C_NO_EXPOSATS<-LLEGIR.poblacio %>% filter(entrada<=20181231) %>% anti_join(C_EXPOSATS,by="idp")

# Fusiono diagnostics agregats a 2018 i recodifico 0/1
C_NO_EXPOSATS<-C_NO_EXPOSATS %>%left_join(dt_problemes_POOL_2018,by="idp") %>% select(-dtindex)
C_NO_EXPOSATS<-C_NO_EXPOSATS %>% 
  mutate_at(vars(starts_with("DG18.") ),funs(ifelse(is.na(.),0,1))) 


# [Filtre 1]: NO EXPOSED EXCLUENTS abans del 31.12.2018
C_NO_EXPOSATS<-C_NO_EXPOSATS %>% mutate(exclusio1_prev_2018=ifelse(DG18.pool==0,0,1)) 
C_NO_EXPOSATS <- C_NO_EXPOSATS %>% select(idp, sexe,dnaix, entrada, sortida, situacio, any_entrada, exclusio1_prev_2018)


# FILTRES ABANS DELS FLOW-CHART DELS EXPOSATS 

# Pels Exposats:DIA INDEX SUPERIOR AL 2005, 
# ENTRADA A LA B.D ANTERIOR AL 31.12.2018 
# No tenir Malalties.Exclusio anterior 2018
# SI a la data Index tenen mes de 100 anys o menys de 35 anys serna EXCLOSOS! 

C_EXPOSATS <-C_EXPOSATS %>% 
  filter(DM_pre2005==0) %>% 
  filter(entrada<=20181231) %>% 
  filter(exclusio1_prev_2018==0) %>% 
  filter(exc_edat==0) %>% 
  filter(!(situacio=="D" & sortida <data_index))  # Data de mort anterior a data index (Data DG)



# [PelS No Exposats:No tenir DM2,EXCLUDE,PREVALENTS_MET anterior 2018]

# 2.	From the entire database (ED),   ----------------------------


C_NO_EXPOSATS<-C_NO_EXPOSATS%>% filter(exclusio1_prev_2018==0)



# Formateig PRE matching 
# Fusionar base de dades en dues : 
dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))

# Netejo dades
rm(C_NO_EXPOSATS,C_EXPOSATS,bd_index,DINDEX,dt_diagnostics,LLEGIR.cmbdh_diagnostics_padris,LLEGIR.diagnostics,exclosos_inici)

gc()

#  A PARTIR D'AQUÍ ES  PODRIA DE MIRAR DE CANVIAR EL SISTEMA!:
dt_temp<-dt_problemes_2018 %>% transmute(idp,
                                         DG18.cancer=ymd(DG18.cancer) ,
                                         DG18.prevalent_CVD=ymd(DG18.prevalent_CVD),
                                         DG18.prevalent_KD=ymd(DG18.prevalent_KD),
                                         DG18.prevalent_MET=ymd(DG18.prevalent_MET))
dt_matching<-dt_matching %>% left_join(dt_temp,by="idp")
rm(dt_temp)


# Excloc difunts anteriors a 20060101 (no té sentit, ja que la ppoblació és del 2006-2018!!)
dt_matching<-dt_matching %>% filter(!(situacio=="D" & sortida <20060101)) 



# [Filtre_pre matching 2]: per generacions :# posteriors a 84   (Massa Joves) / anteriors al 1906 (Massa grans)  
dt_matching<-dt_matching %>% mutate(exclusio2_generacio=if_else(dnaix>19840101 | dnaix<19060101,1,0)) 

table(dt_matching$exclusio2_generacio)
table(dt_matching$exclusio1_prev_2018)


# Generar flow_chart Post_matching i aplicar criteris exclusions ------------

# Generar flow_chart Prematching i aplicar criteris exclusions
flow_chart1<-criteris_exclusio_diagrama(dt=dt_matching,
                                        taulavariables=conductor,
                                        criteris = "exc_pre",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample pre matching"),
                                        colors=c("white","grey"),
                                        forma=c("ellipse","box"))





#flow_chart1

# Aplicar filtres 
dt_matching_pre<-dt_matching
dt_matching<-criteris_exclusio(dt_matching,taulavariables=conductor,criteris="exc_pre")

# 4. Preparar matching i setriskmatching  ----------------------------

#4.	Exact matching the exposed cohort (EC) to the candidate non-exposed patients (CNE)
#   with a ratio 1:10 (EC:CNE) by year of birth (+/-1year), sex, and practice,
#   without replacement (each candidate non-exposed patient can be only matched once). 
#   This is the matched cohort (MC), and the index date is the same as the matched exposed patient.


dt_matching<-dt_matching %>% 
  transmute(idp,dnaix,sexe,grup,dtevent=data_index,entrada,sortida,
            DG18.cancer,DG18.prevalent_CVD,DG18.prevalent_KD,DG18.prevalent_MET) %>%
  left_join(LLEGIR.variables_geo_sanitaries,by="idp")


# Generar data de sortida (Data event / Data de censura)     
dt_matching<-dt_matching %>% 
  mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% 
  mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 

#vii [Parametres d'aparellament: llistaPS=c("sexe","any_naix","iddap")] 

# Parametres d'aparellament
llistaPS<-extreure.variables("matching",conductor)


if (mostra) llistaPS<-llistaPS[llistaPS!="idup"] %>% cbind("iddap") #Si mostra canvio la idup per iddap

num_controls<-10
llavor<-125
set.seed(llavor)
gc()


# viii)					[MATCHING 1:10]   -----------

#heaven::riskSetMatch

detach("package:tidyr", unload = TRUE)
library("heaven",lib.loc="C:/Users/Ramon/Desktop/llibreria_R/heaven_2018")


dades_match<-heaven::riskSetMatch(ptid="idp"                   # Unique patient identifier
                                  ,event="grup"                # 0=Control, 1=case
                                  ,terms=llistaPS              # terms c("n1","n2",...) - list of vairables to match by
                                  ,dat=dt_matching              # dataset with all variables
                                  ,Ncontrols=num_controls         # number of controls to provide
                                  ,oldevent="oldevent"            # To distinguish cases used as controls
                                  ,caseid="caseid"                # variable to group cases and controls (case-ptid)
                                  ,reuseCases=F                   # T og F or NULL - can a case be a control prior to being a case?
                                  ,reuseControls=F                # T or F or NULL - can controls be reused?
                                  ,caseIndex="dtindex_case"       # Integer or date, date where controls must be prior
                                  ,controlIndex="dtindex_control" # controlIndex - Index date for controls
                                  ,NoIndex=FALSE                # If T ignore index
                                  ,cores=1                      # Number of cores to use, default 1
                                  ,dateterms=c("DG18.cancer","DG18.prevalent_CVD","DG18.prevalent_KD","DG18.prevalent_MET")
                                  ) 
library(tidyr)
gc()



# Número de controls per conjunt a risk  
heaven::matchReport(dades_match, id="idp",case="grup",caseid="caseid")

# Número de controls per conjunt a risk  
dades_match[,numControls:=.N,by=caseid]
dades_match<- dades_match %>% mutate(numControls=numControls-1)

table(dades_match$grup,dades_match$numControls)

# Verificació d'aparellament per edad + sexe 
# descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
# descrTable(grup~dnaix+any_naix+sexe,data=dades_match)

# Flowchart 2 afegeixo matching a dt_matching_pre
dt_matching_pre<-dt_matching_pre %>% 
  left_join(transmute(dades_match,idp,exclusio3_match=0),by="idp") %>% 
  mutate(exclusio3_match=ifelse(is.na(exclusio3_match),1,0)) 


# Generar flow_chart Prematching i aplicar criteris exclusions 
flow_chart2<-criteris_exclusio_diagrama(dt=dt_matching_pre,
                                        taulavariables=conductor,
                                        criteris = "exc_pre2",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample pre matching"),
                                        colors=c("white","grey"),
                                        forma=c("ellipse","box"))

# Agregar base de dades aparellada i generar filtres d'exclusion POST MATCHING 

#flow_chart1
#flow_chart2

# Formatejo dt_index_match
dt_index_match<-dades_match %>% 
  transmute(idp,iddap,caseid,grup,dnaix,sexe,dtindex=dtindex_case,numControls,entrada) %>% as_tibble()
# Formatejo bd_index
bd_index<-dt_index_match %>% 
  transmute(idp,dtindex=lubridate::as_date(dtindex))

#5.	From the matched cohort (MC)  ------------------
#   remove patients died before the index date; 
#   then remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date;
#   then keep a randomly selected 5 matched non-exposed patients
#   (good to set a seed to make the random selection replicable). 
#   This is the final non-exposed cohort (FNE).


#x)   [Eliminen TOTS aquells que hagin tingut abans  DATINDEX;[CANCER,CVD,KD,MET]   ]     

# Diagnostic prevalent en data index 

vars_exclusions<-c("DG18.cancer", "DG18.prevalent_CVD", "DG18.prevalent_KD","DG18.prevalent_MET")
dt_index_match<-
  dt_index_match %>% 
  left_join(select(dt_problemes_2018,idp,vars_exclusions),by="idp") %>% 
  mutate_at(vars(starts_with("DG18.")), funs(kk=if_else(ymd(.)>as_date(dtindex) | is.na(.),0,1))) %>% mutate(dtindex=as_date(dtindex)) %>% 
  rename_at(vars( contains("_kk") ), list(~paste("", gsub("DG18.", "DG.", .), sep = "")) ) %>% 
  rename_at(vars( contains("_kk") ), list(~paste("", gsub("_kk", "", .), sep = "")) )

# comptar criteris dins
vars_exclusions<-c("DG.cancer", "DG.prevalent_CVD", "DG.prevalent_KD","DG.prevalent_MET")
dt_index_match<-
  comptar_valors(dt_index_match,vars_exclusions,valor = "1") %>% 
  mutate(excl_diagnostic=if_else(num_valors>0,1,0)) %>% 
  select(-num_valors)

#xi) Filtre EDAT edat. 
# [Generem filtre :no agafarem edats superiors a 100 anys a dtindex , ni inferiors de 35 anys]
dt_index_match<-dt_index_match %>% 
  mutate(edat_dtindex=(as_date(dtindex)-ymd(dnaix))/365.25, 
         exc_edat=ifelse(edat_dtindex>100 | edat_dtindex<35,1,0))

#xii)  Filtre Entrada Clínicia.
# 4. data entrada > 1 any 
dt_index_match<-dt_index_match %>% mutate(exc_antiguitat=ifelse(as_date(dtindex) - ymd(entrada)<365,1,0))
# estudiar Entrada Clínica!.



# xiv ) Generar filtre apestats segons criteris anteriors (DG's i grup d'edat)
dt_index_match<-dt_index_match %>%
  mutate(exc_apestat=ifelse(DG.cancer==1 | DG.prevalent_CVD==1 | DG.prevalent_KD==1 | DG.prevalent_MET==1 | exc_edat==1 ,1,0)) %>% 
  group_by(caseid) %>% 
  mutate(exc_apestat=max(exc_apestat))%>% ungroup()

# Si es apestat però te la patologia no es apestat 
dt_index_match<-dt_index_match %>% 
  mutate(exc_apestat=ifelse(DG.cancer==1 | DG.prevalent_CVD==1 | DG.prevalent_KD==1 | DG.prevalent_MET==1 | exc_edat==1,0,exc_apestat))


#xv)  Casos sense controls 

# Pajarracos sense control eliminats 

dt_index_match<-dt_index_match %>% mutate(exc_0controls=ifelse(numControls==0,1,0))



# Ara generar flow_chart i aplicar criteris 

# Generar flow_chart Post_matching i aplicar criteris exclusions ------------

flow_chart3<-criteris_exclusio_diagrama(dt=dt_index_match,
                                        taulavariables=conductor,
                                        criteris = "exc_post1",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample post matching"))

#flow_chart3

# Aplicar criteris
dt_post_matching<-criteris_exclusio(dt_index_match,taulavariables=conductor,criteris="exc_post1")


# Aplicto criteri 5 controls màxim 

# xiii). FILTRE DE RANDOM 1:5] 
#[agafarem els 5 primers Controls més pròxims a la data de naixament del Cas, la resta els Exclourem!][1:5]             

temp<-dt_post_matching %>% filter(grup==1) %>% select(caseid,dnaix_grup=dnaix)

# Junto data de naixament del grup
dt_post_matching<-dt_post_matching %>% 
  left_join(temp,by="caseid") %>% 
  mutate(distancia_cas=abs(dnaix-dnaix_grup)) %>% 
  group_by(caseid) %>% 
  arrange(grup,distancia_cas) %>% # Ordena de menor
  mutate(idp2 = row_number()) %>% 
  ungroup() %>% 
  mutate(idp2=ifelse(grup==1,0,idp2)) %>%
  mutate(exc_random=ifelse(idp2>5,1,0))


# Netejar variables transitoris
dt_post_matching$distancia_cas=NULL
temp<-NULL


# Generar flow_chart Post_matching i aplicar criteris exclusions ------------
flow_chart4<-criteris_exclusio_diagrama(dt=dt_post_matching,
                                        taulavariables=conductor,
                                        criteris = "exc_post2",
                                        ordre="exc_ordre",
                                        grups="grup",
                                        etiquetes="descripcio",
                                        sequencial = T,
                                        pob_lab=c("SIDIAP","Sample post matching"))


#flow_chart4

# Aplicar filtre criteri d'enclusio 
dt_post_matching<-criteris_exclusio(dt_post_matching,taulavariables=conductor,criteris="exc_post2")


# Actualitzo numero de controls per grup  ------

table(dt_post_matching$exc_0controls)
table(dt_post_matching$numControls)

dt_post_matching<-dt_post_matching %>% group_by(caseid) %>% mutate(numControls=max(idp2)) %>% ungroup()
dt_post_matching<-dt_post_matching %>% mutate(exc_0controls=ifelse(numControls==0,1,0))

table(dt_post_matching$exc_0controls)
table(dt_post_matching$numControls)


# Agregar resta d'historics   --------------------------

#i    LLEGIR.variables_analitiques
#ii   LLEGIR.variables_cliniques
#iii  LLEGIR.tabaquisme
#iv   LLEGIR.farmacs_prescrits
#v    LLEGIR.farmacs_facturat
#vi   LLEGIR.variables_socioeconomiques

# Agregar variables 
dt_variables<-LLEGIR.variables_analitiques %>% bind_rows(LLEGIR.variables_cliniques) %>% 
  transmute(idp,cod=agr,dat,val)
dt_temp<-dt_post_matching %>% transmute(idp,dtindex=lubridate::as_date(dtindex))
dtagr_variables<-agregar_analitiques(dt=dt_variables,bd.dindex=dt_temp,finestra.dies = c(-365,0))

rm(LLEGIR.variables_analitiques)

#min(dt_variables$dat)
#03.01.2005
#max(dt_variables$dat)
#31.12.2018


# Agregar tabac 
LLEGIR.tabaquisme<-LLEGIR.tabaquisme %>% transmute(idp,cod="tabac",dat,val)
dtagr_tabac<-agregar_analitiques(dt=LLEGIR.tabaquisme,bd.dindex=dt_temp,finestra.dies = c(-Inf,0))
rm(LLEGIR.tabaquisme)

# Prescripcions / Facturació pendent de CODIS / AGREGADORS  

#iv   LLEGIR.farmacs_prescrits  
# Prescripcion de CODIS / AGREGADORS 
LLEGIR.farmacs_prescrits<-LLEGIR.farmacs_prescrits %>% transmute(idp,cod,dat,dbaixa)
#
dtagr_prescrip<-agregar_prescripcions(
  dt=LLEGIR.farmacs_prescrits,
  bd.dindex=dt_temp,
  dt.agregadors=select(dt_cataleg,cod,agr),
  prefix="FP.",
  finestra.dies=c(0,0),
  camp_agregador="agr",
  agregar_data=F)

rm(LLEGIR.farmacs_prescrits)


# v    LLEGIR.farmacs_facturat 
# Facturació pendent de CODIS / AGREGADORS 
LLEGIR.farmacs_facturat<-LLEGIR.farmacs_facturat %>% transmute(idp,cod,dat,env)
#
dtagr_facturat<-agregar_facturacio(
  dt=LLEGIR.farmacs_facturat,
  finestra.dies=c(0,30),
  bd.dindex=dt_temp,
  dt.agregadors=select(dt_cataleg,cod,agr),
  prefix="FF.",
  camp_agregador="agr",
  agregar_data=F)

rm(LLEGIR.farmacs_facturat)

# Unió de totes les agregacions -------------------------

#Fem la Taula Final:[PLana]
dt_plana<-dt_post_matching %>% 
  left_join(select(LLEGIR.poblacio,idp,sortida,situacio),by="idp")%>% 
    left_join(select(dtagr_variables,-dtindex),by="idp")%>%
      left_join(select(dtagr_tabac,-dtindex),by="idp")%>%
        left_join(select(dtagr_prescrip,-dtindex),by="idp")%>%
          left_join(select(dtagr_facturat,-dtindex) ,by="idp")%>%
              left_join(LLEGIR.variables_socioeconomiques,by="idp") 


# Eliminor dades
rm(LLEGIR.poblacio,
   LLEGIR.variables_geo_sanitaries,
   LLEGIR.variables_cliniques,
   LLEGIR.variables_socioeconomiques, 
   dt_variables, dt_problemes_2018, dt_problemes_POOL_2018,dt_problemes_2005, dt_post_matching)
gc()




# Salvar taula_plana + flow_charts      -----------------------

saveRDS(dt_plana, file=here::here(dir_dades,"dades_DH.rds"))

save(flow_chart1,
     flow_chart2,
     flow_chart3,
     flow_chart4,
     file=here::here(dir_output,"flow_chart.Rdata"))

save(mostra,file="parametre_mostra.Rdata")


