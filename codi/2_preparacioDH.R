#####################
# 12.05.2020
gc()
rm(list = ls())

# libreries i funcions
library("dplyr")

# Descarregar funcions github -
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

# Llegir parametrs  --------
source("codi/funcio_parametre.R")
load("parametre_mostra.Rdata")
parametres<-parametres_directori(mostra)
dir_dades<-parametres$dir_dades
dir_output<-parametres$dir_output

# Llegir plana
dt_plana<-readRDS(here::here(dir_dades,"dades_DH.rds"))


# Parametres 
conductor<-"conductor_DataHarmonization.xlsx"

# Preparació  -----------------------

# RECODES / CALCULS     
dt_plana<-dt_plana %>% 
  mutate(dtindex=as_date(dtindex), 
         any_index=lubridate::year(lubridate::as_date(dtindex)),
         agein=(as_date(dtindex)-ymd(dnaix))/365.25, 
         exitus=if_else(situacio=="D",1,0),
         temps_FU=ymd(sortida)-as_date(dtindex),
         temps_FU2=(ymd(sortida)-as_date(dtindex))/365.25,
         agein2 =as.numeric(as_date(dtindex)-ymd(dnaix))/365.25,
         dnaix = as.character(dnaix),
         any_index2=as.character(any_index))


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


# Calcul de exitus_basal
dt_plana <- dt_plana %>% mutate(exitus_basasl=ifelse(situacio=="D" & ymd(sortida)<dtindex,"Si","No"))


# Recodificacions automatiques
dt_plana<-recodificar(dt_plana,taulavariables =conductor,"recode",missings = T)






# Salvar dades planes recodificade
saveRDS(dt_plana,file=here::here(dir_dades,"dades_DH.rds"))





