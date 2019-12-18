#--------------------------------#
library(Epi)
library(haven)
library(dplyr)
library(foreign)
library(broom)
library(popEpi)
# Llibreries necessaries 
library("data.table")
library("SNPassoc")
library("htmlwidgets")
library("compareGroups")
library("foreign")
library("lattice")
library("Hmisc")
# library("ggplot2")
library("pander")
library("readxl")
library("knitr")
library("data.table")
library("MatchIt")
library("survival")
library("dplyr")
# library("survminer")
library("purrr")
library("stringr")
library("tidyr")
library("devtools")
#install.packages("here")
library("here")
#--------------------------------#
#--------------------------------#
#setwd("R:/LRWE_Proj26/sl617/dataset/analysis") 
#setwd("R:/LRWE_Proj26/sl617/dataset/analysis") 
setwd("C:/Users/38122893W/Desktop/DataHarmonization")
#--------------------------------#
#testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320
#--------------------------------#

# generar_mostra_fitxers()  ----------------------

# Llegir tots els fitxers RDS dins d'un directori i generar una mostra aleatoria i salvar-lo en un directori "mostra"

# 1 Llegir fitxers sequencialment d'un directori
# 2 posarlos en una llista 
# 3 Afafar la mostra i filtrar-los 
# 4 Salvar-los en un directori




generar_mostra_fitxers<-function(directori="dades/SIDIAP",
                                 fitxer_poblacio="METPLUS_entregable_poblacio_20181126_190346.rds",
                                 mida_mostra=10000,
                                 prefix="test",
                                 directori_test="mostra_test") {
  
  #directori="dades/SIDIAP"
  #fitxer_poblacio="testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds"
  #mida_mostra=10069
  #prefix="test"
  #directori_test="mostra_test"
  
  
  # Funció interna per llegir fitxer txt o rds
  LLEGIR.fitxer<-function(n,directori,fitxer) {
    
    if (stringr::str_detect(fitxer,"\\.txt$")){
      dt<-data.table::fread(directori %>% here::here(fitxer)) %>% as_tibble() %>% head(n)}
    
    if (stringr::str_detect(fitxer,"\\.rds$")){
      dt<-readRDS(directori %>% here::here(fitxer)) %>% as_tibble() %>% head(n)}
    dt}
  
  
  #k<-LLEGIR.fitxer(n=10069, directori="dades/SIDIAP",fitxer="testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds")
  
  
  # Llista de fitxers .rds | .txt
  llista_de_fitxers<-list.files(directori) [list.files(directori) %>% stringr::str_detect("\\.rds$") |
                                              list.files(directori) %>% stringr::str_detect("\\.txt$")] 
  
  # Genero el directori mostra
  directori_mostra<-paste0(directori,"/",directori_test)
  if (!file.exists(directori_mostra)) {
    # Crear directori si no existeix 
    dir.create(file.path(directori,directori_test), showWarnings = FALSE)
  }
  
  
  
  
  # Si NO existeix algun fitxer GENERAR LOS / Si EXISTEIX algun  saltar 
  if (!file.exists(paste0(directori_mostra,"/",llista_de_fitxers)) %>% any()) {
    
    # Llegir ids mostra de fitxer poblacio
    dt_ids<-LLEGIR.fitxer(mida_mostra,directori,fitxer_poblacio) %>% select(idp)
    
    # Posar noms per que els guardi
    llista_de_fitxers<-setNames(llista_de_fitxers,llista_de_fitxers)
    # Llegir fitxers complerts
    llista_rds<-llista_de_fitxers %>% purrr::map(~LLEGIR.fitxer(n=Inf,directori = directori,fitxer=.x))
    
    # Filtrar via semijoint de tota la llista
    llista_rds_redux<-llista_rds %>% purrr::map(~semi_join(.x,dt_ids))
    
    # Ara salvar-los en un surbdirectori amb el nom triat 
    
    # Genero noms de fitxers dins directori test
    llista_de_fitxers<-str_replace_all(llista_de_fitxers, "\\.txt$", ".rds")
    llista_de_noms_fitxers_nous<-paste0(directori_mostra,"/",prefix,llista_de_fitxers)
    
    
    # Salvo en format rds tots els fitxers en directori
    # saveRDS(llista_rds_redux[[1]],file=llista_de_fitxers_nous[1])
    purrr::map2(llista_rds_redux,llista_de_noms_fitxers_nous,~saveRDS(.x,file=.y))
    
  }
  
  if (file.exists(paste0(directori_mostra,"/",llista_de_fitxers)) %>% any()) {
    print ("Algun d'aquests fitxers ja existeix")
  }
  
  
}


generar_mostra<-generar_mostra_fitxers(
  
  directori="dades/SIDIAP",
  fitxer_poblacio="testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds",
  mida_mostra=1,
  prefix="test",
  directori_test="mostra_test"
)



#generar_mostra

###############################################################
#[[C:\Users\38122893W\Desktop\DataHarmonization\dades\SIDIAP]]#
###############################################################




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