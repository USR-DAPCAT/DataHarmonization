#####################
# 12.05.2020

# ANALISIS  --------------

#6.	The final study cohort is the combination of the exposed cohort (EC) 
#   and the final non-exposed cohort (FNE).

#This is a retrospective cohort study. 
#All people diagnosed with diabetes 
#between 01/01/2006 and the latest specific capture 31/12/2018


## Lectura de dades i parametres

memory.limit()
memory.limit(size=64000)
gc()
rm(list = ls())

memory.limit(size=64000)

# libreries i funcions
library("dplyr")
library("LexisPlotR")
library("Epi")
library("popEpi")
library("lintr")
library("splines")
library("Epi")
library("graphics")
library("lubridate")

# Descarregar funcions github -
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# Llegir parametrs  --------
source("codi/funcio_parametre.R")
load("parametre_mostra.Rdata")
parametres<-parametres_directori(mostra)
dir_dades<-parametres$dir_dades
dir_output<-parametres$dir_output
dir_images<-parametres$dir_images
# Llegir plana

dt_plana<-readRDS(here::here(dir_dades,"dades_DH.rds")) %>% select(idp,dtindex,dnaix,sortida,exitus,grup,caseid,temps_FU,sexe,any_index)

gc()

# Parametres 
conductor<-"conductor_DataHarmonization.xlsx"




#iii)
#Tasa de Mortalitat amb  Kaplan-Meier [diabetic/ no-diabètics]


rescox<-coxph(Surv(temps_FU,exitus)~grup,data=dt_plana)    

N=rescox$n
EVENTS=rescox$nevent
coef=summary(rescox)$coef[1,1]
HR=summary(rescox)$coef[1,2]
IC951=summary(rescox)$conf.int[1,3]
IC952=summary(rescox)$conf.int[1,4]
se.coef=summary(rescox)$coef[1,3]
p=summary(rescox)$coef[1,5]

dt_plana$temps_FU<-as.numeric(dt_plana$temps_FU)


taula_events<-dt_plana %>% group_by(grup,sexe) %>% summarise(
  events=sum(exitus), 
  N=n(),
  PTdies=sum(temps_FU),
  PTanys=(sum(temps_FU)/365.25) ,
  taxa=(events/PTanys)*1000) %>% 
  ungroup()


#taula_events

taula_events<-etiquetar_valors(dt=taula_events,variables_factors=conductor,fulla="etiquetes",camp_etiqueta="etiqueta2")


taula_events2<-dt_plana%>% group_by(grup) %>% summarise(
  events=sum(exitus), 
  N=n(),
  PTdies=sum(temps_FU),
  PTanys=(sum(temps_FU)/365.25) ,
  taxa=(events/PTanys)*1000) %>% 
  ungroup()


taula_events2<-etiquetar_valors(dt=taula_events2,variables_factors=conductor,fulla="etiquetes",camp_etiqueta="etiqueta2")

#[Taula de TAXES!]#
#taula_events
#taula_events2


#iv) #GRAFIC  Tasa de Mortalitat amb  Kaplan-Meier [diabetic/ no-diabètics]

# Survival 
fit<- survfit(Surv(temps_FU, exitus) ~ grup, data = dt_plana)
library("survminer")
survminer::ggsurvplot(fit,data = dt_plana)
figura1<-survminer::ggsurvplot(fit,data =dt_plana)
#figura1


#v)



#L  E X I S #


##Part descriptiva  inicial!!!
#mirar LEXIS!!!
#::: preparció LEXIS:[]

#Preparació LEXIS:

dt_plana<-dt_plana %>% mutate(
  dtindex=as_date(dtindex),
  dnaix=ymd(dnaix),
  sortida=ymd(sortida))

dt_plana_Lex<-dt_plana %>% select(idp,dtindex,dnaix,sortida,exitus,grup,caseid)
dt_plana_Lex<-dt_plana_Lex%>%mutate(birth =dnaix)
dt_plana_Lex<-dt_plana_Lex%>%mutate(entry =dtindex)
dt_plana_Lex<-dt_plana_Lex%>%mutate(exit =sortida)
dt_plana_Lex<-dt_plana_Lex%>%mutate(fail =exitus)
dt_plana_Lex<-dt_plana_Lex%>%select(idp,birth,entry,exit,fail,grup,caseid)
dt_plana_Lex<-structure(dt_plana_Lex,class = "data.frame")
dt_plana_Lex <- cal.yr(dt_plana_Lex, format="%y-%m-%d", wh=2:4 )
dt_plana_Lex_grup0<-dt_plana_Lex %>% filter(grup==0)
dt_plana_Lex_grup1<-dt_plana_Lex %>% filter(grup==1)

# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana_Lex<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =  dt_plana_Lex )

#grup1 (DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana_Lex_grup1<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =     dt_plana_Lex_grup1 )
#gravem la figura!



png(here::here(dir_images,'figura2a.png'))
plot(LEXIS_dt_plana_Lex_grup1, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2006,2018), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_plana_Lex_grup1, pch=c(NA,16)[LEXIS_dt_plana_Lex_grup1$fail+1] )
dev.off()

#grup0 (NO DIABTEIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana_Lex_grup0<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  data         =    dt_plana_Lex_grup0 )
png(here::here(dir_images,'figura2b.png'))
plot(LEXIS_dt_plana_Lex_grup0, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2006,2018), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_plana_Lex_grup0, pch=c(NA,16)[LEXIS_dt_plana_Lex_grup0$fail+1] )
dev.off()



#vi)
#TAXA BRUTA! 

#Tasa_de_Mort_1000_Personas_Año_Diabetico
#Tasa_de_Mort_1000_Personas_Año_No_Diabetico

gc()

#Raw mortality by calendar year

S1 <- splitLexis(LEXIS_dt_plana_Lex, time.scale="per", breaks=2006+seq(0,14,1) )
S1<-S1%>%left_join(select(dt_plana,idp,grup))%>%mutate(grup2=(if_else(grup==1,"Diabetic","Non Diabetic")))   
#summary( S1 )

gc()

TAULA<-xtabs( cbind(Y=lex.dur,D=(lex.Xst==1),rate=lex.dur) ~I(floor(per))+grup,data=S1 )
dnam <- dimnames(TAULA)
dnam[[3]] <- c("Y","D","rate")
YDrate <- array( NA, dimnames=dnam, dim=sapply(dnam,length) )
YDrate[,,1:3] <- TAULA

YDrate[,,3] <- YDrate[,,2]/YDrate[,,1]*1000
#YDrate[,,3]

table_rate<-round(ftable( YDrate, row.vars=1 ), 1 ) 
table_rate<-table_rate %>% as.data.frame() %>% unite(kk,"grup","Var3") %>% spread(kk,"Freq") 

#variable.names(table_rate)

colnames(table_rate)[1] <- "Year"
colnames(table_rate)[2] <- "Death_in_Non_Diabetic_Group"
colnames(table_rate)[3] <- "Mortality_Rate_1000_Person_year_Non_Diabetic"
colnames(table_rate)[4] <- "Person_year_Non_Diabetic"
colnames(table_rate)[5] <- "Death_in_Diabetic_group"
colnames(table_rate)[6] <- "Mortality_Rate_1000_Person_year_Diabetic"
colnames(table_rate)[7] <- "Person_year_Diabetic"
# ordenar..

#variable.names(table_rate) 
table_rate<- table_rate%>%select(Year,
                                 Death_in_Non_Diabetic_Group,
                                 Person_year_Non_Diabetic,
                                 Mortality_Rate_1000_Person_year_Non_Diabetic,
                                 Death_in_Diabetic_group,
                                 Person_year_Diabetic,
                                 Mortality_Rate_1000_Person_year_Diabetic)%>% tibble() 



#table_rate



#[Taxa Bruta.png]:-> Taxa Bruta

png(here::here(dir_images,"Taxa_Bruta2.png"))
matplot(as.numeric(dimnames(YDrate)[[1]]), 
        log="y",
        las=1,
        xlab="Period",
        ylab="Mortality Rate 1000 Person-year ,for Period ",
        YDrate[,,"rate"], type="l",
        lty=1, lwd=3,
        col=c("black","blue"),
        xlim=c(2006,2018))



dev.off()


##########################################################################
#12.5.2020
# life_table(dt_plana,2010) 
dt_plana<-dt_plana %>% mutate(any_index2=as.character(any_index))


##########################################################################
# Calcular la taula de vida 
#
#

dt_plana<-dt_plana %>%mutate(year_exit=year(ymd(sortida)) %>% as.character()) 



life_table<-function(dades,year=2010) {
  
  #dades<-dt_plana %>% as_tibble()
  #year=2016
  
  dades<-dades %>% filter(any_index2<=year & ((exitus==1 & year_exit>=year) | exitus==0))
  
  #any_index2<=year
  
  # Mutate exitus posteriors a 2010 vius a 2010
  dades<-dades %>% mutate (exitus=if_else((exitus==1 & year_exit>year) | (exitus==0),0,1)) 
  
  # Resum d'exitus i persones vives per any 
  
  #dades %>%group_by(grup)%>%summarise(exitus=sum(exitus),N=n(), censura=N-exitus,taxa=(exitus/N)*1000)
  
  dades %>%group_by(grup)%>%summarise(exitus=sum(exitus),N=n(), censura=N-exitus)
}


vector_any<-c(2006:2018) 
vector_any<-purrr::set_names(vector_any,vector_any) 
taula_vida<-vector_any %>% map_df(~life_table(dt_plana,.x),.id="Year",grup="Group")
#----------------------------------------------------------------------------------#
taula_vida0<-taula_vida %>%filter(grup==0)
taula_vida1<-taula_vida %>%filter(grup==1)
#----------------------------------------------------------------------------------#
taula_vida0<-taula_vida0%>%select(Year,Censored_Non_DM=censura,Death_Non_DM=exitus,N_Non_DM=N)
taula_vida1<-taula_vida1%>%select(Year,Censored_DM=censura,Death_DM=exitus,N_DM=N)
#----------------------------------------------------------------------------------#
taula_vida2<-taula_vida0 %>% 
  left_join(taula_vida1,by="Year") 
#----------------------------------------------------------------------------------#

###########
#table_rate
###########

###########
#taula_vida
###########


#vii) 

#COX#

# TASAS CON EL MODELO DE COX (Elimino efecte cluster).
COX1<-LEXIS_dt_plana_Lex%>% left_join(select(dt_plana,idp,sexe)) %>% mutate(gender=if_else(sexe=="H","M","W"))
cox_lexis_model <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+gender+age ,data =  COX1)
cox_lexis_ratios <- cbind(HR = exp(coef(cox_lexis_model)), exp(confint(cox_lexis_model)))
#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out <- summary(cox_lexis_model)
cox_lexis_out <- cbind(cox_lexis_ratios,cox_lexis_out$coefficients)
#And we put everything in a nice table
#cox_lexis_out

km <- survfit(Surv(lex.dur,lex.Xst)~factor(grup),data =  COX1)

figura5<-ggsurvplot(km,conf.int=TRUE, legend.labs=c("No Diabético", "Diabètico"),data =  LEXIS_dt_plana_Lex)


cox_lexis_model2 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup),data =  COX1)


cox_lexis_ratios2 <- cbind(HR = exp(coef(cox_lexis_model2)), exp(confint(cox_lexis_model2)))
cox_lexis_out2 <- summary(cox_lexis_model2)
cox_lexis_out2 <- cbind(cox_lexis_ratios2,cox_lexis_out2$coefficients)

cox_lexis_model3 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup),data =  COX1)

cox_lexis_ratios3 <- cbind(HR = exp(coef(cox_lexis_model3)), exp(confint(cox_lexis_model3)))

#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out3 <- summary(cox_lexis_model3)
cox_lexis_out3 <- cbind(cox_lexis_ratios3,cox_lexis_out3$coefficients)



# Salvar taula_plana

save(taula_events,
     taula_events2,
     table_rate,
     taula_vida2,
     figura1,
     cox_lexis_out,
     cox_lexis_out2,
     cox_lexis_out3,
     file=parametres$fitxer_Rdata)

