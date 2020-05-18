#####################
# 12.05.2020

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

dt_plana<-readRDS(here::here(dir_dades,"dades_DH.rds")) %>% select(idp,dtindex,dnaix,sortida,exitus,grup,caseid,temps_FU,sexe,agein2)

load(here::here(parametres$fitxer_Rdata))

gc()

# Parametres 
conductor<-"conductor_DataHarmonization.xlsx"


#Preparació LEXIS:

dt_plana<-dt_plana %>% mutate(
  dtindex=as_date(dtindex),
  dnaix=ymd(dnaix),
  sortida=ymd(sortida))

# fer elnou model :[diabetic/ control    .(periodo,edat,sexe)]


# figures  --------------



#--------------------------------------------------------------------------------------------#
#[25.3.2020]:
#--------------------------------------------------------------------------------------------#
dt_plana_Lex2<-dt_plana%>%select(idp,dtindex,dnaix,sortida,exitus,sexe,caseid,grup)
dt_plana_Lex2<-dt_plana_Lex2%>%mutate(birth =dnaix)
dt_plana_Lex2<-dt_plana_Lex2%>%mutate(entry =dtindex)
dt_plana_Lex2<-dt_plana_Lex2%>%mutate(exit =sortida)
dt_plana_Lex2<-dt_plana_Lex2%>%mutate(fail =exitus)
dt_plana_Lex2<-dt_plana_Lex2%>%mutate(gender =sexe)
dt_plana_Lex2<-dt_plana_Lex2%>%select(idp,birth,entry,exit,fail,gender,caseid,grup)
dt_plana_Lex2<-dt_plana_Lex2%>%mutate(gender=if_else(gender=="H",1,0))
#D:0
#H:1
dt_plana_Lex2<-structure(dt_plana_Lex2,class = "data.frame")
dt_plana_Lex2 <- cal.yr(dt_plana_Lex2, format="%y-%m-%d", wh=2:4 )


#-------------------------------------------------------------------------------------------#
#dt_plana_Lex2_grup0<-dt_plana_Lex2 %>% filter(grup==0)
#dt_plana_Lex2_grup1<-dt_plana_Lex2 %>% filter(grup==1)
#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#

#table(dt_plana_Lex2$gender)

#exemple supin! el mateix!!
#db1_sup <-Lexis(entry = list(period = entry,age = entry -birth),
#                exit = list(period = exit),
#                exit.status = fail,
#                id = idp,
#                data =  dt_plana_Lex2_grup0)



#grup0 (NO_DIABETIS)
# Define a Lexis object with timescales calendar time and age
#LEXIS_dt_plana2_Lex_grup0<- Lexis( 
#  entry        =     list(per=entry),
#  exit         =     list(per=exit,age=exit-birth ),
#  exit.status  =     fail,
#  id           =     idp,
#  data         =     dt_plana_Lex2_grup0 
#)
#-------------------------------------------------------------------------------------------#
#grup1 (DIABETIS)
# Define a Lexis object with timescales calendar time and age
#LEXIS_dt_plana2_Lex_grup1<- Lexis( 
#  entry        =     list(per=entry),
#  exit         =     list(per=exit,age=exit-birth ),
#  exit.status  =     fail,
#  id           =     idp,
#  data         =     dt_plana_Lex2_grup1 )
#-------------------------------------------------------------------------------------------#
# LEXIS_dt_plana2_Lex_grup0
# LEXIS_dt_plana2_Lex_grup1
#-------------------------------------------------------------------------------------------#
#variable.names(dt_plana)





# M O D E L     P O I S S O N   G L M:                              #

#            Tasa de Moratlitat=EDAD*PERIODO*SEXE                   #


#NO DIABTIC
#LEXIS_dt_plana2_Lex_grup0

#dbs0 <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup0, age = seq(35,100,1), per= seq(2006,2018,1))
#a.kn0 <- with(subset(dbs0, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
#p.kn0 <- with(subset(dbs0, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
#r_supin_0 <- glm((lex.Xst==1)~Ns(age, knots = a.kn0)*Ns(per, knots = p.kn0)*gender,
#                 family = poisson,
#                 offset = log(lex.dur),
#                 data   = dbs0 )
#figura_no_diabetic_supin<-summary(r_supin_0)
#figura_no_diabetic_supin
#-------------------------------------------------------------------------------------------#


#SI DIAB?TIC
#LEXIS_dt_plana2_Lex_grup1

#dbs1 <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup1, age = seq(35,100,1), per= seq(2006,2018,1))
#a.kn1 <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
#p.kn1 <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
#r_supin_1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn1)*Ns(per, knots = p.kn1)*gender,
#                 family = poisson,
#                 offset = log(lex.dur),
#                 data   = dbs1 )
#figura_diabetic_supin<-summary(r_supin_1)
#figura_diabetic_supin
#-------------------------------------------------------------------------------------------#



# GR?FIQUES:[]


#-------------------------------------------------------------------------------------------#
#AGE0<-dt_plana%>%filter(grup==0)%>%select(agein2)
#AGE1<-dt_plana%>%filter(grup==1)%>%select(agein2)
#-------------------------------------------------------------------------------------------#
#grafica_supin_0H
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=NO Diabetis,EDAD=MEDIA POB]
#nd0h<- data.frame(per=2006:2018,gender=1,lex.dur=1000,age=mean(AGE0$agein2))
#png(here::here(dir_images,"grafica_supin_0h.png"))

#matplot( nd0h$per,ci.pred(r_supin_0, newdata=nd0h),
#         type="l",
#         lwd=c(3,1,1), 
#         lty=1, col="black", 
#         log="y",
#         ylab="Tasa de Mortalidad cada  1000 Personas-año ",
#         xlab="Periodo", 
#         las=1, 
#         ylim=c(1,1000) )
#rug( p.kn0, lwd=2 )
#dev.off()
#-------------------------------------------------------------------------------------------#
#grafica_supin_0D
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=NO Diabetis,EDAD=MEDIA POB]
#nd0d<- data.frame(per=2006:2018,gender=0,lex.dur=1000,age=mean(AGE0$agein2))
#png(here::here(dir_images,"grafica_supin_0d.png"))
#matplot( nd0h$per,ci.pred(r_supin_0, newdata=nd0d),
#         type="l",
#         lwd=c(3,1,1), 
#         lty=1, col="black", 
#         log="y",
#         ylab="Tasa de Mortalidad cada  1000 Personas-año ",
#         xlab="Periodo", 
#         las=1, 
#         ylim=c(1,1000) )
#rug( p.kn0, lwd=2 )
#dev.off()
#-------------------------------------------------------------------------------------------#
#grafica_supin_1H
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=Diabetis,EDAD=MEDIA POB]
#nd1h<- data.frame(per=2006:2018,gender=1,lex.dur=1000,age=mean(AGE1$agein2))
#png(here::here(dir_images,"grafica_supin_1h.png"))
#matplot( nd1h$per,ci.pred(r_supin_1, newdata=nd1h),
#         type="l",
#         lwd=c(3,1,1), 
#         lty=1, col="black", 
#         log="y",
#         ylab="Tasa de Mortalidad cada  1000 Personas-año ", 
#         xlab="Periodo", 
#         las=1, 
#         ylim=c(1,1000) )
#rug( p.kn1, lwd=2 )
#par( mfrow=c(1,1) )
#dev.off()
#-------------------------------------------------------------------------------------------#
#grafica_supin_1D
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=Diabetis,EDAD=MEDIA POB]
#nd1d<- data.frame(per=2006:2018,gender=0,lex.dur=1000,age=mean(AGE1$agein2))
#png(here::here(dir_images,"grafica_supin_1d.png"))
#matplot( nd1d$per,ci.pred(r_supin_1, newdata=nd1d),
#         type="l",
#         lwd=c(3,1,1), 
#         lty=1, col="black", 
#         log="y",
#         ylab="Tasa de Mortalidad cada  1000 Personas-año ",
#         xlab="Periodo", 
#         las=1, 
#         ylim=c(1,1000) )
#rug( p.kn1, lwd=2 )
#par( mfrow=c(1,1) )
#dev.off()
#-------------------------------------------------------------------------------------------#




#E  X C E L:[]


#-------------------------------------------------------------------------------------------#
#excel : no diabetic
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
#age          <- c(35:100)
#period       <- seq(2006,2018,1)
#gender         <- c(0,1)
#nd           <- expand.grid(age, period,gender)
#colnames(nd) <- c("age","per","gender")
#nd           <- cbind(nd, lex.dur=1000)
#p1           <- ci.pred(r_supin_0, newdata = nd, Exp = FALSE)
#colnames(p1) <- c("es_d", "lb_d", "ub_d")
#acm_DM0       <- cbind(nd,p1, out="acm")

#res_MORTALITY_PRODUCTE_0 <-cbind(acm_DM0, rateD=exp(acm_DM0$es_d), rateD_lb=exp(acm_DM0$lb_d), rateD_ub=exp(acm_DM0$ub_d))


#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
#write.csv2(res_MORTALITY_PRODUCTE_0, here::here(dir_output,"res_MORTALITY_PRODUCTE_0.csv"))
#-------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------#
#excel : diabetic
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
#age          <- c(35:100)
#period       <- seq(2006,2018,1)
#gender         <- c(0,1)
#nd           <- expand.grid(age, period,gender)
#colnames(nd) <- c("age","per","gender")
#nd           <- cbind(nd, lex.dur=1000)
#p1           <- ci.pred(r_supin_1, newdata = nd, Exp = FALSE)
#colnames(p1) <- c("es_d", "lb_d", "ub_d")
#acm_DM1       <- cbind(nd,p1, out="acm")

#res_MORTALITY_PRODUCTE_1 <-cbind(acm_DM1, rateD=exp(acm_DM1$es_d), rateD_lb=exp(acm_DM1$lb_d), rateD_ub=exp(acm_DM1$ub_d))
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
#write.csv2(res_MORTALITY_PRODUCTE_1, file=here::here(dir_output,"res_MORTALITY_PRODUCTE_1.csv"))
#-------------------------------------------------------------------------------------------#




#[V.Xii.2020]


# Salvar taula_plana

###########################################
#[Sinatxis que hem canviat dia 11.5.2020!]
###########################################



#####################################################################
# M O D E L     P O I S S O N   G L M:                              #
#            Tasa de Moratlitat=EDAD*PERIODO                        #
#####################################################################


#HOME,NO DIABIC!
dt_plana_Lex2_grup0H<-dt_plana_Lex2 %>% filter(grup==0 & gender==1)
#HOME,DIABTIC!
dt_plana_Lex2_grup1H<-dt_plana_Lex2 %>% filter(grup==1 & gender==1)
#DONA,NO DIABICA!
dt_plana_Lex2_grup0D<-dt_plana_Lex2 %>% filter(grup==0 & gender==0)
#DONA,DIABTIC!
dt_plana_Lex2_grup1D<-dt_plana_Lex2 %>% filter(grup==1 & gender==0)


#-------------------------------------------------------------------------------------------#
#HOME grup0 (NO_DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana2_Lex_grup0H<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  id           =     idp,
  data         =     dt_plana_Lex2_grup0H 
)
#-------------------------------------------------------------------------------------------#
#HOME grup1 (DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana2_Lex_grup1H<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  id           =     idp,
  data         =     dt_plana_Lex2_grup1H )
#-------------------------------------------------------------------------------------------#
#DONA grup0 (NO_DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana2_Lex_grup0D<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  id           =     idp,
  data         =     dt_plana_Lex2_grup0D 
)
#-------------------------------------------------------------------------------------------#
#DONA grup1 (DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana2_Lex_grup1D<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  id           =     idp,
  data         =     dt_plana_Lex2_grup1D )
#-------------------------------------------------------------------------------------------#


#NO DIAB?IC HOME
#LEXIS_dt_plana2_Lex_grup0H

dbs0H <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup0H, age = seq(35,100,1), per= seq(2006,2018,1))
a.kn0H <- with(subset(dbs0H, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn0H <- with(subset(dbs0H, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
r_supin_0H <- glm((lex.Xst==1)~Ns(age, knots = a.kn0H)*Ns(per, knots = p.kn0H),
                  family = poisson,
                  offset = log(lex.dur),
                  data   = dbs0H )
figura_no_diabetic_H_supin<-summary(r_supin_0H)
#figura_no_diabetic_H_supin
#-------------------------------------------------------------------------------------------#


#SI DIAB?TIC HOME
#LEXIS_dt_plana2_Lex_grup1H

dbs1H <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup1H, age = seq(35,100,1), per= seq(2006,2018,1))
a.kn1H <- with(subset(dbs1H, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn1H<- with(subset(dbs1H, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
r_supin_1H <- glm((lex.Xst==1)~Ns(age, knots = a.kn1H)*Ns(per, knots = p.kn1H),
                  family = poisson,
                  offset = log(lex.dur),
                  data   = dbs1H )
figura_diabetic_H_supin<-summary(r_supin_1H)
#figura_diabetic_H_supin
#-------------------------------------------------------------------------------------------#


#NO DIABTIC DONA
#LEXIS_dt_plana2_Lex_grup0D

dbs0D <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup0D, age = seq(35,100,1), per= seq(2006,2018,1))
a.kn0D <- with(subset(dbs0D, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn0D <- with(subset(dbs0D, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
r_supin_0D <- glm((lex.Xst==1)~Ns(age, knots = a.kn0D)*Ns(per, knots = p.kn0D),
                  family = poisson,
                  offset = log(lex.dur),
                  data   = dbs0D )
figura_no_diabetic_D_supin<-summary(r_supin_0D)
#figura_no_diabetic_D_supin
#-------------------------------------------------------------------------------------------#


#SI DIABTIC DONA
#LEXIS_dt_plana2_Lex_grup1D

dbs1D <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup1D, age = seq(35,100,1), per= seq(2006,2018,1))
a.kn1D <- with(subset(dbs1D, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn1D<- with(subset(dbs1D, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
r_supin_1D <- glm((lex.Xst==1)~Ns(age, knots = a.kn1D)*Ns(per, knots = p.kn1D),
                  family = poisson,
                  offset = log(lex.dur),
                  data   = dbs1D )
figura_diabetic_D_supin<-summary(r_supin_1D)
#figura_diabetic_D_supin
#-------------------------------------------------------------------------------------------#





#GRAFIQUES!!:[]

##GRFICA:::NO DIAB?IC HOME
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=NO Diabetis,EDAD=MEDIA POB]


#glm((lex.Xst==1)~Ns(age, knots = a.kn1H)*Ns(per, knots = p.kn1H),

sexeH<-dt_plana%>%filter(sexe=="H")%>%select(agein2)
sexeD<-dt_plana%>%filter(sexe=="D")%>%select(agein2)




#1)nd0h
nd0h<- data.frame(per=2006:2018,lex.dur=1000,age=mean(sexeH$agein2))

png(here::here(dir_images,"grafica_supin_0h.png"))

matplot( nd0h$per,ci.pred(r_supin_0H, newdata=nd0h),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Mortality Rate every 1000 Person-years ",
         xlab="Period", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0H, lwd=2 )

dev.off()




#2)nd1h
nd1h<- data.frame(per=2006:2018,lex.dur=1000,age=mean(sexeH$agein2))

png(here::here(dir_images,"grafica_supin_1h.png"))

matplot( nd1h$per,ci.pred(r_supin_1H, newdata=nd1h),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Mortality Rate every 1000 Person-years ",
         xlab="Period", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn1H, lwd=2 )

dev.off()

#3)
nd0d<- data.frame(per=2006:2018,lex.dur=1000,age=mean(sexeD$agein2))

png(here::here(dir_images,"grafica_supin_0d.png"))

matplot( nd0d$per,ci.pred(r_supin_0D, newdata=nd0d),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Mortality Rate every 1000 Person-years ",
         xlab="Period", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0D, lwd=2 )

dev.off()


#4)
nd1d<- data.frame(per=2006:2018,lex.dur=1000,age=mean(sexeD$agein2))

png(here::here(dir_images,"grafica_supin_1d.png"))

matplot( nd1d$per,ci.pred(r_supin_1D, newdata=nd1d),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Mortality Rate every 1000 Person-years ",
         xlab="Period", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn1D, lwd=2 )

dev.off()



#
#



#E  X C E L:[]


#-------------------------------------------------------------------------------------------#
#excel : no diabetic HOME
#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
age          <- c(35:100)
period       <- seq(2006,2018,0.1)
#gender         <- c(0,1)
nd           <- expand.grid(age, period)
colnames(nd) <- c("age","per")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r_supin_0H, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM0H       <- cbind(nd,p1, out="acm")
#-------------------------------------------------------------------------------------------#
res_MORTALITY_PRODUCTE_0H <-cbind(acm_DM0H, rateD=exp(acm_DM0H$es_d), rateD_lb=exp(acm_DM0H$lb_d), rateD_ub=exp(acm_DM0H$ub_d))
#-------------------------------------------------------------------------------------------#
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE_0H, here::here(dir_output,"res_MORTALITY_PRODUCTE_0H.csv"), append = T)
#-------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------#
#excel : diabetic HOME
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
age          <- c(35:100)
period       <- seq(2006,2018,0.1)
#gender         <- c(0,1)
nd           <- expand.grid(age, period)
colnames(nd) <- c("age","per")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r_supin_1H, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM1H       <- cbind(nd,p1, out="acm")
#-------------------------------------------------------------------------------------------#
res_MORTALITY_PRODUCTE_1H <-cbind(acm_DM1H, rateD=exp(acm_DM1H$es_d), rateD_lb=exp(acm_DM1H$lb_d), rateD_ub=exp(acm_DM1H$ub_d))
#-------------------------------------------------------------------------------------------#
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE_1H, here::here(dir_output,"res_MORTALITY_PRODUCTE_1H.csv"),append = TRUE)
#-------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------#
#excel : no diabetic DONA
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
age          <- c(35:100)
period       <- seq(2006,2018,0.1)
#gender         <- c(0,1)
nd           <- expand.grid(age, period)
colnames(nd) <- c("age","per")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r_supin_0D, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM0D       <- cbind(nd,p1, out="acm")
#-------------------------------------------------------------------------------------------#
res_MORTALITY_PRODUCTE_0D <-cbind(acm_DM0D, rateD=exp(acm_DM0D$es_d), rateD_lb=exp(acm_DM0D$lb_d), rateD_ub=exp(acm_DM0D$ub_d))
#-------------------------------------------------------------------------------------------#
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE_0D, here::here(dir_output,"res_MORTALITY_PRODUCTE_0D.csv"),append = TRUE)
#-------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------#
#excel : diabetic DONA
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
age          <- c(35:100)
period       <- seq(2006,2018,0.1)
#gender         <- c(0,1)
nd           <- expand.grid(age, period)
colnames(nd) <- c("age","per")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r_supin_1D, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM1D       <- cbind(nd,p1, out="acm")
#-------------------------------------------------------------------------------------------#
res_MORTALITY_PRODUCTE_1D <-cbind(acm_DM1D, rateD=exp(acm_DM1D$es_d), rateD_lb=exp(acm_DM1D$lb_d), rateD_ub=exp(acm_DM1D$ub_d))
#-------------------------------------------------------------------------------------------#
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE_1D, here::here(dir_output,"res_MORTALITY_PRODUCTE_1D.csv"),append = TRUE)
#-------------------------------------------------------------------------------------------#

save(taula_events,
     taula_events2,
     table_rate,
     taula_vida2,
     figura1,
     cox_lexis_out,
     cox_lexis_out2,
     cox_lexis_out3,
     figura_no_diabetic_H_supin,
     figura_diabetic_H_supin,
     figura_no_diabetic_D_supin,
     figura_diabetic_D_supin,
     file=parametres$fitxer_Rdata)
