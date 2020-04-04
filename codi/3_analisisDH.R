# ANALISIS  --------------

#6.	The final study cohort is the combination of the exposed cohort (EC) 
#   and the final non-exposed cohort (FNE).

#This is a retrospective cohort study. 
#All people diagnosed with diabetes 
#between 01/01/2006 and the latest specific capture 31/12/2018


## Lectura de dades i parametres

gc()
rm(list = ls())


# libreries i funcions
library("dplyr")
library("LexisPlotR")
library("Epi")
library("popEpi")
library("lintr")
library("splines")
library("Epi")
library("graphics")

# Descarregar funcions github -
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# Llegir plana

dt_plana<-readRDS("dades/dades_DH.rds")

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
#variable.names(dt_total)


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



png(here::here('images','figura2a.png'))
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
png(here::here("images",'figura2b.png'))
plot(LEXIS_dt_plana_Lex_grup0, grid=0:20*5, col="black", xaxs="i", yaxs="i",xlim=c(2006,2018), ylim=c(35,100), lwd=1, las=1 )
points(LEXIS_dt_plana_Lex_grup0, pch=c(NA,16)[LEXIS_dt_plana_Lex_grup0$fail+1] )
dev.off()



#vi)
#TAXA BRUTA! 

#Tasa_de_Mort_1000_Personas_Año_Diabetico
#Tasa_de_Mort_1000_Personas_Año_No_Diabetico



#Raw mortality by calendar year

S1 <- splitLexis(LEXIS_dt_plana_Lex, time.scale="per", breaks=2006+seq(0,14,1) )
S1<-S1%>%left_join(select(dt_plana,idp,grup))%>%mutate(grup2=(if_else(grup==1,"Diabético","No Diabético")))   
#summary( S1 )

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

colnames(table_rate)[1] <- "ANY"
colnames(table_rate)[2] <- "Mort_No_Diabet"
colnames(table_rate)[3] <- "Tasa_de_Mort_1000_Personas_ANY_No_Diabet"
colnames(table_rate)[4] <- "Per_ANY_No_Diabet"
colnames(table_rate)[5] <- "Mort_Diabet"
colnames(table_rate)[6] <- "Tasa_de_Mort_1000_Personas_ANY_Diabet"
colnames(table_rate)[7] <- "Per_ANY_Diabet"
# ordenar..

variable.names(table_rate)
table_rate<- table_rate%>%select(ANY,
                                 Mort_Diabet,
                                 Per_ANY_Diabet,
                                 Tasa_de_Mort_1000_Personas_ANY_Diabet,
                                 Mort_No_Diabet,
                                 Per_ANY_No_Diabet,
                                 Tasa_de_Mort_1000_Personas_ANY_No_Diabet)%>% tibble() 

#table_rate



#[Taxa Bruta.png]:-> Taxa Bruta

png(here::here("images","Taxa_Bruta.png"))
matplot(as.numeric(dimnames(YDrate)[[1]]), 
        log="y",
        las=1,
        xlab="Periodo",
        ylab="Tasa de Mortalidad Bruta por cada 1000 personas-año ,por Periodo ",
        YDrate[,,"rate"], type="l",
        lty=1, lwd=3,
        col=c("black","blue"),
        xlim=c(2006,2018))



dev.off()


#vii) 


#COX#


#TASAS CON EL MODELO DE COX. 


COX1<-LEXIS_dt_plana_Lex%>% left_join(select(dt_plana,idp,sexe)) %>% mutate(gender=if_else(sexe=="H","M","W"))
cox_lexis_model <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+gender+age+ cluster(caseid),data =  COX1)
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

cox_lexis_model3 <- coxph(Surv(lex.dur,lex.Xst)~factor(grup)+ cluster(caseid),data =  COX1)

cox_lexis_ratios3 <- cbind(HR = exp(coef(cox_lexis_model3)), exp(confint(cox_lexis_model3)))

#As every other time we exponentiat the coefficients to get hazard ratios
cox_lexis_out3 <- summary(cox_lexis_model3)
cox_lexis_out3 <- cbind(cox_lexis_ratios3,cox_lexis_out3$coefficients)

#And we put everything in a nice table
#cox_lexis_out3

#cox_lexis_out
#cox_lexis_out2
#cox_lexis_out3
#



#VIII


# M O D E L     P O I S S O N   G L M   GLOBAL     #



# A partir dels Models Poisson, aplicarem les gràfiques suavitzades de les nostres Taxes , 
# en funcio del periòde o de l'edat!.



# M O D E L     P O I S S O N   G L M:                              #

#            Tasa de Moratlitat=Periodo*GRUP                        #



dbs00 <- popEpi::splitMulti(LEXIS_dt_plana_Lex, per= seq(2006,2018,1))
dbs00<-dbs00 %>%left_join(select(dt_plana,idp,grup))
p.kn0 <- with(subset(dbs00, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))

r00<- glm((lex.Xst==1)~Ns(per, knots = p.kn0)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs00 )

figura00_TOTAL<-summary(r00)
#figura00_TOTAL




#grafica1.png 

#Tasa_Mortlidad=Periodo*GRUPO  [GRUPO=NO Diabetis]

nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000)


png(here::here("images","grafica1.png"))
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

dev.off()

#grafica2.png
#Tasa_Mortlidad=Periodo*GRUPO  [GRUPO=Diabetis]

nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000)

png(here::here("images","grafica2.png"))
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

dev.off()



# M O D E L     P O I S S O N   G L M:                              #

#               Tasa de Moratlitat=EDAD*GRUPO                       #



dbs01 <- popEpi::splitMulti(LEXIS_dt_plana_Lex, age= seq(35,100,1))
dbs01<-dbs01 %>%left_join(select(dt_plana,idp,grup))

a.kn <- with(subset(dbs01, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))


r01<- glm((lex.Xst==1)~Ns( age, knots=a.kn)*grup,
          family = poisson,
          offset = log(lex.dur),
          data   = dbs01 )

figura00_TOTAL2<-summary(r01)
#figura00_TOTAL2



#grafica3.png
#Tasa_Mortlidad=EDAD*GRUPO  [GRUPO=NO Diabetis]

nd00<- data.frame(age=35:100,grup=0,lex.dur=1000)

png(here::here("images","grafica3.png"))
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
#Tasa_Mortlidad=EDAD*GRUPO  [GRUPO=NO Diabetis]

nd00<- data.frame(age=35:100,grup=1,lex.dur=1000)

#ci.pred(r0)
png(here::here("images","grafica4.png"))
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







# M O D E L     P O I S S O N   G L M:                              #

#            Tasa de Moratlitat=EDAD*PERIODO*GRUPO                  #






#Edat mitjana de la Població 


dbs1 <- popEpi::splitMulti(LEXIS_dt_plana_Lex, age = seq(35,100,1), per= seq(2006,2018,1))
dbs1<-dbs1%>%left_join(select(dt_plana,idp,grup))

a.kn <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))

r02 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)*Ns(per, knots = p.kn)*grup,
           family = poisson,
           offset = log(lex.dur),
           data   = dbs1 )
figura02_TOTAL2<-summary(r02)
#figura02_TOTAL2



#grafica5.png
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=NO Diabetis,EDAD=MEDIA POB]
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000,age=mean(dt_plana$agein2))

png(here::here("images","grafica5.png"))
matplot( nd00$per,ci.pred(r02, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ Edad media  NO Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
dev.off()

#grafica6.png
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=Diabetis,EDAD=MEDIA POB]
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000,age=mean(dt_plana$agein2))

png(here::here("images","grafica6.png"))
matplot( nd01$per,ci.pred(r02, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ Edad media Diabéticos cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
dev.off()



# M O D E L     P O I S S O N   G L M:                              #
#            Tasa de Moratlitat=EDAD+PERIODO+GRUPO                  #



r03 <- glm((lex.Xst==1)~Ns(age, knots = a.kn)+Ns(per, knots = p.kn)+grup,
           family = poisson,
           offset = log(lex.dur),
           data   = dbs1 )
figura02_TOTAL3<-summary(r03)
#figura02_TOTAL3


#NO DIABÈTICS! aditiu MODEL mitjana
nd00<- data.frame(per=2006:2018,grup=0,lex.dur=1000,age=mean(dt_plana$agein2))

png(here::here("images","grafica7.png"))
matplot( nd00$per,ci.pred(r03, newdata=nd00),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ Edad media no Diabéticos cada  1000 Personas-año  ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
dev.off()


#DIABÈTICS! aditiu MODEL  mitjana
nd01<- data.frame(per=2006:2018,grup=1,lex.dur=1000,age=mean(dt_plana$agein2))
png(here::here("images","grafica8.png"))
matplot( nd01$per,ci.pred(r03, newdata=nd01),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad 2006-2018/ Edad media Diabéticos cada  1000 Personas-año  ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
par( mfrow=c(1,1) )
dev.off()




#[prediccions taxes producte triple!]

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
res_MORTALITY_PRODUCTE <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
# write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.csv")


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
res_MORTALITY_SUMA <-cbind(acm_DM, rateD=exp(acm_DM$es_d), rateD_lb=exp(acm_DM$lb_d), rateD_ub=exp(acm_DM$ub_d))
write.csv2(res_MORTALITY_SUMA, file="res_MORTALITY_SUMA.csv")




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
dt_plana_Lex2_grup0<-dt_plana_Lex2 %>% filter(grup==0)
dt_plana_Lex2_grup1<-dt_plana_Lex2 %>% filter(grup==1)
#-------------------------------------------------------------------------------------------#

#table(dt_plana_Lex2$gender)

#exemple supin! el mateix!!
db1_sup <-Lexis(entry = list(period = entry,age = entry -birth),
                exit = list(period = exit),
                exit.status = fail,
                id = idp,
                data =  dt_plana_Lex2_grup0)



#grup0 (NO_DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana2_Lex_grup0<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  id           =     idp,
  data         =     dt_plana_Lex2_grup0 
)
#-------------------------------------------------------------------------------------------#
#grup1 (DIABETIS)
# Define a Lexis object with timescales calendar time and age
LEXIS_dt_plana2_Lex_grup1<- Lexis( 
  entry        =     list(per=entry),
  exit         =     list(per=exit,age=exit-birth ),
  exit.status  =     fail,
  id           =     idp,
  data         =     dt_plana_Lex2_grup1 )
#-------------------------------------------------------------------------------------------#
# LEXIS_dt_plana2_Lex_grup0
# LEXIS_dt_plana2_Lex_grup1
#-------------------------------------------------------------------------------------------#
#variable.names(dt_plana)



# M O D E L     P O I S S O N   G L M:                              #

#            Tasa de Moratlitat=EDAD*PERIODO*SEXE                   #


#NO DIABÈTIC
#LEXIS_dt_plana2_Lex_grup0

dbs0 <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup0, age = seq(35,100,1), per= seq(2006,2018,1))
a.kn0 <- with(subset(dbs0, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn0 <- with(subset(dbs0, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
r_supin_0 <- glm((lex.Xst==1)~Ns(age, knots = a.kn0)*Ns(per, knots = p.kn0)*gender,
                 family = poisson,
                 offset = log(lex.dur),
                 data   = dbs0 )
figura_no_diabetic_supin<-summary(r_supin_0)
figura_no_diabetic_supin
#-------------------------------------------------------------------------------------------#


#SI DIABÈTIC
#LEXIS_dt_plana2_Lex_grup1

dbs1 <- popEpi::splitMulti(LEXIS_dt_plana2_Lex_grup1, age = seq(35,100,1), per= seq(2006,2018,1))
a.kn1 <- with(subset(dbs1, lex.Xst==1), quantile(age+lex.dur,(1:5-0.5)/5))
p.kn1 <- with(subset(dbs1, lex.Xst==1), quantile(per+lex.dur,(1:5-0.5)/5))
#-------------------------------------------------------------------------------------------#
r_supin_1 <- glm((lex.Xst==1)~Ns(age, knots = a.kn1)*Ns(per, knots = p.kn1)*gender,
                 family = poisson,
                 offset = log(lex.dur),
                 data   = dbs1 )
figura_diabetic_supin<-summary(r_supin_1)
figura_diabetic_supin
#-------------------------------------------------------------------------------------------#



# GRÀFIQUES:[]


#-------------------------------------------------------------------------------------------#
AGE0<-dt_plana%>%filter(grup==0)%>%select(agein2)
AGE1<-dt_plana%>%filter(grup==1)%>%select(agein2)
#-------------------------------------------------------------------------------------------#
#grafica_supin_0H
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=NO Diabetis,EDAD=MEDIA POB]
nd0h<- data.frame(per=2006:2018,gender=1,lex.dur=1000,age=mean(AGE0$agein2))
png(here::here("images","grafica_supin_0h.png"))

matplot( nd0h$per,ci.pred(r_supin_0, newdata=nd0h),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad cada  1000 Personas-año ",
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
dev.off()
#-------------------------------------------------------------------------------------------#
#grafica_supin_0D
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=NO Diabetis,EDAD=MEDIA POB]
nd0d<- data.frame(per=2006:2018,gender=0,lex.dur=1000,age=mean(AGE0$agein2))
png(here::here("images","grafica_supin_0d.png"))
matplot( nd0h$per,ci.pred(r_supin_0, newdata=nd0d),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad cada  1000 Personas-año ",
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn0, lwd=2 )
dev.off()
#-------------------------------------------------------------------------------------------#
#grafica_supin_1H
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=Diabetis,EDAD=MEDIA POB]
nd1h<- data.frame(per=2006:2018,gender=1,lex.dur=1000,age=mean(AGE1$agein2))
png(here::here("images","grafica_supin_1h.png"))
matplot( nd1h$per,ci.pred(r_supin_1, newdata=nd1h),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad cada  1000 Personas-año ", 
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn1, lwd=2 )
par( mfrow=c(1,1) )
dev.off()
#-------------------------------------------------------------------------------------------#
#grafica_supin_1D
#Tasa_Mortlidad=PERIODO*EDAD*GRUPO  [GRUPO=Diabetis,EDAD=MEDIA POB]
nd1d<- data.frame(per=2006:2018,gender=0,lex.dur=1000,age=mean(AGE1$agein2))
png(here::here("images","grafica_supin_1d.png"))
matplot( nd1d$per,ci.pred(r_supin_1, newdata=nd1d),
         type="l",
         lwd=c(3,1,1), 
         lty=1, col="black", 
         log="y",
         ylab="Tasa de Mortalidad cada  1000 Personas-año ",
         xlab="Periodo", 
         las=1, 
         ylim=c(1,1000) )
rug( p.kn1, lwd=2 )
par( mfrow=c(1,1) )
dev.off()
#-------------------------------------------------------------------------------------------#





#E  X C E L:[]


#-------------------------------------------------------------------------------------------#
#excel : no diabetic
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
age          <- c(35:100)
period       <- seq(2006,2018,1)
gender         <- c(0,1)
nd           <- expand.grid(age, period,gender)
colnames(nd) <- c("age","per","gender")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r_supin_0, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM0       <- cbind(nd,p1, out="acm")

res_MORTALITY_PRODUCTE_0 <-cbind(acm_DM0, rateD=exp(acm_DM0$es_d), rateD_lb=exp(acm_DM0$lb_d), rateD_ub=exp(acm_DM0$ub_d))
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE_0, file="res_MORTALITY_PRODUCTE_0.csv")
#-------------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------------#
#excel : diabetic
#-------------------------------------------------------------------------------------------#
#D:0
#H:1
#-------------------------------------------------------------------------------------------#
age          <- c(35:100)
period       <- seq(2006,2018,1)
gender         <- c(0,1)
nd           <- expand.grid(age, period,gender)
colnames(nd) <- c("age","per","gender")
nd           <- cbind(nd, lex.dur=1000)
p1           <- ci.pred(r_supin_1, newdata = nd, Exp = FALSE)
colnames(p1) <- c("es_d", "lb_d", "ub_d")
acm_DM1       <- cbind(nd,p1, out="acm")

res_MORTALITY_PRODUCTE_1 <-cbind(acm_DM1, rateD=exp(acm_DM1$es_d), rateD_lb=exp(acm_DM1$lb_d), rateD_ub=exp(acm_DM1$ub_d))
#write.xlsx(res_MORTALITY_PRODUCTE, file="res_MORTALITY_PRODUCTE.xlsx")
write.csv2(res_MORTALITY_PRODUCTE_1, file="res_MORTALITY_PRODUCTE_1.csv")
#-------------------------------------------------------------------------------------------#

# Salvar taula_plana

save(taula_events,
     taula_events2,
     table_rate,
     figura1,
     figura00_TOTAL,
     cox_lexis_out,
     cox_lexis_out2,
     cox_lexis_out3,
     figura_no_diabetic_supin,
     figura_diabetic_supin,
     file="DataHarmonization.Rdata")
