##############
#[14.01.2020]#
##############
#
library(Epi)
library(haven)
library(dplyr)
library(foreign)
library(broom)
library(popEpi)
#
setwd("R:/LRWE_Proj26/sl617/dataset/analysis") 
db <-read_dta("db.dta")
nrow(db)

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

