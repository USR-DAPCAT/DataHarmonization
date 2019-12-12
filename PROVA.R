#[03.12.2019]

#############################################################################################
### Curso modelos lineales generales, generalizados y mixtos en ecologia IE-UNAM          ###
### Modelos lineales generalizados mixtos con distribucion binomial, poisson y bin. neg.  ###
### Funciones lmer, glmm.admb, glmmPQL                                                    ###
### Roberto Munguia-Steyer                                                  2011/12/01    ###
#############################################################################################




# Cargando paquetes


install.packages("glmmADMB", repos="http://r-forge.r-project.org",type="source") 
install.packages("coefplot2", repos="http://R-Forge.R-project.org")
install.packages(c("glmmML", "influence.ME", "mlmRev"))

install.packages("bbmle")
install.packages("glmmADMB")
install.packages("coefplot2")

library(bbmle)
library(lme4)
library(glmmADMB)
library(MASS)
library(influence.ME)
library(mlmRev)
library(coefplot2)
library(effects)


## Utilizacion de anticonceptivos en Bangladesh, glmer con distribucion binomial
#help(Contraception)
str(Contraception)
summary(Contraception) # parece que la edad de las mujeres se centro en la media

rl1.lmer <- lmer(use ~ urban*age +(1|district), Contraception, binomial)
summary(rl1.lmer)

#Efectos fijos
rl1.lmer@fixef

#Efectos aleatorios
rl1.lmer@ranef

#Suma de ambos efectos, el valor del intercepto es el que varia, ya que asumimos que es una variable aleatoria de dist. normal
coef(rl1.lmer)

plot(fitted(rl1.lmer), resid(rl1.lmer)) #No es de mucha utilidad en datos binarios

mrlef <- allEffects(rl1.lmer)
summary(mrlef)
plot(mrlef) # Grafica del modelo global, es realmente importante la interaccion?

## Seleccion de modelos

rl2.lmer <- lmer(use ~ urban+age +(1|district), Contraception, binomial)
summary(rl2.lmer)

rl3.lmer <- update(rl2.lmer,~.- urban)
rl4.lmer <- update(rl2.lmer,~.- age)
rl5.lmer <- update(rl4.lmer, ~.-urban)

AICtab(rl1.lmer, rl2.lmer, rl3.lmer, rl4.lmer, rl5.lmer, base=TRUE, weights=TRUE)

summary(rl2.lmer)

mrl2ef <- allEffects(rl2.lmer)
summary(mrl2ef)

#Graficando los valores predichos poblacionales

coefi <- rl2.lmer@fixef
coefi

summary(Contraception)
Contraception$usenum <- ifelse(Contraception$use=="N", 0, 1)
Contraception[1:30,]

par(las=1)
grafdat <- table(Contraception$age,Contraception$usenum) #Cuantas mujeres usan anticonceptivos por edad 
grafdat <- data.frame(grafdat)
grafdat

names(grafdat) <- c("edad", "ac", "freq") #Cambiando los nombres de las columnas y pasando las variables a continuas
grafdat$edad <- as.numeric(grafdat$edad) 
grafdat$ac <- as.numeric(grafdat$ac)-1

par(las=1)
plot(grafdat$edad, grafdat$ac, cex = grafdat$freq/15, 
     ylab="P. de usar anticonceptivos", xlab="Edad") #El tamanho de los puntos es relativo al numero de casos en la misma posicion
curve(plogis(coefi[1] + coefi[3]*x), add=T)
curve(plogis(coefi[1] + coefi[2] + coefi[3]*x), add=T, lty=2)
legend(40,0.95, c("urbana", "rural"), lty=c(2,1))

## Modelos lineales generalizados de distribucion Poisson y binomial negativa
## Mortalidad de cancer por melanomas y su relacion con los rayos UVb

#help(Mmmec)

str(Mmmec)
summary(Mmmec)

Mmmec$lexpected <- log(Mmmec$expected)

m1.lmer <- lmer(deaths ~ uvb + (1|region), Mmmec, poisson,
                offset = lexpected) # Que significa y porque usamos el offset
summary(m1.lmer)

#Analisis de residuos
plot(fitted(m1.lmer), resid(m1.lmer))
qqnorm(resid(m1.lmer))

# Existe sobredispersion en el modelo?

#Paquete glmmADMB
m1.pois.admb <- glmmadmb(deaths ~ uvb + offset(lexpected)+ (1|region),  data= Mmmec, "poisson")
m1.nb.admb <- glmmadmb(deaths ~ uvb + offset(lexpected)+ (1|region),  data= Mmmec, "nbinom")

detach("package:effects")
detach("package:nlme")

## glmmm Poisson-lognormal, agregando variables aleatorias individuales

Mmmec$obs <- 1:nrow(Mmmec)

m2.lmer <- lmer(deaths ~ uvb + (1|region) + (1|obs), Mmmec, poisson,
                offset = lexpected)

AICtab(m1.lmer, m2.lmer, base=TRUE, weights=TRUE) #El modelo con sobredispersion presenta mejor soporte poisson-lognormal

AICtab(m1.pois.admb, m1.nb.admb, base=TRUE, weights=TRUE) #Corroborado con el paquete glmmadmb poisson-binomial negativa

summary(m1.nb.admb)

#Comparando los coeficientes obtenidos con los diferentes modelos


coefplot2(list(lmerpois = m1.lmer,
               lmerpoisln = m2.lmer,
               admbpois = m1.pois.admb,
               admbbn   = m1.nb.admb),
          legend=TRUE, 
          intercept = TRUE,
          legend.x = "topleft")


#Base de datos epil, paquete MASS, Ajusta el ultimo ejemplo de la pagina de ayuda de la funcion glmmPQL 
#con otras funciones de que realicen glmms.
#ojo usa los comandos de la pagina de ayuda para trabajar con la base epil3 en vez de epil
help(glmmPQL)

epil2 <- epil[epil$period == 1, ]
epil2["period"] <- rep(0, 59); epil2["y"] <- epil2["base"]
epil["time"] <- 1; epil2["time"] <- 4
epil2 <- rbind(epil, epil2)
epil2$pred <- unclass(epil2$trt) * (epil2$period > 0)
epil2$subject <- factor(epil2$subject)
epil3 <- aggregate(epil2, list(epil2$subject, epil2$period > 0),
                   function(x) if(is.numeric(x)) sum(x) else x[1])
epil3$pred <- factor(epil3$pred,
                     labels = c("base", "placebo", "drug"))


summary(epil3)

epipois.PQL <- glmmPQL(y ~ pred, random = ~1 | subject,
                       family = poisson, data = epil3)
summary(epipois.PQL)


# usando glmmADMB

epiadmb.pois <- glmmadmb(y ~pred + (1|subject), "poisson", data=epil3)
epiadmb.nb <- update(epiadmb.pois, family="nbinom")

AICtab(epiadmb.pois, epiadmb.nb, base=TRUE, weights=TRUE)


summary(epiadmb.pois) ; summary(epiadmb.nb) 
#Noten el cambio en los e. estandar cuando se considera la sobredispersion


# usando lme4

epilmer.pois <- glmer(y ~pred + (1|subject), poisson, data=epil3)
epilmer.pois@fixef; coef(epiadmb.pois) #los coeficientes del paquete glmmADMB y lme4 son muy parecidos

# modelo lognormal

epil3$obs <- 1:nrow(epil3)
head(epil3)

epilmer.poisln <- glmer(y ~ pred + (1|subject) + (1|obs), poisson, data=epil3)

AICtab(epilmer.pois, epilmer.poisln, base=TRUE, weights=TRUE) #la misma conclusion que con el paquete lmer

# comparando paquetes glmmADMB y lme4

epilmer.poisln@fixef; coef(epiadmb.nb)


# Numero esperado de epilepsias segun el tratamiento


epief <- allEffects(epilmer.poisln)
epief

summary(epief)

#Cotejando los valores estimados de los coeficientes por distintos modelos y funciones.
coefplot2(list(epilmer.pois = epilmer.pois,
               epilmer.poisln = epilmer.poisln,
               epiadmb.pois = epiadmb.pois,
               epiadmb.nb   = epiadmb.nb,
               epipois.PQL = epipois.PQL),
          intercept=TRUE,
          legend=TRUE, legend.x="topright")





