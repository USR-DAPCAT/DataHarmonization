gc()
rm(list = ls())

# Funció parametritzal (Per defecte agafa mostra y conductor natural)
parametres_conductuals<-function(mostra=F,conductor="conductor_DataHarmonization.xls"){
  list(mostra=mostra,conductor=conductor)}


####  Escenari 1  (No s'exclou per antiguitat )---------------
# Generar informe global
parametres<-parametres_conductuals()
mostra<-parametres$mostra
conductor<-parametres$conductor

source(here::here("codi","1_lectura_DH8.R"))
rmarkdown::render(here::here("codi","3_analisisDH8.Rmd"),output_file="Informe1_DH")


####  Escenari 2  (Excloent exposats sense control ) --------------
parametres<-parametres_conductuals(conductor="conductor_DataHarmonization2.xls")
mostra<-parametres$mostra
conductor<-parametres$conductor

source(here::here("codi","1_lectura_DH8.R"))
rmarkdown::render(here::here("codi","3_analisisDH8.Rmd"),output_file="Informe2_DH")


####  Escenari 3 (Excloent exposats s control + contaminats)
parametres<-parametres_conductuals(conductor="conductor_DataHarmonization3.xls")

mostra<-parametres$mostra
conductor<-parametres$conductor

source(here::here("codi","1_lectura_DH8.R"))
rmarkdown::render(here::here("codi","3_analisisDH8.Rmd"),output_file="Informe3_DH0")


##    Escenari 4 (S'exclou per antiguitat )
parametres<-parametres_conductuals(conductor="conductor_DataHarmonization4.xls")
mostra<-parametres$mostra
conductor<-parametres$conductor

source(here::here("codi","1_lectura_DH8.R"))
rmarkdown::render(here::here("codi","3_analisisDH8.Rmd"),output_file="Informe4_DH0")


##    Escenari 5 (s'exclou sense controls, s'aparella per DAP , (No per ABS)  ##
parametres<-parametres_conductuals(conductor="conductor_DataHarmonization4.xls")
mostra<-parametres$mostra
conductor<-parametres$conductor

source(here::here("codi","1_lectura_DH8.R"))
rmarkdown::render(here::here("codi","3_analisisDH8.Rmd"),output_file="Informe5_DH0")





