####  Escenari 1 (Excloent exposats sense control ) --------------
gc()
rm(list = ls())
# Funció parametritzal (Per defecte agafa mostra y conductor natural)
parametres_conductuals<-function(mostra=T,conductor="conductor_DataHarmonization.xlsx"){
  list(mostra=mostra,conductor=conductor)}

####  Escenari 1 (Excloent exposats sense control ) --------------

# Generar informe global
parametres<-parametres_conductuals(mostra = F)
mostra<-parametres$mostra
conductor<-parametres$conductor

# Salvar parametre
save(mostra,file="parametre_mostra.Rdata")

source(here::here("codi","1_lectura_DH.R"))

source(here::here("codi","2_preparacioDH.R"))

source(here::here("codi","3_analisisDH.R"))

source(here::here("codi","3_2_analisisDH.R"))

rmarkdown::render(here::here("codi","4_resultatsDH.Rmd"),output_file="Informe2_DH_final2")


####  Escenari 2  (Excloent exposats sense control + apestats ) --------------

.rs.restartR()
gc()
