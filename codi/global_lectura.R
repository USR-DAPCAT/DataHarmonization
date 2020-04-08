# Codi lectura de tots el fitxers




#i       [dianostics.hospital.cim9]  mult
LLEGIR.cmbdh_diagnostics_padris<-readRDS(directori_dades%>% here::here("DAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds")) %>% as_tibble()

#min(LLEGIR.cmbdh_diagnostics_padris$dat)
#18.12.2006
#max(LLEGIR.cmbdh_diagnostics_padris$dat)
#28.12.2017

#ii       [dianostics.cap.cim10]  mult
LLEGIR.diagnostics<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_diagnostics_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.diagnostics)

#min(LLEGIR.diagnostics$dat)
#12.01.1941
#max(LLEGIR.diagnostics$dat)
#31.12.2018

#iii      [farmacs_facturats] mult
LLEGIR.farmacs_facturat<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_farmacs_facturats_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.farmacs_facturat)
#table(LLEGIR.farmacs_facturat$agr)

#min(LLEGIR.farmacs_facturat$dat)
#01.2006
#max(LLEGIR.farmacs_facturat$dat)
#12.2018

#iv       [farmacs_prescrits] mult
LLEGIR.farmacs_prescrits<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_farmacs_prescrits_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.farmacs_prescrits)
#table(LLEGIR.farmacs_prescrits$agr)

#min(LLEGIR.farmacs_prescrits$dat)
#31.07.2000
#max(LLEGIR.farmacs_prescrits$dat)
#31.12.2018

#v        [població] unic [min entrada 1.1.2006!!!]
LLEGIR.poblacio<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_poblacio_20190926_103409.rds")) %>% as_tibble() %>% 
  mutate(any_entrada=as.character(entrada) %>%stringr::str_sub(1,4))
#-------------------#
#variable.names(LLEGIR.poblacio)

#min(LLEGIR.poblacio$entrada)
#01.01.2006
#max(LLEGIR.poblacio$entrada)
#19.12.2018

#vi       [tabaquisme] mult
LLEGIR.tabaquisme<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_tabaquisme_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.tabaquisme)

#min(LLEGIR.tabaquisme$dat)
#01.01.1930
#max(LLEGIR.tabaquisme$dat)
#19.12.2018

#vii      [analitiques] mult
LLEGIR.variables_analitiques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_analitiques_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.variables_analitiques)

#min(LLEGIR.variables_analitiques$dat)
#02.01.2006
#max(LLEGIR.variables_analitiques$dat)
#31.12.2018


#viii     [variables_cliíniques] mult
LLEGIR.variables_cliniques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_cliniques_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.variables_cliniques)

#min(LLEGIR.variables_cliniques$dat)
#03.01.2005
#max(LLEGIR.variables_cliniques$dat)
#31.12.2018

#ix       [variables geo_sanitàries] unic [prové de la poblacio]
LLEGIR.variables_geo_sanitaries<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_geo_sanitaries_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.variables_geo_sanitaries)

#x        [variables socioeconòmiques] unic [prové de la població]
LLEGIR.variables_socioeconomiques<-readRDS(directori_dades %>% here::here("DAPCRMM_entregable_variables_socioeconomiques_20190926_103409.rds")) %>% as_tibble()
#variable.names(LLEGIR.variables_socioeconomiques)

#xi        [Catàleg]
LLEGIR.variables_Cataleg<-readRDS("dades/SIDIAP/test" %>% here::here("DAPCRMM_entregable_cataleg_20190930_093320.rds")) %>% as_tibble()
#variable.names(LLEGIR.variables_Cataleg)

#table(LLEGIR.variables_Cataleg$agr)

# Llegeixo cataleg 
dt_cataleg<-read_excel("Spain_codes.xlsx") %>% select(cod,agr,exposed)

#the candidate non-exposed patients (CNE) 
dt_non_exposed_pool<-read_excel("Spain_codes.xlsx",sheet ="non-exposed pool" )%>%select(cod,agr)


# [De les dues B.D DIAGNOSTICS ---> AGAFEM DIABETIS[EXPOSED a l'excel(Spain_codes)]                  
#  LLEGIR.farmacs_prescrits 
#  Prescripcion de CODIS / AGREGADORS 
LLEGIR.farmacs_prescrits<-LLEGIR.farmacs_prescrits %>% transmute(idp,cod,dat,dbaixa)
#
#LLEGIR.variables_Cataleg
cataleg_antidiab<-LLEGIR.variables_Cataleg %>% filter(domini=="farmacs_facturats") %>% transmute(cod,agr="AD")



# [dt_diagnostics_global==LLEGIR.cmbdh_diagnostics_padris+LLEGIR.diagnostics]
dt_diagnostics<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))


dt_diagnostics_global<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

#min(dt_diagnostics$dat)
#12.01.1941
#max(dt_diagnostics$dat)
#31.12.2018







