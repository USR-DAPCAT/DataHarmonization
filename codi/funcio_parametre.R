
parametres_directori<-function(T) {
  if (mostra) dir_dades<-"dades_test" else dir_dades<-"dades"
  if (mostra) dir_output<-"output_test" else dir_output<-"output"
  if (mostra) dir_images<-"images_test" else dir_images<-"images"
  if (mostra) fitxer_Rdata<-"DataHarmonization_test.Rdata" else fitxer_Rdata<-"DataHarmonization_test.Rdata"
  list(dir_dades=dir_dades,dir_output=dir_output,fitxer_Rdata=fitxer_Rdata,dir_images=dir_images)
}