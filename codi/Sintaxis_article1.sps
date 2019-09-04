* Encoding: UTF-8.

******************************************.

cd 'E:\Google Drive\CIBERDEM\GEDAPS\TANCATS\ECONTROL2007-13'.

GET
  FILE='EC0713_TAULA_POB_MAIG15.sav'.


freq any_cohort.


FILTER OFF.
USE ALL.
SELECT IF (any_cohort=2007).
EXECUTE.


recode anysDM (lo thru 1=1) (else=0) into  any1_DM. 


freq any1_DM.

FILTER OFF.
USE ALL.
SELECT IF (any1_DM).
EXECUTE.

freq any1_DM.

FILTER OFF.
USE ALL.
SELECT IF (edat>34.9999).
EXECUTE.

DESCRIPTIVES edat.

freq var =Alguna_CV Filtrado_glomerular2 ic.


CROSSTABS Alguna_CV by ic.

FILTER OFF.
USE ALL.
SELECT IF (Alguna_CV=0 and ic=0).
EXECUTE.

DESCRIPTIVES edat.


freq Filtrado_glomerular2 . 

FILTER OFF.
USE ALL.
SELECT IF (missing(Filtrado_glomerular2) or Filtrado_glomerular2>=5).
EXECUTE.



freq Filtrado_glomerular2 QAC_grup . 

**** S'elimina els QAC>=30 (
** seleccionem QAC<30 / missing(QAC)

FILTER OFF.
USE ALL.
SELECT IF (missing(QAC_grup) or QAC_grup=1).
EXECUTE.


freq Filtrado_glomerular2 QAC_grup . 


***********************************************************************     Agregació events (2007-2013)     ********************************************.

freq ci avc artper ic  .


**********  mortalitat ( ) 
* exitus (data_def)    
* ci avc artper ic 
* ANYCV =(ci avc artper ic)
* MRC = Filtrado_glomerular (<5 :<FG 60) OR QAC_grup>1 (>30)


******************************.


























