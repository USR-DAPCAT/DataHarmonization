#https://github.com/tagteam/heaven/blob/5af7c77cb684c1002509c34b9c454fc024f114c3/R/riskSetMatch.R

# devtools::install_github("tagteam/heaven")
# Lectura de fitxers --------------------
#' @title riskSetMatch - Risk set matching
#' 
#' @description 
#' Risk set matching is common term to represent "incidence density sampling" 
#' or "exposure density sampling". In both cases the request is to match by 
#' a series of variables such that the outcome or exposure data of "controls" 
#' are later than the outcome or exposure for cases.
#' 
#' The current program is based on exact matching and allows the user to 
#' specify a "greedy" approach where controls are only used once as well as 
#' allowing the program to reuse controls and to allow cases to be controls 
#' prior to being a case.
#' 
#' In addition to the exact matching the function and also only select controls
#' where time of covariates are missing for both cases and controls, are both
#' before case index, are both after case index - or are missing for case index
#' and after case index for controls. 
#' 
#' To provide necessary speed for large samples the general technique used is 
#' to create a series of risk-groups that have the fixed matching variables 
#' identical (such as birthyear and gender).  The available controls are then 
#' sorted by a random number and selected for consecutive cases if the 
#' "case-date=case index" is later than the corresponding "control-date=
#' control index". The consecutive selection can in theory cause bias and the 
#' result of the function needs careful attention when the number of controls 
#' is small compared to the number of cases.
#' 
#' @usage
#'    riskSetMatch(ptid,event,terms,dat,Ncontrols,oldevent="oldevent"
#'    ,caseid="caseid",reuseCases=FALSE,reuseControls=FALSE,caseIndex=NULL 
#'    ,controlIndex=NULL,NoIndex=FALSE,cores=1,dateterms=NULL)
#' @author Christian Torp-Pedersen
#' @param ptid  Personal ID variable defining participant
#' @param event Defining cases/controls MUST be integer 0/1 - 0 for controls, 1 for case
#' @param terms c(.....) Specifies the variables that should be matched by - 
#' enclosed in ".."
#' @param dat The single dataset with all information - coerced to data.table
#' if data.frame
#' @param Ncontrols  Number of controls sought for each case
#' @param oldevent Holds original value of event - distinguishes cases used as 
#' controls
#' @param caseid Character. Variable holding grouping variable for 
#' cases/controls (=case-ptid)
#' @param reuseCases Logical. If \code{TRUE} a case can be a control prior to 
#' being a case
#' @param reuseControls Logical. If \code{TRUE} a control can be reused for 
#' several cases
#' @param caseIndex Integer/Date. Date variable defining the date where a case 
#' becomes a case. For a case control study this is the date of event of 
#' interest, for a cohort study the date where a case enters an analysis.
#' @param controlIndex Integer/Date. date variable defining the date from which 
#' a controls can no longer be selected.  The controlIndex must be larger than 
#' the caseIndex.  For a case control study this would be the date where a 
#' control has the event of interest or is censored.  For a cohort study it 
#' would be the date where the control disappears from the analysis, e.g. due to 
#' death or censoring.
#' @param NoIndex Logical. If \code{TRUE} caseIndex/controlIndex are ignored
#' @param cores number of cores to use, default is one
#' @param dateterms c(....) A list of variable neames (character) in "dat" 
#' specifying dates of conditions. When a list is specified it is not only
#' checked that the caseIndex is not after controlIndex, but also that for all
#' variables in the list either both control/case dates are missing, both prior
#' to case index, both after case index - or missing for case and with control
#' date after case index.
#' @details 
#' The function does exact matching and keeps 2 dates (indices) apart such that 
#' the date for controls is larger than that for cases. Because the matching 
#' is exact all matching variables must be integer or character. Make sure that
#' sufficient rounding is done on continuous and semicontinuous variables to 
#' ensure a decent number of controls for each case. For example it may be 
#' difficult to find controls for cases of very high age and age should 
#' therefore often be rounded by 2,3 or 5 years - and extreme ages further 
#' aggregated.
#' 
#' For case control studies age may be a relevant matching parameter - for most 
#' cohort studies year of birth is more relevant since the age of a control 
#' varies with time.
#' 
#' Many datasets have comorbidities as time dependent variables. Matching on
#' these requires that the comorbidity date is not (yet) reached for a corres-
#' ponding variables for cases if the case does not have the comorbidity and 
#' similarly that the date has been reached when the case does have that co-
#' morbidity.
#' 
#' For many purposes controls should be reused and cases allowed to be controls 
#' prior to being cases. By default, there is no reuse and this can be adjusted 
#' with "reuseCases" and "reuseControls"
#' 
#' The function can be used for standard matching without the caseIndex/
#' controlIndex (with "NoIndex"), but other packages such as MatchIt are more 
#' likely to be more optimal for these cases.
#' 
#' It may appear tempting always to use multiple cores, but this comes with a 
#' costly overhead because the function machinery has to be distributed to each 
#' defined "worker".  With very large numbers of cases and controls, multiple
#' cores can save substantial amounts of time. When a single core is used a 
#' progress shows progress of matching. There is no progress bar with multiple 
#' cores
#' 
#' The function matchReport may afterwards be used to provide simple summaries 
#' of use of cases and controls
#' @return data.table with cases and controls. After matching, a new variable 
#' "caseid" links controls to cases. Further, a variable "oldevent" holds the 
#' orginal value of "event" - to be used to identify cases functioning
#' as controls prior to being cases.
#' 
#' Variables in the original dataset are preserved. The final dataset includes 
#' all original cases but only the controls that were selected. 
#' 
#' If cases without controls should be removed, this is done by setting the
#' variable removeNoControls to TRUE
#' 
#' @seealso matchReport Matchit
#' 
#' @export
#'
#' @examples
#' require(data.table)
#' case <- c(rep(0,40),rep(1,15)) 
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#' byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#' case.Index <- c(seq(1,40,1),seq(5,47,3))
#' control.Index <- case.Index
#' diabetes <- seq(2,110,2)
#' heartdis <- seq(110,2,-2)
#' diabetes <- c(rep(1,55))
#' heartdis <- c(rep(100,55))
#' library(data.table)
#' dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,control.Index)
#' # Very simple match without reuse - no dates to control for
#' out <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,NoIndex=TRUE)
#' out[]
#' # Risk set matching without reusing cases/controls - 
#' # Some cases have no controls
#' out2 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex="case.Index",
#'   controlIndex="control.Index")
#' out2[]   
#' # Risk set matching with reuse of cases (control prior to case) and reuse of 
#' # controls - more cases get controls
#' out3 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "case.Index",controlIndex="control.Index"
#'   ,reuseCases=TRUE,reuseControls=TRUE)
#' out3[]   
#' # Same with 2 cores
#' out4 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "case.Index",controlIndex="control.Index"
#'   ,reuseCases=TRUE,reuseControls=TRUE,cores=2)  
#' out4[]     
#' #Time dependent matching. In addtion to fixed matching parameters there are
#' #two other sets of dates where it is required that if a case has that condi-
#' #tion prior to index, then controls also need to have the condition prior to
#' #the case index to be eligible - and if the control does not have the condi-
#' #tion prior to index then the same is required for the control.
#' out5 <- riskSetMatch("ptid","case",c("byear","sex"),dat,2,caseIndex=
#'   "case.Index",controlIndex="control.Index"
#'   ,reuseCases=TRUE,reuseControls=TRUE,cores=1,
#'   dateterms=c("diabetes","heartdis"))  
#' out5[]   
#' 
#' #POSTPROCESSING
#' #It may be convinient to add the number of controls found to each case in or-
#' #der to remove cases without controls or where very few controls have been
#' #found.  This is easily obtained using data.table - with the example above:
#' #out5[,numControls:=.N,by=caseid] # adds a column with the number of controls
#'                                  # for each case-ID     
riskSetMatch <- function(ptid     # Unique patient identifier
                         ,event   # 0=Control, 1=case
                         ,terms   # terms c("n1","n2",...) - list of vairables to match by
                         ,dat     # dataset with all variables
                         ,Ncontrols  # number of controls to provide
                         ,oldevent="oldevent" # To distinguish cases used as controls
                         ,caseid="caseid" # variable to group cases and controls (case-ptid)
                         ,reuseCases=FALSE # T og F or NULL - can a case be a control prior to being a case?
                         ,reuseControls=FALSE # T or F or NULL - can controls be reused?
                         ,caseIndex=NULL      # Integer or date, date where controls must be prior
                         ,controlIndex=NULL   # controlIndex - Index date for controls
                         ,NoIndex=FALSE      # If T ignore index
                         ,cores=1 # Number of cores to use, default 1
                         ,dateterms=NULL # character list of date variables
){ 
  .SD=Internal.ptid=pnrnum=cterms=Internal.event=Internal.cterms=label=Internal.event=pnrnum=
    random=.N=Internal.controlIndex=Internal.caseIndex=random=Internal.controlIndex=Internal.caseIndex=NULL
  #check
  if (!is.character(ptid) | !is.character(event) | (!is.null(caseIndex) & !is.character (caseIndex)) |
      (!is.null(dateterms) & !is.character(dateterms))   |
      (!is.null(controlIndex) & !is.character(controlIndex))) stop (" Variables names must be character")
  setDT(dat) # coerce to data.table if necessary
  if(!is.integer(cores) & !(is.numeric(cores))) stop("cores must be integer, default 1")
  cores <- as.integer(cores)
  # copy input data
  datt <- copy(dat)
  datt[,"oldevent":=.SD,.SDcols=event]  #Remeber status of event before match - when cases can turn out as controls
  if (NoIndex) noindex <- 1L else noindex <- 0L # allows omitting index vectors
  setnames(datt,ptid,"Internal.ptid")
  #Check that patient IDs are unique:
  repetitians <- length(datt[,Internal.ptid])-length(unique(datt[,Internal.ptid]))
  if (repetitians>0) stop(paste(" Error, participant ID not unique. Number of repeats:",repetitians))
  datt[,pnrnum:=1:.N]
  # combine matching variables to single term - cterms
  datt[, cterms :=do.call(paste0,.SD),.SDcols=terms] 
  # Select relevant part of table for matching
  cols <-c("pnrnum",event,"cterms")
  if(!NoIndex) cols <-c("pnrnum",caseIndex,controlIndex,event,"cterms")
  if(!NoIndex & !is.null(dateterms)) cols <- c("pnrnum",caseIndex,controlIndex,event,"cterms",dateterms)
  alldata <- datt[,.SD,.SDcols=cols]
  # Rename variables
  if(!NoIndex) RcaseIndex <- caseIndex # remember name of caseIndex
  if(NoIndex) setnames(alldata,cols,c("pnrnum","Internal.event","Internal.cterms"))
  else
    if (!NoIndex & is.null(dateterms)) setnames(alldata,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms"))
  else
    if (!NoIndex & !is.null(dateterms)){
      Internal.dateterms <- paste0("V",seq(1,length(dateterms)))
      setnames(alldata,c("pnrnum","Internal.caseIndex","InternalInternal.controlIndex","Internal.event","Internal.cterms",Internal.dateterms)) 
      alldata[,(Internal.dateterms):=lapply(.SD,as.integer),.SDcols=Internal.dateterms]
    }
  # prepare to split 
  setkey(alldata,Internal.cterms)
  split.alldata <- split(alldata,by="Internal.cterms") # Now a list aplit by Internal.cterms
  if (cores<2){
    # Prepare progress bar
    totalprogress <-as.numeric(length(split.alldata)/1000)
    pb <-txtProgressBar(min = 0, max = 1000, initial = 0, char = "=",
                        width = NA, title, label, style = 1, file = "")
    progress <- 0;
    # Select controls - rbind of each split-member that selects controls
    selected.controls <- do.call("rbind",lapply(split.alldata,function(controls){ # Function handles each split-group, afterward rbind
      # Setnames because data.table called from function
      if (!NoIndex & is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms"))
      else
        if (!NoIndex & !is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms",Internal.dateterms))
      else
        if (NoIndex) setnames(controls,c("pnrnum","Internal.event","Internal.cterms"))
      setkey(controls,Internal.event,pnrnum)
      # Define cases in selected match-group
      cases <- controls[Internal.event==1]
      setkey(cases,pnrnum)
      # If cases cannot become controls they are removed from controls
      if (!reuseCases) controls <- subset(controls,Internal.event==0)
      #find lengths of controls and cases
      Tcontrols<-dim(controls)[1]
      Ncases<-dim(cases)[1]
      # sort controls by random number - so that they can be selected sequentially
      set.seed(17)
      controls[,random:=runif(.N,1,Ncontrols*10)]
      setkey(controls,random)
      NreuseControls <- as.numeric(reuseControls) # Integerlogic for Rcpp
      #Length of dateterm vector
      if(!is.null(dateterms)) Ndateterms=length(dateterms) else Ndateterms <- 0
      if(!NoIndex){
        control.date <- controls[,Internal.controlIndex]
        case.date <- cases[,Internal.caseIndex]
      } else {
        control.date <- 0L
        case.date <- 0L
      }
      #dateterm matrix
      if(!is.null(dateterms)){
        dates.cases <- as.matrix(cases[,.SD,.SDcols=Internal.dateterms])
        dates.controls <- as.matrix(controls[,.SD,.SDcols=Internal.dateterms])
      } 
      else {
        dates.cases <- as.matrix(0)
        dates.controls <- as.matrix(0)
      }
      Output <- .Call('_heaven_Matcher',PACKAGE = 'heaven',Ncontrols,Tcontrols,Ncases,NreuseControls,
                      control.date,case.date,controls[,pnrnum],cases[,pnrnum],
                      Ndateterms,dates.cases,dates.controls,noindex)
      setDT(Output)
      progress <<- progress+1/totalprogress
      #Progress bar
      setTxtProgressBar(pb,progress)
      flush(stdout())
      Output
    })) # end function and do.call
  } #end if cores<2
  else {
    CLUST <- parallel::makeCluster(min(parallel::detectCores(),cores))
    selected.controls <- do.call(rbind,foreach::foreach(controls=split.alldata,.packages=c("heaven"),.export=c("reuseControls")) %dopar% {
      # Setnames because data.table called from function
      if (!NoIndex & is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms"))
      else
        if (!NoIndex & !is.null(dateterms)) setnames(controls,c("pnrnum","Internal.caseIndex","Internal.controlIndex","Internal.event","Internal.cterms",Internal.dateterms))
      else
        if (NoIndex) setnames(controls,c("pnrnum","Internal.event","Internal.cterms"))
      setkey(controls,Internal.event,pnrnum)
      # Define cases in selected match-group
      cases <- controls[Internal.event==1]
      setkey(cases,pnrnum)
      # If cases cannot become controls they are removed from controls
      if (!reuseCases) controls <- subset(controls,Internal.event==0)
      #find lengths of controls and cases
      Tcontrols<-dim(controls)[1]
      Ncases<-dim(cases)[1]
      # sort controls by random number - so that they can be selected sequentially
      set.seed(17)
      controls[,random:=runif(.N,1,Ncontrols*10)]
      setkey(controls,random)
      NreuseControls <- as.numeric(reuseControls) # Integerlogic for Rcpp
      #Length of dateterm vector
      if(!is.null(dateterms)) Ndateterms=length(dateterms) else Ndateterms <- 0
      if(!NoIndex){
        control.date <- controls[,Internal.controlIndex]
        case.date <- cases[,Internal.caseIndex]
      } else {
        control.date <- 0L
        case.date <- 0L
      }
      #dateterm matrix
      if(!is.null(dateterms)){
        dates.cases <- as.matrix(cases[,.SD,.SDcols=Internal.dateterms])
        dates.controls <- as.matrix(controls[,.SD,.SDcols=Internal.dateterms])
      } 
      else {
        dates.cases <- as.matrix(0)
        dates.controls <- as.matrix(0)
      }
      Output <- .Call('_heaven_Matcher',PACKAGE = 'heaven',Ncontrols,Tcontrols,Ncases,NreuseControls,
                      control.date,case.date,controls[,pnrnum],cases[,pnrnum],
                      Ndateterms,dates.cases,dates.controls,noindex)
      setDT(Output)
      Output
    })  
    parallel::stopCluster(CLUST)
    setDT(selected.controls)
  }  #end cores>1
  setnames(selected.controls,c(caseid,"pnrnum"))
  selected.controls[,Internal.event:=0]
  setkey(alldata,Internal.event)
  cases <- alldata[Internal.event==1]
  cases[,caseid:=pnrnum]
  # Create final dataset with cases and controls
  FINAL <- rbind(cases[,list(pnrnum,caseid,Internal.event)],selected.controls[,data.table::data.table(pnrnum,caseid,Internal.event)])
  setkey(FINAL)
  #output
  datt[,(event):=NULL]
  FINAL <- merge(FINAL,datt,by="pnrnum")
  FINAL[,c("cterms","pnrnum"):=NULL] # remove cterms - aggregated terms
  setnames(FINAL,"Internal.ptid",ptid)
  setkeyv(FINAL,c(caseid,"Internal.event"))
  #Add relevant caseid to controls
  if (!NoIndex) FINAL[,eval(caseIndex):=.SD[.N],.SDcols=c(caseIndex),by=caseid]
  setnames(FINAL,"Internal.event",event)
  FINAL 
}
#--------------------------------#



##16.12.2019

#install.packages("remotes")
#remotes::install_github("tagteam/heaven")
#--------------------------------#
#library(heaven)
#heaven::riskSetMatch

# Per installar llibreria heaven::riskSetMatch

#noooo

#library("githubinstall")
#githubinstall("heaven",ref="964bbbd",force=T) # "2018.8.9"

#tagteam/heaven


#--------------------------------##--------------------------------#
#Hola, et comento 
#He estat investigant també, és el que dius tu, 
#van fer dos llibreries més incidenceMatch() etc, i en les noves versions han eliminat la funció riskSetMatch()
#Ho he provat amb les meves dades i les coses que canvien són:
# 1. Els noms dels parámetres canvien, i han alguns arguments opocionals .....com la llavor ,etc..., 
# 2. Va molt més lent ,( no sé perqué)
# 3. S'ha de vigilar amb els noms dels arguments dels paràmetres ja que si són els mateixos que el data.table que et genera, peta (p.e. el nom <event> no es pot posar com a paràmetre)
# 4. No et genera la data index pels grups a risc per tant després l'has d'assignar als grups a risc (casos i controls)
# 4. M'ha trobat uns quants més controls que l'anterior funció però ha tardat molt més. , nose si es per la llavor o que...
# 5. De totes maneres si vols utilitzar la versió antiga pots descarregar la llibreria amb el commit 964bbbd
# Installar versió de github 964bbbd (2018.08.09)

#githubinstall("heaven",ref="964bbbd",force=T) 

#Qualsevol dubte em truques
#Fins aviat 
#--------------------------------##--------------------------------#
#install.packages("heaven")
#--------------------------------------------------------------------------#
#library("heaven")
#--------------------------------------------------------------------------#
#githubinstall("heaven",ref="964bbbd",force=T)
#--------------------------------------------------------------------------#
# 1. Lectura de fitxers 
#memory.size(max=160685)
memory.limit()
#
# Directori Font     ==============================  

#--------------------------------------------------------------------------#
link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)
#--------------------------------------------------------------------------#
#--------------------------------#
#testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320
#--------------------------------#

# generar_mostra_fitxers()  ----------------------

# Llegir tots els fitxers RDS dins d'un directori i generar una mostra aleatoria i salvar-lo en un directori "mostra"


#i        [cmbdh_diagnostics] mult
LLEGIR.cmbdh_diagnostics_padris<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_cmbdh_diagnostics_padris_20190930_093320.rds")) %>% as_tibble()
variable.names(LLEGIR.cmbdh_diagnostics_padris)
#[1] "idp"    "cod"    "dat"    "dx_pos" "dalta"  "calta"  "agr" 
#----------------------------------------------#
#ii       [dianostics]  mult
LLEGIR.diagnostics<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_diagnostics_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.diagnostics)
#[1] "idp"    "cod"    "dat"    "dbaixa" "agr"   
#----------------------------------------------#

#iii      [farmacs_facturats] mult
LLEGIR.farmacs_facturat<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_farmacs_facturats_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_facturat)
#[1]"idp" "cod" "dat" "agr" "env"
#----------------------------------------------#
#iv       [farmacs_prescrits] mult
LLEGIR.farmacs_prescrits<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_farmacs_prescrits_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.farmacs_prescrits)
#[1] "idp"    "cod"    "dat"    "dbaixa" "ics"    "ap"     "agr"
#----------------------------------------------#

#v        [població] unic
LLEGIR.poblacio<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_poblacio_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.poblacio)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#----------------------------------------------#
#vi       [tabaquisme] unic
LLEGIR.tabaquisme<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_tabaquisme_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.tabaquisme)
#[1] "idp"    "val"    "dat"    "dbaixa"
#----------------------------------------------#

#vii      [analitiques] mult
LLEGIR.variables_analitiques<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_analitiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_analitiques)
#[1] "idp" "cod" "dat" "val" "agr"
#----------------------------------------------#
#viii     [variables_cliíniques] mult
LLEGIR.variables_cliniques<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_cliniques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_cliniques)
#[1] "idp" "cod" "dat" "val" "agr"
#----------------------------------------------#

#ix       [variables geo_sanitàries] unic
LLEGIR.variables_geo_sanitaries<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_geo_sanitaries_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_geo_sanitaries)
#[1] "idp"     "idup"    "idabs"   "idrs"    "iddap"   "idambit"  
#----------------------------------------------#  
#x        [variables socioeconòmiques] unic
LLEGIR.variables_socioeconomiques<-readRDS("dades/SIDIAP/test" %>% here::here("testDAPCRMM_entregable_variables_socioeconomiques_20190926_103409.rds")) %>% as_tibble()
variable.names(LLEGIR.variables_socioeconomiques)
#[1] "idp"       "qmedea"    "ruralitat"

                                                  #####################
                                                  #Cohort define steps#
                                                  #####################

#1.	  From the entire database (ED), extract T2DM cohort with any codes in sheet “exposed”; 
#     then remove patients with any codes in sheet “exclude” before the end of the study (31/12/2018); 
#     use the first appearance diagnosis code of T2DM as the index date for exposed patient cohort (T2C).
#     --------------------------------------------------------------------------------------------------------
#     De tota la base de dades (ED), extreu la cohort T2DM amb els codis del full "exposat"; 
#     després elimineu els pacients amb els codis del full "excloure" abans de finalitzar l'estudi (31/12/2018);
#     utilitzeu el codi de diagnòstic de primera aparició de T2DM com a data índex de la cohort del pacient exposada (T2C). 
#     --------------------------------------------------------------------------------------------------------
#2.	  From this cohort (T2C), 
#     remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; 
#     this is the exposed cohort (EC).
#     --------------------------------------------------------------------------------------------------------
#     D’aquesta cohort (T2C), traieu els pacients amb els codis del full “prevalent” o “càncer” 
#     que apareixin abans de la data de l’índex; es tracta de la cohort exposada (CE). 
#     --------------------------------------------------------------------------------------------------------
#3.	  From the entire database (ED), 
#     remove patients with any codes in sheet “non-exposed pool” before the end of the study (31/12/2018), 
#     to get the candidate non-exposed patients (CNE).
#     --------------------------------------------------------------------------------------------------------
#     De tota la base de dades (ED), elimineu els pacients amb els codis del full "piscina no exposada" 
#     abans de finalitzar l'estudi (31/12/2018) per obtenir els pacients candidats no exposats (CNE). 
#     --------------------------------------------------------------------------------------------------------
#4.	  Exact matching the exposed cohort (EC) to the candidate non-exposed patients (CNE) with a ratio 1:10 (EC:CNE)
#     by year of birth (+/-1year), sex, and practice, 
#     without replacement (each candidate non-exposed patient can be only matched once). 
#     This is the matched cohort (MC), and the index date is the same as the matched exposed patient.
#     --------------------------------------------------------------------------------------------------------
#     Correspondre exactament a la cohort exposada (CE)
#     als pacients candidats no exposats (CNE) amb una proporció 1:10 (EC: CNE) 
#     per any de naixement (+/- 1 any), sexe i pràctica, sense substitució 
#     ( cada pacient candidat no exposat només es pot combinar una vegada). Aquesta és la cohort coincident (MC)
#     i la data de l’índex és la mateixa que el pacient exposat igualat. 
#     --------------------------------------------------------------------------------------------------------
#5.	  From the matched cohort (MC), remove patients died before the index date; 
#     then remove patients with any codes in sheet “prevalent” or “cancer” appearing before the index date; 
#     then keep a randomly selected 5 matched non-exposed patients 
#     (good to set a seed to make the random selection replicable). This is the final non-exposed cohort (FNE).
#     --------------------------------------------------------------------------------------------------------
#     De la cohort aparellada (MC), elimineu els pacients morts abans de la data de l’índex; 
#     a continuació, elimineu els pacients amb qualsevol codi en el full "prevalent" o "càncer" 
#     que aparegui abans de la data de l'índex; a continuació, 
#     mantingueu a 5 pacients no exposats seleccionats aleatòriament
#     (és bo establir una llavor perquè la selecció aleatòria sigui replicable). 
#     Es tracta de la cohort final no exposada (FNE).
#     --------------------------------------------------------------------------------------------------------
#6.	  The final study cohort is the combination of the exposed cohort (EC) and the final non-exposed cohort (FNE).
#     --------------------------------------------------------------------------------------------------------
#     La cohort d'estudi final és la combinació de la cohorte exposada (EC) i la cohort final no exposada (FNE)
#     --------------------------------------------------------------------------------------------------------



##############################################################################################################################
# Generar data index -----------
dt_diagnostics<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))

dt_diagnostics_global<-LLEGIR.cmbdh_diagnostics_padris %>% 
  transmute(idp,cod=as.character(cod),dat,agr) %>% 
  bind_rows(select(LLEGIR.diagnostics,idp,cod,dat,agr))


#data minima[]#
dt_cataleg<-read_excel("Spain_codes.xls") %>% select(cod,agr,exposed)

dt_diagnostics<-dt_diagnostics %>% left_join(dt_cataleg,by="cod") %>% filter(exposed=="exposed") 


#-----#
DINDEX<-dt_diagnostics%>% group_by(idp)%>%summarise(data_index=min(dat,na.rm = TRUE))%>%ungroup()
DINDEX
##############################################################################################################################
#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una base de dades dels exposat( TOTS ELS DELS DINDEX!!), amb ANY DE NAIXAMENT+SEXE]
#[[població]] 
# C_EXPOSATS<-DINDEX%>%left_join(LLEGIR.poblacio,by="idp")%>%mutate(Edat=as.numeric(lubridate::ymd(data_index)-lubridate::ymd(dnaix))/365.25 )
C_EXPOSATS<-DINDEX%>%left_join(LLEGIR.poblacio,by="idp")
C_EXPOSATS<-C_EXPOSATS%>%filter(entrada<=20181231) #Excluits entrada després de (31/12/2018)
variable.names(C_EXPOSATS)
#[1] "idp"        "data_index" "sexe"       "dnaix"      "entrada"    "sortida"    "situacio"   "Edat"  
#-----------------------------------------------------------------------------------------------------------------------#
#[ Crearé una altre base de dades , que seran els No exposats,tots menys els exposats!!]
C_NO_EXPOSATS<-LLEGIR.poblacio %>% anti_join(C_EXPOSATS,by="idp")
C_NO_EXPOSATS<-C_NO_EXPOSATS%>%filter(entrada<=20181231) #Excluits entrada després de (31/12/2018)
variable.names(C_NO_EXPOSATS)
#[1]  "idp"      "sexe"     "dnaix"    "entrada"  "sortida"  "situacio"
#-----------------------------------------------------------------------------------------------------------------------#


# Fusionar base de dades en dues : 

dt_matching<-mutate(C_EXPOSATS,grup=1) %>% bind_rows(mutate(C_NO_EXPOSATS,grup=0))

# Preparar matching i setriskmatching #
dt_matching<-dt_matching %>% transmute(idp,dnaix,sexe,grup,dtevent=data_index,sortida)

#   5.2.1 Generar data de sortida (Data event / Data de censura)     -----------------
## dtindex_case 
dt_matching<-dt_matching %>% mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament i grups cada 10 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 


#### Parametres d'aparellament
llistaPS=c("sexe","any_naix")
num_controls<-10
llavor<-125
set.seed(llavor)



# 5.4.1. Aplicar algoritme   -----------
dades_match<-riskSetMatch(ptid="idp"                                # Unique patient identifier
                                  ,event="grup"                # 0=Control, 1=case
                                  ,terms=llistaPS   # terms c("n1","n2",...) - list of vairables to match by
                                  ,dat=dt_matching             # dataset with all variables
                                  ,Ncontrols=num_controls       # number of controls to provide
                                  ,oldevent="oldevent"          # To distinguish cases used as controls
                                  ,caseid="caseid"              # variable to group cases and controls (case-ptid)
                                  ,reuseCases=F                 # T og F or NULL - can a case be a control prior to being a case?
                                  ,reuseControls=F              # T or F or NULL - can controls be reused?
                                  ,caseIndex="dtindex_case"       # Integer or date, date where controls must be prior
                                  ,controlIndex="dtindex_control" # controlIndex - Index date for controls
                                  ,NoIndex=FALSE                # If T ignore index
                                  ,cores=1                      # Number of cores to use, default 1
                                  ,dateterms=NULL               # character list of date variables
)
#falta heaven_Matcher


# Report matchreport -------------
heaven::matchReport(dades_match, id="idp",case="grup",caseid="caseid")

# Número de controls per conjunt a risk  ------------
dades_match[,numControls:=.N,by=caseid]
dades_match<- dades_match %>% mutate(numControls=numControls-1)


# Verificació d'aparellament per edad + sexe 
descrTable(grup~dnaix+any_naix+sexe,data=dt_matching)
descrTable(grup~dnaix+any_naix+sexe,data=dades_match)


# Selecciono
dt_index_match<-dades_match %>% transmute(idp,caseid,grup,dnaix,sexe,dtindex=dtindex_case,numControls) %>% as_tibble()


# Agregar problemes de salut utilitzant com a referencia la data index nova

bd_index<-dt_index_match %>% transmute(idp,dtindex=lubridate::as_date(dtindex))

dt_agregada_agr<-agregar_problemes(select(dt_diagnostics_global,idp,cod,dat),bd.dindex = bd_index,dt.agregadors=select(dt_cataleg,cod,agr))


# Filtrar per exclusions (Eliminar prevalents i cancer)
dt_index_match<-dt_index_match %>% left_join(select(dt_agregada_agr,-dtindex),by="idp") %>% 
  filter(is.na(DG.prevalent) & is.na(DG.cancer))



# Fer-ho!
# Selecciona 5 random 1:5 

table(dt_index_match$numControls,dt_index_match$grup)


































































