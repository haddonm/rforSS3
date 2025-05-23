
library(makehtml)
library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
ddir <- getDBdir()
wdir <- pathtopath(ddir,"/A_CodeR/SA-SS3/")
source(pathtopath(wdir,"ss3_utilities.R")) 


options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

store <- pathtopath(wdir,"snapper/")  # snapper  # Whiting  # garfish
calc <- pathtopath(wdir,"calc/")


pathSS3 <- "C:/Users/malco/Dropbox/A_CodeR/SA-SS3/calc/ss3.exe"
pathSS3

# do_extra <- function(plotreport,extradir,analysis,store,verbose=TRUE,
#                     compare=NULL,paths=NULL) {

printV(basecase)

# Run SS3 scenbario------------------------------------
item <- 4
getCase(index=item,basecase)   # this lists the basecase indices to the screen

#executable <- c("SS","SS","SS","SS","SS","SS","SS3","SS3")
starttime <- Sys.time()
analysis <- getCase(index=item,basecase)  # 
cat("\n\n")
print("New Analysis")
print(analysis)
destination <- paste0(store,analysis,"/")
print(destination)
extradir <- pathtopath(destination,"extra/")
dirExists(extradir)

load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  


  plotreport=plotreport;
  extradir=extradir;
  analysis=analysis; 
  store=store  
  verbose=TRUE; compare=c("GSVBC-CE"); 
  paths=NULL
  
  

  
  
  #  plotreport=plotreport;extradir=extradir;analysis=analysis; store=store  
  #  verbose=TRUE; compare=c("SGBC-S70-M5","SGBC-S75-M5","SGBC-S80-M5","SGBC-S100-M5"); paths=NULL;
  setuphtml(extradir)
  # tables tab-------------------------------
  outsummary <- summarizeSS3(plotreport)
  answer <- round(printV(outsummary$answer),6)
  filename <- paste0(analysis,"_summary_answer.csv")
  addtable(answer,filen=filename,rundir=extradir,category="tables",
           caption=paste0(analysis,"_Quick summary of model outputs."))
  param <- outsummary$param
  pickP <- grep("Main_RecrDev",rownames(param)) 
  filename <- paste0(analysis,"_estimated-parameters_no_Devs.csv")
  addtable(param[-pickP,],filen=filename,rundir=extradir,category="tables",
           caption=paste0(analysis,"_main estimated parameters without the",
                          "recruitment deviates."))
  filename <- paste0(analysis,"_all-estimated-parameters.csv")
  addtable(param,filen=filename,rundir=extradir,category="tables",
           caption=paste0(analysis,"_all estimated parameters sorted by Phase."))
  filename <- paste0(analysis,"_all-NOTestimated-parameters.csv")
  addtable(outsummary$noestpars,filen=filename,rundir=extradir,
           category="tables",caption=paste0(analysis,
                                            "_all estimated parameters sorted by Phase."))
  #summary tab --------
  compscenes <- getreplists(store=store,scenes=analysis,listname="plotreport",
                            verbose=FALSE)
  filename <- paste0(analysis,"_summary.png")
  projout <- projreceffects(compscenes=compscenes,fileout=filename,
                            rundir=extradir,legcex=1.0,
                            startyr=2,console=FALSE)
  addplot(filen=filename,rundir=extradir,category="summary",
          caption="Summary plot of dynamics.")
  # selectivity Tab-------
  filename <- plotselex(plotreport,sex="Female",upbound=0,
                        console=FALSE,rundir=extradir)
  mixsex <- FALSE
  if (length(grep("mixed",filename))) {
    mixsex <- TRUE
    capt <- "Mixed selectivity for each time-block."
  } else {
    capt <- "Female selectivity for each time-block."
  }
  addplot(filen=filename,rundir=extradir,category="selectivity",
          caption=capt)
  again <- grep("mixed",filename)
  if (!mixsex) {
    filename <- plotselex(plotreport,sex="Male",upbound=0,
                          console=FALSE,rundir=extradir)
    addplot(filen=filename,rundir=extradir,category="selectivity",
            caption="Male selectivity for each time-block.")
  }
  # CPUE tab------------------
  cpue <- plotreport$cpue
  filename <- altcpueplot(plotreport$cpue,analysis=analysis,
                          rundir=extradir,height=8,CI=TRUE,console=FALSE)
  addplot(filen=filename,rundir=extradir,category="CPUE",
          caption="Alternative CPUE plot and residuals for each fleet.")
  filename <- "CPUE-table-from-plotreport.csv"
  addtable(cpue,filen=filename,rundir=extradir,category="CPUE",
           caption="CPUE report from plotreport.") 
  # Catch tab-------------------------------
  catches <- getdatasets(plotreport=plotreport,store=store,analysis=analysis,
                         verbose=FALSE)
  flname <- colnames(catches)
  filename <- plotss3catches(catches,extradir=extradir,analysis=analysis,
                             console=FALSE)
  addplot(filen=filename,rundir=extradir,category="catch",
          caption=paste0("Reported catches by Year and fleet.",
                         " Recreational catches are interpolated between surveys"))    
  
  filename <- paste0(analysis,"_Catch-by-Fleet.csv")
  allcatch <- cbind(catches,total=rowSums(catches,na.rm=TRUE))
  addtable(allcatch,filen=filename,rundir=extradir,category="catch",
           caption=paste0("Reported Catch by Fleet. Recreational catches ",
                          "are interpolaterd netween surveys.")) 
  # comparison tab-------------
  if ((!is.null(compare)) | (!is.null(paths))) {
    compscenes <- getreplists(store=store,scenes=compare,paths=paths,
                              listname="plotreport")
    filename <- "Comparison_of_scenarios.png"
    projout <- projreceffects(compscenes=compscenes,fileout=filename,
                              rundir=extradir,legcex=1.0,startyr=2,
                              console=FALSE)
    addplot(filen=filename,rundir=extradir,category="compare",
            caption="Comparison of Scenarios.")
    
    outdepl <- tail(projout$depl,15)
    filename <- "Comparison_Projection_year_depletion.csv"
    addtable(outdepl,filen=filename,rundir=extradir,category="compare",
             caption="Comparison of Final Years' delpetion levels.")
    
    outcat <- tail(projout$totalC,15)
    filename <- "Comparison_Projected_catch_by_scenario.csv"
    addtable(outcat,filen=filename,rundir=extradir,category="compare",
             caption="Comparison of projected catch levels by scenario.")    
    
    # agecomp comparisons  
    if (length(compscenes$total) > 2) {
      warning("Ageproportions of only first two scenarios will be used \n")
    }
    fleetnames <- plotreport$FleetNames
    ageprop1 <- getageprops(compscenes$total[[1]])
    ageprop2 <- getageprops(compscenes$total[[2]])
    agg <- ageprop1$agg
    fleets <- sort(unique(agg[,"Fleet"]))
    nfleet <- length(fleets)
    for (fl in 1 : nfleet) { # fl = 1
      whichfleet <- fleets[fl]
      flname <- fleetnames[whichfleet]
      filename <- plotaggage(agg1=ageprop1$agg,agg2=ageprop2$agg,
                             whichfleet=whichfleet,fleetname=flname,
                             console=FALSE,rundir=extradir,
                             scenarios=compare)
      addplot(filen=filename,rundir=extradir,category="compare",
              caption=paste0("Comparison of Fit to Age Comps aggregated ",
                             "by Year and ",flname,"."))    
      
      filename <- plotageprops(agecomp1=ageprop1,agecomp2=ageprop2,whichfleet=fl,
                               console=FALSE,rundir=extradir,scenarios=compare) 
      addplot(filen=filename,rundir=extradir,category="compare",
              caption=paste0("Comparison of Fit to Age Comps in each year by ",
                             flname))
    }
    # further table of comparisons
    if (nrow(compscenes$total[[1]]$parameters) == 
        nrow(compscenes$total[[2]]$parameters)) {
      outstats <- comparestats(compscenes)
      
      outans <- round(outstats$answer,7)
      filename <- "Comparison_Model_output_scenario.csv"
      addtable(outans,filen=filename,rundir=extradir,category="compare",
               caption="Comparison of model output by scenario.")    
      
      outlik <- round(outstats$likes,6)
      filename <- "Comparison_Model_Likelihoods_scenario.csv"
      addtable(outlik,filen=filename,rundir=extradir,category="compare",
               caption="Comparison of model Likelihoods by scenario.")    
      
      outpar <- round(outstats$param,6)
      filename <- "Comparison_Model_paramters_scenario.csv"
      addtable(outpar,filen=filename,rundir=extradir,category="compare",
               caption="Comparison of model parameters by scenario.")       
      
      outmod <- round(outstats$models,3)
      filename <- "Comparison_Model_structure_scenario.csv"
      addtable(outmod,filen=filename,rundir=extradir,category="compare",
               caption="Comparison of model structure by scenario.") 
    }
  }
  
  make_html(replist=NULL,
            rundir=extradir,
            datadir=NULL,
            width=500,
            openfile=TRUE,
            runnotes=paste0(paste0(compare,collapse="__")," Extra Analyses"),
            verbose=verbose,
            packagename="rforSS3",
            htmlname=paste0(analysis,"_Extra"))
#} # end of do_extra
