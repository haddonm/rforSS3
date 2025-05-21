


#' @title do_extra generates extra plots and tables to complement r4ss outputs
#' 
#' @description do_extra generates extra plots and tables relating to a given
#'     scenario (analysis). It produces a new internal website for viewing the
#'     new results.
#'
#' @param plotreport the output from SS_output
#' @param extradir the path to the extra directory into which to place the new
#'     tables and plots. In my system = store + analysis + extra
#' @param analysis the name of the SS3 model being explored
#' @param store the full path to the directory within which the analysis 
#'     sub-directory is held
#' @param verbose Should warnings and progress be reported to the console?
#'     default = TRUE. 
#' @param compare default = NULL. If multiple scenarios or analyses are to be
#'     compared if they are all stored in the same 'store' then this should be
#'     a character vector of which analyses to compare. If no 
#'     comparisons are to be made then leave as NULL.
#' @param paths If different locations are used to store the analyses to be 
#'     compared then this should be a character vector of the full paths. If no 
#'     comparisons are to be made then leave as NULL. 
#'
#' @returns nothing but it does generate an array of plots and tables inserted
#'     into extradir
#' @export
#'
#' @examples
#' # syntax:
#' # do_extra(plotreport=plotreport,extradir=extradir,analysis=analysis,
#' #          store=store,compare=c("BC-priors","BC-noage"))
do_extra <- function(plotreport,extradir,analysis,store,compare=NULL,
                     paths=NULL,verbose=TRUE) {
  #  plotreport=plotreport;extradir=extradir;analysis=analysis; store=store  
  #  verbose=TRUE; compare=c("BC-priors","BC-noage"); paths=NULL;
  setuphtml(extradir)
  # age-Length keys
  destination <- pathtopath(store,analysis)
  datfile <- pathtopath(destination,paste0(analysis,".dat"))
  dat <- SS_readdat_3.30(file=datfile,verbose = FALSE,section = NULL)
  if (dat$N_agebins > 0) {
    outscene <- getagelenkeys(dat)
    console <- FALSE
    verbose <- FALSE
    if (outscene$nscene > 8) {
      nscene <- outscene$nscene
      iter <- ceiling(outscene$nscene / 8)
      pickscene <- c(1:8)
      for (i in 1:iter) {
        plotagelenkey(outcomp=outscene,rundir=extradir,plotscenes=pickscene,pch=1,
                      pchcex=1.25,pchcol=2,console=console,verbose=verbose)
        pickscene <- pickscene + 8
        pickpick <- which(pickscene <= nscene)
        pickscene <- pickscene[pickpick]
        if ((i < iter) & (console)) readline(prompt="Press [enter] to continue")
      }
    } else {
      plotagelenkey(outcomp=outscene,rundir=extradir,plotscenes=NULL,pch=1,
                    pchcex=1.25,pchcol=2,console=console,verbose=verbose)
    }
  } else {
    warning(cat("No ageing data found for the primary analysis \n"))
  }
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
    if (compscenes$dimcheck) {
      filename <- "Comparison_of_scenarios.png"
      projout <- projreceffects(compscenes=compscenes,fileout=filename,
                                rundir=extradir,legcex=1.0,startyr=2,
                                console=FALSE)
      addplot(filen=filename,rundir=extradir,category="compare",
              caption="Comparison of Scenarios.")
    }
    outdepl <- tail(projout$depl,15)
    filename <- "Comparison_Projection_year_depletion.csv"
    addtable(outdepl,filen=filename,rundir=extradir,category="compare",
             caption="Comparison of Final Years' delpetion levels.")
    
    outcat <- tail(projout$totalC,15)
    filename <- "Comparison_Projected_catch_by_scenario.csv"
    addtable(outcat,filen=filename,rundir=extradir,category="compare",
             caption="Comparison of projected catch levels by scenario.")    
    
    # agecomp comparisons  
    if (dat$N_agebins > 0) {
      if (length(compscenes$total) > 2) {
        warning("Ageproportions of only first two scenarios will be used \n")
      }
      if (compscenes$dimcheck) {
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
      } else { warning(cat("Age comparisons not made Scenarios have different ",
                           "structures.  \n"))
      }
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
  if (length(compscenes$total) > 1) return(invisible(compscenes))
} # end of do_extra



