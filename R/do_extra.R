


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
#' @param linear should linear models be fitted to conditional age-length data,
#'     default=FALSE
#'
#' @returns nothing but it does generate an array of plots and tables inserted
#'     into extradir
#' @export
#'
#' @examples
#' # syntax:
#' # do_extra(plotreport=plotreport,extradir=extradir,analysis=analysis,
#' #          store=store)
do_extra <- function(plotreport,extradir,analysis,store,verbose=TRUE,
                     compare=NULL,paths=NULL,linear=FALSE) {
  #  plotreport=plotreport;extradir=extradir;analysis=analysis; store=store  
  #  verbose=TRUE; compare=NULL; paths=NULL;linear=FALSE
  setuphtml(extradir)
  destination <- pathtopath(store,analysis)
  datfile <- pathtopath(destination,paste0(analysis,".dat"))
  dat <- SS_readdat_3.30(file=datfile,verbose = FALSE,section = NULL)
  console <- FALSE
  # age-Length keys
  # destination <- pathtopath(store,analysis)
  # datfile <- pathtopath(destination,paste0(analysis,".dat"))
  # dat <- SS_readdat_3.30(file=datfile,verbose = FALSE,section = NULL)
  # if (!plotreport$growthvaries) {
  #   LAA <- plotreport$growthseries[1,]
  #   
  # }
  # if (dat$N_agebins > 0) {
  #   outscene <- suppressWarnings(getagelenkeys(dat))
  #   console <- FALSE
  #   verbose <- FALSE
  #   if (!is.null(outscene)) {
  #     if (outscene$nscene > 8) {
  #       nscene <- outscene$nscene
  #       iter <- ceiling(outscene$nscene / 8)
  #       pickscene <- c(1:8)
  #       for (i in 1:iter) {
  #         plotagelenkey(outcomp=outscene,rundir=extradir,plotscenes=pickscene,pch=1,
  #                       pchcex=1.25,pchcol=2,console=console,verbose=verbose)
  #         pickscene <- pickscene + 8
  #         pickpick <- which(pickscene <= nscene)
  #         pickscene <- pickscene[pickpick]
  #         if ((i < iter) & (console)) readline(prompt="Press [enter] to continue")
  #       }
  #     } else {
  #       plotagelenkey(outcomp=outscene,rundir=extradir,plotscenes=NULL,pch=1,
  #                     pchcex=1.25,pchcol=2,console=console,verbose=verbose)
  #     }
  #   }
  # } else {
  #   warning(cat("No conditional age-at-length data found for the primary analysis \n"))
  # }
  # tables tab-------------------------------
  if (verbose) cat("Generating tables tab \n")
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
  if (verbose) cat("Generating summary tab \n")
  compscenes <- getreplists(store=store,scenes=analysis,listname="plotreport",
                            verbose=FALSE)
  filename <- paste0(analysis,"_summary.png")
  projout <- projreceffects(compscenes=compscenes,fileout=filename,
                            rundir=extradir,legcex=1.0,
                            startyr=2,console=console)
  addplot(filen=filename,rundir=extradir,category="summary",
          caption="Summary plot of dynamics.")
  dyn <- cbind(projout$spawnB,projout$totalC,projout$recruits,projout$depl)
  colnames(dyn) <- c("spawnB","totalC","recruits","depletion")
  filename <- paste0(analysis,"_alldynamics.csv")
  addtable(round(dyn,3),filen=filename,rundir=extradir,
           category="summary",
           caption="Predicted dynamics including the projections.")
#  times <- plotreport$timeseries
  # selectivity Tab-------
  if (verbose) cat("Generating selectivity tab \n")
  filename <- plotselex(plotreport,sex="Female",upbound=0,
                        console=console,rundir=extradir)
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
                          console=console,rundir=extradir)
    addplot(filen=filename,rundir=extradir,category="selectivity",
            caption="Male selectivity for each time-block.")
  }
  # CPUE tab------------------
  if (verbose) cat("Generating CPUE tab \n")
  cpue <- plotreport$cpue
  filename <- altcpueplot(plotreport$cpue,analysis=analysis,
                          rundir=extradir,height=8,CI=TRUE,console=console)
  addplot(filen=filename,rundir=extradir,category="CPUE",
          caption="Alternative CPUE plot and residuals for each fleet.")
  filename <- "CPUE-table-from-plotreport.csv"
  addtable(cpue,filen=filename,rundir=extradir,category="CPUE",
           caption="CPUE report from plotreport.") 
  
  
  # Catch tab-------------------------------
  if (verbose) cat("Generating Catch tab \n")
  catches <- getdatasets(plotreport=plotreport,store=store,analysis=analysis,
                         verbose=FALSE)
  flname <- colnames(catches)
  filename <- plotss3catches(catches,extradir=extradir,analysis=analysis,
                             console=console)
  addplot(filen=filename,rundir=extradir,category="catch",
          caption=paste0("Reported catches by Year and fleet.",
                   " Recreational catches are interpolated between surveys",
                         " The red line in lowe plot is a loess curve"))    
  
  filename <- paste0(analysis,"_Catch-by-Fleet.csv")
  allcatch <- cbind(catches,total=rowSums(catches,na.rm=TRUE))
  addtable(allcatch,filen=filename,rundir=extradir,category="catch",
           caption=paste0("Reported Catch by Fleet. Recreational catches ",
                          "are interpolated netween surveys.")) 
  # CAAL tab-----------------------------
  if (verbose) cat("Generating CAAL tab \n")
  compdat <- getalcomp(store=store,analysis=analysis)
  if (!is.null(compdat)) {
    compfem <- compdat$compfem
    compmal <- compdat$compmal
    fleets <- compdat$fleets    
    nfleet <- length(fleets)
    for (i in 1:nfleet) { # i=1
      pickflt <- which(compfem$fleet == i)
      alout <- plotagecomp(compsex=compfem[pickflt,],gender="Female",
                           fleet=fleets[i],rescale=0.0,console=console,
                           plotdir=extradir,plotout=TRUE)
      filename <- alout$filen
      addplot(filen=filename,rundir=extradir,category="CAAL",
              caption=paste0("Conditional Age-at_length data by year for ",
                             "females for fleet ",fleets[i]))    
      alfem <- alout$alcomp
      filename <- plotagelen(alcomp=alfem,gender="Female",fleet=fleets[i],
                           console=FALSE,plotdir=extradir,linear=linear)
      
      addplot(filen=filename,rundir=extradir,category="CAAL",
              caption=paste0("Conditional Age-at_length data by year for ",
                             "females for fleet ",fleets[i],", jittered to ",
                             "indicate density of data.")) 
      if (!is.null(compmal)) {
        pickflt <- which(compmal$fleet == i)
        alout <- plotagecomp(compsex=compmal[pickflt,],gender="Male",
                             fleet=fleets[i],rescale=0.0,console=FALSE,
                             plotdir=extradir,plotout=TRUE)
        filename <- alout$filen
        addplot(filen=filename,rundir=extradir,category="CAAL",
                caption=paste0("Conditional Age-at_length data by year for ",
                               "males for fleet ",fleets[i]))    
        almal <- alout$alcomp
        filename <- plotagelen(alcomp=almal,gender="Male",fleet=fleets[i],
                               console=FALSE,plotdir=extradir,linear=linear)
        addplot(filen=filename,rundir=extradir,category="CAAL",
                caption=paste0("Conditional Age-at_length data by year for ",
                               "males for fleet ",fleets[i],", jittered to ",
                               "indicate density of data."))  
      }
    }
  } else {
    if (verbose) cat("No conditional age-st-length data to plot. \n")
  }
  # Age-composition of first scenario
  # agecomp flt1-------------------------------- 
  if (nrow(plotreport$agedbase) > 0) {
    if (verbose) cat("Generating agecomp tab \n")
    fleetnames <- plotreport$FleetNames
    ageprop1 <- getprops(plotreport$agedbase,fleetnames=fleetnames,
                         comptype="Age")
    agg <- ageprop1$agg
    fleets <- sort(unique(agg[,"Fleet"]))
    nfleet <- length(fleets)
    for (fl in 1 : nfleet) { # fl = 1
        flname <- fleetnames[fleets[fl]]
        filename <- plotaggage(agg1=ageprop1$agg,whichfleet=fleets[fl],
                               fleetname=flname,comptype="Age",console=FALSE,
                               rundir=extradir,scenarios=analysis)
        addplot(filen=filename,rundir=extradir,category="agecomp",
                caption=paste0("Comparison of Fit to Age Comps aggregated ",
                               "by Year and ",flname,"."))    
        
        filename <- plotageprops(agecomp1=ageprop1,whichfleet=1,
                                 comptype="Age",console=FALSE,
                                 rundir=extradir,scenarios=analysis) 
        addplot(filen=filename,rundir=extradir,category="agecomp",
                caption=paste0("Comparison of Fit to Age Comps in each year by ",
                               flname))
      }
    } else { cat("No age composition data to plot.  \n")
  }
  # lengthcomp flt1-----------------
  if (nrow(plotreport$lendbase) > 0) {  
    if (verbose) cat("Generating tables tab \n")
    fleetnames <- plotreport$FleetNames  
    lenprop <- getprops(plotreport$lendbase,fleetnames=fleetnames,
                        comptype="Len")
    agg <- lenprop$agg
    fleets <- sort(unique(agg[,"Fleet"]))
    nfleet <- length(fleets)
    for (fl in 1 : nfleet) { # fl = 2
      whichfleet <- fleets[fl]
      flname <- fleetnames[whichfleet]
      filename <- plotaggage(agg1=lenprop$agg,
                             whichfleet=whichfleet,fleetname=flname,
                             comptype="Len",console=console,rundir=extradir,
                             scenarios=analysis)
      addplot(filen=filename,rundir=extradir,category="LenComp",
              caption=paste0("Comparison of Fit to Len Comps aggregated ",
                             "by Year and ",flname,"."))    
      
      filename <- plotageprops(agecomp1=lenprop,whichfleet=fl,comptype="Len",
                               console=console,rundir=extradir,scenarios=compare) 
      addplot(filen=filename,rundir=extradir,category="LenComp",
              caption=paste0("Comparison of Fit to Len Comps in each year by ",
                             flname))
    }
    # now original data
    lencomp <- dat$lencomp
    lbins <- dat$lbin_vector
    nlbin <- length(lbins)
    fleets <- sort(unique(lencomp[,"fleet"]))
    nfleet <- length(fleets)
    for (fl in 1 : nfleet) { # fl = 1
      flname <- fleetnames[fleets[fl]]
      gender <- c("f","m")
      flnames <- paste0(gender[1],lbins)
      mlnames <- paste0(gender[2],lbins)
      sexes <- unique(lencomp[,"sex"])
      yrs <- sort(unique(lencomp[,"year"]))
      if (sexes > 1) { # sexes identified
        pickL <- which(colnames(lencomp) %in% c(flnames,mlnames))
        if ((nlbin * 2) != length(pickL)) {
          stop(cat("Number of length bins in lencomp differs from lbin_vector \n"))
        }
        fem <- t(lencomp[,pickL[1:nlbin]])
        rownames(fem) <- lbins; colnames(fem) <- yrs
        mal <- t(lencomp[,pickL[(nlbin+1):(nlbin * 2)]])
        rownames(mal) <- lbins; colnames(mal) <- yrs  
        # plot females
        if (sum(colSums(fem,na.rm=TRUE)) > 0) {
          details <- plotcompdata(compdata=expandcolumns(fem),
                                  analysis=analysis,ylabel="Counts",
                                  console=console,outdir=extradir)
          addplot(filen=details$filename,rundir=extradir,category="LenComp",
                  caption=details$caption)
        } else {
          plotnull(msg="No female length data")
        }
        if (sum(colSums(mal,na.rm=TRUE)) > 0) {
          details <- plotcompdata(compdata=expandcolumns(mal),
                                  analysis=analysis,ylabel="Counts",
                                  console=console,outdir=extradir)
          addplot(filen=details$filename,rundir=extradir,category="LenComp",
                  caption=details$caption)          
        } else {
          plotnull(msg="No male length data")
        }  
      } else { # end of if sexes > 1 loop
        pickfl <- which(lencomp$fleet == fl)
        tmp <- lencomp[pickfl,]
        yrs <- sort(unique(tmp[,"year"]))
        mixed <- t(lencomp[pickfl,7:(nlbin+6)])
        rownames(mixed) <- lbins; colnames(mixed) <- yrs 
        if (sum(colSums(mixed,na.rm=TRUE)) > 0) {
          details <- plotcompdata(compdata=expandcolumns(mixed),
                       analysis=analysis, ylabel="Counts",
                      console=console,outdir=extradir)
          addplot(filen=details$filename,rundir=extradir,category="LenComp",
                  caption=details$caption)          
        } else {
          plotnull(msg="No female length data")
        }
      }  
    }  
  } else { 
    warning(cat("No length composition data to plot.  \n"))
  }  
  # comparison tab-------------
  if ((!is.null(compare)) | (!is.null(paths))) {
    if (verbose) cat("Generating comparison tab \n")
    compscenes <- getreplists(store=store,scenes=compare,paths=paths,
                              listname="plotreport")
    if (!compscenes$dimdiff) {
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
    
    cpue <- compscenes$cpue
    filename <- comparecpueplot(cpue,rundir=extradir,height=8,CI=TRUE,
                                console=FALSE)
    addplot(filen=filename,rundir=extradir,category="compare",
            caption="Comparison of CPUE. Note CI are from 1st scenario only.")
    # agecomp comparisons  
    if (nrow(compscenes$total[[1]]$agedbase) > 0) {
      if (length(compscenes$total) > 2) {
        warning("Age proportions of only first two scenarios will be used \n")
      }
      if (!compscenes$dimdiff) {
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
    } else {
      cat("Simple Age-composition data not used")
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



