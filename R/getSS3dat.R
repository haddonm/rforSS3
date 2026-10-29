


#' @title getSS3datasets uses SS_readdat_3.30 to extract data sets used by SS3
#' 
#' @description getSS3datasets only extracts the catches, the index or CPUE, 
#'     any length-composition data, any age-composition data, and some 
#'     structural details, It puts all of these inside a list, which is returned
#'
#' @param analysis the name of the SS3 model being explored
#' @param store the full path to the directory within which the analysis 
#'     sub-directory is held 
#' @param ss3datafile if not using store and analysis to generate path to data-
#'     file, then enter the full path here, toehrwise leave as NULL
#' @param verbose should progress updates be sent ot the console, default=TRUE
#'
#' @returns invisibly a list containing the catches, the cpue, the agecomp,
#'     the lencomp, and structural details in struct
#' @export
#'
#' @examples
#' # syntax datasets <- getSS3datasets(store,analysis,verbose=TRUE)
getSS3datasets <- function(store,analysis,ss3datafile=NULL,verbose=TRUE) {
# store=store;analysis=analysis;verbose=TRUE
  destination <- pathtopath(store,analysis)
  datfile <- pathtopath(destination,paste0(analysis,".dat"))
  dat <- SS_readdat_3.30(file=datfile,verbose=verbose)
  fleetnames <- dat$fleetnames
  nfleet <- length(fleetnames)
  catch <- dat$catch  
  cpue <- dat$CPUE
  agecomp <- dat$agecomp
  lencomp <- dat$lencomp
  lenbins <- NULL; nlens <- NULL
  if (dat$use_lencomp == 1) {
    lenbins <- dat$lbin_vector
    nlens <- length(lenbins)
  }
  agebins <- NULL; nages <- NULL
  if (!is.null(agecomp)) {
    agebins <- dat$agebin_vector
    nages <- length(agebins)
  }
  pickpos <- which(catch[,"year"] > 0)
  catch <- catch[pickpos,]
  catchyrs <- sort(unique(catch[,"year"]))
  catnyr <- length(catchyrs)
  catches <- as.data.frame(matrix(0,nrow=catnyr,ncol=nfleet,
                                  dimnames=list(catchyrs,fleetnames)))
  for (i in 1:nfleet) { # i = 1
    pickfleet <- which(catch[,"fleet"] == i)
    if (length(pickfleet) > 0) {
      flcatch <- catch[pickfleet,]
      pickrow <- match(flcatch[,"year"],catchyrs)
      catches[pickrow,i] <- flcatch[,"catch"]
    }
  }
  fleetnum <- sort(unique(cpue[,"index"]))
  cefleets <- length(fleetnum)
  ceyrs <- sort(unique(cpue[,"year"]))
  nceyrs <- length(ceyrs)
  columns <- c("year",fleetnames[fleetnum])
  obscpue <- as.data.frame(matrix(0,nrow=nceyrs,ncol=(cefleets+1),
                                  dimnames=list(ceyrs,columns)))
  obscpue[,"year"] <- ceyrs
  for (i in 1:cefleets) { # i = 1
    pickfleet <- which(cpue[,"index"] == i)
    if (length(pickfleet) > 0) {
      flce <- cpue[pickfleet,]
      pickrow <- match(flce[,"year"],ceyrs)
      obscpue[pickrow,(i+1)] <- flce[,"obs"]
    }
  }
  struct <- list(st_endyr=c(dat$styr,dat$endyr),
                 maxcatchyrs=catchyrs,
                 nsexes=dat$Nsexes,
                 nfleet=nfleet,
                 fleetnames=fleetnames,
                 nlens=nlens,
                 lenbins=lenbins,
                 nages=nages,
                 agebins=agebins)  
  return(invisible(list(catches=catches,cpue=obscpue,agecomp=agecomp,
                        lencomp=lencomp,struct=struct)))
} # end of getSS3datasets

plotscaledcatches <- function(catches,plotdir,addtitle="",console=TRUE,
                              addline=TRUE) {
  #  catches=catches;extradir=plotdir; console=TRUE
  fleetnames <- colnames(catches)
  nfleet <- length(fleetnames)
  yrs <- as.numeric(rownames(catches))
  nyr <- length(yrs)
  if (console) { filen="" } else {
    fileout <- paste0(addtitle,"_meanscaled_catches_for_",nfleet,"_fleets.png")
    filen <- pathtopath(plotdir,fileout)
  }
  plotprep(width=7,height=4,newdev=TRUE,filename=filen,verbose=FALSE)
  parset(plots=c(1,1),cex=1.0,margin=c(0.3,0.45,0.1,0.1))
  maxy <- getmax(catches)
  label <- paste0(addtitle," Mean Scaled Catches by Fleet")
  plot(yrs,catches[,1],type="l",lwd=3,col=1,xlab="",
       ylab=label,ylim=c(0,maxy),yaxs="i",panel.first=grid)
  if (nfleet > 1) for (fl in 2:nfleet) lines(yrs,catches[,fl],lwd=3,col=fl)
  legend("topright",fleetnames,col=1:nfleet,lwd=4,bty="n",cex=1.1)
  if (addline) abline(h=1,lwd=1,col=1,lty=3)
  if (!console) dev.off()
  return(invisible(filen))
} # end of plotscaledcatches

plotfltcpue <- function(cpue,rundir="",addtitle="",smooth=TRUE,console=TRUE) { 
  yrs <- cpue[,"year"]
  nyrs <- length(yrs)
  numcol <- ncol(cpue)
  fleets <- colnames(cpue)[2:numcol]
  nfleet <- length(fleets)
  if (console) { filen="" } else {
    fileout <- paste0(addtitle,"_CPUE_for_allfleets.png")
    filen <- pathtopath(rundir,fileout)
  }
  hgt <- nfleet*3.5
  plotprep(width=10,height=hgt,newdev=FALSE,filename=filen,verbose=FALSE)
  parset(plots=c(nfleet,1),margin=c(0.25,0.4,0.05,0.1))
  for (fl in 1:nfleet) { # fl = 1
    ce <- cpue[,(fl+1)]
    ce[ce == 0] <- NA
    maxy <- getmax(ce)  
    label <- paste0(addtitle,fleets[fl])
    plot(yrs,ce,type="l",lwd=2,xlab="",ylab=label,
         ylim=c(0,maxy),xlim=c(yrs[1],yrs[nyrs]),panel.first=grid())
    points(yrs,ce,pch=16,cex=1.0,col=2)
    if (smooth) {
      cedat <- cbind(year=yrs,cpue=ce)
      smoother <- getrmse(cedat,natlog=FALSE)
      lines(yrs,smoother$predictedCE,lwd=4,col=2)
      rmse <- round(smoother$rmse,4)
      legend("topright",paste0("rmse = ",rmse),lwd=3,col=2,cex=1.25,bty="n")
    }
  }
  if (!console) dev.off()
  return(invisible(filen))
} # end of plotfltcpue


#' @title SS3inputdata extracts plots and tabulates the input data to SS3
#' 
#' @description SS3inputdata extracts, plots, and tabulates the input data to 
#'     an SS3 scenario by using the SS_readdat_3.30 function from r4ss. It only 
#'     deals with the catches, the index or CPUE, any length-composition data,
#'     any age-composition data, snd some structural details, It provides saved 
#'     plots and tables providing a basic review of what information SS3 as 
#'     to work with. 
#'
#' @param analysis the name of the SS3 model being explored
#' @param store the full path to the directory within which the analysis 
#'     sub-directory is held 
#' @param plotdir the full path to the datareview directory into which to place 
#'     the new tables and plots. In my system = store + analysis + plotdir
#' @param addtitle a character vector providing additional details added to 
#'     filenames and labels. An example might be 'SNA_TWL_Fem' indicating 
#'     female snapper taken by trawl.
#' @param console should the plots go to the screen, default = FALSE which 
#'     means they will all be saved in plotdir
#' @param verbose should details of progress go to console, default = TRUE
#' @param subsetC If not all fishing gears take catches across the full extent
#'     of the fisheries years, this allows a sub-setting so that when scaled to
#'     the mean of each time-series, each time-series is comparable
#' @param lenrowhgt the heigh of each row when plotting length- and 
#'     age-composition.
#'
#' @returns nothing but it can generate an internal website with multiple plots
#'     as png files, and tables as csv files.
#' @export
#'
#' @examples
#' print("wait on data sets")
#' # minimum syntax SS3inputdata(store=store,analysis=analysis,plotdir=plotdir,
#' #                             lenrowhgt=3)
SS3inputdata <- function(store,analysis,plotdir,addtitle="",
                         console=FALSE,verbose=TRUE,subsetC=NULL,
                         lenrowhgt=3) {
  dirExists(plotdir)
  setuphtml(plotdir)
  datasets <- getSS3datasets(store=store,analysis=analysis,verbose=TRUE)
  struct <- datasets$struct
  nfleet <- struct$nfleet
  fleetnames <- struct$fleetnames
  # catch tab------------------
  if (verbose) cat("Generating observed catches tab \n")
  catches <- datasets$catches
  filename <- plotss3catches(catches,extradir=plotdir,analysis=addtitle,
                             console=console)
  if (!console) {
    addplot(filen=filename,rundir=plotdir,category="catch",
          caption=paste0("Reported catches by Year and fleet.",
                     " Recreational catches are interpolated between surveys",
                         " The red line in lower plot is a loess curve"))    
    filename <- paste0(analysis,"_Catch-by-Fleet.csv")
    allcatch <- cbind(catches,total=rowSums(catches,na.rm=TRUE))
    addtable(allcatch,filen=filename,rundir=plotdir,category="catch",
             caption=paste0("Reported Catch by Fleet. Recreational catches ",
                            "are interpolated netween surveys.")) 
  }
  
  titleadd <- addtitle
  scaledcatches <- catches
  maxCyrs <- struct$maxcatchyrs
  nyrs <- length(maxCyrs)
  pickF <- apply(catches,2,countgtzero)
  flts <- which(pickF == nyrs)
  for (flt in flts) {
    scaledcatches[,flt] <- catches[,flt]/mean(catches[,flt])
  }
  filename <- plotscaledcatches(scaledcatches[,flts],plotdir=plotdir,
                                addtitle=titleadd,console=console)
  if (!console) {
    addplot(filen=filename,rundir=plotdir,category="catch",
          caption=paste0("Reported catches by Year and fleet, where catches ",
                         "have been scaled by the mean of each time-series",
                         " with aim of illustrating whether each series has",
                         "  a similar trend. All have same length time-series"))  
    filename <- paste0(titleadd,"_Scaled_Catch-by-Fleet.csv")
    addtable(scaledcatches,filen=filename,rundir=plotdir,category="catch",
             caption=paste0("Reported Scaled Catches by Fleet. Recreational ",
                            "catches are interpolated between surveys.")) 
  }
  
  if (!is.null(subsetC)) {
    yrs <- as.numeric(rownames(catches))
    pickS <- match(subsetC,yrs)
    for (flt in 1:nfleet) {
      scaledcatches[pickS,flt] <- catches[pickS,flt]/mean(catches[pickS,flt])
    }
    addlabel <- paste0(addtitle,"_subsetted")
    filename <- plotscaledcatches(scaledcatches[pickS,],plotdir=plotdir,
                               addtitle=addlabel,console=console)
    if (!console) {
      addplot(filen=filename,rundir=plotdir,category="catch",
        caption=paste0("Reported catches by Year and fleet, where the catches ",
                       "have been scaled by the mean of each time-series",
                       " with the aim of illustrating whether each series has",
                       "  a similar trend. Subset TS so all series included."))  
      filename <- paste0(titleadd,"_Short_TS_Scaled_Catch-by-Fleet.csv")
      addtable(scaledcatches[pickS,],filen=filename,rundir=plotdir,
               category="catch",
               caption=paste0("Reported Scaled Catches by Fleet over selected ",
                              "years. Recreational ",
                              "catches are interpolated between surveys.")) 
    }
  } # end of !is.null(subsetC if statement
  # CPUE tab-------------------
    if (verbose) cat("Generating observed CPUE tab \n")
    cpue <- datasets$cpue
    filename <- plotfltcpue(cpue,rundir=plotdir,addtitle=addtitle,
                            console=console)
    if (!console) {
      addplot(filen=filename,rundir=plotdir,category="CPUE",
              caption="Observed CPUE plot for each fleet. redline = loess.")
      filename <- "CPUE-table-from-SS3-datafile.csv"
      addtable(cpue,filen=filename,rundir=plotdir,category="CPUE",
               caption="CPUE observations into SS3.") 
    }
  # length tab-----------------------
    lencomp <- datasets$lencomp
    if (!is.null(lencomp)) {  
      lbins <- struct$lenbins
      nlbin <- length(lbins)
      fleets <- sort(unique(lencomp[,"fleet"]))
      nfleet <- length(fleets)
      for (fl in 1 : nfleet) { # fl = 1
        flname <- fleetnames[fleets[fl]]
        flnum <- fleets[fl]
        gender <- c("f","m")
        flnames <- paste0(gender[1],lbins)
        mlnames <- paste0(gender[2],lbins)
        sexes <- unique(lencomp[,"sex"])
        yrs <- sort(unique(lencomp[,"year"]))
        if (length(sexes) > 1) { # sexes identified
          pickL <- which(colnames(lencomp) %in% c(flnames,mlnames))
          if ((nlbin * 2) != length(pickL)) {
            stop(cat("Number of length bins differs from lbin_vector \n"))
          }
          fem <- t(lencomp[,pickL[1:nlbin]])
          rownames(fem) <- lbins; colnames(fem) <- yrs
          mal <- t(lencomp[,pickL[(nlbin+1):(nlbin * 2)]])
          rownames(mal) <- lbins; colnames(mal) <- yrs  
          # plot females
          if (sum(colSums(fem,na.rm=TRUE)) > 0) {
            details <- plotcompdata(compdata=expandcolumns(fem),
                                  analysis=paste0(analysis,"_Female_",flname),
                                    ylabel="Counts",console=console,
                                    rowhgt=lenrowhgt,outdir=plotdir)
            if (!console) {
              addplot(filen=details$filename,rundir=plotdir,category="LenComp",
                      caption=details$caption)
              filename <- paste0(addtitle,"_Female_Length_comp_obs_data.csv")
              addtable(fem,filen=filename,rundir=plotdir,category="LenComp",
                       caption="Female length composition data input to SS3.") 
            }
          } else {
            plotnull(msg="No female length data")
          }
          if (sum(colSums(mal,na.rm=TRUE)) > 0) {
            details <- plotcompdata(compdata=expandcolumns(mal),
                                    analysis=paste0(analysis,"_Male_",flname),
                                    ylabel="Counts",console=console,
                                    rowhgt=lenrowhgt,outdir=plotdir)
            if (!console) {
              addplot(filen=details$filename,rundir=plotdir,category="LenComp",
                      caption=details$caption)          
              filename <- paste0(addtitle,"_Male_Length_comp_obs_data.csv")
              addtable(mal,filen=filename,rundir=plotdir,category="LenComp",
                       caption="Male length composition data input to SS3.") 
            }
          } else {
            plotnull(msg="No male length data")
          }  
        } else { # end of if sexes > 1 loop
          pickfl <- which((lencomp$fleet == flnum) & (lencomp$year > 0))
          tmp <- lencomp[pickfl,]
          yrs <- sort(unique(tmp[,"year"]))
          mixed <- t(lencomp[pickfl,7:(nlbin+6)])
          rownames(mixed) <- lbins; colnames(mixed) <- yrs 
          doplot <- FALSE
          if (length(yrs) == 1) {
            if (sum(mixed,na.rm=TRUE) > 0) doplot <- TRUE
          } else {
            if (sum(colSums(mixed,na.rm=TRUE)) > 0) doplot <- TRUE
          }
          if(doplot) { 
            details <- plotcompdata(compdata=expandcolumns(mixed),
                                    analysis=paste0(addtitle,"_Mixed_",flname), 
                                    ylabel="Counts",console=console,
                                    rowhgt=lenrowhgt,outdir=plotdir)
            if (!console) {
              addplot(filen=details$filename,rundir=plotdir,category="LenComp",
                    caption=details$caption) 
              sampsize <- colSums(expandcolumns(mixed),na.rm=TRUE)
              filename <- paste0(flname,"_sample_size_in_each_year.csv")
              addtable(sampsize,filen=filename,rundir=plotdir,category="LenComp",
                       caption=paste0(flname," sample size in each year.")) 
              filename <- paste0(flname,
                                 paste0(addtitle,"_Length_comp_obs_data.csv"))
              addtable(mixed,filen=filename,rundir=plotdir,category="LenComp",
                       caption="Length composition data input to SS3.")  
            } 
          } else {
            plotnull(msg="No female length data")
          } 
        }  # end of mixed loop
      }    # end of fleet loop
    } else { 
      warning(cat("No length composition data to plot.  \n"))
    } 
  
  # age tab-------------------------
    agecomp <- datasets$agecomp
    if (!is.null(agecomp)) {  
      agebins <- struct$agebins
      nages <- length(agebins)
      fleets <- sort(unique(agecomp[,"fleet"]))
      nfleet <- length(fleets)
      pickP <- which(agecomp[,"year"] > 0)
      agecomp <- agecomp[pickP,]
      for (fl in 1 : nfleet) { # fl = 1
        flname <- fleetnames[fleets[fl]]
        flnum <- fleets[fl]
        gender <- c("f","m")
        femnames <- paste0(gender[1],agebins)
        malnames <- paste0(gender[2],agebins)
        pickA <- which(colnames(agecomp) %in% c(femnames,malnames))
        if ((nages * 2) != length(pickA)) {
          stop(cat("Number of length bins differs from lbin_vector \n"))
        }
        pickF <- which(agecomp[,"fleet"] == flnum)
        femagecomp <- agecomp[pickF,c(1:3,pickA[1:nages])]
        yrs <- sort(unique(femagecomp[,"year"]))       
        fem <- t(femagecomp[,4:(nages+3)])
        rownames(fem) <- agebins; colnames(fem) <- yrs
        malagecomp <- agecomp[pickF,c(1:3,pickA[(nages+1):(nages * 2)])]
        yrs <- sort(unique(femagecomp[,"year"]))       
        mal <- t(malagecomp[,4:(nages+3)])
        rownames(mal) <- agebins; colnames(mal) <- yrs  
        # plot females
        if (sum(colSums(fem,na.rm=TRUE)) > 0) {
          females <- TRUE
          details <- plotcompdata(compdata=expandcolumns(fem),
                                  analysis=paste0(analysis,"_Female_Age_",flname),
                                  ylabel="Counts",console=console,
                                  rowhgt=lenrowhgt,outdir=plotdir)
          if (!console) {
            addplot(filen=details$filename,rundir=plotdir,category="agecomp",
                    caption=details$caption)
            filename <- paste0(addtitle,"_Female_Age_comp_obs_data.csv")
            addtable(fem,filen=filename,rundir=plotdir,category="agecomp",
                     caption="Female age composition data input to SS3.") 
          }
        } else {
          plotnull(msg="No female age data")
        }
        if (sum(colSums(mal,na.rm=TRUE)) > 0) {
          males <- TRUE
          details <- plotcompdata(compdata=expandcolumns(mal),
                                  analysis=paste0(analysis,"_Male_Age_",flname),
                                  ylabel="Counts",console=console,
                                  rowhgt=lenrowhgt,outdir=plotdir)
          if (!console) {
            addplot(filen=details$filename,rundir=plotdir,category="agecomp",
                    caption=details$caption)          
            filename <- paste0(addtitle,"_Male_Age_comp_obs_data.csv")
            addtable(mal,filen=filename,rundir=plotdir,category="agecomp",
                     caption="Male age composition data input to SS3.") 
          }
        } else {
          plotnull(msg="No male age data")
        }  
        if ((males) & (females)) {
          sampf <- round(colSums(expandcolumns(fem),na.rm=TRUE),1)
          sampm <- round(colSums(expandcolumns(mal),na.rm=TRUE),1)
          tot <- sampf+sampm 
          sampsize <- cbind(sampf,sampm,tot,sampf/tot,sampm/tot) 
          colnames(sampsize) <- c("female","male","total","P-female","P-male")
          numrow <- nrow(sampsize)
          sampsize <- rbind(sampsize,apply(sampsize,2,mean,na.rm=TRUE))
          rownames(sampsize)[numrow+1] <- "mean"
          yrs <- as.numeric(rownames(sampsize)[1:numrow])
          filen <- "" 
          if (!console) { 
            fileout <- paste0(addtitle,analysis,'_sex-ratio-or-ages.png') 
            filen <- pathtopath(plotdir,fileout) 
          }
          plotprep(width=8,height=4.5,newdev=TRUE,filename=filen,verbose=FALSE) 
          parset(plots=c(1,1),cex=1.0,margin=c(0.3,0.45,0.1,0.1)) 
          maxy <- getmax(sampsize[,c("P-female","P-male")])
          plot(yrs,sampsize[1:numrow,"P-female"],type='l',lwd=2,col=1,xlab="", 
               ylab="Sex-ratio of ageing data",ylim=c(0,maxy),yaxs='i',
               panel.first=grid) 
          lines(yrs,sampsize[1:numrow,"P-male"],lwd=2,col=2)
          abline(h=0.5,lwd=1,col=1,lty=3)
          legend('bottomleft',legend=c("females","males"),col=c(1:2),lwd=3,bty='n',
                 cex=1.2) 
          if (!console) {
            dev.off() 
            addplot(filen=filen,rundir=plotdir,category="agecomp",
                    caption="Sex-ratio of ageing data, expected ratio = 0.5") 
            filename <- paste0(addtitle,"_Age_Sample_sizes_by_Sex_by_year.csv")
            addtable(sampsize,filen=filename,rundir=plotdir,category="agecomp",
                     caption="Age sample sizes by sex by year.") 
          }
        }
      }
    } 
  # struct tab-----------------------
    make_html(replist=NULL,
              rundir=plotdir,
              datadir=NULL,
              width=500,
              openfile=TRUE,
              runnotes=paste0(paste0(addtitle,collapse="__")," Observed Data"),
              verbose=verbose,
              packagename="rforSS3",
              htmlname=paste0(analysis,"_DataReview"))
  
} # end of SS3inputdata


