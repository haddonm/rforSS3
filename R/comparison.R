

#' @title comparecpueplot compare fits to cpue and devs for fleets and scenarios
#' 
#' @description comparecpueplot compare the fits to the cpue and the residuals 
#'     for all fleets across multiple scenarios. The first column of plots is 
#'     the observed vs expected cpue (the fit) and the second column is an
#'     explicit residual plot on a linear scale. The rows are the fleets. The 
#'     CI, if included, are only derived from the first scenario.
#'
#' @param cpue a list of the cpue objects out of a set of plotreports
#' @param rundir the directory into which to place the plot if saved
#' @param height the height of the plot, the default = 8, which suits 2 fleets.
#'     if you have fewer then adjust this appropriately. If more than 5 then
#'     external to the function it may be best to subset the cpue input data. 
#' @param CI default = TRUE confidence bounds will be plotted. 
#' @param console default = TRUE so plot goes to console.
#'
#' @returns invisibly the filename, to be used by addplot it also plots a graph
#' @export
#'
#' @examples
#' # syntax: 
#' # extradir <- pathtopath(destination,"extra/")
#' print("wait on data sets")
#' # cpue=cpue; rundir=extradir; height=8;CI=TRUE;console=TRUE
comparecpueplot <- function(cpue,rundir,height=8,CI=TRUE,console=TRUE) {
  nscen <- length(cpue)
  scenes <- names(cpue)
  for (i in 1:nscen) colnames(cpue[[i]]) <- tolower(colnames(cpue[[i]]))
  fleets <- unique(cpue[[1]][,"fleet_name"])
  nfleet <- length(fleets)
  yrrange <- range(cpue[[1]][,"yr"])
  if (console) { filen="" } else {
    fileout <- "Comparison_CPUE_for_allfleets_scenarios.png"
    filen <- pathtopath(rundir,fileout)
  }
  plotprep(width=9,height=height,newdev=FALSE,filename=filen,verbose=FALSE)
  parset(plots=c(nfleet,2),margin=c(0.25,0.4,0.05,0.1))
  for (fl in 1:nfleet) { # fl = 1
    dat <- cpue[[1]][cpue[[1]][,"fleet_name"]==fleets[fl],]
    maxy <- getmax(dat[,c("obs","exp")])    
    if (CI) {
      lower <- qlnorm(.025,meanlog = log(dat[,"obs"]),sdlog = dat[,"se"])
      upper <- qlnorm(.975,meanlog = log(dat[,"obs"]),sdlog = dat[,"se"])
      maxy <- getmax(upper)
    }
    rown <- nrow(dat)
    plot(dat[,"yr"],dat[,"exp"],type="l",lwd=2,xlab="",ylab=fleets[fl],
         ylim=c(0,maxy),xlim=c(yrrange[1],yrrange[2]),panel.first=grid())
    points(dat[,"yr"],dat[,"obs"],pch=16,cex=1.0,col=2)
    if (CI) {
      arrows(x0=dat[,"yr"],y0=lower,x1=dat[,"yr"],y1=upper,
             length=0.025,angle=90,col=1,lwd=1,code=3)
    }
    for (scen in 2:nscen) {
      dat2 <- cpue[[scen]][cpue[[scen]][,"fleet_name"]==fleets[fl],]
      lines(dat2[,"yr"],dat2[,"exp"],lwd=2,col=scen)
    }
    legend("topleft",scenes,col=1:nscen,lwd=3,bty="n",cex=1.2)
    plot(dat[,"yr"],dat[,"dev"],type="p",pch=16,cex=1.0,xlab="",ylab="Deviate",
         xlim=c(yrrange[1],yrrange[2]),panel.first=grid())
    abline(h=0.0,lwd=1.0,col=1)
    for (i in 1:rown) lines(c(dat[i,"yr"],dat[i,"yr"]),c(0.0,dat[i,"dev"]),lwd=1)
    for (scen in 2:nscen) {
      points(dat2[,"yr"],dat2[,"dev"],pch=16,col=scen)
      for (i in 1:rown) lines(c(dat2[i,"yr"],dat2[i,"yr"]),c(0.0,dat2[i,"dev"]),
                              lwd=1,col=scen)
    }
  }
  if (!console) dev.off()
  return(invisible(filen))
} # end of comparecpueplot

#' @title comparestats takes in a set of plotreports/replists and compares them
#' 
#' @description comparestats takes in a set of plotreports/replists collected
#'     by the 'getreplists' function and generates an outstats object that
#'     contains, for each scenario, summary outputs, the likelihoods, the
#'     estimable parameter values, and the standard model structure. 
#'
#' @param x a set of plotreports/replists output from getreplists
#' 
#' @seealso{
#'   \link{getreplists}
#' }
#'
#' @returns a list of the answer, likes, param, and model matrices.
#' @export
#'
#' @examples
#' print("wait on example data")
comparestats <- function(x) { # x = compscenes
  x1 <- x$scenesum
  scenes <- names(x1) 
  nscen <- length(scenes)
  answer <- sapply(x1,"[[","answer") 
  firstlike <- x1[[1]]$likes
  nlik <- nrow(firstlike)
  likenames <- rownames(firstlike)
  likes <- matrix(NA,nrow=nlik,ncol=nscen,
                  dimnames=list(likenames,scenes))
  likes[,1] <- firstlike[,"values"]
  for (i in 2:nscen) { # i = 2
    namelike2 <- rownames(x1[[i]]$likes)
    pickL <- match(namelike2,likenames)
    if (length(pickL) != nlik) warning(cat("Differeing number of likelihoods \n"))
    likes[pickL,i] <- x1[[i]]$likes[,"values"]      
  }
  firstpar <- x1[[1]]$param
  npar <- nrow(firstpar)
  namepar <- rownames(firstpar)
  param <- matrix(0,nrow=npar,ncol=nscen,
                  dimnames=list(namepar,scenes))
  param[,1] <- firstpar[,"Value"]
  for (i in 2:nscen) {
    nextpar <- x1[[i]]$param
    if (nrow(nextpar) == npar) {
      param[,i] <- nextpar[,"Value"]
    } else {
      warning(cat("Number of parameters in Scenario ",i,"differs from first \n"))
      param[,i] <- nextpar[match(rownames(firstpar),rownames(nextpar)),"Value"]
    }
  }
  x2 <- x$total[[1]]
  ages <- x2$Age_Comp_Fit_Summary
  nafleet <- nrow(ages); nacol <- ncol(ages)
  agefleet <- numeric(nafleet); names(agefleet) <- ages[1:nafleet,nacol]
  agefleet <- ages[1:nafleet,2]
  lens <- x2$Length_Comp_Fit_Summary
  nlfleet <- nrow(lens); nlcol <- ncol(lens)
  lenfleet <- numeric(nlfleet); names(lenfleet) <- ages[1:nlfleet,nlcol] 
  lenfleet <- lens[1:nlfleet,2]
  fleets <- unique(agefleet,lenfleet)
  fltnames <- paste(x2$FleetNames[fleets],"A","L",sep=",")
  tuneinfo <- get_tuning_table(x2,fleets=fleets,option="Francis",
                               write=FALSE,verbose=FALSE)
  addflt <- paste0(x2$FleetNames[fleets],c("-A","-L"),collapse=" ")
  rows <- c("nsex","startyr","endyr","B0","startSSB","endSSB","enddepl",
            "nfleets","NatM-Fem","NatM-Mal",addflt)
  models <- matrix(0,nrow=length(rows),ncol=nscen,dimnames=list(rows,scenes))
  for (i in 1:nscen) { # i=1
    x2 <- x$total[[i]]
    getpar <- x2$parameters
    picksex <- grep("NatM",rownames(getpar))
    models[1,i] <- length(picksex)
    models[2,i] <- x2$startyr
    models[3,i] <- x2$endyr
    deriv <- x2$derived_quants
    models[4,i] <- deriv["SSB_Virgin","Value"]
    models[5,i] <- deriv[paste0("SSB_",models[2,i]),"Value"]
    models[6,i] <- deriv[paste0("SSB_",models[3,i]),"Value"]
    models[7,i] <- deriv[paste0("Bratio_",models[3,i]),"Value"]
    fleets <- x2$FleetNames
    models[8,i] <- length(fleets)
    models[9,i] <- getpar[picksex[1],"Value"]
    models[10,i] <- NA
    if (length(picksex) == 2) models[10,i] <- getpar[picksex[2],"Value"]
    if (models[10,i] == 0) models[10,i] <- models[9,i]
  }
  return(list(answer=answer,likes=likes,param=param,models=models))
} # end of comparestats




#' @title copyproffiles copies a set of files over from the calc to the profdir
#' 
#' @description copyproffiles is used to copy a set of files (see default
#'     filenames in getfiles argument) from teh directory in which a scenarios
#'     calculations were made into the profdir when likelihood profile 
#'     calculations are to occur. Best to have a separate directory to save
#'     filling up your calc directory with large numbers of files I doubt you
#'     will use again (thought maybe)
#'
#' @param calc the full path to the directory in which SS3 calculations occur
#' @param profdir the full path to the directory in which the likelihood
#'     profile calculations are to occur
#' @param getfiles a character vector of the files names to be moved from the
#'     calc to the profdir directory. default = ss.ctl, ss.dat, ss3.par,
#'     starter.ss, and forecast.ss. The latter two are names required by SS3.
#'     The ss.ctl and ss.dat names are compared with those in the starter file
#'     and cahnged if necessary
#' @param findtext the text in the starter.ss file that identifies the line 
#'     where one places a 1 (instead of 0) so that prior likelihoods are
#'     included when doing likelihood profiles. default='_prior_like' but
#'     'use prior like' is also common and is found in starter.ss_new
#'
#' @returns nothing, but it copies the files listed in 'getfiles' from the calc
#'     directory to the profdir directory, and sets the 'use prior likelihood'
#'     argument in the starter.ss file to 1.  
#' @export
#'
#' @examples
#' print("read the code")
copyproffiles <- function(calc,profdir,
                          getfiles=c("ss.ctl","ss.dat","ss3.par","starter.ss",
                                     "forecast.ss"),
                          findtext="_prior_like") {
  startfile <- pathtopath(calc,"starter.ss")
  if (file.exists(startfile)) {
    starter <- readLines(con = startfile)
    pickP <- grep(findtext,starter,fixed=TRUE)
    cutstart <- substr(starter[pickP[1]],2,nchar(starter[pickP[1]]))
    starter[pickP] <- paste0("1",cutstart)    
    write(starter,file=startfile)
  } else {
    stop(cat("No starter.ss file found in ",calc," \n"))
  }
  ctlfile <- unlist(strsplit(starter[grep(".ctl",starter,fixed=TRUE)],"#"))
  ctlfile <- removeEmpty(ctlfile)[1]
  if (ctlfile != getfiles[1]) {
    warning(cat("Control file named ",ctlfile," not ",getfiles[1]," \n"))
    getfiles[1] <- ctlfile
  }
  datfile <- unlist(strsplit(starter[grep(".dat",starter,fixed=TRUE)],"#"))
  datfile <- removeEmpty(datfile)[1]
  if (datfile != getfiles[2]) {
    warning(cat("Data file named ",datfile," not ",getfiles[2]," \n"))
    getfiles[2] <- datfile
  }
  numfil <- length(getfiles)
  fileexist <- numeric(numfil)
  for (fil in 1:numfil) {
    filename <- pathtopath(calc,getfiles[fil])
    if (file.exists(filename)) {
      fileout <- pathtopath(profdir,getfiles[fil])
      file.copy(filename,fileout,overwrite=TRUE,copy.date=FALSE)
      fileexist[fil] <- 1
    }
  }
  if (sum(fileexist) != numfil) {
    pick <- which(fileexist == 0)
    label <- paste0("Missing file: ",pathtopath(calc,getfiles[pick])," \n")
    warning(label)
  }
} # end of copyproffiles



#' @title getprojdyn tabulates the projected catches from a set of scenarios
#' 
#' @description getprojdyn is used to tabulate the projected catches, spawning
#'     biomass depletion, spawning biomass, and estimated parameters from a
#'     set of different scenarios within SS3. It requires the use of the 
#'     function getreplists to aggregate the outputs from the different 
#'     scenarios.
#'
#' @param compscenes the output from getreplist
#' 
#' @seealso{
#'    \link{getreplists}
#' } 
#'
#' @returns a list of projcatch, projdepl, projspawn, and parmscen - the 
#'     estimated parameters from each scenario
#' @export
#'
#' @examples
#' # compscenes <- getreplists(store=store,
#' #                           scenes=c("BC-5-5-1","BC-5-35-1","BC-5-5-75"),
#' #                           listname="plotreport")
#' # outproj <- getprojdyn(compscenes)
getprojdyn <- function(compscenes) {
  catches <- compscenes$catches
  scenes <- names(catches)
  nscen <- length(scenes)
  scen1catch <- catches[[1]]
  pickproj <- which(scen1catch$Era == "FORE")
  pick1 <- pickproj[1]
  projpick <- c((pick1-5):(pick1-1), pickproj)
  numrow <- length(projpick)
  yrs <- scen1catch[projpick,"year"]
  columns <- c("year",scenes)
  total <- compscenes$total
  parm <- total[[1]]$parameters
  pickparm <- which(parm[,"Phase"] > 0)
  nparm <- length(pickparm)
  parmscen <- matrix(0,nrow=nparm,ncol=nscen,
                     dimnames=list(parm[pickparm,"Label"],scenes))
  projcatch <- as.data.frame(matrix(nrow=numrow,ncol=length(columns),
                                    dimnames=list(yrs,columns)))
  projcatch[,1] <- yrs
  projdepl <- projspawn <- projcatch
  for (i in 1:nscen) {
    projcatch[,i+1] <- catches[[i]]$TotalC[projpick]
    projdepl[,i+1] <- catches[[i]]$Depletion[projpick]
    projspawn[,i+1] <- catches[[i]]$SpawnB[projpick]
    parmscen[,i] <- total[[i]]$parameters[pickparm,"Value"]
  }
  return(list(projcatch=projcatch,projdepl=projdepl,projspawn=projspawn,
              parmscen=parmscen))
} # getprojdyn

#' @title getreplists loads a set of SS_output outputs into a set of lists
#' 
#' @description getreplists allows comparisons between alternative scenarios run
#'     in Stock synthesis 3 by loading the different replist objects generated
#'     by the r4ss function SS_output. Each of the different scenarios is saved
#'     into a list, as well as extracting the timeseries from each, and the 
#'     catches. Two approaches to identifying the paths leading to the various
#'     scenarios are implemented. The first uses the method implemented when
#'     using rforSS3 that has a 'store' which is the directory containing 
#'     separate sub-directories for each scenario being considered and it 
#'     generates the filenames to be generated from the various identified
#'     scenes or scenarios. The alternative is to provide a character vector
#'     containing the full paths and filenames of the SS_output objects to be
#'     compared. cpue has been added to the output.
#'
#' @param store is a directory that contains separate sub-directories for each
#'     scenario, each of which contains an object generated by SS_output. If 
#'     the paths option is used instead then store can be set to NA or NULL.
#' @param listname The standard name for the object output from SS_output, if
#'     the paths option is used instead then store can be set to NA or NULL.
#' @param scenes a character vector of the names given to each scenario, this
#'     is required by both approaches for file loading as these names will be
#'     used to label each scenario.
#' @param paths default = NULL, meaning the rforSS3 approach will be used, which
#'     requires only the scenes argument to contain information. Otherwise, 
#'     scenes can be NULL and paths should contain a character vector made up 
#'     of the complete path and filename for each Rdata file stored wherever 
#'     they are stored.
#' @param verbose default = TRUE warnings will be given
#'      
#' @seealso{
#'   \link[r4ss]{SS_output}, \link{projectedcatches}
#' }
#' 
#' @returns a list of lists so for each scenario a list of the timeseries, of
#'     the catches, of the SS_output object in their entirety and the output
#'     from summarizeSS3.
#' @export
#'
#' @examples
#' # require(r4ss)
#' # require(codeutils)
#' # compscenes <- getreplists(store="c:/afishsps/",listname="plotreport",
#' #             scenes=c("basecase_1","basecase_Rp7"),paths=NULL)
#' # alternatively
#' # compscenes <- getreplists(store=NULL,listname=NULL,
#' #                           scenes=c("basecase_1","basecase_Rp7"),
#' #             paths=c("c:/afishsps/basecase_1/plotreport_basecase_1.Rdata",
#' #                     "c:/afishsps/basecase_1/plotreport_basecase_Rp7.Rdata")
getreplists <- function(store,listname,scenes,paths=NULL,verbose=TRUE) {
#  store=store;scenes=compare;paths=NULL;listname="plotreport"  
  nscen <- length(scenes)
  total <- makelist(scenes)
  dyn <- makelist(scenes)
  cpue <- makelist(scenes)
  catches <- makelist(scenes)
  scenesum <- makelist(scenes)
  for (i in 1:nscen) {  # i = 1
    if (is.null(paths)) {
      filen <- paste0(listname,"_",scenes[i],".Rdata")      
      pickdir <- pathtopath(store,scenes[i])
      filename <- pathtopath(pickdir,filen)
    } else {
      filename <- paths[i]
    }
    load(filename)
    total[[i]] <- get(listname)
    scenesum[[i]] <- summarizeSS3(total[[i]])
    timeseries <- total[[i]]$timeseries
    dyn[[i]] <- timeseries
    catches[[i]] <- projectedcatches(total[[i]])
    cpue[[i]] <- total[[i]]$cpue
  }
  dimdiff <- FALSE  
  if (nscen > 1) {
    catchdims <- sapply(catches,dim)
    if (sum(catchdims[,1]) - sum(catchdims[,2]) != 0) dimdiff <- TRUE
    if (dimdiff) warning(cat("Dimensions for dynamics differ either in years ",
                             "or someother factor. Are you using CondAge@Len? ",
                         "Many of the comparison plots and table will fail \n"))
  }
  return(list(timeseries=dyn,catches=catches,total=total,scenesum=scenesum,
              scenes=scenes,dimdiff=dimdiff,cpue=cpue))
} # end of getreplists

#' @title projectedcatches allows alternative SS3 model catches to be compared
#' 
#' @description projectedcatches After fitting an optimal stock assessment
#'     model using SS3 it is usual to generate the predicted catches needed to
#'     move a fishery towards a given harvest strategy policy target reference 
#'     point. To do this it used to be standard practice to use recruitment 
#'     levels taken from the predicted stock recruitment relationship. However,
#'     it has become a good practice to consider the recent history of 
#'     predicted recruitment and modify the projected recruitment levels taken
#'     of the stock recruitment relationship such that they are similar to the 
#'     last 5 to 10 years. SS3 has an option in the forecast file called either
#'     # multiplier on base recruitment or # scalar/multiplier or value applied 
#'     to SR-curve. If such a modification is made it would be usual practice 
#'     to compare the predicted catches with and with the adjustment. This
#'     function extracts the total catches 
#'
#' @param plotreport the output from SS_outout, a replist
#' 
#' @seealso{
#'   \link[r4ss]{SS_output}, \link{getreplists}
#' }
#'
#' @returns A matrix of the catches including the projections, of spawnB, 
#'     recruits, depletion, total catches and ERA
#' @export
#'
#' @examples
#' # catches <- projectedcatches(plotreport=paste0(destination,"filename.Rdata"))
projectedcatches <- function(plotreport) {
  fleets <- plotreport$FleetNames
  timeseries <- plotreport$timeseries
  deadB <- grep("dead(B)",names(timeseries),fixed=TRUE)
  findfleets <-which(plotreport$fleet_type == 1)
  namecols <- c("year","SpawnB","recruits",
                paste0("C(B)_",fleets[findfleets]),"TotalC",
                "Depletion","ABC-Buffer","Era")
  columns <- c(2,7,8,deadB)
  ncatch <- length(deadB)
  catches <- timeseries[,columns]
  depl=catches[,2]/catches[1,2]
  abc <-  timeseries[,"ABC_buffer"]
  era <-  timeseries[,"Era"]
  catchall <- cbind(catches,rowSums(catches[,4:(ncatch+3)]),depl,abc,era)
  colnames(catchall) <- namecols
  return(catchall)
} # end of projectedcatches


#' @title plotreceffects compares SpawnB, Catches, Recruits and depletion
#' 
#' @description plotreceffects generates a combined plot of SpawnB, Catches, 
#'     Recruits, and spawning biomass depletion for all scenarios being 
#'     compared. This assumes all scenarios have similar properties such as 
#'     having the same endyr and same number of parameters.
#'
#' @param compscenes the output from getreplists, this is a list of three lists
#'     the timeseries from each scenario's SS_output replist, the catches from
#'     each scenario, and the total replist objects from each scenario for
#'     further analyses. 
#' @param fileout what filename to give to saved output, default='temp.png'. 
#'     This is only used if console = FALSE
#' @param rundir the full path to the directory into which the output plot 
#'     should be saved if console = FALSE. The default = ""
#' @param legcex default=1.25 but if there are many scenarios it should be less
#' @param startyr in which year shouyld the plots start? default = 2
#' @param console default = TRUE, meaning the plot will be sent to the console
#' 
#' @seealso{
#'   \link[r4ss]{SS_output}, \link{projectedcatches}, \link{getreplists}
#' }
#'
#' @returns an invisible list of SpawnB, Total Catches, recruits, spawning
#'     biomass depletion, standardized Instantenous F, and F from biggest Fleet 
#' @export
#'
#' @examples
#' # require(hplot)
#' # require(codeutils)
#' # compscenes getreplists(store="c:/afishsps/",listname="plotreport",
#' #             scenes=c("basecase_1","basecase_Rp7"),paths=NULL)
#' # projout <- projreceffects(compscenes,rundir="",console=TRUE)
#' # str(projout)
projreceffects <- function(compscenes,fileout="temp.png",rundir="",legcex=1.25,
                           startyr=2,console=TRUE) {
  #  compscenes=compscenes; rundir="extradir";legcex=1.0;console=TRUE; startyr=2; fileout=""
  if (startyr < 2) startyr <- 2  # avoiding input error
  catches <- compscenes$catches
  scenes <- names(catches)
  nscen <- length(catches)
  numrow <- nrow(catches[[1]])
  pickyrs <- startyr:numrow  # omit the first row
  allyrs <- catches[[1]]$year
  yrs <- allyrs[pickyrs]
  nyrs <- length(yrs)
  endyr <- compscenes$total[[1]]$endyr
  oldpar <- par(no.readonly=TRUE)
  filen <- ""
  if (!console) {
    filen <- pathtopath(rundir,fileout)
  }
  plotprep(width=10,height=9,newdev=TRUE,filename=filen,cex=1.0,verbose=FALSE)
  parset(plots=c(3,2),cex=1.0,margin=c(0.3,0.45,0.1,0.1),
         outmargin=c(1,0,0,0))
  spawnB <- sapply(catches,"[[","SpawnB")
  rownames(spawnB) <- allyrs
  maxy <- getmax(spawnB[pickyrs,])
  plot(yrs,spawnB[pickyrs,1],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
       ylab="Spawning Biomass",xlab="",panel.first=grid())
  #points(yrs[1],spawnB[1,1])
  if (nscen > 1) 
    for (i in 2:nscen) lines(yrs,spawnB[pickyrs,i],lwd=2,col=i)
  abline(v=(endyr + 0.5),lwd=1,col=1,lty=3)
  mtext("(a) ",side=1,line=-1.1,outer=FALSE,adj=1,cex=1.20) # end spawning biom
  totalC <- sapply(catches,"[[","TotalC")
  rownames(totalC) <- allyrs
  maxy <- getmax(totalC[pickyrs,])
  plot(yrs,totalC[pickyrs,1],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
       ylab="Catch (t)",xlab="",panel.first=grid())
  if (nscen > 1) 
    for (i in 2:nscen) lines(yrs,totalC[pickyrs,i],lwd=2,col=i)
  abline(v=(endyr + 0.5),lwd=1,col=1,lty=3)
  legend("bottomleft",legend=scenes,lwd=3,col=1:nscen,cex=legcex,bty="n") 
  mtext("(b) ",side=1,line=-1.1,outer=FALSE,adj=1,cex=1.20) # end catch
  recruits <- sapply(catches,"[[","recruits")
  rownames(recruits) <- allyrs
  projvals <- tail(recruits,1)
  maxy <- getmax(recruits[pickyrs,])
  plot(yrs,recruits[pickyrs,1],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
       ylab="Recruits",xlab="",panel.first=grid())
  lines(yrs,rep(projvals[1],nyrs),lwd=1,lty=2,col=1)
  if (nscen > 1) {
    for (i in 2:nscen) {
      lines(yrs,recruits[pickyrs,i],lwd=2,col=i)
      lines(yrs,rep(projvals[i],nyrs),lwd=1,lty=2,col=i)
    }
  }
  abline(v=(endyr + 0.5),lwd=1,col=1,lty=3)
  mtext("(c) ",side=1,line=-1.1,outer=FALSE,adj=1,cex=1.20) # end recruits
  depl <- sapply(catches,"[[","Depletion")
  rownames(depl) <- allyrs
  maxy <- getmax(depl[pickyrs,])
  plot(yrs,depl[pickyrs,1],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
       ylab="Spawning Biomass Depletion",xlab="",panel.first=grid())
  if (nscen > 1)
    for (i in 2:nscen) lines(yrs,depl[pickyrs,i],lwd=2,col=i)
  abline(v=(endyr + 0.5),lwd=1,col=1,lty=3)
  abline(h=0.2,lwd=1,col=2,lty=3)
  mtext("(d) ",side=1,line=-1.1,outer=FALSE,adj=1,cex=1.20) # end SSBdepl
  # plot instantaneous F rates
  total <- compscenes$total
  scenes <- names(total)
  nscen <- length(scenes)
  instF <- makelist((scenes))
  fleetF <- which(total[[1]]$fleet_type == 1)
  nfleet <- length(fleetF)
  for ( i in 1:nscen) instF[[i]] <- total[[i]]$exploitation[,c(1,4:(6+nfleet))]
  first <- instF[[1]]
  allyrs <- first[,"Yr"]
  fnyrs <- length(allyrs)
  pickfyrs <- (startyr-1):fnyrs 
  yrs <- allyrs[pickfyrs]
  Fstd <- sapply(instF,"[[","F_std")
  maxy <- getmax(Fstd[pickfyrs,])
  plot(yrs,first[pickfyrs,"F_std"],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
       ylab="Overall Instantaneous F",xlab="",panel.first=grid())
  if (nscen > 1)
    for (i in 2:nscen) lines(yrs,instF[[i]][pickfyrs,"F_std"],lwd=3,col=i)
  abline(v=(endyr + 0.5),lwd=1,col=1,lty=3)
  legend("bottomleft",legend=scenes,lwd=3,col=1:nscen,cex=legcex,bty="n") 
  mtext("(e) ",side=1,line=-1.1,outer=FALSE,adj=1,cex=1.20) # end recruits
  # Plot individual fleet with maximum F
  numcol <- ncol(first)
  flnames <- colnames(first)
  fleetF <- apply(first,2,max,na.rm=TRUE)[5:numcol]
  pickFl <- which.max(fleetF) + 4
  Flmax <- flnames[pickFl]
  maxFL <- sapply(instF,"[[",Flmax)
  rownames(maxFL) <- allyrs
  maxy <- getmax(maxFL[pickfyrs,])
  plot(yrs,maxFL[pickfyrs,1],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
       ylab=paste0("Fleet with Maximum F: ",Flmax),xlab="",panel.first=grid())
  if (nscen > 1)
    for (i in 2:nscen) lines(yrs,maxFL[pickfyrs,i],lwd=2,col=i)
  abline(v=(endyr + 0.5),lwd=1,col=1,lty=3)
  mtext("(f) ",side=1,line=-1.1,outer=FALSE,adj=1,cex=1.20) # end recruits
  mtext("Year",side=1,outer=TRUE,cex=1.2,line=-0.2)
  if (!console) dev.off()
  on.exit(par(oldpar))
  return(invisible(list(spawnB=spawnB,totalC=totalC,recruits=recruits,
                        depl=depl,instF=instF,maxFL=maxFL)))
} # end of projreceffects

#' @title plotageprops compares annual age composition data
#' 
#' @description plotageprops compares annual age composition data
#'     by plotting each separately with the predicted proportions from just the
#'     first two scenarios being compared, plotted on top of each other. The
#'     proportions are scaled by effective sample size, not the nominal sample
#'     size (see getageprops). This can be applied to two or one scenario.
#'
#' @param agecomp1 the output of getageprops applied to the first scenario's
#'     plotreport
#' @param agecomp2 the output of getageprops applied to the second scenario's
#'     plotreport. Only the expected values are used from this one. Default 
#'     value = NULL, which means only the age proportions of the first agecomp1
#'     will be plotted.
#' @param whichfleet the fleet number (actual names obtained from array dims)
#' @param height height of the plot, adjust depending on number of years/plots
#' @param comptype default = 'Age', the capital letter is important. But could
#'     be 'Len'
#' @param console should the plot be saved as a png or go to console, 
#'     default = TRUE
#' @param rundir the directory into which any plot should be saved, default=''
#' @param scenarios the names of the scenarios, for labels.
#' 
#' @seealso{
#'       \link{getageprops}, \link{makehtml}
#' }
#'
#' @returns invisibly the filename of the plot for use with makehtml
#' @export
#'
#' @examples
#' # compare==c("SGBC-5-4-100-43","SGBC-5-4-80-6")
plotageprops <- function(agecomp1,agecomp2=NULL,whichfleet=1,height=9,
                         comptype="Age",console=TRUE,rundir="",
                         scenarios=c(1,2)) {
  #   agecomp1=ageprop1;  whichfleet <- 1; agecomp2=NULL; console=TRUE;rundir=""
  #   scenarios=analysis; height=9; comptype
  namedims <- dimnames(agecomp1[[1]])
  fleetnames <- namedims[[4]]
  nfleet <- length(fleetnames)
  nsex <- length(namedims[[3]])
  plotfleet <- 1
  if (nfleet > 1) {
    plotfleet <- whichfleet
  }
  ages <- as.numeric(unlist(namedims[[2]]))
  nages <- length(ages)
  yrs <- as.numeric(unlist(namedims[[1]]))
  nyr <- length(yrs)
  propyr <- agecomp1$propyr[,,,plotfleet]
  exppropyr <- agecomp1$exppropyr[,,,plotfleet]
  if (!is.null(agecomp2)) {
    expprop2 <- agecomp2$exppropyr[,,,plotfleet]
  }
  if (console) { filen="" } else {
    scenes <- paste0(scenarios,collapse="_")
    fileout <- paste0(comptype,"_Comp_Fits_",scenes,"_",fleetnames[whichfleet],".png")
    filen <- pathtopath(rundir,fileout)
  }
  plotprep(width=10,height=height,newdev=FALSE,filename=filen,verbose=FALSE)
  parset(pickbound(nyr+1),byrow=FALSE,margin=c(0.2,0.2,0.1,0.05),
         outmargin=c(1.5,1.5,0,0))
  maxy <- getmax(c(propyr,exppropyr),mult=1.005)
  for (i in 1:nyr) { # i = 5
    yr <- yrs[i]
    if (nsex == 2) {
      femprop <- propyr[i,,1]
      malprop <- -propyr[i,,2]
      plot(x=ages,y=seq(-maxy,maxy,length=nages),type="l",lwd=0,col=0,
           xlab="",ylab="",panel.first=grid())
      lines(ages,femprop,lwd=1,col=1)
      points(ages,femprop,cex=0.75,pch=16)
      polygon(x=c(ages[1],ages,ages[nages],ages[1]),y=c(0,femprop,0,0),col="darkgrey")  
      lines(ages,exppropyr[i,,1],lwd=3,col=2)
      lines(ages,malprop,lwd=1,col=1)
      points(ages,malprop,cex=0.75,pch=16)
      polygon(x=c(ages[1],ages,ages[nages],ages[1]),y=c(0,malprop,0,0),col="lightgrey") 
      lines(ages,-exppropyr[i,,2],lwd=3,col=4)
      if (!is.null(agecomp2)) {
        lines(ages,expprop2[i,,1],lwd=3,col=3,lty=2)
        lines(ages,-expprop2[i,,2],lwd=3,col=5,lty=2)
      }
      mtext(yr,side=3,outer=FALSE,adj=1,cex=1.0,line=-1.1)
    } else {
      ndim <- length(dim(propyr))
      if (length(dim) == 3) {
        mixprop <- propyr[i,,1]  
        expprop <- exppropyr[i,,1]
      } else {
        mixprop <- propyr[i,]
        expprop <- exppropyr[i,]
      }
      if (!is.null(agecomp2)) {
        pickY <- match(rownames(expprop2),rownames(propyr))
        index2 <- match(rownames(propyr),rownames(expprop2))
        if (length(pickY) == nyr) index2 <- pickY
      }
      plot(x=ages,y=seq(0,maxy,length=nages),type="l",lwd=0,col=0,xlab="",
           ylab="",panel.first=grid())
      lines(ages,mixprop,lwd=1,col=1)
      points(ages,mixprop,cex=0.75,pch=16)
      polygon(x=c(ages[1],ages,ages[nages],ages[1]),y=c(0,mixprop,0,0),col="darkgrey")  
      lines(ages,expprop,lwd=3,col=2)
      if (!is.null(agecomp2)) {
        if (i %in% pickY) lines(ages,expprop2[index2[i],],lwd=3,col=3,lty=2)
      }
      mtext(yr,side=3,outer=FALSE,adj=1,cex=1.0,line=-1.1)
    }
  } # end of yrs loop
  plotnull(msg=" ")
  leg <- NULL
  for (i in 1:2) leg <- c(leg,paste0(scenarios[i],"_Female"),
                          paste0(scenarios[i],"_Male"))
  if (!is.null(agecomp2)) {
    legend("bottomright",legend=leg,col=c(2,4,3,5),lwd=3,lty=c(1,1,2,2),
           cex=1.2,bty="n")
  } else {
    if (namedims[[3]] == "mixed") {
      # do nothing
    } else {
      legend("bottomright",legend=leg[1:2],col=c(2,4),lwd=3,lty=c(1,1),
             cex=1.2,bty="n")
    }
  }
  mtext(comptype,side=1,outer=TRUE,cex=1.2,line=-0.2)
  mtext(paste0("Proportion for Fleet ",fleetnames[whichfleet]),side=2,
        outer=TRUE,cex=1.2,line=-0.2)
  if (!console) dev.off()
  return(invisible(filen))
} # end of plotageprops

#' @title plotaggage plots age composition proportions aggregated across years
#' 
#' @description plotaggage uses the outputs of getageprops to plot a single
#'     graph of the aggregated age proportions either singly or with the 
#'     predicted values of two scenarios plotted on top of each other
#'
#' @param agg1 the output of getageprops applied to plotreport from scenario 1
#' @param agg2 the output of getageprops applied to plotreport from scenario 1,
#'     default = NULL, which would mean only the first scenario would be 
#'     plotted.
#' @param whichfleet the fleet number
#' @param fleetname the name of the fleet, default=''
#' @param height height of the plot, adjust as desired. default = 7
#' @param comptype default = 'Age', the capital letter is important. But could
#'     be 'Len'
#' @param console should the plot be saved as a png or go to console, 
#'     default = TRUE
#' @param rundir the directory into which any plot should be saved, default=''
#' @param scenarios the names of the scenarios, for labels.
#'
#' @seealso{
#'       \link{getageprops}, \link{makehtml}
#' }
#'
#' @returns invisibly the filename of the plot for use with makehtml
#' @export
#'
#' @examples
#' # syntax: compare==c("SGBC-5-4-100-43","SGBC-5-4-80-6")
plotaggage <- function(agg1,agg2=NULL,whichfleet=1,fleetname="",height=7,
                       comptype="Age",console=TRUE,rundir="",scenarios=c(1,2)) {
  #  agg1=lenprop$agg;agg2=NULL;whichfleet=fleets[fl];fleetname=flname;
  #  height=4;console=TRUE;rundir=extradir;scenarios=analysis;comptype="Len"
  fleets <- sort(unique(agg1[,"Fleet"]))
  nfleet <- length(fleets)
  pickF <- which(agg1[,"Fleet"]==whichfleet)
  aggF <- agg1[pickF,]
  bins <- sort(unique(aggF[,comptype]))
  nbins <- length(bins)
  sex <- sort(unique(aggF[,"Sex"]))
  nsex <- length(sex)
  if (console) { filen="" } else {
    scenes <- paste0(scenarios,collapse="_")
    tmplab <- paste0("_",comptype,"Comp_Fits_")
    fileout <- paste0("Aggregated_",fleetname,tmplab,scenes,"_",
                      ".png")
    filen <- pathtopath(rundir,fileout)
  }
  plotprep(width=8,height=height,newdev=FALSE,filename=filen,verbose=FALSE)
  parset()
  if (nsex == 1) {
    prop1 <- aggF[,c("Obs","Exp")]
    expprop2 <- NULL
    agg2F <- NULL
    maxy2 <- NA
    if (!is.null(agg2)) {
      pickF2 <- which(agg2[,"Fleet"]==whichfleet)
      agg2F <- agg2[pickF2,]
      expprop2 <- agg2F[,"Exp"]
      maxy2 <- getmax(expprop2,mult=1.005)
    }
    maxy1 <- getmax(prop1,mult=1.005)
    maxy <- max(c(maxy1,maxy2),na.rm=TRUE)
    label <- paste0("Proportion for ",fleetname)
    plot(x=bins,y=seq(0,maxy,length=nbins),type="l",lwd=0,col=0,
         xlab=comptype,ylab=label,panel.first=grid())
    polygon(x=c(bins[1],bins,bins[nbins],bins[1]),y=c(0,prop1[,1],0,0),
            col="darkgrey") 
    lines(bins,prop1[,1],lwd=2,col=1)
    points(bins,prop1[,1],cex=1.5,pch=16)
    lines(bins,prop1[,2],lwd=4,col=2)
    if (!is.null(agg2)) lines(bins,expprop2,lwd=4,col=3)   
    legend("topright",legend=scenarios,col=c(2,3),lwd=4,lty=c(1,1),
           cex=1.5,bty="n")  
  } else {
    femprop1 <- matrix(0,nrow=nbins,ncol=2,dimnames=list(bins,c("Obs","Exp")))
    malprop1 <- femprop1
    fem1 <- aggF[aggF[,"Sex"]==1,]
    pickbin <- match(fem1[,comptype],bins)
    femprop1[pickbin,1] <- fem1[,"Obs"] 
    femprop1[pickbin,2] <- fem1[,"Exp"]
    mal1 <- aggF[aggF[,"Sex"]==2,]
    pickbin <- match(mal1[,comptype],bins)
    malprop1[pickbin,1] <- mal1[,"Obs"] 
    malprop1[pickbin,2] <- mal1[,"Exp"]
    expprop2 <- NULL
    agg2F <- NULL
    if (!is.null(agg2)) {
      pickF2 <- which(agg2[,"Fleet"]==whichfleet)
      agg2F <- agg2[pickF2,]
      expprop2 <- femprop1
      expprop2[,1] <- agg2F[agg2F[,"Sex"]==1,"Exp"]
      expprop2[,2] <- agg2F[agg2F[,"Sex"]==2,"Exp"]
    }
    maxy1 <- getmax(aggF[,c("Obs","Exp")],mult=1.005)
    maxy2 <- NULL
    if (!is.null(agg2F)) maxy2 <- getmax(expprop2,mult=1.005)
    maxy <- max(maxy1,maxy2)
    label <- paste0("Proportion for ",fleetname)
    plot(x=bins,y=seq(-maxy,maxy,length=nbins),type="l",lwd=0,col=0,
         xlab=comptype,ylab=label,panel.first=grid())
    polygon(x=c(bins[1],bins,bins[nbins],bins[1]),y=c(0,femprop1[,1],0,0),
            col="darkgrey") 
    lines(bins,femprop1[,1],lwd=2,col=1)
    points(bins,femprop1[,1],cex=1.5,pch=16)
    lines(bins,femprop1[,2],lwd=4,col=2)
    polygon(x=c(bins[1],bins,bins[nbins],bins[1]),y=c(0,-malprop1[,1],0,0),
            col="lightgrey") 
    lines(bins,-malprop1[,1],lwd=2,col=1)
    points(bins,-malprop1[,1],cex=1.5,pch=16)
    lines(bins,-malprop1[,2],lwd=4,col=4)
    if (!is.null(agg2F)) {
      lines(bins,expprop2[,1],lwd=4,col=3,lty=2)
      lines(bins,-expprop2[,2],lwd=4,col=5,lty=2)
    }
    leg <- NULL
    for (i in 1:2) leg <- c(leg,paste0(scenarios[i],"_Female"),
                            paste0(scenarios[i],"_Male"))
    if (!is.null(agg2F)) {
      legend("topright",legend=leg,col=c(2,4,3,5),lwd=3,lty=c(1,1,2,2),
             cex=1.5,bty="n")    
    } else {
      legend("topright",legend=leg[1:2],col=c(2,4),lwd=3,lty=c(1,1),
             cex=1.5,bty="n")       
    }
  } # end of 2 sex loop
  if (!console) dev.off()
  return(invisible(filen))
} # end of plotaggage



#' @title plotselex generates a plot of the selectivity used in SS3
#' 
#' @description plotselex provides an alternative plot of the selectivity 
#'     curves for a given gender and for specific years. This is required 
#'     because the 3-D plots generated by R4SS are often difficult to interpret.
#'     By inputting specific years plotselex can address selectivity in 
#'     different time-blocks. 
#'
#' @param plotreport the output from SS_output after running SS3
#' @param sex the gender as in 'Male' or 'Female', default = 'Female', use
#'     this even if sex = 0 is used, it will be ignored
#' @param upbound upper value of ylim, default = 0, meaning ylim=c(0,1.1)
#' @param legendcex the font size for the legend, default = 1.1
#' @param labelcex default = 1.5, font size for axis labels
#' @param console default = TRUE, should plot go to console or png file?
#' @param rundir directory into which to place plots, default=""
#'
#' @return invisibly the filename of the plot being made.
#' @export
#'
#' @examples
#' # plotreport=plotreport; sex="Female";yrs=c(1984,2004,2016); upbound=0; console=TRUE
plotselex <- function(plotreport,sex="Female",upbound=0,
                      legendcex=1.1,labelcex=1.5,console=TRUE,rundir="") {
  # plotreport=plotreport; sex="Female"; upbound=0; console=TRUE
  # legendcex=1.1;labelcex=1.5;rundir=extradir  
  sel <- plotreport$sizeselex
  pickL <- which(sel[,"Factor"] == "Lsel")
  sel <- sel[pickL,]
  sexes <- sort(unique(sel[,"Sex"]))
  nsex <- length(sexes)
  if (nsex == 1) {
    sexname <- "mixed"
    picksex <- sexes # not consistent with data file
  }  
  if (nsex == 2) {
    sexname <- sex
    picksex <- ifelse(sex == "Female",1,2)    
  }
  allnames <- plotreport$FleetNames
  allfleets <- sort(unique(sel$Fleet))
  fleettype <- plotreport$fleet_type
  fleetnames <- allnames[fleettype == 1]
  nfleet <- length(fleetnames)
  fleets <- allfleets[fleettype == 1]
  startyr <- plotreport$startyr
  endyr <- plotreport$endyr
  uniqueyrs <- sort(unique(sel$Yr))
  pickyrs <- which((uniqueyrs >= startyr) & (uniqueyrs <= endyr))
  yrs <- uniqueyrs[pickyrs]
  if (console) { filen="" } else {
    years <- paste0(yrs,"_",collapse="")
    txt <- paste0("selectivity_for_",years,"for_",sexname,".png")
    filen <- pathtopath(rundir,txt)
  }
  plotprep(width=10,height=7,newdev=FALSE,filename=filen,verbose=FALSE)
  parset(plots=pickbound(nfleet+1),cex=1.0,byrow=FALSE) 
  for (fleet in 1:nfleet){ #  fleet=1
    pickS <- which((sel$Fleet == fleet) & (sel$Sex == picksex)) # sex 1 = F, 2 = M
    # cat(fleet,length(pickS),"\n")
    hnts <- sel[pickS,]
    numcols <- ncol(hnts)
    lens <- as.numeric(colnames(hnts)[6:numcols])
    if (upbound > 0) lens <- lens[which(lens <= upbound)]
    nlen <- length(lens) 
    nsel <- ncol(hnts)
    pickY <- which(hnts[,"Yr"] %in% yrs)
    selvals <- hnts[pickY,6:nsel]
    nyr <- length(yrs)
    plot(lens,selvals[1,],type="l",lwd=2,ylim=c(0,1.1),yaxs="i",
         xlab="Total Length (mm)",ylab=paste0(fleetnames[fleet],"_Selectivity"),
         panel.first=grid())
    abline(v=getL50(selvals[1,]),lwd=1,col=1,lty=3)
    for(i in 2:nyr) {  #   i = 2
      lines(lens,selvals[i,],lwd=2,col=i)
      abline(v=getL50(selvals[i,]),lwd=1,col=1,lty=3)
    }
    legend("bottomright",legend=yrs,col=c(1:nyr),lwd=3,bty="n",cex=legendcex)
  }
  plotnull(msg=sex,cex=labelcex)
  if (!console)  dev.off()
  return(invisible(filen))
} # end of plotselex

