








library(makehtml)
library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)

ddir <- getDBdir()                          # get dropdir dir
wdir <- pathtopath(ddir,"/A_CodeR/SS3run/") # use your own working directory
source(pathtopath(wdir,"ss3_utilities.R"))  # where one stores ss3_utilities.R

store <- pathtopath(wdir,"kingfish/") 
destination <- pathtopath(store,"BC")
analysis <- "BC"


agecmop <- getagecomp(rundir="",destination=destination,analysis=analysis,
                      console=TRUE)

sampsize <- plotreport$age_comp_fit_table[,c("Fleet","Yr","Sexes","Nsamp_in","Nsamp_adj","effN")]
rownames(sampsize) <- 1:nrow(sampsize)
sampsize

yrs <- sampsize[,"Yr"]
nyr <- length(yrs)



store <- "c:/Users/malco/DropBox/A_CodeR/SA-SS3/garfish/"
analysis <- "SGBC-5-4-80-6"
destination <- pathtopath(store,analysis)
load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  

ageprop80 <- getagecomps(plotreport)

analysis <- "SGBC-5-4-100-43"
destination <- pathtopath(store,analysis)
load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  

ageprop43 <- getagecomps(plotreport)

plotageprops(agecomp1=ageprop80,agecomp2=ageprop43,whichfleet=1,console=TRUE,rundir="") 

# get adjusted proportions of catch at age by year 

# aggregate catch by age-----------------

agedbase <- plotreport$agedbase

columns <- c(1,6,9,13,16,17,18,20,21,22)

dbase <- agedbase

Bins <- sort(unique(agedbase[["Bin"]]))
nbins <- length(Bins)
df <- data.frame(
  Nsamp_adj = agedbase[["Nsamp_adj"]],
  effN = agedbase[["effN"]],
  obs = agedbase[["Obs"]] * agedbase[["Nsamp_adj"]],
  exp = agedbase[["Exp"]] * agedbase[["Nsamp_adj"]]
)
agg <- aggregate(
  x = df,
  by = list(
    bin = agedbase[["Bin"]], f = agedbase[["Fleet"]],
    sex = agedbase[["Sex"]]
  ),
  FUN = sum
)
agg[["obs"]] <- agg[["obs"]] / agg[["Nsamp_adj"]]
agg[["exp"]] <- agg[["exp"]] / agg[["Nsamp_adj"]]
colnames(agg) <- c("Age","Fleet","Sex","Nsamp_adj","effN","Obs","Exp")
agg


scenarios=c("SGBC-5-4-100-6","SGBC-5-4-100-43")
console=TRUE
height=7
agg1 <- agg
agg2 <- NULL
plotfleet=2
fleetname="HNT"



# end agg--------------------------------

cbind(sampsize,femtot+maltot)

pfem <- fem
for (i in 1:nyr) pfem[,i] <- fem[,i]/femtot[i]
          
adjmult <- sampsize[,"Nsamp_adj"]/sampsize[,"Nsamp_in"]                                  
adjfem <- fem
for (i in 1:nyr) adjfem[,i] <- fem[,i] * adjmult[i]

totadjF <- colSums(adjfem)
padjfem <- fem
for (i in 1:nyr) padjfem[,i] <- adjfem[,i]/totadjF[i]

cbind(pfem[,1],padjfem[,1])

femconsole=TRUE

if (length(names(agecomp)) == 2) {
  agefemale <- agecomp$females
  namefleet <- names(agefemale)
  nfleet <- length(namefleet)
  for (fl in 1:nfleet) {
    femages <- agefemale[[fl]]
    exagefem <- expandcolumns(femages)
    label <- paste0("Female ages - Fleet ",namefleet[fl])
    plotcompdata(exagefem,analysis=analysis,ylabel=label,console=TRUE,
                 outdir="",barcol="red",bordercol="black",horizline=0) 
  }
  agemale <- agecomp$males
  for (fl in 1:nfleet) {
    mages <- agemale[[fl]]
    exagemal <- expandcolumns(mages)
    label <- paste0("Male ages - Fleet ",namefleet[fl])
    if (console) devAskNewPage(ask=TRUE)
    plotcompdata(exagemal,analysis=analysis,ylabel=label,console=TRUE,
                 outdir="",barcol="red",bordercol="black",horizline=0) 
  }
}
if ((length(names(agecomp)) == 1) & (names(agecomp) == "amixed")) {
  amixed <- agecomp[[1]]
  nfleet <- length(amixed)
  namefleet <- names(amixed)
  for (fl in 1:nfleet) { # fl=2
    compages <- amixed[[fl]]
    exages <- expandcolumns(compages)
    label <- paste0("Male ages - Fleet ",namefleet[fl])
    plotcompdata(exages,analysis=analysis,ylabel=label,console=TRUE,
                 outdir="",barcol="red",bordercol="black",horizline=0) 
    if (console) devAskNewPage(ask=TRUE)
  }
}
  


addplots <- extraplots(destination,analysis) {
  plotdir <- paste0(destination,"plots")
  files <- dir(plotdir)
  infofile <- files[grep(".csv",files)]
  
  plotselex(plotreport,sex="Female",yrs=c(1984,2004,2016),upbound=365,
            console=TRUE)
  
} # end of extra


destination <- "c:/Users/malco/DropBox/A_CodeR/SA-SS3/snapper/GSVBC/"
load(pathtopath(destination,paste0("plotreport_",endpart(destination),".Rdata")))  



destination <- "c:/Users/malco/DropBox/A_CodeR/SA-SS3/garfish/SGBC-5-4-100-6/"
load(pathtopath(destination,paste0("plotreport_",endpart(destination),".Rdata")))  

library(qmdutils)

makeQuarto(rundir="C:/Users/malco/Dropbox/A_CodeR/SA-SS3/course/",
           filename="Assessment-template.qmd",
           title="Assessment of -aspecies",
           city="Adelaide")





#catches ---------------------




#' @title getagecomp puts the ageing data from the SS dat file into matrices
#' 
#' @description getagedata extracts the age composition data from 
#'
#' @param rundir the directory in which the fishery directories are found
#' @param destination the fishery directory within the rundir
#' @param analysis the name of the analysis directory within the destination, 
#'     default = NULL, in which case filen neeeds to be populated with the full
#'     name of the SS3 data file to be found in the analysis directory
#' @param filen default = NULL or, if 'analysis = NULL this needs to be the full
#'     filename of the SS3 data file
#'
#' @returns
#' @export
#'
#' @examples
getagecomp <- function(rundir,destination,analysis=NULL,filen=NULL) {
  #  destination=destination; analysis = analysis; filen=NULL; console=TRUE
  if (is.null(analysis)) {
    filename <- pathtopath(destination,filen)
  } else {
    filename <- pathtopath(destination,paste0(analysis,".dat"))
  }
  compdat <- SS_readdat_3.30(file=filename,verbose=TRUE)
  abins <- compdat$agebin_vector
  nbins <- length(abins)
  agecomp <- compdat$agecomp
  sexes <- unique(agecomp[,"sex"])
  if ((length(sexes) == 2) | (sexes == 3)) {
    nsex <- 2
    sexname <- c("females","males")
  } 
  if (sexes == 0) {
    nsex <- 1
    sexname <- "mixed"
  }
  pickflt <- which(agecomp[,"fleet"] > 0)
  if (length(pickflt) > 0) {
    agecomp <- agecomp[pickflt,]
    afleets <- unique(agecomp[,"fleet"])
    flnames <- compdat$fleetnames[afleets]
    anfleets <- length(afleets)
    if (agecomp[1,"sex"] == 3) {
      afemales <- makelist(flnames) 
      amales <- makelist(flnames)
      for (fl in 1:anfleets) { # fl=1
        pickfl <- which(agecomp[,"fleet"] == afleets[fl])
        agecompfl <- agecomp[pickfl,]
        anyrs <- nrow(agecompfl)
        ayrs <- agecompfl[,"year"]
        females <- matrix(0,nrow=nbins,ncol=anyrs,dimnames=list(abins,ayrs))
        males <- females
        for (yr in 1:anyrs) { # yr=1
          females[,yr] <- as.numeric(agecompfl[yr,10:(10+nbins-1)])
          males[,yr] <- as.numeric(agecompfl[yr,(10+nbins):(10+(2*nbins)-1)])
        }
        afemales[[fl]] <- females
        amales[[fl]] <- males
      }
      out <- list(females=afemales,males=amales)
    }
    if (agecomp[1,"sex"] == 0) {
      amixed <- makelist(flnames) 
      for (fl in 1:anfleets) { # fl=1
        pickfl <- which(agecomp[,"fleet"] == afleets[fl])
        agecompfl <- agecomp[pickfl,]
        anyrs <- nrow(agecompfl)
        ayrs <- agecompfl[,"year"]
        mixed <- matrix(0,nrow=nbins,ncol=anyrs,dimnames=list(abins,ayrs))
        for (yr in 1:anyrs) { # yr=1
          mixed[,yr] <- as.numeric(agecompfl[yr,10:(10+nbins-1)])
        }
        amixed[[fl]] <- mixed
      }
      out <- list(amixed=amixed)
    }
  } else {
    stop("No age comp data used")
  }
  return(out)
} # end of getagecomp


library(hplot)


outlen <- getlenprops(plotreport=plotreport,timeblocks=c(1983,1988,1989,2008,2009,2023))

plot1(agg[,"Bin"],agg[,"Obs"])



plotaggage <- function(agg1,agg2=NULL,plotfleet=1,fleetname="",height=7,
                       console=TRUE,rundir="",scenarios=c(1,2)) {
  #  agg1=ageprop1$agg;agg2=ageprop2$agg;plotfleet=whichfleet;fleetname=flname;
  #  height=4;console=TRUE;rundir=extradir;scenarios=compare
  fleets <- sort(unique(agg1[,"Fleet"]))
  nfleet <- length(fleets)
  pickF <- which(agg1[,"Fleet"]==plotfleet)
  aggF <- agg1[pickF,]
  ages <- sort(unique(aggF[,"Age"]))
  nages <- length(ages)
  sex <- sort(unique(aggF[,"Sex"]))
  nsex <- length(sex)
  if (console) { filen="" } else {
    scenes <- paste0(scenarios,collapse="_")
    fileout <- paste0("Aggregated_",fleetname,"AgeComp_Fits_",scenes,"_",
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
      pickF2 <- which(agg2[,"Fleet"]==plotfleet)
      agg2F <- agg2[pickF2,]
      expprop2 <- agg2F[,"Exp"]
      maxy2 <- getmax(expprop2,mult=1.005)
    }
    maxy1 <- getmax(prop1,mult=1.005)
    maxy <- max(c(maxy1,maxy2),na.rm=TRUE)
    label <- paste0("Proportion for ",fleetname)
    plot(x=ages,y=seq(0,maxy,length=nages),type="l",lwd=0,col=0,
         xlab="Ages (Yrs)",ylab=label,panel.first=grid())
    polygon(x=c(ages[1],ages,ages[nages],ages[1]),y=c(0,prop1[,1],0,0),
            col="darkgrey") 
    lines(ages,prop1[,1],lwd=2,col=1)
    points(ages,prop1[,1],cex=1.5,pch=16)
    lines(ages,prop1[,2],lwd=4,col=2)
    if (!is.null(agg2)) lines(ages,expprop2,lwd=4,col=3)   
    legend("topright",legend=scenarios,col=c(2,3),lwd=4,lty=c(1,1),
           cex=1.5,bty="n")  
  } else {
    femprop1 <- aggF[aggF[,"Sex"]==1,c("Obs","Exp")]
    malprop1 <- aggF[aggF[,"Sex"]==2,c("Obs","Exp")]
    expprop2 <- NULL
    agg2F <- NULL
    if (!is.null(agg2)) {
      pickF2 <- which(agg2[,"Fleet"]==plotfleet)
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
    plot(x=ages,y=seq(-maxy,maxy,length=nages),type="l",lwd=0,col=0,
         xlab="Ages (Yrs)",ylab=label,panel.first=grid())
    polygon(x=c(ages[1],ages,ages[nages],ages[1]),y=c(0,femprop1[,1],0,0),
            col="darkgrey") 
    lines(ages,femprop1[,1],lwd=2,col=1)
    points(ages,femprop1[,1],cex=1.5,pch=16)
    lines(ages,femprop1[,2],lwd=4,col=2)
    polygon(x=c(ages[1],ages,ages[nages],ages[1]),y=c(0,-malprop1[,1],0,0),
            col="lightgrey") 
    lines(ages,-malprop1[,1],lwd=2,col=1)
    points(ages,-malprop1[,1],cex=1.5,pch=16)
    lines(ages,-malprop1[,2],lwd=4,col=4)
    if (!is.null(agg2F)) {
      lines(ages,expprop2[,1],lwd=4,col=3,lty=2)
      lines(ages,-expprop2[,2],lwd=4,col=5,lty=2)
    }
    leg <- NULL
    for (i in 1:2) leg <- c(leg,paste0(scenarios[i],"_Female"),
                            paste0(scenarios[i],"_Male"))
    legend("topright",legend=leg,col=c(2,4,3,5),lwd=3,lty=c(1,1,2,2),
           cex=1.5,bty="n")    
  } # end of 2 sex loop
  if (!console) dev.off()
  return(invisible(filen))
} # end of plotaggage





do_compare <- function(compdir,store,compare,paths=NULL,verbose=TRUE) {
    #   compare=c("SGBC-5-4-100-6","SGBC-5-4-100-43"); paths=NULL
    setuphtml(compdir)
    compscenes <- getreplists(store=store,scenes=compare,paths=paths,
                              listname="plotreport")
    filename <- "Comparison_of_scenarios.png"
    projout <- projreceffects(compscenes=compscenes,fileout=filename,
                              rundir=compdir,legcex=1.0,startyr=2,
                              console=FALSE)
    addplot(filen=filename,rundir=compdir,category="compare",
            caption="Comparison of Scenarios.")
    
    outdepl <- tail(projout$depl,15)
    filename <- "Comparison_Projection_year_depletion.csv"
    addtable(outdepl,filen=filename,rundir=compdir,category="compare",
             caption="Comparison of Final Years' delpetion levels.")
    
    outcat <- tail(projout$totalC,15)
    filename <- "Comparison_Projected_catch_by_scenario.csv"
    addtable(outcat,filen=filename,rundir=compdir,category="compare",
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
                             plotfleet=whichfleet,fleetname=flname,
                             console=FALSE,rundir=compdir,
                             scenarios=compare)
      addplot(filen=filename,rundir=compdir,category="compare",
              caption=paste0("Comparison of Fit to Age Comps aggregated ",
                             "by Year and ",flname,"."))    
      
      filename <- plotageprops(agecomp1=ageprop1,agecomp2=ageprop2,whichfleet=fl,
                               console=FALSE,rundir=compdir,scenarios=compare) 
      addplot(filen=filename,rundir=compdir,category="compare",
              caption=paste0("Comparison of Fit to Age Comps in each year by ",
                             flname))
    }
    
    # further table of comparisons
    if (nrow(compscenes$total[[1]]$parameters) == 
        nrow(compscenes$total[[2]]$parameters)) {
      outstats <- comparestats(compscenes)
      
      outans <- round(outstats$answer,7)
      filename <- "Comparison_Model_output_scenario.csv"
      addtable(outans,filen=filename,rundir=compdir,category="compare",
               caption="Comparison of model output by scenario.")    
      
      outlik <- round(outstats$likes,6)
      filename <- "Comparison_Model_Likelihoods_scenario.csv"
      addtable(outlik,filen=filename,rundir=compdir,category="compare",
               caption="Comparison of model Likelihoods by scenario.")    
      
      outpar <- round(outstats$param,6)
      filename <- "Comparison_Model_paramters_scenario.csv"
      addtable(outpar,filen=filename,rundir=compdir,category="compare",
               caption="Comparison of model parameters by scenario.")       
      
      outmod <- round(outstats$models,3)
      filename <- "Comparison_Model_structure_scenario.csv"
      addtable(outmod,filen=filename,rundir=compdir,category="compare",
               caption="Comparison of model structure by scenario.") 
    }
  
  
} # end of do_compare


# getagelenkeys---------------------------


 
#' @title plotagelenkey plots all age-length keys derived form getagelenkeys
#' 
#' @description plotagelenkey is used to produce plots of all age-length keys
#'     used in SS3 when using conditional age-at-length. The age-length keys
#'     are naive in not having ageing error applied. That aspect remains under
#'     development. The getagelenkeys functions identifies combinations of
#'     the factors of gender, year, and fleet. The function is setup to plot
#'     a maximum of eight plots in a single graph. If the number of scenarios
#'     is greate than 8 then plotscenes can specify which to plot. Otherwise
#'     it retains its default = NULL
#'
#' @param outcomp the output of getagelenkeys applied to an SS3 data file,
#'     which contains ageing data set up to apply conditional age-at-length.
#' @param rundir the directory into which to place plots if console=FALSE,
#'     default = ''
#' @param plotscenes this can be used to plot sets of specific scenarios, made 
#'     up of gender + year + fleetname. default = NULL
#' @param pch the character to use in the plots, default = 1
#' @param pchcex the size of the character used in the plots
#' @param pchcol the colour of the character used in the plots
#' @param console should each plot go to the console, the default, or be 
#'     saved to rundir as a st of png files each identified by the scenes
#'     plotted
#' @param verbose should any feedback and warnings be sent to the console. 
#'     default = TRUE
#'     
#' \seealso{
#'   \link{getagelenkeys}
#' }
#'
#' @returns nothing but it does generate a set of plots.
#' @export
#'
#' @examples
#' # outcomp=outscene; console=TRUE;plotscenes=c(9:16)
#' # pchcex=1.25;pchcol=2;pch=1
plotagelenkey <- function(outcomp,rundir="",plotscenes=NULL,pch=1,pchcex=1.25,
                          pchcol=2,console=TRUE,verbose=TRUE) { 
  list2env(outcomp, envir = environment())
  scnames <- scenenames
  usescenes <- allscenes
  usesampN <- sampleN
  if (!is.null(plotscenes)) {
    nscene <- length(plotscenes)
    scnames <- scenenames[plotscenes]    
    usescenes <- makelist(scnames)
    for (i in 1:nscene) {
      usescenes[[i]] <- allscenes[[plotscenes[i]]]
    } 
    usesampN <- sampleN[plotscenes]
  } else {
    if ((nscene > 8) & (console)) {
      if (verbose) 
        warning(cat("Only the first 8 agelength keys will be plotted"))
      plotscenes <- 1:8
      nscene <- length(plotscenes)
      scnames <- scenenames[plotscenes]    
      usescenes <- makelist(scnames)
      for (i in 1:nscene) {
        usescenes[[i]] <- allscenes[[plotscenes[i]]]
      } 
      usesampN <- sampleN[plotscenes]
    } 
  }
  agelim <- range(agevector)
  sizelim <- range(lenvector)
  plotprep(width=9,height=9,filename="")
  parset(plots=c(4,2),margin=c(0.3,0.5,0.05,0.05),cex=1.0,
         outmargin=c(1,0.1,1,0.1),byrow=FALSE)
  for (sc in 1:nscene) { #  sc=2
    if (usesampN[sc] > 0) {
      agekey <- usescenes[[sc]]
      numrow <- nrow(agekey) 
      pickC <- which(colnames(agekey) == "nsamp")   
      label <- scnames[sc]
      plot(agevector,1:nages,type="p",pch=1,cex=1,xlim=c(agelim),
           ylim=c(sizelim),col=0,panel.first=grid(),xlab="Age",ylab=label)
      for (i in 1:nages) {
        for (j in 1:numrow) { # i=1;j=1
          lcomp <- mean(agekey[j,"lbin_lo"],agekey[j,"lbin_hi"],na.rm=TRUE)
          npt <- agekey[j,(pickC+i)]
          if (npt > 0) {
            points(x=rep(agevector[i],npt),y=rep(lcomp,npt),pch=pch,
                   cex=pchcex,col=pchcol)  
          }
        }
      }
      mtext(paste0("N = ",usesampN[sc]),side=1,line=-1.2,adj=1)
    }  else {
      plotnull(msg="No Data")
      mtext(scnames[sc],side=2,line=1,cex=1.0)
    }
  }
  mtext("Age",side=1,outer=TRUE,cex=1.2,line=-0.2)
  mainlab <- paste0("Scenes_",min(plotscenes),":",max(plotscenes))
  mtext(mainlab,side=3,outer=TRUE,cex=1.2,line=0)
} # end of plotagelenkey


if ((outscene$nscene > 8) & (console==TRUE)) {
  nscene <- outscene$nscene
  iter <- ceiling(outscene$nscene / 8)
  pickscene <- c(1:8)
  for (i in 1:iter) {
    plotagelenkey(outcomp=outscene,rundir="",plotscenes=pickscene,pch=1,
                  pchcex=1.25,pchcol=2,console=TRUE,verbose=TRUE)
    pickscene <- pickscene + 8
    pickpick <- which(pickscene <= nscene)
    pickscene <- pickscene[pickpick]
    if (i < iter) readline(prompt="Press [enter] to continue")
  }
} # end of plotagelengkeys








