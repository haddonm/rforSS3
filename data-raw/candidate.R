






#' @title getagecomp puts the ageing data from the SS dat file into matrices
#' 
#' @description getagedata extracts the age composition data from 
#'
#' @param rundir 
#' @param destination 
#' @param analysis 
#' @param filen 
#' @param console 
#'
#' @returns
#' @export
#'
#' @examples
getagecomp <- function(rundir,destination,analysis=NULL,filen=NULL,
                        console=TRUE) {
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
  return(out)
} # end of getagecomp

library(hplot)
library(codeutils)


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






















