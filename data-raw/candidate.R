


filename <- comparecpueplot(cpue,rundir=extradir,height=8,CI=TRUE,console=TRUE)





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


# getlencomp--------------------
console=TRUE
rundir=""
labeldiv=1
scene <- paste0("Garfish_",analysis)

lencomp <- dat$lencomp
lbins <- dat$lbin_vector
nlbin <- length(lbins)

gender <- c("f","m")
flnames <- paste0(gender[1],lbins)
mlnames <- paste0(gender[2],lbins)
sexes <- unique(lencomp[,"sex"])
yrs <- lencomp[,"year"]
nsamp <- lencomp[,"Nsamp"]
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
    plotcompdata(compdata=expandcolumns(fem),analysis=paste0(scene,"_female"),
                 ylabel="Counts",console=console,outdir=rundir)
  }
  if (sum(colSums(mal,na.rm=TRUE)) > 0) {
    plotcompdata(compdata=expandcolumns(mal),analysis=paste0(scene,"_male"),
                 ylabel="Counts",console=console,outdir=rundir)
  }  
  
}







lendbase <- plotreport$lendbase

lenprop <- getprops(lendbase)


# get both L50 from a domed selectivity curve
library(rforSS3)
library(codeutils)
library(hplot)
p <- c(10,15,10,40,-7,-1)

ages <- 0:30
sel <- domed(p,ages)

plotprep(width=9, height = 5)
parset()
plot(ages,sel,type="l",lwd=2,col=1,ylim=c(0,1.05),yaxs="i",panel.first=grid())

p <- c(10,15,10,40,-7,-1)
finds50 <- function(p,bins) { # p=p; bins=0:30
  nbin <- length(bins)
  sel <- domed(p,bins)
  pickE <- which(sel[1:p[1]] <= 0.5)
  cl <- bins[2] - bins[1]
  lowindex <- tail(pickE,1)
  hiindex <- lowindex + 1
  lowv <- sel[lowindex]
  hiv <- sel[hiindex]
  deltabin <- 0.5 - lowv
  newbin <- bins[lowindex] + deltabin
  bins2 <- c(bins[1:lowindex],newbin,bins[hiindex:nbin])
  sel2 <- domed(p,bins2)
  match(bins,bins2)
  
} # end of finds50

finds50(p,bin=7)



S <- domed(p,L=seq(0,30,1))

p <- c(10,15,10,40,-7,-1)
ages=0:30
S <- domed(p,ages)

plot1(0:30,S,lwd=2)


optimize(f=finds50,interval=c(5,8),selfun=domed,p=p,maximum=FALSE,tol=1e-09)


yrF <- optimize(matchC1,interval=c(0,maxF),M=M,cyr=obsC,
                Nyr=Nt[,yr-1],sel=sel,waa=aaw,
                maximum=FALSE,tol=1e-09)$minimum

lines(0:10,sel,lwd=2,col=4)


domed <- function(p, L) { # p=p; L = seq(0:10)
  nL <- length(L)
  J1 <- 1/(1 + exp(-20*((L - p[1])/(1 + abs(L - p[1])))))
  J2 <- 1/(1 + exp(-20*((L - p[2])/(1 + abs(L - p[2])))))   
  comp1 <- 1/(1 + exp(-p[5])) # inverse logit forced to be 0 - 1
  comp2 <- exp((-(L - p[1])^2)/p[3])
  comp3 <- exp((-(L[1] - p[1])^2)/p[3])
  asc <- comp1 + (1 - comp1) * ((comp2 - comp3)/(1 - comp3))
  comp4 <- 1/(1 + exp(-p[6])) # inverse logit 
  comp5 <- exp((-(L - p[2])^2)/p[4])
  comp6 <- exp((-(L[nL] - p[2])^2)/p[4])
  dsc <- 1 + (comp4 - 1) * ((comp5 - 1)/(comp6 - 1))
  sel <- (asc * (1 - J1)) + J1 * (1 - J2 + dsc * J2)
  sel <- sel/max(sel) # to ensure a maximum = 1.0
  return(sel)
} # end of domed

invlogit <- function(x) {
  return(exp(-x)/(1 + exp(-x)))
}

yrs <- size24$Yr
nyr <- length(yrs)
lens <- seq(11,99,2)
nl <- length(lens)
lw <- lens[2]- lens[1]
s1 <- size24[1,6:50]


plot1(lens,s1)

sel <- domed(p=c(45.85,46.79,5.308,1.699,-3.8,0.75363),L=lens)
lines(lens,sel,lwd=2,col=2)

invlogit(0.75363)

invlogit(-3.18064)




p <- c(45.8546,-3.18064,5.308,5.699,-999,-10)
bins <- seq(11,99,2)
sel <- domeSS3(p,bins)
plot1(bins,sel,lwd=2)

cbind(bins,round(sel,4))



comp <- alfem[,,20]

lenbins <- as.numeric(rownames(comp))
agebins <- as.numeric(colnames(comp))
nages <- length(agebins)
sumbyage <- colSums(comp,na.rm=TRUE)
xvect <- NULL
yvect <- NULL
for (i in 1:nages) {
  xvect <- c(xvect,rep(agebins[i],sumbyage[i]))
  yvect <- c(yvect,rep(lenbins,comp[,i]))
}

lens <- sort(unique(MQMF::vB(p=c(35,0.5,2),xvect)))
mod <- lm(yvect ~ xvect)
plotprep(width=9,height=5)
parset()
plot(base::jitter(xvect,factor=0.5),base::jitter(yvect,factor=0.5),type="p",
     cex=1.0,pch=1,ylim=c(0,lenbins[length(lenbins)]))
abline(mod,lwd=2,col=2)


# TEST do_extra-----------------------

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

calc <- pathtopath(wdir,"HAB/calc/")
pathSS3 <- "C:/Users/malcolmhaddon/Dropbox/A_CodeR/SA-SS3/HAB/calc/ss3.exe"
pathSS3
dirExists(calc)

store <- pathtopath(wdir,"HAB/whiting/")  # snapper  # whiting  # garfish
dirExists(store)

basecase <- c("GSV_03_mh",    # snapper
              "whiting_1_mh" # whiting
)

item = 2
analysis <- getCase(index=item,basecase) 
destination <- pathtopath(store,analysis)
print(destination)
extradir <- pathtopath(destination,"extra/")
dirExists(extradir)

load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  

outlists <- do_extra(plotreport=plotreport,extradir=extradir,analysis=analysis,
                     store=store,verbose = TRUE,
                     compare=NULL,linear=FALSE)




# quicksummary--------

columns <- colnames(times)

sort(unique(times$Sex))

fleets <- plotreport$FleetNames
nfleet <- length(fleets)

numrow <- nrow(plotreport$agedbase)
if (numrow > 0) agedbase <- plotreport$agedbase

numrow <- nrow(plotreport$lendbase)
lendbase <- plotreport$lendbase

# retrospective------------------

# run the retrospective analyses
retro(
  dir = new_mod_path, # wherever the model files are
  oldsubdir = "", # subfolder within dir
  newsubdir = "retrospectives", # new place to store retro runs within dir
  years = 0:-5, # years relative to ending year of model
  exe = "ss3"
)

# load the 6 models
retroModels <- SSgetoutput(dirvec = file.path(
  new_mod_path, "retrospectives",
  paste("retro", 0:-5, sep = "")
))
# summarize the model results
retroSummary <- SSsummarize(retroModels)
# create a vector of the ending year of the retrospectives
endyrvec <- retroSummary[["endyrs"]] + 0:-5
# make plots comparing the 6 models
# showing 2 out of the 19 plots done by SSplotComparisons
SSplotComparisons(retroSummary,
                  endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"),
                  subplot = 2, # only show one plot in vignette
                  print = TRUE, # send plots to PNG file
                  plot = FALSE, # don't plot to default graphics device
                  plotdir = new_mod_path
)


# calculate Mohn's rho, a diagnostic value
rho_output <- SSmohnsrho(
  summaryoutput = retroSummary,
  endyrvec = endyrvec,
  startyr = retroSummary[["endyrs"]] - 5,
  verbose = FALSE
)


# jittering-----------------

# define a new directory
jitter_dir <- file.path(mod_path, "jitter")
# copy over the stock synthesis model files to the new directory
copy_SS_inputs(dir.old = mod_path, dir.new = jitter_dir)
# run the jitters
jitter_loglike <- jitter(
  dir = jitter_dir,
  Njitter = 100,
  jitter_fraction = 0.1 # a typically used jitter fraction
)





