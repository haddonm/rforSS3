






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


agecomp <- getagecomp(rundir=rundir,destination=destination,analysis=analysis,
                      console=TRUE)


console=TRUE

if (length(names(agecomp)) == 2) {
  agefemale <- agecomp$females
  namefleet <- names(agefemale)
  nfleet <- length(namefleet)
  for (fl in 1:nfleet) {
    femages <- agefemale[[fl]]
    exagefem <- expandcolumns(femages)
    label <- paste0("Female ages - Fleet ",namefleet[fl])
    plotcompdata2(exagefem,analysis=analysis,ylabel=label,console=TRUE,
                 outdir="",barcol="red",bordercol="black",horizline=0) 
  }
  agemale <- agecomp$males
  for (fl in 1:nfleet) {
    mages <- agemale[[fl]]
    exagemal <- expandcolumns(mages)
    label <- paste0("Male ages - Fleet ",namefleet[fl])
    if (console) devAskNewPage(ask=TRUE)
    plotcompdata2(exagemal,analysis=analysis,ylabel=label,console=TRUE,
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
    plotcompdata2(exages,analysis=analysis,ylabel=label,console=TRUE,
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







summarizeSS3A <- function(replist) {  # replist=plotreport
  likes <- replist$likelihoods_used
  param <- replist$parameters
  M <- param["NatM_uniform_Fem_GP_1","Value"] #NatM_p_1_Fem_GP_1
  steep <- param["SR_BH_steep","Value"]
  sigR <- param["SR_sigmaR","Value"]
  maxgrad <- replist$maximum_gradient_component
  pickp <- which(param[,"Phase"] > 0)
  columns <- c("Value","Init","Prior","Pr_type","Phase","Min","Max","Gradient")
  param2 <- param[pickp,columns]
  answer <- c(round(replist$endyr),replist$current_depletion,replist$SBzero,
              (1-replist$sprseries[nrow(replist$sprseries),"SPR"]),M,steep,sigR,
              likes["TOTAL",1],likes["Survey",1],likes["Length_comp",1],
              likes["Age_comp",1],likes["Recruitment",1],likes["Parm_priors",1],
              likes["Forecast_Recruitment",1],maxgrad)
  names(answer) <-  c("EndYear","Depletion","Bzero","1-SPR","M","h","sigmaR",
                      "TotalL","Index","LengthCompL","AgeCompL","Recruit",
                      "Param_Priors","Forecast_Recruitment","Maximum_Gradient")
  return(list(answer=answer,param=param2,likes=likes))
}


parreport <- summarizeSS3A(plotreport)$param


parreport[pickP,]




