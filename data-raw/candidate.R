






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
































