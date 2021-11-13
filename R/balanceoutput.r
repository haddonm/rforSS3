# Sun Feb 12 12:07:05 2017 ------------------------------

#' @title fixLengthVar - adjust variances of length composition in the ctl file
#'
#' @description fixLengthVar  in the ctl file adjust variances concerning length
#'     composition. Do this by inputting a copy of the control file and the
#'     report file. The report obtained from using SS_output, usually called
#'     plotreport and saved as 'plotreport_', combined with the analysis names,
#'     and then '.RData'. This is stored in
#'     the the sub-directory for the particular analysis as the
#'     'plotreport_analysis.RData' file.
#'     The function finds the variance adjustment suggested in the report
#'     file and transfers that into the correct place in the control file. It
#'     searches in the #'     report file for the phrase 'Francis Weights -
#'     len', which occurs more than once, but it uses the first occurrance. This
#'     number has to be multiplied by the current variance in the control file
#'     and the result put back into the ctl file.
#'
#' @param incontrol a copy of the control file as stored in the 'analysis'
#'     sub-directory and read in as text using readLines
#' @param inrepout the 'analysis.txt' file stored in the 'analysis' sub-
#'     directory and read in as text using readLines
#' @param fleets identifies index of the fleets whose lengths are being
#'     corrected; defaults to c(1,3,4,5)
#' @return a list containing the text object containing a copy of the revised
#'     ctl file, and a matrix of the variance adjustments and the proportional
#'     changes made.
#' @export fixLengthVar
#'
#' @examples
#' print("Still to make up an example")
fixLengthVar <- function(incontrol,inrepout,fleets=c(1,3,4,5)) {
  # incontrol=control; inrepout=plotreport;fleets=NA
  outcontrol <- incontrol
  if (is.na(fleets[1])) {
     nfleet <- 0
   } else {
     nfleet <- length(fleets)
   }
   if (nfleet > 0) {
      WeightsLen <- rep(NA,nfleet)
      for (Ifleet in 1:nfleet) {
         xx <- r4ss::SSMethod.TA1.8(inrepout,"len",fleet=fleets[Ifleet],
                                    maxpanel=1000,plotit=F,printit=FALSE)
         if(!is.null(xx)) WeightsLen[Ifleet] <- xx[1]
      }
      pickcL <- grep("length comps",incontrol)
      lencomp <- removeEmpty(unlist(strsplit(incontrol[pickcL],"#")))
      lencomp2 <- paste0("  #",lencomp[2])
      varadj <- as.numeric(removeEmpty(unlist(strsplit(lencomp[1]," "))))
      answer <- matrix(0,nrow=nfleet,ncol=3,
                       dimnames=list(fleets,c("old","new","%diff","absDiff")))
      answer[,"old"] <- varadj[fleets]
      answer[,"new"] <- varadj[fleets] * WeightsLen
      answer[,"%diff"] <- 100*(1-(answer[,"old"]/answer[,"new"]))
      answer[,"absDiff"] <- abs(answer[,"old"] - answer[,"new"])
      varadj[fleets] <- answer[,"new"]
      outcontrol[pickcL] <- paste0(makeLabel(varadj,insep="  "),lencomp2)
  }
  return(list(outcontrol=outcontrol,answer=answer))
} # end of fixLengthVar

# Sort out the Age variance adjustment  incontrol=newcontrol; inrepout=repout
#' @title fixAgeCompVar - in the ctl file adjust variances concerning age composition
#'
#' @description fixAgeCompVar - in the ctl file adjust variances concerning age
#'     composition. Do this by inputting a text copy of the control file and the report
#'     object. The report  The report obtained from using SS_output, usually called
#'     plotreport and saved as 'plotreport_', combined with the analysis names,
#'     and then '.RData'. This is stored in the the sub-directory for the
#'     particular analysis as the 'plotreport_analysis.RData' file.
#'     The function finds the variance
#'     adjustment suggested in the report file and transfers that into the
#'     correct place in the control file. It uses the function
#'     'SSMethod.TA1.8' to find the Francis weights required. This number has
#'     to be multiplied by the current variance adjustment in the control file
#'     and the result put back into the ctl file.
#' @param incontrol - a copy of the control file as stored in the 'analysis'
#'     sub-directory and read in as text using readLines
#' @param inrepout - the R object obtained from using SS_output, usually called
#'     plotreport and saved as 'plotreport_', combined with the analysis names,
#'     and then '.RData'
#' @param fleets - identifies index of the fleet whose lengths are being corrected;
#'     defaults to c(1,3)
#' @param CAL conditional Age-at-Length data rather than classical age composition
#'     data; default = FALSE, meaning no conditional-age-at-length
#' @return a list containing the text object containing a copy of the revised ctl file,
#'    and the variance adjustment and the proportional change made.
#' @export
#'
#' @examples
#' print("See vignette for worked example.") # incontrol=control; inrepout=plotreport; fleets=agepos
fixAgeCompVar <- function(incontrol, inrepout, fleets=c(1,3), CAL=FALSE) {
   nfleet <- length(fleets)
   WeightsAge <- NULL
   for (Ifleet in 1:nfleet) {
      if (CAL) {
         xx <- SSMethod.Cond.TA1.8(inrepout,fleet=Ifleet,maxpanel=1000,plotit=F,
                                    printit=FALSE)
         if(!is.null(xx)) WeightsAge[Ifleet] <- c(WeightsAge,xx[1])
        } else {
         xx <- r4ss::SSMethod.TA1.8(inrepout,"age",fleet=fleets[Ifleet],
                                 maxpanel=1000,plotit=F,printit=FALSE)
         WeightsAge <- c(WeightsAge,xx[1])
      }
   }
   outcontrol <- incontrol
   index1 <- grep("#_Factor",incontrol)
   tmp <- grep("-9999",incontrol)
   index2 <- tmp[tmp > index1][1] - 1
   index1 <- index1 + 1
   indices <- index1:index2
   input <- incontrol[indices]
   nvar <- length(input)
   varadj <- matrix(0,nrow=nvar,ncol=3,dimnames=list(1:nvar,c("Fact","Flt","Val")))
   for (i in 1:nvar) varadj[i,] <- as.numeric(removeEmpty(unlist(strsplit(input[i]," "))))
   pickage <- which(varadj[,"Fact"] == 5)
   if (length(pickage) > 1) varadj <- varadj[pickage,]
   answer <- matrix(0,nrow=nfleet,ncol=5,
                    dimnames=list(fleets,c("old","new","%diff","absDiff","Mult")))
   answer[,"old"] <- varadj[fleets,"Val"]
   answer[,"new"] <- varadj[fleets,"Val"] * WeightsAge
   answer[,"%diff"] <- 100*(1-(answer[,"old"]/answer[,"new"]))
   answer[,"absDiff"] <- abs(answer[,"old"] - answer[,"new"])
   answer[,"Mult"] <- WeightsAge
   varadj[fleets,"Val"] <- answer[,"new"]
   outcontrol[indices[pickage]] <- paste0("    ",makeLabel(varadj,insep="   "),"    ")
   return(list(outcontrol=outcontrol,agevar=answer))
}  # end of fixAgeCompVar

   #  incontrol = newcontrol; inrepout <- repout; sigRLim=0.55
#' @title fixRecruit - replaces the recruitment bias-adjustment in the ctl file
#'
#' @description fixRecruit - replaces the recruitment bias-adjustment in the ctl
#'    file. Do this by inputting a text copy of the control file and the report
#'    file. The report file is the text generated after the model has been fitted
#'    and the replist and plots are generated. This is stored in the the sub-directory
#'    for the particular analysis as the 'analysis.txt' file. The function uses
#'    'r4ss::SS_fitbiasramp' to find the suggested
#'    varianceadjustment and transfers that into the
#'    correct place in the control file.
#' @param incontrol - a copy of the control file as stored in the 'analysis'
#'     sub-directory and read in as text using readLines
#' @param inrepout - the R object obtained from using SS_output, usually called
#'     plotreport and saved as 'plotreport_', combined with the analysis names,
#'     and then '.RData'
#' @param sigRLim - a vector or two numbers, the lower and upper limits for
#'     sigmaR. This allows for adjustments to be made to sigmaR but only down to
#'     the lower limit, which defaults to 0.25, or up to the upper limit, which
#'     defaults to 0.7.
#'
#' @return a list containing the text object containing a copy of the revised ctl file,
#'    and the variance adjustment and the proportional change made.
#' @export
#'
#' @examples
#' print("Still to make up an example for fixRecruit")
fixRecruit <- function(incontrol,inrepout,sigRLim=c(0.25,0.7)) {
   outcontrol <- incontrol
   rows <- c("yrnobias","fullbiasfirst","fullbiaslast",
             "recentnobias","maxbiasadj","sigmaR")
   columns <- c("original","update","%Diff","absDiff")
   recdat <- matrix(0,nrow=6,ncol=4,dimnames=list(rows,columns))
   pickRecC <- grep("last_early_yr_nobias",incontrol)
   orig <- NULL
   for (i in 1:5) {
      orig <- c(orig,getfirst(outcontrol[pickRecC + i - 1],rnd=FALSE))
   }
   recdat[,"original"] <- c(orig,inrepout$sigma_R_in)
   sink("tmp.txt")  # to prevent printing error messages
   biasest <- r4ss::SS_fitbiasramp(inrepout,plot=FALSE,print=FALSE)
   sink()
   badj <- biasest$df
   newsigR <- inrepout$sigma_R_info$alternative_sigma_R[1]
   recdat[,"update"] <- c(badj$value,newsigR)
   for (i in 1:5) {  # i <-  1
      outcontrol[pickRecC + i - 1] <- paste0(badj[i,1],"  ",badj[i,2])
   }
   if ((newsigR > sigRLim[1]) & (newsigR < sigRLim[2])) repsigR <- newsigR
   if (newsigR > sigRLim[2]) { repsigR <- sigRLim[2] }
   if (newsigR < sigRLim[1]) { repsigR <- sigRLim[1] }
   vecsigR <- grep("SR_sigmaR",incontrol)
   sigRbits <- removeEmpty(unlist(strsplit(incontrol[vecsigR],"#")))
   second <- paste0("  #",sigRbits[2])
   varadj <- as.numeric(removeEmpty(unlist(strsplit(sigRbits[1]," "))))
   varadj[3] <- repsigR
   varadj <- makeLabel(varadj,insep="  ")
   outcontrol[vecsigR] <- paste0(varadj,second,"  ",inrepout$sigma_R_in)
   recdat[,"update"] <- c(badj$value,repsigR)
   recdat[,"%Diff"] <- 100*(1-(recdat[,"update"]/recdat[,"original"]))
   recdat[,"absDiff"] <- abs(recdat[,"update"] - recdat[,"original"])
   return(list(outcontrol=outcontrol,recdat=recdat))
} # end of fixRecruit


# Now do CPUE  incontrol <- control; inrepout <- repout; numindex=numIndex; cepos=c(2,3)
#' @title fixIndex - adjusts the variances of the relative abundance indices
#'
#' @description fixIndex - adjusts the variances of the relative abundance indices
#'     It does this through using the readLines version of the control file and the
#'     txt file from the analysis sub-directory. The function finds the variance
#'     adjustments for numindex indices and adds them to the values already in the
#'     ctl file.
#' @param incontrol - a copy of the control file as stored in the 'analysis'
#'     sub-directory and read in as text using readLines
#' @param inrepout - the 'analysis.txt' file stored in the 'analysis' sub-directory and
#'     read in as text using readLines
#' @param cepos - the position of the indices of relative abundance in the sequence of
#'    different data sources declared; defaults to c(2,3), meaning in the sequence of
#'    TRAWL-CPUE-BIOMASS in teh data file, positions 2 and 3 refer to indices.
#' @return a list containing the text object containing a copy of the revised ctl file,
#'    and the variance adjustment and the proportional change made.
#' @export fixIndex
#' @examples
#' print("Still to make up an example for fixIndex")
fixIndex <- function(incontrol,inrepout,cepos=c(2,3)) {
  numindex <- length(cepos)
  outcontrol <- incontrol
  columns <- c("original","update","propDiff")
  cedat <- matrix(0,nrow=numindex,ncol=3,dimnames=list(1:numindex,columns))
  adjce <- numeric(numindex)
  cecontrol <- grep("add_to_survey",incontrol)
  cerecord <- removeEmpty(unlist(strsplit(incontrol[cecontrol],"#")))
  second <- paste0("#  ",cerecord[2])
  adjCPUE <- as.numeric(removeEmpty(unlist(strsplit(cerecord[1]," "))))
  cedat[,"original"] <- adjCPUE[cepos]
  cerep <- grep("index_variance_tuning_check",inrepout)
  for (i in 1:numindex) {  # i <- 1
    adjce[i] <- tail(suppressWarnings(as.numeric(removeEmpty(unlist(strsplit(inrepout[cerep + i + 1]," "))))),1)
    cedat[i,2:3] <- c(adjce[i],abs(cedat[i,1]-adjce[i]))
  }
  adjCPUE[cepos] <- adjce
  outcontrol[cecontrol] <- paste(makeLabel(adjCPUE,insep="  "),second,sep="  ")
  return(list(outcontrol=outcontrol,cedat=cedat))
} # end of fixIndex

# inLen <- ansLen; inAge <- ansAge; inrec <- recdat
#' @title checkConverged - checks variance adjustments
#'
#' @description checkConverged - checks variance adjustments. Takes the
#'    outputs from fixLengthVar, fixAgeVar, fixRecruit, and fixIndex
#'    found in balanceoutput.r and checks to see if the changes to
#'    the vaiance adjustments are small enough to count as converged
#' @param inLen - the vector of numbers from fixLengthVar
#' @param inAge - the vector of numbers from fixAgeVar
#' @param inrec - the recdat matrix from fixRecruit
#' @param incpue - the cedat matrix from fixIndex
#'
#' @return TRUE or FALSE; logical
#' @export checkConverged
#'
#' @examples
#' print("Need to develop an example")
checkConverged <- function(inLen,inAge,inrec,incpue) {
   if (!is.null(inLen)) {
      nlen <- 1
      if (!is.null(dim(inLen))) nlen <- length(inLen[,3])
      if (nlen > 1) {
         len <- (abs((sum(inLen[,3])/nlen) - 1) < 0.01)
      } else {
         len <- (abs(inLen[3] - 1) < 0.01)
      }
   } else {
      len = TRUE
   }
   nage <- 1
   if (!is.null(dim(inAge))) nage <- length(inAge[,3])
   if (nage > 1) {
      age <- (abs((sum(inAge[,3])/nage) - 1) < 0.01)
   } else {
      age <- (abs(inAge[,3] - 1) < 0.01)
   }
   rec <- (abs(1 - sum(inrec[1:5,1]/inrec[1:5,2])/5) < 0.01)
 #  rec <- !(sum(inrec[1:5,"propDiff"]) > 0.05) # five times 1 perc
   ce  <- !(sum(incpue[,"propDiff"]) > (0.01 * length(incpue[,"propDiff"])))
   cat("Converged:  \n")
   cat("Length Comp: ",len,"\n")
   cat("Age Comp:    ",age,"\n")
   cat("Recruitment: ",rec,"\n")
   cat("CPUE/Survey: ",ce,"\n")
   return(len & age & rec & ce)
}  # end of checkConverged

#' @title getfleets - identifies the names given to all fleets in ctl file
#'
#' @description getfleets identifies the names given to all fleets as
#'    listed in the ctl file. It uses the '%' symbol to identify the
#'    vector of fleet names so to use this requires that that symbol
#'    is not used anywhere else in the ctl file, even in comments. This
#'    will be fixed in later versions by excluding comment lines
#'
#' @param store the directory in which the seperate analysis directories
#'    are kept
#' @param analysis the specific analysis within 'store' being
#'    cosidered
#' @return fleets - a vector of the fleet names
#' @export getfleets
#'
#' @examples
#' print("See the vignette for a worked example")
getfleets <- function(store,analysis) {
   file <- paste0(store,analysis,"/",analysis,".dat")
   indat <- readLines(file)
   pickfleet <- grep("%",indat)
   fleets <- unlist(strsplit(indat[pickfleet],"%"))
   return(fleets)
} # end of getfleets

#' @title getadjust extracts the newvariance adjustment factors
#'
#' @description getadjust extracts the newvariance adjustment factors. We all
#'     need more time to write descritpions!
#'
#' @param incontrol the actual control file for the SS3 analysis
#' @param replist the object into which the output of SS_output was assigned
#' @param CAL conditional Age-at-Length data rather than classical age composition
#'     data; default = TRUE, meaning conditional-age-at-length is assumed as usual
#' @param ghosts should ghost fleets be excluded; default is TRUE
#'
#' @return a list containing the revised control file and the varadj matrix
#' @export
#'
#' @examples
#' \dontrun{
#' print("Still to be developed")
#' }
getadjust <- function(incontrol,replist,CAL=TRUE,ghosts=FALSE) {
  # incontrol=control; replist=plotreport; CAL=TRUE; ghosts=TRUE
   fleets <- replist$FleetNames
   if (ghosts) {
     pickF <- grep("Ghost",fleets)
     if (length(pickF) > 0) {
       fleets <- fleets[-pickF]
     }
   }
   Nfleet <- length(fleets)
   factortable <- as.data.frame(matrix(c(1, "add_to_survey_CV",
                                         2, "add_to_discard_stddev",
                                         3, "add_to_bodywt_CV",
                                         4, "mult_by_lencomp_N",
                                         5, "mult_by_agecomp_N",
                                         6, "mult_by_size-at-age_N", # cond-age-at-len
                                         7, "mult_by_generalized_sizecomp"),
                                       ncol = 2, byrow = TRUE))
   colnames(factortable) <- c("Factor","Meaning")
   index1 <- grep("#_Factor",incontrol)
   tmp <- grep("-9999",incontrol)
   index2 <- tmp[tmp > index1][1] - 1
   index1 <- index1 + 1
   indices <- index1:index2
   input <- incontrol[indices]
   nvar <- length(input)
   columns <- c("Factor","Fleet","NewValue","OldValue","Adjust","Meaning",
                "FleetName","%Change")
   varadj <- as.data.frame(matrix(0,nrow=nvar,ncol=length(columns),
                                  dimnames=list(1:nvar,columns)))
   for (i in 1:nvar)
      varadj[i,c(1,2,4)] <- as.numeric(removeEmpty(unlist(strsplit(input[i]," "))))
   varadj[,"Meaning"] <- factortable[varadj[,"Factor"],"Meaning"]
   varadj[,"FleetName"] <- fleets[varadj[,"Fleet"]]
   # Now get the adjustment factors from the SS_out object using r4ss functions
   WeightsLen <- rep(NA,Nfleet); names(WeightsLen) <- fleets
   WeightsAge <- rep(NA,Nfleet); names(WeightsAge) <- fleets
   WeightsCon <- rep(NA,Nfleet); names(WeightsAge) <- fleets
   for (Ifleet in 1:Nfleet) {  # Ifleet=1
      xx1 <- SSMethod.TA1.8(replist,"len",fleet=Ifleet,maxpanel=1000,plotit=FALSE,
                            printit=FALSE)
      if(!is.null(xx1)) WeightsLen[Ifleet] <- xx1[1]
      if (CAL) {
         xx2 <- SSMethod.Cond.TA1.8(replist,fleet=Ifleet,maxpanel=1000,plotit=FALSE,
                                   printit=FALSE)
         if(!is.null(xx2)) WeightsCon[Ifleet] <- xx2[1]
      } else {
         xx2 <- r4ss::SSMethod.TA1.8(replist,type="age",fleet=Ifleet,
                                    maxpanel=1000,plotit=FALSE,printit=FALSE)
         if(!is.null(xx2)) WeightsAge[Ifleet] <- xx2[1]
      }
   }
   finalwts <- cbind(1:Nfleet,WeightsLen,WeightsAge,WeightsCon)
   # if (length(which(finalwts[,2:3] > 0)) != dim(varadj)[1])
   #    warning("Number of variance adjustments differs from the input variance adjustments")
   # do lengths
   pickL <- which(finalwts[,"WeightsLen"] > 0)
   pickLv <- which(varadj[,"Factor"] == 4)
   if (length(pickL) > 0) varadj[pickLv,"Adjust"] <- finalwts[pickL,"WeightsLen"]
   varadj[pickLv,"NewValue"] <- varadj[pickLv,"OldValue"] * varadj[pickLv,"Adjust"]
   # do ages
   pickA <- which(finalwts[,"WeightsAge"] > 0)
   pickAv <- which(varadj[,"Factor"] == 5)
   if (length(pickA) > 0) varadj[pickAv,"Adjust"] <- finalwts[pickA,"WeightsAge"]
   varadj[pickAv,"NewValue"] <- varadj[pickAv,"OldValue"] * varadj[pickAv,"Adjust"]
   # do cond
   pickC <- which(finalwts[,"WeightsCon"] > 0)
   pickCv <- which(varadj[,"Factor"] == 6)
   if (length(pickC) > 0) varadj[pickCv,"Adjust"] <- finalwts[pickC,"WeightsCon"]
   varadj[pickCv,"NewValue"] <- varadj[pickCv,"OldValue"] * varadj[pickCv,"Adjust"]

   varadj[,"%Change"] <- 100 * abs(1-(varadj[,"NewValue"]/varadj[,"OldValue"]))
   outcontrol <- incontrol
   for (i in 1:nvar)
      outcontrol[indices[i]] <- paste0("    ",makeLabel(varadj[i,1:3],insep="   "),"    ")
   return(list(newcontrol=outcontrol,varadj=varadj))
}  # end of getadjust
