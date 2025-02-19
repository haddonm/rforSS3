

#' @title changeCtl alters a parameter within the SS3 ctl file
#'
#' @description changeCtl alters a parameter within the SS3 ctl file
#'     once it has been copied into the calc directory. This means the
#'     original is never messed with. It finds the param string within
#'     the ctl file andmodifies that line of text by replacing the
#'     variable value at varpos (typically 3) with the newvalue. Once
#'     completed the function outputs a new ctl file as text, which then
#'     needs to be written back into the calc directory using the
#'     write command
#'
#' @param param  the text used to identify the line within the control file
#'     containing the parameter to change
#' @param newvalue the value of the parameter to be set
#' @param ctlfile the name of the control file to change, defaults = "ss.ctl"
#' @param directory the directory in which the control exists that is to be
#'     changed; defaults to NA but should be calc, defined as the subdirectory
#'     in which all calculations occur within rforSS3
#' @param pos the position in the vector of numbers in the line; default 3
#' @param pos2  position of the phase, which for a likelihood profile should
#'     be turned negative; defaults to 7
#'
#' @return new control file
#' @export
#'
#' @examples
#' \dontrun{
#' # typical syntax might be
#' varpos=3; pos2=7; profvalue[1]=0.15
#' control <- changeCtl("NatM_p_1_Fem_GP_1",profvalue[1],ctlfile="ss3.ctl",calc,
#'                       pos=varpos,pos2=phasepos)
#' write(control,file=paste0(calc,"ss3.ctl"))
#' print("See worked example in the rforSS3 vignette")
#' }
changeCtl <- function(param,newvalue,ctlfile="ss.ctl",directory=NA,pos=3,pos2=7) {
  ctrlfile <- pathtopath(directory,ctlfile)
  control <- readLines(con = ctrlfile)
  pickP <- grep(param,control,fixed=TRUE)
  if (length(pickP) == 0) stop(cat(param," not in ",ctlfile,"\n"))
  if (length(pickP) > 1) {
    warning("More than one ",param," in ",ctlfile,"; using first value")
    pickP <- pickP[1]
  }
  cutline <- unlist(strsplit(control[pickP],"#"))
  endbit <- cutline[2]
  paramLine <- as.numeric(removeEmpty(unlist(strsplit(cutline[1]," "))))
  paramLine[pos] <- newvalue
  if (paramLine[pos2] > 0) paramLine[pos2] <- paramLine[pos2] * (-1)
  num <- length(paramLine)
  invect <- as.character(paramLine)
  ans <- invect[1]
  if (num > 1) for (i in 2:num) ans <- paste(ans,invect[i],sep="  ")
  control[pickP] <- paste(ans,"#",endbit,sep="  ")
  #  write(control,file=ctrlfile)
  #  cat("New ",ctrlfile,"  written \n")
  return(control)
}  # end of changeCtl


#' @title changePar is used to alter a parameter within ss.par
#'
#' @description changePar is used, when conducting a likelihood profile, to
#'     change the value of a given parameter within ss.par so that the model
#'     can be re-fitted while keeping this parameter constant. This function
#'     may now be redundant as I believe r4ss now has a profile function?
#'
#' @param newvalue the new value for the given parameter
#' @param directory In which directory will the parameters be found.
#'
#' @return it re-writes the ss.par file and prints a confirmation to the console
#' @export
changePar <- function(newvalue,directory) {  # newvalue=9.0; directory=calc
  parfile <- pathtopath(directory,"ss.par")
  pars <- readLines(con = parfile)
  pickP <- grep("SR_parm[1]",pars,fixed=TRUE)
  if (length(pickP) == 0) stop("SR_parm[1] not in ss.par")
  if (length(pickP) > 1) {
    warning("More than one SR_parm[1] in ss.par; using first value")
    pickP <- pickP[1]
  }
  pars[pickP+1] <- newvalue
  write(pars,file=parfile)
  cat("New ",parfile,"  written \n")
}  # end of changeParam

#' @title getplotreports collets the plotreports and summaries into lists
#' 
#' @description getplotreports gathers the plotreports and the summarySS3
#'      answers into a list containing a list of plotreports and a matrix of
#'      summarySS3-answers. These can then be used when comparing the outputs
#'      fromdifferent scenarios.
#'
#' @param store the directory containing the subdirectories of each of the 
#'     scenarios  
#' @param subdir a vector of subdirectories to be found in store, obtained
#'     using dir(store)
#' @param picksubdir a vector of the indicies of the subdirectories to include
#'     into the comparisons.
#'
#' @return an invisible list containing a list of plotreports for each scenario
#'     selected, as well as a matrix of summarySS3$answer vectors for each 
#'     scenario
#' @export
#'
#' @examples
#' # syntxt used
#' # replists <- getplotreports(store=store, subdir=subdir, picksubdir=c(3,6,7))
getplotreports <- function(store,subdir,picksubdir) {
  scenes <- subdir[picksubdir]
  nscen <- length(scenes)
  replists <- makelist(scenes)
  summarySS3 <- NULL
  for (i in 1:nscen) {
    origin <- pathtopath(store,scenes[i])
    plotreport <- NULL
    filename <- pathtopath(origin,paste0("plotreport_",scenes[i],".Rdata"))
    load(filename)
    replists[[i]] <- plotreport
    summarySS3 <- cbind(summarySS3,summarizeSS3(plotreport)$answer)
  }
  colnames(summarySS3) <- scenes
  return(invisible(list(replists=replists,summarySS3=summarySS3)))
} # end of getplotreports

#' @title getL50 estimates the L50 from a selectivity vector
#' 
#' @description getL50 searches for the value cloest to the L50 when given a
#'     vector of selectivity at size or age
#'
#' @param x a vector of selectivity values whose names are the lengths or ages
#'     with which they are associated. The names are necessary for this function
#'     to work.
#'
#' @return a single value defining the L50
#' @export
#'
#' @examples
#' print("Wait on an example")
getL50 <- function(x) {
  leng <- as.numeric(names(x))
  near <- which.closest(0.5,x)
  near1 <- ifelse((x[near] < 0.5),near+1,near-1)
  index <- sort(c(near,near1))
  downup <- leng[index]
  spanlen <- downup[2] - downup[1]
  span <- x[index[2]] - x[index[1]]
  diff <- x[index[2]] - 0.5
  L50 <- leng[index[2]] - (diff/span) * spanlen
  return(L50)
} # end of getL50



#' @title profilestarter gets the starter file ready for a likelihood-profile 
#' 
#' @description profilestarter reads in the starter.ss file from the calc 
#'     directory - ensure that is the starter.ss for the scenario whose 
#'     likelihood profile you want. It then changes the value of the two fields
#'     init-values-src and prior-like to 1 rather than zero. If they are already
#'     set to 1 this will remain the case.
#' 
#'
#' @param calc the directory defined as the one in which the SS3 calculaitons
#'     will occur
#' @param findtext a vector containing identifying text for the required two 
#'     lines in the starter file. If they are not c("init_values_src",
#'     "prior_like"), then, obviously, you should alter 'findtext' to reflect
#'     whatever text you have put there.
#' @param verbose should progress be sent to the console? default = FALSE
#'
#' @return nothing but it does alter the contents of the starter.ss file
#' @export
#'
#' @examples
#' print("Wait on an example being prepared")
profilestarter <- function(calc,findtext=c("init_values_src","prior_like"),
                           verbose=FALSE) {
  # calc=calc; findtext=c("init_values_src","prior_like"); verbose=TRUE  
  startfile <- pathtopath(calc,"starter.ss")
  starter <- readLines(con = startfile)
  pickP <- grep(findtext[1],starter,fixed=TRUE)
  cutstart <- substr(starter[pickP[1]],2,nchar(starter[pickP[1]]))
  starter[pickP] <- paste0("1",cutstart)
  pickP2 <- grep(findtext[2],starter,fixed=TRUE)
  cutstart2 <- substr(starter[pickP2[1]],2,nchar(starter[pickP2[1]]))
  starter[pickP2] <- paste0("1",cutstart2)
  if (verbose) print(starter)
  write(starter,file=startfile)
  if (verbose) cat("New ",startfile,"  written \n")
} # end of profilestarter


#' @title sscopyto copies the ctl, dat, par, sta, and for files to new directory
#'
#' @description sscopyto copies the ctl, dat, par, sta, and for files from one
#'     directory to another, changing the name to match to destination
#'     sub-directory as appropriate. Used in conjunction with rforSS3 in the
#'     development of a bridging analyis, or to prepare for a likelihood
#'     profile, or sensitivity analysis. Now 'copyto' includes the option of
#'     copying to a completely different path and will create the 'todir' if it
#'     does not exist.
#'
#' @param origin the store directory in which the fromdir subdirectory exists
#' @param fromdir the name of the source sub-directory
#' @param todir the full path to the destination sub-directory
#' @param neworigin the new store directory, Default = NA. If it is NA then
#'     the destination store directory will be the same as the origin store. If
#'     neworigin has a full pathway in it, then it will be used instead of the
#'     origin and the todir will be created in it if it does not exist.
#'     neworigin might be akin to the 'basecase' store but be called
#'     'profile' or 'sensitivity'
#'
#' @return a vector of 1 or -1 denoting which files are transferred
#' @export
#'
#' @examples
#' \dontrun{
#' # When conducting a bridging analysis using 'store' to define the path to
#' # the various subdirecotries holding each step
#' sscopyto(store,"addcatches","addsurvey")
#' #
#' # When the bridging analysis is completed and one might want to do a
#' # set of likelihood profiles
#' sscopyto(store,"basecase17","profileM",
#'        neworigin="C:/Rcode/ss3/oro2017/profile/")
#' # is equivalent to:
#' sscopyto(origin=store,fromdir="basecase17",todir="profileM",
#'        neworigin="C:/Rcode/ss3/oro2017/profile/")
#' }
sscopyto <- function (origin, fromdir, todir, neworigin = NA) {
   sourcedir <- pathtopath(origin, fromdir)
   if (is.na(neworigin)) {
      destdir <- pathtopath(origin, todir)
   } else {
      destdir <- pathtopath(neworigin, todir)
   }
   if (!dir.exists(sourcedir))
      stop(cat(paste0(fromdir, " does not exist!   \n\n")))
   if (!dir.exists(destdir))
      dir.create(destdir, recursive = TRUE)
   postfix <- c(".ctl", ".dat", ".par", ".for", ".sta")
   numfix <- length(postfix)
   fileexist <- numeric(numfix)
   names(fileexist) <- postfix
   fil = 1
   for (fil in 1:5) {
      filename <- pathtopath(sourcedir, paste0(fromdir,postfix[fil]))
      tofile <- if (file.exists(filename)) {
         fileout <- pathtopath(destdir, paste0(todir,postfix[fil]))
         file.copy(filename, fileout, overwrite = TRUE, copy.date = TRUE)
         fileexist[fil] <- 1
      } else {
         warning(cat(filename, " does not exist in ", paste0(origin,
                                                             fromdir), "\n\n"))
      }
   }
   return(fileexist)
}
# end of sscopyto


#' @title summarizeSS3 provides a set of summary statistics from SS_output
#'
#' @description summarizeSS3 provides a set of summary statistics from the
#'     object generated by SS_output. The vector 'answer' contains the final
#'     year, the final depletion, Bzero, 1-SPR, the total likelihood, and other
#'     likelihoods. In addition, a matrix of parameters are output which
#'     contains all estimated parameters = phase > 0 (including the recruitment 
#'     deviates)
#'
#' @param replist the object generated by SS_output
#'
#' @return a list of results and estimated parameters, and all likelihoods
#' @export
#'
#' @examples
#' print("See the vignette for a worked example")
summarizeSS3 <- function(replist) {  # replist=plotreport
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


