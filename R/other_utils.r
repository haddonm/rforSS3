

#' @title changeCtl alters a parameter within the SS3.3 ctl file
#'
#' @description changeCtl alters a parameter within the SS3.3 ctl file
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
#' @param ctlfile the name of the control file to change, defaults = "ss3.ctl"
#' @param directory the directory in which the control exists that is to be
#'     changed; defaults to NA but should be calc, defined as the subdirectory
#'     in which all calculations occur within r4sessf
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
#' print("See worked example in the r4sessf vignette")
#' }
changeCtl <- function(param,newvalue,ctlfile="ss3.ctl",directory=NA,pos=3,pos2=7) {
  ctrlfile <- filenametoPath(directory,ctlfile)
  control <- readLines(con = ctrlfile)
  pickP <- grep(param,control,fixed=TRUE)
  if (length(pickP) == 0) stop(param," not in ss3.ctl")
  if (length(pickP) > 1) {
    warning("More than one ",param," in ss3.ctl; using first value")
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
  parfile <- filenametoPath(directory,"ss.par")
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

#' @title plotprep: sets up a window and the par values for a single plot
#'
#' @description plotprep: sets up a window and the par values for a single plot.
#'   it checks to see if a graphics device is open and opens a new one if not.
#'   This is simply a utility function to save typing the standard syntax.
#'   Some of the defaults can be changed. Typing the name without () will
#'   provide a template for modification. If 'windows' is called repeatedly this
#'   will generate a new active graphics device each time leaving the older ones
#'   inactive but present. For quick exploratory plots this behaviour is not
#'   wanted, hence the check if an active device exists already or not.
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param plots defaults to c(1,1), but arranges multiple plots. If used it may
#'    be necessary to print out this code and adjust the mai and oma variables
#' @param usefont default is 7 (bold Times); 1 = sans serif, 2 = sans serif bold
#' @param newdev reuse a previously defined graphics device or make a new one;
#'    defaults to TRUE
#' @param filename defaults to "" = do not save to a filename. If a filename is
#' @return Checks for and sets up a graphics device and sets the default plotting
#'   par values. This changes the current plotting options!
#' @export
#' @examples
#' \dontrun{
#' x <- rnorm(1000,mean=0,sd=1.0)
#' plotprep()
#' hist(x,breaks=30,main="",col=2)
#' }
plotprep <- function(width=6,height=4,plots=c(1,1),usefont=7,newdev=TRUE,filename="") {
  if (newdev) suppressWarnings(dev.off())
  lenfile <- nchar(filename)
  if (lenfile > 3) {
    end <- substr(filename,(lenfile-3),lenfile)
    if (end != ".png") filename <- paste0(filename,".png")
    png(filename=filename,width=width,height=height,units="in",res=300)
  } else {
  if (names(dev.cur()) %in% c("null device","RStudioGD"))
      dev.new(width=width,height=height,noRStudioGD = TRUE)
  }
  par(mfrow = plots,mai=c(0.45,0.45,0.1,0.05),oma=c(0.0,0.0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=usefont,font=usefont)
  if (lenfile > 0) cat("\n Remember to place 'graphics.off()' after the plot \n")
} # end of plotprep



#' @title printV returns a vector cbinded to 1:length(invect)
#'
#' @description printV takes an input vector and generates another vector of
#'     numbers 1:length(invect) which it cbinds to itself. This is primarily
#'     useful when trying to print out a vector which can be clumsy to read when
#'     print across the screen. applying printV leads to a single vector being
#'     printed down the screen. Deprecated, now in rutilsMH
#'
#' @param invect the input vector to be more easily visualized, this can be
#'     numbers, characters, or logical. If logical the TRUE and FALSE are
#'     converted to 1's and 0's
#'
#' @return a dataframe containing the vector 1:length(invect), and invect.
#' @export
#'
#' @examples
#' vec <- rnorm(10,mean=20,sd=2)
#' printV(vec)
#' vec <- letters
#' printV(vec)
#' vec <- c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
#' printV(vec)
printV <- function(invect) {
   n <- length(invect)
   outvect <- as.data.frame(cbind(1:n,invect))
   return(outvect)
} # end of printV


## find an index in a vector, invect, closest to given value x
## default is to return the value, set index=T to return the index

#' @title which.closest find the number closest to a given value
#'
#' @description which.closest finds either the number in a vector which is
#'     closest to the input value or its index value
#'
#' @param x the value to lookup
#' @param invect the vector in which to lookup the value x
#' @param index should the closest value be returned or its index; default=TRUE
#'
#' @return by default it returns the index in the vector of the value closest to
#'     the input  value
#' @export
#'
#' @examples
#' vals <- rnorm(100,mean=5,sd=2)
#' pick <- which.closest(5.0,vals,index=TRUE)
#' pick
#' vals[pick]
#' which.closest(5.0,vals,index=FALSE)
which.closest <- function(x,invect,index=T) {
   pick <- which.min(abs(invect-x))
   if (index) {
      return(pick)
   } else {
      return(invect[pick])
   }
} # end of which.closest



#' @title copyto copies the ctl, dat, par, sta, and for files to new directory
#'
#' @description copyto copies the ctl, dat, par, sta, and for files from one
#'     directory to another, changing the name to match to destination
#'     sub-directory as appropriate. Used in conjunction with r4sessf in the
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
#' copyto(store,"addcatches","addsurvey")
#' #
#' # When the bridging analysis is completed and one might want to do a
#' # set of likelihood profiles
#' copyto(store,"basecase17","profileM",
#'        neworigin="C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/profile/")
#' # is equivalent to:
#' copyto(origin=store,fromdir="basecase17",todir="profileM",
#'        neworigin="C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/profile/")
#' }
copyto <- function (origin, fromdir, todir, neworigin = NA) {
   sourcedir <- filenametoPath(origin, fromdir)
   if (is.na(neworigin)) {
      destdir <- filenametoPath(origin, todir)
   } else {
      destdir <- filenametoPath(neworigin, todir)
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
      filename <- filenametoPath(sourcedir, paste0(fromdir,postfix[fil]))
      tofile <- if (file.exists(filename)) {
         fileout <- filenametoPath(destdir, paste0(todir,postfix[fil]))
         file.copy(filename, fileout, overwrite = TRUE, copy.date = TRUE)
         fileexist[fil] <- 1
      } else {
         warning(cat(filename, " does not exist in ", paste0(origin,
                                                             fromdir), "\n\n"))
      }
   }
   return(fileexist)
}
# end of copyto


#' @title summarizeSS3 provides a set of summary statistics from SS_output
#'
#' @description summarizeSS3 provides a set of summary statistics from the
#'     object generated by SS_output. The vector 'answer' contains the final
#'     year, the final depletion, Bzero, 1-SPR, the total likelihood, and other
#'     likelihoods. In addition, a matrix of parameters are output which
#'     contains all parameters that start with a prior (except the
#'     recruitment devs)
#'
#' @param replist the object generated by SS_output
#'
#' @return a list of results and parameters that start with priors
#' @export
#'
#' @examples
#' \dontrun{
#' print("See the vignette for a worked example")
#' }
summarizeSS3 <- function(replist) {  # replist=plotreport
   likes <- replist$likelihoods_used
   param <- replist$parameters
   M <- param["NatM_p_1_Fem_GP_1","Value"]
   steep <- param["SR_BH_steep","Value"]
   sigR <- param["SR_sigmaR","Value"]
   pickp <- which((param[,"Pr_type"] != "dev") & (param[,"Phase"] > 0))
   param2 <- param[pickp,c("Value","Init","Prior","Pr_type","Phase","Min","Max")]
   answer <- c(round(replist$endyr),replist$current_depletion,replist$SBzero,
               (1-replist$last_years_SPR),M,steep,sigR,
               likes["TOTAL",1],likes["Survey",1],likes["Length_comp",1],
               likes["Age_comp",1],likes["Recruitment",1],likes["Parm_priors",1])
   names(answer) <-  c("EndYear","Depletion","Bzero","1-SPR","M","h","sigmaR","TotalL","Index",
                       "LengthCompL","AgeCompL","Recruit","Param_Priors")
   return(list(answer=answer,param=param2))
}


