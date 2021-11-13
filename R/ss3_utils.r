
#' @title cleanDir - removes any previous analyses and plots stored in 'SubDir'
#'
#' @description cleanDir -  removes any previous analyses and plots stored in
#'    'SubDir'. Apart from the plot directory and contents it also removes
#'    checkup.sso, CompReport.sso, covar.sso, CumReport.sso,
#'    echoinput.sso, Forecast-report.sso, ParmTrace.sso, Report.sso,
#'    SIS_table.sso, warning.sso, ss3.cor, ss3.std, ss3.rep, 1rundetails.txt,
#'    2rundetails.txt, and 3rundetails.txt.  if the files are missing a warning
#'    is given, which can be ignored. If they are presnet lots of TRUEs will
#'    be printed.
#' @param Dir - the storage directory containing the subdirectories for each of
#'    the bridging analyses.
#' @param SubDir - the bridging analysis subdirectory under consideration
#' @return The files listed under description are removed from 'SubDir';
#' @export cleanDir
#' @examples
#' \dontrun{
#' library(r4sessf)  # Dir <- store; SubDir <- "basecase"
#' print("An example has still to be written")
#' }
cleanDir <- function(Dir,SubDir) {
   delfiles <- c("CompReport.sso","covar.sso","CumReport.sso",
                 "echoinput.sso","Forecast-report.sso","ParmTrace.sso",
                 "Report.sso","SIS_table.sso","warning.sso","ss.cor","ss.std",
                 "ss.rep","1rundetails.txt","2rundetails.txt","3rundetails.txt")
   targetpath <- filenametoPath(Dir,SubDir)
   removefiles <- filenametoPath(targetpath,delfiles)
   numfiles <- length(removefiles)
   for (i in 1:numfiles) {
      if (file.exists(removefiles[i])) {
         file.remove(removefiles[i])
      } else {
         cat(paste0(delfiles[i],"  is not present  \n"))
      }
   }
   targetpath <- filenametoPath(targetpath,"plots")
   file.remove(dir(targetpath,full.names=TRUE))
}  # end of cleanDir


#' @title codeBlock - delineate some comment lines ready to document code
#'
#' @description codeBlock - To assist in breaking up R files into sections this
#'    function generates a block of hashes which can be pasted into an R files
#'    so that only the descriptive text need be altered
#' @param rows - the number of open rows within the code block ready for text;
#' @return a block of hashes with space for descriptive text
#' @export codeBlock
#' @examples
#' \dontrun{
#' library(r4sessf)
#' codeBlock()
#' codeBlock(6)
#' }
codeBlock <- function(rows=2) {
   cat("#------------------------------------------------------------------\n")
   for (i in 1:rows) cat("#  comment  \n")
   cat("#------------------------------------------------------------------\n")
}

#' @title copyfiles - copies required files from store to the calc directory
#'
#' @description copyfiles - copies required files from store to the calc
#'    directory. An example store directory might be named 'balanced' or
#'    'basecase' and the files named basecase.ctl, basecase.dat, etc. The
#'    files copied are the ctl, dat, par, forcast, and starter
#'    files. In the process of copying the analysis name ('basecase') is
#'    replaced with SS3 ready for the SS3 software.
#' @param x - the analysis to be conducted
#' @param origin - the directory name as the source of the files to copy
#' @param destination - the calc directory into which the files are copied
#' @return Nothing, but it copies the files from the storage directory to
#'    the calc directory ready to run SS3
#' @export copyfiles
#' @examples
#' \dontrun{
#' # library(r4sessf)  # x <- analysis; origin <- store; destination=calc
#' print("An example has still to be written")
#' }
copyfiles <- function(x,origin,destination) {
   postfix <- c(".ctl",".dat",".par",".for",".sta")
   outfile <- c("ss3.ctl","ss3.dat","ss.par","forecast.ss","starter.ss")
   numfix <- length(postfix)
   fileexist <- numeric(numfix)
   for (fil in 1:numfix) {
      filename <- paste0(origin,x,"/",x,postfix[fil])
      if (file.exists(filename)) {
         fileout <- paste0(destination,outfile[fil])
         file.copy(filename,fileout,overwrite=TRUE,copy.date=TRUE)
         fileexist[fil] <- 1
      }
   }
   if (sum(fileexist) != numfix) {
      pick <- which(fileexist == 0)
      label <- paste0("Missing file: ",
                      paste0(origin,x,"/",x,postfix[pick]),"\n")
      warning(label)
   }
} # end of copyfiles

#' @title dirExists: Checks for the existence of a directory
#'
#' @description dirExists: Creates a directory if it does not already exist
#'   it use dir.create but avoids the warning message is one already exists
#' @param indir a character string containing the name of the directory to
#'   be created if it does not already exist
#' @return a message to the screen if the directory exists or is created; if
#'   the latter then it also creates the directory as listed in 'indir'.
#' @export dirExists
#' @examples
#' \dontrun{
#' # library(r4sessf)
#' indirect <- getwd()
#' dirExists(indirect)
#' }
dirExists <- function(indir) {
   if (dir.exists(indir)) {
      cat(indir,":  exists  \n")
   } else {
      dir.create(file.path(indir))
      cat(indir,":  created  \n")
   }
}  # end of dirExists


#' @title filenametoPath - safely add a filename or subdirectory to a path
#'
#' @description filenametoPath - safely add a filename or subdirectory to a path
#'    uses pathtype to get the seperator and then checks the end character.
#'    If the separator is nothing of a '/' or a '//' then it reacts accordingly.
#'    Without this one can unwittingly include extra separators or none at all.
#'
#' @param inpath - the path to be analysed
#' @param infile - the filename or subdirectory to be added to the path 'inpath'
#'
#' @return the completed filename or extended path
#' @export
#' @examples
#' \dontrun{
#' resultpath <- "C:\\place\\Rcode\\place2\\dir3\\"
#' infile <- "starter.ss"
#' filenametoPath(resultpath,infile)
#' }
filenametoPath <- function(inpath,infile) {
   typepath <- pathtype(inpath)
   endpath <- pathend(inpath)
   if (is.na(endpath)) {
      outfile <- paste(inpath,infile,sep=typepath)
   } else { outfile <- paste(inpath,infile,sep="")
   }
   return(outfile)
} # end of filenametoPath


#' @title firstNum - converts the first string in a vector to a number
#'
#' @description firstNum - converts the first string in a vector to a number.
#'    If the first part of the string is not a number then firstNum will
#'    issue a stop warning if the first part is not a number. If there are
#'    tab markers present these will first be converted to spaces.
#' @param intxt - a vector of characters, the first one of which is a number
#' @return a single number
#' @export firstNum
#' @examples
#' \dontrun{
#' # library(r4sessf)
#' inputtxt <- "1  2   3   4"
#' x <- unlist(strsplit(inputtxt," "))
#' print(x)
#' print(firstNum(x))
#' }
firstNum <- function(intxt) {
   intxt <- gsub("\\t"," ",intxt)
   vect <- removeEmpty(unlist(strsplit(intxt," ")))
   ans <- suppressWarnings(as.numeric(vect[1]))
   if (is.na(ans)) stop("First part of ",intxt," not a number")
   return(ans)
} # end of firstNum


#' @title fixstarter - saves a new starter.ss file ready to use the par file
#'
#' @description fixstarter - saves a new starter.ss file ready to use the
#'    par file. This entails reading in the current starter.ss file, finding
#'    the line with 'use init value' in it, changing the number at the start
#'    from 0 to 1, and resaving the file.
#' @param directory - the calculation directory in which starter.ss can be found
#' @param toscreen - defaults to FALSE, meaning no information is sent ot the
#'    screen when this function is called.
#' @return starter.ss is modified so that ss3 will use the previously
#'    estimated par file as the starting point in the estimation
#' @export fixstarter
#' @examples
#' \dontrun{
#' #library(r4sessf)
#' print("An example has still to be written")
#' }
fixstarter <- function(directory,toscreen=FALSE) {
   startfile <- filenametoPath(directory,"starter.ss")
   starter <- readLines(con = startfile)
   pickP <- grep("use init value",starter,fixed=TRUE)
   if (length(pickP) != 1) stop("More than one '0=use init values' in STARTER.SS")
   cutstart <- substr(starter[pickP[1]],2,nchar(starter[pickP[1]]))
   starter[pickP] <- paste0("1",cutstart)
   if (toscreen) print(starter)
   write(starter,file=startfile)
   cat("New ",startfile,"  written \n")
}  # end of projstarter

# parse a single line to get the single number at the front but
# to keep the text following a '#'
# txtlist <- repout; index <- pickRec
#' @title getfirst - parse a given line to get a number occurring before #
#'
#' @description getfirst - parse a single line to get the single number
#'    at the front brealng the line up at '#'
#' @param txtlist - the results of applying readLines to the analysis.txt,
#'    where 'analysis' is the name of the store directory being considered
#' @param index the location in the list of character strings to be parsed
#' @param rnd - should the resulting number be truncated? Default = TRUE
#' @return a single number deriving from analysis.txt used in the automatic
#'    balancing of variances or recruitment residuals
#' @export getfirst
#' @examples
#' \dontrun{
#' library(r4sessf)
#' print("being developed")
#' #getfirst(txt,2,rnd=FALSE)
#' }
getfirst <- function(txtlist,index,rnd=TRUE) {
   twopiece <- removeEmpty(unlist(strsplit(txtlist[index],"#")))
   if (rnd) {
      newyear <- round(as.numeric(twopiece[1]))
   } else {
      newyear <- as.numeric(twopiece[1])
   }
   return(newyear)
} # end of getfirst

#' @title getCase - given a list of subdirectories this return the selected one
#'
#' @description - getCase - in a bridging analysis where one migrates an old assessment
#'    to a new basecase one uses a 'basecase' directory to hold the subdirectories
#'    chosen to hold each step in the bridging analysis. getCase simply holds this
#'    vector of names and returns the selected one, optionally printing the name
#'    to the screen. This functions main use is to simplify the use of the vector of
#'    subdirectory names so that if one wants to add news one it stays simple.
#' @param basecase - the vector of names of the brdiging analyses
#' @param index - the index of the analysis wanted
#' @param printout - a logical variable determining whether to print the name to the
#'    screen or not; defaults to TRUE
#' @return a character string containing the name of the subdirectory contianing the
#'    ctl, dat, sta, for, and par files for the particular analysis wanted.
#' @export getCase
#' @examples
#' \dontrun{
#' basecase <- c("origbase24f","origbase24fb","origbase24z","newCatCE","newRecs",
#' "newRecRB","newAAL","newAgeComp","ageingerror","newLenComp",
#' "newLenatAge","balanced")
#' case <- 3
#' getCase(basecase,case)
#' }
getCase <- function(basecase,index=1,printout=TRUE) {
   if (printout) print(cbind(1:length(basecase),basecase),quote = FALSE)
   return(basecase[index])
} # end of getCase

#' @title getlast - parse a given line to get a text occurring after #
#'
#' @description getlast - parse a single line to get the text occurring
#'    after a '#'
#' @param txtlist - the results of applying readLines to the analysis.txt,
#'    where 'analysis' is the name of the store directory being considered
#' @param index the location in the list of character strings to be parsed
#' @return a character string starting with # to be added to the end of
#'    the number obtained from getfirst, used in the automatic
#'    balancing of variances or recruitment residuals
#' @export getlast
#' @examples
#' \dontrun{
#' library(r4sessf)
#' print("being developed")
#' #getlast(txt,2)
#' }
getlast <- function(txtlist,index) {
   twopiece <- removeEmpty(unlist(strsplit(txtlist[index],"#")))
   twopiece[2] <- paste0("# ",twopiece[2])
   return(twopiece[2])
}  # end of get last


#' @title getNum - converts a given string in a vector to a number
#'
#' @description getNum - converts a given string in a vector to a
#'    number
#' @param intxt - a character vector containing the number wanted
#' @param index - the index in the vector of character objects that
#'    is to be converted to a number; defaults to 1
#' @return a single number
#' @export getNum
#' @examples
#' \dontrun{
#' # library(r4sessf)
#' inputtxt <- "[1]  2   3   4"
#' x <- unlist(strsplit(inputtxt," "))
#' print(x)
#' print(getNum(x,index=2))
#' }
getNum <- function(intxt,index=1) {
   vect <- unlist(strsplit(intxt," "))
   ans <- suppressWarnings(as.numeric(vect[index]))
   if (is.na(ans)) warning("The index in getNum is not pointing to a number")
   return(ans)
} # end of getNum


#' @title getStatus - extract the values of B0 and Current Depletion
#'
#' @description getStatus - extracts the values of B0 and Current
#'    Depletion from the analysis.txt file readin using readLines.
#' @param txtlist - the list of character statements derived from
#'    applyig readLines to the output file analysis.txt
#' @return a vector of B0 and current Depltion
#' @export getStatus
#' @examples
#' \dontrun{
#' # library(r4sessf)
#' print("run an example and apply this to the output file")
#' }
getStatus <- function(txtlist) {  # txtlist <- plotreport
   index <- grep("SBzero",txtlist)
   sprs <- txtlist[index][[1]]
   Bzero <- sprs[1,"SSBZero"]
   index <- grep("current_depletion",txtlist)
   depl  <- getNum(txtlist[index+1],2)
   ans <- c(Bzero,depl)
   names(ans) <- c("B0","Depletion")
   return(ans)
}  # end of getStatus

#' @title makeLabel: Convert a vector of numbers or strings into a single label
#'
#' @description makeLabel: Convert a vector of numbers of strings into a single
#'   label
#' @param invect the vector of numbers or strings to be converted into a
#'   single string.
#' @param insep defaults to '_' but can be any selected character used to
#'   separate each value
#' @return a character string containing the invect as a single string.
#' @export makeLabel
#' @examples
#' \dontrun{
#' library(r4sessf)
#' x <- c(1,2,3,4,5)
#' makeLabel(x)
#' makeLabel(x,"-")
#' }
makeLabel <- function(invect,insep="_") {
   nlab <- length(invect)
   invect <- as.character(invect)
   ans <- invect[1]
   if (nlab > 1) for (i in 2:nlab) ans <- paste(ans,invect[i],sep=insep)
   return(ans)
}  # end of makeLabel

#' @title pathend - determines what character is at the end of a path
#'
#' @description pathend - determines what character is at the end of a path
#'    uses pathtype to get the seperator and then checks the end character
#' @param inpath - the path to be analysed
#' @return the end character of the path; either NA, '/', or '\\'
#' @export pathend
#' @examples
#' \dontrun{
#' resultpath <- "C:\\place\\Rcode\\place2\\dir3\\"
#' pathend(resultpath)
#' }
pathend <- function(inpath) {
   lookfor <- pathtype(inpath)
   endpath <- NA
   if (lookfor == "/") {
      if(length(grep("/$",inpath)) > 0) endpath <- "/"
   } else {
      if(length(grep("\\\\$",inpath)) > 0) endpath <- "\\"
   }
   return(endpath)
} # end of pathend

#' @title pathtype - finds the type of separator used in a path
#'
#' @description pathtype - finds the type of separator used in a path;
#'    this is either a '/' or a '\\'
#' @param inpath - the path to be analysed
#' @return the type of path divider, either a 0 = '\\' or a
#'    1 = '/'
#' @export pathtype
#' @examples
#' \dontrun{
#' resultpath <- "C:\\place\\Rcode\\place2\\dir3\\"
#' pathtype(resultpath)
#' }
pathtype <- function(inpath) {
  typepath <- "/"
  if (length(grep("\\\\",inpath)) > 0) typepath <- "\\"
  return(typepath)
} # end of pathtype



#' @title runSS3 - calls SS3 -nohess twice, then SS3 to calculate the hessian
#'
#' @description runSS3 - this switches to /calc, sets up and calls SS3 -nohess,
#'    then alters starter.ss to use the estimated .par file from the first run.
#'    It recalls SS3 -nohess. Finally, it re-uses the new .par file in a call to
#'    SS3, which also estimates the Hessian matrix. Finally it returns to the
#'    working directory.
#' @param wkdir - the full working directory eg fld2016
#' @param exec the name of the executable within the calc subdirectory to use
#'     with the particular set of SS files
#' @param calcdir - the directory in which all calculations occur; defaults to
#'    "calc/"
#' @return conducts three separate SS3 runs for the input data. Sends messages
#'    to the screen for each run. Generates numerous files in teh /calc directory.
#' @export runSS3
#' @examples
#' \dontrun{
#' print("Still need to develop a real example using included datasets")
#' }
runSS3 <- function(wkdir,exec="SS3",calcdir="calc/") {
      cat("Switching to the calc directory and beginning the run. \n")
      calc <- paste0(wkdir,calcdir)
      setwd(calc)
      command <- paste0(calc,exec,".exe -nohess")
      command1 <- paste0(command," > ",paste0(calc,"1rundetails.txt"))
      command2 <- paste0(command," > ",paste0(calc,"2rundetails.txt"))
      shell(command1,wait=TRUE,invisible=T)
      # modify the starter.ss file here to use the par file
      cat("\n")
      cat("End of first -nohess Run  \n")
      fixstarter(calc,toscreen=F)
      cat("starter.ss file modified to use the .par file \n\n")
      shell(command2,wait=TRUE,invisible=T)
      cat("\n  End of second -nohess Run  \n")
      cat(" Final run, with Hessian generation \n\n")
      command <- paste0(calc,exec,".exe")
      command3 <- paste0(command," > ",paste0(calc,"3rundetails.txt"))
      shell(command3,wait=TRUE,invisible=T)
      setwd(wkdir)
}  # end of runSS3



#' @title removeEmpty - removes empty strings from a vector of strings
#'
#' @description removeEmpty - removes empty strings from a vector of strings
#' @param invect - a vector of characters
#' @return a vector of strings without empty ones
#' @export removeEmpty
#' @examples
#' \dontrun{
#' # library(r4sessf)
#' inputtxt <- "a  b   c   g"
#' x <- unlist(strsplit(inputtxt," "))
#' print(x)
#' print(removeEmpty(x))
#' }
removeEmpty <- function(invect) {
   return(invect[nchar(invect) > 0])
}


#' @title summarySS3 pulls out summary information from report file
#'
#' @description summarySS3 pulls out summary information from report file
#'
#' @param outfile - text file containing screen dump from SS_output and SS_plots
#'
#' @return a list containing run details, number of paramters, and the
#'    likelihoods estimated
#' @export summarySS3
#'
#' @examples
#' \dontrun{
#' print("After using runSS3 on an 'analysis' this will generate an")
#' print("'analysis.txt' file in the 'analysis' sub-directory.")
#' print("summarySS3('analysis.txt')")
#' }
summarySS3 <- function(outfile) {  # outfile <- fileout
   repout <- readLines(outfile)
   pickW <- grep("warnings",repout)[3]
   pickL <- grep("likelihoods_used",repout)
   pickLR <- grep("likelihoods_by_fleet",repout)
   header <- repout[(pickW + 1):(pickL - 1)]
   header <- unlist(strsplit(header,'"'))
   header <- header[-grep("]",header)]
   header <- removeEmpty(gsub(" ","",header))
   pickP <- grep("N_estimated_parameters",repout)
   npar <- as.numeric(unlist(strsplit(repout[pickP+1]," "))[2])
   likelihoods <- repout[(pickL + 2):(pickLR - 1)]
   likelihoods <- removeEmpty(unlist(strsplit(likelihoods," ")))
   nlike <- length(likelihoods)
   likes <- as.data.frame(matrix(likelihoods,nrow=nlike/3,ncol=3,byrow=TRUE))
   rownames(likes) <- likes[,1]; likes <- likes[,-1]
   colnames(likes) <- c("Likelihood","Lambda")
   likes[,"Likelihood"] <- as.numeric(likes[,"Likelihood"])
   likes[,"Lambda"] <- as.numeric(likes[,"Lambda"])
   pickB0 <- grep("SBzero",repout)
   Bzero <- as.numeric(unlist(strsplit(repout[pickB0+1]," "))[2])
   pickD <- grep("current_depletion",repout)
   depl <- as.numeric(unlist(strsplit(repout[pickD+1]," "))[2])
   answer <- list(header=header,npar=npar,likes=likes,Bzero=Bzero,Depletion=depl)
   return(answer)
}

#' @title storeresults - Copies result files back into destination directory
#'
#' @description storeresults - Stores result files back into destination
#'    directory; the origin would usually be 'calc'and the destination would
#'    generally be the directory from which the SS3 files were copied. The
#'    files copied are: checkup.sso, CompReport.sso, covar.sso, CumReport.sso,
#'    echoinput.sso, Forecast-report.sso, ParmTrace.sso, Report.sso,
#'    SIS_table.sso, warning.sso, ss3.ctl, ss3.dat, ss3.par, ss3.rep, ss3.cor,
#'    ss3.std, forecast.ss, starter.ss, 1rundetails.txt, 2rundetails.txt,
#'    3rundetails.txt
#' @param origin - the directory from which the files are t obe copied.
#' @param destination - the directory into which the files are to be copied.
#' @return The files listed under description are copied to the destination
#'    directory. If any are missing a warning is given
#' @export storeresults
#' @examples
#' \dontrun{
#' library(r4sessf)
#' print("An example has still to be written")
#' }
storeresults <- function(origin,destination) {
   getfiles <- c("CompReport.sso","covar.sso","CumReport.sso",
                 "echoinput.sso","Forecast-report.sso","ParmTrace.sso",
                 "Report.sso","SIS_table.sso","warning.sso","ss3.ctl",
                 "ss3.dat","ss.par","ss.rep","ss.cor","ss.std",
                 "forecast.ss","starter.ss","wtatage.ss_new",
                 "1rundetails.txt","2rundetails.txt","3rundetails.txt")
   nfiles <- length(getfiles)
   for (fil in 1:nfiles) { # fil <- 1
      x <- getfiles[fil]
      filename <- filenametoPath(origin,x)
      if (file.exists(filename)) {
         fileout <- filenametoPath(destination,x)
         file.copy(filename,fileout,overwrite=TRUE,copy.date=TRUE)
      } else {
         warning(paste0(x,"  missing from 'calc'   \n"))
      }
   }
} # end of storeresults




