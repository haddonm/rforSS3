

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
#' # x <- analysis; origin <- store; destination=calc
#' print("An example has still to be written")
#' }
copyfiles <- function(x,origin,destination) {
  postfix <- c(".ctl",".dat",".for",".sta",".par")
  outfile <- c("ss.ctl","ss.dat","forecast.ss","starter.ss","ss3.par")
  numfix <- length(postfix)
  fileexist <- numeric(numfix)
  for (fil in 1:numfix) {
    filename <- paste0(origin,x,"/",x,postfix[fil])
    if (file.exists(filename)) {
      fileout <- paste0(destination,outfile[fil])
      file.copy(filename,fileout,overwrite=TRUE,copy.date=FALSE)
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
#'   
#' @param indir a character string containing the name of the directory to
#'   be created if it does not already exist
#' @param create default = TRUE, should the input directory be created if it
#'     does not already exist?
#'   
#' @return a message to the screen if the directory exists or is created; if
#'   the latter then it also creates the directory as listed in 'indir'.
#'   
#' @export dirExists
#' @examples
#' \dontrun{
#' indirect <- getwd()
#' dirExists(indirect,create=FALSE)
#' }
dirExists <- function(indir,create=TRUE) {
   if (dir.exists(indir)) {
      cat(indir,":  exists  \n")
   } else {
     if (create) {
       dir.create(file.path(indir))
       cat(indir,":  created  \n")
     } else {
       label <- paste0(indir," does not exist, would you like to create it?")
       if (askYesNo(label,
                  prompts=getOption("askYesNo", gettext(c("y", "y", "Cancel"))))) {
         dir.create(file.path(indir))
         cat(indir,":  created  \n")
       }
     }
   }
}  # end of dirExists


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
#' @param directory the calculation directory in which starter.ss can be found
#' @param findtext the text that identifies the line in starter.ss that 
#'     contains the line that needs modification.
#' @param toscreen defaults to FALSE, meaning no information is sent to the
#'    screen when this function is called.
#' @return starter.ss is modified so that ss3 will use the previously
#'    estimated par file as the starting point in the estimation
#' @export fixstarter
#' @examples
#' print("An example has still to be written")
#' # typical syntax  fixstarter(calc,findtext="init_values_src")
fixstarter <- function(directory,findtext="use init value",toscreen=FALSE) {
   startfile <- pathtopath(directory,"starter.ss")
   starter <- readLines(con = startfile)
   pickP <- grep(findtext,starter,fixed=TRUE)
   if (length(pickP) != 1) stop("More than one or no ",findtext," in starter.ss")
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
#' @description getStatus extracts the values of all the likelihoods used, as
#'    well as the B0 and Current Depletion. Best displayed with round(,7).
#'    
#' @param txtlist the output object from applying the SS_output function in SS3
#' 
#' @return a vector of all likelihoods, B0 and current depletion
#' @export getStatus
#' 
#' @examples
#' print("run an example or load a plotreport file and apply this function")
getStatus <- function(txtlist) {  # txtlist <- plotreport
  tmp <- txtlist$likelihoods_used
  label <- rownames(tmp)
  rbind(tmp,c(txtlist$SBzero,NA),c(txtlist$current_depletion,NA))
  tmp1 <- rbind(tmp,c(txtlist$SBzero,NA),c(txtlist$current_depletion,NA))
  rownames(tmp1) <- c(label,"SBzero","Current_Depl")
  return(tmp1)
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


#' @title removeEmpty - removes empty strings from a vector of strings
#'
#' @description removeEmpty - removes empty strings from a vector of strings
#' @param invect - a vector of characters
#' @return a vector of strings without empty ones
#' @export removeEmpty
#' @examples
#' \dontrun{
#' inputtxt <- "a  b   c   g"
#' x <- unlist(strsplit(inputtxt," "))
#' print(x)
#' print(removeEmpty(x))
#' }
removeEmpty <- function(invect) {
   return(invect[nchar(invect) > 0])
}

#' @title sel24 implements SS3's selectivity pattern 24
#' 
#' @description sel24 uses 6 parameters and a set of mean size or age classes 
#'     to calculate SS3's selectivity pattern 24, which is a double normal with 
#'     a defined peak at 1.0, and tail controls, that is it has parameters for 
#'     the selectivity of the initial and final size/age classes. There is an 
#'     ascending limb and a descending limb with the potential of a plateau in 
#'     between. The six parameters are 1) the age/size where selectivity first 
#'     becomes 1.0, 2) the size/age where selectivity first begins to decline, 
#'     3) the steepness or width of the ascending limb, 4) the steepness or
#'     width of the descending limb, 5) the selectivity of the first age/size 
#'     class, and 6) the selectivity of the last age/size class. The descending 
#'     limb of any dome shaped selectivity curves imply that the fishing gear 
#'     used is unable to collect all representatives of the larger or older 
#'     classes. The predicted numbers of smaller or younger animals, that are 
#'     only partially selected, are inflated because of the partial selection. 
#'     If any larger or older animals are, in fact, caught, then the same 
#'     inflation can happen to those animals as a result of the partial 
#'     selection implied by the dome shape. Small and young animals weigh very 
#'     little, the same cannot be said for the larger or older animals. Some 
#'     people refer to the extra biomass this phenomenon can imply as 'ghost 
#'     biomass', even though it might be real. Whatever the case, when using 
#'     dome shaped selectivity it is best to be aware of this issue and to be 
#'     cautious about how this is interpreted. A version of this function was
#'     first developed for the MQMF package (Haddon, 2023).
#'
#' @param p a vector of six parameters.
#' @param L a vector of the mean of nL age/size classes
#'
#' @return a vector of selectivity at length/age
#' @export
#' 
#' @references Methot, R.D. and C.R, Wetzel (2013) Stock synthesis: A biological 
#'     and statistical framework for fish stock assessment and fishery 
#'     management. Supplementary material, Appendix A. Equs A1.30 onwards. 
#'     \emph{Fisheries Research} 142:86-99.
#'     
#' @references Haddon M (2023). \emph{MQMF: Modelling and Quantitative Methods 
#'     in Fisheries}. R package version 0.1.5,<https://github.com/haddonm/MQMF/>.
#'
#' @examples
#'   L <- seq(180,410,1)
#'   p <- p <- c(230,250,155,600,-5,1.5)
#'   sel <- sel24(p,L)
#'   plot(L,sel,type="l",xlab="Age",ylab="Selectivity",lwd=2)
sel24 <- function(p,L) {
  nL <- length(L)
  comp1 <- 1/(1 + exp(-p[5]))
  comp2 <- exp((-(L - p[1])^2)/p[3])
  comp3 <- exp((-(L[1] - p[1])^2)/p[3])
  asc <- comp1 + (1 - comp1) * ((comp2 - comp3)/(1 - comp3))
  comp4 <- 1/(1 + exp(-p[6]))
  comp5 <- exp((-(L - p[2])^2)/p[4])
  comp6 <- exp((-(L[nL] - p[2])^2)/(p[4]-1))
  dsc <- 1 + (comp4 - 1) * ((comp5 - 1)/(comp6 - 1))
  J1 <- 1/(1 + exp(-(20*(L - p[1])/(1 + abs(L - p[1])))))
  J2 <- 1/(1 + exp(-20*((L - p[2])/(1 + abs(L - p[2])))))
  sel <- (asc * (1 - J1)) + J1 * (1 - J2 + dsc * J2)
  return(sel)
} # end of sel24


#' @title summarySS3 pulls out summary information from report file
#'
#' @description summarySS3 pulls out summary information from report file
#'
#' @param outfile - text file containing screen dump from SS_output and SS_plots
#'
#' @return a list containing run details, number of parameters, and the
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
#'    generally be the directory from which the SS3 files were copied.
#'    
#' @param origin - the directory from which the files are to be copied.
#' @param destination - the directory into which the files are to be copied.
#' @param getfiles a vector of filenames to be retrieved from the origin
#'     directory (usually calc) and copied to the destination directory. The
#'     current files chosen are: CompReport.sso, covar.sso, CumReport.sso,
#'     echoinput.sso, Forecast-report.sso, ParmTrace.sso Report.sso, 
#'     SIS_table.sso, warning.sso, ss3.par, ss3.std, control.ss_new, and
#'     starter.ss_new
#' 
#' @return The files listed under getfiles are copied from the origin to the 
#'     destination directory. If any are missing a warning is given
#'     
#' @export
#' 
#' @examples
#' \dontrun{
#' # typical syntax
#'  store <- pathtopath(wkdir,"basecase/")  # define directories
#'  calc <- pathtopath(wkdir,"calc/")
#'  
#'  item <- 7    #  which scenario subdirectory inside basecase to be run
#'  getCase(index=item,basecase) # this lists the basecase indices to the screen
#'  analysis <- getCase(index=item,basecase)  # define analysis directory
#'  destination <- pathtopath(store,analysis)
#'  # run SS3
#'  storeresults(calc,destination)
#' }
storeresults <- function(origin,destination,getfiles=c("CompReport.sso",
                         "covar.sso","CumReport.sso","echoinput.sso",
                         "Forecast-report.sso","ParmTrace.sso","Report.sso",
                         "SIS_table.sso","warning.sso","ss3.par","ss3.std",
                         "control.ss_new","starter.ss_new")) {
   getfiles <- c("CompReport.sso","covar.sso","CumReport.sso",
                 "echoinput.sso","Forecast-report.sso","ParmTrace.sso",
                 "Report.sso","SIS_table.sso","warning.sso","ss3.par","ss3.std",
                 "control.ss_new","starter.ss_new")
   nfiles <- length(getfiles)
   for (fil in 1:nfiles) { # fil <- 1
      x <- getfiles[fil]
      filename <- pathtopath(origin,x)
      if (file.exists(filename)) {
         fileout <- pathtopath(destination,x)
         file.copy(filename,fileout,overwrite=TRUE,copy.date=TRUE)
      } else {
         warning(paste0(x,"  missing from 'calc'   \n"))
      }
   }
} # end of storeresults




