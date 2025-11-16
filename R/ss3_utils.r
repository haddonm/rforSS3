
#' @title altcpueplot plots the fit to cpue and residuals for all fleets
#' 
#' @description altcpueplot generates a pair of plots for each fleet with cpue
#'     data. The first is observed vs expected (the fit) and the second is an
#'     explicit residual plot on a linear scale.
#'
#' @param cpue the plotreport$cpue object from r4ss
#' @param analysis the name of the model scenario
#' @param rundir the directory into which to place the plot if saved
#' @param height the height of the plot, the default = 8, which suits 4 fleets.
#'     if you have fewer then adjust this appropriately. If more than 5 then
#'     external to the function it may be best to subset the cpue input data
#'     and annotate the analysis argument to keep each plot separate. 
#' @param CI default = TRUE confidence bounds will be plotted. 
#' @param console default = TRUE so plot goes to console.
#'
#' @returns invisibly the filename, it also plots a graph
#' @export
#'
#' @examples
#' # syntax: 
#' # extradir <- pathtopath(destination,"extra/")
#' # filename <- altcpueplot(plotreport$cpue,analysis="SGBC-5-4-8-6",
#' #                         rundir=extradir,height=8,CI=TRUE,console=TRUE)
altcpueplot <- function(cpue,analysis,rundir,height=8,CI=TRUE,console=TRUE) { 
  colnames(cpue) <- tolower(colnames(cpue))
  fleets <- unique(cpue[,"fleet_name"])
  nfleet <- length(fleets)
  yrrange <- range(cpue[,"yr"])
  if (console) { filen="" } else {
    fileout <- paste0(analysis,"_CPUE_for_allfleets.png")
    filen <- pathtopath(rundir,fileout)
  }
  plotprep(width=9,height=height,newdev=FALSE,filename=filen,verbose=FALSE)
  parset(plots=c(nfleet,2),margin=c(0.25,0.4,0.05,0.1))
  for (fl in 1:nfleet) { # fl = 1
    dat <- cpue[cpue[,"fleet_name"]==fleets[fl],]
    maxy <- getmax(dat[,c("obs","exp")])    
    if (CI) {
      lower <- qlnorm(.025,meanlog = log(dat[,"obs"]),sdlog = dat[,"se"])
      upper <- qlnorm(.975,meanlog = log(dat[,"obs"]),sdlog = dat[,"se"])
      maxy <- getmax(upper)
    }
    rown <- nrow(dat)
    
    plot(dat[,"yr"],dat[,"exp"],type="l",lwd=2,xlab="",ylab=fleets[fl],
         ylim=c(0,maxy),xlim=c(yrrange[1],yrrange[2]),panel.first=grid())
    points(dat[,"yr"],dat[,"obs"],pch=16,cex=1.0,col=2)
    if (CI) {
      arrows(x0=dat[,"yr"],y0=lower,x1=dat[,"yr"],y1=upper,
             length=0.025,angle=90,col=1,lwd=1,code=3)
    }
    plot(dat[,"yr"],dat[,"dev"],type="p",pch=16,cex=1.0,xlab="",ylab="Deviate",
         xlim=c(yrrange[1],yrrange[2]),panel.first=grid())
    abline(h=0.0,lwd=1.0,col=1)
    for (i in 1:rown) lines(c(dat[i,"yr"],dat[i,"yr"]),c(0.0,dat[i,"dev"]),lwd=1)
  }
  if (!console) dev.off()
  return(invisible(filen))
} # end of altcpueplot



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
#' # x=analysis; origin=store; destination=calc
#' }
copyfiles <- function(x,origin,destination) {
  postfix <- c(".ctl",".dat",".for",".sta",".par")
  outfile <- c("ss.ctl","ss.dat","forecast.ss","starter.ss","ss3.par")
  numfix <- length(postfix)
  fileexist <- numeric(numfix)
  for (fil in 1:numfix) {
    first <- pathtopath(origin,x)
    second <- paste0(x,postfix[fil])
    filename <- pathtopath(first,second)
    if (file.exists(filename)) {
      fileout <- pathtopath(destination,outfile[fil])
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

#' @title domed calculates domed selectivity curves
#' 
#' @description domed uses 6 parameters and a set of mean size or age classes 
#'     to calculate a domed selectivity curve with a maximum of 1.0 (rescaling 
#'     can be done outside the function), but has parameters for the selectivity 
#'     of the initial and final size/age classes. There is an ascending limb and 
#'     a descending limb with the potential of a plateau in between. The six 
#'     parameters are:
#'     
#'     1. the age/size where selectivity first becomes 1.0, peak1
#'     
#'     2. the size/age where selectivity first begins to decline, peak2
#'     
#'     3. the steepness of the ascending limb, asc ln(width)
#'     
#'     4. the steepness of the descending limb, dsc  ln(width)
#'     
#'     5. the selectivity of the first age/size class, selmin, and 
#'     
#'     6. the selectivity of the last age/size class, selmax 
#'     
#'     The selectivity of the first and last composition classes, selmin and 
#'     selmax, are the inverse logit transformation of the value used in the 
#'     calculations.The equations here are a modified form of those in 
#'     Hurtado-Ferro et al, (2014) combined with some from Methot and Wetzel, 
#'     (2013).
#'     The descending limb of any dome shaped selectivity curves imply that the 
#'     fishing gear used is unable to collect all representatives of the larger 
#'     or older classes. The predicted numbers of smaller or younger animals, 
#'     that are only partially selected, are inflated because of the partial 
#'     selection. If any larger or older animals are, in fact, caught, then the 
#'     same inflation can happen to those animals as a result of the partial 
#'     selection implied by the dome shape. Small and young animals weight 
#'     very little, the same cannot be said for the larger or older animals. 
#'     Some people refer to the extra biomass this phenomenon can imply as 
#'     'ghost biomass', even though it might be real. Whatever the case, when 
#'     using dome shaped selectivity it is best to be aware of this issue and 
#'     to be cautious about how this is interpreted. The 20* terms in the J1 
#'     and J2 factors are required to force the joins to be as effective as
#'     required (see Methot and Wetzel). 
#'
#' @param p a vector of six parameters.
#' @param L a vector of the mean of nL age/size classes
#'
#' @return a vector of selectivities
#' @export
#' 
#' @references Methot, R.D. and C.R, Wetzel (2013) Stock synthesis: A biological 
#'     and statistical framework for fish stock assessment and fishery management. 
#'     Supplementary material, Appendix A. Equs A1.30 onwards. 
#'     \emph{Fisheries Research} 142:86-99.
#'     
#'     Hurtado-Ferro, F., Punt, A.E., and K.T. Hill (2014) Use of multiple 
#'     selectivity patterns as a proxy for spatial structure. 
#'     \emph{Fisheries Research} 158:102-115.
#'
#' @examples
#'   L <- seq(1,60,1)
#'   p <- c(25,32,16,33,-5,-2)
#'   sel <- domed(p,L)
#'   plot(L,sel,type="l",xlab="Age",ylab="Selectivity",lwd=2)
domed <- function(p, L) { # p=p; L = 6
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

#' @title domeSS3 calculates domed selectivity curve, pattern24 for size in SS33
#' 
#' @description domeSS3 uses 6 parameters and a set of mean size or age classes 
#'     to calculate a domed selectivity curve with a maximum of 1.0 (rescaling 
#'     can be done outside the function), but has parameters for the selectivity 
#'     of the initial and final size/age classes. There is an ascending limb and 
#'     a descending limb with the potential of a plateau in between. The 
#'     description in the SS3 technical description, p 9-10 Methot and Wetzel
#'     (2013, supplementary material = Appendix A) is somewhat confusing, there
#'     is a peak2 but no peak1. the sequence of calculation needs to be peak2,
#'     J1 (join1), J2, t1min, t2min, asc, dsc, and then the domed selectivity.
#'     It is confusing because some of the parameters require highly modified 
#'     inverse logit transformation and some don't, so their values are not 
#'     intuitively obvious, and their descriptions in the User Manual are 
#'     misleading parameters 3 and 4 for example are not natural logs.
#'     The six parameters are:
#'     
#'     1. the age/size where selectivity first becomes 1.0, p[1], a positive
#'     value within the range of bin values
#'     
#'     2. a modified inverse logit of the width of the plateau, typically 
#'     -5 : 10, it is used to estimate the size/age where selectivity first 
#'     begins to decline = peak2
#'     
#'     3. the steepness of the ascending limb, a modified inverse logit, used 
#'     when calculating t1min and asc
#'     
#'     4. the steepness of the descending limb, a modified inverse logit, used 
#'     when calculating t2min and dsc
#'     
#'     5. the selectivity of the first age/size class, a modified inverse logit, 
#'     used when calculating asc
#'     
#'     6. the selectivity of the last age/size class,  a modified inverse logit, 
#'     used when calculating dsc 
#'     
#'     The descending limb of any dome shaped selectivity curves imply that the 
#'     fishing gear used is unable to collect all representatives of the larger 
#'     or older classes. The predicted numbers of smaller or younger animals, 
#'     that are only partially selected, are inflated because of the partial 
#'     selection. If any larger or older animals are, in fact, caught, then the 
#'     same inflation can happen to those animals as a result of the partial 
#'     selection implied by the dome shape. Small and young animals weigh 
#'     very little, the same cannot be said for the larger or older animals. 
#'     Some people refer to the extra biomass this phenomenon can imply as 
#'     'ghost biomass', even though it might be real. Whatever the case, when 
#'     using dome shaped selectivity it is best to be aware of this issue and 
#'     to be cautious about how this is interpreted. The 20* terms in the J1 
#'     and J2 factors are required to force the joins to be as effective as
#'     required (see Methot and Wetzel 2013). 
#'
#' @param p a vector of six parameters.
#' @param bins a vector of the mean of nb age/size classes
#'
#' @return a vector of selectivities
#' @export
#' 
#' @references Methot, R.D. and C.R, Wetzel (2013) Stock synthesis: A biological 
#'     and statistical framework for fish stock assessment and fishery management. 
#'     Supplementary material, Appendix A. Equs A1.30 - A1.34. 
#'     \emph{Fisheries Research} 142:86-99.
#'     
#'     Hurtado-Ferro, F., Punt, A.E., and K.T. Hill (2014) Use of multiple 
#'     selectivity patterns as a proxy for spatial structure. 
#'     \emph{Fisheries Research} 158:102-115.
#'
#' @examples
#' # from selex_length_example https://github.com/nmfs-ost/ss3-user-examples
#'   p <- c(45.8546,-3.18064,5.308,1.699,-999,0.75363)
#'   bins <- seq(11,99,2)
#'   sel <- domeSS3(p,bins)
#'   plot(bins,sel,type="l",xlab="Age",ylab="Selectivity",lwd=2)
domeSS3 <- function(p,bins) {
  nb <- length(bins)
  lw <- bins[2] - bins[1]
  peak2 <- p[1] + lw + (0.99 * bins[nb] - p[1] - lw)/(1+exp(-p[2]))
  J1 <- 1/(1 + exp(-20*(bins - p[1])/(1 + abs(bins - p[1]))))
  J2 <- 1/(1 + exp(-20*(bins - peak2)/(1 + abs(bins - peak2))))
  tlmin <- exp((-(bins[1] - p[1])^2)/exp(p[3]))
  t2min <- exp((-(bins[nb] - peak2)^2)/exp(p[4]))
  asc <- (1/(1 + exp(-p[5]))) + (1 - (1/(1 + exp(-p[5])))) * 
    ((exp((-(bins - p[1])^2)/exp(p[3])) - tlmin)/(1 - tlmin))
  dsc <- 1 + ((1/(1 + exp(-p[6]))) - 1) * 
    (exp((-(bins - peak2)^2)/exp(p[4])) - 1)/(t2min - 1)
  sel <- asc * (1 - J1) + J1*((1 - J2) + J2 * dsc)
  return(sel)
} # end of domess3


#' @title endpart extracts the last part of an R path listing
#' 
#' @description endpart takes a path listing and extracts the final part. This
#'     is used to find the subdirectory name into which an analysis is to be
#'     placed, or could be used to find only the filename of a file at the end
#'     of a long path. endpart automatically finds whether the user has '/'
#'     or '\\\\' as the section separator.
#'
#' @param x a character vector of the path to a subdirectory or filename
#'
#' @returns the final character string - either a subdirectory or filename
#' @export
#'
#' @examples
#' first <- "C:/Users/public/Public Documents/first/"
#' second <- "C:\\Users\\public\\Public Documents\\second"
#' print(first)
#' print(second)
#' endpart(first)
#' endpart(second)
endpart <- function(x) {
  tmp <- unlist(strsplit(x, ""))
  use1 <- length(which(tmp == "/"))
  if (use1 > 0) {
    last <- tail(unlist(strsplit(x,split="/")),1)
  } else {
    last <- tail(unlist(strsplit(x,split="\\\\")),1)
  }
  return(last)
} # end of endpart

#' @title fillvector expands a vector of values for classes across known classes
#' 
#' @description fillvector given a vector of values for a series of, possibly
#'     incomplete, classes, which could be size or age classes, and places the 
#'     known values into the correct bins of the complete set of classes. Thus,
#'     with ages 0:6 given a vector of numbers-at-age of 1=12,2=24,3=6, 4= 5,
#'     fillvector would generate c(0,12,24,6,5,0,0)
#'     
#' @param x a vector of values each with the name of its respective class
#' @param vals a vector of class values to check against the names of the x
#'     vector. 
#'
#' @return a vector of length length(vals), containing the x values in the 
#'     correct cells
#' @export
#'
#' @examples
#' x <- c(12,24,6,1)
#' names(x) <- c("1","2","3","5")
#' ageclasses <- 0:6
#' fillvector(x=x,vals=ageclasses)
fillvector <- function(x,vals) { # x=fems; vals = sizes
  if (length(x) == length(vals)) {
    return(x)
  } else {
    out <- numeric(length(vals))
    names(out) <- vals
    whichpos <- match(as.numeric(names(x)),vals)
    out[whichpos] <- x
    return(out)
  }
} # end of fillvector

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
#' # typical syntax  fixstarter(calc,findtext="use init value")
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

#' @title getalcomp extracts any conditional age-at-length data from ss.dat
#' 
#' @description getalcomp extracts any conditional age-at-length data directly 
#'     from the SS3 dat file. It produces an array of length-bin x age-bin X
#'     year ready for eahc to be plotted. 
#'
#' @param store this is the full path to the directory holding all the scenario 
#'     sub-directories.
#' @param analysis is a character string identifying the particular scenario
#'     being explored. The dat file to be examined is held as 'analysis.dat'.
#'
#' @returns  two arrays of the female and make age-len key data as arrays of
#'     length-bins x age-bins x years by gender plus the fleet names with 
#'     conditional age-at-length data. If no such data then NULL
#' @export
#'
#' @examples
#' \dontrun{
#'   store <-  "c:/Users/userA/SS3/EG/afish/"
#'   analysis <- "sp_area_1"
#'   # inside sp_area_1 there needs to be a sp_area_1.dat file
#'   allcomp <- getalcomp(store-store,analysis=analysis)
#' }
getalcomp <- function(store,analysis) {
  destination <- pathtopath(store,analysis)
  datfile <- pathtopath(destination,paste0(analysis,".dat"))
  dat <- SS_readdat_3.30(file=datfile,verbose = FALSE,section = NULL)
  agebins <- dat$agebin_vector
  nages <- length(agebins)
  agecomp <- dat$agecomp
  pickpos <- which(agecomp$Lbin_lo > 0)
  if (length(pickpos) > 0) {
    compage <- agecomp[pickpos,]
    sexes <- sort(unique(compage$sex))
    nsex <- length(sexes)    
    activefleets <- sort(unique(compage$fleet))
    activefleets <- activefleets[activefleets > 0]
    fleets <- dat$fleetnames[activefleets]
    pickactive <- which(compage$fleet > 0)
    compage <- compage[pickactive,]
    yrs <- sort(unique(compage[,"year"]))
    nyr <- length(yrs)
    startcol <- which(colnames(compage) == "Nsamp")
    othercols <- which(colnames(compage) %in% c("year","Lbin_lo","fleet","sex"))
    if (nsex > 1) {
      pickfem <- which(compage$sex == 1)
      pickF <- grep("f",colnames(compage))
      if (length(pickF) == 0) {
        pickF <- grep("a",colnames(compage))
      }
      pickF <- pickF[which(pickF > startcol)]
      compfem <- compage[pickfem,c(othercols,pickF)]
      pickmal <- which(compage$sex == 2)
      pickM <- grep("m",colnames(compage))
      pickM <- pickM[which(pickM > startcol)]
      compmal <- compage[pickmal,c(othercols,pickM)]
    } else {
      pickF <- grep("f",colnames(compage))
      pickF <- pickF[which(pickF > startcol)]     
      if (length(pickF) == 0) {
        pickF <- grep("a",colnames(compage))
      }
      pickF <- pickF[which(pickF > startcol)]
      compfem <- compage[,c(othercols,pickF)]
      pickM <- grep("m",colnames(compage))
      pickM <- pickM[which(pickM > startcol)]  
      if ((nsex == 1) & (sexes == 0)) {
        compmal <- NULL 
      } else {
        compmal <- compage[,c(othercols,pickM)]
      }
    }
    invisible(list(compfem=compfem,compmal=compmal,fleets=fleets))
  } else {
    print("No conditional Age-at-Length data found.")
    invisible(NULL)
  }
} # end of getalcomp

#' @title getagelenkeys extracts data from SS3 data file about age composition
#' 
#' @description getagelenkeys is used to extract an sumry of all the age-
#'     compositon data and conditional age-at-length data from an SS3 data file.
#'     The inputdata are best generated using the SS_readdat_3.30. This function
#'     has not been tested on versions of SS < 3.30. 
#'
#' @param inputdata the output of SS_readdat_3.30.
#' @param verbose default=TRUE, should messages be sent to the console.
#'
#' @returns A list of all agelenkeys in the data set relating to gender, year, 
#'     and fleet, The number of scenarios, years, and fleets.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(rforSS3)
#'   library(r4ss)
#'   library(codeutils)
#'   library(hplot)
#'   ddir <- getDBdir()
#'   wdir <- pathtopath(ddir,"/A_CodeR/SS3run/")
#'   store <- pathtopath(wdir,"egfish/")
#'   analysis <- "BaseCase"
#'   destination <- pathtopath(store,analysis)
#'   datfile <- pathtopath(destination,paste0(analysis,".dat"))
#'   dat <- SS_readdat_3.30(file=datfile,verbose = TRUE,
#'                          echoall = lifecycle::deprecated(),section = NULL)
#'   outscene <- getagelenkeys(dat)
#'   str(outscene,max.level=1)
#'   str(outscene$allscene,max.level=1)
#' }
getagelenkeys <- function(inputdata,verbose=TRUE) {
  #  inputdata=dat; verbose=TRUE
  agecp <- inputdata$agecomp
  colnames(agecp) <- tolower(colnames(agecp))
  agecols <- colnames(agecp)
  picklen <- which(agecols %in% c("lbin_lo"))
  pickpos <- which(agecp[,picklen] > 0)
  if (length(pickpos) > 0) {  # are there any cond AaL data?  
    pickfleet <- which(agecols %in% c("fleet","FltSvy"))  
    usecolnameflt <- agecols[pickfleet]  
    pickpos <- which(agecp[,usecolnameflt] > 0)
    if (length(pickpos) == 0) { # is available cond AaL data being used?
      stop("Available conditional age-at-length data not being used.")
    } else {
      picksex <- which(agecols %in% c("sex","gender"))  
      usecolnamesex <- agecols[picksex]  
      pickyr <- which(agecols %in% c("Yr","year"))
      usecolnameyr <- agecols[pickyr]
      pickpos <- which((agecp[,picklen] > 0) & (agecp[,pickfleet] > 0) &
                         (agecp[,pickyr] > 0)) 
      agecp <- agecp[pickpos,] # only keep +ve cond AaL data 
      binwidth <- inputdata$binwidth
      lenvector <- seq(min(agecp[,"lbin_lo"]),max(agecp[,"lbin_hi"],binwidth))
      agevector <- inputdata$agebin_vector
      nages <- length(agevector)
      fleets <- sort(unique(agecp[,pickfleet]))
      fleetnames <- inputdata$fleetnames[fleets]
      nfleet <- length(fleets)
      years <- sort(unique(agecp[,pickyr]))
      nyear <- length(years)
      sexes <- sort(unique(agecp[,picksex]))
      gender <- ifelse(sexes > 0,"twosex","combined")
      if (gender == "twosex") sexes <- c(1,2)
      nsex <- ifelse(sexes > 0,2,1)
      initcols <- c(usecolnameyr,usecolnameflt,usecolnamesex,
                    "lbin_lo","lbin_hi","nsamp")
      leninit <- length(initcols)
      startcounts <- which(agecols == "nsamp") + 1
      numcol <- ncol(agecp)
      if (gender == "combined") {  # read all columns of just the f columns
        pickcols <- startcounts:(startcounts + nages - 1)
      } else {
        pickcols <- startcounts : numcol
      }
      columns <- c(initcols,agecols[pickcols])
      agecomp <- agecp[,columns]
      scenes <- expand.grid(sex=sexes,years=years,fleets=fleets)
      scenames <- expand.grid(sex=gender,years=years,fleets=fleetnames)
      nscene <- nrow(scenes)
      sampleN <- numeric(nscene)
      scenename <- NULL
      for (i in 1:nscene) 
        scenename <- c(scenename,paste0(scenames[i,1],"_",scenames[i,2],
                                        "_",scenames[i,3]))
      allscenes <- makelist(scenename)
      for (i in 1:nscene) {  # i = 1
        pickrecs <- which((agecomp[,usecolnameyr] == scenes[i,"years"]) & 
                            (agecomp[,usecolnameflt] == scenes[i,"fleets"]) &
                            (agecomp[,usecolnamesex] == scenes[i,"sex"]) & 
                            (agecomp$lbin_lo > 0))        
        agekey <- agecomp[pickrecs,]
        sampleN[i] <- sum(agekey[,"nsamp"],na.rm=TRUE)
        allscenes[[i]] <- agekey[order(agekey$lbin_lo),]        
      }
      names(sampleN) <- scenename
      ageerror <- inputdata$ageerror
    }  # cond AaL data being used   
  } else {
    if (verbose) 
      warning(cat("No Conditional age-at-length data, age structure plotted."))
    return(NULL)
  }
  return(invisible(list(allscenes=allscenes,nscene=nscene,nyear=nyear,
                        years=years,nfleet=nfleet,fleetnames=fleetnames,
                        gender=gender,sexes=sexes,ageerror=ageerror,
                        sampleN=sampleN,scenenames=scenename,nages=nages,
                        lenvector=lenvector,agevector=agevector)))
} # end of getagelenkeys

#' @title getaggprops calculates the proportions at age aggregated across years
#'
#' @param plotreport the object output from SS_output
#'
#' @returns a list of the proportional distribution of ages in each year, the
#'     expected proportions of ages in each year, and the aggregated proportions
#'     of ages across years
#' @export
#'
#' @examples
#' print("Wait n example data")
getageprops <- function(plotreport) {
  agedbase <- plotreport$agedbase
  fleets <- unique(agedbase[,"Fleet"])
  nfleet <- length(fleets)
  fleetnames <- plotreport$FleetNames[fleets]
  sex <- unique(agedbase[,"Sex"])
  nsex <- length(sex)
  if (nsex > 1) sexes <- c("female","male") else sexes <- "mixed"
  ages <- unique(agedbase[,"Bin"])
  nages <- length(ages)
  yrs <- unique(agedbase[,"Yr"])
  nyr <- length(yrs)
  propyr <- array(0,dim=c(nyr,nages,nsex,nfleet),
                  dimnames=list(yrs,ages,sexes,fleetnames))
  exppropyr <- propyr
  for (yr in 1:nyr) {   # yr = 6; sx = 1; fl =1
    for (sx in 1:nsex) {
      for (fl in 1:nfleet) {
        pick <- which((agedbase[,"Yr"] == yrs[yr]) & 
                        (agedbase[,"Sex"] == sex[sx]) & 
                        (agedbase[,"Fleet"] == fleets[fl]))
        if (length(pick) == 0) {
          propyr[yr,,sx,fl] <- rep(0,nages)
          exppropyr[yr,,sx,fl] <- rep(0,nages)   
        } else {
          yrdat <- agedbase[pick,]
          if (length(pick) < nages) {
            pick <- match(yrdat[,"Bin"],ages) 
            Exp <- obs <- rep(0,nages)
            obs[pick] <- yrdat[,"Obs"]
            Exp[pick] <- yrdat[,"Exp"]
          } else {
            obs <- yrdat[,"Obs"]
            Exp <- yrdat[,"Exp"]
          }
          propyr[yr,,sx,fl] <- obs
          exppropyr[yr,,sx,fl] <- Exp
        }
      }
    }
  }
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
    ),FUN = sum
  )
  agg[["obs"]] <- agg[["obs"]] / agg[["Nsamp_adj"]]
  agg[["exp"]] <- agg[["exp"]] / agg[["Nsamp_adj"]]
  colnames(agg) <- c("Age","Fleet","Sex","Nsamp_adj","effN","Obs","Exp")
  # agg
  return(list(propyr=propyr,exppropyr=exppropyr,agg=agg))
} # end of getageprops

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

#' @title getdatasets tabulates the data in a printer friendly fashion
#' 
#' @description getdatasets tabulates the data in a printer friendly fashion
#'     so that this can be presented in appendices to enhance transparency.
#'     Currently, it returns only the catches but this will be expanded in 
#'     later versions.
#'
#' @param plotreport the output of SS_output     
#' @param store the path to the directory within which the analysis is held
#' @param analysis the name of the SS3 model being explored
#' @param verbose Should warnings and progress be reported to the console?
#'     default = TRUE.
#'     
#' @seealso{
#'   \link{SS_readdat_3.30}, \link{SS_output}
#' }
#'
#' @returns invisibly returns the catches by fleet in a matrix
#' @export
#'
#' @examples
#' # typical syntax
#' # getdatasets(plotreport=plotreport,store=store,analysis=analysis,
#' #             verbose=FALSE)
getdatasets <- function(plotreport,store,analysis,verbose=TRUE) {
  destination <- pathtopath(store,analysis)
  datfile <- pathtopath(destination,paste0(analysis,".dat"))
  indat <- SS_readdat_3.30(file=datfile,verbose=verbose)
  catch <- indat$catch
  pickpos <- which(catch[,"year"] > 0)
  catch <- catch[pickpos,]
  yrs <- sort(unique(catch[,"year"]))
  nyr <- length(yrs)
  fleetnames <- plotreport$FleetNames
  nfleet <- length(fleetnames)
  catches <- matrix(0,nrow=nyr,ncol=nfleet,dimnames=list(yrs,fleetnames))
  for (i in 1:nfleet) { # i = 1
    pickfleet <- which(catch[,"fleet"] == i)
    if (length(pickfleet) > 0) {
      flcatch <- catch[pickfleet,]
      pickrow <- match(flcatch[,"year"],yrs)
      catches[pickrow,i] <- flcatch[,"catch"]
    }
  }
  return(invisible(catches))
} # end of getdatasets

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



#' @title getprops gets proportions of agecomp or lencomp aggregated across years
#'
#' @param compbase an object from SS_output output object, either 
#'     plotreport$agedbase or plotreport$lendbase
#' @param fleetnames the vector of fleetnames, usually plotreport$FlkeetNames
#' @param comptype default = 'Len' but could be 'Age', describes the typw of 
#'     data being extracted as length cor age omposition.
#'
#' @returns a list of the proportional distribution of agecomps or lencomps in 
#'     each year, the related expected proportions in each year, and the 
#'     aggregated proportions across years
#' @export
#'
#' @examples
#' print("Wait n example data")
getprops <- function(compbase,fleetnames,comptype="Len") { # compbase=plotreport$lendbase
  fleets <- unique(compbase[,"Fleet"])
  nfleet <- length(fleets)
  fleetnames <- fleetnames[fleets]
  sex <- unique(compbase[,"Sex"])
  nsex <- length(sex)
  if (nsex > 1) sexes <- c("female","male") else sexes <- "mixed"
  ages <- unique(compbase[,"Bin"])
  nages <- length(ages)
  yrs <- unique(compbase[,"Yr"])
  nyr <- length(yrs)
  propyr <- array(0,dim=c(nyr,nages,nsex,nfleet),
                  dimnames=list(yrs,ages,sexes,fleetnames))
  exppropyr <- propyr
  for (yr in 1:nyr) {   # yr = 6; sx = 1; fl =1
    for (sx in 1:nsex) {
      for (fl in 1:nfleet) {
        pick <- which((compbase[,"Yr"] == yrs[yr]) & 
                        (compbase[,"Sex"] == sex[sx]) & 
                        (compbase[,"Fleet"] == fleets[fl]))
        if (length(pick) == 0) {
          propyr[yr,,sx,fl] <- rep(0,nages)
          exppropyr[yr,,sx,fl] <- rep(0,nages)   
        } else {
          yrdat <- compbase[pick,]
          if (length(pick) < nages) {
            pick <- match(yrdat[,"Bin"],ages) 
            Exp <- obs <- rep(0,nages)
            obs[pick] <- yrdat[,"Obs"]
            Exp[pick] <- yrdat[,"Exp"]
          } else {
            obs <- yrdat[,"Obs"]
            Exp <- yrdat[,"Exp"]
          }
          propyr[yr,,sx,fl] <- obs
          exppropyr[yr,,sx,fl] <- Exp
        }
      }
    }
  }
  Bins <- sort(unique(compbase[["Bin"]]))
  nbins <- length(Bins)
  df <- data.frame(
    Nsamp_adj = compbase[["Nsamp_adj"]],
    effN = compbase[["effN"]],
    obs = compbase[["Obs"]] * compbase[["Nsamp_adj"]],
    exp = compbase[["Exp"]] * compbase[["Nsamp_adj"]]
  )
  agg <- aggregate(
    x = df,
    by = list(
      bin = compbase[["Bin"]], f = compbase[["Fleet"]],
      sex = compbase[["Sex"]]
    ),FUN = sum
  )
  agg[["obs"]] <- agg[["obs"]] / agg[["Nsamp_adj"]]
  agg[["exp"]] <- agg[["exp"]] / agg[["Nsamp_adj"]]
  colnames(agg) <- c(comptype,"Fleet","Sex","Nsamp_adj","effN","Obs","Exp")
  # agg
  return(list(propyr=propyr,exppropyr=exppropyr,agg=agg))
} # end of getprops

#' @title get_tuning_table copied completely from r4ss currently not exported
#' 
#' @description get_tuning_table is a function copied directly from r4ss. For
#'     some reason, it is not exported from r4ss! So to use it in a comparison
#'     one needs to add r4ss::: at its call, which upsets the building 
#'     process when compiling a package. This is a temporary fix and a request
#'     will be made to the r4ss team to export the function (it seems like an
#'     oversight).
#'
#' @param replist the output from SS_output
#' @param fleets which fleets to extract tuning variance information
#' @param option which tuning method to use, default = 'Francis', alternatives
#'     include 'none', 'Francis', 'MI', or 'DI'  
#' @param digits number of digits to round numbers to, default = 6
#' @param write Write suggested tunings to a file 'suggested_tunings.ss',
#'     default = FALSE
#' @param verbose should comments and output be written to the console, 
#'     default = TRUE
#'
#' @returns the tuning table
#' @export
#'
#' @examples
#' print("no example available")
get_tuning_table <- function (replist, fleets, option, digits = 6, write = FALSE, 
                              verbose = TRUE) {
  tuning_table <- data.frame(factor = integer(), fleet = integer(), 
                             Var_adj = double(), Hash = character(), 
                             Old_Var_adj = double(), 
                             New_Francis = double(), New_MI = double(), 
                             Francis_mult = double(), Francis_lo = double(),  
                             Francis_hi = double(), MI_mult = double(), 
                             Type = character(), Name = character(), 
                             Note = character(), stringsAsFactors = FALSE)
  for (type in c("len", "age", "size")) {
    for (fleet in fleets) {
      if (verbose) {
        message("calculating ", type, " tunings for fleet ", 
                fleet)
      }
      if (type == "len") {
        tunetable <- replist[["Length_Comp_Fit_Summary"]]
        factor <- 4
        has_marginal <- fleet %in% replist[["lendbase"]][["Fleet"]]
        has_conditional <- FALSE
      }
      if (type == "age") {
        tunetable <- replist[["Age_Comp_Fit_Summary"]]
        factor <- 5
        has_marginal <- fleet %in% replist[["agedbase"]][["Fleet"]]
        has_conditional <- fleet %in% replist[["condbase"]][["Fleet"]]
      }
      if (type == "size") {
        tunetable <- replist[["Size_Comp_Fit_Summary"]]
        factor <- 7
        has_marginal <- fleet %in% replist[["sizedbase"]][["Fleet"]]
        has_conditional <- FALSE
      }
      if (has_marginal & has_conditional) {
        warning("fleet", fleet, "has both conditional ages and marginal ages", 
                "\ntuning will be based on conditional ages")
      }
      if (has_marginal | has_conditional) {
        Francis_mult <- NULL
        Francis_lo <- NULL
        Francis_hi <- NULL
        Francis_output <- SSMethod.TA1.8(fit = replist, 
                                         type = type, fleet = fleet, plotit = FALSE, 
                                         printit = FALSE)
        if (has_conditional) {
          Francis_output <- SSMethod.Cond.TA1.8(fit = replist, 
                                                fleet = fleet, plotit = FALSE, printit = FALSE)
        }
        Francis_mult <- Francis_output[1]
        Francis_lo <- Francis_output[2]
        Francis_hi <- Francis_output[3]
        Note <- ""
        if (is.null(Francis_output)) {
          Francis_mult <- NA
          Francis_lo <- NA
          Francis_hi <- NA
          Note <- "No Francis weight"
        }
        Curr_Var_Adj <- NA
        if ("Curr_Var_Adj" %in% names(tunetable)) {
          Curr_Var_Adj <- tunetable[["Curr_Var_Adj"]][tunetable[["Fleet"]] == 
                                                        fleet]
        }
        if ("Var_Adj" %in% names(tunetable)) {
          Curr_Var_Adj <- tunetable[["Var_Adj"]][tunetable[["Fleet"]] == 
                                                   fleet]
        }
        if (is.na(Curr_Var_Adj)) {
          stop("Model output missing required values, perhaps due to an older version of SS3")
        }
        MI_mult <- NA
        if ("HarMean(effN)/mean(inputN*Adj)" %in% names(tunetable)) {
          MI_mult <- tunetable$"HarMean(effN)/mean(inputN*Adj)"[tunetable[["Fleet"]] == 
                                                                  fleet]
        }
        if ("MeaneffN/MeaninputN" %in% names(tunetable)) {
          MI_mult <- tunetable$"MeaneffN/MeaninputN"[tunetable[["Fleet"]] == 
                                                       fleet]
        }
        if ("factor" %in% names(tunetable)) {
          MI_mult <- tunetable[["Recommend_var_adj"]][tunetable[["Fleet"]] == 
                                                        fleet]/tunetable[["Curr_Var_Adj"]][tunetable[["Fleet"]] == 
                                                                                             fleet]
        }
        if (all(c("HarMean", "mean_Nsamp_adj") %in% names(tunetable))) {
          MI_mult <- tunetable[["HarMean"]][tunetable[["Fleet"]] == 
                                              fleet]/tunetable[["mean_Nsamp_adj"]][tunetable[["Fleet"]] == 
                                                                                     fleet]
        }
        if (is.na(MI_mult)) {
          stop("Model output missing required values, perhaps due to an older version of SS3")
        }
        newrow <- data.frame(factor = factor, fleet = fleet, 
                             New_Var_adj = NA, hash = "#", Old_Var_adj = round(Curr_Var_Adj, 
                                                                               digits), New_Francis = round(Curr_Var_Adj * 
                                                                                                              Francis_mult, digits), New_MI = round(Curr_Var_Adj * 
                                                                                                                                                      MI_mult, digits), Francis_mult = round(Francis_mult, 
                                                                                                                                                                                             digits), Francis_lo = round(Francis_lo, digits), 
                             Francis_hi = round(Francis_hi, digits), MI_mult = round(MI_mult, 
                                                                                     digits), Type = type, Name = replist[["FleetNames"]][fleet], 
                             Note = Note, stringsAsFactors = FALSE)
        tuning_table <- rbind(tuning_table, newrow)
      }
    }
  }
  if (option == "none") {
    tuning_table[["New_Var_adj"]] <- tuning_table[["Old_Var_adj"]]
  }
  if (option == "Francis") {
    tuning_table[["New_Var_adj"]] <- tuning_table[["New_Francis"]]
    NAvals <- is.na(tuning_table[["New_Var_adj"]])
    tuning_table[["New_Var_adj"]][NAvals] <- tuning_table[["New_MI"]][NAvals]
    tuning_table[["Note"]][NAvals] <- paste0(tuning_table[["Note"]][NAvals], 
                                             "--using MI value")
  }
  if (option == "MI") {
    tuning_table[["New_Var_adj"]] <- tuning_table[["New_MI"]]
  }
  names(tuning_table)[1] <- "#factor"
  rownames(tuning_table) <- 1:nrow(tuning_table)
  tunetable_size <- replist[["Size_Comp_Fit_Summary"]]
  if (!is.null(tunetable_size) && "par1" %in% names(tunetable_size)) {
    warning("This function may not work for generalized size composition data ", 
            "in models prior to SS3 version 3.30.20.")
  }
  if (write) {
    file <- file.path(replist[["inputs"]][["dir"]], "suggested_tuning.ss")
    if (verbose) {
      message("writing to file ", file)
    }
    write.table(tuning_table, file = file, quote = FALSE, 
                row.names = FALSE)
  }
  return(tuning_table)
} # end of get_tuning_table


#' @title headtail presents the head and tail of a matrix, data.frame, or tibble
#' 
#' @description headtail is merely a utility function that displays both the
#'     head and tail of a matrix or data.frame. It will translate a tibble into 
#'     a data.frame but any other object will return its structure using the 
#'     str(x, max.level=1) function. The head in the output is separated from 
#'     its tail by two lines of dots.
#'
#' @param x a matrix or data.frame 
#' @param n how many rows from the first and last to be displayed, default=5
#' @param verbose should reports be made to the console
#'
#' @return a data.frame of 2 x n + 2 rows containing the head and tail of x
#' @export
#'
#' @examples
#' dat <- matrix(rnorm(100,mean=10,sd=1),nrow=25,ncol=4,dimnames=list(1:25,1:4))
#' headtail(dat,5)
#' dat <- list(dat=dat,numrow=nrow(dat),numcol=ncol(dat))
#' headtail(dat,5)
headtail <- function(x,n=5,verbose=TRUE) { # x = fltcatch;n =5
  if (inherits(x,"tbl")) {
    x <- as.data.frame(unclass(x), stringsAsFactors = FALSE,
                       check.names=FALSE)
    if (verbose) cat("Original object was a tibble \n") 
  }  
  if ((inherits(x,c("matrix","data.frame"))) & (length(dim(x) == 2))) {
    numcol <- ncol(x)
    rnames <- rownames(x)
    if (is.null(rnames)) rnames <- 1:numrow
    out <- as.data.frame(rbind(head(x,n),rep(".",numcol),rep(".",numcol),tail(x,n))) 
    numrow <- nrow(x)
    rownames(out) <- c(rnames[1:n],"a","b",rnames[(numrow-n+1):numrow])
  } else {
    out <- str(x,max.level = 1)
  }
  return(out)
} # end of headtail

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

#' @title plotagecomp literally plots the conditional age-at-length data 
#' 
#' @description plotagecomp generates a plot of the conditional age-at-length
#'     data where each data point is jittered so that a direct impression of 
#'     the density of observations is presented. More than 20 plots in a single
#'     panel would get too busy, it is up to the user to control the flow of 
#'     the number of years of data sent to the function.
#'
#' @param compsex the array of lengthbin x agebins x year for a given gender
#' @param gender a character string denoting the gender being illustrated
#' @param fleet which fleet is the data representing, a character string
#' @param rescale if the default value fails to provide a statisfactory plot
#'     thenm enter a value here to obtain what's needed. The default = 0, in
#'     which case the number of age-bins is divided by 18, which usally works.
#' @param console should the plot go to the console or to a file. default=TRUE
#' @param plotdir the full path to the plotting directory if a file is to be 
#'     saved.
#' @param plotout should a plot be produced or just output the alcomp? defualt
#'     = TRUE
#'     
#' @seealso{
#'     \link{plotagelen}, \link{getalcomp}
#' }
#'
#' @returns invisibly a list of the filename used and a copy of the 3D array
#'    of age-length-year developed.
#' @export
#'
#' @examples
#' print("wait on example data - wait a while!")
#' # compsex=compfem[pickflt,]; gender="Female"; fleet=fleets[1]; rescale=0.0;
#' # console=TRUE;plotdir="";plotout=TRUE 
plotagecomp <- function(compsex,gender,fleet,rescale=0,console=TRUE,
                        plotdir="",plotout=TRUE) {
  tmp <- colnames(compsex)
  pickcut <- which(tmp == "Lbin_lo")
  tmp <- tmp[-(1:pickcut)]
  whatchar <- substr(tmp[1],1,1)
  tmp <- gsub(whatchar,"",tmp)
  agebins <- as.numeric(tmp)
  nages <- length(agebins)  
  if (rescale == 0) rescale <- nages / 18
  yrs <- sort(unique(compsex[,"year"]))
  nyr <- length(yrs)
  lenbins <- sort(unique(compsex$Lbin_lo))
  nlbins <- length(lenbins)
  alcomp <- array(0,dim=c(nlbins,nages,nyr),
                  dimnames=list(lenbins,agebins,yrs))
  for (yr in 1:nyr) { # yr = 1
    pickyr <- which(compsex[,"year"] == yrs[yr])
    tmp <- compsex[pickyr,]
    numcol <- ncol(tmp)
    if (nrow(tmp)  < nlbins) {
      lens <- match(tmp[,"Lbin_lo"],lenbins)
    } else {
      lens <- 1:nlbins
    }
    for (i in 5:numcol) alcomp[lens,(i-4),yr] <- tmp[,i]
  }
  filen=""
  if (plotout) {
    radcomp <- sqrt(alcomp)
    scale <- max(radcomp,na.rm=TRUE)
    radcomp <- radcomp/scale
    xvect <- rep(agebins,nlbins)
    yvect <- rep(lenbins,nages)
    if (!console) {
      fileout <- paste0("Scaled_AgeCompdata_",fleet,"_",gender,".png")
      filen <- pathtopath(plotdir,fileout)
    }
    plotprep(width=10, height=10,newdev=TRUE,filename=filen,verbose=FALSE)
    parset(plots=pickbound(nyr),outmargin=c(1,1.3,0.1,0.1),
           margin=c(0.3,0.35,0.05,0.05),byrow=FALSE)
    for (i in 1:nyr) { # i = 1
      agelen <- radcomp[,,i]  
      agelen[agelen == 0] <- NA
      nobs <- sum(alcomp[,,i],na.rm=TRUE)
      label <- paste0(yrs[i],"_",nobs)
      maxy <- getmax(yvect)
      plot(xvect,yvect,type="n",xlab="",ylab=label,ylim=c(0,maxy),
           panel.first=grid())
      for (age in 1:nages) {
        if (sum(agelen[,age],na.rm=TRUE) > 0) {
          symbols(rep((age-1),nlbins),lenbins,circles=(agelen[,age]*rescale),
                  inches=FALSE,bg=rgb(1, 0, 0, 0.5), fg = "grey",
                  xlab="",ylab="",add=TRUE)
        }
      }
    }
    label <- paste0("Age ",gender," ",fleet)
    mtext(label,side=1,line=-0.5,outer=TRUE,cex=1.25,font=7)
    mtext("Length",side=2,line=0,outer=TRUE,cex=1.25,font=7)
  }
  invisible(list(filen=filen,alcomp=alcomp))
} # end of plotagecomp

#' @title plotagelen plots a jittered version of conditional age-at-length data
#' 
#' @description plotagelen uses base::jitter to plot any conditional 
#'     age-at-length observed data for each year. r4ss has replaced the usual 
#'     jitter function hence the need for the namespace. 
#' 
#'
#' @param alcomp the output from plotagecomp an array of length x age x year
#' @param gender a character label for the plots, usually 'Male', 'Female', or
#'     'mixed'
#' @param fleet which fleet, again for a plot label, either a number of name
#' @param console should the plot go to the console or a file, default=TRUE
#' @param plotdir the full path to the directory in which to store the file, 
#'     default = ''
#' @param trendline if = 'linear' then it adds a linear model y on x to the 
#'     plot of length against age, or if 'vb' it uses an input set of vonB
#'     parameters as c(Linf, K, t0) to plot a vB curve to each year of data,
#'     or if NULL, the default, it adds nothing.
#' @param vbpar default = NULL or it is a vector of c(Linf, K, t0) then it uses
#'     thoe values to add a vB curve to each year of datas.
#'     
#' @seealso{
#'     \link{plotagecomp}, \link{getalcomp}
#' }
#' @returns nothing but it does generate a plot
#' @export
#'
#' @examples
#' print("Wait on data sets")
#' # alcomp=alfem;gender="Female";fleet=fleets[1];console=TRUE;plotdir="";vbpar=vb1
plotagelen <- function(alcomp,gender,fleet,console=TRUE,plotdir="",
                       trendline=FALSE, vbpar=NULL) {
  inmat <- alcomp[,,1]
  ages <- as.numeric(colnames(inmat))
  lght <- as.numeric(rownames(inmat))
  yrs <- as.numeric(dimnames(alcomp)[[3]])
  nyr <- length(yrs)
  filen=""
  if (!console) {
    fileout <- paste0("AgeLenKey_",fleet,"_",gender,".png")
    filen <- pathtopath(plotdir,fileout)
  }
  linmodout <- matrix(0,nrow=nyr,ncol=2,dimnames=list(yrs,c("inter","grad")))
  plotprep(width=10, height=10,newdev=TRUE,filename=filen,verbose=FALSE)
  parset(plots=pickbound(nyr),outmargin=c(1,1.3,0.1,0.1),
         margin=c(0.3,0.35,0.05,0.05),byrow=FALSE)
  for (yr in 1:nyr) {
    inmat <- alcomp[,,yr]
    tot <- colSums(inmat)
    pickcol <- which(tot > 0)
    xvect <- NULL
    yvect <- NULL
    for (i in 1:length(pickcol)) { # i = 1
      xvect <- c(xvect,rep(ages[pickcol[i]],tot[pickcol[i]]))
      yvect <- c(yvect,rep(lght,inmat[,pickcol[i]]))
    }
    linmod <- lm(yvect ~ xvect)
    nobs <- length(xvect)
    label <- paste0(yrs[yr],"_",nobs)
    plot(base::jitter(xvect),base::jitter(yvect),type="p",pch=1,cex=1,
         xlim=c(ages[1],ages[length(ages)]),
         ylim=c(0,lght[length(lght)]),xlab="",ylab=label,
         panel.first = grid())
    if (!is.null(trendline)) {
      trendline <- tolower(trendline)
      if (trendline == "linear") {
        abline(linmod,lwd=2,col=2)
        mod <- coefficients(linmod)
        label <- paste0(round(mod[1],3),"  ",round(mod[2],3))
        legend("bottomright",label,lwd=0,col=0,bty="n",cex=1.2) 
        linmodout[yr,] <- mod
      } else {
        if (trendline == "vb") {
          vbage <- seq(ages[1],ages[length(ages)],length=100)
          vblen <- vbpar[1] * (1 - exp(-vbpar[2] * (vbage - vbpar[3])))
          lines(vbage,vblen,lwd=2,col=2)
        }
      }
    }
  }
  tmpvb <- round(vbpar,4)
  legend("bottomright",c(paste0("Linf ",tmpvb[1]),paste0("K    ",tmpvb[2]),
                         paste0("t0   ",tmpvb[3])),col=0,lwd=0,bty="n",cex=1.0)
  label <- paste0("Age ",gender," ",fleet)
  mtext(label,side=1,line=-0.5,outer=TRUE,cex=1.25,font=7)
  mtext("Length",side=2,line=0,outer=TRUE,cex=1.25,font=7)
  if (trendline != "linear") linmodout <- NULL
  if (trendline == "vb") linmodout <- vbpar
  invisible(list(filen=filen,linmodout=linmodout))
} # end of plotagelen

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
#' @seealso{
#'   \link{getagelenkeys}
#' }
#'
#' @returns nothing but it does generate a set of plots.
#' @export
#'
#' @examples
#' # outcomp=outscene; console=TRUE;plotscenes=NULL
#' # pchcex=1.25;pchcol=2;pch=1; verbose=TRUE
plotagelenkey <- function(outcomp,rundir="",plotscenes=NULL,pch=1,pchcex=1.25,
                          pchcol=2,console=TRUE,verbose=TRUE) { 
  allscenes <- outcomp$allscenes
  scnames <- outcomp$scenenames
  nscene <- outcomp$nscene
  nages <- outcomp$nages
  usescenes <- allscenes
  usesampN <- outcomp$sampleN
  if (!is.null(plotscenes)) {
    nscene <- length(plotscenes)
    scnames <- outcomp$scenenames[plotscenes]    
    usescenes <- makelist(scnames)
    for (i in 1:nscene) {
      usescenes[[i]] <- allscenes[[plotscenes[i]]]
    } 
    usesampN <- usesampN[plotscenes]
  } else {
    plotscenes <- 1:nscene
    if ((nscene > 8) & (console)) {
      if (verbose) 
        warning(cat("Only the first 8 agelength keys will be plotted"))
      plotscenes <- 1:8
      nscene <- length(plotscenes)
      scnames <- scnames[plotscenes]    
      usescenes <- makelist(scnames)
      for (i in 1:nscene) {
        usescenes[[i]] <- allscenes[[plotscenes[i]]]
      } 
      usesampN <- usesampN[plotscenes]
    } 
  }
  mainlab <- paste0("scenes_",min(plotscenes),"-",max(plotscenes))
  agevector <- outcomp$agevector
  agelim <- range(agevector)
  sizelim <- range(outcomp$lenvector)
  filen <- ""
  if (!console) {
    fnam <- paste0("agelengthkeys_",mainlab,".png")
    filen <- pathtopath(rundir,fnam)
    caption <- paste0("Age-Length keys for ",paste0("_",scnames,collapse="_"))
  }
  plotprep(width=9,height=9,filename=filen)
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
  mtext(mainlab,side=3,outer=TRUE,cex=1.2,line=0)
  if (!console) {
    dev.off()
    addplot(filen=filen,rundir=rundir,category="AgeLenKeys",caption=caption)
    
  }
} # end of plotagelenkey

#' @title plotss3catches generates a plot of catch by fleet and of total catch
#' 
#' @description plotss3catches generates a two-panel plot with catches-by-fleet
#'     and total catches across all fleets, which also has a loess smoother 
#'     imposed to assist in identifying any trends in total catch. 
#'
#' @param catches an output from getdatasets
#' @param extradir the full path to the directory fof extra plots and tables
#' @param analysis the name of the analysis or scenario whose catches are being 
#'    plotted
#' @param console should the plot go to the console or be saved as a png file
#'     to the extradir
#'
#' @returns nothing but it does plot a graph
#' @export
#'
#' @examples
#' # typical syntax
#' # plotss3catches(catches=catches, extradir=extradir,console=FALSE)
plotss3catches <- function(catches,extradir,analysis,console=TRUE) {
  #  catches=catches;extradir=extradir; console=TRUE
  fleetnames <- colnames(catches)
  nfleet <- length(fleetnames)
  yrs <- as.numeric(rownames(catches))
  nyr <- length(yrs)
  totC <- rowSums(catches,na.rm=TRUE)
  if (console) { filen="" } else {
    fileout <- paste0(analysis,"_Reported_catches_for_",nfleet,"_fleets.png")
    filen <- pathtopath(extradir,fileout)
  }
  plotprep(width=7,height=6,newdev=FALSE,filename=filen,verbose=FALSE)
  parset(plots=c(2,1),cex=1.0,margin=c(0.3,0.45,0.1,0.1))
  maxy <- getmax(catches)
  plot(yrs,catches[,1],type="l",lwd=2,col=1,xlab="",
       ylab="Catches (t) by Fleet",ylim=c(0,maxy),yaxs="i",panel.first=grid)
  if (nfleet > 1) for (fl in 2:nfleet) lines(yrs,catches[,fl],lwd=2,col=fl)
  legend("topright",legend=fleetnames,col=c(1:nfleet),lwd=3,bty="n",cex=1.0)
  maxy2 <- getmax(totC)
  curve <- loess(totC ~ c(1:nyr))
  plot(yrs,totC,type="l",lwd=2,col=1,xlab="",ylim=c(0,maxy2),yaxs="i",
       ylab="Total Catches (t) Across Fleets",panel.first=grid())
  lines(yrs,curve$fitted,lwd=2,col=2)
  if (!console) dev.off()
  return(invisible(filen))
} # end of plotss3catches




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
#' @param replaceCS should the control and starter files be replaced by the
#'     SS3 generated control.ss_new and starter.ss_new files (this is useful
#'     if using the tune_comps function for iterstive re-weighting)
#' @param getfiles a vector of filenames to be retrieved from the origin
#'     directory (usually calc) and copied to the destination directory. The
#'     current files chosen are: CompReport.sso, covar.sso, CumReport.sso,
#'     echoinput.sso, Forecast-report.sso, ParmTrace.sso Report.sso, 
#'     SIS_table.sso, warning.sso, ss3.par, ss3.std, control.ss_new, and
#'     starter.ss_new
#'     
#' @seealso{
#'     \link{tune_comps}
#' }     
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
#'  storeresults(calc,destination,replaceCS=FALSE)
#' }
storeresults <- function(origin,destination,replaceCS=FALSE,
                         getfiles=c("CompReport.sso",
                         "covar.sso","CumReport.sso","echoinput.sso",
                         "Forecast-report.sso","Report.sso",
                         "SIS_table.sso","warning.sso","ss3.par","ss3.std",
                         "ss.dat",
                         "control.ss_new","data_echo.ss_new","starter.ss_new")) {
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
   if (replaceCS) {
     analysis <- tail(unlist(strsplit(destination,split="/")),1)
     fileout <- paste0(destination,analysis,".ctl")
     filename <- pathtopath(origin,"control.ss_new")     
     file.copy(filename,fileout,overwrite=TRUE,copy.date=FALSE)
     fileout <- paste0(destination,analysis,".sta")
     filename <- pathtopath(origin,"starter.ss_new")
     file.copy(filename,fileout,overwrite=TRUE,copy.date=FALSE)
   }
} # end of storeresults



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
  pickF <- grep("ForeRecr",rownames(param))
  param <- param[-pickF,]
  pickp <- which(param[,"Phase"] > 0)
  columns <- c("Num","Value","Init","Phase","Min","Max",
               "Parm_StDev","Gradient")
  param2 <- param[pickp,columns]
  param2 <- cbind(param2,CV=abs(param2[,"Parm_StDev"]/param2[,"Value"]))
  colnames(param2) <- c("Num","Value","Init","Phase","Min","Max",
                        "Par_SD","Gradient","CV")
  param3 <- param2[order(param2[,"Phase"]),]
  pickN <- which(param[,"Phase"] < 0)
  columns <- c("Num","Value","Init","Phase","Min","Max")
  noestpars <- param[pickN,columns]
  answer <- c(round(replist$endyr),replist$current_depletion,replist$SBzero,
              (1-replist$sprseries[nrow(replist$sprseries),"SPR"]),M,steep,sigR,
              likes["TOTAL",1],likes["Survey",1],likes["Length_comp",1],
              likes["Age_comp",1],likes["Recruitment",1],likes["Parm_priors",1],
              likes["Forecast_Recruitment",1],maxgrad)
  names(answer) <-  c("EndYear","Depletion","Bzero","1-SPR","M","h","sigmaR",
                      "TotalL","Index","LengthCompL","AgeCompL","Recruit",
                      "Param_Priors","Forecast_Recruitment","Maximum_Gradient")
  return(list(answer=answer,param=param3,likes=likes,noestpars=noestpars))
} # end of summarizeSS3







