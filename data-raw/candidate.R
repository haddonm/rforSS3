


library(rforSS3)
library(r4ss)
library(knitr)
library(rforcpue)
library(codeutils)
library(hplot)

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

wkdir <- pathtopath(getDBdir(),"projects/TheGameMine/")

store <- paste0(wkdir,"basecase/")  # define directories
calc <- paste0(wkdir,"calc/")
# generate these two directories
dirExists(store)
dirExists(calc)

basecase <- c("start",
              "commonly",
              "GOARex",
              "twofleet",
              "matlen",
              "fourfleet",
              "aspm",
              "whiting")         ## 8 ages have small delta         
numdirs <- length(basecase)
# now safely generate the directories
for (direct in 1:numdirs) dirExists(paste0(store,basecase[direct]))


files <- dir(calc,pattern="Report")
printV(files)

filen <- pathtopath(calc,"Report1.sso")
str1(filen)
tmp <- names()


profile <- function (dir, oldctlfile = "control.ss_new", masterctlfile = lifecycle::deprecated(), 
          newctlfile = "control_modified.ss", linenum = NULL, string = NULL, 
          profilevec = NULL, usepar = FALSE, globalpar = FALSE, parlinenum = NULL, 
          parstring = NULL, saveoutput = TRUE, overwrite = TRUE, whichruns = NULL, 
          prior_check = TRUE, read_like = TRUE, exe = "ss3", verbose = TRUE, 
          ...) 
{
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  if (lifecycle::is_present(masterctlfile)) {
    lifecycle::deprecate_warn(when = "1.46.0", what = "profile(masterctlfile)", 
                              with = "profile(oldctlfile)")
    oldctlfile <- masterctlfile
  }
  check_exe(exe = exe, dir = dir, verbose = verbose)
  if (is.null(linenum) & is.null(string)) {
    stop("You should input either 'linenum' or 'string' (but not both)")
  }
  if (!is.null(linenum) & !is.null(string)) {
    stop("You should input either 'linenum' or 'string' (but not both)")
  }
  if (usepar) {
    if (is.null(parlinenum) & is.null(parstring)) {
      stop("Using par file. You should input either 'parlinenum' or ", 
           "'parstring' (but not both)")
    }
    if (!is.null(parlinenum) & !is.null(parstring)) {
      stop("Using par file. You should input either 'parlinenum' or ", 
           "'parstring' (but not both)")
    }
  }
  if (!is.null(linenum)) {
    npars <- length(linenum)
  }
  if (!is.null(string)) {
    npars <- length(string)
  }
  if (usepar) {
    if (!is.null(parlinenum)) {
      npars <- length(parlinenum)
    }
    if (!is.null(parstring)) {
      npars <- length(parstring)
    }
  }
  if (is.na(npars) || npars < 1) {
    stop("Problem with the number of parameters to profile over. npars = ", 
         npars)
  }
  if (is.null(profilevec)) {
    stop("Missing input 'profilevec'")
  }
  if (npars == 1) {
    n <- length(profilevec)
  }
  else {
    if ((!is.data.frame(profilevec) & !is.matrix(profilevec)) || 
        ncol(profilevec) != npars) {
      stop("'profilevec' should be a data.frame or a matrix with ", 
           npars, " columns")
    }
    n <- length(profilevec[[1]])
    if (any(unlist(lapply(profilevec, FUN = length)) != n)) {
      stop("Each element in the 'profilevec' list should have length ", 
           n)
    }
    if (verbose) {
      if (!is.null(string)) {
        profilevec_df <- data.frame(profilevec)
        names(profilevec_df) <- string
        message("Profiling over ", npars, " parameters\n", 
                paste0(profilevec_df, collapse = "\n"))
      }
    }
  }
  if (is.null(whichruns)) {
    whichruns <- 1:n
  }
  else {
    if (!all(whichruns %in% 1:n)) {
      stop("input whichruns should be NULL or a subset of 1:", 
           n, "\n", sep = "")
    }
  }
  if (verbose) {
    message("Doing runs: ", paste(whichruns, collapse = ", "), 
            ",\n  out of n = ", n)
  }
  converged <- rep(NA, n)
  totallike <- rep(NA, n)
  liketable <- NULL
  if (verbose) {
    message("Changing working directory to ", dir, ",\n", 
            " but will be changed back on exit from function.")
  }
  setwd(dir)
  stdfile <- file.path(dir, "ss.std")
  starter.file <- dir()[tolower(dir()) == "starter.ss"]
  if (length(starter.file) == 0) {
    stop("starter.ss not found in", dir)
  }
  starter <- SS_readstarter(starter.file, verbose = FALSE)
  if (starter[["ctlfile"]] != newctlfile) {
    stop("starter file should be changed to change\n", "'", 
         starter[["ctlfile"]], "' to '", newctlfile, "'")
  }
  if (prior_check & starter[["prior_like"]] == 0) {
    stop("for likelihood profile, you should change the starter file value of\n", 
         " 'Include prior likelihood for non-estimated parameters'\n", 
         " from 0 to 1 and re-run the estimation.\n")
  }
  if (usepar & starter[["init_values_src"]] == 0) {
    stop("With setting 'usepar=TRUE', change the starter file value", 
         " for initial value source from 0 (ctl file) to 1 (par file).\n")
  }
  if (!usepar & starter[["init_values_src"]] == 1) {
    stop("Change the starter file value for initial value source", 
         " from 1 (par file) to 0 (par file) or change to", 
         " profile(..., usepar = TRUE).")
  }
  if (usepar) {
    file.copy("ss.par", "parfile_original_backup.sso")
  }
  for (i in whichruns) {
    newrepfile <- paste("Report", i, ".sso", sep = "")
    if (!overwrite & file.exists(newrepfile)) {
      message("skipping profile i=", i, "/", n, " because overwrite=FALSE\n", 
              "  and file exists: ", newrepfile)
    }
    else {
      message("running profile i=", i, "/", n)
      if (npars == 1) {
        newvals <- profilevec[i]
      }
      else {
        newvals <- as.numeric(profilevec[i, ])
      }
      SS_changepars(dir = NULL, ctlfile = oldctlfile, newctlfile = newctlfile, 
                    linenums = linenum, strings = string, newvals = newvals, 
                    estimate = FALSE, verbose = TRUE, repeat.vals = TRUE)
      ctltable_new <- SS_parlines(ctlfile = newctlfile)
      if (!any(ctltable_new[["PHASE"]] == 1)) {
        warning("At least one parameter needs to be estimated in phase 1.\n", 
                "Edit control file to add a parameter\n", "which isn't being profiled over to phase 1.")
      }
      if (usepar) {
        if (globalpar) {
          par <- readLines("parfile_original_backup.sso")
        }
        else {
          par <- readLines("ss.par")
        }
        for (ipar in 1:npars) {
          if (!is.null(parstring)) {
            parlinenum <- grep(parstring[ipar], par, 
                               fixed = TRUE) + 1
          }
          if (length(parlinenum) == 0) {
            stop("Problem with input parstring = '", 
                 parstring[ipar], "'")
          }
          parline <- par[parlinenum[ipar]]
          parval <- as.numeric(parline)
          if (is.na(parval)) {
            stop("Problem with parlinenum or parstring for par file.\n", 
                 "line as read: ", parline)
          }
          par[parlinenum[ipar]] <- ifelse(npars > 1, 
                                          profilevec[i, ipar], profilevec[i])
        }
        note <- c(paste("# New par file created by profile() with the value on line number", 
                        linenum), paste("# changed from", parval, "to", 
                                        profilevec[i]))
        par <- c(par, "#", note)
        message(paste0(note, collapse = "\n"))
        writeLines(par, paste0("ss_input_par", i, ".ss"))
        writeLines(par, "ss.par")
      }
      if (file.exists(stdfile)) {
        file.remove(stdfile)
      }
      if (file.exists("Report.sso")) {
        file.remove("Report.sso")
      }
      run(dir = dir, verbose = verbose, exe = exe, ...)
      converged[i] <- file.exists(stdfile)
      onegood <- FALSE
      if (read_like && file.exists("Report.sso") & file.info("Report.sso")$size > 
          0) {
        onegood <- TRUE
        Rep <- readLines("Report.sso", n = 400)
        skip <- grep("LIKELIHOOD", Rep)[2]
        nrows <- grep("Crash_Pen", Rep) - skip - 1
        like <- read.table("Report.sso", skip = skip, 
                           nrows = nrows, header = TRUE, fill = TRUE)
        liketable <- rbind(liketable, as.numeric(like[["logL.Lambda"]]))
      }
      else {
        liketable <- rbind(liketable, rep(NA, 10))
      }
      if (saveoutput) {
        file.copy("Report.sso", paste("Report", i, ".sso", 
                                      sep = ""), overwrite = overwrite)
        file.copy("CompReport.sso", paste("CompReport", 
                                          i, ".sso", sep = ""), overwrite = overwrite)
        file.copy("covar.sso", paste("covar", i, ".sso", 
                                     sep = ""), overwrite = overwrite)
        file.copy("warning.sso", paste("warning", i, 
                                       ".sso", sep = ""), overwrite = overwrite)
        file.copy("admodel.hes", paste("admodel", i, 
                                       ".hes", sep = ""), overwrite = overwrite)
        file.copy("ss.par", paste("ss.par_", i, ".sso", 
                                  sep = ""), overwrite = overwrite)
      }
    }
  }
  if (onegood) {
    liketable <- as.data.frame(liketable)
    names(liketable) <- like[["Component"]]
    bigtable <- cbind(profilevec[whichruns], converged[whichruns], 
                      liketable)
    names(bigtable)[1] <- "Value"
    return(bigtable)
  }
  else {
    stop("Error: no good Report.sso files created in profile")
  }
}




plotreport  <- SS_output(dir=destination, 
                         repfile = "Report.sso",
                         compfile = "CompReport.sso",
                         covarfile = "covar.sso",
                         forefile = "Forecast-report.sso",
                         wtfile = "wtatage.ss_new",
                         warnfile = "warning.sso",
                         forecast=FALSE,
                         covar=TRUE,
                         readwt=FALSE,
                         verbose=TRUE)



calc

files <- dir(calc)
pick <- grep("Report",files,fixed=TRUE)
files[pick]

profvar <- "LnR0"
ctlline <- 123
parline <- 49
newctl <- "ss.ctl"
profvec <- c(7.7,7.8,7.9,8.0,8.1,8.2,8.3)
exename <- "ss"
steps <- length(profvec)


profile(dir=calc,oldctlfile="control.ss_new",newctlfile=newctl,linenum=ctlline,
        profilevec=c(7.7,7.8,7.9,8.0,8.1,8.2,8.3),usepar=TRUE,
        parlinenum=parline,exe=exename,show_in_console = TRUE)

plotreport  <- SS_output(dir=calc, 
                         repfile = "Report1.sso",compfile = "CompReport1.sso",
                         covarfile = "covar1.sso",forefile = "Forecast-report1.sso",
                         wtfile = "wtatage1.ss_new",warnfile = "warning1.sso",
                         forecast=FALSE,covar=TRUE,readwt=FALSE,
                         verbose=FALSE,printstats=FALSE)
profout <- summarizeSS3(plotreport)
numlikes <- nrow(profout$likes)
rows <- rownames(profout$likes)

likelihoods <- matrix(0,nrow=numlikes,ncol=steps,dimnames=list(rows,profvec))
ansrows <- names(profout$answer)
numans <- length(ansrows)
answer <- matrix(0,nrow=numans,ncol=steps,dimnames=list(ansrows,profvec))
likelihoods[,1] <- profout$likes[,"values"]
answer[,1] <- profout$answer 
for (i in 2:steps) {
  cat("step ",i,"\n")
   rep <- paste0("Report",i,".sso")
   comp <- paste0("CompReport",i,".sso")
   covar <- paste0("covar",i,".sso")
   fore <- paste0("Forecast-report",i,".sso")
   wts <- paste0("wtatage",i,".ss_new")
   warn <- paste0("warning",i,".sso")
   plotreport  <- SS_output(dir=calc, repfile = rep, compfile = comp,
                            covarfile = covar, forefile = fore,
                            wtfile = wts,warnfile = warn,
                            forecast=FALSE,covar=TRUE,readwt=FALSE,
                            verbose=FALSE,printstats=FALSE)
   profout <- summarizeSS3(plotreport)
   likelihoods[,i] <- profout$likes[,"values"]
   answer[,i] <- profout$answer
}


repfile <- pathtopath(calc,"Report.sso")

Rep <- readLines(repfile, n = 400)
skip <- grep("LIKELIHOOD", Rep)[2]
nrows <- grep("Crash_Pen", Rep) - skip - 1
like <- read.table("Report.sso", skip = skip, 
                   nrows = nrows, header = TRUE, fill = TRUE)
liketable <- rbind(liketable, as.numeric(like[["logL.Lambda"]]))



like <- read.table(repfile, skip = skip, 
                   nrows = nrows, header = TRUE, fill = TRUE)



