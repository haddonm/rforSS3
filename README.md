# Introduction

rforSS3 is currently under development as the R package r4ss (at https://github.com/r4ss/r4ss) has now included a good deal of the original functionality that I had cobbled together. r4ss now provides functions that assist with iteriatve re-weighting of the variances of composition data and also conducting Likelihood profiles so the functions that assisted with those operations are no longer needed. Previously rforSS3 contained a runSS3 command but r4ss's is much more through with regards error checking the inputs.

rforSS3 is primarily for use when using my own strategy for running SS3, whic his set up using:


library(rforSS3)
library(codeutils)
library(r4ss)

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

*wkdir is the directory within which the directory structure is setup.*
*save a copy of this R file in the working directory to secure any customizations*
*that are made*

wkdir <- pathtopath(getDBdir(),"projects/a_species/")

*source("C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/oro2017utils.R")*
*The directory structure inside the wkdir will include 'store' and 'calc'*
*'store' contains a directory for each step in the move from the previous *
*balanced basecase to the new balanced basecase.* 
*'calc' is the directory in which all the SS3 files are put ready for analysis.*
*SS3.exe needs to be put in 'calc'.* 
store <- pathtopath(wkdir,"basecase/")  # define directories
calc <- pathtopath(wkdir,"calc/")
*generate these two directories*
dirExists(store,create=TRUE)  
dirExists(calc,create=TRUE)

*what sub-directories will be used within the 'basecase' directory?*
*Change these names to suit those that will actually be used.*
*The order represents the order in which data will be added to the earliest of*
*simplest model leading to the final balanced basecase*
basecase <- c("init",
              "initaddcatch",
              "initaddindex",
              "balancevariances",
              "adjustrecrbias") 
numdirs <- length(basecase)
*now safely generate the directories*
for (direct in 1:numdirs) dirExists(paste0(store,basecase[direct]))
{
print("Now ensure that you have X.ctl, X.dat, X.par, X.for, and X.sta file",quote=FALSE)
print("in each directory; the .for will become forcast.ss and the .sta file",quote=FALSE)
print("will become the starter.ss file",quote=FALSE)
}

The approach that works for me is to have the basecase directory and have it contain a set of sub-directories, one each for the different variants of an analysis that is wanted. In the example code above there is a starting model structure, which might have been produced in a previous assessment. In a new assessment, one might incrementally add data sets to determine their relative contribution to any changes and help determine whether the information within any data is in conflict with other data sets. 

Within each of the sub-directories describing the run variants there needs to be at least four files. For example, in the *init* sub_directory there needs to be  the following:

* init.ctl  - the control file 
* init.dat  - the data file
* init.sta  - the starter file
* init.for  - the forecast file

optionally one can also have  an init.par file for initial parameter values

In the *calc* directory one only needs to start with a copy of the latest ss3.exe

The essence of the approach is that it copies the relevant files over to the calc directory, changing their names appropriately on the way so that ss3 recognizes them. After the calculations are completed it copies required files from the calc directory back to the selected case sub-directory within the store (here = basecase). It also drives the production of the results.


## Using run_SS3.R

One proceeds by calling 

iter <- 1                     #  or whichever case you wish to run
getCase(index=iter,basecase)  # just to ensure the case you selected is the one you want. 

starttime <- Sys.time()
    iter <- 1    # pick analysis or create a loop here iter is chosen, can be hashed out 
    analysis <- getCase(index=iter,basecase)  # iter could be: for (iter = 1:length(basecase)) 
    cat("\n\n") 
    print("New Analysis") 
    print(analysis) 
    destination <- paste0(store,analysis,"/") 
    print(destination) 
    cleanDir(store,analysis) 
    # cleans out any old analyses if present
    
    copyfiles(analysis,store,calc) # copy and rename the needed files into calc 
    #now call SS3 -nohess twice, second time using the pars from the
    #first, then the third time to calculate the hessian
    
    run(dir=calc,exe="ss3",extras="-stopph 0 -nohess",show_in_console = TRUE, 
        skipfinished = FALSE) 
    #This option is used when developing new options and simply checks that all 
    #the input files can be read  correctly. It can, of course, be hashed out 
    #so the full estimation can occur as in: 
    
    run(dir=calc,exe="ss3",extras="-nohess -maxfn 500",show_in_console = TRUE,
        skipfinished = FALSE)   
    #which attempts to find an optimum model fit. if it generates a reasonable 
    #fit then one might proceed to generating the hessian* 
    
    fixstarter(calc,findtext="init_values_src") 
    #First change the use_init_values argument in the starter file. This alters 
    #the starter.ss file within the calc directory and leaves your original 
    #as it was* 
    
    # estimate the inverse Hessian
    run(dir=calc,exe="ss3",extras="-maxfn 500",show_in_console = TRUE,
        skipfinished = FALSE)

    # Now return the required files back into the source directory defined
    # in store and basecase
    
    storeresults(calc,destination)
    # now print and plot the results using r4ss
    # first go to the directory where the results have been stored
    print(store)
    setwd(store)
    print(paste0("Running from ",destination))
    fileout <- pathtopath(destination,paste0(analysis,".txt"))
    sink(fileout)  # save the screen output to a text file, just in case

    plotreport  <- SS_output(dir=destination, 
                             repfile = "Report.sso",
                             compfile = "CompReport.sso",
                             covarfile = "covar.sso",
                             forefile = "Forecast-report.sso",
                             wtfile = "wtatage.ss_new",
                             warnfile = "warning.sso",
                             forecast=FALSE,  # alter this to TRUE if you forecast
                             covar=TRUE,      # assumes a hessian was possible
                             readwt=FALSE,
                             verbose=TRUE)

    finalplot <- SS_plots(replist=plotreport, plot=1:26,
                          minbthresh=Blimit,
                          uncertainty=TRUE, datplot=TRUE,
                          forecastplot=FALSE, png=TRUE,html=FALSE)
    SS_html(replist=plotreport,
            plotdir=pathtopath(destination,"plots"),title=analysis)
    # this latter allows each 'website display' to be named after the case selected
    
    sink()   # close off fileout containing screen dump from SS_output and SS_plots
             # if you type to the console and nothing appears to happen you may have
             # forgotten this step
    
    filename <- filenametoPath(destination,paste0("plotreport_",analysis,".Rdata"))
    save(plotreport,file=filename)   # for later analysis if desired
    cat("\n\nplotreport saved to ",filename)
    # load(filename)
    cat("SS_output and SS_plots txt sent to ",fileout,"\n")
    # Tidy up; return to wkdir
    setwd(wkdir)
endtime <- Sys.time()
print(endtime - starttime)


You may also find use for:

round(printV(summarizeSS3(plotreport)$answer),6)

print(summarizeSS3(plotreport)$param)

which summarize the model outputs and the model likelihoods by data source.



