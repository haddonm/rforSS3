
# rm(list=ls(all=TRUE))   # clears all objects from the primary environment
# Get latest (master) version of r4ss from Github:

# devtools::install_github("r4ss/r4ss")



# Set up for analyses --------------------------------------------------------

library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
library(makehtml)

ddir <- getDBdir()
wkdir <- pathtopath(ddir,"/A_CodeR/SA-SS3/")
source(pathtopath(wkdir,"ss3_utilities.R")) 
#library(r4maps)

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

# wkdir is the directory within which the directory structure is setup.
# save a copy of this R file in the working directory to secure any customizations
# that are made

#source("C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/oro2017utils.R")
# The directory structure inside the wkdir will include 'store' and 'calc'
# 'store' contains a directory for each step in the move from the previous
# balanced basecase to the new balanced basecase.
# 'calc' is the directory in which all the SS3 files are put ready for analysis.
# SS3.exe needs to be put in 'calc'.
store <- pathtopath(wkdir,"HAB/garfish/")  # define directories
calc <- pathtopath(wkdir,"HAB/calc/")
profile <- pathtopath(wkdir,"HAB/profile/")
# generate these two directories
dirExists(store)
dirExists(calc)
dirExists(profile)

SS3path <- pathtopath(calc,"ss3.exe")
# what sub-directories will be used within 'basecase'?
# Change these names to suit those that will actually be used.
# The order represents the order in which data will be added to the earliest of
# simplest model leading to the final balanced basecase

#      sscopyto(origin=store,fromdir="GAR_SG_DEV",todir="GAR_SG_DEV_10")

if (endpart(store) == "garfish") { # GARFISH BASECASE  ----------------
  basecase <- c("GAR_SG_DEV",
                "GAR_SG_mh",
                "GAR_SG_DEV_05",
                "GAR_SG_DEV_10",
                "GAR_GSV_DEV")    # Natural Morality = 0.6
} else {
  stop("Are you in the wrong directory? \n")
}


numdirs <- length(basecase)
# now safely generate the directories
for (direct in 1:numdirs) dirExists(paste0(store,basecase[direct]))
printV(basecase)


{
print("Now ensure that you have X.ctl, X.dat, X.par, X.for, and X.sta file",quote=FALSE)
print("in each directory; the .for will become forcast.ss and the .sta file",quote=FALSE)
print("will become the starter.ss file",quote=FALSE)
}


# sscopyto(origin=store,fromdir="SGBC-S75-M5",todir="SGBC-S75-M5-mh")


# now ensure that you have X.ctl, X.dat, X.par, X.for, and X.sta file in each
# directory where X is the name of each directory. The .ctl, .dat, and .par
# files relate to files of the same name but the .for file will become the
# forcast.ss file and the .sta file will become the starter.ss file. 
# The idea is to conduct a bridging analysis from an old assessment
# to a new assessment that includes all the new data and is balanced.
#
# loop through the directories of 'basecase' if you have prepared the five
# required files in each directory, that is the  X.ctl, X.dat, X.par, X.for,
# and X.sta file. That would entail including extra data in the dat file and
# adjusting the .ctl file accordingly.
#
# remember 'basecase' is the directory in which these sub-directories are kept.
#L -----
# Fit a model ------
#numiter <- length(basecase)
printV(basecase)


item <- 2
getCase(index=item,basecase)   # this lists the basecase indices to the screen

#executable <- c("SS","SS","SS","SS","SS","SS","SS3","SS3")
starttime <- Sys.time()
    analysis <- getCase(index=item,basecase)  # 
    cat("\n\n")
    print("New Analysis")
    print(analysis)
    destination <- paste0(store,analysis,"/")
    print(destination)
    extradir <- pathtopath(destination,"extra")
    dirExists(extradir)
   # cleanDir(store,analysis)   # cleans out any old analyses if present
    copyfiles(analysis,store,calc) # copy and rename the needed files into calc
    # now call SS3 -nohess twice, second time using the pars from the
    # first, then the third time to calculate the hessian
     # run(dir=calc,exe="ss3",extras="-stopph 0 -nohess",show_in_console = TRUE,
     #       skipfinished = FALSE)
    run(dir=calc,exe=SS3path,extras="-nohess -maxfn 700",show_in_console = TRUE,
        skipfinished = FALSE)   

    fixstarter(calc,findtext="use init values")  # Check the findtext
 # estimate the inverse Hessian
    run(dir=calc,exe=SS3path,extras="-maxfn 700",show_in_console = TRUE,
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
    sink(fileout)
    Btarget <- 0.50  # 
    Blimit  <- 0.20  # default Commonwealth Limit Reference Point

    plotreport  <- SS_output(dir=destination, 
                             repfile = "Report.sso",
                             compfile = "CompReport.sso",
                             covarfile = "covar.sso",
                             forefile = "Forecast-report.sso",
                             wtfile = "wtatage.ss_new",
                             warnfile = "warning.sso",
                             forecast=TRUE,
                             covar=TRUE,
                             readwt=FALSE,
                             verbose=TRUE)

    finalplot <- SS_plots(replist=plotreport, plot=1:26,
                          btarg=Btarget,minbthresh=Blimit,
                          uncertainty=TRUE, datplot=TRUE,
                          forecastplot=TRUE, 
                          png=TRUE,html=FALSE)
    SS_html(replist=plotreport,plotdir=pathtopath(destination,"plots"),
            title=analysis,width=600)
    
    
    sink()   # close off fileout containing screen dump from SS_output and SS_plots
    
   filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
    save(plotreport,file=filename)
    cat("\n\nplotreport saved to ",filename)
    # load(filename)
    cat("SS_output and SS_plots txt sent to ",fileout,"\n")
    # Tidy up; return to wkdir
    setwd(wkdir)
 

endtime <- Sys.time()
print(endtime - starttime)

tldr <- summarizeSS3(plotreport)

round(printV(tldr$answer),6)

toXL(tldr$answer)

print(summarizeSS3(plotreport)$param)

#   load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  

#       sort(names(plotreport))

# End model fit --------------------------------------

# Compare outputs----------------------------



compscenes <- getreplists(store=store,
                          scenes=c("GAR_SG_mh","GAR_SG_DEV"),
                          listname="plotreport")

projout <- projreceffects(compscenes=compscenes,legcex=1.0,startyr=2,console=TRUE)

tail(projout$depl)

outproj <- getprojdyn(compscenes)

outlists <- rforSS3::do_extra(plotreport = plotreport,
                              extradir = extradir,
                              analysis = analysis,
                              store = store,
                              compare = c("GAR_SG_mh","GAR_SG_DEV_05"),
                              verbose = TRUE)





# Tuning variances --------------------------------------


fltnames <- plotreport$FleetNames  
pickfleets <- c(1)
nfleet <- length(pickfleets)
tuneinfo <- r4ss:::get_tuning_table(plotreport,fleets=2,digits=6,
                                    option="Francis")
tuneinfo

tuneinfo[,1:3]



i <- 1  # for loop required if more than one fleet with comp data
cat("   ",tuneinfo[1,1],tuneinfo[1,2],tuneinfo[1,3],
    "  # Variance adjustment_",fltnames[pickfleets[i]],"_Length  \n")
cat("   ",tuneinfo[2,1],tuneinfo[2,2],tuneinfo[2,3],
    "  # Variance adjustment_",fltnames[pickfleets[i]],"_Age \n")
}



tune_comps(plotreport,fleets=c(2),option="Francis",digits=7,niters_tuning = 3,init_run=FALSE,
           dir=calc,exe=SS3path,extras="-nohess",allow_up_tuning=TRUE,verbose=TRUE,
            show_in_console=TRUE)

storeresults(calc,destination) # NOTE ctl and sta files replaced

# now re-run the whole model from line 106 so that hessian is calculated

# Recruit bias Ramp-----------------------

biasramp <- SS_fitbiasramp(plotreport,
                           verbose = FALSE,
                           startvalues = NULL,
                           method = "BFGS",
                           twoplots = TRUE,
                           transform = FALSE,
                           plot = TRUE,
                           print = FALSE, # print to png files?
                           plotdir = "",
                           shownew = TRUE,
                           oldctl = NULL,
                           newctl = NULL,
                           altmethod = "nlminb",
                           exclude_forecast = FALSE,
                           pwidth = 6.5,pheight = 5,punits = "in",
                           ptsize = 10,res = 300,cex.main = 1)
biasramp

# paste the following into the ctl file.
newpars <- biasramp$df
{
for (i in 1:4) {
  cat("  ",newpars[i,"value"],newpars[i,"label"],"\n")
}
cat("  ",newpars[5,"value"],"#_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0) \n")
}


# Likelihood Profile -----------------------------------
# Now do the profile

copyproffiles(calc=calc,profdir=profdir)

setwd(profdir)

profvec <- c(0.39,0.41,0.43,0.45,0.47,0.49,0.51,0.53,0.55,0.57,0.59,0.61,0.63,0.65)  # NatM for garfish
# check linenums in control.ss_new and in ss3.par

profans <- r4ss::profile(dir=profdir,oldctlfile="ss.ctl",newctlfile="ss.ctl",
                         linenum=75,profilevec=profvec,
                         usepar=TRUE,parlinenum=5,exe="ss3",
                         extras="-nohess -maxfn 600",show_in_console = TRUE)


outout <- SSgetoutput(keyvec=c(1:14),verbose=FALSE)
sumout <- SSsummarize(outout)

# PinerPlot(summaryoutput=sumout,component = "TOTAL",
#           profile.string="_NatM_p_1_Fem_GP_1")

outlike <- getobjgrad(profdir=profdir,profvec=profvec,parlinenum=29)

alllikes <- sumout$likelihoods

nprof <- length(profvec)
numrow <- nrow(alllikes)
scalelike <- alllikes
for (i in 1:numrow) scalelike[i,1:nprof] <- alllikes[i,1:nprof] - min(alllikes[i,1:nprof])
tmp <- t(apply(scalelike[,1:14],1,range))
rownames(tmp) <- scalelike[,15]
round(tmp,5)


plotprep(width=10,height=6)
parset(cex=1.0)
plot(profvec,scalelike[which(scalelike[,"Label"] == "TOTAL"),1:14] ,type="l",
     lwd=2,col=1,xlab="Natural Mortality",ylab="Scaled Likelihoods",
     panel.first = grid())
abline(h=1.92,lwd=2,col=1,lty=3)
pickL <- which(scalelike[,"Label"] == "Length_comp")
lines(profvec,scalelike[pickL,1:14],lwd=2,col=2)
pickL <- which(scalelike[,"Label"] == "Age_comp")
lines(profvec,scalelike[pickL,1:14],lwd=2,col=3)
pickL <- which(scalelike[,"Label"] == "Recruitment")
lines(profvec,scalelike[pickL,1:14],lwd=2,col=4)
pickL <- which(scalelike[,"Label"] == "Survey")
lines(profvec,scalelike[pickL,1:14],lwd=2,col=6)
legend("topright",c("Total","Length_comp","Age_comp","recruit","CPUE"),lwd=3,
       col=c(1,2,3,4,6),bty="n",cex=1.5)




if (any(profans[,"converged[whichruns]"]) == FALSE) {
  cat("Some parameter values failed to converge \n")
} else {
  cat("all trials succeeded inconverging  \n")
}
 
invar <- "LnR0"
minlike <- 0.01
keepcols <- which(abs(colSums(profans)) > minlike)
limprofans <- profans[,keepcols]
colans <- ncol(limprofans)
numcol <- colans - 2
columns <- colnames(limprofans)[3:colans]
numrow <- length(profvec)
likes <- matrix(0,nrow=numrow,ncol=numcol,dimnames=list(profvec,columns))
mins <- apply(limprofans[,3:colans],2,min,na.rm=TRUE) 
for (i in 1:numcol) {
  likes[,i] <- limprofans[,(i+2)]-mins[i]
}
round(limprofans[,3:colans],5)
round(likes,5)

pickfinal <- which(apply(likes,2,max,na.rm=TRUE) > minlike)
limlikes <- likes[,pickfinal]
round(limlikes,5)
numfinal <- length(pickfinal)
label <- colnames(limlikes)

maxy <- getmax(limlikes[3:13,])
plotprep(width=10, height=6)
parset()
plot(profvec[3:13],limlikes[3:13,"TOTAL"],type="l",lwd=4,col=1,xlab=invar,
     ylab="Likelihood Difference",ylim=c(0,maxy),panel.first = grid())
for (i in 2:numfinal) lines(profvec[3:13],limlikes[3:13,i],col=i,lwd=3)
abline(h=c(0,1.92),lwd=1,col=1,lty=c(1,3))
legend("topright",legend=label,lwd=4,col=1:numfinal,bty="n",cex=1.5)


#Forecast report ----------------------------------------
replistSG <- plotreport
sort(names(replistSG))

filename <- pathtopath(destination,"Forecast-report.sso")
forecast <- readLines(filename)
length(forecast)

forecastyrs <- replistSG$nforecastyears
#pickL <- grep("FORECAST:_With_F_to_match_adjusted_catch",forcast)
pickL <- grep("FORECAST:_With_Constant_F",forecast)
pred <- forecast[pickL:(pickL+forecastyrs+1)]
n <- length(pred)

columns <- removeEmpty(unlist(strsplit(pred[2]," ")))
label <- c("year","bio-Smry","SpawnBio","Depletion","recruit-0","Total_Catch")
pickcol <- which(columns %in% label)
columns[pickcol]
result <- matrix(0,nrow=forecastyrs,ncol=length(pickcol))
colnames(result) <- label

for(i in 3:n) { # i = 3
  txt <- removeEmpty(unlist(strsplit(pred[i]," ")))
  result[(i-2),] <- as.numeric(txt[pickcol])
}
rownames(result) <- result[,"year"]
result

toXL(result)

sort(names(replistSG))

catch <- replistSG$catch
properties(catch)

obscatch <- tapply(catch$dead_bio,list(catch$Yr,catch$Fleet),sum,na.rm=TRUE)
rowSums(obscatch,na.rm=TRUE)

catch <- replistGSV$catch
properties(catch)

obscatch <- tapply(catch$dead_bio,list(catch$Yr,catch$Fleet),sum,na.rm=TRUE)
rowSums(obscatch,na.rm=TRUE)


# end forecast report--------------------------------

# Tabulate results---------------------------------------------

analysis <- "SGbasecase"
destination <- pathtopath(store,analysis)

filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)

SSexecutivesummary(replist=plotreport,
  plotfolder = "default",
  ci_value = 0.95,
  es_only = FALSE,
  fleetnames = NULL,
  add_text = "SG",
  so_units = "millions of eggs",
  divide_by_2 = FALSE,
  endyr = NULL,
  adopted_ofl = NULL,
  adopted_abc = NULL,
  adopted_acl = NULL,
  forecast_ofl = NULL,
  forecast_abc = NULL,
  verbose = TRUE
)


replist <- plotreport
fleetnames=NULL

nfleets <- replist[["nfleets"]]
startyr <- replist[["startyr"]]
foreyr <- replist[["nforecastyears"]]
endyr <-  replist[["endyr"]]

years <- (endyr - 9):(endyr + 1)
fore <- (endyr + 1):(endyr + foreyr)
years_minus_final <- years[1:(length(years) - 1)]
all <- startyr:max(fore)
nareas <- replist[["nareas"]]
fleetnames <- if (is.null(fleetnames) || fleetnames[1] == 
                  "default") {
  replist[["FleetNames"]]
}    else {
  fleetnames
}

# Get All PARAMETERS---------------------------------------

library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
wkdir <- pathtopath(getDBdir(),"projects/SA_SS3_Garfish/sagarfish/")
store <- pathtopath(wkdir,"basecase/")  # define directories
analysis <-  "SGbasecase"
tmpdest <- paste0(store,analysis,"/")
filename <- pathtopath(tmpdest,paste0("plotreport_",analysis,".Rdata"))
load(filename)

pars <- plotreport$parameters
pickP <- which((pars[,"Phase"] > 0) & (pars[,"Value"] != 0))
fittedpars <- pars[pickP,]
usecols <- c(3,5,12)
fittedpars[,usecols]

pickrec <- grep("RecrDev",fittedpars[,"Label"])
nrec <- length(pickrec)

mainpars <- fittedpars[-pickrec,usecols]  # 

# Estimated parameters
knitr::kable(mainpars[,usecols],row.names=TRUE,digits=c(5,0,12))

pars <- plotreport$parameters
pickP <- which((pars[,"Phase"] < 0) & (pars[,"Value"] != 0))
fixedpars <- pars[pickP,c(3,5)]
# Fixed parameters
knitr::kable(fixedpars,digits=c(6,0))

# Projected Catches-----------------------------------
sort(names(plotreport))
printV(names(plotreport$timeseries))
fleets <- plotreport$FleetNames
nfleet <- length(fleets)
namecols <- c("year","SpawnB","recruits",paste0("C(B)_",fleets),"TotalC",
              "Depletion","ABC-Buffer","Era")
columns <- c(2,7,8,17,25,33,41,49)
catches <- plotreport$timeseries[,columns]
depl=catches[,2]/catches[1,2]
abc <-  plotreport$timeseries[,"ABC_buffer"]
era <-  plotreport$timeseries[,"Era"]
catchall <- cbind(catches,rowSums(catches[,4:8]),depl,abc,era)
colnames(catchall) <- namecols
catchall


# Quick Report-----------------------------------
analysis <-  "SGbasecase"
store <- "c:/Users/malco/DropBox/A_CodeR/SA-SS3/garfish/"
destination <- paste0(store,analysis,"/")
filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)
printV(round(summarizeSS3(plotreport)$answer,6))

# SELECTIVITY--------------------------------------
analysis <-  "BC-5-4-75"
store <- "c:/Users/malco/DropBox/A_CodeR/SA-SS3/garfish/"
destination <- paste0(store,analysis,"/")
filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)


plotselex(plotreport,sex="Male",yrs=c(1984,2004,2016))

plotselex(plotreport,sex="Female",yrs=c(1984,2004,2016))

# BIOMASS _at_AGE ----------------------------------------------
batage <- plotreport$batage
printV(colnames(batage))

pickcol <- c(3,8,10,11,12:19)
batage <- batage[,pickcol]
batage$totB <- NA
batage$totB <- rowSums(batage[,7:12])

pickB <- which((batage$Sex == 1) & (batage[,"Beg/Mid"] == "B"))
bfem <- batage[pickB,]
bfem$depl <- NA
bfem$depl <- bfem$totB/bfem$totB[1]
bfem

pickB <- which((batage$Sex == 2) & (batage[,"Beg/Mid"] == "B"))
bmal <- batage[pickB,]
bmal$depl <- NA
bmal$depl <- bmal$totB/bmal$totB[1]
bmal

# Biomass
yrs <- 1984:2027
picky <- match(yrs,bmal[,"Yr"])
maxy <- getmax(bmal[picky,"totB"])
plotprep(width=9,height=4.5)
parset()
plot(yrs,bmal[picky,"totB"],type="l",lwd=2,ylim=c(0,maxy),xlab="",
     ylab="Total Biomass",panel.first=grid())
lines(yrs,bfem[picky,"totB"],lwd=2,col=2)
legend("bottomleft",c("Male","Female"),col=c(1,2),lwd=3,bty="n",cex=1.5)

# depletion
yrs <- 1984:2027
picky <- match(yrs,bmal[,"Yr"])
maxy <- getmax(bmal[picky,"depl"])
plotprep(width=9,height=4.5)
parset()
plot(yrs,bmal[picky,"depl"],type="l",lwd=2,ylim=c(0,maxy),xlab="",
     ylab="Total Biomass Depletion",panel.first=grid())
lines(yrs,bfem[picky,"depl"],lwd=2,col=2)
legend("bottomleft",c("Male","Female"),col=c(1,2),lwd=3,bty="n",cex=1.5)



# NUMBERES-AT-AGE-----------------------------------------------------

natage <- plotreport$natage_annual_2_with_fishery
properties(natage)

yrs <- sort(unique(natage[,"Yr"]))
nyr <- length(yrs)
nagef <- natage[which(natage[,"Sex"] == 1),]
nagem <- natage[which(natage[,"Sex"] == 2),]

sumfreqf <- rowSums(nagef[,5:10],na.rm=TRUE)
sumfreqm <- rowSums(nagem[,5:10],na.rm=TRUE)

sumfreqcntf <- sumfreqcntm <- sumfreqf
for (i in 1:nyr) {
  sumfreqcntf[i] <- sum(nagef[i,5:10] * 1:6) 
  sumfreqcntm[i] <- sum(nagem[i,5:10] * 1:6) 
}
avagef <- sumfreqcntf/sumfreqf
avagem <- sumfreqcntm/sumfreqm
cbind(yrs,avagef,avagem)


# Get answers ----------------

repfile <- pathtopath(destination,"report.sso")
repdat <- readLines(repfile)
pick <- grep("SSB_",repdat)
ssb <- repdat[pick[3:65]]

yrs <- 26:85
nyr <- length(yrs)
columns <- c("Value","ssb","sd","depl")
ssbdat <- as.data.frame(matrix(0,nrow=nyr,ncol=length(columns),
                               dimnames=list(yrs,columns)))
for (i in 1:nyr) { # i=1
  tmp <- unlist(strsplit(ssb[i]," "))
  if (length(tmp) == 3) {
    ssbdat[i,1] <- tmp[1]
    ssbdat[i,2:3] <- as.numeric(tmp[2:3])
  }
}
ssbdat

pickpos <- which(ssbdat[,2] > 0)
ssbdat <- ssbdat[pickpos,]
pickB0 <- which(ssbdat[,1] == "SSB_unfished")
ssbdat[1:pickB0,4] <- ssbdat[1:pickB0,"ssb"]/ssbdat[pickB0,2]
ssbdat

nfished <- as.data.frame(unlist(strsplit(ssbdat[1:(pickB0-1),1],"_")))
maxy <- getmax(ssbdat[1:(nrow(ssbdat)-1),2])
plotprep(width=9,height=8)
parset(plots=c(2,1))
plot(yrs,ssbdat[,"ssb"],type="l",lwd=2,ylab="Spawning Stock Biomass",
     xlab="years",panel.first=grid(),ylim=c(0,maxy),yaxs="i")


pickts <- grep("report:16",repdat)[2]
pickend <- grep("report:17",repdat)[2] - 2

timeser <- repdat[pickts:pickend]

yrs <- 26:76
columns <- unlist(strsplit(timeser[2]," "))
tsdat <- matrix(0,nrow=51,ncol=45,dimnames=list(yrs,columns[1:45]))
for (i in 3:55) {
  tmp <- as.numeric(unlist(strsplit(timeser[i]," ")))
  tsdat[i,] <- tmp[1:45]
}

pick <- c(2,5:8,19,20)
tsdat[,pick]

plot1(yrs,tsdat[,"SpawnBio"])













laa <- as.matrix(plotreport$growthseries[1,5:85]) #length-at-age
ages <- as.numeric(colnames(laa))                 # ages   
laa <- as.numeric(laa)
class(laa)
class(ages)
laa <- cbind(ages,laa)
rownames(laa) <- ages

matur <- plotreport$biology[,c("Mean_Size","Mat_len")]
index <- numeric(81)
for (i in 1:81) index[i] <- which.closest(laa[i,1],matur[,1])
maa <- matur[index,2]                            # maturity at age

plotprep()
plot(ages,maa,type="l",lwd=2)

laam <- cbind(laa,maa)                      # ages, laa, maa

baa <- plotreport$batage

yrcol <- which(colnames(baa) == "Yr")
# first pick females at beginning of year
pick <- which((baa[,"Sex"] == 1) &
              (baa[,"Beg/Mid"] == "B"))
nyr <- length(pick)
fem <- matrix(0,nrow=nyr,ncol=81)
for (i in 1:nyr) fem[i,] <- as.numeric(maa * baa[pick[i],13:93])
   
pr <- plotreport
pr$timeseries

ans <- pr$derived_quants
answer <- ans[189:210,]
answer <- answer[,-c(1,4,5)]

round(answer,4)
exp(answer["ln(SPB)_2018",1] - answer["ln(SPB)_1980",1])

# End get answers ----------------------------------

#L -------
# New Autobalancing --------
# initial setup ready for aut-balancing

iter <- 9    # pick analysis or create a loop here origbase is chosen
analysis <- getCase(index=iter,basecase)
starttime <- Sys.time()
destination <- paste0(store,analysis,"/")
ctrlfile <- filenametoPath(destination,paste0(analysis,".ctl"))


steps <- 7
rown <- c("Parm_priors","Parm_softbounds","F_Ballpark","Catch","TOTAL",
          "Survey","Age_comp","Recruitment","Depletion","Bzero","1-SPR")
result <- matrix(0,nrow=length(rown),ncol=(steps+1),dimnames=list(rown,0:steps))
result
# do the SS3 fit.
analysis <- getCase(index=iter,basecase)
executable <- c("SS3.24z","SS3.24z","SS3","SS3","SS3","SS3","SS3","SS3","SS3")
for (stp in 1:steps) {   #   stp=3
   cleanDir(store,analysis)   # cleans out any old analyses if present
   copyfiles(analysis,store,calc) # copy and rename the needed files into calc
   runSS3(wkdir,exec=executable[iter],calcdir="calc/")
   destination <- paste0(store,analysis,"/")
   print(paste0("Running from ",destination))
   storeresults(calc,destination)
   fileout <- filenametoPath(destination,paste0(analysis,".txt"))
   sink(fileout,split=TRUE)
   Btarget <- 0.48; Blimit  <- 0.20
   plotreport  <- SS_output(dir=destination, covar=TRUE,
                            forecast=FALSE, cormax=0.95, cormin=0.01,
                            printhighcor=50)
   finalplot <- SS_plots(replist=plotreport, btarg=Btarget,
                         minbthresh=Blimit,
                         uncertainty=TRUE, plot=1:24,datplot=TRUE,
                         forecastplot=FALSE, png=TRUE)
   sink()  # close off fileout containing screen dump from SS_output and SS_plots
   filename <- filenametoPath(destination,paste0("plotreport_",analysis,".Rdata"))
   save(plotreport,file=filename)
   cat("SS_output and SS_plots txt sent to ",fileout,"\n\n")
   setwd(wkdir)
   print(summarizeSS3(plotreport))
   cat("\n")
   likes <- plotreport$likelihoods_used
   pickrows <- c(8,9,11,2,1,4,5,6)
   result[,stp] <- c(likes[pickrows,"values"],plotreport$current_depletion,
                   plotreport$SBzero,(1-plotreport$last_years_SPR))
   control <- readLines(ctrlfile)

   # Fix Composition variances ------
   outA <- getadjust(control,plotreport)
   varadj <- outA$varadj
   newcontrol <- outA$newcontrol
   # outA <- fixAgeCompVar(control,plotreport,fleets=ageflt)
   # ansAge <- outA$agevar
   # newcontrol <- outA$outcontrol
   # Fix recruitment -----
   out <- fixRecruit(newcontrol,plotreport,sigRLim = c(0.25,0.7))  # change the sigmaLim value
   recdat <- out$recdat
   newcontrol <- out$outcontrol
   # pickR <- which(abs(recdat[,"%Diff"]) > 1)
   # if (length(pickR) == 0) break
   cat("\n")
   endtime <- Sys.time()
   cat("End of step ",stp,"   ",(endtime - starttime),"\n\n")
   print(round(result,5))
   cat("\n\n")
   print(varadj)
   maxadj <- max(abs(varadj[,"%Change"]),na.rm=TRUE)
   cat("\n Maximum % Change ",maxadj,"\n\n")
   print(recdat)   # merely identifying where in the code to change things
   maxrecadj <- max(abs(recdat[,"%Diff"]),na.rm=TRUE)
   cat("\n Maximum %Diff ",maxrecadj,"\n")
   maxrecabs <- max(abs(recdat[,"absDiff"]))
   cat("\n Maximum Absolute Diff ",maxrecabs,"\n\n")
   # change these numbers (the 1.5 and the 0.75 to alter the stopping rule)
   # the 1.5% relates to the % difference in parameters
   # the 0.75 relates to the absolute difference in teh recruitment years,
   # which can be small % but large numebrs of years.
   varadjfin <- any(abs(varadj[,"%Change"]) > 1.5)
   recfin <- ((any(abs(recdat[,"%Diff"]) > 1.5)) |
                 (maxrecabs > 0.75))
   if (((!varadjfin) & (!recfin)) | (stp >= steps)) {
      cat("All adjustments < 1.5%, iterations stopped early \n")
      cat("Control file not updated further. \n\n")
      break
   }
   write(newcontrol,file=ctrlfile)
}

round(result,5)


# L -----
# Get Likelihood results -----


wkdir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/"
setwd(wkdir)

store <- paste0(wkdir,"basecase/")  # define directories

basecase <- c("origbase",       ## 1 repeat previous results
              "origbalance",    ## 2 rebalance the original
              "translated",     ## 3 Convert origbalance to SS3.3
              "addcatches",     ## 4 add the recent catches
              "addsurvey",      ## 5 add the recent survey and recalibrated surveys
              "addnewage",      ## 6 add the recent ageing data
              "ageingerror",    ## 7 add the ageing errir matrix
              "extendrec",      ## 8 extend the number of recruitment residuals estimated
              "basecase17")     # 9 the new rebalanced basecase

basecase <- c("yearSGbase",
              "yearGSVbase")

numdirs <- length(basecase)
columns <- c("EndYear","Depletion","Bzero","1-SPR","TotalL","Index",
             "LengthCompL","AgeCompL","Recruit","Bcurr")
results <- NULL
# now safely generate the directories
for (direct in 1:numdirs) { # direct <- 1
   analysis <- getCase(index=direct,basecase,printout = FALSE)
   destination <- paste0(store,analysis,"/")
   filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
   load(filename)
   outans <- summarizeSS3(plotreport)
   results <- cbind(results,outans$likes[,"values"])
}
results[,"Bcurr"] <- results[,"Depletion"] * results[,"Bzero"]


results[,c(1,2,3,5,6,8,9,10)]






# SpB trajectories --------
#' Plot the spawning biomass trajectories obtained from eahc of the plotreports
#' under the timeseries objects

wkdir <- pathtopath(getDBdir(),"projects/SA_SS3_Garfish/sagarfish/")

store <- pathtopath(wkdir,"basecase/")  # define directories

#

properties(timser)

analyses <- analysis#c("yearSGbase","yearGSVbase")    

pickcols <- c(2,3,5,7,8,14,20,21)

numdirs <- length(analyses)
years <- 1984:2024
nyrs <- length(years)
Sbio <- matrix(NA,nrow=nyrs,ncol=numdirs,dimnames=list(years,analyses))
deplet <- matrix(NA,nrow=nyrs,ncol=numdirs,dimnames=list(years,analyses))
recruit <- matrix(NA,nrow=nyrs,ncol=numdirs,dimnames=list(years,analyses))



for (direct in 1:numdirs) { # direct <- 1
   analysis <- getCase(index=direct,analyses,printout = FALSE)
   destination <- pathtopath(store,analysis)
   filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
   load(filename)
   tims <- plotreport$timeseries[,pickcols]
   pickY <- which(tims$Era == "TIME")
   Sbio[,direct] <- tims[pickY,"SpawnBio"]
   deplet[,direct] <- tims[pickY,"SpawnBio"]/tims[1,"SpawnBio"]
   recruit[,direct] <- tims[pickY,"Recruit_0"]
   
}

Sbio

deplet

recruit
pickY <- which(timser$Era == "TIME")
pickC <- grep("obs_cat",colnames(timser))
catch <- rowSums(timser[pickY,pickC],na.rm=TRUE)

harvest <- plotreport$exploitation

plotprep(width=8,height=9)
plot1(harvest$Yr[2:40],harvest$F_std[2:40],type="p",cex=1.25)
lines(harvest$Yr[2:40],harvest$F_std[2:40],lwd=1,lty=2)

pickscen <- c(1,3,5,8,9)
nline <- length(pickscen)
par(mfrow=c(3,1),mai=c(0.3,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=1.0, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
ymax <- max(results,na.rm=TRUE) * 1.05
plot(years,results[,1],type="n",xlab="",ylab="",ylim=c(0,ymax),yaxs="i",xaxt="n")
xinc <- seq(1980,2015,5)
axis(side=1,at=xinc,labels=xinc,cex=1.5)
grid()
for (i in 1:nline) lines(years,results[,pickscen[i]],lwd=3,col=i)
points(range(years),c(results[1,9],results[40,9]),pch=16,cex=1.25,col=2)
legend(2000,45000,basecase[pickscen],col=c(1:nline),lwd=4,bty="n",cex=1.1)
title(ylab=list("Female Spawning Biomass (t)", cex=1.0, font=7))

ymax <- max(deplet,na.rm=TRUE) * 1.05
plot(years,deplet[,1],type="n",xlab="",ylab="",ylim=c(0,ymax),yaxs="i",xaxt="n")
axis(side=1,at=xinc,labels=xinc,cex=1.5)
grid()
for (i in 1:nline) lines(years,deplet[,pickscen[i]],lwd=3,col=i)
points(range(years),c(deplet[1,9],deplet[40,9]),pch=16,cex=1.25,col=2)
abline(h=c(0.2,0.48),col=2,lty=2)
title(ylab=list("Spawning Biomass Depletion", cex=1.0, font=7))

# select from 1992 onwards
deplet2 <- deplet[16:40,]
yrs <- as.numeric(rownames(deplet2))
ymax <- max(deplet2,na.rm=TRUE) * 1.05
plot(yrs,deplet2[,1],type="n",xlab="",ylab="",ylim=c(0,ymax),yaxs="i",xaxt="n")
xinc2 <- seq(1993,2017,4)
axis(side=1,at=xinc2,labels=xinc2,cex=1.5)
grid()
for (i in 1:nline) lines(yrs,deplet2[,pickscen[i]],lwd=3,col=i)
lines(yrs,deplet2[,9],lwd=5,col=5)
points(range(yrs),c(deplet2[1,9],deplet2[25,9]),pch=16,cex=1.25,col=2)
abline(h=c(0.2,0.48),col=2,lty=2)
legend(1997,0.38,basecase[pickscen],col=c(1:nline),lwd=4,bty="n",cex=1.1)
title(ylab=list("Spawning Biomass Depletion", cex=1.0, font=7))



# SPR vs F -----


wkdir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/"
setwd(wkdir)

store <- paste0(wkdir,"basecase/")  # define directories

basecase <- c("origbase",       ## 1 repeat previous results
              "origbalance",    ## 2 rebalance the original
              "translated",     ## 3 Convert origbalance to SS3.3
              "addcatches",     ## 4 add the recent catches
              "addsurvey",      ## 5 add the recent survey and recalibrated surveys
              "addnewage",      ## 6 add the recent ageing data
              "ageingerror",    ## 7 add the ageing errir matrix
              "extendrec",      ## 8 extend the number of recruitment residuals estimated
              "basecase17")     # 9 the new rebalanced basecase


numdirs <- length(basecase)
years <- 1978:2017
results <- matrix(NA,nrow=40,ncol=numdirs,dimnames=list(years,basecase))
deplet <- matrix(NA,nrow=40,ncol=numdirs,dimnames=list(years,basecase))


direct <- 9
analysis <- getCase(index=direct,basecase,printout = FALSE)
destination <- paste0(store,analysis,"/")
filename <- filenametoPath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)

yield <- plotreport$equil_yield
columns <- colnames(yield)
columns[13] <- "East_1"
colnames(yield)=columns
properties(yield)
pickcols <- c(3,4,5,6,8,9,10,11)
yield[1:20,pickcols]

plotprep(width=9,height=9)
pairs(yield[,pickcols])

# plot the graphs of yield vs Depletion and yield vs SSB
plotprep(width=9,height=6)
par(mfcol=c(2,2),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
catch <- yield[,"Tot_Catch"]
deplet <- yield[,"Depletion"]
ssb <- yield[,"SSB"]
Fval <- yield[,"F_report"]
spr <- yield[,"SPR"]
pickmsy <- which.max(catch)
msy <- catch[pickmsy]
pick48 <- which.closest(0.48,deplet)
pickpF <- which(Fval <= 0.0425)

plot(deplet,catch,type="l",ylim=c(0,msy*1.05),yaxs="i",xlab="",ylab="",lwd=2)
abline(h=msy,col=2,lty=2,lwd=2)
abline(v=c(deplet[pickmsy],0.2,0.48),col=c(2,3,3),lty=2,lwd=c(2,2,2))
title(ylab=list("Surplus Production (t)", cex=1.0, font=7),
      xlab=list("Spawning Biomass Depletion", cex=1.0, font=7))

plot(ssb,catch,type="l",ylim=c(0,msy*1.05),yaxs="i",xlab="",ylab="",lwd=2)
abline(h=msy,col=2,lty=2,lwd=2)
abline(v=c(ssb[pickmsy]),col=c(2),lty=2,lwd=c(2))
title(ylab=list("Surplus Production (t)", cex=1.0, font=7),
      xlab=list("Spawning Biomass (t)", cex=1.0, font=7))

plot(Fval[pickpF],catch[pickpF],type="l",ylim=c(0,msy*1.05),yaxs="i",
     xlab="",ylab="",lwd=2)
abline(v=Fval[pickmsy],col=c(2),lty=2,lwd=c(2))
title(ylab=list("Surplus Production (t)", cex=1.0, font=7),
      xlab=list("Annual Harvest Rate", cex=1.0, font=7))

ymax <- max((1-spr[pickpF]),na.rm=TRUE) * 1.05
plot(Fval[pickpF],(1-spr[pickpF]),type="l",lwd=3,ylim=c(0,ymax),yaxs="i",
     xlab="",ylab="")
abline(v=Fval[pickmsy],col=c(2),lty=2,lwd=c(2))
title(ylab=list("1 - SPR", cex=1.0, font=7),
      xlab=list("Annual Harvest Rate", cex=1.0, font=7))



{
cat("MSY  = ",msy,"\n")
cat("MEY  = ",catch[pick48],"\n")
cat("Dmsy = ",deplet[pickmsy],"\n")
cat("Fmsy = ",Fval[pickmsy],"\n")
cat("Bmsy = ",ssb[pickmsy],"\n")
}


# Dynamic SPR -----------


wkdir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/"
setwd(wkdir)

store <- paste0(wkdir,"basecase/")  # define directories

basecase <- c("origbase",       ## 1 repeat previous results
              "origbalance",    ## 2 rebalance the original
              "translated",     ## 3 Convert origbalance to SS3.3
              "addcatches",     ## 4 add the recent catches
              "addsurvey",      ## 5 add the recent survey and recalibrated surveys
              "addnewage",      ## 6 add the recent ageing data
              "ageingerror",    ## 7 add the ageing errir matrix
              "extendrec",      ## 8 extend the number of recruitment residuals estimated
              "basecase17")     # 9 the new rebalanced basecase

numdirs <- length(basecase)
years <- 1978:2017
results <- matrix(NA,nrow=40,ncol=numdirs,dimnames=list(years,basecase))
deplet <- matrix(NA,nrow=40,ncol=numdirs,dimnames=list(years,basecase))


direct <- 9
analysis <- getCase(index=direct,basecase,printout = FALSE)
destination <- paste0(store,analysis,"/")
filename <- filenametoPath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)


spr <- plotreport$sprseries
colnames(spr)
properties(spr)
pickcols <- c(1,2,3,5,7,8,12,26:29)
round(spr[,pickcols],4)






# RecruitmentDeviates -----


wkdir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/"
setwd(wkdir)
store <- paste0(wkdir,"basecase/")  # define directories
basecase <- c("origbase",       ## 1 repeat previous results
              "origbalance",    ## 2 rebalance the original
              "translated",     ## 3 Convert origbalance to SS3.3
              "addcatches",     ## 4 add the recent catches
              "addsurvey",      ## 5 add the recent survey and recalibrated surveys
              "addnewage",      ## 6 add the recent ageing data
              "ageingerror",    ## 7 add the ageing errir matrix
              "extendrec",      ## 8 extend the number of recruitment residuals estimated
              "basecase17")     # 9 the new rebalanced basecase
numdirs <- length(basecase)
years <- 1905:2017
nyr <- length(years)
recruit <- matrix(NA,nrow=nyr,ncol=numdirs,dimnames=list(years,basecase))
recruit2 <- matrix(NA,nrow=nyr,ncol=numdirs,dimnames=list(years,basecase))
recruitdev <- matrix(NA,nrow=nyr,ncol=numdirs,dimnames=list(years,basecase))


for (direct in 1:numdirs) { # direct <- 9
   analysis <- getCase(index=direct,basecase,printout = FALSE)
   destination <- paste0(store,analysis,"/")
   filename <- filenametoPath(destination,paste0("plotreport_",analysis,".Rdata"))
   load(filename)
   pickY <- match(plotreport$recruit$Yr,years)
   recruitdev[pickY,direct] <- plotreport$recruit[pickY,"dev"]
   recruit[pickY,direct] <- plotreport$recruit[pickY,"pred_recr"]
   recruit2[pickY,direct] <- plotreport$recruit[pickY,"bias_adjusted"]
}
recruit

getminY <- function(x,mult=1.05) {
   ymin <- min(recruit,na.rm=TRUE)
   if (ymin < 0) {
      ymin <- ymin * mult
   } else {
      ymin <- ymin * (2 - mult)
   }
   return(ymin)
}


plotrecruits <- function(inrec,legx,legy,pickyrs,ylabel,newplot=TRUE) {
   if (newplot) {
      plotprep(width=7,height=5)
      par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.1),oma=c(0.0,0,0.0,0.0))
      par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
   }
   ymax <- max(inrec[pickyrs,],na.rm=TRUE) * 1.05
   ymin <- getminY(inrec[pickyrs,],mult=1.1)
   years <- as.numeric(rownames(inrec[yrs,]))
   plot(years,inrec[pickyrs,numdirs],type="n",ylim=c(2000,ymax),yaxs="i",
        xlab="",ylab="")
   abline(h=inrec[1,numdirs],lwd=1,col=1,lty=2)
   lines(years,inrec[pickyrs,1],lwd=2,col=4)
   for (i in 2:numdirs) lines(years,inrec[pickyrs,i],lwd=1,col=1)
   lines(years,inrec[pickyrs,9],lwd=3,col=2)
   title(ylab=list(ylabel, cex=1.0, font=7),
         xlab=list("Year", cex=1.0, font=7))
   legend(legx,legy,c("origbase","basecase17","others"),col=c(4,2,1),lwd=3,
          bty="n",cex=1.2)
}

newplot=FALSE
inrec=recruit;
ylabel <- "Age-0 Recruits"
legx <- 1980; legy <- 6000
yrs <- which(plotreport$recruit[,"era"] %in% c("Main","Late","Forecast")); pickyrs=yrs
plotrecruits(recruit,legx,legy,yrs,ylabel,newplot=FALSE)


deriv <- plotreport$derived_quants
pickSPB <- grep("SPB",rownames(deriv))
years <- 1980:2016

tims <- plotreport$timeseries
catch <- tims[2:39,"obs_cat:_1"]

plotprep(width=8,height=6)
par(mfrow=c(2,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)

ylabel <- "Age-0 Recruits"
legx <- 1980; legy <- 6000
yrs <- which(plotreport$recruit[,"era"] %in% c("Main","Late","Forecast"))
plotrecruits(recruit,legx,legy,yrs,ylabel,newplot=FALSE)


ymax2 <- max(catch,na.rm=TRUE) * 1.05
plot(years,catch,type="l",lwd=2,col=1,ylim=c(0,ymax2),yaxs="i",
     xaxt="n",xlab="",ylab="")
xval <- seq(1980,2015,5)
axis(side=1,at=xval,labels = xval,cex=1.0,font=7)
grid()
title(ylab=list("Total Catches (t)", cex=1.0, font=7))






# Get Parameters ------

sort(names(plotreport))

pars <- plotreport$parameters
dim(pars)
properties(pars)
pickcols <- c(1,3,18,19,5:16)
pickrows <- c(26,106,107,147,150,151)
head(pars[,pickcols],25)

pick <- which(pars[,"Phase"] > 0)
pick

pickcols <- c(1,3,18,19,5:16)
pickrows <- c(26,106,107,147,150,151)
pars[pickrows,pickcols]

# derived model values

deriv <- plotreport$derived_quants
pickSPB <- grep("SPB",rownames(deriv))
years <- 1979:2016
spb <- deriv[2:39,"Value"]
B0 <- spb[1]
stdev <- deriv[2:39,"StdDev"]
tims <- plotreport$timeseries
catch <- tims[2:39,"obs_cat:_1"]

plotprep(width=7,height=6)
par(mfrow=c(2,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)

ymax <- max((spb+1.96*stdev),na.rm=TRUE) * 1.05
plot(years,spb,type="l",lwd=2,col=1,ylim=c(0,ymax),yaxs="i",
     xaxt="n",xlab="",ylab="")
xval <- seq(1980,2015,5)
axis(side=1,at=xval,labels = xval,cex=1.0,font=7)
grid()
lines(years,(spb+1.96*stdev),lwd=1,col=2)
lines(years,(spb-1.96*stdev),lwd=1,col=2)
abline(h=c((0.2 * B0),(0.48*B0)),col=3,lwd=2,lty=2)
title(ylab=list("Female Spawning Biomass (t)", cex=1.0, font=7))

ymax2 <- max(catch,na.rm=TRUE) * 1.05
plot(years,catch,type="l",lwd=2,col=1,ylim=c(0,ymax2),yaxs="i",
     xaxt="n",xlab="",ylab="")
xval <- seq(1980,2015,5)
axis(side=1,at=xval,labels = xval,cex=1.0,font=7)
grid()
title(ylab=list("Total Catches (t)", cex=1.0, font=7))


cbind(years,spb,stdev,catch)



plotreport$Data_File







sort(names(plotreport))

# age Composition -------

library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
library(makehtml)
ddir <- getDBdir()
wkdir <- pathtopath(ddir,"/A_CodeR/SA-SS3/")
source(pathtopath(wkdir,"ss3_utilities.R")) 
options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)
store <- pathtopath(wkdir,"HAB/garfish/")  # define directories
calc <- pathtopath(wkdir,"HAB/calc/")
profile <- pathtopath(wkdir,"HAB/profile/")
# generate these two directories
dirExists(store)
dirExists(calc)
dirExists(profile)
SS3path <- pathtopath(calc,"ss3.exe")
if (endpart(store) == "garfish") { # GARFISH BASECASE  ----------------
  basecase <- c("GAR_SG_mh",
                "GAR_GSV_DEV")    # Natural Morality = 0.6
} else {
  stop("Are you in the wrong directory? \n")
}
numdirs <- length(basecase)
# now safely generate the directories
for (direct in 1:numdirs) dirExists(paste0(store,basecase[direct]))
printV(basecase)
item <- 2
analysis <- getCase(index=item,basecase)  # 
cat("\n\n")
print("New Analysis")
print(analysis)
destination <- paste0(store,analysis,"/")
print(destination)
extradir <- pathtopath(destination,"extra")

load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  

source(pathtopath(wkdir,"ss3_utilities.R")) 


sort(names(plotreport))
times <- plotreport$timeseries
properties(times)
pickcols <- c(2,3,5,7,8,9,14,16,22,24,30,32,38,40,46,48,54)
head(times[,pickcols])
picksel <- grep("sel(B)",colnames(times),fixed=TRUE)
times[,picksel] <- round(times[,picksel],3)
times[,c(2,3,5,7,9,picksel)]


cpue <- plotreport$cpue
properties(cpue)  # from codeutils
pickcols <- c(1,2,4,9,10:18)
cpue[,pickcols]


# do females

compdat <- getalcomp(store=store,analysis=analysis)
str1(compdat)
compfem <- compdat$compfem
compmal <- compdat$compmal
fleets <- compdat$fleets

alout <- plotagecomp(compsex=compfem,gender="Female",fleet=fleets[1],
                     rescale=0.6,console=TRUE,plotdir="",plotout=TRUE)

alfem <- alout$alcomp

plotagelen(alcomp=alfem,gender="Female",fleet=fleets[1],console=TRUE,
           plotdir="")


# do males

alout <- plotagecomp(compsex=compmal,gender="Male",fleet=fleets[1],
                     rescale=0.6,console=TRUE,plotdir="",plotout=TRUE)




  
alcomp <- alout$alcomp

console=TRUE
gender="Female"
fleet <- fleetnames[1] 

plotagelen(alcomp=alcomp,gender="Female",fleet=fleetnames[1],console=TRUE,
           plotdir="")



if (!console) {
  addplot(filen=filename,rundir=extradir,category="LenComp",
          caption=paste0("Comparison of Fit to Len Comps aggregated ",
                         "by Year and ",flname,".")) 
}








agebins <- plotreport$agebins
years <- c(1992,1995,1999,2001,2004,2010,2012,2016)
sexes <- c(1,2)
pickage <- 15:80

# prepare data for comparison
 
label <- c("survey30","survey35","survey40")
ncomp <- length(label)
fem <- vector("list",ncomp); names(fem) <- label
mal <- vector("list",ncomp); names(mal) <- label

for (index in 1:ncomp) { # index = 1
   analysis <-  label[index]
   tmpdest <- paste0(store,analysis,"/")
   filename <- filenametoPath(tmpdest,paste0("plotreport_",analysis,".Rdata"))
   load(filename)
   tmpage <- plotreport$agedbase
   pickF <- which(tmpage[,"Sex"] == 1)
   fem[[index]] <- tmpage[pickF,]
   pickM <- which(tmpage[,"Sex"] == 2)
   mal[[index]] <- tmpage[pickM,]
} 


# do females first

plotprep(width=9,height=9,newdev=FALSE)
par(mfcol=c(4,4),mai=c(0.25,0.2,0.05,0.05),oma=c(1.0,1,1.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
ymax <- max(fem[[1]][,c("Obs","Exp")],na.rm=TRUE) * 1.05
for (yr in 1:8) {  # yr=1
   pickyr <- which(fem[[1]][,"Yr"] == years[yr])
   x <- fem[[1]][pickyr,"Bin"]
   y <- fem[[1]][pickyr,"Obs"]
   y1 <- fem[[1]][pickyr,"Exp"]
   y2 <- fem[[2]][pickyr,"Exp"]
   y3 <- fem[[3]][pickyr,"Exp"]
   plot(x[pickage],y[pickage],type="l",lwd=3,col=1,
        xlab="",ylab="",ylim=c(0,0.085),yaxs="i")
  # lines(x[pickage],y1[pickage],lwd=4,col=2,lty=3)
   lines(x[pickage],y1[pickage],lwd=2,col=2,lty=1)
   lines(x[pickage],y2[pickage],lwd=2,col=3,lty=1)
   lines(x[pickage],y3[pickage],lwd=2,col=4,lty=1)
   mtext(years[yr],side=3,outer=F,line=-1.1,font=7,cex=1.1)
   
   if (yr %in% c(1,5)) mtext("Female",side=3,outer=F,line=0.0,font=7,cex=1.0)
}
ymax <- max(mal[[1]][,c("Obs","Exp")],na.rm=TRUE) * 1.05
for (yr in 1:8) {
   pickyr <- which(mal[[1]][,"Yr"] == years[yr])
   x <- mal[[1]][pickyr,"Bin"]
   y <- mal[[1]][pickyr,"Obs"]
   y1 <- mal[[1]][pickyr,"Exp"]
   y2 <- mal[[2]][pickyr,"Exp"]
   y3 <- mal[[3]][pickyr,"Exp"]
   plot(x[pickage],y[pickage],type="l",lwd=3,col=1,
        xlab="",ylab="",ylim=c(0,0.085),yaxs="i")
  # lines(x[pickage],y1[pickage],lwd=4,col=2,lty=3)
   lines(x[pickage],y1[pickage],lwd=2,col=2,lty=1)
   lines(x[pickage],y2[pickage],lwd=2,col=3,lty=1)
   lines(x[pickage],y3[pickage],lwd=2,col=4,lty=1)
   mtext(years[yr],side=3,outer=F,line=-1.1,font=7,cex=1.1)
   if (yr %in% c(1,5)) mtext("Male",side=3,outer=F,line=0.0,font=7,cex=1.0)
}
mtext("Ages",side=1,outer=T,line=0.0,font=7,cex=1.0)
mtext("Proportion at Age",side=2,outer=T,line=0.0,font=7,cex=1.0)



# Profile on M -------

library(r4sessf)
library(r4ss)
library(knitr)
library(codeutils)
options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

wkdir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/"
setwd(wkdir)
storeorig <- paste0(wkdir,"basecase/")  # define directories
calc <- paste0(wkdir,"calc/")
dirExists(store)
dirExists(calc)

source("oro2017utils.r")

profiledir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/profile/"

copyto(storeorig,"basecase17","profileM",useorigin=FALSE,neworigin=profiledir)

store <- profiledir

profile <- c("profileM",        # Profile natural mortality
             "profileLnR0")     # Profile on the Log of R0

analysis <- profile[1]

#' READ THIS:
#' An important point is that natural mortality is identified in two places
#' one for females using 'NatM_p_1_Fem_GP_1' as a line identifier and
#' one for males using   'NatM_p_1_Mal_GP_1' as a line identifier. The GP_1
#' relates to the growth parameters and the 1 presumably relates to the number
#' of growth time or other blocks one might have.
#' The important bit is that the following code is set up for one sex and one
#' block. If you have different GROWTH IN TWO BLOCKS for a one sex model or
#' the same growth but ESTIMATE NATURAL MORTALITY FOR EACH SEX, this would
#' imply THAT THIS CODE WOULD NEED MODIFICATION!
#'
profvalues <- seq(0.023,0.047,0.001)
nprof <- length(profvalues)
identifier <- "NatM_p_1_Fem_GP_1"
varpos <- 3
phasepos <- 7

profileans <- vector("list",nprof)
names(profileans) <- profvalues
outputdir <- filenametoPath(store,analysis)
outputfile <- filenametoPath(outputdir,paste0(analysis,".RData"))

starttime <- Sys.time()

i <- 1
print(paste0(analysis,"    ",profvalues[i]))
copyfiles(analysis,store,calc) # copy and rename the needed files into calc
control <- changeCtl(identifier,profvalues[i],ctlfile="ss3.ctl",calc,pos=varpos,pos2=phasepos)
write(control,file=paste0(calc,"ss3.ctl"))

setwd(calc)

command <- paste0(calc,"ss3.exe -nohess")
command1 <- paste0(command," > ",paste0(calc,"1rundetails.txt"))
command2 <- paste0(command," > ",paste0(calc,"2rundetails.txt"))
shell(command1,wait=TRUE,invisible=T)
cat("\n\n")
cat("End of first -nohess Run  \n")
fixstarter(calc,toscreen=F)
cat("starter.ss file modified to use the .par file \n\n")
shell(command2,wait=TRUE,invisible=F)
cat("\n  End of second -nohess Run  \n")
# fileout <- filenametoPath(outputdir,paste0(analysis,"_",profvalues[i],".txt"))
# sink(fileout)
replist <- SS_output(dir=calc, covar=FALSE,
                     forecast=FALSE, cormax=0.95, cormin=0.01,
                     printhighcor=50,verbose=FALSE)
# sink()
profileans[[i]] <- replist
summarizeSS3(replist)
for (i in 2:nprof) {
   print(paste0(analysis,"    ",profvalues[i]))
   copyfiles(analysis,store,calc) # copy and rename the needed files into calc
   control <- changeCtl(identifier,profvalues[i],ctlfile="ss3.ctl",
                        calc,pos=varpos,pos2=phasepos)
   write(control,file=paste0(calc,"ss3.ctl"))
   changePar(profvalues[i],calc)
   shell(command1,wait=TRUE,invisible=F)
   cat("\n\n")
   cat("End of first -nohess Run  \n")
   fixstarter(calc,toscreen=F)
   shell(command2,wait=TRUE,invisible=F)
   cat("\n  End of second -nohess Run  \n")
   # fileout <- filenametoPath(outputdir,paste0(analysis,"_",profvalues[i],".txt"))
   # sink(fileout)
   replist <- SS_output(dir=calc, covar=FALSE,
                     forecast=FALSE, cormax=0.95, cormin=0.01,
                     printhighcor=50,verbose=FALSE)
   # sink()
   profileans[[i]] <- replist
   endtime <- Sys.time()
   print(endtime - starttime)
   print(summarizeSS3(replist))
}


save(profileans,file=outputfile)

str(profileans,max.level=1)

load(outputfile)

columns <- c("EndYear","Depletion","Bzero","1-SPR","TotalL","Index",
             "LengthCompL","AgeCompL","Recruit")

results <- matrix(0,nrow=nprof,ncol=length(columns),
                  dimnames=list(profvalues,columns))
for (i in 1:nprof) {
   results[i,] <- summarizeSS3(profileans[[i]])
}
results
LL <- results[,"TotalL"]
optL <- min(LL)
L95 <- optL + 1.92

plotprep(width=7,height=7)
pairs(results[,c(2,3,4,5)])

plotprep()
plot(profvalues,LL,type="b",lwd=2,xlab="",ylab="")
abline(h=L95,col=2,lwd=2)
title(ylab=list("Total Likelihood", cex=1.0, font=7),
      xlab=list("Natural Mortality M", cex=1.0, font=7))


plotprep(width=7,height=8,plots=c(4,1))

fontn <- 7 #"serif"
if (names(dev.cur()) %in% c("null device","RStudioGD"))
   dev.new(width=7,height=12,noRStudioGD = TRUE)
graphfile <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/profile_on_M.png"
if (file.exists(graphfile)) file.remove(graphfile)
png(filename=graphfile,width=150,height=180,units="mm",res=300)#,family=7)
par(mfrow=c(4,1),mai=c(0.25,0.25,0.05,0.05),oma=c(1.0,1.0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=fontn,font=fontn,font.lab=fontn)
likes <- c(5,6,8,9)
label <- colnames(results)[likes]
for (i in 1:4) {
   yval <- results[,likes[i]]
   plot(profvalues,yval,type="p",pch=16,col=1,xlab="",ylab="")
   lines(profvalues,yval,lwd=1,col=2)
   abline(v=c(0.031,0.04),lwd=2,col=c("grey","green"))
   mtext(label[i],side=3,outer=F,line=-1.1,font=fontn,cex=1.2)
   if (i == 1) abline(h=L95,col=4,lwd=1)
}
mtext("negative Log-Likelihood",side=2,outer=T,line=0.0,font=fontn,cex=1.2)
mtext("Natural Mortality M",side=1,outer=T,line=0.0,font=fontn,cex=1.0)
legend(0.0225,4.5,c("Assumed M","Optimum -veLL M","95% CI"),
       col=c(3,"grey",4),lwd=3,bty="n",cex=1.0)
dev.off()


cbind(LL,rowSums(results[,c(6,8,9)]),LL-rowSums(results[,c(6,8,9)]))


for (i in 2:4) lines(profvalues,scaledL[,i],lwd=2,col=i)
title(ylab=list("Total Likelihood", cex=1.0, font=7),
      xlab=list("Natural Mortality M", cex=1.0, font=7))
abline(v=0.031,lwd=2,col=5)


# Profile on LnR0 -------

library(r4sessf)
library(r4ss)
library(knitr)
library(codeutils)
options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)
wkdir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/"
setwd(wkdir)
storeorig <- paste0(wkdir,"basecase/")  # define directories
calc <- paste0(wkdir,"calc/")
dirExists(store)
dirExists(calc)
source("oro2017utils.r")

profiledir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/profile/"

copyto(storeorig,"basecase17","profileLnR0",useorigin=FALSE,neworigin=profiledir)

store <- profiledir

profile <- c("profileM",        # Profile natural mortality
             "profileLnR0")     # Profile on the Log of R0

analysis <- profile[2]
#' READ THIS:
#' An important point is that natural mortality is identified in two places
#' one for females using 'NatM_p_1_Fem_GP_1' as a line identifier and
#' one for males using   'NatM_p_1_Mal_GP_1' as a line identifier. The GP_1
#' relates to the growth parameters and the 1 presumably relates to the number
#' of growth time or other blocks one might have.
#' The important bit is that the following code is set up for one sex and one
#' block. If you have different GROWTH IN TWO BLOCKS for a one sex model or
#' the same growth but ESTIMATE NATURAL MORTALITY FOR EACH SEX, this would
#' imply THAT THIS CODE WOULD NEED MODIFICATION!
#'
#'  optimum LnR0 = 9.12357799388
#'  Needed to modify the phases of the assessment
#'  as only LnR0 was being solved in Phase 1
profvalues <- seq(8.9,9.26,0.02)
nprof <- length(profvalues)
identifier <- "SR_LN(R0)"
varpos <- 3
phasepos <- 7

profileans <- vector("list",nprof)
names(profileans) <- profvalues
outputdir <- filenametoPath(store,analysis)
outputfile <- filenametoPath(outputdir,paste0(analysis,".RData"))

starttime <- Sys.time()

i <- 1
print(paste0(analysis,"    ",profvalues[i]))
copyfiles(analysis,store,calc) # copy and rename the needed files into calc
control <- changeCtl(identifier,profvalues[i],ctlfile="ss3.ctl",calc,pos=varpos,pos2=phasepos)
write(control,file=paste0(calc,"ss3.ctl"))

setwd(calc)

command <- paste0(calc,"ss3.exe -nohess")
command1 <- paste0(command," > ",paste0(calc,"1rundetails.txt"))
command2 <- paste0(command," > ",paste0(calc,"2rundetails.txt"))
shell(command1,wait=TRUE,invisible=T)
cat("\n\n")
cat("End of first -nohess Run  \n")
fixstarter(calc,toscreen=F)
cat("starter.ss file modified to use the .par file \n\n")
shell(command2,wait=TRUE,invisible=F)
cat("\n  End of second -nohess Run  \n")
# fileout <- filenametoPath(outputdir,paste0(analysis,"_",profvalues[i],".txt"))
# sink(fileout)
replist <- SS_output(dir=calc, covar=FALSE,
                     forecast=FALSE, cormax=0.95, cormin=0.01,
                     printhighcor=50,verbose=FALSE)
# sink()
profileans[[i]] <- replist
summarizeSS3(replist)
for (i in 2:nprof) {
   print(paste0(analysis,"    ",profvalues[i]))
   copyfiles(analysis,store,calc) # copy and rename the needed files into calc
   control <- changeCtl(identifier,profvalues[i],ctlfile="ss3.ctl",
                        calc,pos=varpos,pos2=phasepos)
   write(control,file=paste0(calc,"ss3.ctl"))
   changePar(profvalues[i],calc)
   shell(command1,wait=TRUE,invisible=F)
   cat("\n\n")
   cat("End of first -nohess Run  \n")
   fixstarter(calc,toscreen=F)
   shell(command2,wait=TRUE,invisible=F)
   cat("\n  End of second -nohess Run  \n")
   # fileout <- filenametoPath(outputdir,paste0(analysis,"_",profvalues[i],".txt"))
   # sink(fileout)
   replist <- SS_output(dir=calc, covar=FALSE,
                        forecast=FALSE, cormax=0.95, cormin=0.01,
                        printhighcor=50,verbose=FALSE)
   # sink()
   profileans[[i]] <- replist
   endtime <- Sys.time()
   print(endtime - starttime)
   print(summarizeSS3(replist))
}


save(profileans,file=outputfile)

str(profileans,max.level=1)

columns <- c("EndYear","Depletion","Bzero","1-SPR","TotalL","Index",
             "LengthCompL","AgeCompL","Recruit")

results <- matrix(0,nrow=nprof,ncol=length(columns),
                  dimnames=list(profvalues,columns))
for (i in 1:nprof) {
   results[i,] <- summarizeSS3(profileans[[i]])
}
results
LL <- results[,"TotalL"]
optL <- min(LL)
L95 <- optL + 1.92
pickmin <- which.min(LL)

plotprep(width=7,height=7)
pairs(results[,c(2,3,4,5)])

plotprep()
plot(profvalues,LL,type="b",lwd=2,xlab="",ylab="")
abline(h=L95,col=2,lwd=2)

title(ylab=list("Total Likelihood", cex=1.0, font=7),
      xlab=list("Natural Logarithm R0", cex=1.0, font=7))

plotprep(width=7,height=8,plots=c(4,1))
savefile <- TRUE
if (savefile) {
   fontn <- 7 #"serif"
   if (names(dev.cur()) %in% c("null device","RStudioGD"))
      dev.new(width=7,height=12,noRStudioGD = TRUE)
   graphfile <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/profile_on_LnR0.png"
   if (file.exists(graphfile)) file.remove(graphfile)
}
png(filename=graphfile,width=150,height=180,units="mm",res=300)#,family=7)
par(mfrow=c(4,1),mai=c(0.25,0.25,0.05,0.05),oma=c(1.0,1.0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=fontn,font=fontn,font.lab=fontn)
likes <- c(5,6,8,9)
label <- colnames(results)[likes]
for (i in 1:4) { # i = 1
   yval <- results[,likes[i]]
   plot(profvalues,yval,type="p",pch=16,col=1,xlab="",ylab="")
   lines(profvalues,yval,lwd=1,col=2)
   abline(v=c(9.08,9.124),lwd=2,col=c("grey","green"))
   mtext(label[i],side=3,outer=F,line=-1.1,font=fontn,cex=1.2)
   if (i == 1) abline(h=L95,col=4,lwd=1)
}
mtext("negative Log-Likelihood",side=2,outer=T,line=0.0,font=fontn,cex=1.2)
mtext("Natural Logarithm R0",side=1,outer=T,line=0.0,font=fontn,cex=1.0)
legend(8.9,7,c("Fitted LnR0","Optimum -veLL LnR0","95% CI"),
       col=c(3,"grey",4),lwd=3,bty="n",cex=1.0)
if (savefile) dev.off()


cbind(LL,rowSums(results[,c(6,8,9)]),LL-rowSums(results[,c(6,8,9)]))


plotprep()
yval <- results[,likes[1]]
plot(profvalues,yval,type="p",pch=16,col=1,xlab="",ylab="")
lines(profvalues,yval,lwd=1,col=2)
abline(v=c(9.08,9.124),lwd=2,col=c("grey","green"))


# make LOO directory------
storeorig <- paste0(wkdir,"basecase/")

loodir <- "C:/A_CSIRO/Rcode/SESSF/ss3/oro2017/loo/"

copyto(storeorig,"basecase17","looage",neworigin=loodir)


# Examine report.sso file----------------------------------


wkdir <- pathtopath(getDBdir(),"projects/SA_SS3_Garfish/sagarfish/")
store <- pathtopath(wkdir,"basecase/")  # define directories
analyses <- c("yearSGbase",
              "yearGSVbase") 
#
i = 1 

analysis <- pathtopath(store,analyses[i])
filename <- pathtopath(analysis,"report.sso")
dat <- readLines(filename)

dat[37:80]

pickL <- grep("TIME_SERIES",dat)
dat[1250:1290]

pars <- dat[(pickL[2]+2):(pickL[2]+27)]
npars <- length(pars)
columns <- c("name","value","Estimated?")
numcol <- length(columns)
pararray <- as.data.frame(matrix(0,nrow=npars,ncol=numcol))
colnames(pararray) <- columns

for (i in 1:npars) {
  outsplit <- removeEmpty(unlist(strsplit(pars[i],split=" ")))
  pararray[i,] <- outsplit[c(2,3,5)] 
}
pararray

# plotreport examination----------------------------------

i = 1 

analysis <- pathtopath(store,analyses[i])

filename <- pathtopath(analysis,paste0("plotreport_",analyses[i],".Rdata"))
load(filename)
  
tmp <- str1(plotreport)

catch <- plotreport$catch
properties(catch)


tims <- plotreport$timeseries
totvulnbio <- tapply(catch$vuln_bio,catch$Yr,sum,na.rm=TRUE)
totdeadbio <- tapply(catch$dead_bio,catch$Yr,sum,na.rm=TRUE)
cbind(totvulnbio,totdeadbio,totdeadbio/totvulnbio)


# Compare CPUE --------------------------------------------------

label <- c("survey30","survey35","survey40")
ncomp <- length(label)
ce <- vector("list",ncomp); names(ce) <- label

for (index in 1:ncomp) { # index = 1
   analysis <-  label[index]
   tmpdest <- paste0(store,analysis,"/")
   filename <- filenametoPath(tmpdest,paste0("plotreport_",analysis,".Rdata"))
   load(filename)
   ce[[index]] <- plotreport$cpue
} 


category <- c("towedbody","hull")
cpue <- ce[[1]]
pickg <- which(cpue[,"Fleet_name"] == category[1])
gear1 <- cpue[pickg,]
years <- gear1[,"Yr"]
nyrs <- length(years)
cr <- matrix(0,nrow=nyrs,ncol=ncomp,dimnames=list(years,label))
se <- matrix(0,nrow=nyrs,ncol=ncomp,dimnames=list(years,label))

for (index in 1:ncomp) { # index=1
  cr[,index] <- ce[[index]][pickg,"Obs"]
  se[,index] <- ce[[index]][pickg,"SE"]
}


# Size-Selectivity
wkdir <- pathtopath(getDBdir(),"projects/SA_SS3_Garfish/sagarfish/")

store <- pathtopath(wkdir,"basecase/")  # define directories
destination <- pathtopath(store,analysis)

basecase <- c("initialSG",
              "initialGSV",
              "yearSG",
              "yearSGagelen",
              "yearSGtimeblock",
              "yearSGe2",
              "yearSGdemo",
              "winsumSG",
              "halfyrSG")   

analysis <- getCase(index=5,basecase,printout = FALSE)
filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)
sel <- plotreport$sizeselex



# get projected catches -------------------------------
total <- compscenes$total

sort(names(total[[1]]))

times <- compscenes$timeseries


catches <- compscenes$catches

printV(colnames(catches[[1]]))

tmp <- total[[1]]$parameters

which(tmp[,"Phase"] > 0)





total <- compscenes$total
columns <- c(2,5,13,14,15,16)
round(total[[1]]$equil_yield[,columns],3)


scenes <- names(total)
nscen <- length(scenes)

cpue <- makelist(scenes)

for (i in 1:nscen) cpue[[i]] <- total[[i]]$cpue
fleets <- unique(cpue[[1]]$Fleet_name)
nfleet <- length(fleets)
cedat <- makelist(fleets)

for (i in 1:nfleet) { #  i = 1
  pickflt <- which(cpue[[i]]$Fleet_name == fleets[i])
  cedat[[i]] <- cpue[[i]][pickflt,c("Yr","Obs","Exp")] 
  obs <- cpue[[i]][pickflt,"Obs"]
}

plotprep(width=10,height=9)
parset(plots=pickbound(nfleet))


# compare F by fleet

compscenes <- getreplists(store=store,
                          scenes=c("SGbasecase","SGbasecase_R7",
                                   "SGbasecase_Fmsy","SGbasecase_loop2",
                                   "SGbasecase_infl"),
                          listname="plotreport")


catches <- compscenes$catches

total <- compscenes$total
scenes <- names(total)
nscen <- length(scenes)
instF <- makelist((scenes))
nfleet <- total[[1]]$nfleets
for ( i in 1:nscen) instF[[i]] <- total[[i]]$exploitation[,c(1,4:(6+nfleet))]
first <- instF[[1]]
yrs <- first[,"Yr"]
plotprep(width=10, height=6)
parset(plots=c(1,2))
maxy <- getmax(sapply(instF,"[[","F_std"))
plot(yrs,first[,"F_std"],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
     ylab="Overall Instantaneous F",xlab="",panel.first=grid())
for (i in 2:nscen) lines(yrs,instF[[i]][,"F_std"],lwd=3,col=i)
legend("bottomleft",legend=scenes,lwd=3,col=1:nscen,cex=1.25,bty="n") 
# Plot individual fleet with maximum F
numcol <- ncol(first)
flnames <- colnames(first)
fleetF <- apply(first,2,max,na.rm=TRUE)[5:numcol]
pickFl <- which.max(fleetF) + 4
Flmax <- flnames[pickFl]
maxFL <- sapply(instF,"[[",Flmax)
maxy <- getmax(maxFL)
plot(yrs,maxFL[,1],type="l",lwd=2,col=1,ylim=c(0,maxy),yaxs="i",
     ylab=paste0("Fleet with Maximum F: ",Flmax),xlab="",panel.first=grid())
for (i in 2:nscen) lines(yrs,maxFL[,i],lwd=2,col=i)




# end-of-file--------
