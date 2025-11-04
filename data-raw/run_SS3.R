


library(makehtml)
library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
ddir <- getDBdir()
wdir <- pathtopath(ddir,"/A_CodeR/SS3run/")
source(pathtopath(wdir,"ss3_utilities.R")) 
#library(r4maps)

options("show.signif.stars"=FALSE,"stringsAsFactors"=FALSE,
        "max.print"=50000,"width"=240)

#store <- pathtopath(wdir,"kingfish/")  # snapper  # Whiting  # kingfish #
store <- "C:/Users/MalcolmHaddon/Dropbox/A_CodeR/SS3/model_files/"
calc <- pathtopath(wdir,"calc/")

#file path
pathSS3 <- "C:/Users/MalcolmHaddon/Dropbox/A_CodeR/SS3run/calc/ss3.exe"
pathSS3

# generate these two directories
dirExists(store)
dirExists(calc)

if (endpart(store) == "kingfish") {  # Kingfish BASECASE--------------------
  basecase <- c("BC",
                "BC-priors",
                "BC-age",
                "BC-noage")
}
if (endpart(store) == "model_files") { # SS33 model examples -------------------
  basecase <- c("empirical_wtatage_and_age_selex",
                "selex_age_example",
                "selex_length_example",
                "simple",
                "simple_long",
                "simple_long_wtatage",
                "simple_with_discard")
}


numdirs <- length(basecase)
# now safely generate the directories
for (direct in 1:numdirs) dirExists(pathtopath(store,basecase[direct]))
printV(basecase)

   # copy_SS_inputs(dir.old=pathtopath(store,basecase[2]),
   #                dir.new=pathtopath(store,basecase[3]))


# sscopyto(origin=store,fromdir="BC-priors",todir="BC-noage")


printV(basecase)

# Run SS3 scenbario------------------------------------
item <- 3
getCase(index=item,basecase)   # this lists the basecase indices to the screen

#executable <- c("SS","SS","SS","SS","SS","SS","SS3","SS3")
starttime <- Sys.time()
analysis <- getCase(index=item,basecase)  # 
cat("\n\n")
print("New Analysis")
print(analysis)
destination <- pathtopath(store,analysis)
print(destination)
extradir <- pathtopath(destination,"extra/")
dirExists(extradir)

copyfiles(analysis,store,calc) # copy and rename the needed files into calc

run(dir=calc,exe=pathSS3,extras="-nohess -maxfn 1000",show_in_console = TRUE,
    skipfinished = FALSE)   

fixstarter(calc,findtext="use init values")  # Check  findtext inside starter

# estimate the inverse Hessian
run(dir=calc,exe="ss3",extras="-maxfn 1000",show_in_console = TRUE,
    skipfinished = FALSE)

storeresults(calc,destination)  # save results to analysis directory 
print(store)
setwd(store)
print(paste0("Running from ",destination))
fileout <- pathtopath(destination,paste0(analysis,".txt"))
sink(fileout)
Btarget <- 0.50  # 
Blimit  <- 0.20  # default Commonwealth Limit Reference Point
plotreport  <- SS_output(dir=destination, repfile = "Report.sso",
                         compfile = "CompReport.sso",covarfile = "covar.sso",
                         forefile = "Forecast-report.sso",
                         wtfile = "wtatage.ss_new", warnfile = "warning.sso",
                         forecast=TRUE,covar=TRUE,readwt=FALSE,verbose=TRUE)

finalplot <- SS_plots(replist=plotreport, plot=1:26,btarg=Btarget,
                      minbthresh=Blimit,uncertainty=TRUE, datplot=TRUE,
                      forecastplot=TRUE,png=TRUE,html=FALSE)
SS_html(replist=plotreport,plotdir=pathtopath(destination,"plots"),
        title=analysis,width=600)
sink()   # close off fileout containing screen dump from SS_output and SS_plots

filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
save(plotreport,file=filename)
cat("\n\nplotreport saved to ",filename)

outlists <- do_extra(plotreport=plotreport,extradir=extradir,analysis=analysis,
                     store=store,verbose = TRUE,
                     compare=NULL)

# load(filename)
cat("SS_output and SS_plots txt sent to ",fileout,"\n")
# Tidy up; return to wkdir
setwd(wdir)
endtime <- Sys.time()
print(endtime - starttime)
round(printV(summarizeSS3(plotreport)$answer),6)
paramreport <- summarizeSS3(plotreport)$param
pickP <- grep("Main_RecrDev",rownames(paramreport))
print(paramreport[-pickP,])


# addplots <- extraplots(destination,analysis) {
#   
#   
# } # end of extra


#   load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))  


# Compare Scenarios---------------------------------
#source(pathtopath(wdir,"ss3_utilities.R")) 

analysis <- "SGBC-5-4-80-6"
store <- "c:/Users/malco/DropBox/A_CodeR/SS3run/kingfish/"



sort(names(plotreport))

compscenes <- getreplists(store=store,
                         # scenes=c("GSVBC","GSVBC-eggsm"),
                         #scenes=c("SGBC-5-4-100-6","SGBC-5-4-100-6test"),
                          scenes=c("BC",
                                   "BC-priors"),
                          listname="plotreport")

projout <- projreceffects(compscenes=compscenes,legcex=1.0,startyr=2,console=TRUE)

tail(projout$depl,15)
tail(projout$totalC,15)

outstats <- comparestats(compscenes)

round(outstats$answer,7)
round(outstats$likes,6)
round(outstats$param,6)
round(outstats$models,3)


# Adjust Recruit bias Ramp-----------------------

biasramp <- SS_fitbiasramp(plotreport,
                           verbose = FALSE,
                           startvalues = NULL,
                           method = "BFGS",
                           twoplots = TRUE,
                           transform = FALSE,
                           plot = TRUE,
                           print = FALSE, # print to png files?
                           plotdir = "default",
                           shownew = TRUE,
                           oldctl = NULL,
                           newctl = NULL,
                           altmethod = "nlminb",
                           exclude_forecast = FALSE,
                           pwidth = 6.5,pheight = 5,punits = "in",
                           ptsize = 10,res = 300,cex.main = 1)
newpars <- biasramp$df
{
  for (i in 1:4) {
    cat("  ",newpars[i,"value"],newpars[i,"label"],"\n")
  }
  cat("  ",newpars[5,"value"],
      "#_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0) \n")
}



# Tuning variances --------------------------------------

analysis <- "BC-priors"
store <- "c:/Users/malco/DropBox/A_CodeR/SS3run/kingfish/"
destination <- pathtopath(store,analysis)
load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))


fltnames <- plotreport$FleetNames  
pickfleets <- c(1,2,3,4)
nfleet <- length(pickfleets)
tuneinfo <- r4ss:::get_tuning_table(plotreport,fleets=pickfleets,
                                    option="Francis")
tuneinfo



pathSS3 <- "C:/Users/malco/Dropbox/A_CodeR/SS3run/calc/ss3.exe"

tune_comps(plotreport,fleets=c(1,2),option="Francis",digits=7,niters_tuning = 3,
           init_run=FALSE,
           dir=calc,
           exe=pathSS3,extras="-nohess",
           allow_up_tuning=TRUE,verbose=TRUE,show_in_console=TRUE)

storeresults(calc,destination)


# Alt SELECTIVITY plots--------------------------------------
analysis <-  "SGBC-5-4-100-6"
store <- "c:/Users/malco/DropBox/A_CodeR/SS3run/kingfish/"
destination <- pathtopath(store,analysis)
load(pathtopath(destination,paste0("plotreport_",analysis,".Rdata")))
extradir <- pathtopath(destination,"extra/")
dirExists(extradir)




plotselex(plotreport,sex="Female",upbound=0,
          console=TRUE,rundir=extradir)
dev.off()

plotselex(plotreport,sex="Male",upbound=0,
          console=TRUE,rundir=extradir)
dev.off()

# Alt CPUE plots-------------
#analysis <- "SGbasecase"
analysis <- "BC-priors"
destination <- pathtopath(store,analysis)
filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)
sort(names(plotreport))

cpue <- plotreport$cpue
colnames(cpue) <- tolower(colnames(cpue))
fleets <- unique(cpue[,"fleet_name"])
nfleet <- length(fleets)
yrrange <- range(cpue[,"yr"])
plotprep(width=8, height=8)
parset(plots=c(nfleet,2),margin=c(0.3,0.4,0.05,0.1))
for (fl in 1:nfleet) { # fl = 1
  dat <- cpue[cpue[,"fleet_name"]==fleets[fl],]
  rown <- nrow(dat)
  maxy <- getmax(dat[,c("obs","exp")])
  plot(dat[,"yr"],dat[,"exp"],type="l",lwd=2,xlab="",ylab=fleets[fl],
       ylim=c(0,maxy),xlim=c(yrrange[1],yrrange[2]),panel.first=grid())
  points(dat[,"yr"],dat[,"obs"],pch=16,cex=1.0,col=2)
  plot(dat[,"yr"],dat[,"dev"],type="p",pch=16,cex=1.0,xlab="",ylab="Deviate",
       xlim=c(yrrange[1],yrrange[2]),panel.first=grid())
  abline(h=0.0,lwd=1.0,col=1)
  for (i in 1:rown) lines(c(dat[i,"yr"],dat[i,"yr"]),c(0.0,dat[i,"dev"]),lwd=1)
}

cpue <- plotreport$cpue
colnames(cpue) <- tolower(colnames(cpue))
fltnum <- sort(unique(cpue[,"fleet"]))
nfleet <- length(fltnum)
fleets <- NULL
tmpyears <- makelist(fltnum)
for (i in 1:nfleet) {
  pick <- which(cpue[,"fleet"] == fltnum[i])
  fleets <- c(fleets,cpue[pick[1],"fleet_name"])
}
yrrange <- range(cpue[,"yr"])
yrs <- yrrange[1]:yrrange[2]
nyrs <- length(yrs)
years <- matrix(NA,nrow=nyrs,ncol=nfleet,dimnames=list(yrs,fleets))
obs <- exp <- years
for (i in 1:nfleet) {  # i = 1
  pickF <- which(cpue[,"fleet"] == fltnum[i])
  pickR <- match(cpue[pickF,"yr"],yrs)
  years[pickR,i] <- cpue[pickF,"yr"]
  obs[pickR,i] <- cpue[pickF,"obs"]/mean(cpue[pickF,"obs"],na.rm=TRUE)
  exp[pickR,i] <- cpue[pickF,"exp"]/mean(cpue[pickF,"exp"],na.rm=TRUE)
}
maxy <- getmax(c(obs,exp))
plotprep(width=9,height=5)
parset()
plot(yrs,obs[,1],type="p",pch=16,cex=1.2,col=1,xlim=c(yrrange),ylim=c(0,maxy),
     panel.first=grid())
lines(yrs,obs[,1],lwd=1,lty=2,col=1)
for (i in 2:nfleet) {
  points(yrs,obs[,i],pch=16,cex=1.2,col=i)
  lines(yrs,obs[,i],lwd=1,lty=2,col=i)
}
for (i in 1:nfleet) lines(yrs,exp[,i],lwd=2,col=i)

# Likelihood Profile -----------------------------------
# profile on M---------------------------
require(codeutils)
require(hplot)
require(rforSS3)
require(r4ss)

ddir <- getDBdir()   # obviously define your own directory structure
wdir <- pathtopath(ddir,"/A_CodeR/SS3run/")
source(pathtopath(wdir,"ss3_utilities.R")) 
calc <- pathtopath(wdir,"calc/")
profdir <- pathtopath(wdir,"profdirgar-5M/")
dirExists(profdir)

# ss3.exe file path
pathSS3 <- pathtopath(calc,"/ss3.exe")
pathSS3
# Assumes ss.ctl, ss.dat, ss3.par, starter.ss, and forecast.ss are in calc
# directory, CHECK the LINE NUMBERS to change in the ss.ctl and ss3.par files
# check the text to search for in the starter.ss file, is it: 'prior_like'
# if not change it either in the function prompt or in the starter file.

copyproffiles(calc=calc,profdir=profdir,
              getfiles = c("ss.ctl","ss.dat","ss3.par","starter.ss","forecast.ss"),
              findtext="prior_like")

setwd(profdir)
dir(profdir)

profvec <- seq(0.4,0.6,0.025)
nvec <- length(profvec)
nvec
# check linenums in control.ss_new and in ss3.par

profans <- r4ss::profile(dir=profdir,oldctlfile="ss.ctl",newctlfile="ss.ctl",
                         linenum=53,
                         profilevec=profvec,usepar=TRUE,
                         parlinenum=5,
                         exe=pathSS3,extras="-nohess -maxfn 600",show_in_console = TRUE)

save(profans,file=pathtopath(profdir,"profans.RData"))
#   load(pathtopath(profdir,"profans.RData"))

profans2 <- profans
# make sure the colnames reflect your own model’s structure
colnames(profans2) <- c("Value","Conv","Total","Catch","EquilC","Survey",
                        "Lencomp","agecomp","Recruit","InitEQ","ForeRec",
                        "ParPr","softbound","Pardevs","CrPen","MaxGrad")
library(knitr)
kable(profans2,digits=c(2,0,3,6,3,3,3,3,3,3,4,3,6,3,3,7))

numcol <- ncol(profans)
scaledprof <- apply(profans[,3:numcol],2,function(x) x - min(x,na.rm=TRUE))
scaledrange <- apply(scaledprof,2,max)
printV(sort(round(scaledrange,2),decreasing=TRUE))

sumnoLC <- sum_noLC - min(sum_noLC)


plotprep(width=10,height=6,filen=””)
parset(cex=1.0)
plot(profvec,scaledprof[,"TOTAL"] ,type="l",
     lwd=2,col=1,xlab="Natural Mortality",ylab="Scaled Likelihoods",
     panel.first = grid(),yaxs="i",ylim=c(0,10))
abline(h=1.92,lwd=2,col=1,lty=3)
lines(profvec,scaledprof[,"Length_comp"],lwd=2,col=2)
lines(profvec,scaledprof[,"Age_comp"],lwd=2,col=3)
lines(profvec,scaledprof[,"Recruitment"],lwd=2,col=4)
lines(profvec,scaledprof[,"Survey"],lwd=2,col=6)
lines(profvec,scaledprof[,"Equil_catch"],lwd=2,col=5)
legend("topright",
       c("Total","Length_comp","Age_comp","recruit","CPUE","EquilCatch"),lwd=3,
       col=c(1,2,3,4,5,6),lty=c(1,1,1,1,1,1),bty="n",cex=1.25)
if (nchar(filen) > 0) dev.off()

setwd(wdir)


# profile on R0---------------------------
ddir <- getDBdir()
wdir <- pathtopath(ddir,"/A_CodeR/SS3run/")
source(pathtopath(wdir,"ss3_utilities.R")) 

calc <- pathtopath(wdir,"calc/")

profdir <- pathtopath(wdir,"profdirgar-5M-R0")
dirExists(profdir)
# Assumes ss.ctl, ss.dat, ss3.par, starter.ss, and forecast.ss are in calc
# check the line numbers to change in the ss.ctl and ss3.par files
# check the text to search for in the starter.ss file, is it: 'prior_like'
# if not change it.

copyproffiles(calc=calc,profdir=profdir,
              getfiles = c("ss.ctl","ss.dat","ss3.par","starter.ss","forecast.ss"),
              findtext="prior_like")

setwd(profdir)
dir(profdir)

profvec <- seq(9.46,9.68,0.01)
nvec <- length(profvec)
nvec
# check linenums in control.ss_new and in ss3.par

profans <- r4ss::profile(dir=profdir,oldctlfile="ss.ctl",newctlfile="ss.ctl",
                         linenum=86,profilevec=profvec,
                         usepar=TRUE,parlinenum=49,exe=pathSS3,
                         extras="-nohess -maxfn 600",show_in_console = TRUE)

save(profans,file=pathtopath(profdir,"profans.RData"))
#   load(pathtopath(profdir,"profans.RData"))

profans2 <- profans
colnames(profans2) <- c("Value","Conv","Total","Catch","EquilC","Survey",
                        "Lencomp","agecomp","Recruit","InitEQ","ForeRec",
                        "ParPr","softbound","Pardevs","CrPen","MaxGrad")
library(knitr)
kable(profans2,digits=c(2,0,3,6,3,3,3,3,3,3,4,3,6,3,3,7))

numcol <- ncol(profans)
scaledprof <- apply(profans[,3:numcol],2,function(x) x - min(x,na.rm=TRUE))
scaledrange <- apply(scaledprof,2,max)
printV(sort(round(scaledrange,2),decreasing=TRUE))

# sum_noLC <- rowSums(profans[,c(4,5,6,8:15)])
# sumnoLC <- sum_noLC - min(sum_noLC)



plotprep(width=10,height=6)
parset(cex=1.0)
plot(profvec,scaledprof[,"TOTAL"] ,type="l",
     lwd=2,col=1,xlab="Ln(R0)",ylab="Scaled Likelihoods",
     panel.first = grid(),yaxs="i",ylim=c(0,10))
abline(h=1.92,lwd=2,col=1,lty=3)
lines(profvec,scaledprof[,"Length_comp"],lwd=2,col=2)
lines(profvec,scaledprof[,"Age_comp"],lwd=2,col=3)
lines(profvec,scaledprof[,"Recruitment"],lwd=2,col=4)
lines(profvec,scaledprof[,"Survey"],lwd=2,col=6)
lines(profvec,scaledprof[,"Equil_catch"],lwd=2,col=5)
#lines(profvec,sumnoLC,lwd=2,lty=2,col=1)
legend("topright",
       c("Total","Length_comp","Age_comp","recruit","CPUE","EquilCatch"),lwd=3,
       col=c(1,2,3,4,5,6),lty=c(1,1,1,1,1,1),bty="n",cex=1.25)


setwd(wdir)





# exploreM---------------------------
ages <- 0:50
M <- seq(0.1,0.75,0.05)
surv <- exp(-M)

mortab <- matrix(0,nrow=51,ncol=14,dimnames=list(0:50,M))
mortab[1,] <- 100
for (j in 1:14) {
  for (i in 2:51) {
    mortab[i,j] <- mortab[i-1,j] * surv[j] 
  }
}

round(mortab,2)

whichgt1 <- function(x) (which(x < 1)[1] - 1)

pickrow <- apply(mortab,2,whichgt1)
maxage <- ages[pickrow]
names(maxage) <- M
maxage

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

yrs <- 1983:2037
picky <- match(yrs,bmal[,"Yr"])
maxy <- getmax(bmal[picky,"totB"])
plotprep(width=9,height=4.5)
parset()
plot(yrs,bmal[picky,"totB"],type="l",lwd=2,ylim=c(0,maxy),xlab="",
     ylab="Total Biomass",panel.first=grid())
lines(yrs,bfem[picky,"totB"],lwd=2,col=2)

allbatage <- plotreport$batage
pick <- which(allbatage[,"Beg/Mid"] == "M")
batage <- allbatage[pick,]
years <- unique(batage[,"Yr"])
bage <- batage[,13:19]

sexB <- tapply(bage,list(batage[,"Yr"],batage[,"Sex"]),sum)

plotprep(width=9,height=5)
parset()
maxy <- getmax(sexB)
plot(years,sexB[,1],type="l",lwd=2,col=1,xlab="",ylab="Biomass",ylim=c(0,maxy),
     yaxs="i",panel.first=grid())
lines(years,sexB[,2],lwd=2,col=2)
legend



# do_extra plots tables -----------------------------------------
library(makehtml)
library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
ddir <- getDBdir()
wdir <- pathtopath(ddir,"/A_CodeR/SS3run/")
store <- pathtopath(wdir,"Whiting")  # snapper  # Whiting  # kingfish
analysis <-   "GSV"     #     #  "GSVBC"   #  "SGBC-S100-M5" 
destination <- pathtopath(store,analysis)
extradir <- pathtopath(destination,"extra/")
dirExists(extradir)

filename <- pathtopath(destination,paste0("plotreport_",analysis,".Rdata"))
load(filename)

#source(pathtopath(wdir,"ss3_utilities.R")) 




outlists <- do_extra(plotreport=plotreport,extradir=extradir,analysis=analysis,
                     store=store,verbose = FALSE,
                     compare=c("GSV","GSV_SingleArea_BC_m0.2_nForecast"))
                    # compare=c("SGBC-S70-M5","SGBC-S75-M5","SGBC-S80-M5","SGBC-S100-M5"))


# plotagelenkey------------------------
library(makehtml)
library(rforSS3)
library(r4ss)
library(codeutils)
library(hplot)
ddir <- getDBdir() # get dropdir dir


# wdir <- pathtopath(ddir,"/A_CodeR/SA-SS3/") # use your own working directory
# store <- pathtopath(wdir,"snapper/") 
# destination <- pathtopath(store,"GSVBC-CE")
# analysis <- "GSVBC-CE"

wdir <- pathtopath(ddir,"/A_CodeR/SS3run/")
store <- pathtopath(wdir,"kingfish/") 
analysis <- "BC"

sourcedir <-  pathtopath(ddir,"/A_CodeR/SS3run/")
destination <- pathtopath(store,analysis)
datfile <- pathtopath(destination,paste0(analysis,".dat"))
source(pathtopath(sourcedir,"ss3_utilities.R"))  # where one stores ss3_utilities.R

dat <- SS_readdat_3.30(file=datfile,verbose = TRUE,
                       echoall = lifecycle::deprecated(),section = NULL)

outscene <- getagelenkeys(dat)
str1(outscene)
str1(outscene$allscene[[1]])

extradir <- pathtopath(destination,"extra2/")
dirExists(extradir)
rundir <- extradir
console <- FALSE
verbose <- FALSE
setuphtml(extradir)

if (outscene$nscene > 8) {
  nscene <- outscene$nscene
  iter <- ceiling(outscene$nscene / 8)
  pickscene <- c(1:8)
  for (i in 1:iter) {
    plotagelenkey(outcomp=outscene,rundir=rundir,plotscenes=pickscene,pch=1,
                  pchcex=1.25,pchcol=2,console=console,verbose=verbose)
    pickscene <- pickscene + 8
    pickpick <- which(pickscene <= nscene)
    pickscene <- pickscene[pickpick]
    if ((i < iter) & (console)) readline(prompt="Press [enter] to continue")
  }
} else {
  plotagelenkey(outcomp=outscene,rundir=rundir,plotscenes=NULL,pch=1,
                pchcex=1.25,pchcol=2,console=console,verbose=verbose)
}
make_html(replist=NULL,rundir=extradir,
          datadir=NULL,width=500,
          openfile=TRUE,
          runnotes="Extra Analyses",
          verbose=verbose,
          packagename="rforSS3",
          htmlname=paste0(analysis,"_Extra"))











