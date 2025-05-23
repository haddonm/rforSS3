% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison.R
\name{comparecpueplot}
\alias{comparecpueplot}
\title{comparecpueplot compare fits to cpue and devs for fleets and scenarios}
\usage{
comparecpueplot(cpue, rundir, height = 8, CI = TRUE, console = TRUE)
}
\arguments{
\item{cpue}{a list of the cpue objects out of a set of plotreports}

\item{rundir}{the directory into which to place the plot if saved}

\item{height}{the height of the plot, the default = 8, which suits 2 fleets.
if you have fewer then adjust this appropriately. If more than 5 then
external to the function it may be best to subset the cpue input data.}

\item{CI}{default = TRUE confidence bounds will be plotted.}

\item{console}{default = TRUE so plot goes to console.}
}
\value{
invisibly the filename, to be used by addplot it also plots a graph
}
\description{
comparecpueplot compare the fits to the cpue and the residuals
for all fleets across multiple scenarios. The first column of plots is
the observed vs expected cpue (the fit) and the second column is an
explicit residual plot on a linear scale. The rows are the fleets. The
CI, if included, are only derived from the first scenario.
}
\examples{
# syntax: 
# extradir <- pathtopath(destination,"extra/")
print("wait on data sets")
# cpue=cpue; rundir=extradir; height=8;CI=TRUE;console=TRUE
}
