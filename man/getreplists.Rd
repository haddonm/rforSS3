% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison.R
\name{getreplists}
\alias{getreplists}
\title{getreplists loads a set of SS_output outputs into a set of lists}
\usage{
getreplists(store, listname, scenes, paths = NULL, verbose = TRUE)
}
\arguments{
\item{store}{is a directory that contains separate sub-directories for each
scenario, each of which contains an object generated by SS_output. If
the paths option is used instead then store can be set to NA or NULL.}

\item{listname}{The standard name for the object output from SS_output, if
the paths option is used instead then store can be set to NA or NULL.}

\item{scenes}{a character vector of the names given to each scenario, this
is required by both approaches for file loading as these names will be
used to label each scenario.}

\item{paths}{default = NULL, meaning the rforSS3 approach will be used, which
requires only the scenes argument to contain information. Otherwise,
scenes can be NULL and paths should contain a character vector made up
of the complete path and filename for each Rdata file stored wherever
they are stored.}

\item{verbose}{default = TRUE warnings will be given}
}
\value{
a list of lists so for each scenario a list of the timeseries, of
the catches, of the SS_output object in their entirety and the output
from summarizeSS3.
}
\description{
getreplists allows comparisons between alternative scenarios run
in Stock synthesis 3 by loading the different replist objects generated
by the r4ss function SS_output. Each of the different scenarios is saved
into a list, as well as extracting the timeseries from each, and the
catches. Two approaches to identifying the paths leading to the various
scenarios are implemented. The first uses the method implemented when
using rforSS3 that has a 'store' which is the directory containing
separate sub-directories for each scenario being considered and it
generates the filenames to be generated from the various identified
scenes or scenarios. The alternative is to provide a character vector
containing the full paths and filenames of the SS_output objects to be
compared. cpue has been added to the output.
}
\examples{
# require(r4ss)
# require(codeutils)
# compscenes <- getreplists(store="c:/afishsps/",listname="plotreport",
#             scenes=c("basecase_1","basecase_Rp7"),paths=NULL)
# alternatively
# compscenes <- getreplists(store=NULL,listname=NULL,
#                           scenes=c("basecase_1","basecase_Rp7"),
#             paths=c("c:/afishsps/basecase_1/plotreport_basecase_1.Rdata",
#                     "c:/afishsps/basecase_1/plotreport_basecase_Rp7.Rdata")
}
\seealso{
{
\link[r4ss]{SS_output}, \link{projectedcatches}
}
}
