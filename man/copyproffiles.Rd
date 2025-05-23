% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison.R
\name{copyproffiles}
\alias{copyproffiles}
\title{copyproffiles copies a set of files over from the calc to the profdir}
\usage{
copyproffiles(
  calc,
  profdir,
  getfiles = c("ss.ctl", "ss.dat", "ss3.par", "starter.ss", "forecast.ss"),
  findtext = "_prior_like"
)
}
\arguments{
\item{calc}{the full path to the directory in which SS3 calculations occur}

\item{profdir}{the full path to the directory in which the likelihood
profile calculations are to occur}

\item{getfiles}{a character vector of the files names to be moved from the
calc to the profdir directory. default = ss.ctl, ss.dat, ss3.par,
starter.ss, and forecast.ss. The latter two are names required by SS3.
The ss.ctl and ss.dat names are compared with those in the starter file
and cahnged if necessary}

\item{findtext}{the text in the starter.ss file that identifies the line
where one places a 1 (instead of 0) so that prior likelihoods are
included when doing likelihood profiles. default='_prior_like' but
'use prior like' is also common and is found in starter.ss_new}
}
\value{
nothing, but it copies the files listed in 'getfiles' from the calc
directory to the profdir directory, and sets the 'use prior likelihood'
argument in the starter.ss file to 1.
}
\description{
copyproffiles is used to copy a set of files (see default
filenames in getfiles argument) from teh directory in which a scenarios
calculations were made into the profdir when likelihood profile
calculations are to occur. Best to have a separate directory to save
filling up your calc directory with large numbers of files I doubt you
will use again (thought maybe)
}
\examples{
print("read the code")
}
