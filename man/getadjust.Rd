% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balanceoutput.r
\name{getadjust}
\alias{getadjust}
\title{getadjust extracts the newvariance adjustment factors}
\usage{
getadjust(incontrol, replist, CAL = TRUE, ghosts = FALSE)
}
\arguments{
\item{incontrol}{the actual control file for the SS3 analysis}

\item{replist}{the object into which the output of SS_output was assigned}

\item{CAL}{conditional Age-at-Length data rather than classical age composition
data; default = TRUE, meaning conditional-age-at-length is assumed as usual}

\item{ghosts}{should ghost fleets be excluded; default is TRUE}
}
\value{
a list containing the revised control file and the varadj matrix
}
\description{
getadjust extracts the newvariance adjustment factors. We all
need more time to write descritpions!
}
\examples{
\dontrun{
print("Still to be developed")
}
}
