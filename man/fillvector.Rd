% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss3_utils.r
\name{fillvector}
\alias{fillvector}
\title{fillvector expands a vector of values for classes across known classes}
\usage{
fillvector(x, vals)
}
\arguments{
\item{x}{a vector of values each with the name of its respective class}

\item{vals}{a vector of class values to check against the names of the x
vector.}
}
\value{
a vector of length length(vals), containing the x values in the
correct cells
}
\description{
fillvector given a vector of values for a series of, possibly
incomplete, classes, which could be size or age classes, and places the
known values into the correct bins of the complete set of classes. Thus,
with ages 0:6 given a vector of numbers-at-age of 1=12,2=24,3=6, 4= 5,
fillvector would generate c(0,12,24,6,5,0,0)
}
\examples{
x <- c(12,24,6,1)
names(x) <- c("1","2","3","5")
ageclasses <- 0:6
fillvector(x=x,vals=ageclasses)
}
