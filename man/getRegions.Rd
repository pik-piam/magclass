% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRegions.R
\name{getRegions}
\alias{getRegions}
\alias{getRegions<-}
\title{Get regions}
\usage{
getRegions(x)

getRegions(x) <- value
}
\arguments{
\item{x}{MAgPIE object}

\item{value}{Vector containing the new region names of the MAgPIE objects.
If you also want to change the mapping of regions to cell please use
\code{\link{getRegionList}} instead.}
}
\value{
Regions of the MAgPIE-object
}
\description{
Extracts regions of a MAgPIE-object
}
\section{Functions}{
\itemize{
\item \code{getRegions(x) <- value}: overwrite region names

}}
\examples{

# a <- read.magpie("example.mz")
# getRegions(a)
}
\seealso{
\code{\link{getYears}}, \code{\link{getNames}},
\code{\link{getCPR}}, \code{\link{read.magpie}}, \code{\link{write.magpie}},
\code{"\linkS4class{magpie}"}
}
\author{
Jan Philipp Dietrich
}
