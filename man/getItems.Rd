% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getItems.R
\name{getItems}
\alias{getItems}
\alias{getItems<-}
\title{Get Items}
\usage{
getItems(x, dim = NULL, split = FALSE, full = FALSE)

getItems(x, dim, full = NULL, maindim = NULL, raw = FALSE) <- value
}
\arguments{
\item{x}{MAgPIE object}

\item{dim}{Dimension for which the items should be returned. Either number or
name of dimension or a vector of these. See \code{\link{dimCode}} for more details.}

\item{split}{Boolean which determines whether a main dimension should be split in subdimensions.
Only applicable to main dimensions (1,2,3) and ignored for all other.}

\item{full}{if TRUE dimension names are returned as they are (including repetitions), if FALSE only
the dimension elements (unique list of entries) are returned.}

\item{maindim}{main dimension the data should be added to (does not need to be set if \code{dim} exists
in the data. Should be set if \code{dim} might not exist, or if \code{dim} might potentially exist
in a different main dimension than the one anticipated).}

\item{raw}{if set to FALSE inputs will be corrected (e.g. dots replaced by the letter "p") if necessary. If
TRUE data will be written as is (risking the creation of inconsistent objects).}

\item{value}{a vector with the length of the main dimension the dimnames should be replaced in / added to.
If set to NULL the corresponding dimension will be removed.}
}
\value{
items of the requested dimension in the MAgPIE-object. If split=TRUE and applied to a
main dimension (1,2,3) a list of items for each sub-dimension.
}
\description{
Extract items of a given (sub-)dimension of a MAgPIE-object
}
\section{Functions}{
\itemize{
\item \code{getItems(x, dim, full = NULL, maindim = NULL, raw = FALSE) <- value}: set dimension names

}}
\examples{
x <- maxample("pop")
getItems(x, "scenario")
getItems(x, 3.1)
getItems(x, "i") <- paste0("REG", seq_len(ncells(x)))
getItems(x, "i")
y <- x[, 1, ]
getItems(y, "t") <- NULL
}
\seealso{
\code{\link{dimCode}}
}
\author{
Jan Philipp Dietrich
}
