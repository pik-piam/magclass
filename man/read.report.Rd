% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.report.R
\name{read.report}
\alias{read.report}
\title{Read file in report format}
\usage{
read.report(file, as.list = TRUE, showSeparatorWarning = TRUE)
}
\arguments{
\item{file}{file name the object should be read from.}

\item{as.list}{if TRUE a list is returned (default), if FALSE it is tried to
merge all information in one MAgPIE object (still under development and
works currently only if the entries for the different models and scenarios
have exactly the same regions and years).}

\item{showSeparatorWarning}{Boolean (default value TRUE) that decides whether the warning
about the replacement of dots in variable names is displayed (default value) or not.}
}
\description{
This function reads the content of a reporting file (a file in the model
intercomparison file format *.mif) into a list of MAgPIE objects or a single
MAgPIE object.
}
\details{
The \strong{Model Intercomparison File Format (MIF)} is the default file
format for data produced by Integrated Assessment Models. It is based on the
common format used for Model Intercomparison Projects such as EMF and SSP
with some slight changes/clarifications in its definition. For interactions
between models this format should be used. For everything else it is at least
recommended to use this format, too.

Aim of this standardization is to achieve a more flexible and smooth
communication between models and to facilitate the creation of aggregated
outputs from integrated assessment scenario runs which then can easily be
uploaded to external databases such as the EMF or SSP database. By using this
standard most of the required decisions for a working input output interface
between models have already been specified which significantly reduces the
required work to get a new interaction running.

\strong{Definition}

The format is characterized by the following features:

\itemize{
  \item The file ending is ".mif"
  \item The file is written in ASCII format
  \item Entries are separated with ";", every line ends with a ";"
  \item The file always contains a header
  \item The format of the header is: \code{Model;Scenario;Region;Variable;Unit;<ADDITIONAL_COLUMNS>;<YEARS>;}
}

The first 5 entries always have to exist, <ADDITIONAL_COLUMNS> is additional
information which can be added optionally (e.g. "Description") and <YEARS>
are the years for which data is delivered. <YEARS> are always written as 4
digit numbers. In the (very unlikely) case that a year before 1000 is used
the number has to start with a 0, e.g. 0950. <ADDITIONAL_COLUMNS> can be
anything, there are no further rules at the moment what it can contain.
However, there are strict rules for naming these columns. Allowed are single
names starting with a capital letter without special characters in it except
"_" which is allowed. Examples: "Description" allowed, "More Description" not
allowed, "More_Description" allowed, "123Description" not allowed,
"Description123" allowed. Scripts using this format must be able to ignore
additional columns. For years there are no specific limitations/requirements
which years should be reported. Scripts dealing with this data must be able
to work with different temporal resolutions. For variables basically
everything can be reported here. Missing values have to be marked with "N/A".
}
\examples{
\dontrun{
read.report("report.csv")
}

}
\seealso{
\code{\link{write.report}}
}
\author{
Jan Philipp Dietrich
}
