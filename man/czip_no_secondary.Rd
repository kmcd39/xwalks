% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{czip_no_secondary}
\alias{czip_no_secondary}
\title{czip_no_secondary}
\format{
An object of class \code{data.frame} with 32941 rows and 4 columns.
}
\usage{
czip_no_secondary
}
\description{
Unique row/ZIP with Xwalk to CZs. They're not co-terminous so % overlap is
included. For ZIPs that span multiple CZs, this xwalk will only contain a
xwalk to the CZ that contains the greatest share of the ZIP.
}
\keyword{datasets}
