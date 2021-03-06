% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lumerical.R
\name{read.lumerical}
\alias{read.lumerical}
\title{Read lumerical data}
\usage{
read.lumerical(filename)
}
\arguments{
\item{filename}{the name of the file which the data are to be read from. If it does not contain an absolute path, the file name is relative to the current working directory, getwd(). Tilde-expansion is performed where supported.}
}
\value{
returns either a data.frame for the 1D data type, or a list object with coordinates and a matrix of extracted values for the 2D data type.
}
\description{
Read lumerical data
}
\details{
read.lumerical will attempt to query the file to discover if it is a 1D or 2D data type. The line-scans of the 1D type retuns a data.frame. The 2D dat type returns a list object containing coordinated and a matrix of values for the 2D plane.
}
\examples{
# read.lumeircal("results.txt")
}
\author{
Tom Constant
}
