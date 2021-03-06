% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RtestingHelpers.R
\name{testmeFileSingle}
\alias{testmeFileSingle}
\title{Run all tests defined in an R-script}
\usage{
testmeFileSingle(file, expectationsFolder, useGit, print = F, logger = LogAt1)
}
\arguments{
\item{file}{Path to R-script}

\item{expectationsFolder}{Folder into which test results are either vivified (see \code{testMe()})
or from which expectations are read for comparison after vivification.}

\item{useGit}{logical to inidicate whether vivifications are to be commited by a call to `git`}

\item{print}{logicial to indicate whether a report is to be printed}

\item{logger}{function that is used to print results, can be used to redirect output}
}
\value{
returns a list for each test executed that contains testing status and number of subtests run
}
\description{
Identify tests in an R file and run the tests
}
\details{
This function takes the path to a single R-file. Function names ending in \code{'_test'} are
considered to contain test definitions. These functions are called and test results are returned.
}
