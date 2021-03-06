% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RtestingHelpers.R
\name{TestMe}
\alias{TestMe}
\title{Run tests defined in the current function}
\usage{
TestMe(mode = list(), which = -2)
}
\arguments{
\item{mode}{list with modes of comparison. By default \code{compare} is used for the comparison. Elements with names of the test can overwrite the testing mode. Available modes are \code{c('compare', 'round8', 'equal', 'error', 'image')}.}

\item{which}{integer to indicate which environment relative to current is to be used to look for tests.}
}
\value{
undefined return value
}
\description{
Searches the current function for test definitions, runs the tests and compares to expectations.
}
\examples{
\dontrun{
myTests = function() {
  T1 = 1 + 1;
  T2 = sqrt(exp(10));
  TestMe(list(T2 = 'round8'));
}
}
}
