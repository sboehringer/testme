#' This is package `testme`
#'
#' Rapid Development of Software Tests
#'
#' @details
#' Simplify unit and integrated testing by using implicit definitions. When writing new functions, users usually use example invocations for checking. Exactly this should be and is enough to develop tests using \code{testme}. Use \code{?'testme-package'} or visit the project wiki <https://github.com/sboehringer/testme/wiki> for a tutorial.
#'
#' The idea of the \code{testme} package is to call examples of a function to test. Return values are
#' assigned to variables with a defined pattern, by default \code{T1, T2, ...}. These expressions are 
#' grouped into a function which calls \code{TestMe} as a last step. This is enough to define the
#' tests. The first run of the tests will store the return values as expectations. Subsequent calls 
#' will compare to these expecatations.
#'
#' @examples
#' \dontrun{
#'  # initialize the testing environment
#'  testmeEnvInit(expectationsFolder = tempdir())
#'  # define the test
#'  myTest = function(){ T1 = 1 + 1; TestMe(); }
#'  # defines test expectation (vivification)
#'  runTestFunction('myTest')
#'  # first real comparison
#'  runTestFunction('myTest')
#'  # error introduced
#'  myTest = function(){ T1 = 3; TestMe(); }
#'  runTestFunction('myTest')
#' }
#' @seealso {runTestFunction()} for starting the main workflow

"_PACKAGE"
