#' This is package `testme`
#'
#' Rapid development of software tests
#'
#' @details
#' Simplify unit and integrated testing by using implicit definitions. When writing new functions, users usually use example invocations for checking. Exactly this should be and is enough to develop tests using `testme`. Use `?"testme-package"` for a tutorial.
#' This package \code{testme}.
#' @examples
#' \dontrun{
#'  # initialize the testing environment
#'  testmeEnvInit()
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
#'globalVariables(c("LogAt1"))

"_PACKAGE"
