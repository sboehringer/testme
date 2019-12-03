#' This is package `testme`
#'
#' Rapid development of software tests
#'
#' @details
#' Simplify unit and integrated testing by using implicit definitions. When writing new functions, users usually use example invocations for checking. Exactly this should be and is enough to develop tests using `testme`. Use `?"package-testme"` for a tutorial.
#' This package.
#' @examples
#' \dontrun{
#'  myTest = function(){ T1 = 1 + 1; TestMe(); }
#'  # defines test expectation (vivification)
#'  runTestFunctionSingle('myTest')
#'  # first real comparison
#'  runTestFunctionSingle('myTest')
#'  # error introduced
#'  myTest = function(){ T1 = 3; TestMe(); }
#'  runTestFunctionSingle('myTest')
#' }
#' @seealso {createPackage()} for starting the main workflow

"_PACKAGE"
