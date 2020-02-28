#
#	RtestingHelpers.R
#Thu Aug 16 17:46:14 CEST 2018

#
#	<p> package definition
#

packageDefinition = list(
	name = 'testme',
	files = c('Rmeta.R', 'Rdata.R', 'Rsystem.R', 'Rfunctions.R'),
	#instFiles = list(Rscripts = 'Dev/pkg-minimal.R'),
	testing = list(
		doInstall = TRUE,
		tests = c('RtestsPackages/testme/testme.R'),
		prevent = FALSE	# set to TRUE when bootstrapping package/testme
	),
	description = list(
		title = 'Rapid Development of Software Tests',
		# version to be documented in news section
		#version = '0.1-0',
		author = 'Stefan B\uf6hringer <r-packages@s-boehringer.org>',
		description = 'Simplify unit and integrated testing by using implicit definitions. When writing new functions, users usually use example invocations for checking. Exactly this should be and is enough to develop tests using `testme`. Use `?"testme-package"` or visit the project wiki (on github) for a tutorial.',
		depends = c('compare', 'methods', 'utils', 'stats'),
		suggests = c(),
		news = "1.2-1	Minor fix\n1.2-0	Documentation updates and fixes. Additions for package testing (runPackageTests). Tests no longer installed when --as-cran is used (see vignette).\n1.1-0	Disable testing on CRAN.\n1.0-0	Documentation complete. RC3.\n0.9-4	Package test installation fix.RC2.\n0.9-3	Package tests for testme. RC1. 0.9-2	Minor fix R-session non-isolation.\n0.9-1	Minor fix R-session isolation.\n0.9-0	RunTests function to run full testing battery in current or isolated R-session\n0.8-1	Bug fix R CMD build.\n0.8-0	Clean CRAN check. Beta version.\n0.7-1	Docu updates.\n0.7-0	All core functions documented\n0.6-0	Pre-alpha version. Needs more documentation\n0.5-0	error free cran-check, some warnings left\n0.4-0	fixed errors. logger function for test output\n0.3-0	`installPackageTests` function. Allow to install unit tests into a package folder \n\t and create required additional required files to have R run the tests on installation.\n0.2-0	Export functions\n0.1-0	Initial release",
		vignettes = 'vignettes/vignette-testme.Rmd'
	),
	git = list(
		readme = '## Installation\n```{r}\nlibrary(devtools);\ninstall_github("sboehringer/testme")\n```\n\nDevelopment of this package takes place on https://github.com/sboehringer/testme\n\nThe wiki is available at https://github.com/sboehringer/testme/wiki',
		push = F,
		pushOnNewVersion = T,
		remote = 'https://github.com/sboehringer/testme.git'
	)
);
# Additional exports [RegenericAll*]
#' @export Deparse
# Imports
#' @import compare
#' @import methods
#' @importFrom "stats" "as.formula" "median" "model.matrix" "na.omit" "optimize" "runif" "sd" "setNames"
#' @importFrom "utils" "capture.output" "read.table" "recover" "write.table"
globalVariables(c("LogAt1", 'valueMapperStandard', 'plot_save', 'install_local'))

#__PACKAGE_DOC__
#
# The idea of the \code{testme} package is to call examples of a function to test. Return values are
# assigned to variables with a defined pattern, by default \code{T1, T2, ...}. These expressions are 
# grouped into a function which calls \code{TestMe} as a last step. This is enough to define the
# tests. The first run of the tests will store the return values as expectations. Subsequent calls 
# will compare to these expecatations.
#
# @examples
# \dontrun{
#  # initialize the testing environment
#  testmeEnvInit(expectationsFolder = tempdir())
#  # define the test
#  myTest = function(){ T1 = 1 + 1; TestMe(); }
#  # defines test expectation (vivification)
#  runTestFunction('myTest')
#  # first real comparison
#  runTestFunction('myTest')
#  # error introduced
#  myTest = function(){ T1 = 3; TestMe(); }
#  runTestFunction('myTest')
# }
# @seealso {runTestFunction()} for starting the main workflow
#__PACKAGE_DOC_END__

#
#	<p> initialization
#

#Library('compare');
# <!><A><N> temporary disabling of reset for debugging
testmeEnv = new.env();	# avoid R warnings [not strictly needed dt <<- assignment later]

#
#	<p> global interface
#

#' Log message at level 1
#'
#' Call function Log with a log level of 1.
#' @param s Message to be printed
#' @export LogAt1
LogAt1 = function(s)Log(s, 1);
Mget = function(x, envir, mode = 'any', ifnotfound, ...) {
	envS = substitute(envir);
	if (class(envS) == 'name' && !exists(as.character(envS), envir = parent.frame())) return(ifnotfound);
	v = mget(x, envir, mode, ifnotfound = NA, ...);
	r = if (is.na(v)) ifnotfound else v[[x]];
	return(r);
}
#' Run tests defined in single function
#'
#' The function is exepected to call \code{TestMe()} to invoke testing. The function is called and test results are collected.
#'
#' @param testName function name as charater vector with one element
#' @param logger function that prints logging information
#' @return Test result summary is returned as a list with components \code{result}, the boolean test state, \code{NsubTests}, the number of tests performed in the function
# @seealso {runTestFunction()} for passing a character vector with many elements
runTestFunctionSingle = function(testName, logger = LogAt1) {
	assign('name', testName, testmeEnv);	# global variable holding the test name
	Log = Mget('logger', testmeEnv, 'function', ifnotfound = logger);
	testFunction = get(testName);
	rTest = try(testFunction());
	r = if (is.list(rTest)) {
		Log(Sprintf("Test: %{testName}s [N = %{N}d]", N = rTest$NsubTests));
		rTest$result;
	} else {
		# legacy test
		Log(Sprintf("Test: %{testName}s [N = ?], legacy"));
		rTest
	}
	if (class(r) == 'try-error' || !isTRUE(r)) Log(Sprintf("********* test: %{testName}s failed"))
	r = if (class(r) == 'try-error') FALSE else if (class(r) == 'comparison') isTRUE(r) else r;
	list(if (is.list(rTest)) rTest else list(result = rTest, NsubTests = 1))
}
#' Run tests defined in functions
#'
#' Tests are defined by a charachter vector containing function names. Each function is run using \code{runTestFunctionSingle}.
#'
#' @param testName charater vector with function names
#' @param logger function that prints logging information
#' @return Test result summary is returned as a list of lists with components \code{result}, the boolean test state, \code{NsubTests}, the number of tests performed in the function
#' @export runTestFunction
# @seealso {runTestFunctionSingle()} which this function is a vectorized version of
runTestFunction = Vectorize(runTestFunctionSingle, 'testName');

#' Run all tests defined in an R-script
#'
#' Identify tests in an R file and run the tests
#'
#' This function takes the path to a single R-file. Function names ending in \code{'_test'} are
#' considered to contain test definitions. These functions are called and test results are returned.
#'
#' @param file Path to R-script
#' @param expectationsFolder Folder into which test results are either vivified (see \code{testMe()})
#'   or from which expectations are read for comparison after vivification.
#' @param useGit logical to inidicate whether vivifications are to be commited by a call to `git`
#' @param print logicial to indicate whether a report is to be printed
#' @param logger function that is used to print results, can be used to redirect output
#' @return returns a list for each test executed that contains testing status and number of subtests run
#' @export testmeFileSingle
testmeFileSingle = function(file, expectationsFolder, useGit, print = F, logger = LogAt1) {
	testmeEnvInit(expectationsFolder = splitPath(expectationsFolder)$absolute, logger = logger);
	owd = setwd(splitPath(file)$dir);
	on.exit(setwd(owd));

	tests = findTestsFiles(splitPath(file)$file)$tests;
	source(splitPath(file)$file);
	
	# <p> git
	if (useGit) gitCommitVivifications();
	# <p> start testing
	#allGood = runTestMe(tests, logLevel);
	rTests = runTestFunction(tests);
	if (print) testmePrintReport(rTests);
	return(rTests);
}

#' Run all tests defined in a collection of R-scripts
#'
#' Collect testing functions from files and run tests
#'
#' This function takes a character vector of pathes to R-files. It is a vectorized version of \code{testmeFileSingle}
#'
#' @param file character vector of R-scripts containing tests
#' @param file Path to R-script
#' @param expectationsFolder Folder into which test results are either vivified (see \code{testMe()})
#'   or from which expectations are read for comparison after vivification.
#' @param useGit logical to inidicate whether vivifications are to be commited by a call to `git`
#' @param print logicial to indicate whether a report is to be printed
#' @param logger function that is used to print results, can be used to redirect output
#' @return returns a list for each test executed that contains testing status and number of subtests run
#' @export testmeFile
testmeFile = Vectorize(testmeFileSingle, 'file');

# # <A> indirection necessary due to roxygen not being able to handle Vectorize properly
# testmeFileV = Vectorize(testmeFileSingle, 'file');
# testmeFile = function(file, expectationsFolder, useGit, print = F, logger = LogAt1)
# 	testmeFileV(file, expectationsFolder, useGit, print, logger)

#' Run all tests defined in a folder
#'
#' Collect files from a folder using a pattern, extract testing functions and run tests.
#'
#' @param dir path to older with R-scripts containing tests
#' @param expectationsFolder path to folder containing expectations for tests
#' @param filePattern pattern to identify file names containting tests. Passed to \code{list.files}.
#' @param useGit boolean to indicate whether to commit changes in expectation using \code{git}.
#' @param logLevel detail of logging information (>4: debugging information)
#' @param print boolean to indicate whether a test report is to be printed
#' @return test results as returned by \code{testmeFile}
#' @export testmeDir
testmeDir = function(dir = 'Rtests', expectationsFolder = 'Rtests/RtestsExpectations',
	filePattern = '.R$', useGit = T, logLevel = 4, print = T) {
	Rfiles = list.files(dir, filePattern, full.names = TRUE);
	rTests = testmeFile(Rfiles, expectationsFolder, useGit = F);
	# <p> git
	if (useGit) gitCommitVivifications();
	if (print) testmePrintReport(rTests);
	return(rTests);
}

packageTestFileTemplates = list(
	standard = "# This runs tests `%{base}s`\n#testmeEnvInit('RtestsExpectations', logger = print);\nlibrary('testme');\nprint(testmeFileSingle('testme/%{file}s', 'testme/RtestsExpectations', useGit = FALSE, logger = print));\n",
	cran =  "# tess on cran currently skipped"
);

InstallPackageTest = function(packageDir, testPath, createReference, asCran = FALSE) {
	testBase = Sprintf('%{packageDir}s/tests');
	dest = Sprintf('%{testBase}s/testme');
	Dir.create(dest, recursive = T, logLevel = 2);
	File.copy(testPath, dest, symbolicLinkIfLocal = F, overwrite = T);
	base = splitPath(testPath)$base;
	runFileName = Sprintf('%{testBase}s/%{base}s_run.R');
	packageTestFileTemplate = packageTestFileTemplates[['standard']];
	runFile = Sprintf(packageTestFileTemplate, splitPath(testPath));
	if (asCran) {
		LogS(2, "We currently skip tests on CRAN. Removing %{runFileName}s");
		if (file.exists(runFileName)) file.remove(runFileName);
		return();
	}
	writeFile(runFileName, runFile);

	if (createReference) {
		assign('logger', print, testmeEnv);
		# run twice for possible vivifications; <i> test for vivifications
		#source(runFileName, chdir = T);
		#output = capture.output(source(runFileName, chdir = T), type = 'output');
		#writeFile(Sprintf('%{testBase}s/%{base}s_run.Rout.save', splitPath(testPath)), join(output, "\n"));
		dir = splitPath(runFileName)$dir;
		#SystemS('cd %{dir}q ; Rscript --vanilla %{runFileName}q', 2);
		#SystemS('cd %{dir}q ; Rscript --vanilla %{runFileName}q > %{testBase}q/%{base}q_run.Rout.save 2>&1',
		#	2);
		SystemS('cd %{dir}q ; R --silent --vanilla < %{runFileName}q', 2);
		SystemS('cd %{dir}q ; R --silent --vanilla < %{runFileName}q > %{testBase}q/%{base}q_run.Rout.save 2>&1', 2);
		#SystemS('cd %{dir}q ; Rscript %{runFileName}q', 2);
		#SystemS('cd %{dir}q ; Rscript %{runFileName}q > %{testBase}q/%{base}q_run.Rout.save 2>&1', 2);
		#print(output)
	}
}
InstallPackageTests = function(packageDir, testPathes, ...)
	lapply(testPathes, InstallPackageTest, packageDir = packageDir, ...);

#' Prepare test files for an R-package
#'
#' Takes pathes to test files and installs these tests in an R-package. Also creates required expectation files.
#'
#' @param packageDir path to R-package folder structure
#' @param testPathes character vector with files containing tests of the package
#' @param createReference boolean to indicate whether a reference output is to be created for R when the package tests are run by standard R functions.
#' @param asCran boolean to indicate whether tests should be prepared for running on CRAN
#' @return undefined return value.
#' @export installPackageTests
installPackageTests = function(packageDir, testPathes, createReference = TRUE, asCran = FALSE) {
	InstallPackageTests(packageDir, testPathes, createReference, asCran = asCran);
}

#' Run tests intalled in R-package
#'
#' This is a convenience function to call \code{runTests()} on the test folder of an R-package. The tests are assumed to have been installed with \code{installPackageTests} before. The package must be loaded via \code{library} before calling this function.
#'
#' @param packageName name of the R-package to be tested. Uses \code{system.file} to find package files.
#' @param packageDir alternatively to \code{packageName}, the package folder can be specified directly. Useful, for example, when testing a local developement tree.
#' @param isolateSession boolean to indicate whether tests should be run in seperate R session
#' @param useGit boolean to indicate whether git commit should be ran on the testFolder after running tests
#' @return value of runTests
#' @export installPackageTests
runPackageTests = function(packageName, packageDir, isolateSession = TRUE, useGit = FALSE) {
# 	if (!Sprintf('package:%{packageName}s') %in% search()) {
# 		loadNamespace(packageName);
# 		on.exit(unload(packageName));
# 	}
	testDir = if (missing(packageName))
		Sprintf('%{packageDir}s/tests/testme') else
		system.file('tests/testme', package = packageName);
	if (testDir == "") {
		LogS(1, 'Package %{packageName}q not loaded. Please load package first.');
		return(100);
	}
	runTests(testDir, useGit = useGit, isolateSession = isolateSession);
}

#
#	<p> initialization
#

#' Initialize testing environment
#'
#' Create package internal environment to hold global state. Calling this function is not required under normal operation. This function is called internally, when whole files or directories are tested. It needs to be called when single testing functions are to be evaluated in an ad-hoc fashion (see vignette).
#'
#' @param expectationsFolder Path to folder to store test expectations in. Defaults to the \code{RtestsExpectations} sub-folder of argument \code{d}.
#' @param d Base folder holding tests. Defauts to \code{tempdir()}
#' @param logger Function used to print messages. Defaults to \code{LogAt1} which prints to stderr and includes the date
#' @export testmeEnvInit
testmeEnvInit = function(expectationsFolder = Sprintf('%{d}s/RtestsExpectations', d = tempdir()),
	logger = LogAt1) {
	#testmeEnv <<- new.env();
	assign('expectationsFolder', expectationsFolder, testmeEnv);
	assign('Ndash', firstDef(options('testme')$Ndash, 100), testmeEnv);
	assign('logger', logger, testmeEnv);
	Dir.create(expectationsFolder, logLevel = 2);
	return(testmeEnv);
}

#
#	<p> git
#

gitCommitVivifications = function() {
	expectationsFolder = get('expectationsFolder', testmeEnv);
	expFiles = list.files(path = expectationsFolder, pattern = '[.](R|png)$', full.names = T);
	#gitLs = System(Sprintf('git ls-files %{expectationsFolder}s'), return.output = T)
	#files = pop(splitString('\n', gitLs$output));
	gitAdd = Sprintf('git add %{files}s', files = join(sapply(expFiles, qs)));
	System(gitAdd, 5);

	gitCommit = Sprintf('git commit -m "new test vivifications" %{files}s',
		files = join(sapply(expFiles, qs)));
	System(gitCommit, 5);
}

#
#	<p> report
#

testmePrintReport = function(rTests) {
	Ndash = get('Ndash', testmeEnv);
	sep = con(join(rep('-', Ndash), ''), "\n");
	# <p> unit files
	if (!all(c('result', 'NsubTests', 'Nvivified') %in% names(rTests[[1]]))) {
		cat(sep);
		N = length(rTests);
		Logs("#TestUnits: %{N}d [%{names}s]", names = join(names(rTests), ', '), logLevel = 1);
		cat(sep);
		names(rTests) = NULL;
		rTests = unlist.n(rTests, 1);
	}
	# <p> tests
	N = length(rTests);
	nms = names(rTests);
	Logs("#Tests: %{N}d [%{names}s]", names = join(nms, ', '), logLevel = 1);
	cat(sep);

	# <p> descriptives
	Nsub = sum(list.kpu(rTests, 'NsubTests'));
	Nviv = sum(list.kpu(rTests, 'Nvivified'));
	Logs("%{N}d tests performed, %{Nsub}d subtests, %{Nviv}d vivified", logLevel = 1);
	cat(sep);

	# <p> results
	r = as.logical(list.kpu(rTests, 'result'));
	failed = is.na(r) | !r;
	if (!any(failed)) Log("All tests passed.", 1) else {
		Log(Sprintf("#Failed tests: %{N}d [%{n}s]", N = sum(failed), n = join(nms[failed], sep = ', ')), 1)
	}
	cat(sep);

	return(!any(failed));
}

#
#	<p> file handling
#

findTestsFiles = function(Rfiles, testNamePattern = '_test$') {
	r = with(list(), {
		SourceLocal(Rfiles, envir = parent.frame());
		tests = MatchRegex('_test$', ls(envir = parent.frame()));
	});
	return(list(tests = r));
}
findTestsDir = function(dir, filePattern = '.R$', testNamePattern = '_test$') {
	Rfiles = list.files(dir, filePattern, full.names = TRUE);
	tests = findTestsFiles(Rfiles, testNamePattern = testNamePattern);
	return(c(tests, list(files = Rfiles)));
}

#
#	<p> auto-vivification
#

findVariables = function(patterns = c('^rTest\\d+$', '^T\\d+$'), which = -2) {
	ns = names(sys.frame(which));
	vs = lapply(patterns, FetchRegexpr, ns);
	unlist(vs)
}

TestPlot = function(plot, width = 20, height = 20, extension = 'png') {
	plotPath = tempfile('testingPlot', fileext = Sprintf('.%{extension}s'));
	Log.expr(3, plot_save(plot, plot_path = plotPath, width = width, height = height));
	return(plotPath);
}

readExpectationImage = function(path)return(Deparse(path));
# assume value to be a path returned by TestPlot
writeExpectationImage = function(path, value) {
	File.copy(value, path, symbolicLinkIfLocal = F);
	return(path);
}

readExpectationDeparse = function(path)return(readFile(path));
writeExpectationDeparse = function(path, value)writeFile(path, Deparse(value));

vivifyExtensions = list(deparse = 'R', image = 'png');

vivifyExpectation = function(test, expectationsFolder = 'RtestsExpectations', mode = 'deparse',
	sepChar = '+') with(test, {
	# <p> given as inline expectation
	if (length(expect) > 1) {
		stop('expectation should be deparsed expression');
		print(expect);
	}
	if (!is.na(expect)) return(test);

	# <p> vivfy from file
	pathExpectation = Sprintf('%{expectationsFolder}s/%{nameFunction}s%{sepChar}s%{name}s.%{ext}s',
		ext = vivifyExtensions[[mode]]);
	a = list(path = pathExpectation);
	if (file.exists(pathExpectation)) {
		LogS(5, 'Path expectation: %{pathExpectation}s [read]');
		return(merge.lists(test, list(expect = callDelegate('readExpectation', mode, a))));
	}
	# <p> create expectation
	#expectation = Deparse(value);
	#writeFile(pathExpectation, expectation);
	callDelegate('writeExpectation', mode, args = c(a, list(value = value)));
	LogS(3, 'Path expectation: %{pathExpectation}s [vivified] [mode=%{mode}s]');
	return(merge.lists(test, list(
		expect = callDelegate('readExpectation', mode, a), created = T)));
})

testFindExpectation = function(n, prefixDict = list(T = 'E', rTest = 'rExp'),
	which, nameFunction = NULL, mode = 'deparse') {
	nmT = Regexpr('(?<prefix>.*)(?<number>\\d+)', n, captures = T, concatMatches = F)[[1]];
	nmE = con(prefixDict[[nmT$prefix]], nmT$number);
	#print(names(sys.frame(which)));
	env = sys.frame(which);
	r = list(nameFunction = nameFunction, name = n,
		value = get(n, env), expect = mget(nmE, env, ifnotfound = NA)[[1]]);
	LogS(5, 'Test found: %{n}s');
	return(vivifyExpectation(r, expectationsFolder = get('expectationsFolder', testmeEnv),  mode = mode));
}

testsFindExpectation = function(ns, ..., which = -2, mode = list(), postfix = '_test') {
	# case distinction for Rtesting.R vs. direct call
	nmTestRaw = if (exists('testmeEnv'))
		get('name', testmeEnv) else
		deparse(sys.calls()[[sys.nframe() + which + 1]]);
	# <p> prettify name: remove postfix, trailing '()', if present
	re = Sprintf('(?<name>.*?)(?:%{postfix}s)?(?:\\(\\))?$');
	nmTest = Regexpr(re, nmTestRaw, captures = T, global = F);

	LogS(5, 'Test function: %{nmTestRaw}s [-> %{nmTest}s]');
	#r = lapply(ns, testFindExpectation, ..., which = which - 2, nameFunction = nmTest);
	r = lapply(ns, function(n) {
		testFindExpectation(n, ..., which = which - 2,
			nameFunction = nmTest, mode = firstDef(mode[[n]], 'deparse'))
	});
	return(setNames(r, list.kp(r, 'name')));
}

#' Run tests defined in the current function
#'
#' Searches the current function for test definitions, runs the tests and compares to expectations.
#'
#' @param mode list with modes of comparison. By default \code{compare} is used for the comparison. Elements with names of the test can overwrite the testing mode. Available modes are \code{c('compare', 'round8', 'equal', 'error', 'image')}.
#' @param which integer to indicate which environment relative to current is to be used to look for tests.
#' @return undefined return value
#' @examples
#' \dontrun{
#' myTests = function() {
#'   T1 = 1 + 1;
#'   T2 = sqrt(exp(10));
#'   TestMe(list(T2 = 'round8'));
#' }
#' }
#' @export TestMe
TestMe = function(mode = list(), which = -2) {
	tests = findVariables(which = which);
	#print(tests);
	exps = testsFindExpectation(tests, which = which - 1, mode = mode);
	#print(exps)
	TestsCompare(exps, mode);	
}

TestsCompareSingle = function(test, mode = 'compare') {
	return(with(test, Compare(value, try(Eval(expect), silent = T), mode)));
}

TestsCompare = function(tests, mode = list()) {
	comparisons = lapply(tests, function(test)TestsCompareSingle(test,
		mode = firstDef(mode[[test$name]], 'compare')));
	list(result = all(sapply(comparisons, isTRUE)),
		NsubTests = length(tests), Nvivified = length(list.kpu(tests, 'created'))
	)
}


#
#	<p> compare
#

Round = function(o, digits = 8) {
	if (class(o) %in% c('integer', 'complex', 'numeric')) return(round(o, digits = digits));
	if (is.list(o))return(lapply(o, Round, digits = digits));
	stop("Couldn't round");
}

compare_data.frame = function(a, b) {
	r = nlapply(a, function(n)compare(a[[n]], b[[n]])$result);
	all(unlist(r));
}
compare_Matrix = function(a, b) {
	cv = compare(Avu(a), Avu(b))$result;
	cn = compare(dimnames(a), dimnames(b))$result;
	return(all(c(cv, cn)));
}

compare_ = function(a, b) {
	#r = if (class(a) %in% c('matrix'))
	#	callDelegate('compare_', class(a), list(a = a, b = b)) else
	#	compare(a, b);
	print(c(class(a), class(b)));
	r = compare(a, b);
	return(r);
}

compareImage = function(a, b, threshold = 10) {
	diff = tempfile('testImageCompare', fileext = '.png');
	o = System(Sprintf('compare %{a}q %{b}q -metric RMSE %{diff}q'), 5, return.error = T);
	diff = Regexpr('\\d*([.]\\d+)?', o$output.err);
	return(diff < threshold);
}

Compare = function(a, b, mode = NULL, do.print = TRUE, logger = LogAt1) {
	if (class(b) == 'try-error' && class(a) == 'try-error') mode = 'error';
	if (is.null(mode)) mode = 'compare';
	r = switch(mode,
		'error' = compare(0L, 0L),
		'equal' = compareEqual(a, b),
		'compare' = compare(a, b),
		'round8' = compare(Round(a, 8), Round(b, 8)),
		'image' = compareImage(a, b)
	);
	if (do.print && !compare::isTRUE(r)) {
		Log = Mget('logger', testmeEnv, ifnotfound = logger);
		Log(join(c('*** Compare report start ', rep('*', 45)), ''));
		Log(Sprintf('Comparion [%{mode}s] resulted in unequal result'));
		Log('Comparison result');
		Log(compare::isTRUE(r));
		Log('Comparison object [a]');
		Log(a);
		Log('Comparison object [b]');
		Log(b);
		Log(join(c('--- Compare report end ---', rep('-', 45)), ''));
	}
	return(compare::isTRUE(r));
}

TestCompare = function(result, expectation, modes = as.list(rep('compare', length(result)))) {
	comparisons = ilapply(result, function(r, i)Compare(r, expectation[[i]], mode = modes[[i]]));
	list(result = all(sapply(comparisons, isTRUE)), NsubTests = length(result))
}

#' Legacy internal test function (TestCompareDeparsed)
#'
#' This function takes reult/expectation arguments and performs a comparison.
#'
#' @param result list of results of the current test function
#' @param expectation list of Deparsed, expected results
#' @param modes comparison modes
TestCompareDeparsed = function(result, expectation, modes = as.list(rep('compare', length(result)))) {
	TestCompare(result, lapply(expectation, function(e)try(eval(parse(text = e)), silent = T)), modes = modes)
}

#
#	<p> legacy functions
#

runTestMe = function(tests, logLevel = 4) {
	Ndash = get('Ndash', testmeEnv);
	sep = con(join(rep('-', Ndash), ''), "\n");
	cat(sep);
	N = length(tests);
	Logs("%{N}d tests found: %{names}s", names = join(tests, ', '), logLevel = 1);
	cat(sep);

	rTests = sapply(tests, runTestFunction);
	cat(sep);
	Nsub = sum(list.kpu(rTests, 'NsubTests'));
	Nviv = sum(list.kpu(rTests, 'Nvivified'));
	Logs("%{N}d tests performed, %{Nsub}d subtests, %{Nviv}d vivified", logLevel = 1);
	cat(sep);
	r = sapply(list.kp(rTests, 'result'), isTRUE);
	allGood = all(r);
	if (allGood) Log("All tests passed", 1) else {
		Log(Sprintf("Failing tests: %{tests}s", tests = join(tests[!unlist(r)], sep = ', ')), 1)
	}
	return(allGood);
}

#' Initialze test environment and run tests
#'
#' Internal function for initialzing test environment and running tests stored in a folder
#'
#' @param testsFolder Folder to be tests. Default folder is taken from `options('testme')$testme$testsFolder`.
#' @param expectationsFolder Folder where expectations are stored. Default folder is taken from `options('testme')$testme$expectationsFolder`.
#' @param Ndash integer to set length of visual separators
#' @param useGit boolean to indicate whether vivified expectations should be committed using git
#' @param logLevel integer to indicate verbosity of logging information
#' @return returns 0 on success, value greater 0 if tests failed
#' @export runTestsInternal
runTestsInternal = function(
	testsFolder = firstDef(options('testme')$testme$testsFolder, './Rtests'),
	expectationsFolder = options('testme')$testme$expectationsFolder,
	useGit = TRUE, Ndash = 1e2, logLevel = 4) {

	if (is.null(expectationsFolder)) expectationsFolder = Sprintf('%{testsFolder}s/RtestsExpectations');
	# <p> create clean environment
	# make expectationsFolder absolute due to later chdir
	testmeEnvInit(splitPath(expectationsFolder)$absolute);

	Log.setLevel(logLevel);
	# <p> locate tests, source tests
	nms = findTestsDir(testsFolder);
	tests = nms$tests;
	SourceLocal(nms$files);

	# <p> git
	if (useGit) gitCommitVivifications();
	# <p> start testing
	allGood = exprInDir(runTestMe(tests, logLevel), testsFolder);
	return(allGood);
}
	
runTestsRTemplate = "library('testme');\nlibrary('methods');\nlibrary('compare');\n%{src}s\nallGood = testme:::runTestsInternal(%{testsFolder}t, %{expectationsFolder}t, useGit = %{useGit}s);\n";
runTestsRTemplateI = join(c(runTestsRTemplate, "quit(status = ifelse(allGood, 0, 100));\n"), "\n");

#' Run tests in isolation
#'
#' Runs tests in a folder in a separate R session.
#'
#' @param testsFolder Folder to be tests. Default folder is taken from `options('testme')$testme$testsFolder`.
#' @param expectationsFolder Folder where expectations are stored. Default folder is taken from `options('testme')$testme$expectationsFolder`.
#' @param sourceFiles pathes to files that should be sourced prior to running the tests. These would typically hold initialization code.
#' @param isolateSession boolean to indicate whether a new R session should be started (using `Rscript`) or whether to run tests in the current R session
#' @param useGit logical to indicate whether test vivifications should be commited
#' @return returns 0 on success, value greater 0 if tests failed
#' @examples
#' \dontrun{
#'   runTests()
#' }
#' @export runTests
runTests = function(
	testsFolder = firstDef(options('testme')$testme$testsFolder, './Rtests'),
	expectationsFolder = options('testme')$testme$expectationsFolder,
	sourceFiles = options('testme')$testme$sourceFiles,
	isolateSession = TRUE,
	useGit = TRUE) {

	if (isolateSession) {
		src = join(Sprintf('source(%{sourceFiles}t, chdir = TRUE);'), "\n");
		tmpsrc = tempfile();
		writeFile(tmpsrc, Sprintf(ifelse(isolateSession, runTestsRTemplateI, runTestsRTemplate)));
		SystemS('Rscript %{tmpsrc}q')
	} else {
		SourceLocal(sourceFiles);
		runTestsInternal(testsFolder, expectationsFolder, useGit = useGit);
	}
}


getComparePairs = function(prefixes = c('rTest', 'rExp'), envir) {
	#print(names(envir));
	N = FetchRegexpr(Sprintf('^%{search}s(\\d+)$', search = prefixes[1]), names(envir), captures = T);
	if (!length(N)) return(NULL);
	N = sort(N);
	r = lapply(prefixes, function(prefix)lapply(paste0(prefix, N), get, envir = envir));
	names(r) = c('test', 'expectation');
	r
}

#' Legacy internal test function (getTests)
#'
#' This function takes a pair of reults/expectation and performs a comparison.
#'
#' @param prefixes variable name prefixes for detecting test computations and expectations
#' @param which which `sys.frame` to search
#' @return a list with elements `tests` and `expectation` with character strings of the corresponding test/expectation pairs
#' @details Call as argument to TestCompareDeparsedList(getTests()), otherwise which argument has to be modified; e.g.: separate line before: which = -1
getTests = function(prefixes = list(c('rTest', 'rExp'), c('T', 'E')), which = -2) {
	env = sys.frame(which);
	pairs = lapply(prefixes, getComparePairs, envir = env);
	pairs = pairs[!sapply(pairs, is.null)];
	r = list(test = list.kp(pairs, 'test', n = 1), expectation = list.kp(pairs, 'expectation', n = 1));
	r
}

#' Legacy internal test function (TestCompareDeparsedList)
#'
#' This function takes a pair of reults/expectation and performs a comparison.
#'
#' @param pair list with elements `test` (current computation) and `expectation` (deparsed, expected result) to be compared
#' @param modes comparison modes
TestCompareDeparsedList = function(pair, modes = as.list(rep('compare', length(result)))) {
	result = pair$test;
	expectation = pair$expectation;
	TestCompare(result, lapply(expectation, function(e)try(eval(parse(text = e)), silent = T)), modes = modes)
}
