#
#	RtestingHelpers.R
#Thu Aug 16 17:46:14 CEST 2018

#
#	<p> package definition
#

packageDefinition = list(
	name = 'testme',
	files = c('Rmeta.R', 'Rdata.R', 'Rsystem.R'),
	#instFiles = list(Rscripts = 'Dev/pkg-minimal.R'),
	description = list(
		title = 'Rapid development of software tests',
		# version to be documented in news section
		#version = '0.1-0',
		author = 'Stefan Böhringer <r-packages@s-boehringer.org>',
		description = 'Simplify unit and integrated testing by using implicit definitions. When writing new functions, users usually use example invocations for checking. Exactly this should be and is enough to develop tests using `testme`. Use `?"package-testme"` for a tutorial.',
		depends = c('roxygen2', 'devtools'),
		suggests = c('jsonlite', 'yaml'),
		news = "0.1-0	Initial release"
	),
	git = list(
		readme = '## Installation\n```{r}\nlibrary(devtools);\ninstall_github("sboehringer/testme")\n```\n',
		push = F,
		pushOnNewVersion = T,
		remote = 'https://github.com/sboehringer/testme.git'
	)
);

#__PACKAGE_DOC__
# This package.
# @examples
# \dontrun{
#  file.show(system.file('Rscripts/pkg-minimal.R', package = 'package'))
#  createPackage(system.file('Rscripts/pkg-minimal.R', package = 'package'))
# }
# @seealso {createPackage()} for starting the main workflow
#__PACKAGE_DOC_END__

#
#	<p> initialization
#

Library('compare');
# <!><A><N> temporary disabling of reset for debugging
#testmeEnv = new.env();

#
#	<p> global interface
#

runTestFunctionSingle = function(testName) {
	assign('name', testName, testmeEnv);	# global variable holding the test name
	testFunction = get(testName);
	rTest = try(testFunction());
	r = if (is.list(rTest)) {
		Log(Sprintf("Test: %{testName}s [N = %{N}d]", N = rTest$NsubTests), 1);
		rTest$result;
	} else {
		# legacy test
		Log(Sprintf("Test: %{testName}s [N = ?], legacy"), 1);
		rTest
	}
	if (class(r) == 'try-error' || !isTRUE(r)) Log(Sprintf("********* test: %{testName}s failed"), 1)
	r = if (class(r) == 'try-error') FALSE else if (class(r) == 'comparison') isTRUE(r) else r;
	list(if (is.list(rTest)) rTest else list(result = rTest, NsubTests = 1))
}
runTestFunction = Vectorize(runTestFunctionSingle, 'testName');

testmeFileSingle = function(file, expectationsFolder, useGit, print = F) {
	testmeEnvInit(expectationsFolder = splitPath(expectationsFolder)$absolute);
	owd = setwd(splitPath(file)$dir);
	on.exit(setwd(owd));

	tests = findTestsFiles(splitPath(file)$file)$tests;
	Source(splitPath(file)$file);
	
	# <p> git
	if (useGit) gitCommitVivifications();
	# <p> start testing
	#allGood = runTestMe(tests, logLevel);
	rTests = RunTestFunction(tests);
	if (print) testmePrintReport(rTests);
	return(rTests);
}
testmeFile = Vectorize(testmeFileSingle, 'file');

#' Run all tests defined in a folder
#'
#' Collect files from a folder using a pattern, extract testing functions and run tests.
#'
#' @export
testmeDir = function(dir = 'Rtests', expectationsFolder = 'Rtests/RtestsExpectations',
	filePattern = '.R$', useGit = T, logLevel = 4, print = T) {
	Rfiles = list.files(dir, filePattern, full.names = TRUE);
	rTests = TestmeFile(Rfiles, expectationsFolder, useGit = F);
	# <p> git
	if (useGit) gitCommitVivifications();
	if (print) testmePrintReport(rTests);
	return(rTests);
}

#
#	<p> initialization
#

testmeEnvInit = function(expectationsFolder = Sprintf('%{d}s/RtestsExpectations', d = tempdir())) {
	testmeEnv <<- new.env();
	assign('expectationsFolder', expectationsFolder, testmeEnv);
	assign('Ndash', firstDef(options('testme')$Ndash, 100), testmeEnv);
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
		Source(Rfiles, envir = parent.frame());
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

vivifyExpectation = function(test, pathExpect = 'RtestsExpectations', mode = 'deparse') with(test, {
	# <p> given as inline expectation
	if (length(expect) > 1) {
		stop('expectation should be deparsed expression');
		print(expect);
	}
	if (!is.na(expect)) return(test);

	# <p> vivfy from file
	pathExpectation = Sprintf('%{pathExpect}s/%{nameFunction}s:%{name}s.%{ext}s',
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
	return(vivifyExpectation(r, mode = mode));
}

testsFindExpectation = function(ns, ..., which = -2, mode = list()) {
	# case distinction for Rtesting.R vs. direct call
	nmTestRaw = if (exists('testmeEnv'))
		get('name', testmeEnv) else
		deparse(sys.calls()[[sys.nframe() + which + 1]]);
	nmTest = Regexpr('(?<name>.*)_test(?:\\(\\))?$', nmTestRaw, captures = T, global = F);

	LogS(5, 'Test function: %{nmTestRaw}s [-> %{nmTest}s]');
	#r = lapply(ns, testFindExpectation, ..., which = which - 2, nameFunction = nmTest);
	r = lapply(ns, function(n) {
		testFindExpectation(n, ..., which = which - 2,
			nameFunction = nmTest, mode = firstDef(mode[[n]], 'deparse'))
	});
	return(setNames(r, list.kp(r, 'name')));
}

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
	if (class(o) %in% c('complex', 'numeric')) return(round(o, digits = digits));
	if (is.list(o))return(lapply(o, Round, digits = digits));
	stop("Couldn't round");
}

compare_data.frame = function(a, b) {
	r = nlapply(a, function(n)compare(a[[n]], b[[n]]));
	all(unlist(r));
}
compare_Matrix = function(a, b) {
	cv = compare(Avu(a), Avu(b));
	cn = compare(dimnames(a), dimnames(b));
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

Compare = function(a, b, mode = NULL, do.print = TRUE) {
	if (class(b) == 'try-error' && class(a) == 'try-error') mode = 'error';
	if (is.null(mode)) mode = 'compare';
	r = switch(mode,
		'error' = compare(0L, 0L),
		'equal' = compareEqual(a, b),
		'compare' = compare(a, b),
		'round8' = compare(Round(a, 8), Round(b, 8)),
		'image' = compareImage(a, b)
	);
	if (do.print && !isTRUE(r)) {
		Logs(join(c('*** Compare report start ', rep('*', 45)), ''), logLevel = 1);
		Logs('Comparion [%{mode}s] resulted in unequal result', logLevel = 1);
		Logs('Comparison object [a]', logLevel = 1);
		print(a);
		Logs('Comparison object [b]', logLevel = 1);
		print(b);
		Logs(join(c('--- Compare report end ---', rep('-', 45)), ''), logLevel = 1);
	}
	r
}

TestCompare = function(result, expectation, modes = as.list(rep('compare', length(result)))) {
	comparisons = ilapply(result, function(r, i)Compare(r, expectation[[i]], mode = modes[[i]]));
	list(result = all(sapply(comparisons, isTRUE)), NsubTests = length(result))
}

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

runTestsInternal = function(Ndash = 1e2, dir = 'Rtests',
	useGit = TRUE, expectationsFolder = 'RtestsExpectations', logLevel = 4) {

	# <p> create clean environment
	testmeEnvInit(expectationsFolder);

	# <p> change working directory
	owd = getwd();

	Log.setLevel(logLevel);
	# <p> locate tests, source tests
	nms = findTestsDir(dir);
	tests = nms$tests;
	Source(nms$files);
	setwd(dir);

	# <p> git
	if (useGit) gitCommitVivifications();
	# <p> start testing
	allGood = runTestMe(tests, logLevel);
	if (notE(owd)) setwd(owd)
	return(allGood);
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

# call as argument to TestCompareDeparsedList(getTests())
#	otherwise which argument has to be modified; e.g.: separate line before: which = -1
getTests = function(prefixes = list(c('rTest', 'rExp'), c('T', 'E')), which = -2) {
	env = sys.frame(which);
	pairs = lapply(prefixes, getComparePairs, envir = env);
	pairs = pairs[!sapply(pairs, is.null)];
	r = list(test = list.kp(pairs, 'test', n = 1), expectation = list.kp(pairs, 'expectation', n = 1));
	r
}

# pair: expect result of getTests
TestCompareDeparsedList = function(pair, modes = as.list(rep('compare', length(result)))) {
	result = pair$test;
	expectation = pair$expectation;
	TestCompare(result, lapply(expectation, function(e)try(eval(parse(text = e)), silent = T)), modes = modes)
}