#
#	testme.R
#Tue Feb 11 16:59:34 CET 2020

# testing of package testme is not straightforward as a global state is maintained in testmeEnv
# preventing to call test internal functions within tests. The approach taken is to test functions
# which are indpendent of this global state and use some generic tests for integrative testing

findVariables_test = function() {
	C1 = 1;
	C2 = 2;
	C4 = 4;
	rC7 = 2;

	T1 = testme:::findVariables(patterns = c('^rC\\d+$', '^C\\d+$'), which = -1);

	TestMe();
}


integrative_test = function() {
	T1 = 1 + 1;

	T2 = 3;
	E2 = 3;

	T3 = try(if (4 != E2) stop(), silent = T);

	TestMe();
}

Round_test = function() {
	T1 = try(testme:::Round(NULL), silent = T);
	T2 = testme:::Round(pi*1:4);
	T3 = testme:::Round(pi*1:4, digits = 2);
	#T4 = Round(1:4);	# integer not yet supported

	TestMe();
}

compare_test = function() {
	T1 = compare::compare(matrix(1), matrix(1));
	T2 = compare::compare(matrix(1), matrix(1:2));
	T3 = compare::compare(data.frame(a = 1), data.frame(a = 1));
	T4 = compare::compare(data.frame(a = 1), data.frame(b = 1));
	T5 = capture.output(compare::compare(NULL, NA));

	TestMe();
}
