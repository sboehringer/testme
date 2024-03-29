#
#	Rmeta.R
#Wed Jun  3 15:11:27 CEST 2015
#Mon 27 Jun 2005 10:49:06 AM CEST
#system("~/src/Rprivate/exportR.sh");
#system("~/src/Rprivate/exportR.sh"); source("RgenericAllRaw.R"); source("Rgenetics.R"); loadLibraries();
#system('. ~/src/Rprivate/exportR.sh ; cp ~/src/Rprivate/RgenericAllRaw.R .');

#
#	<p> Meta-helpers
#

#
#	level dependend logging
# moved from Rsystem.R to break dependency cylce (22.3.2017)
#Global..Log..Level = 4;
#Default..Log..Level = 4;
#assign(Default..Log..Level, 4, envir = .GlobalEnv);
Log_env__ <- new.env();
assign('DefaultLogLevel', 4, envir = Log_env__);

# <p> work-arounds for CRAN submissions
# capture.output replaces \n with space
Capture.output = function(expr, envir = parent.frame()) {
	tf = tempfile();
	sink(tf);
	on.exit(sink());
	r = eval(expr, envir = envir);
	return(readFile(tf));
}
Print = function(..., envir = parent.frame())Capture.output(print(...), envir = envir)

#' Log a message to stderr.
#' 
#' Log a message to stderr. Indicate a logging level to control verbosity.
#' 
#' This function prints a message to stderr if the condition is met that a
#' global log-level is set to greater or equal the value indicated by
#' \code{level}. \code{Log.level} returns the current logging level.
#' 
#' @aliases Log Log.setLevel Log.level
#' @param o Message to be printed.
#' @param level If \code{Log.setLevel} was called with this value, subsequent
#' calls to \code{Log} with values of \code{level} smaller or equal to this
#' value will be printed.
#' @param doPrint additional object that will be output using \code{print}.
#' @author Stefan Böhringer <r-packages@@s-boehringer.org>
#' @seealso \code{\link{Log.setLevel}}
#' @return inivisble formatted message or NULL if nothing was logged
#' @keywords io logging
# #' @examples
# #' \dontrun{
# #' 	Log.setLevel(4);
# #' 	Log('hello world', 4);
# #' 	Log.setLevel(3);
# #' 	Log('hello world', 4);
# #' }
Log = function(o, level = get('DefaultLogLevel', envir = Log_env__), doPrint = NULL) {
	if (level <= get('GlobalLogLevel', envir = Log_env__)) {
		#cat(sprintf("R %s: %s\n", date(), as.character(o)));
		message(sprintf("R %s: %s\n", date(), as.character(o)));
		if (!is.null(doPrint)) message(Print(doPrint));
	}
}
Logs = function(o, level = get('DefaultLogLevel', envir = Log_env__), ..., envir = parent.frame()) {
	Log(Sprintf(o, ..., envir = envir), level = level);
}
LogS = function(level, s, ..., envir = parent.frame()) {
	Log(Sprintf(s, ..., envir = envir), level = level);
}

Log.level = function()get('GlobalLogLevel', envir = Log_env__);
Log.setLevel = function(level = get('GlobalLogLevel', envir = Log_env__)) {
	assign("GlobalLogLevel", level, envir = Log_env__);
}
Log.expr = function(level, expr, envir = parent.frame()) {
	oldLevel = Log.level();
	on.exit(Log.setLevel(oldLevel));
	Log.setLevel(level);
	return(eval(expr, envir = envir));
}

Log.setLevel(4);	# default


Stop = function(..., call. = TRUE, domain = NULL, envir = parent.frame()) {
	stop(Sprintf(list(...)[[1]], envir = envir), call., domain)
}

#
#	<p> Meta-functions
#

#
#		Environments
#

# copy functions code adapted from restorepoint R package
object.copy = function(obj) {
	# Dealing with missing values
	if (is.name(obj)) return(obj);
	obj_class = class(obj);

	copy =
		if ('environment' %in% obj_class) environment.copy(obj) else
		if (all('list' == class(obj))) list.copy(obj) else
		#if (is.list(obj) && !(is.data.frame(obj))) list.copy(obj) else
		obj;
	return(copy)
}
list.copy = function(l)lapply(l, object.copy);
environment.restrict = function(envir__, restrict__= NULL) {
	if (!is.null(restrict__)) {
		envir__ = as.environment(List_(as.list(envir__), min_ = restrict__));
	}
	envir__
}
environment.copy = function(envir__, restrict__= NULL) {
	as.environment(eapply(environment.restrict(envir__, restrict__), object.copy));
}

bound_vars = function(f, functions = FALSE) {
	fms = formals(f);
	# variables bound in default arguments
	vars_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e)))));
	# variables used in the body
	vars_body = setdiff(all.vars(body(f)), names(fms));
	vars = setdiff(unique(c(vars_defaults, vars_body)), c('...', '', '.GlobalEnv'));
	if (functions) {
		vars = vars[!sapply(vars, function(v)is.function(rget(v, envir = environment(f))))];
	}
	vars
}
bound_fcts_std_exceptions = c('Lapply', 'Sapply', 'Apply');
bound_fcts = function(f, functions = FALSE, exceptions = bound_fcts_std_exceptions) {
	fms = formals(f);
	# functions bound in default arguments
	fcts_defaults = unique(unlist(sapply(fms, function(e)all.vars(as.expression(e), functions = TRUE))));
	# functions bound in body
	fcts = union(fcts_defaults, all.vars(body(f), functions = TRUE));
	# remove variables
	#fcts = setdiff(fcts, c(bound_vars(f, functions), names(fms), '.GlobalEnv', '...'));
	fcts = setdiff(fcts, c(bound_vars(f, functions = functions), names(fms), '.GlobalEnv', '...'));
	# remove functions from packages
	fcts = fcts[
		sapply(fcts, function(e) {
			f_e = rget(e, envir = environment(f));
			!is.null(f_e) && environmentName(environment(f_e)) %in% c('R_GlobalEnv', '') && !is.primitive(f_e)
	})];
	fcts = setdiff(fcts, exceptions);
	fcts
}


environment_evaled = function(f, functions = FALSE, recursive = FALSE) {
	vars = bound_vars(f, functions);
	e = nlapply(vars, function(v) rget(v, envir = environment(f)));
	#Log(sprintf('environment_evaled: vars: %s', join(vars, ', ')), 7);
	#Log(sprintf('environment_evaled: functions: %s', functions), 7);
	if (functions) {
		fcts = bound_fcts(f, functions = TRUE);
		fcts_e = nlapply(fcts, function(v){
			#Log(sprintf('environment_evaled: fct: %s', v), 7);
			v = rget(v, envir = environment(f));
			#if (!(environmentName(environment(v)) %in% c('R_GlobalEnv')))
			v = environment_eval(v, functions = TRUE);
		});
		#Log(sprintf('fcts: %s', join(names(fcts_e))));
		e = c(e, fcts_e);
	}
	#Log(sprintf('evaled: %s', join(names(e))));
	r = new.env();
	lapply(names(e), function(n)assign(n, e[[n]], envir = r));
	#r = if (!length(e)) new.env() else as.environment(e);
	parent.env(r) = .GlobalEnv;
	#Log(sprintf('evaled: %s', join(names(as.list(r)))));
	r
}
environment_eval = function(f, functions = FALSE, recursive = FALSE) {
	environment(f) = environment_evaled(f, functions = functions, recursive = recursive);
	f
}

#
#	Parsing, evaluation
#

Parse = function(text, ...) {
	parse(text = text, ...)
}
Eval = function(e, ..., envir = parent.frame(), autoParse = TRUE) {
	if (autoParse && is.character(e)) e = Parse(e, ...);
	eval(e, envir = envir)
	
}

#
#		Freeze/thaw
#
# -> moved to Ext


#
#	</p> freeze/thaw functions
#

#
#	<p> calls
#
# <!> assume matched call
# <A> we only evaluate named args
callEvalArgs = function(call_, env_eval = FALSE) {
	#if (is.null(call_$envir__) || is.null(names(call_$args))) return(call_);
	#if (is.null(call_$envir) || !length(call_$args)) return(call_);

	# <p> evaluate args
	if (length(call_$args)) {
		args = call_$args;
		callArgs = lapply(1:length(args), function(i)eval(args[[i]], envir = call_$envir));
		# <i> use match.call instead
		names(callArgs) = setdiff(names(call_$args), '...');
		call_$args = callArgs;
	}

	if (env_eval) {
		call_$fct = environment_eval(call_$fct, functions = FALSE, recursive = FALSE);
	}
	# <p> construct return value
	#callArgs = lapply(call_$args, function(e){eval(as.expression(e), call_$envir)});
	call_
}

#callWithFunctionArgs = function(f, args, envir__ = parent.frame(), name = NULL) {
callWithFunctionArgs = function(f__, args__, envir__ = environment(f__), name = NULL, env_eval = FALSE) {
	if (env_eval) f = environment_eval(f__, functions = FALSE, recursive = FALSE);
	call_ = list(
		fct = f__,
		envir = environment(f__),
		args = args__,
		name = name
	);
	call_
}
#
#	</p> calls
#

isClosure = function(call_) {
	if (is.symbol(call_)) return(FALSE);
	# (\(.)expr(.)))
	return(length(call_) == 1 || (call_[[1]] == 'function'));
}

functionFromExpr = function(expr, base = function(.).) {
	body(base) = expr;
	return(base);
}
# function or call
functionName = function(fOrC) {
	if (class(fOrC) == 'function') NULL else as.character(fOrC[[1]])
}
# function or name
functionByName = function(name, call__, envir__) {
	if (class(call__) == 'function') call__ else get(name, envir = envir__)
}

# call__
#	function name: e.g. which, is.logical ...
#	closure: function call with curried arguments: Slice(I = 1)
#	expression: expression with var `.`, e.g. Levels[.]
encapsulateCall = function(call__, ..., envir__ = environment(call__), do_evaluate_args__ = FALSE,
	unbound_functions = FALSE) {
	# closure
	isClosure = isClosure(call__);
	#print(list(call = call__, isClosure = isClosure))
	# function body of call
	name = functionName(call__);
	# closure or named function
	fct = functionByName(name, call__, envir__);
	callm = if (isClosure) NULL else if (!is.primitive(fct)) {
		callm = match.call(definition = fct, call = call__);
		as.list(callm)[-1]
	} else as.list(call__)[-1];
	args = if (do_evaluate_args__) {
		nlapply(callm, function(e)eval(callm[[e]], envir = envir__))
	} else {
		#nlapply(callm, function(e)callm[[e]])
		# <A> changed 1.12.2022
		callm
	}
	# <N> this recursion should be handles by the language parser
	if (!isClosure && name == '%.%') {
		composition = eval(call__, envir = envir__);
		return(encapsulateCall(composition, envir__ = envir__));
	}
	# unbound variables in body fct
	#unbound_vars = 
	call_ = list(
		fct = fct,
		envir = envir__,

		#args = as.list(sys.call()[[2]])[-1],
		args = args,

		name = name
	);
	return(call_);
}

encapsulateExpr = function(expr, ..., envir__ = environment(expr), do_evaluate_args__ = FALSE, unbound_functions = FALSE) {
	# expression; <N> cannot be detected by is.expression
	#	anonymous functions have to be encapsulated in '(' and ')'
	#	due to binding rules
	# precondition: class(expr) == '(')
	call_ = if (any(as.character(expr[[2]][[1]]) != 'function')) {
		functionFromExpr(expr[[2]]);
	} else {	# closure, remove '(', body
		functionFromExpr(expr[[2]][[3]]);
	}
	return(encapsulateCall(call_, envir__ = envir__));
}

# fct_: symbol of the function, envir_get: environmen to get the function from using get
encapsulateFunction = function(fct_, envir__ = envir__, envir_get = envir__) {
	list(
		fct = if (isClosure(fct_)) fct_ else get(fct_, envir = envir_get),
		envir = envir__,
		args = list(),
		name = as.character(fct_)
	)
}

encapsulateCallOrFunction = function(call_, envir__, envir_get) {
	if (is.symbol(call_)) {
		encapsulateFunction(call_, envir__, envir_get)
	} else if (class(call_) == '(') {
		encapsulateExpr(call_, envir__ = envir__)
	} else if (is.call(call_)) {
		encapsulateCall(call_, envir__ = envir__)
	} else	# raw values might be passed
		call_
}

composeEncapsulated = function(F2, F1) {
	#print(list(F2 = F2, F1 = F1));
	function(...) {
		r1 = do.call(F1$fct, args = c(F1$args, list(...)), envir = F1$envir);
		r2 = do.call(F2$fct, args = c(list(r1), F2$args), envir = F2$envir);
		return(r2);
	}
}

`%.%` <- compose <- function(f2, f1, envir__ = parent.frame(), envir_get = parent.frame()) {
	F2  = encapsulateCallOrFunction(sys.call()[[2]], envir__ = envir__, envir_get = parent.frame());
	F1  = encapsulateCallOrFunction(sys.call()[[3]], envir__ = envir__, envir_get = parent.frame());
	return(composeEncapsulated(F2, F1));
}

Id = identity;

#' Deparsing of expression
#'
#' Create single character string from R expression
#'
#' Calls deparse on argument and pastes together the return value of \code{deparse()} resulting
#' in a single character string. Operates very similar to \code{dput()}, except, it cannot write to
#' a file.
#'
#' @param o Expression/Object to be deparsed
#'
#' @return single character vector with the deparsed expression
#' @seealso [deparse()] which this function wraps
#' @seealso [eval()] for the inverse operation
#' @seealso [dput()] similar function
#' @seealso [dget()] similar to eval of character string
#' @return character string with the deparsed R-object
#' @examples
#' \dontrun{
#'	Deparse(3)
#'	Deparse(1 + 2)
#'	Deparse(matrix(1:10, ncol = 5))
#'	eval(Deparse(matrix(1:10, ncol = 5)))
#' }
Deparse = function(o)join(deparse(o));
