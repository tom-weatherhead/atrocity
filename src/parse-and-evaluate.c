/* atrocity/src/parse-and-evaluate.c */

#include <stdlib.h>
#include <stdio.h>
/* #include <string.h> */
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "parser.h"
#include "evaluate.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Functions */

/* TODO: Try to eliminate code duplication in this file. */

LISP_VALUE * parseStringAndEvaluate(char * str, LISP_ENV * globalEnv) {
	CharSource * cs = createCharSource(str);
	LISP_EXPR * parseTree = parseExpression(cs);
	LISP_VALUE * value = evaluate(parseTree, globalEnv);

	/* TODO? : freeExpression(parseTree); */
	freeCharSource(cs);

	return value;
}

/* This function is used to populate the global environment with
'built-in' functions. */
void parseAndEvaluateEx(char * str, LISP_ENV * globalEnv, BOOL verbose) {
	failIf(globalEnv == NULL, "globalEnv is NULL");

	LISP_ENV * originalGlobalEnv = globalEnv;

	if (verbose) {
		printf("\nInput: '%s'\n", str);
	}

	if (globalEnv == NULL) {
		globalEnv = createGlobalEnvironment();
	}

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	LISP_VALUE * value = parseStringAndEvaluate(str, globalEnv);
	/* CharSource * cs = createCharSource(str);
	LISP_EXPR * parseTree = parseExpression(cs);
	LISP_VALUE * value = evaluate(parseTree, globalEnv); */

	if (verbose) {
		printf("Output: ");
		printValue(value);
		printf("\n");
	}

	/* Note bene: freeClosure is currently mostly disabled to avoid
	 * double-freeing things. We must fix this. */
	freeValue(value);
	/* freeExpression(parseTree);
	freeCharSource(cs); */

	if (originalGlobalEnv == NULL) {
		freeGlobalEnvironment(globalEnv);
	}
}

/* **** The End **** */
