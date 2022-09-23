/* atrocity/src/parse-and-evaluate.c */

#include <stdlib.h>
#include <stdio.h>

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"
#include "parser.h"
#include "evaluate.h"
#include "print.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Functions */

LISP_VALUE * parseStringAndEvaluate(char * str, LISP_ENV * globalEnv) {
	CharSource * cs = createCharSource(str);
	LISP_EXPR * parseTree = parseExpression(cs);
	LISP_VALUE * value = evaluate(parseTree, globalEnv);

	freeCharSource(cs);

	return value;
}

/* This function is used to populate the global environment with
'built-in' functions. */
void parseAndEvaluateEx(char * str, LISP_ENV * globalEnv, BOOL verbose) {
	failIf(globalEnv == NULL, "globalEnv is NULL");
	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	if (verbose) {
		printf("\nInput: '%s'\n", str);
	}

	LISP_VALUE * value = parseStringAndEvaluate(str, globalEnv);

	if (verbose) {
		printf("Output: ");
		printValue(value);
		printf("\n");
	}
}

/* **** The End **** */
