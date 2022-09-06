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

void parseAndEvaluateEx(char * str, LISP_ENV * globalEnv, BOOL verbose) {
	LISP_ENV * originalGlobalEnv = globalEnv;

	if (verbose) {
		printf("\nInput: '%s'\n", str);
	}

	if (globalEnv == NULL) {
		globalEnv = createGlobalEnvironment();
	}

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	CharSource * cs = createCharSource(str);
	LISP_EXPR * parseTree = parseExpression(cs);
	LISP_VALUE * value = evaluate(parseTree, globalEnv);

	if (verbose) {
		printf("Output: ");
		printValue(value);
		printf("\n");
	}

	/* Note bene: freeClosure is currently mostly disabled to avoid
	 * double-freeing things. We must fix this. */
	freeValue(value);
	freeExpression(parseTree);
	freeCharSource(cs);

	if (originalGlobalEnv == NULL) {
		freeGlobalEnvironment(globalEnv);
	}
}

/* void parseAndEvaluate(char * str) {
	/ * LISP_VALUE * value = * / parseAndEvaluateEx(str, NULL, TRUE);
} */

/* void parseAndEvaluateStringList(char * strs[]) {
	LISP_ENV * globalEnv = createGlobalEnvironment();
	int i;

	for (i = 0; ; ++i) {
		char * str = strs[i];

		if (str == NULL) {
			break;
		}

		printf("\nInput %d: '%s'\n", i, str);

		CharSource * cs = createCharSource(str);
		LISP_EXPR * parseTree = parseExpression(cs);
		LISP_VALUE * value = evaluate(parseTree, globalEnv);

		printf("Output %d: ", i);
		printValue(value);
		printf("\n");

		freeCharSource(cs);
	}

	freeGlobalEnvironment(globalEnv);
} */

/* **** The End **** */
