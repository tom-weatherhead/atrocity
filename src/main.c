/* atrocity/src/main.c */

/* ThAW: Started on 2022-08-15 */

/* cd into the src directory. Then: */
/* To compile and link: $ make */
/* To run: $ ./atrocity -t */
/* To remove all build products: $ make clean */
/* To do all of the above: $ make clean && make && ./atrocity -t */
/* To run a script: E.g. $ ./atrocity ../scripts/test001.scm */
/* To enter the read-eval-print loop: $ ./atrocity */

/* TODO? : Create:
- domain-object-model.c
- parse-and-evaluate.c

Then main.c will contain only the function main()
*/

/* TODO: Add this stuff:
rplaca -> Done?
Dot (i.e. '.'; e.g. (cons 1 2) -> (1 . 2) : A pair, but not a list.)
QuoteKeyword (e.g. for (quote 1 2 3))
Real (i.e. floating-point) numbers?
floor
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */
#include <time.h>

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "input-output.h"
#include "parser.h"
#include "evaluate.h"
#include "tests.h"

/* Function prototypes */

/* Forward references */

/* Constants */

/* Global variables */

LISP_VALUE * globalNullValue = NULL;
LISP_VALUE * globalTrueValue = NULL;

/* Functions */

/* **** BEGIN parseAndEvaluate **** */

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

void parseAndEvaluate(char * str) {
	/* LISP_VALUE * value = */ parseAndEvaluateEx(str, NULL, TRUE);
}

void parseAndEvaluateStringList(char * strs[]) {
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
}

/* **** END parseAndEvaluate **** */

/* **** The Main MoFo **** */

int main(int argc, char * argv[]) {
	BOOL enableTests = FALSE;
	BOOL enableVersion = FALSE;
	char * filename = NULL;
	int i;

	/* Use the current time to seed the random number generator: */
	srand(time(NULL));

	for (i = 1; i < argc; ++i) {
		/* printf("argv[%d] = %s\n", i, argv[i]); */

		if (!strcmp(argv[i], "-t")) {
			enableTests = TRUE;
		} else if (!strcmp(argv[i], "-v")) {
			enableVersion = TRUE;
		} else if (filename == NULL && argv[i][0] != '-') {
			filename = argv[i];
		}
	}

	if (enableVersion) {
		printf("\nAtrocity version 0.0.0\n");
	} else if (enableTests) {
		runTests();
	} else if (filename != NULL) {
		execScriptInFile(filename, NULL);
	} else {
		readEvalPrintLoop();
	}

	return 0; /* Zero (as a Unix exit code) means success. */
}

/* **** The End **** */
