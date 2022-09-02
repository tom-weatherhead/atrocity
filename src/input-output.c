/* atrocity/src/input-output.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "parser.h"
#include "evaluate.h"
#include "tests.h"
#include "utilities.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants */

static char commentChar = ';';

/* Functions */

void execScriptInFile(char * filename, LISP_ENV * globalEnv) {
	FILE * fp = fopen(filename, "r");

	if (fp == NULL) {
		fprintf(stderr, "execScriptInFile: Failed to open the file '%s'\n", filename);
		return;
	}

	printf("\nExecuting script...\n\n");

	LISP_ENV * originalGlobalEnvParam = globalEnv;

	const int bufSize = 4096;
	const int bufSizeInBytes = bufSize * sizeof(char);
	char * str = (char *)malloc(bufSizeInBytes);

	if (str == NULL) {
		fatalError("malloc() failed in execScriptInFile()");
	}

	int i = 0;
	int bracketDepth = 0;

	memset(str, 0, bufSizeInBytes);

	if (globalEnv == NULL) {
		globalEnv = createGlobalEnvironment();
	}

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	for (;;) {
		int cn = fgetc(fp);

		if (cn == EOF) {
			break;
		}

		char c = (char)cn;

		/* printf("Char: '%c' (int %d)\n", c, cn);
		printf("str: '%s'\n", str); */

		if (c == commentChar) {

			do {
				cn = fgetc(fp);

				if (cn == EOF) {
					break;
				}

				c = (char)cn;
			} while (c != '\n');

			if (cn == EOF) {
				break;
			}
		}

		if (c == '\n' && bracketDepth != 0) {
			c = ' ';
		}

		if (c == '\n') {

			if (strlen(str) == 0) {
				continue;
			} else if (isStringAllWhitespace(str)) {
				memset(str, 0, bufSizeInBytes);
				i = 0;
				continue;
			}

			/* printf("Parsing '%s' (length %lu)...\n", str, strlen(str)); */

			CharSource * cs = createCharSource(str);

			LISP_EXPR * parseTree = parseExpression(cs);

			/* printf("Evaluating...\n"); */

			LISP_VALUE * value = evaluate(parseTree, globalEnv);

			/* printf("Output: "); */
			printValue(value);
			printf("\n");

			freeCharSource(cs);

			memset(str, 0, bufSizeInBytes);
			i = 0;
		} else {

			if (c == '(') {
				++bracketDepth;
			} else if (c == ')') {
				--bracketDepth;

				if (bracketDepth < 0) {
					fprintf(stderr, "execScriptInFile: More ) than (\n");
					break;
				}
			}

			str[i++] = c;

			if (i >= bufSize) {
				fprintf(stderr, "execScriptInFile: Text buffer overflow\n");
				break;
			}
		}
	}

	fclose(fp);

	if (originalGlobalEnvParam == NULL) {
		freeGlobalEnvironment(globalEnv);
	}

	free(str);

	printf("\nScript execution complete.\n");
}

/* TODO: Unify the functions execScriptInFile(), readEvalPrintLoop(), and
the parseAndEvaluate() that is used in runTests() */
/* Pass this new function a getLine callback function so it can get lines
of code (or NULL for EOF) as it needs them. */
/* Output to string (or a list of strings) (or a list of Scheme values?) */

void readEvalPrintLoop() {
	const int bufsize = 1024;
	const int bufsizeInBytes = bufsize * sizeof(char);
	char * buf = (char *)malloc(bufsizeInBytes);
	int i;
	LISP_ENV * globalEnv = createGlobalEnvironment();

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	printf("\nStarting the read-eval-print loop...\n\n");

	for (i = 0; ; ++i) {
		memset(buf, 0, bufsizeInBytes);
		printf("%d > ", i);

		/* scanf("%s", buf); */ /* No. */
		/* gets(buf); */ /* This is unsafe as fsck. Buffer overflow city. */
		fgets_wrapper(buf, bufsize, stdin);

		int len = strlen(buf);

		if (len > 0 && buf[len - 1] == '\n') {
			printf("Trimming newline...\n");
			buf[len - 1] = '\0';
		}

		/* printf("strlen(buf) is: %lu\n", strlen(buf));
		printf("buf is: '%s'\n", buf); */

		if (strlen(buf) == 0) {
			printf("buf is empty.\n\n");
			continue;
		} /* else if (!strcmp(buf, "help") || !strcmp(buf, "?")) {
			printf("This is the help information (TODO).\n\n");
			continue;
		} */ else if (!strcmp(buf, "exit") || !strcmp(buf, "quit") || !strcmp(buf, "bye")) {
			printf("\nExiting...\n");
			break;
		/* } else if (!strncmp(buf, "load ", 5)) { */
		} else if (!strcmp(buf, "load")) {
			/* TODO: Load script from file into REPL environment.
			E.g. load ../scripts/labyrinth.scm
			fatalError("fsck"); */

			/* execScriptInFile(buf + 5, globalEnv); */
			execScriptInFile("../scripts/labyrinth.scm", globalEnv);
			continue;
		} else if (!strcmp(buf, "fsck")) {
			fatalError("fsck");
		}

		/* printf("Evaluating '%s' (length %lu)...\n", str, strlen(str)); */

		CharSource * cs = createCharSource(buf);

		/* printf("Parsing...\n"); */

		LISP_EXPR * parseTree = parseExpression(cs);

		/* printf("Evaluating...\n"); */

		LISP_VALUE * value = evaluate(parseTree, globalEnv);

		/* printf("Output: "); */
		printValue(value);
		printf("\n\n");

		freeCharSource(cs);
	}

	freeGlobalEnvironment(globalEnv);
	free(buf);

	printf("REPL complete.\n");
}

/* **** The End **** */
