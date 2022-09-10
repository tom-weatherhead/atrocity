/* atrocity/src/input-output.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

/* #include "char-source.h" */

#include "create-and-destroy.h"
#include "environment.h"
#include "memory-manager.h"
#include "parse-and-evaluate.h"
#include "print.h"
#include "utilities.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants */

static char commentChar = ';';
static int readScriptBufSize = 4096;
static int replBufSize = 1024;

/* Functions */

void execScriptInFile(char * filename, LISP_ENV * globalEnv) {
	FILE * fp = fopen(filename, "r");

	if (fp == NULL) {
		fprintf(stderr, "execScriptInFile: Failed to open the file '%s'\n", filename);
		return;
	}

	printf("\nExecuting script...\n\n");

	LISP_ENV * originalGlobalEnvParam = globalEnv;

	const int bufSize = readScriptBufSize;
	const int bufSizeInBytes = bufSize * sizeof(char);
	char * str = (char *)mmAlloc(bufSizeInBytes);

	if (str == NULL) {
		fatalError("mmAlloc() failed in execScriptInFile()");
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

			LISP_VALUE * value = parseStringAndEvaluate(str, globalEnv);

			printValue(value);
			/* collectGarbage(); */
			printf("\n");

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
				/* TODO: Use a StringBuilder */
				fprintf(stderr, "execScriptInFile: Text buffer overflow\n");
				break;
			}
		}
	}

	fclose(fp);

	if (originalGlobalEnvParam == NULL) {
		freeGlobalEnvironment(globalEnv);
	}

	mmFree(str);

	printf("\nScript execution complete.\n");
}

/* TODO: Unify the functions execScriptInFile(), readEvalPrintLoop(), and
the parseAndEvaluate() that is used in runTests() */
/* Pass this new function a getLine callback function so it can get lines
of code (or NULL for EOF) as it needs them. */
/* Output to string (or a list of strings) (or a list of Scheme values?) */

void readEvalPrintLoop() {
	const int bufsize = replBufSize;
	const int bufsizeInBytes = bufsize * sizeof(char);
	char * buf = (char *)mmAlloc(bufsizeInBytes);
	int i;
	LISP_ENV * globalEnv = createGlobalEnvironment();
	SCHEME_UNIVERSAL_TYPE * exprTreesToMark[] = { globalEnv, globalTrueValue, globalNullValue, NULL };

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

		if (strlen(buf) == 0) {
			printf("buf is empty.\n\n");
		} /* else if (!strcmp(buf, "help") || !strcmp(buf, "?")) {
			printf("This is the help information (TODO).\n\n");
		} */ else if (!strcmp(buf, "exit") || !strcmp(buf, "quit") || !strcmp(buf, "bye")) {
			printf("\nExiting...\n");
			break;
		} else if (!strcmp(buf, "load")) {
			execScriptInFile("../scripts/labyrinth.scm", globalEnv);
		} else if (!strncmp(buf, "load ", 5)) {
			/* TODO: Load script from file into REPL environment.
			E.g. load ../scripts/labyrinth.scm */

			execScriptInFile(buf + 5, globalEnv);
		} else {
			LISP_VALUE * value = parseStringAndEvaluate(buf, globalEnv);

			printValue(value);
			printf("\n\n");

			/* const int numFreed = collectGarbage(exprTreesToMark);

			printf("gc: %d block(s) of memory freed.\n", numFreed); */
		}
	}

	freeGlobalEnvironment(globalEnv);
	mmFree(buf);

	printf("REPL complete.\n");
}

/* **** The End **** */
