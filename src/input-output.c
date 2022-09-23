/* atrocity/src/input-output.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "memory-manager.h"
#include "parse-and-evaluate.h"
#include "print.h"
#include "string-builder.h"
#include "utilities.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants */

static char commentChar = ';';
/* static int readScriptBufSize = 4096;
static int replBufSize = 1024; */

/* Functions */

STRING_BUILDER_TYPE * appendLineFromFileToStringBuilder(STRING_BUILDER_TYPE * sb, FILE * file) {

	if (sb != NULL) {
		failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "appendLineFromFileToStringBuilder() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (1)");
	}

	if (sb == NULL) {
		sb = createStringBuilder(0);
	}

	failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "appendLineFromFileToStringBuilder() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (2)");

	for (;;) {
		const int cn = fgetc(file);

		if (cn == EOF) {
			break;
		}

		const char c = (char)cn;

		if (c == '\n') {
			break;
		}

		/* printf("appendLineFromFileToStringBuilder() : appendCharToStringBuilder...\n"); */

		failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "appendLineFromFileToStringBuilder() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (3)");

		appendCharToStringBuilder(sb, c);
	}

	return sb;
}

static int charStateMachine(char * str, int len, int * pBracketDepth, BOOL * pIsACompleteExpression) {
	failIf(pBracketDepth == NULL, "charStateMachine() : pBracketDepth == NULL");
	failIf(pIsACompleteExpression == NULL, "charStateMachine() : pIsACompleteExpression == NULL");

	int bracketDepth = *pBracketDepth;
	int isDoubleQuoted = 0;
	int i;

	*pIsACompleteExpression = FALSE;

	if (len < 0) {
		len = strlen(str);
	}

	for (i = 0; i < len; ++i) {

		if (str[i] == commentChar) {

			if (!isDoubleQuoted) {
				/* This commentChar is not inside a string */
				/* If bracketDepth > 0 then the comment is inside an expr */
				break;
			} else {
				continue;
			}
		}

		switch (str[i]) {
			case '"':
				isDoubleQuoted = 1 - isDoubleQuoted; /* Toggle this state */
				break;

			case '(':
				++bracketDepth;
				break;

			case ')':
				--bracketDepth;
				failIf(bracketDepth < 0, "charStateMachine: More ) than (");
				break;

			default:
				break;
		}
	}

	/* Return the length of the part of the string before any comment starts */

	/* TODO: Trim any trailing whitespace. Is the string all whitespace? */

	/* TODO: Have we reached the EOF? */

	if (!isDoubleQuoted && bracketDepth == 0) {
		*pIsACompleteExpression = TRUE;
	}

	*pBracketDepth = bracketDepth;

	return i;
}

/* Version 1 */
/* void execScriptInFileV1(char * filename, LISP_ENV * globalEnv) {
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

	memset(str, 0, bufSizeInBytes);

	int i = 0;
	int bracketDepth = 0;

	if (globalEnv == NULL) {
		globalEnv = createGlobalEnvironment();
	}

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	STRING_BUILDER_TYPE * sb = NULL;

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
			printf("\n");

			SCHEME_UNIVERSAL_TYPE * exprTreesToMark[] = { globalEnv, globalTrueValue, globalNullValue, sb, NULL };

			const int numFreed = collectGarbage(exprTreesToMark);

			printf("gc: %d block(s) of memory freed.\n", numFreed);

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
		freeGlobalEnvironment(/ * globalEnv * /);
	}

	mmFree(str);

	printf("\nScript execution complete.\n");
} */

void execScriptInFile(char * filename, LISP_ENV * globalEnv) {
	FILE * file = fopen(filename, "r");

	if (file == NULL) {
		fprintf(stderr, "execScriptInFile: Failed to open the file '%s'\n", filename);
		return;
	}

	printf("\nExecuting script...\n\n");

	LISP_ENV * originalGlobalEnvParam = globalEnv;

	/* int i = 0; */
	int bracketDepth = 0;

	if (globalEnv == NULL) {
		globalEnv = createGlobalEnvironment();
	}

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	STRING_BUILDER_TYPE * sb = NULL;
	STRING_BUILDER_TYPE * sbAccumulator = createStringBuilder(0);

	for (;;) {
		/* TODO: Use appendLineFromFileToStringBuilder(sb, file);

		I.e. : */

		if (sb != NULL) {
			failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "execScriptInFile() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (1)");
		}

		clearStringBuilder(sb);

		if (sb != NULL) {
			failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "execScriptInFile() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (2)");
		}

		sb = appendLineFromFileToStringBuilder(sb, file);

		char * buf = sb->name;
		BOOL isACompleteExpression = FALSE;

		/* printf("sb contains '%s'\n", sb->name); */

		const int len = charStateMachine(buf, -1, &bracketDepth, &isACompleteExpression);

		/* printf("charStateMachine() returned len = %d\n", len); */

		const int isEof = feof(file);

		if (len == 0) {

			if (isEof) {
				/* We have finished reading and interpreting the file. */
				break;
			} else {
				/* The current line contains nothing to interpret. */
				continue;
			}
		}

		/* Are we appending the current line onto (a) previous line(s)
		in order to complete an expression? If so, append a space to
		the previous text before appending the current line. */

		if (!isStringBuilderEmpty(sbAccumulator)) {
			sbAccumulator = appendCharToStringBuilder(sbAccumulator, ' ');
		}

		/* printf("appendCharsToStringBuilder()...\n"); */

		sbAccumulator = appendCharsToStringBuilder(sbAccumulator, sb->name, len);

		/* printf("Done appendCharsToStringBuilder()\n"); */

		/* int cn = fgetc(fp);

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
			} */

		/* if (bracketDepth > 0) { */
		if (!isACompleteExpression) {
			/* printf("Not a complete expression.\n"); */
			continue;
		}

		char * str = sbAccumulator->name;

		/* printf("parseStringAndEvaluate() : str is '%s'\n", str); */

		LISP_VALUE * value = parseStringAndEvaluate(str, globalEnv);

		printValue(value);
		printf("\n");

		clearStringBuilder(sbAccumulator);

		SCHEME_UNIVERSAL_TYPE * exprTreesToMark[] = { globalEnv, globalTrueValue, globalNullValue, sb, sbAccumulator, NULL };

		const int numFreed = collectGarbage(exprTreesToMark);

		printf("gc: %d block(s) of memory freed.\n", numFreed);

		/*	memset(str, 0, bufSizeInBytes);
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
		} */
	}

	fclose(file);

	if (originalGlobalEnvParam == NULL) {
		freeGlobalEnvironment(/* globalEnv */);
	}

	/* mmFree(str); */

	printf("\nScript execution complete.\n");
}

/* TODO: Unify the functions execScriptInFile(), readEvalPrintLoop(), and
the parseAndEvaluate() that is used in runTests() */
/* Pass this new function a getLine callback function so it can get lines
of code (or NULL for EOF) as it needs them. */
/* Output to string (or a list of strings) (or a list of Scheme values?) */

void readEvalPrintLoop() {
	/* const int bufsize = replBufSize;
	const int bufsizeInBytes = bufsize * sizeof(char);
	char * buf = (char *)mmAlloc(bufsizeInBytes); */
	int i;
	LISP_ENV * globalEnv = createGlobalEnvironment();
	/* STRING_BUILDER_TYPE * sb = NULL; */

	printf("\nStarting the read-eval-print loop...\n\n");

	for (i = 0; ; ++i) {
		/* memset(buf, 0, bufsizeInBytes); */
		printf("%d > ", i);

		/* TODO: Use appendLineFromFileToStringBuilder(sb, stdin); */

		/* scanf("%s", buf); */ /* No. */
		/* gets(buf); */ /* This is unsafe as fsck. Buffer overflow city. */
		/* fgets_wrapper(buf, bufsize, stdin); */

		/* if (sb != NULL) {
			failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "readEvalPrintLoop() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (1)");
		}

		clearStringBuilder(sb);

		if (sb != NULL) {
			failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "readEvalPrintLoop() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (2)");
		} */

		STRING_BUILDER_TYPE * sb = appendLineFromFileToStringBuilder(NULL, stdin);

		char * buf = sb->name;
		const int len = strlen(buf);

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

			SCHEME_UNIVERSAL_TYPE * exprTreesToMark[] = { globalEnv, globalTrueValue, globalNullValue, /* sb, */ NULL };

			const int numFreed = collectGarbage(exprTreesToMark);

			printf("gc: %d block(s) of memory freed.\n", numFreed);
		}
	}

	freeGlobalEnvironment(/* globalEnv */);
	/* mmFree(buf); */

	printf("REPL complete.\n");
}

/* **** The End **** */
