/* atrocity/src/input-output.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "types.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "memory-manager.h"
#include "parse-and-evaluate.h"
#include "print.h"
#include "string-builder.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants */

static char commentChar = ';';

/* Functions */

STRING_BUILDER_TYPE * appendLineFromFileToStringBuilder(STRING_BUILDER_TYPE * sb, FILE * file) {

	for (;;) {
		const int cn = fgetc(file);

		if (cn == EOF) {
			break;
		}

		const char c = (char)cn;

		if (c == '\n') {
			break;
		}

		sb = appendCharToStringBuilder(sb, c);
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

	/* Trim any trailing whitespace. Is the string all whitespace? */

	while (i > 0 && isspace(str[i - 1])) {
		--i;
	}

	if (!isDoubleQuoted && bracketDepth == 0) {
		*pIsACompleteExpression = TRUE;
	}

	*pBracketDepth = bracketDepth;

	return i;
}

static STRING_BUILDER_TYPE * func1(FILE * file, STRING_BUILDER_TYPE * sbAccumulator, STRING_BUILDER_TYPE ** psb, int * pBracketDepth, BOOL * pBreak, BOOL * pContinue) {
	*pBreak = FALSE;
	*pContinue = FALSE;

	clearStringBuilder(*psb);
	*psb = appendLineFromFileToStringBuilder(*psb, file);

	BOOL isACompleteExpression = FALSE;

	const int len = charStateMachine((*psb)->name, -1, pBracketDepth, &isACompleteExpression);

	if (len == 0) {

		if (feof(file)) {
			/* We have finished reading and interpreting the file. */
			*pBreak = TRUE;
		} else {
			/* The current line contains nothing to interpret. */
			*pContinue = TRUE;
		}

		return sbAccumulator;
	}

	/* Are we appending the current line onto (a) previous line(s)
	in order to complete an expression? If so, append a space to
	the previous text before appending the current line. */

	if (!isStringBuilderEmpty(sbAccumulator)) {
		sbAccumulator = appendCharToStringBuilder(sbAccumulator, ' ');
	}

	*pContinue = !isACompleteExpression;
	sbAccumulator = appendCharsToStringBuilder(sbAccumulator, (*psb)->name, len);

	return sbAccumulator;
}

void execScriptInFile(char * filename, LISP_ENV * globalEnv) {
	FILE * file = fopen(filename, "r");

	if (file == NULL) {
		fprintf(stderr, "execScriptInFile: Failed to open the file '%s'\n", filename);
		return;
	}

	printf("\nExecuting script...\n\n");

	LISP_ENV * originalGlobalEnvParam = globalEnv;

	int bracketDepth = 0;

	if (globalEnv == NULL) {
		globalEnv = createGlobalEnvironment();
	}

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	STRING_BUILDER_TYPE * sb = NULL;
	STRING_BUILDER_TYPE * sbAccumulator = NULL; /* createStringBuilder(0); */

	for (;;) {
		BOOL shouldBreak = FALSE;
		BOOL shouldContinue = FALSE;

		sbAccumulator = func1(file, sbAccumulator, &sb, &bracketDepth, &shouldBreak, &shouldContinue);

		if (shouldBreak) {
			break;
		} else if (shouldContinue) {
			continue;
		}

		LISP_VALUE * value = parseStringAndEvaluate(sbAccumulator->name, globalEnv);

		printValue(value);
		printf("\n");

		clearStringBuilder(sbAccumulator);

		SCHEME_UNIVERSAL_TYPE * exprTreesToMark[] = { globalEnv, globalTrueValue, globalNullValue, sb, sbAccumulator, NULL };

		const int numFreed = collectGarbage(exprTreesToMark);

		printf("gc: %d block(s) of memory freed.\n", numFreed);
	}

	fclose(file);

	if (originalGlobalEnvParam == NULL) {
		freeGlobalEnvironment(/* globalEnv */);
	}

	printf("\nScript execution complete.\n");
}

void readEvalPrintLoop() {
	int i = 0;
	int bracketDepth = 0;
	LISP_ENV * globalEnv = createGlobalEnvironment();
	STRING_BUILDER_TYPE * sb = NULL;
	STRING_BUILDER_TYPE * sbAccumulator = NULL;

	printf("\nStarting the read-eval-print loop...\n\n");

	for (;;) {

		if (isStringBuilderEmpty(sbAccumulator)) {
			printf("%d > ", i);
		} else {
			printf("(%d continued) > ", i);
		}

		/* scanf("%s", buf); */ /* No. */
		/* gets(buf); */ /* This is unsafe as fsck. Buffer overflow city. */
		/* fgets_wrapper(buf, bufsize, stdin); */

		BOOL shouldBreak = FALSE;
		BOOL shouldContinue = FALSE;

		sbAccumulator = func1(stdin, sbAccumulator, &sb, &bracketDepth, &shouldBreak, &shouldContinue);

		if (shouldBreak) {
			break;
		} else if (shouldContinue) {
			continue;
		}

		char * buf = sbAccumulator->name;
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
			/* This will call collectGarbage(), which will free our sb and sbAccumulator */
			execScriptInFile("../scripts/labyrinth.scm", globalEnv);
			sb = NULL;
			sbAccumulator = NULL;
		} else if (!strncmp(buf, "load ", 5)) {
			/* TODO: Load script from file into REPL environment.
			E.g. load ../scripts/labyrinth.scm */

			/* This will call collectGarbage(), which will free our sb and sbAccumulator */
			execScriptInFile(buf + 5, globalEnv);
			sb = NULL;
			sbAccumulator = NULL;
		} else {
			LISP_VALUE * value = parseStringAndEvaluate(buf, globalEnv);

			printValue(value);
			printf("\n\n");
			++i;

			clearStringBuilder(sbAccumulator);

			SCHEME_UNIVERSAL_TYPE * exprTreesToMark[] = { globalEnv, globalTrueValue, globalNullValue, sb, sbAccumulator, NULL };

			const int numFreed = collectGarbage(exprTreesToMark);

			printf("gc: %d block(s) of memory freed.\n", numFreed);
		}
	}

	freeGlobalEnvironment(/* globalEnv */);

	printf("REPL complete.\n");
}

/* **** The End **** */
