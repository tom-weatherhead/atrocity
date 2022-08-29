/* atrocity/src/main.c */

/* ThAW: Started on 2022-08-15 */

/* To compile and link: $ make */
/* To run: $ ./atrocity -t */
/* To remove all build products: $ make clean */
/* To do all of the above: $ make clean && make && ./atrocity -t */
/* To run a script: E.g. $ ./atrocity ../scripts/test001.scm */

/* TODO: create domain-object-model.c */

/* TODO: Add this stuff:
string? (for Labyrinth) (Done?)
listtostring (for Labyrinth)
Real (i.e. floating-point) numbers?
floor
QuoteKeyword (e.g. for (quote 1 2 3))
rplaca
rplacd
Dot (i.e. '.'; e.g. (cons 1 2) -> (1 . 2) : A pair, but not a list.)
call/cc (for Labyrinth)
throw (for Labyrinth) (Done?)
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
#include "parser.h"
#include "evaluate.h"

/* Function prototypes */

/* Constants */

/* Global variables */

LISP_VALUE * globalNullValue = NULL;
LISP_VALUE * globalTrueValue = NULL;

static char commentChar = ';';

/* Functions */

void fatalError(char * str) {
	fprintf(stderr, "\nFatal error: '%s'\nAborting the program...\n", str);
	exit(1);
}

void parseAndEvaluate(char * str) {
	printf("\nInput: '%s'\n", str);

	globalNullValue = createNull();
	globalTrueValue = createSymbolValue("T"); /* Use 'T ; i.e. createSymbolValue("T") */

	CharSource * cs = createCharSource(str);

	LISP_ENV * globalEnv = createEnvironment(NULL);

	/* BEGIN: Predefined variables in the global environment */
	LISP_VAR * varNull = createVariable("null");

	addToEnvironment(globalEnv, varNull, globalNullValue);
	/* END: Predefined variables in the global environment */

	LISP_EXPR * parseTree = parseExpression(cs);

	LISP_VALUE * value = evaluate(parseTree, globalEnv);

	printf("Output: ");
	printValue(value);
	printf("\n");

	/* Note bene: freeClosure is currently mostly disabled to avoid
	 * double-freeing things. We must fix this. */
	freeValue(value);
	freeExpression(parseTree);
	freeVariable(varNull);
	freeEnvironment(globalEnv);
	freeCharSource(cs);

	freeValue(globalTrueValue);
	freeValue(globalNullValue);
	globalNullValue = NULL;
}

void parseAndEvaluateStringList(char * strs[]) {
	int i = 0;

	globalNullValue = createNull();
	globalTrueValue = createSymbolValue("T"); /* Use 'T ; i.e. createSymbolValue("T") */

	LISP_ENV * globalEnv = createEnvironment(NULL);

	/* BEGIN: Predefined variables in the global environment */
	LISP_VAR * varNull = createVariable("null");

	addToEnvironment(globalEnv, varNull, globalNullValue);
	/* END: Predefined variables in the global environment */

	for (;;) {
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
		++i;
	}

	freeVariable(varNull);
	freeEnvironment(globalEnv);

	freeValue(globalTrueValue);
	freeValue(globalNullValue);
	globalNullValue = NULL;
}

void testGetIdentifier(char * str) {
	char dstBuf[8];
	int i = 0;
	CharSource * cs = createCharSource(str);

	printf("\nTest getIdentifier('%s') :\n\n", str);

	while (getIdentifier(cs, dstBuf, sizeof(dstBuf)) > 0) {
		printf("getIdentifier %d: dstBuf is '%s'\n", i++, dstBuf);
	}

	freeCharSource(cs);
}

void runTests() {
	printf("\nRunning tests...\n");

	parseAndEvaluate("7");
	parseAndEvaluate("+");
	parseAndEvaluate("(+ 2 3)");
	parseAndEvaluate("(+ (+ 2 3) 8)");
	parseAndEvaluate("(if 1 2 3)"); /* Value is 2 */
	parseAndEvaluate("(if null 2 3)"); /* Value is 3 */
	parseAndEvaluate("(+ 13 21)");
	parseAndEvaluate("(- 21 13)");
	parseAndEvaluate("(* 7 13)");
	parseAndEvaluate("(/ 100 7)");
	parseAndEvaluate("(% 100 7)");
	parseAndEvaluate("(null? null)");
	parseAndEvaluate("(null? '())");
	parseAndEvaluate("(null? 13)");
	parseAndEvaluate("(lambda (x) x)"); /* The identity function */
	parseAndEvaluate("(lambda (x) 1)"); /* A constant function */
	parseAndEvaluate("((lambda (x) x) 13)"); /* Call the identity function */
	parseAndEvaluate("((lambda (x y) (+ x y)) 5 8)");

	parseAndEvaluate("(((lambda (x) (lambda (y) y)) 5) 8)"); /* Succeeds */

	parseAndEvaluate("(((lambda (x) (lambda (y) x)) 5) 8)");

	parseAndEvaluate("(((lambda (x) (lambda (y) (+ x y))) 5) 8)");

	parseAndEvaluate("(set! n 7)");

	/* cons */
	parseAndEvaluate("(cons 1 null)");
	parseAndEvaluate("(cons 1 (cons 2 null))");
	parseAndEvaluate("(cons 1 (cons 2 (cons 3 null)))");
	parseAndEvaluate("(cons 1 '(2 3))");

	/* car */
	parseAndEvaluate("(car (cons 1 null))");
	parseAndEvaluate("(car '(1))");
	parseAndEvaluate("(car (cons 1 (cons 2 null)))");
	parseAndEvaluate("(car '(1 2))");

	/* cdr */
	parseAndEvaluate("(cdr (cons 1 null))");
	parseAndEvaluate("(cdr '(1))");
	parseAndEvaluate("(cdr (cons 1 (cons 2 null)))");
	parseAndEvaluate("(cdr '(1 2))");

	/* let */
	parseAndEvaluate("(let ((a 7)) a)");
	parseAndEvaluate("(let ((a 5) (b 8)) (+ a b))");

	/* let* */
	parseAndEvaluate("(let* ((a 7)) a)");
	parseAndEvaluate("(let* ((a 7) (b (+ a 1))) b)");

	/* letrec */
	/* From thaw-grammar: 'LL(1) Scheme letrec test' */
	/* The result should be 4 */
	char * strs5[] = {
		"(letrec ((countones (lambda (l) (if (null? l) 0 (if (= (car l) 1) (+ 1 (countones (cdr l))) (countones (cdr l))))))) (countones '(1 2 3 1 0 1 1 5)))",
		NULL
	};

	parseAndEvaluateStringList(strs5);

	/* begin */
	parseAndEvaluate("(begin 1 2 4 3)");
	parseAndEvaluate("(begin (set! n 2) (+ n 3))");

	/* This {} syntax only works as an initializer; i.e. when strs is declared */
	char * strs[] = {"(begin (set! n 2) (+ n 3))", NULL};

	parseAndEvaluateStringList(strs);

	char * strs2[] = {
		"(set! add (lambda (a b) (+ a b)))",
		"(add 13 21)",
		NULL
	};

	parseAndEvaluateStringList(strs2);

	char * strs3[] = {
		"(set! n 1)",
		"(while (<= n 5) (begin (print n) (set! n (+ n 1))))",
		NULL
	};

	parseAndEvaluateStringList(strs3);

	/* cond test */
	char * strs4[] = {
		/* "(set! n 1)", */
		"(set! n 2)",
		/* "(set! n 3)", */
		/* "(set! n 4)", */
		"(cond ((= n 1) 111) ((= n 2) 222) ((= n 3) 333) (1 777))",
		NULL
	};

	parseAndEvaluateStringList(strs4);

	/* curry test */
	char * strs6[] = {
		"(set! curry2 (lambda (f) (lambda (x) (lambda (y) (f x y)))))",
		"(((curry2 +) 2) 3)",
		NULL
	};

	parseAndEvaluateStringList(strs6);

	/* char * strs[] = {, NULL}; */

	/* TODO: letrec print call/cc */

	/* parseAndEvaluate(""); */

	/* parseAndEvaluateStringList(["", "", "", ..., NULL]); */

	/* testGetIdentifier("abc (def weatherhead) ghi");
	testGetIdentifier("(+1 7)");
	testGetIdentifier("(((a b) c) d)");
	/ * testGetIdentifier(""); */

	printf("\nDone.\n");
}

static BOOL isStringAllWhitespace(char * str) {
	const int len = strlen(str);
	int i;

	for (i = 0; i < len; ++i) {

		if (str[i] != ' ' && str[i] != '\t' && str[i] != '\n') {
			return FALSE;
		}
	}

	return TRUE;
}

void execScriptInFile(char * filename, LISP_ENV * globalEnv) {
	FILE * fp = fopen(filename, "r");

	if (fp == NULL) {
		fprintf(stderr, "execScriptInFile: Failed to open the file '%s'\n", filename);
		return;
	}

	printf("\nExecuting script...\n\n");

	LISP_ENV * originalGlobalEnvParam = globalEnv;
	LISP_VAR * varNull = NULL;

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
		globalNullValue = createNull();
		globalTrueValue = createSymbolValue("T");

		globalEnv = createEnvironment(NULL);

		/* BEGIN: Predefined variables in the global environment */
		varNull = createVariable("null");

		addToEnvironment(globalEnv, varNull, globalNullValue);
	}

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
		freeVariable(varNull);
		freeEnvironment(globalEnv);

		freeValue(globalTrueValue);
		freeValue(globalNullValue);
		globalNullValue = NULL;
	}

	free(str);

	printf("\nScript execution complete.\n");
}

static char * fgets_wrapper(char * buffer, size_t buflen, FILE * fp) {
	/* From https://stackoverflow.com/questions/1694036/why-is-the-gets-function-so-dangerous-that-it-should-not-be-used */

	if (fgets(buffer, buflen, fp) != 0) {
		size_t len = strlen(buffer);

		if (len > 0 && buffer[len - 1] == '\n') {
			buffer[len - 1] = '\0';
		}

		return buffer;
	}

	return 0;
}

void readEvalPrintLoop() {
	const int bufsize = 1024;
	const int bufsizeInBytes = bufsize * sizeof(char);
	char * buf = (char *)malloc(bufsizeInBytes);
	int i;

	globalNullValue = createNull();
	globalTrueValue = createSymbolValue("T"); /* Use 'T ; i.e. createSymbolValue("T") */

	LISP_ENV * globalEnv = createEnvironment(NULL);

	/* BEGIN: Predefined variables in the global environment */
	LISP_VAR * varNull = createVariable("null");

	addToEnvironment(globalEnv, varNull, globalNullValue);

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

		printf("Parsing...\n");

		LISP_EXPR * parseTree = parseExpression(cs);

		printf("Evaluating...\n");

		LISP_VALUE * value = evaluate(parseTree, globalEnv);

		/* printf("Output: "); */
		printValue(value);
		printf("\n\n");

		freeCharSource(cs);
	}

	freeVariable(varNull);
	freeEnvironment(globalEnv);

	freeValue(globalTrueValue);
	freeValue(globalNullValue);
	globalNullValue = NULL;

	free(buf);

	printf("REPL complete.\n");
}

/* **** The Main MoFo **** */

int main(int argc, char * argv[]) {
	BOOL enableTests = FALSE;
	BOOL enableVersion = FALSE;
	char * filename = NULL;
	int i;

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
