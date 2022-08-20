/* atrocity/src/main.c */

/* ThAW: Started on 2022-08-15 */

/* To compile and link: $ make */
/* To run: $ ./atrocity */
/* To remove all build products: $ make clean */

/* TODO: create domain-object-model.c */
/* TODO: create memory-manager.c ; Use a reference-counting scheme? */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"
#include "parser.h"
#include "evaluate.h"

/* Function prototypes */

/* Constants */

/* Global variables */

LISP_VALUE * globalNullValue = NULL;
LISP_VALUE * globalTrueValue = NULL;

/* Functions */

/* BOOL isValueCallable(LISP_VALUE * value) {
	return value->type == lispValueType_PrimitiveOperator || value->type == lispValueType_Closure;
} */

void printValue(LISP_VALUE * value) {

	switch (value->type) {
		case lispValueType_Number:
			printf("Number: %d", value->value);
			break;

		case lispValueType_String:
			printf("String: '%s'", value->name);
			break;

		case lispValueType_PrimitiveOperator:
			printf("PrimitiveOperator: '%s'", value->name);
			break;

		case lispValueType_Closure:
			printf("<closure>");
			break;

		case lispValueType_Pair:
			printf("Pair: (");
			printValue(value->pair->head);
			printf(" ");
			printValue(value->pair->tail);
			printf(")");
			break;

		case lispValueType_Null:
			printf("Null");
			break;

		default:
			printf("<invalidValue>");
			break;
	}
}

/* BEGIN: Environment stuff */

LISP_VALUE * lookupVariableInNameValueList(LISP_VAR * var, LISP_NAME_VALUE_LIST_ELEMENT * nvle) {

	while (nvle != NULL) {
		/* printf("  Comparing var name '%s' to '%s'...\n", var->name, nvle->name); */

		if (!strcmp(nvle->name, var->name)) {
			/* printf("  Match\n"); */
			return nvle->value;
		}

		nvle = nvle->next;
	}

	return NULL;
}

LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env) {
	LISP_VALUE * value = NULL;

	while (env != NULL) {
		/* printf("lookupVariableInEnvironment: Looking for '%s' in nameValueList\n", var->name); */
		value = lookupVariableInNameValueList(var, env->nameValueList);

		if (value != NULL) {
			break;
		}

		/* printf("lookupVariableInEnvironment: Moving to the next env frame\n");
		printf("  env->nameValueList is %ld\n", (long)env->nameValueList);
		printf("  env is %ld\n", (long)env);
		printf("  env->next is %ld\n", (long)env->next); */

		if (env == env->next) {
			fprintf(stderr, "lookupVariableInEnvironment: env == env->next; breaking...\n");
			break;
		}

		env = env->next;
	}

	/* printf("lookupVariableInEnvironment: Returning value %ld\n", (long)value); */

	return value;
}

BOOL updateIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, LISP_VAR * var, LISP_VALUE * value) {

	while (nvle != NULL) {

		if (!strcmp(nvle->name, var->name)) {
			nvle->value = value;
			return TRUE;
		}

		nvle = nvle->next;
	}

	return FALSE;
}

BOOL updateIfFoundInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {


	while (env != NULL) {

		if (updateIfFoundInNameValueList(env->nameValueList, var, value)) {
			return TRUE;
		}

		env = env->next;
	}

	return FALSE;
}

void addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {

	if (lookupVariableInEnvironment(var, env) != NULL) {
		fprintf(stderr, "addToEnvironment() : The variable '%s' already exists in this environment. Update not yet implemented.", var->name);
		return;
	}

	env->nameValueList = createNameValueListElement(var->name, value, env->nameValueList);
}

void setValueInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {

	if (!updateIfFoundInEnvironment(env, var, value)) {
		addToEnvironment(env, var, value);
	}
}

void printEnvironment(LISP_ENV * env) {
	int i = 0;

	printf("printEnvironment:\n");

	while (env != NULL) {
		LISP_NAME_VALUE_LIST_ELEMENT * nvle = env->nameValueList;
		int j = 0;

		printf("  Frame %d:\n", i++);

		while (nvle != NULL) {
			printf("    Value %d: %s = ", j++, nvle->name);
			printValue(nvle->value);
			printf("\n");
			nvle = nvle->next;
		}

		env = env->next;
	}

	printf("End of printEnvironment\n");
}

/* END: Environment stuff */

// **** Etc. ****

void parseAndEvaluate(char * str) {
	printf("\nInput: '%s'\n", str);

	globalNullValue = createNull();
	globalTrueValue = createStringValue("T"); /* Use 'T ; i.e. createSymbolValue("T") */

	CharSource * cs = createCharSource(str);

	LISP_ENV * globalEnv = createEnvironment(NULL);

	LISP_VAR * varNull = createVariable("null");

	addToEnvironment(globalEnv, varNull, globalNullValue);

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
	globalTrueValue = createStringValue("T"); /* Use 'T ; i.e. createSymbolValue("T") */

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

	/* car */
	parseAndEvaluate("(car (cons 1 null))");
	parseAndEvaluate("(car (cons 1 (cons 2 null)))");

	/* cdr */
	parseAndEvaluate("(cdr (cons 1 null))");
	parseAndEvaluate("(cdr (cons 1 (cons 2 null)))");

	/* let */
	parseAndEvaluate("(let ((a 7)) a)");
	parseAndEvaluate("(let ((a 5) (b 8)) (+ a b))");

	/* let* */
	parseAndEvaluate("(let* ((a 7)) a)");
	parseAndEvaluate("(let* ((a 7) (b (+ a 1))) b)");

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

	/* char * strs[] = {, NULL}; */

	/* TODO: letrec print call/cc */

	/* parseAndEvaluate(""); */

	/* parseAndEvaluateStringList(["", "", "", ..., NULL]); */

	/* testGetIdentifier("abc (def weatherhead) ghi");
	testGetIdentifier("(+1 7)");
	testGetIdentifier("(((a b) c) d)");
	/ * testGetIdentifier(""); */

	printf("\nDone.\n\n");
}

/* **** The Main MoFo **** */

int main(int argc, char * argv[]) {
	runTests();

	return 0; /* Zero (as a Unix exit code) means success. */
}

/* **** The End **** */
