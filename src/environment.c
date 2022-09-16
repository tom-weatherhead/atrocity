/* atrocity/src/environment.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "parse-and-evaluate.h"

/* Global variables */

LISP_VALUE * globalNullValue = NULL;
LISP_VALUE * globalTrueValue = NULL;

/* Function prototypes */

/* Functions */

static LISP_VALUE * lookupVariableInNameValueList(LISP_VAR * var, LISP_NAME_VALUE_LIST_ELEMENT * nvle) {

	while (nvle != NULL) {

		if (!strcmp(nvle->name, var->name)) {
			return getValueInNameValuePairListElement(nvle);
		}

		nvle = nvle->next;
	}

	return NULL;
}

LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env) {
	LISP_VALUE * value = NULL;

	while (env != NULL) {
		value = lookupVariableInNameValueList(var, getNameValuePairListInEnv(env));

		if (value != NULL) {
			break;
		}

		if (env == env->next) {
			fprintf(stderr, "lookupVariableInEnvironment: env == env->next; breaking...\n");
			break;
		}

		env = env->next;
	}

	return value;
}

BOOL updateNameIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, char * name, LISP_VALUE * value) {

	while (nvle != NULL) {

		if (!strcmp(nvle->name, name)) {
			getValueInNameValuePairListElement(nvle) = value;
			return TRUE;
		}

		nvle = nvle->next;
	}

	return FALSE;
}

BOOL updateIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, LISP_VAR * var, LISP_VALUE * value) {
	return updateNameIfFoundInNameValueList(nvle, var->name, value);
}

static BOOL updateIfFoundInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {


	while (env != NULL) {

		if (updateIfFoundInNameValueList(getNameValuePairListInEnv(env), var, value)) {
			return TRUE;
		}

		env = env->next;
	}

	return FALSE;
}

void addNameToEnvironment(LISP_ENV * env, char * name, LISP_VALUE * value) {
	getNameValuePairListInEnv(env) = createNameValueListElement(name, value, getNameValuePairListInEnv(env));
}

void addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {

	if (lookupVariableInEnvironment(var, env) != NULL) {
		fprintf(stderr, "addToEnvironment() : The variable '%s' already exists in this environment. Update not yet implemented.", var->name);
		return;
	}

	addNameToEnvironment(env, var->name, value);
}

void setValueInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {

	if (!updateIfFoundInEnvironment(env, var, value)) {
		/* Instead of calling something like addBubbleDown(),
		move env to point to the last element in the list;
		that element will be the globalEnv. */

		while (env->next != NULL) {
			env = env->next;
		}

		/* Now env == globalEnv */
		/* The pair (var, value) will be added to the globalEnv */

		addToEnvironment(env, var, value);
	}
}

/* void printEnvironment(LISP_ENV * env) {
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
} */

LISP_ENV * createGlobalEnvironment() {
	failIf(globalTrueValue != NULL, "globalTrueValue is already non-NULL");
	failIf(globalNullValue != NULL, "globalNullValue is already non-NULL");

	globalNullValue = createNull();
	globalTrueValue = createSymbolValue("T"); /* I.e. 'T */

	LISP_ENV * globalEnv = createEnvironment(NULL);

	/* BEGIN: Predefined variables in the global environment */

	/* ; BEGIN Define commonly-used lambda expressions here.
	; Of particular importance are combine, compose, and curry. */

	/* Combine: In Javascript array terms,
	((combine f sum zero) lst) := lst.map(f).reduce(sum, zero); */

	/* ; Version 2 of combine, using letrec: see Kamin page 126
	(set! combine (lambda (f sum zero)
		(letrec
			((loop (lambda (l) (if (null? l) zero (sum (f (car l)) (loop (cdr l)))))))
			loop
		)
	)) */
	parseAndEvaluateEx(
		"(set! combine (lambda (f sum zero) (letrec ((loop (lambda (l) (if (null? l) zero (sum (f (car l)) (loop (cdr l))))))) loop)))",
		globalEnv,
		FALSE
	);

	parseAndEvaluateEx("(set compose (lambda (f g) (lambda (x) (g (f x)))))", globalEnv, FALSE);

	/* Curry a function that takes two parameters */
	parseAndEvaluateEx("(set! curry (lambda (f) (lambda (x) (lambda (y) (f x y)))))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! id (lambda (x) x))", globalEnv, FALSE);

	/*
	*/

	/* TODO? : We could implement Boolean logic functionally:
	parseAndEvaluateEx("(set! true (lambda (t f) t))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! false (lambda (t f) f))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! if (lambda (b x y) (b x y)))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! and (lambda (x y) (if x y x)))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! or (lambda (x y) (if x x y)))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! not (lambda (x) (if x false true)))", globalEnv, FALSE);

	parseAndEvaluateEx("", globalEnv, FALSE);
	*/

	parseAndEvaluateEx("(set! compose2args (lambda (f g) (lambda (x y) (g (f x y)))))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! reverse2args (lambda (f) (lambda (x y) (f y x))))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! not (lambda (x) (if x '() 'T)))", globalEnv, FALSE);

	/* TODO? : We could write 'soft' implementations of: mod > <= >= !=
	parseAndEvaluateEx("(set! > (reverse2args <))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! >= (compose2args < not))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! <= (reverse2args >=))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! <> (compose2args = not))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! != <>)", globalEnv, FALSE);

	parseAndEvaluateEx("(set! mod (lambda (m n) (- m (* n (/ m n)))))", globalEnv, FALSE);

	parseAndEvaluateEx("", globalEnv, FALSE);
	*/

	parseAndEvaluateEx("(set! mod %)", globalEnv, FALSE);
	parseAndEvaluateEx("(set! gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! <> !=)", globalEnv, FALSE);

	/* atom? Version 3; '() is a list but not a pair */
	parseAndEvaluateEx("(set! atom? (compose pair? not))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! any (combine id or '()))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! all (combine id and 'T))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! mapc (lambda (f) (combine f cons '())))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! mapcar (lambda (f l) ((mapc f) l)))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! append (lambda (l1 l2) ((combine id cons l2) l1)))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! reverse (lambda (l) (letrec ((rev-aux (lambda (l1 l2) (if (null? l1) l2 (rev-aux (cdr l1) (cons (car l1) l2)))))) (rev-aux l '()))))", globalEnv, FALSE);

	/* length : Adapted from Kamin page 29 */
	parseAndEvaluateEx("(set! length (lambda (l) (if (null? l) 0 (+1 (length (cdr l))))))", globalEnv, FALSE);
	/*
(set! equal (lambda (l1 l2) (cond ((atom? l1) (= l1 l2)) ((atom? l2) '()) ((equal (car l1) (car l2)) (equal (cdr l1) (cdr l2))) ('T '()) ))) ; Version 2

; Version 2 (or use combine ?)
(set find (lambda (pred lis)
	(cond
		((null? lis) '())
		((pred (car lis)) 'T)
		('T (find pred (cdr lis)))
	)
))

; TODO: filter : Try using combine and/or flatten1 ?
; flatten1: (flatten1 '((a) () (b) (c d))) -> '(a b c d)
(set! flatten1 (combine id append '()))

; pred2list :
(set! pred2list (lambda (f a) (if (f a) (list a) '())))
; Then:
(set! filter (lambda (pred l) (combine ...)))

; Original:
(set filter (lambda (pred l) ; Returns only the elements of l for which pred is true.
	(cond
		((null? l) '())
		((pred (car l)) (cons (car l) (filter pred (cdr l))))
		('T (filter pred (cdr l)))
	)
))
	*/

	/* Version 2: */
	parseAndEvaluateEx("(set! +1 ((curry +) 1))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! null '())", globalEnv, FALSE);

	/* END: Predefined variables in the global environment */

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	return globalEnv;
}

void freeGlobalEnvironment(LISP_ENV * globalEnv) {
	/* freeEnvironment(globalEnv); */
	/* freeValue(globalTrueValue); */
	globalTrueValue = NULL;
	/* freeValue(globalNullValue); */
	globalNullValue = NULL;
}

/* **** The End **** */
