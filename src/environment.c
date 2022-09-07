/* atrocity/src/environment.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "parse-and-evaluate.h"

/* External variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Function prototypes */

/* Functions */

static LISP_VALUE * lookupVariableInNameValueList(LISP_VAR * var, LISP_NAME_VALUE_LIST_ELEMENT * nvle) {

	while (nvle != NULL) {

		if (!strcmp(nvle->name, var->name)) {
			return nvle->value;
		}

		nvle = nvle->next;
	}

	return NULL;
}

LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env) {
	LISP_VALUE * value = NULL;

	while (env != NULL) {
		value = lookupVariableInNameValueList(var, env->nameValueList);

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

static BOOL updateIfFoundInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {


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

	/* TODO? : We could write 'soft' implementations of:
	mod > <= >= !=
	(set! mod (lambda (m n) (- m (* n (/ m n)))))
	(set! > (lambda (x y) (< y x))) ; or (set! > (reverse2args <))
	(set! <= (compose2args > not))
	(set! >= (compose2args < not))
	(set! != (compose2args = not)) */

	/* We could also implement Boolean logic functionally:
	(set! true (lambda (t f) t))
	(set! false (lambda (t f) f))
	(set! if (lambda (b x y) (b x y)))
	(set! and (lambda (x y) (if x y x)))
	(set! or (lambda (x y) (if x x y)))
	(set! not (lambda (x) (if x false true))) */

	parseAndEvaluateEx("(set! compose2args (lambda (f g) (lambda (x y) (g (f x y)))))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! reverse2args (lambda (f) (lambda (x y) (f y x))))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! not (lambda (x) (if x '() 'T)))", globalEnv, FALSE);
	parseAndEvaluateEx("(set! mod %)", globalEnv, FALSE);
	parseAndEvaluateEx("(set! gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))", globalEnv, FALSE);
	/*
; (set! > (reverse2args <)) ; Comment out if Scheme implements > as a primop
; (set! and (lambda (x y) (if x y x)))
; (set! or (lambda (x y) (if x x y)))
; (set! mod (lambda (m n) (- m (* n (/ m n)))))

; (set! atom? (lambda (x) (or (null? x) (or (number? x) (or (symbol? x) (string? x)))))) ; What about primop? and closure? ?
; (set! atom? (compose list? not)) ; Version 2
(set! atom? (compose pair? not)) ; Version 3; '() is a list but not a pair

; (set! equal (lambda (l1 l2) (if (atom? l1) (= l1 l2) (if (atom? l2) '() (if (equal (car l1) (car l2)) (equal (cdr l1) (cdr l2)) '()))))) ; Version 1
(set! equal (lambda (l1 l2) (cond ((atom? l1) (= l1 l2)) ((atom? l2) '()) ((equal (car l1) (car l2)) (equal (cdr l1) (cdr l2))) ('T '()) ))) ; Version 2

; (set! >= (compose2args < not)) ; Comment out if Scheme implements >= as a primop
; (set! <= (compose2args > not)) ; Comment out if Scheme implements <= as a primop
; (set! <> (compose2args = not))
(set! <> !=)
(set! any (lambda (l) (if (null? l) '() (if (car l) 'T (any (cdr l))))))
(set! all (lambda (l) (if (null? l) 'T (if (not (car l)) '() (all (cdr l))))))

(set! any2 (combine id or '()))
(set! all2 (combine id and 'T))

; (set! mapcar (lambda (f l) (if (null? l) '() (cons (f (car l)) (mapcar f (cdr l)))))) ; Original definition.
; (set! mapc (curry mapcar)) ; Original definition.  From page 101.
(set! mapc (lambda (f) (combine f cons '()))) ; Second definition.
(set! mapcar (lambda (f l) ((mapc f) l))) ; Second definition.
	*/

	/* ; (set! +1 (lambda (n) (+ n 1))) ; Version 1 */
	/* Version 2: */
	parseAndEvaluateEx("(set! +1 ((curry +) 1))", globalEnv, FALSE);

	parseAndEvaluateEx("(set! null '())", globalEnv, FALSE);

	/* END: Predefined variables in the global environment */

	failIf(globalTrueValue == NULL, "globalTrueValue is NULL");
	failIf(globalNullValue == NULL, "globalNullValue is NULL");

	return globalEnv;
}

void freeGlobalEnvironment(LISP_ENV * globalEnv) {
	freeEnvironment(globalEnv);
	freeValue(globalTrueValue);
	globalTrueValue = NULL;
	freeValue(globalNullValue);
	globalNullValue = NULL;
}

/* **** The End **** */
