/* atrocity/src/tests.c */

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

void parseAndEvaluate(char * str);

/* Functions */

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

void testGetIdentifier(char * str) {
	char dstBuf[maxStringValueLength];
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

	/* parseAndEvaluate(""); */

	/* parseAndEvaluateStringList(["", "", "", ..., NULL]); */

	/* Tests from thaw-grammar:
test('LL(1) Scheme let* non-recursive test', () => {
	// 2014/02/17 : Derived from Kamin page 126.

	// Assert that let* is not a clone of letrec.

	expect(() =>
		evaluateToISExpression(
			[
				'(let*',
				'((countones (lambda (l)',
				'(if (null? l) 0',
				'	(if (= (car l) 1) (+ 1 (countones (cdr l)))',
				'	(countones (cdr l)))))))',
				"(countones '(1 2 3 1 0 1 1 5)))"
			].join('\n')
		)
	).toThrow();
});

test('LL(1) Scheme call/cc test', () => {
	// From Kamin page 128.
	schemeTest([
		['(set mod (lambda (m n) (- m (* n (/ m n)))))', '<closure>'],
		['(set gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))', '<closure>'],
		[
			'(set gcd* (lambda (l) ' +
				'(call/cc (lambda (exit) ' +
				'(letrec ((gcd*-aux (lambda (l) ' +
				'    (if (= (car l) 1) (exit 1) ' +
				'        (if (null? (cdr l)) (car l) ' +
				'            (gcd (car l) (gcd*-aux (cdr l)))))))) ' +
				'    (gcd*-aux l))))))',
			'<closure>'
		],
		["(gcd* '(9 27 81 60))", '3'],
		["(gcd* '(101 202 103))", '1'],
		["(gcd* '(9 27 1 81 60))", '1'],
		["(gcd* '(9 27 81 60 1 NotANumber))", '1']
	]);
});

test('LL(1) Scheme static scope test', () => {
	// See page 135 of Kamin, or pages 128-137 for more context about static vs. dynamic scope.
	schemeTest([
		['(set add (lambda (x) (lambda (y) (+ x y))))', '<closure>'],
		['(set add1 (add 1))', '<closure>'],
		['(set f (lambda (x) (add1 x)))', '<closure>'],
		// Assert that our Scheme uses static scope, as Scheme should.
		['(f 5)', '6']
	]);
});

test('LL(1) Scheme Global vs. Local Variable test', () => {
	schemeTest([
		['(set a 1)', '1'],
		['(set afunc (lambda () a))', '<closure>'],
		['(set func2 (lambda (a) (afunc)))', '<closure>'],
		['(func2 0)', '1']
	]);
});
	*/

	/* testGetIdentifier("abc (def weatherhead) ghi");
	testGetIdentifier("(+1 7)");
	testGetIdentifier("(((a b) c) d)");
	/ * testGetIdentifier(""); */

	printf("\nDone.\n");
}

/* **** The End **** */
