/* atrocity/src/tests.c */

/* TODO:
- Test the parsing (without evaluation) of a macro
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h" /* Needed to provide BOOL */

/* #include "char-source.h" */
#include "create-and-destroy.h"
#include "environment.h"
#include "memory-manager.h"
#include "parse-and-evaluate.h"
#include "print.h"
#include "string-builder.h"

/* Function prototypes */

/* Functions */

/* void testGetIdentifier(char * str) {
	char dstBuf[maxStringValueLength];
	int i = 0;
	CharSource * cs = createCharSource(str);

	printf("\nTest getIdentifier('%s') :\n\n", str);

	while (getIdentifier(cs, dstBuf, sizeof(dstBuf), NULL) > 0) {
		printf("getIdentifier %d: dstBuf is '%s'\n", i++, dstBuf);
	}

	freeCharSource(cs);
} */

static void multitest(char * inputs[], char * expectedOutputs[]) {
	/* const int sizeOfActualOutput = maxStringValueLength * sizeof(char); */
	char * actualOutput = NULL; /* mmAlloc(sizeOfActualOutput); */
	LISP_ENV * globalEnv = createGlobalEnvironment();

	BOOL valuePrintedSuccessfully = TRUE;
	BOOL outputValuesMatch = TRUE;
	char * input = NULL;
	char * expectedOutput = NULL;
	int i;

	for (i = 0; valuePrintedSuccessfully && outputValuesMatch; ++i) {
		input = inputs[i];
		expectedOutput = expectedOutputs[i];

		if (input == NULL || expectedOutput == NULL) {
			break;
		}

		LISP_VALUE * value = parseStringAndEvaluate(input, globalEnv);

		/* memset(actualOutput, 0, sizeOfActualOutput); */

		/* valuePrintedSuccessfully = printValueToString(value, actualOutput, maxStringValueLength); */
		STRING_BUILDER_TYPE * sb = printValueToString(NULL, value);

		actualOutput = sb->name;

		/* Note bene: freeClosure is currently mostly disabled to avoid
		 * double-freeing things. We must fix this. */
		/* freeValue(value); */

		outputValuesMatch = !strcmp(actualOutput, expectedOutput);
	}

	if (!valuePrintedSuccessfully) {
		fprintf(stderr, "\nTest failed: Output string truncated\n");
		fprintf(stderr, "  Input: %s\n", input);
		fprintf(stderr, "  Expected output: %s\n", expectedOutput);
	} else if (!outputValuesMatch) {
		fprintf(stderr, "\nTest failed:\n");
		fprintf(stderr, "  Input: %s\n", input);
		fprintf(stderr, "  Expected output: %s\n", expectedOutput);
		fprintf(stderr, "  Actual output: %s\n\n", actualOutput);
	}

	freeGlobalEnvironment(globalEnv); /* TODO: -> freeAllStructs(); */

	/* mmFree(actualOutput); */

	if (!valuePrintedSuccessfully || !outputValuesMatch) {
		exit(1);
	}
}

static void test(char * input, char * expectedOutput) {
	char * inputs[] = { input, NULL };
	char * expectedOutputs[] = { expectedOutput, NULL };

	multitest(inputs, expectedOutputs);
}

static void testStringBuilder() {
	printf("testStringBuilder() : BEGIN\n");

	STRING_BUILDER_TYPE * sb = createStringBuilder(0);

	appendToStringBuilder(sb, "abcdefg");
	appendToStringBuilder(sb, "hijklmno");
	appendToStringBuilder(sb, "p");
	appendToStringBuilder(sb, "qrstuv");
	appendToStringBuilder(sb, "wxyz");

	failIf(strlen(sb->name) != 26, "strlen(sb->name) != 26");

	printf("sb->name is '%s'\n", sb->name);
	printf("sb->maxNameLength is %d\n", sb->maxNameLength);
	failIf(sb->maxNameLength != 32, "sb->maxNameLength != 32");

	/* failIf(, "");
	failIf(, "");
	failIf(, "");
	failIf(, "");
	failIf(, ""); */

	printf("testStringBuilder() : END\n");
}

void runTests() {
	printf("\nRunning tests...\n");

	test("7", "7");
	test("+", "+");
	test("(+ 2 3)", "5");
	test("(+ (+ 2 3) 8)", "13");
	test("(if 1 2 3)", "2");
	test("(if null 2 3)", "3");
	test("(+ 13 21)", "34");
	test("(- 21 13)", "8");
	test("(* 7 13)", "91");
	test("(/ 100 7)", "14");
	test("(% 100 7)", "2");
	test("(null? null)", "T");
	test("(null? '())", "T");
	test("(null? 13)", "()");
	test("(lambda (x) x)", "<closure>"); /* The identity function */
	test("(lambda (x) 1)", "<closure>"); /* A constant function */
	test("((lambda (x) x) 13)", "13"); /* Call the identity function */
	test("((lambda (x y) (+ x y)) 5 8)", "13");

	test("(((lambda (x) (lambda (y) y)) 5) 8)", "8"); /* Succeeds */

	test("(((lambda (x) (lambda (y) x)) 5) 8)", "5");

	test("(((lambda (x) (lambda (y) (+ x y))) 5) 8)", "13");

	test("(set! n 7)", "7");

	/* cons */
	test("(cons 1 null)", "(1)");
	test("(cons 1 (cons 2 null))", "(1 2)");
	test("(cons 1 (cons 2 (cons 3 null)))", "(1 2 3)");
	test("(cons 1 '(2 3))", "(1 2 3)");

	/* car */
	test("(car (cons 1 null))", "1");
	test("(car '(1))", "1");
	test("(car (cons 1 (cons 2 null)))", "1");
	test("(car '(1 2))", "1");

	/* cdr */
	test("(cdr (cons 1 null))", "()");
	test("(cdr '(1))", "()");
	test("(cdr (cons 1 (cons 2 null)))", "(2)");
	test("(cdr '(1 2))", "(2)");

	/* let */
	test("(let ((a 7)) a)", "7");
	test("(let ((a 5) (b 8)) (+ a b))", "13");

	/* let* */
	test("(let* ((a 7)) a)", "7");
	test("(let* ((a 7) (b (+ a 1))) b)", "8");

	/* letrec */
	/* From thaw-grammar: 'LL(1) Scheme letrec test' */
	test("(letrec ((countones (lambda (l) (if (null? l) 0 (if (= (car l) 1) (+ 1 (countones (cdr l))) (countones (cdr l))))))) (countones '(1 2 3 1 0 1 1 5)))", "4");

	/* begin */
	test("(begin 1 2 4 3)", "3");
	test("(begin (set! n 2) (+ n 3))", "5");

	/* This {} syntax only works as an initializer; i.e. when strs is declared */
	char * inputs2[] = {
		"(set! add (lambda (a b) (+ a b)))",
		"(add 13 21)",
		NULL
	};
	char * expectedResults2[] = {
		"<closure>",
		"34",
		NULL
	};
	multitest(inputs2, expectedResults2);

	char * inputs3[] = {
		"(set! n 1)",
		"(while (<= n 5) (begin (print n) (set! n (+ n 1))))",
		NULL
	};
	char * expectedResults3[] = {
		"1",
		"()",
		NULL
	};

	multitest(inputs3, expectedResults3);

	/* cond test */
	char * inputs4[] = {
		/* "(set! n 1)", */
		"(set! n 2)",
		/* "(set! n 3)", */
		/* "(set! n 4)", */
		"(cond ((= n 1) 111) ((= n 2) 222) ((= n 3) 333) (1 777))",
		NULL
	};
	char * expectedResults4[] = {
		"2",
		"222",
		NULL
	};

	multitest(inputs4, expectedResults4);

	/* curry test */
	char * inputs6[] = {
		"(set! curry2 (lambda (f) (lambda (x) (lambda (y) (f x y)))))",
		"(((curry2 +) 2) 3)",
		NULL
	};
	char * expectedResults6[] = {
		"<closure>",
		"5",
		NULL
	};

	multitest(inputs6, expectedResults6);

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

	testStringBuilder();

	printf("\nDone.\n");
}

/* **** The End **** */
