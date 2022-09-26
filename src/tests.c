/* atrocity/src/tests.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h" /* Needed to provide BOOL */

#include "associative-array.h"
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
	char * actualOutput = NULL;
	LISP_ENV * globalEnv = createGlobalEnvironment();

	BOOL valuePrintedSuccessfully = TRUE;
	BOOL outputValuesMatch = TRUE;
	char * input = NULL;
	char * expectedOutput = NULL;
	int i;
	STRING_BUILDER_TYPE * sb = NULL;

	for (i = 0; valuePrintedSuccessfully && outputValuesMatch; ++i) {
		input = inputs[i];
		expectedOutput = expectedOutputs[i];

		if (input == NULL || expectedOutput == NULL) {
			break;
		}

		LISP_VALUE * value = parseStringAndEvaluate(input, globalEnv);

		clearStringBuilder(sb);
		sb = printValueToStringEx(sb, value, NULL, TRUE);

		actualOutput = sb->name;

		outputValuesMatch = strlen(expectedOutput) > 0 && !strcmp(actualOutput, expectedOutput);
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

	freeGlobalEnvironment();
	freeAllStructs();

	if (!valuePrintedSuccessfully || !outputValuesMatch) {
		exit(1);
	}
}

static void test(char * input, char * expectedOutput) {
	char * inputs[] = { input, NULL };
	char * expectedOutputs[] = { expectedOutput, NULL };

	multitest(inputs, expectedOutputs);
}

static void testAssociativeArray() {
	printf("testAssociativeArray() : BEGIN\n");

	LISP_VALUE * associativeArray = createAssociativeArray();

	LISP_VALUE * keyAbc = createStringValue("abc");
	LISP_VALUE * key456 = createNumericValue(456);

	LISP_VALUE * value123 = createNumericValue(123);
	LISP_VALUE * valueDef = createStringValue("def");

	LISP_VALUE * getResult0 = aaGet(associativeArray, keyAbc);

	failIf(getResult0 != NULL, "testAssociativeArray() : getResult0 != NULL");

	aaSet(associativeArray, keyAbc, value123);
	aaSet(associativeArray, key456, valueDef);

	LISP_VALUE * getResult1 = aaGet(associativeArray, keyAbc);
	LISP_VALUE * getResult2 = aaGet(associativeArray, key456);

	failIf(getResult1 == NULL, "testAssociativeArray() : getResult1 == NULL");
	failIf(getResult1->type != value123->type, "testAssociativeArray() : getResult1 has wrong type");
	failIf(getIntegerValueInValue(getResult1) != getIntegerValueInValue(value123), "testAssociativeArray() : getResult1 has wrong value");

	failIf(getResult2 == NULL, "testAssociativeArray() : getResult2 == NULL");
	failIf(getResult2->type != valueDef->type, "testAssociativeArray() : getResult2 has wrong type");
	failIf(getNameInValue(getResult2) == NULL, "testAssociativeArray() : getResult2 name == NULL");
	failIf(getNameInValue(valueDef) == NULL, "testAssociativeArray() : valueDef name == NULL");
	failIf(strcmp(getNameInValue(getResult2), getNameInValue(valueDef)), "testAssociativeArray() : getResult2 has wrong name");

	/* Update a key's value */
	LISP_VALUE * value1337 = createNumericValue(1337);

	aaSet(associativeArray, keyAbc, value1337);

	LISP_VALUE * getResult3 = aaGet(associativeArray, keyAbc);

	failIf(getResult3 == NULL, "testAssociativeArray() : getResult3 == NULL");
	failIf(getResult3->type != value1337->type, "testAssociativeArray() : getResult3 has wrong type");
	failIf(getIntegerValueInValue(getResult3) != getIntegerValueInValue(value1337), "testAssociativeArray() : getResult3 has wrong value");

	freeAllStructs();

	printf("testAssociativeArray() : END\n");
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

	printf("testStringBuilder() : END\n");
}

void runTests() {
	printf("\nRunning tests...\n");

	/* testGetIdentifier("abc (def weatherhead) ghi");
	testGetIdentifier("(+1 7)");
	testGetIdentifier("(((a b) c) d)");
	/ * testGetIdentifier(""); */

	testAssociativeArray();
	testStringBuilder();

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

	/* filter */
	char * inputsFilter[] = {
		/* "(set! flatten1 (combine id append '()))", */

		"(set! pred2list (lambda (f) (lambda (a) (if (f a) (list a) '()))))",

		/* "(set! filter (lambda (pred l) (flatten1 (mapcar (pred2list pred) l))))", */
		"(set! filter (lambda (pred l) ((combine (pred2list pred) append '()) l)))",

		"(filter (lambda (n) (= 0 (mod n 2))) '(1 2 3 4 5 6 7 8))",

		NULL
	};
	char * expectedResultsFilter[] = {
		/* "<closure>", */
		"<closure>",
		"<closure>",
		"(2 4 6 8)",
		NULL
	};

	multitest(inputsFilter, expectedResultsFilter);

	/* length - version 2 */
	char * inputsLengthV2[] = {
		"(set! lengthv2 (combine (lambda (n) 1) + 0))",
		"(lengthv2 '())",
		"(lengthv2 '(2 3 5 7))",
		NULL
	};
	char * expectedResultsLengthV2[] = {
		"<closure>",
		"0",
		"4",
		NULL
	};

	multitest(inputsLengthV2, expectedResultsLengthV2);

	/* Call/cc test - From Kamin page 128 */

	char * inputsCallCC[] = {
		"(set! gcd* (lambda (l) \
			(call/cc (lambda (exit) \
				(letrec ((gcd*-aux (lambda (l) \
					(if (= (car l) 1) (exit 1) \
						(if (null? (cdr l)) (car l) \
							(gcd (car l) (gcd*-aux (cdr l)))))))) \
					(gcd*-aux l))))))",
		"(gcd* '(9 27 81 60))",
		"(gcd* '(101 202 103))",
		"(gcd* '(9 27 1 81 60))",
		"(gcd* '(9 27 81 60 1 NotANumber))",
		NULL
	};
	char * expectedResultsCallCC[] = {
		"<closure>",
		"3",
		"1",
		"1",
		"1",
		NULL
	};

	multitest(inputsCallCC, expectedResultsCallCC);

	/* Scheme static scope test
	See page 135 of Kamin, or pages 128-137 for more context about static vs. dynamic scope. */

	char * inputsStaticScope[] = {
		"(set! add (lambda (x) (lambda (y) (+ x y))))",
		"(set! add1 (add 1))",
		"(set! f (lambda (x) (add1 x)))",
		/* Assert that our Scheme uses static scope, as Scheme should. */
		"(f 5)",
		NULL
	};
	char * expectedResultsStaticScope[] = {
		"<closure>",
		"<closure>",
		"<closure>",
		"6",
		NULL
	};

	multitest(inputsStaticScope, expectedResultsStaticScope);

	/* Scheme Global vs. Local Variable test */

	char * inputsGlobalVsLocalVar[] = {
		"(set! a 1)",
		"(set! afunc (lambda () a))",
		"(set! func2 (lambda (a) (afunc)))",
		/* Assert that our Scheme uses static scope, as Scheme should. */
		"(func2 0)",
		NULL
	};
	char * expectedResultsGlobalVsLocalVar[] = {
		"1",
		"<closure>",
		"<closure>",
		"1",
		NULL
	};

	multitest(inputsGlobalVsLocalVar, expectedResultsGlobalVsLocalVar);

	/* streams test */

	// See Kamin pages 176-178 : "SASL vs. Scheme"
	// This Scheme code uses zero-argument closures to mimic SASL thunks.
	// If s is a stream, (car s) is a number, and ((cadr s)) is a stream.

	char * inputsStreams[] = {
		"(set cadr (lambda (x) (car (cdr x))))",
		"(set add-streams (lambda (s1 s2) \
			(list (+ (car s1) (car s2)) (lambda () (add-streams ((cadr s1)) ((cadr s2))))) \
		))",
		"(set stream-first-n (lambda (n s) \
			(if (= n 0) '() \
				(cons (car s) (stream-first-n (- n 1) ((cadr s))))) \
		))",
		"(set powers-of-2 (list 1 (lambda () (add-streams powers-of-2 powers-of-2))))",
		"(set fibonacci (list 0 (lambda () (list 1 (lambda () (add-streams fibonacci ((cadr fibonacci))))))))",
		"(stream-first-n 5 powers-of-2)",
		"(stream-first-n 8 fibonacci)",
		NULL
	};
	char * expectedResultsStreams[] = {
		"<closure>",
		"<closure>",
		"<closure>",
		"(1 <closure>)",
		"(0 <closure>)",
		"(1 2 4 8 16)",
		"(0 1 1 2 3 5 8 13)",
		NULL
	};

	multitest(inputsStreams, expectedResultsStreams);

	/* rplaca and rplacd test - See Kamin page 55 */

	char * inputsRplacaRplacd[] = {
		"(set x '(a b c))",
		"(set y x)",
		"(rplaca y 'd)",
		"y",
		"x",
		"(rplacd y 'e)",
		"y",
		"x",
		NULL
	};
	char * expectedResultsRplacaRplacd[] = {
		"(a b c)",
		"(a b c)",
		"d",
		"(d b c)",
		"(d b c)",
		"e",
		"(d . e)",
		"(d . e)",
		NULL
	};

	multitest(inputsRplacaRplacd, expectedResultsRplacaRplacd);

	/* macro test */
	/* From Kamin pages 56-57, and Exercise 12, from pages 62-63 (in the LISP chapter) */

	char * inputsMacro[] = {
		"(define-macro for (indexvar lower upper body) \
			(list 'begin \
				(list 'set indexvar lower) \
				(list 'while \
					(list '<= indexvar upper) \
					(list 'begin body \
						(list 'set indexvar (list '+ indexvar 1))))))",
		"(set! sum 0)",
		"(for x 1 10 (set sum (+ sum x)))",
		"sum",
		NULL
	};
	char * expectedResultsMacro[] = {
		"T",
		"0",
		"()",
		"55",
		NULL
	};

	multitest(inputsMacro, expectedResultsMacro);

	/* BUG: Any tests placed after the macro test will seg fault.
	Do we need to clear the list of macros? */

	/* array test */

	char * inputsArray[] = {
		"(set! a [])",
		"a",
		"(alength a)",

		"(apush a 1)",
		"(apush a 2)",
		"(apush a 3)",
		"a",
		"(alength a)",

		"(apop a)",
		"a",
		"(alength a)",

		"(ashift a)",
		"a",
		"(alength a)",

		"(aunshift a 4)",
		"a",
		"(alength a)",

		/* TODO: Test aslice */

		NULL
	};
	char * expectedResultsArray[] = {
		"[]",
		"[]",
		"0",

		"1",
		"2",
		"3",
		"[1, 2, 3]",
		"3",

		"3",
		"[1, 2]",
		"2",

		"1",
		"[2]",
		"1",

		"4",
		"[4, 2]",
		"2",

		NULL
	};

	multitest(inputsArray, expectedResultsArray);

	/* associative array test */

	char * inputsAssociativeArray[] = {
		"(set! aa {})",
		"(aasize aa)",
		"(aaget aa \"abc\")",

		"(aaset aa \"abc\" 123)",
		"(aaset aa 456 \"def\")",
		"(aaget aa \"abc\")",
		"(aaget aa 456)",
		"(aaset aa \"abc\" 1337)",
		"(aaget aa \"abc\")",
		"(aasize aa)",

		"(aaset aa '(7 13) '())",
		"(aaget aa '(7 13))",
		"(aasize aa)",
		"(aaset aa '(7 13) (cons '(2 3) (aaget aa '(7 13))))",
		"(aaget aa '(7 13))",
		"(aasize aa)",

		"(aadeletekey aa \"abc\")",
		"(aaget aa \"abc\")",
		"(aasize aa)",

		NULL
	};
	char * expectedResultsAssociativeArray[] = {
		"<associative array>",
		"0",
		"()",

		"123",
		"def",
		"123",
		"def",
		"1337",
		"1337",
		"2",

		"()",
		"()",
		"3",
		"((2 3))",
		"((2 3))",
		"3",

		"1337",
		"()",
		"2",

		NULL
	};

	multitest(inputsAssociativeArray, expectedResultsAssociativeArray);

	printf("\nDone.\n");
}

/* **** The End **** */
