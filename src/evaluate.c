/* atrocity/src/evaluate.c */

/* **** Evaluation of expressions **** */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "evaluate.h"
#include "print.h"
#include "thunk.h"
#include "utilities.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants */

static char * twoArgumentPrimops[] = {
	"+", "-", "*", "/", "%", "<", ">", "<=", ">=", "=", "!=",
	"cons", "rplaca", "rplacd", NULL
};

/* Local variables */

static int nextContinuationId = 0;

/* Function prototypes */

/* Forward references */

static LISP_VALUE * evaluateClosureCall(LISP_CLOSURE * closure, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env);

/* Functions */

static LISP_VALUE * booleanToClonedValue(int b) {
	return cloneValue(b ? globalTrueValue : globalNullValue);
}

/* static LISP_VALUE * evaluateAndCompareType(LISP_EXPR * operandExpr, LISP_ENV * env, int lispValueType) {
	LISP_VALUE * operandValue = evaluate(operandExpr, env); */
static LISP_VALUE * evaluateAndCompareType(LISP_VALUE * operandValue, LISP_ENV * env, int lispValueType) {
	failIf(operandValue->type == lispValueType_Thunk, "evaluateAndCompareType() : operandValue is a thunk");

	if (operandValue->type == lispPseudoValueType_ContinuationReturn) {
		return operandValue;
	}

	const int b = operandValue->type == lispValueType;

	return booleanToClonedValue(b);
}

static BOOL areValuesEqual(LISP_VALUE * v1, LISP_VALUE * v2) {
	dethunk(v1);
	dethunk(v2);

	failIf(v1->type == lispValueType_Thunk, "areValuesEqual() : v1 is a thunk");
	failIf(v2->type == lispValueType_Thunk, "areValuesEqual() : v2 is a thunk");

	if (v1->type != v2->type) {
		return FALSE;
	}

	switch (v1->type) {
		case lispValueType_Null:
			return TRUE;

		case lispValueType_Number:
			return getIntegerValueInValue(v1) == getIntegerValueInValue(v2);

		case lispValueType_String:
		case lispValueType_Symbol:
		case lispValueType_PrimitiveOperator:
			return !strcmp(getNameInValue(v1), getNameInValue(v2));

		case lispValueType_Pair:
			return areValuesEqual(getHeadInPair(v1), getHeadInPair(v2)) && areValuesEqual(getTailInPair(v1), getTailInPair(v2));

		case lispValueType_Closure:
			return FALSE;

		default:
			fprintf(stderr, "areValuesEqual() : Unexpected value type %d\n", v1->type);
			break;
	}

	return FALSE;
}

/* static LISP_VALUE * exprListToListValue(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env) {

	if (exprList == NULL) {
		return createNull();
	}

	LISP_VALUE * head = evaluate(getExprInExprList(exprList), env);

	if (head->type == lispPseudoValueType_ContinuationReturn) {
		return head;
	}

	LISP_VALUE * tail = exprListToListValue(exprList->next, env);

	if (tail->type == lispPseudoValueType_ContinuationReturn) {
		return tail;
	}

	return createPair(head, tail);
} */

static BOOL evaluatesToNull(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * value = evaluate(expr, env);

	return value->type == lispValueType_Null;
}

static LISP_VALUE * evaluateAnd(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {

		if (evaluatesToNull(getExprInExprList(actualParamExprs), env)) {
			/* Enable short-circuiting */
			return globalNullValue;
		}
	}

	return globalTrueValue;
}

static LISP_VALUE * evaluateOr(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {

		if (!evaluatesToNull(getExprInExprList(actualParamExprs), env)) {
			/* Enable short-circuiting */
			return globalTrueValue;
		}
	}

	return globalNullValue;
}

/* Stolen from C# */
/* E.g. In C#, a ?? b ?? c is equiv to (a != null) ? a : ((b != null) ? b : c) */

static LISP_VALUE * evaluateDoubleQuestionMark(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {
		LISP_VALUE * value = evaluate(getExprInExprList(actualParamExprs), env);

		if (value->type != lispValueType_Null) {
			return value;
		}

		/* freeValue(value); */
	}

	return globalNullValue;
}

static LISP_VALUE * listOfValuesToListValue(SCHEME_UNIVERSAL_TYPE * listOfValuesOrThunks) {

	if (listOfValuesOrThunks == NULL) {
		return createNull();
	}

	/* TEMP:
	return createPair(getValueInValueListElement(listOfValuesOrThunks), listOfValuesToListValue(listOfValuesOrThunks->next)); */

	return createPair(dethunk(getValueInValueListElement(listOfValuesOrThunks)), listOfValuesToListValue(listOfValuesOrThunks->next));
}

static LISP_VALUE * evaluatePrimitiveOperatorCall(char * op, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	/* printf("evaluatePrimitiveOperatorCall: op is %s\n", op); */

	LISP_VALUE * result = NULL;

	/* BEGIN Thunk support */

	LISP_VALUE_LIST_ELEMENT * listOfValuesOrThunks = exprListToListOfValuesOrThunks(actualParamExprs, env);

	/* Handle cond, cons, if, list: */

	/* if (!strcmp(op, "cond")) {
	} else */ if (!strcmp(op, "cons") && listOfValuesOrThunks != NULL && listOfValuesOrThunks->next != NULL && listOfValuesOrThunks->next->next == NULL) {
		return createPair(getValueInValueListElement(listOfValuesOrThunks), getValueInValueListElement(listOfValuesOrThunks->next));
	} else if (!strcmp(op, "if") && listOfValuesOrThunks != NULL && listOfValuesOrThunks->next != NULL && listOfValuesOrThunks->next->next != NULL && listOfValuesOrThunks->next->next->next == NULL) {
		LISP_VALUE * operand1Value = dethunk(getValueInValueListElement(listOfValuesOrThunks));

		if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
			return operand1Value;
		}

		LISP_VALUE * operand2Thunk = getValueInValueListElement(listOfValuesOrThunks->next);
		LISP_VALUE * operand3Thunk = getValueInValueListElement(listOfValuesOrThunks->next->next);

		return dethunk(operand1Value->type != lispValueType_Null ? operand2Thunk : operand3Thunk);
	} else if (!strcmp(op, "list")) {
		return listOfValuesToListValue(listOfValuesOrThunks);
	}

	/* switch (this.name.value) {
		case 'cons':
			return new SExpressionList(argumentsAsSExpressions[0], argumentsAsSExpressions[1]);
			-> return createPair(listOfValuesOrThunks->value1, listOfValuesOrThunks->next->value1);

		case 'list':
			return SExpressionList.makeFromList(argumentsAsSExpressions);
			-> return listOfValuesToListValue(listOfValuesOrThunks);

		case 'if':
			return this.executeIf(argumentsAsSExpressions, globalInfo as SASLGlobalInfo);
			->
			if (!strcmp(op, "if") && actualParamExprs->next->next != NULL && getExprInExprList(actualParamExprs->next->next) != NULL) {
				LISP_EXPR * operand3Expr = getExprInExprList(actualParamExprs->next->next);
				LISP_VALUE * operand1Value = dethunk(operand1Expr);

				if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
					return operand1Value;
				}

				return dethunk(operand1Value->type != lispValueType_Null ? operand2Expr : operand3Expr);
			}

		case 'cond':
			return this.executeCond(argumentsAsSExpressions, globalInfo);

		default:
			break;
	}

	*/

	/* Dethunk: */
	/* const evaluatedArguments = argumentsAsSExpressions.map((sexpr) =>
		isThunk(sexpr) ? sexpr.dethunk(globalInfo) : sexpr
	);
	*/
	LISP_VALUE_LIST_ELEMENT * evaluatedArguments = dethunkList(listOfValuesOrThunks);

	/* Handle car, cdr:
	- car: Return the dethunked head of arg 0
	- cdr: Return the dethunked tail of arg 0 */
	if (evaluatedArguments != NULL && evaluatedArguments->next == NULL && (!strcmp(op, "car") || !strcmp(op, "cdr"))) {
		LISP_VALUE * pair = getValueInValueListElement(evaluatedArguments);

		/* printf("evaluatePrimpOp: car or cdr: pair->type is %d\n", pair->type); */

		failIf(pair->type != lispValueType_Pair, "evaluatePrimpOp: Was expecting a pair for car or cdr");

		if (!strcmp(op, "car")) {
			return dethunk(getHeadInPair(pair));
		} else /* if (!strcmp(op, "cdr")) */ {
			return dethunk(getTailInPair(pair));
		}
	}

	/* END Thunk support */

	/* BEGIN : These primops can take any number of args, including zero. * /
	if (!strcmp(op, "list")) {
		return exprListToListValue(actualParamExprs, env);
	} else */ if (!strcmp(op, "and")) {
		return evaluateAnd(actualParamExprs, env);
	} else if (!strcmp(op, "or")) {
		return evaluateOr(actualParamExprs, env);
	} else if (!strcmp(op, "??")) {
		return evaluateDoubleQuestionMark(actualParamExprs, env);
	}
	/* END : These primops can take any number of args, including zero. */

	/* if (actualParamExprs != NULL && getExprInExprList(actualParamExprs) != NULL) { */
	if (evaluatedArguments != NULL && getValueInValueListElement(evaluatedArguments) != NULL) {
		/* LISP_EXPR * operand1Expr = getExprInExprList(actualParamExprs); */
		LISP_VALUE * operand1Value = getValueInValueListElement(evaluatedArguments);

		/* BEGIN : Value type predicates */
		if (!strcmp(op, "null?")) {
			failIf(operand1Value->type == lispValueType_Thunk, "null? : operand1 is a thunk");
			return evaluateAndCompareType(operand1Value, env, lispValueType_Null);
		} else if (!strcmp(op, "number?")) {
			return evaluateAndCompareType(operand1Value, env, lispValueType_Number);
		} else if (!strcmp(op, "string?")) {
			return evaluateAndCompareType(operand1Value, env, lispValueType_String);
		} else if (!strcmp(op, "symbol?")) {
			return evaluateAndCompareType(operand1Value, env, lispValueType_Symbol);
		} else if (!strcmp(op, "pair?")) {
			return evaluateAndCompareType(operand1Value, env, lispValueType_Pair);
		} else if (!strcmp(op, "list?")) {
			/* Note the difference between a pair and a list.
			The set of lists is a proper subset of the set of pairs.
			E.g. (cons 1 2) = (1 . 2) is a pair, but not a list. */
			return booleanToClonedValue(isList(operand1Value));
		} else if (!strcmp(op, "primop?")) {
			return evaluateAndCompareType(operand1Value, env, lispValueType_PrimitiveOperator);
		} else if (!strcmp(op, "closure?")) {
			return evaluateAndCompareType(operand1Value, env, lispValueType_Closure);
		}
		/* END : Value type predicates */
		else if (!strcmp(op, "print")) {
			/* LISP_VALUE * operand1Value = evaluate(operand1Expr, env); */

			printValue(operand1Value);
			printf("\n");

			/* Return without freeing the values */

			return operand1Value;
		} else if (!strcmp(op, "random")) {
			/* LISP_VALUE * operand1Value = evaluate(operand1Expr, env); */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			}

			if (operand1Value->type != lispValueType_Number || getIntegerValueInValue(operand1Value) <= 0) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : random : Bad parameter\n");
				fprintf(stderr, "  operand1Value->type is: %d\n", operand1Value->type);
				fprintf(stderr, "  getIntegerValueInValue(operand1Value) is: %d\n", getIntegerValueInValue(operand1Value));
				fatalError("evaluatePrimitiveOperatorCall() : random : Bad parameter");
			}

			return createNumericValue(rand() % getIntegerValueInValue(operand1Value));
		} else /* if (!strcmp(op, "car")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : car : Operand is not a pair; type %d\nOperand is: ", operand1Value->type);
				printValue(operand1Value);
				fprintf(stderr, "\noperand1Expr type is %d\n", operand1Expr->type);
				fatalError("evaluatePrimitiveOperatorCall() : car : Operand is not a pair");
			}

			return getHeadInPair(operand1Value);
		} else if (!strcmp(op, "cdr")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair; type %d\n", operand1Value->type);
				fatalError("evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair");
			}

			return getTailInPair(operand1Value);
		} else */ if (!strcmp(op, "listtostring")) {
			/* TODO? : Let listtostring take 1-3 params:
			- The list
			- separatorBetweenListItems (string)
			- printBracketsAroundList (Boolean) */
			/* LISP_VALUE * operand1Value = evaluate(operand1Expr, env); */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (!isList(operand1Value)) {
				fatalError("evaluatePrimitiveOperatorCall() : listtostring : Operand is not a list");
			}

			STRING_BUILDER_TYPE * sb = printValueToString(NULL, operand1Value, NULL, FALSE);

			return createStringValue(sb->name);
		} else if (!strcmp(op, "throw")) {
			fprintf(stderr, "An exception has been thrown.\n");

			/* LISP_VALUE * operand1Value = evaluate(operand1Expr, env); */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type == lispValueType_String || operand1Value->type == lispValueType_Symbol) {
				fprintf(stderr, "    Message: '%s'\n", operand1Value->name);
			}

			fatalError("An exception has been thrown.");
		} else if (!strcmp(op, "call/cc")) {
			/* Call with current continuation */
			/* The arg must be a lambda expr that takes exactly one arg. */
			/* LISP_VALUE * operand1Value = evaluate(operand1Expr, env); */

			/* Remember: Evaluating a lambda expression results in a closure.
			This is different from calling a closure. */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Closure) {
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure");
			} else if (getArgsInClosure(operand1Value) == NULL || getArgsInClosure(operand1Value)->next != NULL) {
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Closure does not take exactly one argument");
			}

			SCHEME_UNIVERSAL_TYPE * currentContinuation = createContinuation(nextContinuationId++);

			/* Now call the closure (operand1Value), passing in
			the currentContinuation as the one and only parameter */

			LISP_VALUE * result = evaluateClosureCall(operand1Value, createExpressionListElement(createExpressionFromValue(currentContinuation), NULL), env);

			if (result->type == lispPseudoValueType_ContinuationReturn && getContinuationIdInValue(result) == getContinuationIdInValue(currentContinuation)) {
				/* Unwrap the value inside */

				return getContinuationReturnValueInValue(result);
			}

			return result;
		}

		/* if (actualParamExprs->next != NULL && getExprInExprList(actualParamExprs->next) != NULL) {
			LISP_EXPR * operand2Expr = getExprInExprList(actualParamExprs->next); */
		if (evaluatedArguments->next != NULL && getValueInValueListElement(evaluatedArguments->next) != NULL) {
			LISP_VALUE * operand2Value = getValueInValueListElement(evaluatedArguments->next);

			if (isStringInList(op, twoArgumentPrimops)) {
				/* LISP_VALUE * operand1Value = evaluate(operand1Expr, env); */

				if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
					return operand1Value;
				}

				/* LISP_VALUE * operand2Value = evaluate(operand2Expr, env); */

				if (operand2Value->type == lispPseudoValueType_ContinuationReturn) {
					return operand2Value;
				}

				// TODO:
				// if (!strcmp(op, "eq?")) {
				// } else if (!strcmp(op, "eqv?")) {
				// } else if (!strcmp(op, "equal?")) {
				// } else
				if (!strcmp(op, "=")) {
					/* Note: In real Scheme, = vs. eq? vs. eqv? vs. equal? :

					See https://stackoverflow.com/questions/16299246/what-is-the-difference-between-eq-eqv-equal-and-in-scheme

					Some behaviour of eq? and eqv? are implementation-specific.

					(eq? 2 2)     => depends upon the implementation -> Use =, eqv?. or equal?
					(eq? "a" "a") => depends upon the implementation

					(eqv? 2 2)     => #t
					(eqv? "a" "a") => depends upon the implementation -> Use equal?

					In general:

					- Use the = predicate when you wish to test whether two numbers are equivalent. (= only works for numbers.)
					- Use the eqv? predicate when you wish to test whether two non-numeric non-complex values are equivalent.
					- Use the equal? predicate when you wish to test whether two complex values (lists, vectors, etc.) are equivalent.
					- Don't use the eq? predicate unless you know exactly what you're doing. (The eq? predicate is used to check whether its two parameters respresent the same object in memory.)

					See also the Scheme language specification: 11.5  Equivalence predicates :
					http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.5

					See also e.g. https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_8.html

					In LISP:
					- equal: takes two arguments and returns t if they are
					structurally equal or nil otherwise.
					- eq: takes two arguments and returns t if they are same
					identical objects, sharing the same memory location
					or nil otherwise. (i.e. reference-equals)
					- eql: takes two arguments and returns t if the arguments
					are eq, or if they are numbers of the same type with
					the same value, or if they are character objects that
					represent the same character, or nil otherwise. */
					result = booleanToClonedValue(areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "!=")) {
					result = booleanToClonedValue(!areValuesEqual(operand1Value, operand2Value));
				} else /* if (!strcmp(op, "cons")) {
					/ * Return without freeing the values * /
					return createPair(operand1Value, operand2Value);
				} else */ if (!strcmp(op, "rplaca")) {

					if (operand1Value->type != lispValueType_Pair) {
						fprintf(stderr, "evaluatePrimitiveOperatorCall() : rplaca : Operand is not a pair; type %d\n", operand1Value->type);
						fatalError("evaluatePrimitiveOperatorCall() : rplaca : Operand is not a pair");
					}

					getHeadInPair(operand1Value) = operand2Value;

					return operand2Value;
				} else if (!strcmp(op, "rplacd")) {

					if (operand1Value->type != lispValueType_Pair) {
						fprintf(stderr, "evaluatePrimitiveOperatorCall() : rplacd : Operand is not a pair; type %d\n", operand1Value->type);
						fatalError("evaluatePrimitiveOperatorCall() : rplacd : Operand is not a pair");
					}

					getTailInPair(operand1Value) = operand2Value;

					return operand2Value;
				} else if (operand1Value->type == lispValueType_Number && operand2Value->type == lispValueType_Number) {
					/* Both operands must be numeric */
					const int operand1 = getIntegerValueInValue(operand1Value);
					const int operand2 = getIntegerValueInValue(operand2Value);

					if (!strcmp(op, "+")) {
						result = createNumericValue(operand1 + operand2);
					} else if (!strcmp(op, "-")) {
						result = createNumericValue(operand1 - operand2);
					} else if (!strcmp(op, "*")) {
						result = createNumericValue(operand1 * operand2);
					} else if (!strcmp(op, "/")) {
						failIf(operand2 == 0, "Division by zero error");

						result = createNumericValue(operand1 / operand2);
					} else if (!strcmp(op, "%")) {
						failIf(operand2 == 0, "Modulus by zero error");

						result = createNumericValue(operand1 % operand2);
					} else if (!strcmp(op, "<")) {
						result = booleanToClonedValue(operand1 < operand2);
					} else if (!strcmp(op, ">")) {
						result = booleanToClonedValue(operand1 > operand2);
					} else if (!strcmp(op, "<=")) {
						result = booleanToClonedValue(operand1 <= operand2);
					} else if (!strcmp(op, ">=")) {
						result = booleanToClonedValue(operand1 >= operand2);
					}
				}
			}

			/* Handle if */
			/* There must be 3 operands. First, evaluate the first operand. */
			/* If it is non-null, then evaluate and return the second operand. */
			/* Else evaluate and return the third operand. */

			/* if (!strcmp(op, "if") && actualParamExprs->next->next != NULL && getExprInExprList(actualParamExprs->next->next) != NULL) {
				LISP_EXPR * operand3Expr = getExprInExprList(actualParamExprs->next->next);
				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

				if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
					return operand1Value;
				}

				result = evaluate(operand1Value->type != lispValueType_Null ? operand2Expr : operand3Expr, env);
			} */
		}
	}

	failIf(result == NULL, "evaluatePrimOp was about to return NULL");
	failIf(result->type == lispValueType_Thunk, "evaluatePrimOp was about to return a thunk");

	return result;
}

static LISP_VALUE * evaluateClosureCall(LISP_CLOSURE * closure, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	/* TODO? : Deal with thunks? */

	LISP_ENV * newEnv = createEnvironment(getEnvInClosure(closure));

	LISP_VAR_LIST_ELEMENT * np = getArgsInClosure(closure); /* closure->args; */
	LISP_EXPR_LIST_ELEMENT * ep = actualParamExprs;

	while (np != NULL || ep != NULL) {

		if (np == NULL || ep == NULL) {
			/* The formal and actual parameter lists have different lengths. */
			/* freeEnvironment(newEnv); */
			fatalError("evaluateClosureCall() : The formal and actual parameter lists have different lengths.");
			return NULL;
		}

		failIf(np->type != schemeStructType_VariableListElement, "evaluateClosureCall() : np->type != schemeStructType_VariableListElement");

		LISP_VALUE * value = evaluate(getExprInExprList(ep), env); /* TODO: env or closure->env ? */

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* printf("Closure call: Binding name '%s' to value ", np->name);
		printValue(value);
		printf("\n"); */

		/* if (!strcmp(np->name, "newLevel")) {
			exit(1);
		} */

		addNameToEnvironment(newEnv, np->name, value);
		np = np->next;
		ep = ep->next;
	}

	LISP_VALUE * result = evaluate(getBodyInClosure(closure), newEnv);

	/* ThAW 2022-08-18 : Don't free newEnv: Someone is still using it. E.g.:
	parseAndEvaluate("(((lambda (x) (lambda (y) x)) 5) 8)"); */

	return result;
}

static LISP_VALUE * evaluateFunctionCall(LISP_FUNCTION_CALL * functionCall, LISP_ENV * env) {
	LISP_VALUE * callableValue = evaluate(getFirstExprInFunctionCall(functionCall), env);

	failIf(callableValue == NULL, "evaluateFunctionCall: callableValue is NULL");
	failIf(callableValue->type == lispValueType_Thunk, "evaluateFunctionCall: callableValue is a thunk");

	/* printf("evaluateFunctionCall: callableValue->type is %d\n", callableValue->type); */

	if (callableValue->type == lispPseudoValueType_ContinuationReturn) {
		return callableValue;
	}

	switch (callableValue->type) {
		case lispValueType_PrimitiveOperator:
			return evaluatePrimitiveOperatorCall(callableValue->name, getActualParamExprsInFunctionCall(functionCall), env);

		case lispValueType_Closure:
			return evaluateClosureCall(callableValue, getActualParamExprsInFunctionCall(functionCall), env);

		case lispPseudoValueType_Continuation:
			/* There must be exactly one actual parameter */

			if (getActualParamExprsInFunctionCall(functionCall) == NULL || getActualParamExprsInFunctionCall(functionCall)->next != NULL) {
				fatalError("evaluateFunctionCall() : Bad number of parameters (!= 1) when calling a continuation");
				return NULL;
			}

			LISP_VALUE * actualParamValue = evaluate(getExprInExprList(getActualParamExprsInFunctionCall(functionCall)), env);

			if (actualParamValue->type == lispPseudoValueType_ContinuationReturn) {
				return actualParamValue;
			}

			return createContinuationReturn(getContinuationIdInValue(callableValue), actualParamValue);

		default:
			fatalError("evaluateFunctionCall() : Attempted to call an uncallable value");
			return NULL;
	}
}

static LISP_VALUE * evaluateLambdaExpression(LISP_LAMBDA_EXPR * lambdaExpr, LISP_ENV * env) {
	return createClosure(getArgsInLambdaExpr(lambdaExpr), getBodyInLambdaExpr(lambdaExpr), env);
}

static LISP_VALUE * evaluateSetExpression(LISP_EXPR * setExpr, LISP_ENV * env) {

	if (setExpr->type != lispExpressionType_SetExpr) {
		fatalError("evaluateSetExpression() : Expression is not a Set expression");
		return NULL;
	}

	LISP_VALUE * value = evaluate(getExprInExpr(setExpr), env);

	if (value->type == lispPseudoValueType_ContinuationReturn) {
		return value;
	}

	/* X TODO: Use addBubbleDown() instead of setValueInEnvironment() */
	setValueInEnvironment(env, getVarInExpr(setExpr), value);

	return value;
}

static LISP_VALUE * evaluateLetExpression(LISP_EXPR * expr, LISP_ENV * env) {
	/* LISP_ENV * newEnv = createEnvironment(NULL); */
	LISP_ENV * newEnv = createEnvironment(env);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList;

	for (varExprPairList = getVarExprPairListInExpr(expr); varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		LISP_VALUE * value = evaluate(getExprInVarExprPairListElement(varExprPairList), env);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* Note: This constructs the list in reverse order... */
		/* TODO: Implement this using recursion instead. */
		addNameToEnvironment(newEnv, varExprPairList->name, value);
	}

	/* newEnv->next = env; */

	LISP_VALUE * result = evaluate(getExprInExpr(expr), newEnv);

	/* newEnv->next = NULL; */ /* BUG FIX? 2022-09-12 */

	return result;
}

static LISP_VALUE * evaluateLetStarExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = getVarExprPairListInExpr(expr);

	while (varExprPairList != NULL) {
		LISP_ENV * newEnv = createEnvironment(env);
		LISP_VALUE * value = evaluate(getExprInVarExprPairListElement(varExprPairList), env);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* newEnv->value1 = createNameValueListElement(varExprPairList->name, value, newEnv->value1); */
		addNameToEnvironment(newEnv, varExprPairList->name, value);

		env = newEnv;
		varExprPairList = varExprPairList->next;
	}

	return evaluate(getExprInExpr(expr), env);
}

static LISP_VALUE * evaluateLetrecExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(env);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = getVarExprPairListInExpr(expr);

	/* printf("evaluateLetrecExpression: Begin\n"); */

	for (; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		/* Add all variables that are bound in this.bindings to newEnvFrame before any closures are created in the next loop. */
		addNameToEnvironment(newEnv, varExprPairList->name, globalNullValue);
	}

	/* printf("evaluateLetrecExpression: Done first part\n"); */

	for (varExprPairList = getVarExprPairListInExpr(expr); varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		LISP_VALUE * value = evaluate(getExprInVarExprPairListElement(varExprPairList), newEnv);

		failIf(value == NULL, "evaluateLetrecExpression 1a: value is NULL");
		failIf(value->type == lispValueType_Thunk, "evaluateLetrecExpression 1b: value is a thunk");

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		updateNameIfFoundInNameValueList(newEnv->value1, varExprPairList->name, value);
	}

	/* printf("evaluateLetrecExpression: Done second part\n");

	printf("evaluateLetrecExpression: expr->type is %d\n", expr->type); */

	LISP_EXPR * exprInExpr = getExprInExpr(expr);

	/* printf("evaluateLetrecExpression: exprInExpr->type is %d\n", exprInExpr->type);

	if (exprInExpr->type == lispExpressionType_FunctionCall) {
		printf("**** Crash for FunctionCall?\n");
	} */

	LISP_VALUE * result = evaluate(exprInExpr, newEnv);

	/* if (exprInExpr->type == lispExpressionType_FunctionCall) {
		printf("**** No crash for FunctionCall\n");
	}

	printf("evaluateLetrecExpression: result->type is %d\n", result->type); */

	failIf(result == NULL, "evaluateLetrecExpression 2a: result is NULL");
	failIf(result->type == lispValueType_Thunk, "evaluateLetrecExpression 2b: result is a thunk");

	/* printf("evaluateLetrecExpression: Done\n"); */

	return result;
}

static LISP_VALUE * evaluateBeginExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = globalNullValue;
	LISP_EXPR_LIST_ELEMENT * exprList;

	for (exprList = getExprListInExpr(expr); exprList != NULL; exprList = exprList->next) {
		result = evaluate(getExprInExprList(exprList), env);

		if (result->type == lispPseudoValueType_ContinuationReturn) {
			break;
		}
	}

	if (result == NULL) {
		fatalError("evaluateBeginExpression() tried to return NULL");
	}

	return result;
}

static LISP_VALUE * evaluateWhileExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = NULL;

	for (;;) {
		result = evaluate(getExprInExpr(expr), env);

		if (result->type == lispValueType_Null || result->type == lispPseudoValueType_ContinuationReturn) {
			break;
		}

		LISP_VALUE * value = evaluate(getExpr2InExpr(expr), env);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}
	}

	return result;
}

static LISP_VALUE * evaluateCondExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_EXPR_PAIR_LIST_ELEMENT * exprPair;

	for (exprPair = getExprPairListInExpr(expr); exprPair != NULL; exprPair = exprPair->next) {
		LISP_VALUE * conditionValue = evaluate(getExprInPairListElement(exprPair), env);
		/* TODO: Dethunk */

		if (conditionValue->type == lispPseudoValueType_ContinuationReturn) {
			return conditionValue;
		} else if (conditionValue->type != lispValueType_Null) {
			return evaluate(getExpr2InPairListElement(exprPair), env);
			/* TODO: Dethunk */
		}
	}

	return globalNullValue;
}

LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * value = NULL;
	LISP_VALUE * result = NULL;

	switch (expr->type) {
		case lispExpressionType_Value:
			/* Return a clone of the value so it can be freed separately */
			result = cloneValue(getValueInExpr(expr));
			break;

		case lispExpressionType_Variable:
			value = lookupVariableInEnvironment(getVarInExpr(expr), env);

			if (value == NULL) {
				fprintf(stderr, "evaluate() : Undefined variable '%s'\n", getVarInExpr(expr)->name);
				fatalError("evaluate() : Undefined variable");
				return NULL;
			}

			result = cloneValue(value);
			break;

		case lispExpressionType_FunctionCall:
			result = evaluateFunctionCall(expr, env);
			break;

		case lispExpressionType_LambdaExpr:
			result = evaluateLambdaExpression(expr, env);
			break;

		case lispExpressionType_SetExpr:
			result = evaluateSetExpression(expr, env);
			break;

		case lispExpressionType_Let:
			result = evaluateLetExpression(expr, env);
			break;

		case lispExpressionType_LetStar:
			result = evaluateLetStarExpression(expr, env);
			break;

		case lispExpressionType_Letrec:
			result = evaluateLetrecExpression(expr, env);
			break;

		case lispExpressionType_Begin:
			result = evaluateBeginExpression(expr, env);
			break;

		case lispExpressionType_While:
			result = evaluateWhileExpression(expr, env);
			break;

		case lispExpressionType_Cond:
			result = evaluateCondExpression(expr, env);
			break;

		default:
			fatalError("evaluate() : Unrecognized expression type");
			return NULL;
	}

	failIf(result == NULL, "evaluate() : Was about to return NULL");
	failIf(result->type == lispValueType_Thunk, "evaluate() : Was about to return a thunk");

	return result;
}

/* **** The End **** */
