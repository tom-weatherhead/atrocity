/* atrocity/src/evaluate.c */

/* **** Evaluation of expressions **** */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "array.h"
#include "associative-array.h"
#include "create-and-destroy.h"
#include "environment.h"
#include "evaluate.h"
#include "macro.h"
#include "print.h"
#include "utilities.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants */

/* Primops (primitive operators) : TODO? :
- Write >, <=, and >= in terms of <, not
- Write != in terms of =, not
- Distinguish between reference-equal and value-equal
*/
/* "and", "or", "??", */
/* Not yet implemented: "quote", "floor" */

static char * oneArgumentPrimops[] = {
	/* Type predicates */
	/* TODO: Remove either list? or pair? */
	"closure?", "list?", "null?", "number?", "pair?", "primop?", "string?", "symbol?",

	/* For LISP-style lists: */
	"car", "cdr", "list", "listtostring",

	/* For arrays (ordered sequences of values) : */
	"alength", "apop", "apeek", "ashift",

	/* For associative arrays: */
	"aasize",

	/* Miscellaneous */
	"print", "random", "throw", "call/cc",

	NULL
};
static char * twoArgumentPrimops[] = {
	"+", "-", "*", "/", "%", "<", ">", "<=", ">=", "=", "!=",
	"cons", "rplaca", "rplacd",
	"aaget", "aadeletekey",
	"apush", "aunshift",
	"ref=",
	NULL
};
static char * threeArgumentPrimops[] = { "if", "aaset", "aslice", NULL };

/* Local variables */

SCHEME_UNIVERSAL_TYPE * macroList = NULL;
static int nextContinuationId = 0;

/* Function prototypes */

/* Forward references */

static LISP_VALUE * evaluateClosureCall(LISP_CLOSURE * closure, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env);

/* Functions */

BOOL isPrimop(char * str) {
	return isStringInList(str, oneArgumentPrimops) || isStringInList(str, twoArgumentPrimops) || isStringInList(str, threeArgumentPrimops);
}

static LISP_VALUE * booleanToSchemeValue(int b) {
	return b ? globalTrueValue : globalNullValue;
}

static LISP_VALUE * evaluateAndCompareType(LISP_EXPR * operandExpr, LISP_ENV * env, int lispValueType) {
	LISP_VALUE * operandValue = evaluate(operandExpr, env);

	if (operandValue->type == lispPseudoValueType_ContinuationReturn) {
		return operandValue;
	}

	const int b = operandValue->type == lispValueType;

	return booleanToSchemeValue(b);
}

BOOL areValuesEqual(LISP_VALUE * v1, LISP_VALUE * v2) {

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

static LISP_VALUE * exprListToListValue(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env) {

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
}

/* static BOOL evaluatesToNull(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * value = evaluate(expr, env);

	return value->type == lispValueType_Null;
}

static LISP_VALUE * evaluateAnd(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {

		if (evaluatesToNull(getExprInExprList(actualParamExprs), env)) {
			/ * Enable short-circuiting * /
			return globalNullValue;
		}
	}

	return globalTrueValue;
}

static LISP_VALUE * evaluateOr(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {

		if (!evaluatesToNull(getExprInExprList(actualParamExprs), env)) {
			/ * Enable short-circuiting * /
			return globalTrueValue;
		}
	}

	return globalNullValue;
} */

/* Stolen from C# */
/* E.g. In C#, a ?? b ?? c is equiv to (a != null) ? a : ((b != null) ? b : c) */

/* static LISP_VALUE * evaluateDoubleQuestionMark(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {
		LISP_VALUE * value = evaluate(getExprInExprList(actualParamExprs), env);

		if (value->type != lispValueType_Null) {
			return value;
		}
	}

	return globalNullValue;
} */

static LISP_VALUE * evaluatePrimitiveOperatorCall(char * op, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	LISP_VALUE * result = NULL;

	/* BEGIN : These primops can take any number of args, including zero. */
	if (!strcmp(op, "list")) {
		return exprListToListValue(actualParamExprs, env);
	} /* else if (!strcmp(op, "and")) {
		return evaluateAnd(actualParamExprs, env);
	} else if (!strcmp(op, "or")) {
		return evaluateOr(actualParamExprs, env);
	} else if (!strcmp(op, "??")) {
		return evaluateDoubleQuestionMark(actualParamExprs, env);
	} */
	/* END : These primops can take any number of args, including zero. */

	/* if (actualParamExprs == NULL) {
		/ * These primops take exactly zero arguments * /
	} else */
	if (actualParamExprs != NULL) {
		LISP_EXPR * operand1Expr = getExprInExprList(actualParamExprs);
		LISP_VALUE * operand1Value = NULL;

		failIf(operand1Expr == NULL, "evaluatePrimitiveOperatorCall() : operand1Expr == NULL");

		if (isStringInList(op, oneArgumentPrimops)) {
			operand1Value = evaluate(operand1Expr, env);

			/* TODO? Is this worth doing?
			if (operand1Value->type == lispValueType_Array && isStringInList(op, arrayPrimops)) {
				return evaluateArrayPrimitiveOperatorCall(op, operand1Value, actualParamExprs->next, env);
			} else if (operand1Value->type == lispValueType_AssociativeArray && isStringInList(op, arrayPrimops)) {
				return evaluateAssociativeArrayPrimitiveOperatorCall(op, operand1Value, actualParamExprs->next, env);
			} */
		}

		/* These primops take exactly one argument */

		/* BEGIN : Value type predicates */
		if (!strcmp(op, "null?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Null);
		} else if (!strcmp(op, "number?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Number);
		} else if (!strcmp(op, "string?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_String);
		} else if (!strcmp(op, "symbol?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Symbol);
		} else if (!strcmp(op, "pair?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Pair);
		} else if (!strcmp(op, "list?")) {
			/* Note the difference between a pair and a list.
			The set of lists is a proper subset of the set of pairs.
			E.g. (cons 1 2) = (1 . 2) is a pair, but not a list. */
			return booleanToSchemeValue(isList(evaluate(operand1Expr, env)));
		} else if (!strcmp(op, "primop?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_PrimitiveOperator);
		} else if (!strcmp(op, "closure?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Closure);
		}
		/* END : Value type predicates */
		else if (!strcmp(op, "print")) {
			printValue(operand1Value);
			printf("\n");

			return operand1Value;
		} else if (!strcmp(op, "random")) {

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
		} else if (!strcmp(op, "car")) {

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

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair; type %d\n", operand1Value->type);
				fatalError("evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair");
			}

			return getTailInPair(operand1Value);
		} else if (!strcmp(op, "listtostring")) {
			/* TODO? : Let listtostring take 1-3 params:
			- The list
			- separatorBetweenListItems (string)
			- printBracketsAroundList (Boolean) */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (!isList(operand1Value)) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : listtostring : Operand is not a list\n");
				fatalError("evaluatePrimitiveOperatorCall() : listtostring : Operand is not a list");
			}

			STRING_BUILDER_TYPE * sb = printValueToString(NULL, operand1Value);

			return createStringValue(sb->name);
		} else if (!strcmp(op, "throw")) {
			fprintf(stderr, "An exception has been thrown.\n");

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type == lispValueType_String || operand1Value->type == lispValueType_Symbol) {
				fprintf(stderr, "    Message: '%s'\n", operand1Value->name);
			}

			fatalError("An exception has been thrown.");
		} else if (!strcmp(op, "call/cc")) {
			/* Call with current continuation */
			/* The arg must be a lambda expr that takes exactly one arg. */

			/* Remember: Evaluating a lambda expression results in a closure.
			This is different from calling a closure. */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Closure) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure\n");
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure");
			} else if (getArgsInClosure(operand1Value) == NULL || getArgsInClosure(operand1Value)->next != NULL) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Closure does not take exactly one argument\n");
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
		} else if (!strcmp(op, "alength")) {
			return getArrayLength(operand1Value);
		} else if (!strcmp(op, "apop")) {
			return pop(operand1Value);
		} else if (!strcmp(op, "apeek")) {
			return peek(operand1Value);
		} else if (!strcmp(op, "ashift")) {
			return shiftArray(operand1Value);
		} else if (!strcmp(op, "aasize")) {
			return aaSize(operand1Value);
		}

		if (actualParamExprs->next != NULL) {
			LISP_EXPR * operand2Expr = getExprInExprList(actualParamExprs->next);

			failIf(operand2Expr == NULL, "evaluatePrimitiveOperatorCall() : operand2Expr == NULL");

			/* These primops take exactly two arguments */

			if (isStringInList(op, twoArgumentPrimops)) {
				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

				if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
					return operand1Value;
				}

				LISP_VALUE * operand2Value = evaluate(operand2Expr, env);

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
					result = booleanToSchemeValue(areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "!=")) {
					result = booleanToSchemeValue(!areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "cons")) {
					return createPair(operand1Value, operand2Value);
				} else if (!strcmp(op, "rplaca")) {

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
				} else if (!strcmp(op, "aaget")) {
					return aaGet(operand1Value, operand2Value);
				} else if (!strcmp(op, "aadeletekey")) {
					return aaDeleteKey(operand1Value, operand2Value);
				} else if (!strcmp(op, "apush")) {
					return push(operand1Value, operand2Value);
				} else if (!strcmp(op, "aunshift")) {
					return unshiftArray(operand1Value, operand2Value);
				} else if (!strcmp(op, "ref=")) {
					return booleanToSchemeValue(operand1Value == operand2Value);
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

						if (operand2 == 0) {
							fprintf(stderr, "Division by zero error\n");
						} else {
							result = createNumericValue(operand1 / operand2);
						}
					} else if (!strcmp(op, "%")) {

						if (operand2 == 0) {
							fprintf(stderr, "Modulus by zero error\n");
						} else {
							result = createNumericValue(operand1 % operand2);
						}
					} else if (!strcmp(op, "<")) {
						result = booleanToSchemeValue(operand1 < operand2);
					} else if (!strcmp(op, ">")) {
						result = booleanToSchemeValue(operand1 > operand2);
					} else if (!strcmp(op, "<=")) {
						result = booleanToSchemeValue(operand1 <= operand2);
					} else if (!strcmp(op, ">=")) {
						result = booleanToSchemeValue(operand1 >= operand2);
					}
				}
			}

			if (actualParamExprs->next->next != NULL) {
				LISP_EXPR * operand3Expr = getExprInExprList(actualParamExprs->next->next);

				failIf(operand3Expr == NULL, "evaluatePrimitiveOperatorCall() : operand3Expr == NULL");

				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);
				LISP_VALUE * operand2Value = NULL;
				LISP_VALUE * operand3Value = NULL;

				if (strcmp(op, "if")) {
					/* If the primop is not "if"... */
					operand2Value = evaluate(operand2Expr, env);
					operand3Value = evaluate(operand3Expr, env);
				}

				/* These primops take exactly three arguments */

				if (!strcmp(op, "if")) {
					/* There must be 3 operands. First, evaluate the first operand. */
					/* If it is non-null, then evaluate and return the second operand. */
					/* Else evaluate and return the third operand. */

					if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
						return operand1Value;
					}

					/* Only one of [operand2Expr, operand3Expr] will be evaluated */
					result = evaluate(operand1Value->type != lispValueType_Null ? operand2Expr : operand3Expr, env);
				} else if (!strcmp(op, "aaset")) {
					failIf(operand1Value == NULL, "evaluatePrimitiveOperatorCall() : aaset : operand1Value == NULL");
					failIf(operand2Value == NULL, "evaluatePrimitiveOperatorCall() : aaset : operand2Value == NULL");
					failIf(operand3Value == NULL, "evaluatePrimitiveOperatorCall() : aaset : operand3Value == NULL");

					result = aaSet(operand1Value, operand2Value, operand3Value);

					failIf(result == NULL, "evaluatePrimitiveOperatorCall() : aaset : result == NULL");
				} else if (!strcmp(op, "aslice")) {
					result = sliceArray(operand1Value, operand2Value, operand3Value);
				}
			}
		}
	}

	if (result == NULL) {
		fprintf(stderr, "evaluatePrimitiveOperatorCall() : result == NULL; op is '%s'\n", op);
	}

	return result;
}

static LISP_VALUE_LIST_ELEMENT * evaluateExpressionList(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env) {

	if (exprList == NULL) {
		return NULL;
	}

	LISP_VALUE * value = evaluate(getExprInExprList(exprList), env);
	LISP_VALUE_LIST_ELEMENT * next = evaluateExpressionList(exprList->next, env);

	return createValueListElement(value, next);
}

static LISP_VALUE * evaluateClosureCall(LISP_CLOSURE * closure, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	LISP_VALUE_LIST_ELEMENT * valueList = evaluateExpressionList(actualParamExprs, env);
	LISP_ENV * newEnv = composeEnvironment(getArgsInClosure(closure), valueList, getEnvInClosure(closure));

	return evaluate(getBodyInClosure(closure), newEnv);

	/* ThAW 2022-08-18 : Don't free newEnv: Someone is still using it. E.g.:
	parseAndEvaluate("(((lambda (x) (lambda (y) x)) 5) 8)"); */
}

static SCHEME_UNIVERSAL_TYPE * getMacro(LISP_EXPR * expr) {

	if (expr->type != lispExpressionType_Variable) {
		return NULL;
	}

	char * macroName = getVarInExpr(expr)->name;
	SCHEME_UNIVERSAL_TYPE * mle = NULL;

	for (mle = macroList; mle != NULL; mle = mle->next) {
		SCHEME_UNIVERSAL_TYPE * macro = getMacroInMacroListElement(mle);

		if (!strcmp(macroName, macro->name)) {
			return macro;
		}
	}

	return NULL;
}

static LISP_VALUE * evaluateFunctionCall(LISP_FUNCTION_CALL * functionCall, LISP_ENV * env) {
	LISP_EXPR * firstExpr = getFirstExprInFunctionCall(functionCall);

	/* If firstExpr is a variable, search for a macro with the same name */
	SCHEME_UNIVERSAL_TYPE * macro = getMacro(firstExpr);

	if (macro != NULL) {
		return invokeMacro(macro, getActualParamExprsInFunctionCall(functionCall), env);
	}

	LISP_VALUE * callableValue = evaluate(firstExpr, env);

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
				fprintf(stderr, "evaluateFunctionCall() : Bad number of parameters (!= 1) when calling a continuation\n");
				fatalError("evaluateFunctionCall() : Bad number of parameters (!= 1) when calling a continuation");
				return NULL;
			}

			LISP_VALUE * actualParamValue = evaluate(getExprInExprList(getActualParamExprsInFunctionCall(functionCall)), env);

			if (actualParamValue->type == lispPseudoValueType_ContinuationReturn) {
				return actualParamValue;
			}

			return createContinuationReturn(getContinuationIdInValue(callableValue), actualParamValue);

		default:
			fprintf(stderr, "evaluateFunctionCall() : Attempted to call an uncallable value\n");
			fatalError("evaluateFunctionCall() : Attempted to call an uncallable value");
			return NULL;
	}
}

static LISP_VALUE * evaluateLambdaExpression(LISP_LAMBDA_EXPR * lambdaExpr, LISP_ENV * env) {
	return createClosure(getArgsInLambdaExpr(lambdaExpr), getBodyInLambdaExpr(lambdaExpr), env);
}

static LISP_VALUE * evaluateSetExpression(LISP_EXPR * setExpr, LISP_ENV * env) {

	if (setExpr->type != lispExpressionType_SetExpr) {
		fprintf(stderr, "evaluateSetExpression() : Expression is not a Set expression\n");
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
	LISP_ENV * newEnv = createEnvironment(NULL);
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

	newEnv->next = env;

	LISP_VALUE * result = evaluate(getExprInExpr(expr), newEnv);

	newEnv->next = NULL;

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

		addNameToEnvironment(newEnv, varExprPairList->name, value);

		env = newEnv;
		varExprPairList = varExprPairList->next;
	}

	return evaluate(getExprInExpr(expr), env);
}

static LISP_VALUE * evaluateLetrecExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(env);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = getVarExprPairListInExpr(expr);

	for (; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		/* Add all variables that are bound in this.bindings to newEnvFrame before any closures are created in the next loop. */
		addNameToEnvironment(newEnv, varExprPairList->name, globalNullValue);
	}

	for (varExprPairList = getVarExprPairListInExpr(expr); varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		LISP_VALUE * value = evaluate(getExprInVarExprPairListElement(varExprPairList), newEnv);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		updateNameIfFoundInNameValueList(getNameValuePairListInEnv(newEnv), varExprPairList->name, value);
	}

	return evaluate(getExprInExpr(expr), newEnv);
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


		if (conditionValue->type == lispPseudoValueType_ContinuationReturn) {
			return conditionValue;
		} else if (conditionValue->type != lispValueType_Null) {
			return evaluate(getExpr2InPairListElement(exprPair), env);
		}
	}

	return globalNullValue;
}

static LISP_VALUE * evaluateDefineMacroExpression(LISP_EXPR * macro, LISP_ENV * env) {
	/* Note bene: This does not check for an existing macro with the same name. Just create a schemeStructType_Macro object and add it to the head of the list. */

	macroList = createMacroListElement(macro, macroList);

	return globalTrueValue;
}

static BOOL isValueType(int type) {
	return type >= lispType_FirstValueType && type <= lispType_LastValueType;
}

LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = NULL;
	LISP_VALUE * value = NULL;

	switch (expr->type) {
		case lispExpressionType_Value:
			result = getValueInExpr(expr);
			break;

		case lispValueType_QuotedConstantWithApostrophe:
			result = getValueInApostropheQuotedValue(expr);
			break;

		case lispValueType_QuotedConstantWithQuoteKeyword:
			result = getValueInQuoteQuotedValue(expr);
			break;

		case lispExpressionType_Variable:
			value = lookupVariableInEnvironment(getVarInExpr(expr), env);

			if (value == NULL) {
				fprintf(stderr, "evaluate() : Undefined variable '%s'\n", getVarInExpr(expr)->name);
				fatalError("evaluate() : Undefined variable");
				return NULL;
			}

			result = value;
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

		case lispExpressionType_Macro:
			result = evaluateDefineMacroExpression(expr, env);
			break;

		default:
			fatalError("evaluate() : Unrecognized expression type");
			return NULL;
	}

	if (result == NULL) {
		fprintf(stderr, "evaluate() : result == NULL : expr type is %d\n", expr->type);
	}

	failIf(result == NULL, "evaluate() : result == NULL");
	failIf(!isValueType(result->type), "evaluate() : result->type is not a value type");

	return result;
}

/* **** The End **** */
