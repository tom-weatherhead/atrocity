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

static LISP_VALUE * evaluateAndCompareType(LISP_EXPR * operandExpr, LISP_ENV * env, int lispValueType) {
	LISP_VALUE * operandValue = evaluate(operandExpr, env);

	if (operandValue->type == lispPseudoValueType_ContinuationReturn) {
		return operandValue;
	}

	const int b = operandValue->type == lispValueType;

	/* freeValue(operandValue); */

	return booleanToClonedValue(b);
}

static BOOL areValuesEqual(LISP_VALUE * v1, LISP_VALUE * v2) {

	if (v1->type != v2->type) {
		return FALSE;
	}

	switch (v1->type) {
		case lispValueType_Null:
			return TRUE;

		case lispValueType_Number:
			return v1->value == v2->value;

		case lispValueType_String:
		case lispValueType_Symbol:
		case lispValueType_PrimitiveOperator:
			return !strcmp(v1->name, v2->name);

		case lispValueType_Pair:
			return areValuesEqual(v1->pair->head, v2->pair->head) && areValuesEqual(v1->pair->tail, v2->pair->tail);

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

	LISP_VALUE * head = evaluate(exprList->expr, env);

	if (head->type == lispPseudoValueType_ContinuationReturn) {
		return head;
	}

	LISP_VALUE * tail = exprListToListValue(exprList->next, env);

	if (tail->type == lispPseudoValueType_ContinuationReturn) {
		return tail;
	}

	return createPair(head, tail);
}

static BOOL evaluatesToNull(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * value = evaluate(expr, env);
	BOOL result = value->type == lispValueType_Null;

	/* freeValue(value); */

	return result;
}

static LISP_VALUE * evaluateAnd(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {

		if (evaluatesToNull(actualParamExprs->expr, env)) {
			/* Enable short-circuiting */
			return globalNullValue;
		}
	}

	return globalTrueValue;
}

static LISP_VALUE * evaluateOr(LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {

	for (; actualParamExprs != NULL; actualParamExprs = actualParamExprs->next) {

		if (!evaluatesToNull(actualParamExprs->expr, env)) {
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
		LISP_VALUE * value = evaluate(actualParamExprs->expr, env);

		if (value->type != lispValueType_Null) {
			return value;
		}

		/* freeValue(value); */
	}

	return globalNullValue;
}

static LISP_VALUE * evaluatePrimitiveOperatorCall(char * op, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	LISP_VALUE * result = NULL;

	/* BEGIN : These primops can take any number of args, including zero. */
	if (!strcmp(op, "list")) {
		return exprListToListValue(actualParamExprs, env);
	} else if (!strcmp(op, "and")) {
		return evaluateAnd(actualParamExprs, env);
	} else if (!strcmp(op, "or")) {
		return evaluateOr(actualParamExprs, env);
	} else if (!strcmp(op, "??")) {
		return evaluateDoubleQuestionMark(actualParamExprs, env);
	}
	/* END : These primops can take any number of args, including zero. */

	if (actualParamExprs != NULL && actualParamExprs->expr != NULL) {
		LISP_EXPR * operand1Expr = actualParamExprs->expr;

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
			return booleanToClonedValue(isList(evaluate(operand1Expr, env)));
		} else if (!strcmp(op, "primop?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_PrimitiveOperator);
		} else if (!strcmp(op, "closure?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Closure);
		}
		/* END : Value type predicates */
		else if (!strcmp(op, "print")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			printValue(operand1Value);
			printf("\n");

			/* Return without freeing the values */

			return operand1Value;
		} else if (!strcmp(op, "random")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			}

			if (operand1Value->type != lispValueType_Number || operand1Value->value <= 0) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : random : Bad parameter\n");
				fatalError("evaluatePrimitiveOperatorCall() : random : Bad parameter");
			}

			return createNumericValue(rand() % operand1Value->value);
		} else if (!strcmp(op, "car")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : car : Operand is not a pair; type %d\nOperand is: ", operand1Value->type);
				printValue(operand1Value);
				fprintf(stderr, "\noperand1Expr type is %d\n", operand1Expr->type);
				fatalError("evaluatePrimitiveOperatorCall() : car : Operand is not a pair");
			}

			return operand1Value->pair->head;
		} else if (!strcmp(op, "cdr")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair; type %d\n", operand1Value->type);
				fatalError("evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair");
			}

			return operand1Value->pair->tail;
		} else if (!strcmp(op, "listtostring")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (!isList(operand1Value)) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : listtostring : Operand is not a list\n");
				fatalError("evaluatePrimitiveOperatorCall() : listtostring : Operand is not a list");
			}

			/* TODO: Use a StringBuilder */
			LISP_VALUE * result = createStringValue("");

			if (!printValueToString(operand1Value, result->name, getNumCharsAllocatedToNameBufInValue(result))) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : listtostring : Destination string buffer overflow\n");
				fatalError("evaluatePrimitiveOperatorCall() : listtostring : Destination string buffer overflow");
			}

			return result;
		} else if (!strcmp(op, "throw")) {
			fprintf(stderr, "An exception has been thrown.\n");

			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type == lispValueType_String || operand1Value->type == lispValueType_Symbol) {
				fprintf(stderr, "    Message: '%s'\n", operand1Value->name);
			}

			fatalError("An exception has been thrown.");
		} else if (!strcmp(op, "call/cc")) {
			/* Call with current continuation */
			/* The arg must be a lambda expr that takes exactly one arg. */
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			/* Remember: Evaluating a lambda expression results in a closure.
			This is different from calling a closure. */

			if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
				return operand1Value;
			} else if (operand1Value->type != lispValueType_Closure) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure\n");
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure");
			} else if (getArgsInClosure(operand1Value->closure) == NULL || getArgsInClosure(operand1Value->closure)->next != NULL) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Closure does not take exactly one argument\n");
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Closure does not take exactly one argument");
			}

			LISP_VALUE * currentContinuation = createUndefinedValue();

			currentContinuation->type = lispPseudoValueType_Continuation;
			currentContinuation->continuationId = nextContinuationId++;

			/* Now call the closure (operand1Value), passing in
			the currentContinuation as the one and only parameter */

			LISP_VALUE * result = evaluateClosureCall(operand1Value->closure, createExpressionListElement(createExpressionFromValue(currentContinuation), NULL), env);

			if (result->type == lispPseudoValueType_ContinuationReturn && result->continuationId == currentContinuation->continuationId) {
				/* Unwrap the value inside */

				return result->continuationReturnValue;
			}

			return result;
		}

		if (actualParamExprs->next != NULL && actualParamExprs->next->expr != NULL) {
			LISP_EXPR * operand2Expr = actualParamExprs->next->expr;

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
					result = booleanToClonedValue(areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "!=")) {
					result = booleanToClonedValue(!areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "cons")) {
					/* Return without freeing the values */
					return createPair(operand1Value, operand2Value);
				} else if (!strcmp(op, "rplaca")) {

					if (operand1Value->type != lispValueType_Pair) {
						fprintf(stderr, "evaluatePrimitiveOperatorCall() : rplaca : Operand is not a pair; type %d\n", operand1Value->type);
						fatalError("evaluatePrimitiveOperatorCall() : rplaca : Operand is not a pair");
					}

					operand1Value->pair->head = operand2Value;

					return operand2Value;
				} else if (!strcmp(op, "rplacd")) {

					if (operand1Value->type != lispValueType_Pair) {
						fprintf(stderr, "evaluatePrimitiveOperatorCall() : rplacd : Operand is not a pair; type %d\n", operand1Value->type);
						fatalError("evaluatePrimitiveOperatorCall() : rplacd : Operand is not a pair");
					}

					operand1Value->pair->tail = operand2Value;

					return operand2Value;
				} else if (operand1Value->type == lispValueType_Number && operand2Value->type == lispValueType_Number) {
					/* Both operands must be numeric */
					const int operand1 = operand1Value->value;
					const int operand2 = operand2Value->value;

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
						result = booleanToClonedValue(operand1 < operand2);
					} else if (!strcmp(op, ">")) {
						result = booleanToClonedValue(operand1 > operand2);
					} else if (!strcmp(op, "<=")) {
						result = booleanToClonedValue(operand1 <= operand2);
					} else if (!strcmp(op, ">=")) {
						result = booleanToClonedValue(operand1 >= operand2);
					}
				}

				/* freeValue(operand2Value);
				freeValue(operand1Value); */
			}

			/* Handle if */
			/* There must be 3 operands. First, evaluate the first operand. */
			/* If it is non-null, then evaluate and return the second operand. */
			/* Else evaluate and return the third operand. */

			if (!strcmp(op, "if") && actualParamExprs->next->next != NULL && actualParamExprs->next->next->expr != NULL) {
				LISP_EXPR * operand3Expr = actualParamExprs->next->next->expr;
				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

				if (operand1Value->type == lispPseudoValueType_ContinuationReturn) {
					return operand1Value;
				}

				result = evaluate(operand1Value->type != lispValueType_Null ? operand2Expr : operand3Expr, env);
			}
		}
	}

	return result;
}

static LISP_VALUE * evaluateClosureCall(LISP_CLOSURE * closure, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(getEnvInClosure(closure));

	LISP_VAR_LIST_ELEMENT * np = getArgsInClosure(closure); /* closure->args; */
	LISP_EXPR_LIST_ELEMENT * ep = actualParamExprs;

	while (np != NULL || ep != NULL) {

		if (np == NULL || ep == NULL) {
			/* The formal and actual parameter lists have different lengths. */
			fprintf(stderr, "evaluateClosureCall() : The formal and actual parameter lists have different lengths.\n");
			freeEnvironment(newEnv);
			fatalError("evaluateClosureCall() : The formal and actual parameter lists have different lengths.");
			return NULL;
		}

		failIf(np->type != schemeStructType_VariableListElement, "evaluateClosureCall() : np->type != schemeStructType_VariableListElement");

		LISP_VALUE * value = evaluate(ep->expr, env); /* TODO: env or closure->env ? */

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* newEnv->nameValueList = createNameValueListElement(np->var->name, value, newEnv->nameValueList); */
		newEnv->value1 = createNameValueListElement(np->name, value, newEnv->value1);
		/* freeValue(value); */
		np = np->next;
		ep = ep->next;
	}

	LISP_VALUE * result = evaluate(getBodyInClosure(closure), newEnv);

	/* ThAW 2022-08-18 : Don't free newEnv: Someone is still using it. E.g.:
	parseAndEvaluate("(((lambda (x) (lambda (y) x)) 5) 8)"); */

	return result;
}

static LISP_VALUE * evaluateFunctionCall(LISP_FUNCTION_CALL * functionCall, LISP_ENV * env) {
	LISP_VALUE * callableValue = evaluate(functionCall->firstExpr, env);

	if (callableValue->type == lispPseudoValueType_ContinuationReturn) {
		return callableValue;
	}

	LISP_VALUE * continuationReturnValue = NULL;

	switch (callableValue->type) {
		case lispValueType_PrimitiveOperator:
			return evaluatePrimitiveOperatorCall(callableValue->name, functionCall->actualParamExprs, env);

		case lispValueType_Closure:
			return evaluateClosureCall(callableValue->closure, functionCall->actualParamExprs, env);

		case lispPseudoValueType_Continuation:
			/* There must be exactly one actual parameter */

			if (functionCall->actualParamExprs == NULL || functionCall->actualParamExprs->next != NULL) {
				fprintf(stderr, "evaluateFunctionCall() : Bad number of parameters (!= 1) when calling a continuation\n");
				fatalError("evaluateFunctionCall() : Bad number of parameters (!= 1) when calling a continuation");
				return NULL;
			}

			LISP_VALUE * actualParamValue = evaluate(functionCall->actualParamExprs->expr, env);

			if (actualParamValue->type == lispPseudoValueType_ContinuationReturn) {
				return actualParamValue;
			}

			continuationReturnValue = createUndefinedValue();
			continuationReturnValue->type = lispPseudoValueType_ContinuationReturn;
			continuationReturnValue->continuationId = callableValue->continuationId;
			continuationReturnValue->continuationReturnValue = actualParamValue;

			return continuationReturnValue;

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

	LISP_VALUE * value = evaluate(setExpr->expr, env);

	if (value->type == lispPseudoValueType_ContinuationReturn) {
		return value;
	}

	/* X TODO: Use addBubbleDown() instead of setValueInEnvironment() */
	setValueInEnvironment(env, setExpr->var, value);

	return value;
}

static LISP_VALUE * evaluateLetExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(NULL);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList;

	for (varExprPairList = expr->varExprPairList; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		LISP_VALUE * value = evaluate(varExprPairList->expr, env);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* Note: This constructs the list in reverse order... */
		/* TODO: Implement this using recursion instead. */
		/* newEnv->nameValueList = createNameValueListElement(varExprPairList->var->name, value, newEnv->nameValueList); */
		newEnv->value1 = createNameValueListElement(varExprPairList->name, value, newEnv->value1);
	}

	newEnv->next = env;

	LISP_VALUE * result = evaluate(expr->expr, newEnv);

	newEnv->next = NULL;

	return result;
}

static LISP_VALUE * evaluateLetStarExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = expr->varExprPairList;

	while (varExprPairList != NULL) {
		LISP_ENV * newEnv = createEnvironment(env);
		LISP_VALUE * value = evaluate(varExprPairList->expr, env);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* newEnv->nameValueList = createNameValueListElement(varExprPairList->var->name, value, newEnv->nameValueList); */
		newEnv->value1 = createNameValueListElement(varExprPairList->name, value, newEnv->value1);

		env = newEnv;
		varExprPairList = varExprPairList->next;
	}

	return evaluate(expr->expr, env);
}

static LISP_VALUE * evaluateLetrecExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(env);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = expr->varExprPairList;

	for (; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		/* Add all variables that are bound in this.bindings to newEnvFrame before any closures are created in the next loop. */
		/* addToEnvironment(newEnv, varExprPairList->var, globalNullValue); */
		addNameToEnvironment(newEnv, varExprPairList->name, globalNullValue);
	}

	for (varExprPairList = expr->varExprPairList; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		LISP_VALUE * value = evaluate(varExprPairList->expr, newEnv);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}

		/* updateIfFoundInNameValueList(newEnv->nameValueList, varExprPairList->var, value); */
		updateNameIfFoundInNameValueList(newEnv->value1, varExprPairList->name, value);
	}

	return evaluate(expr->expr, newEnv);
}

static LISP_VALUE * evaluateBeginExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = globalNullValue;
	LISP_EXPR_LIST_ELEMENT * exprList;

	for (exprList = expr->exprList; exprList != NULL; exprList = exprList->next) {
		result = evaluate(exprList->expr, env);

		if (result->type == lispPseudoValueType_ContinuationReturn) {
			break;
		}
	}

	if (result == NULL) {
		fprintf(stderr, "evaluateBeginExpression() tried to return NULL\n");
		fatalError("evaluateBeginExpression() tried to return NULL");
	}

	return result;
}

static LISP_VALUE * evaluateWhileExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = NULL;

	for (;;) {
		result = evaluate(expr->expr, env);

		if (result->type == lispValueType_Null || result->type == lispPseudoValueType_ContinuationReturn) {
			break;
		}

		LISP_VALUE * value = evaluate(expr->expr2, env);

		if (value->type == lispPseudoValueType_ContinuationReturn) {
			return value;
		}
	}

	return result;
}

static LISP_VALUE * evaluateCondExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_EXPR_PAIR_LIST_ELEMENT * exprPair;

	for (exprPair = expr->exprPairList; exprPair != NULL; exprPair = exprPair->next) {
		LISP_VALUE * conditionValue = evaluate(exprPair->expr, env);


		if (conditionValue->type == lispPseudoValueType_ContinuationReturn) {
			return conditionValue;
		} else if (conditionValue->type != lispValueType_Null) {
			return evaluate(exprPair->expr2, env);
		}
	}

	return globalNullValue;
}

LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * value = NULL;

	switch (expr->type) {
		case lispExpressionType_Value:
			/* Return a clone of the value so it can be freed separately */
			return cloneValue(expr->value);

		case lispExpressionType_Variable:
			value = lookupVariableInEnvironment(expr->var, env);

			if (value == NULL) {
				fprintf(stderr, "evaluate() : Undefined variable '%s'\n", expr->var->name);
				fatalError("evaluate() : Undefined variable");
				return NULL;
			}

			return cloneValue(value);

		case lispExpressionType_FunctionCall:
			return evaluateFunctionCall(expr->functionCall, env);

		case lispExpressionType_LambdaExpr:
			return evaluateLambdaExpression(expr->lambdaExpr, env);

		case lispExpressionType_SetExpr:
			return evaluateSetExpression(expr, env);

		case lispExpressionType_Let:
			return evaluateLetExpression(expr, env);

		case lispExpressionType_LetStar:
			return evaluateLetStarExpression(expr, env);

		case lispExpressionType_Letrec:
			return evaluateLetrecExpression(expr, env);

		case lispExpressionType_Begin:
			return evaluateBeginExpression(expr, env);

		case lispExpressionType_While:
			return evaluateWhileExpression(expr, env);

		case lispExpressionType_Cond:
			return evaluateCondExpression(expr, env);

		default:
			fatalError("evaluate() : Unrecognized expression type");
			return NULL;
	}
}

/* **** The End **** */
