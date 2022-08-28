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

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Function prototypes */

/* Forward references */

/* Functions */

LISP_VALUE * booleanToClonedValue(int b) {
	return cloneValue(b ? globalTrueValue : globalNullValue);
}

LISP_VALUE * evaluateAndCompareType(LISP_EXPR * operandExpr, LISP_ENV * env, int lispValueType) {
	LISP_VALUE * operandValue = evaluate(operandExpr, env);
	const int b = operandValue->type == lispValueType;

	freeValue(operandValue);

	return booleanToClonedValue(b);
}

BOOL areValuesEqual(LISP_VALUE * v1, LISP_VALUE * v2) {

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
		case lispValueType_Closure:
		default:
			break;
	}

	return FALSE;
}

static LISP_VALUE * exprListToListValue(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env) {

	if (exprList == NULL) {
		return createNull();
	}

	LISP_VALUE * head = evaluate(exprList->expr, env);
	LISP_VALUE * tail = exprListToListValue(exprList->next, env);

	return createPair(head, tail);
}

LISP_VALUE * evaluatePrimitiveOperatorCall(char * op, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	/* TODO: call/cc */

	LISP_VALUE * result = NULL;
	/* printf("evaluateFunctionCall() : Operator is '%s'\n", op); */

	if (!strcmp(op, "list")) {
		/* 'list' can take any number of args, including zero. */
		return exprListToListValue(actualParamExprs, env);
	}

	if (actualParamExprs != NULL && actualParamExprs->expr != NULL) {
		LISP_EXPR * operand1Expr = actualParamExprs->expr;

		/* printf("evaluateFunctionCall() : Operand 1 is: ");
		printValue(operand1Value); */
		if (!strcmp(op, "null?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Null);
		} else if (!strcmp(op, "number?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Number);
		} else if (!strcmp(op, "string?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_String);
		} else if (!strcmp(op, "symbol?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Symbol);
		} else if (!strcmp(op, "pair?") || !strcmp(op, "list?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Pair);
		} else if (!strcmp(op, "primop?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_PrimitiveOperator);
		} else if (!strcmp(op, "closure?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Closure);
		} else if (!strcmp(op, "print")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			printValue(operand1Value);
			printf("\n");

			/* Return without freeing the values */

			return operand1Value;
		} else if (!strcmp(op, "random")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type != lispValueType_Number || operand1Value->value <= 0) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : random : Bad parameter\n");
				fatalError("evaluatePrimitiveOperatorCall() : random : Bad parameter");
			}

			return createNumericValue(rand() % operand1Value->value);
		} else if (!strcmp(op, "car")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : car : Operand is not a pair\n");
				fatalError("evaluatePrimitiveOperatorCall() : car : Operand is not a pair");
			}

			return operand1Value->pair->head;
		} else if (!strcmp(op, "cdr")) {
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type != lispValueType_Pair) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair\n");
				fatalError("evaluatePrimitiveOperatorCall() : cdr : Operand is not a pair");
			}

			return operand1Value->pair->tail;
		} else if (!strcmp(op, "throw")) {
			fprintf(stderr, "An exception has been thrown.\n");

			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type == lispValueType_String || operand1Value->type == lispValueType_Symbol) {
				fprintf(stderr, "    Message: '%s'\n", operand1Value->name);
			}

			fatalError("An exception has been thrown.");
		} else if (!strcmp(op, "call/cc")) {
			/* Call with current continuation */
			/* The arg must be a lambda expr that takes exactly one arg. */
			LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

			if (operand1Value->type != lispValueType_Closure) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure\n");
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Operand is not a closure");
			} else if (operand1Value->closure->args == NULL || operand1Value->closure->args->next != NULL) {
				fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Closure does not take exactly one argument\n");
				fatalError("evaluatePrimitiveOperatorCall() : call/cc : Closure does not take exactly one argument");
			}

			fprintf(stderr, "evaluatePrimitiveOperatorCall() : call/cc : Implementation not complete\n");
			fatalError("evaluatePrimitiveOperatorCall() : call/cc : Implementation not complete");
		}

		if (actualParamExprs->next != NULL && actualParamExprs->next->expr != NULL) {
			LISP_EXPR * operand2Expr = actualParamExprs->next->expr;

			/* printf("evaluateFunctionCall() : Operand 2 is: ");
			printValue(operand2Value); */

			if (
				!strcmp(op, "+") ||
				!strcmp(op, "-") ||
				!strcmp(op, "*") ||
				!strcmp(op, "/") ||
				!strcmp(op, "%") ||
				!strcmp(op, "<") ||
				!strcmp(op, ">") ||
				!strcmp(op, "<=") ||
				!strcmp(op, ">=") ||
				!strcmp(op, "=") ||
				!strcmp(op, "!=") ||
				!strcmp(op, "cons")
			) {
				/* Both operands must be numeric */
				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);
				LISP_VALUE * operand2Value = evaluate(operand2Expr, env);

				if (!strcmp(op, "=")) {
					/* printf("= : operand1Value is ");
					printValue(operand1Value);
					printf("\n= : operand2Value is ");
					printValue(operand2Value);
					printf("\n"); */
					result = booleanToClonedValue(areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "!=")) {
					result = booleanToClonedValue(!areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "cons")) {
					/* Return without freeing the values */
					return createPair(operand1Value, operand2Value);
				} else if (operand1Value->type == lispValueType_Number && operand2Value->type == lispValueType_Number) {
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

				freeValue(operand2Value);
				freeValue(operand1Value);
			}

			/* Handle if */
			/* There must be 3 operands. First, evaluate the first operand. */
			/* If it is non-null, then evaluate and return the second operand. */
			/* Else evaluate and return the third operand. */

			if (!strcmp(op, "if") && actualParamExprs->next->next != NULL && actualParamExprs->next->next->expr != NULL) {
				LISP_EXPR * operand3Expr = actualParamExprs->next->next->expr;
				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);

				result = evaluate(operand1Value->type != lispValueType_Null ? operand2Expr : operand3Expr, env);
			}
		}
	}

	return result;
}

LISP_VALUE * evaluateClosureCall(LISP_CLOSURE * closure, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(closure->env);

	LISP_VAR_LIST_ELEMENT * np = closure->args;
	LISP_EXPR_LIST_ELEMENT * ep = actualParamExprs;

	while (np != NULL || ep != NULL) {

		if (np == NULL || ep == NULL) {
			/* The formal and actual parameter lists have different lengths. */
			fprintf(stderr, "evaluateClosureCall() : The formal and actual parameter lists have different lengths.\n");
			freeEnvironment(newEnv);
			return NULL;
		}

		LISP_VALUE * value = evaluate(ep->expr, env);

		newEnv->nameValueList = createNameValueListElement(np->var->name, value, newEnv->nameValueList);
		/* freeValue(value); */
		np = np->next;
		ep = ep->next;
	}

	LISP_VALUE * result = evaluate(closure->body, newEnv);

	newEnv->next = NULL;
	/* TODO? : Free the values, but not the names, in this environment?

	ThAW 2022-08-18 : Don't free newEnv: Someone is still using it. E.g.:
	parseAndEvaluate("(((lambda (x) (lambda (y) x)) 5) 8)");

	freeEnvironment(newEnv); */

	return result;
}

LISP_VALUE * evaluateFunctionCall(LISP_FUNCTION_CALL * functionCall, LISP_ENV * env) {
	LISP_VALUE * callableValue = evaluate(functionCall->firstExpr, env);

	if (callableValue->type == lispValueType_PrimitiveOperator) {
		return evaluatePrimitiveOperatorCall(callableValue->name, functionCall->actualParamExprs, env);
	} else if (callableValue->type == lispValueType_Closure) {
		return evaluateClosureCall(callableValue->closure, functionCall->actualParamExprs, env);
	} else {
		fprintf(stderr, "evaluateFunctionCall() : Attempted to call an uncallable value\n");
		return NULL;
	}
}

LISP_VALUE * evaluateLambdaExpression(LISP_LAMBDA_EXPR * lambdaExpr, LISP_ENV * env) {
	return createClosure(lambdaExpr->args, lambdaExpr->body, env);
}

LISP_VALUE * evaluateSetExpression(LISP_EXPR * setExpr, LISP_ENV * env) {

	if (setExpr->type != lispExpressionType_SetExpr) {
		fprintf(stderr, "evaluateSetExpression() : Expression is not a Set expression\n");
		return NULL;
	}

	/* printf("evaluate()...\n");
	printf("env is %lu\n", env);
	printf("setExpr is %lu\n", setExpr);
	printf("setExpr->expr is %lu\n", setExpr->expr); */

	LISP_VALUE * value = evaluate(setExpr->expr, env);

	/* printf("setValueInEnvironment()...\n"); */

	setValueInEnvironment(env, setExpr->var, value);

	/* printf("Set var '%s' to value ", setExpr->var->name);
	printValue(value);
	printf("\n"); */

	return value;
}

LISP_VALUE * evaluateLetExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(NULL);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList;

	for (varExprPairList = expr->varExprPairList; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		LISP_VALUE * value = evaluate(varExprPairList->expr, env);

		/* Note: This constructs the list in reverse order... */
		/* TODO: Implement this using recursion instead. */
		newEnv->nameValueList = createNameValueListElement(varExprPairList->var->name, value, newEnv->nameValueList);
	}

	newEnv->next = env;

	LISP_VALUE * result = evaluate(expr->expr, newEnv);

	newEnv->next = NULL;

	return result;
}

LISP_VALUE * evaluateLetStarExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = expr->varExprPairList;

	while (varExprPairList != NULL) {
		LISP_ENV * newEnv = createEnvironment(env);
		LISP_VALUE * value = evaluate(varExprPairList->expr, env);

		newEnv->nameValueList = createNameValueListElement(varExprPairList->var->name, value, newEnv->nameValueList);

		env = newEnv;
		varExprPairList = varExprPairList->next;
	}

	LISP_VALUE * result = evaluate(expr->expr, env);

	return result;
}

LISP_VALUE * evaluateLetrecExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_ENV * newEnv = createEnvironment(env);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = expr->varExprPairList;

	for (; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		/* Add all variables that are bound in this.bindings to newEnvFrame before any closures are created in the next loop. */
		/* newEnv->nameValueList = createNameValueListElement(varExprPairList->var->name, globalNullValue, newEnv->nameValueList); */
		/* TODO: Use addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) */
		addToEnvironment(newEnv, varExprPairList->var, globalNullValue);
	}

	for (varExprPairList = expr->varExprPairList; varExprPairList != NULL; varExprPairList = varExprPairList->next) {
		/* newEnvFrame.add(varExprPairList->var->name, evaluate(varExprPairList->expr, newEnv)); */
		/* TODO: Use updateIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, LISP_VAR * var, LISP_VALUE * value) */
		updateIfFoundInNameValueList(newEnv->nameValueList, varExprPairList->var, evaluate(varExprPairList->expr, newEnv));
	}

	return evaluate(expr->expr, newEnv);
}

LISP_VALUE * evaluateBeginExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = NULL;
	LISP_EXPR_LIST_ELEMENT * exprList;

	for (exprList = expr->exprList; exprList != NULL; exprList = exprList->next) {
		result = evaluate(exprList->expr, env);
	}

	return result;
}

LISP_VALUE * evaluateWhileExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = NULL;

	for (;;) {
		result = evaluate(expr->expr, env);

		if (result->type == lispValueType_Null) {
			break;
		}

		evaluate(expr->expr2, env);
	}

	return result;
}

LISP_VALUE * evaluateCondExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_EXPR_PAIR_LIST_ELEMENT * exprPair;

	for (exprPair = expr->exprPairList; exprPair != NULL; exprPair = exprPair->next) {
		LISP_VALUE * conditionValue = evaluate(exprPair->expr, env);

		if (conditionValue->type != lispValueType_Null) {
			return evaluate(exprPair->expr2, env);
		}
	}

	return globalNullValue;
}

/* LISP_VALUE * evaluateCallCCExpression(LISP_EXPR * expr, LISP_ENV * env) {} */

LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env) {

	switch (expr->type) {
		case lispExpressionType_Value:
			/* Return a clone of the value so it can be freed separately */
			return cloneValue(expr->value);

		case lispExpressionType_Variable:
			return cloneValue(lookupVariableInEnvironment(expr->var, env));

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

		/* case lispExpressionType_CallCC:
			return evaluateCallCCExpression(expr, env); */

		default:
			return NULL;
	}
}

/* **** The End **** */
