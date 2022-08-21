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

LISP_VALUE * evaluatePrimitiveOperatorCall(char * op, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	/* TODO: print call/cc */

	LISP_VALUE * result = NULL;
	/* printf("evaluateFunctionCall() : Operator is '%s'\n", op); */

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
		} else if (!strcmp(op, "pair?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Pair);
		} else if (!strcmp(op, "primop?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_PrimitiveOperator);
		} else if (!strcmp(op, "closure?")) {
			return evaluateAndCompareType(operand1Expr, env, lispValueType_Closure);
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
				!strcmp(op, "!=")
			) {
				/* Both operands must be numeric */
				LISP_VALUE * operand1Value = evaluate(operand1Expr, env);
				LISP_VALUE * operand2Value = evaluate(operand2Expr, env);

				if (!strcmp(op, "=")) {
					result = booleanToClonedValue(areValuesEqual(operand1Value, operand2Value));
				} else if (!strcmp(op, "!=")) {
					result = booleanToClonedValue(!areValuesEqual(operand1Value, operand2Value));
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
	/* printf("BEGIN evaluateClosureCall()\n");
	printf("closure->env = %ld\n", (long)closure->env); */

	/* LISP_ENV * newEnv = createEnvironment(env); */
	LISP_ENV * newEnv = createEnvironment(closure->env);
	/* LISP_ENV * newEnv = createEnvironment(NULL); */

	/* printf("Just after createEnvironment: newEnv = %ld\n", (long)newEnv); */

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

		/* printf("evaluateClosureCall() : Associating the variable '%s' with the value: ", np->var->name);
		printValue(value);
		printf("...\n"); */

		newEnv->nameValueList = createNameValueListElement(np->var->name, value, newEnv->nameValueList);
		/* freeValue(value); */
		np = np->next;
		ep = ep->next;
	}

	/* if (closure->env == newEnv) {
		fprintf(stderr, "AAAARGH! evaluateClosureCall: closure->env == newEnv\n");
		printf("closure->env = %ld\n", (long)closure->env);
		printf("newEnv = %ld\n", (long)newEnv);
		return NULL;
	}

	if (env->next == env) {
		fprintf(stderr, "AAAARGH! evaluateClosureCall: env->next == env\n");
		return NULL;
	} */

	/* newEnv->next = closure->env; */

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

	LISP_VALUE * value = evaluate(setExpr->expr, env);

	setValueInEnvironment(env, setExpr->var, value);

	return value;
}

LISP_VALUE * evaluateConsExpression(LISP_EXPR * expr, LISP_ENV * env) {

	if (expr->type != lispExpressionType_Cons) {
		fprintf(stderr, "evaluateConsExpression() : Expression is not a Cons expression\n");
		return NULL;
	}

	LISP_VALUE * head = evaluate(expr->expr, env);
	LISP_VALUE * tail = evaluate(expr->expr2, env);

	return createPair(head, tail);
}

LISP_VALUE * evaluateCarExpression(LISP_EXPR * expr, LISP_ENV * env) {

	if (expr->type != lispExpressionType_Car) {
		fprintf(stderr, "evaluateCarExpression() : Expression is not a Car expression\n");
		return NULL;
	}

	LISP_VALUE * value = evaluate(expr->expr, env);

	if (value->type != lispValueType_Pair || value->pair == NULL) {
		fprintf(stderr, "evaluateCarExpression() : Argument value is not a pair.\n");
		return NULL;
	}

	return cloneValue(value->pair->head);
}

LISP_VALUE * evaluateCdrExpression(LISP_EXPR * expr, LISP_ENV * env) {

	if (expr->type != lispExpressionType_Cdr) {
		fprintf(stderr, "evaluateCdrExpression() : Expression is not a Cdr expression\n");
		return NULL;
	}

	LISP_VALUE * value = evaluate(expr->expr, env);

	if (value->type != lispValueType_Pair || value->pair == NULL) {
		fprintf(stderr, "evaluateCdrExpression() : Argument value is not a pair.\n");
		return NULL;
	}

	return cloneValue(value->pair->tail);
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

/* LISP_VALUE * evaluateLetrecExpression(LISP_EXPR * expr, LISP_ENV * env) {} */

LISP_VALUE * evaluateBeginExpression(LISP_EXPR * expr, LISP_ENV * env) {
	LISP_VALUE * result = NULL;
	LISP_EXPR_LIST_ELEMENT * exprList;

	for (exprList = expr->exprList; exprList != NULL; exprList = exprList->next) {
		result = evaluate(exprList->expr, env);
	}

	return result;
}

/* LISP_VALUE * evaluatePrintExpression(LISP_EXPR * expr, LISP_ENV * env) {} */

/* LISP_VALUE * evaluateCallCCExpression(LISP_EXPR * expr, LISP_ENV * env) {} */

LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env) {

	switch (expr->type) {
		case lispExpressionType_Value:
			/* Return a clone of the value so it can be freed separately */
			return cloneValue(expr->value);

		case lispExpressionType_Variable:
			/* printf("evaluate() : Evaluating the variable named '%s'\n", expr->var->name); */
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

		/* case lispExpressionType_Letrec:
			return evaluateLetrecExpression(expr, env); */

		case lispExpressionType_Cons:
			return evaluateConsExpression(expr, env);

		case lispExpressionType_Car:
			return evaluateCarExpression(expr, env);

		case lispExpressionType_Cdr:
			return evaluateCdrExpression(expr, env);

		case lispExpressionType_Begin:
			return evaluateBeginExpression(expr, env);

		/* case lispExpressionType_Print:
			return evaluatePrintExpression(expr, env); */

		/* case lispExpressionType_CallCC:
			return evaluateCallCCExpression(expr, env); */

		default:
			return NULL;
	}
}

/* **** The End **** */
