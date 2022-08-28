/* atrocity/src/create-and-destroy.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"

/* **** Value struct creation functions **** */

static LISP_VALUE * createUndefinedValue() {
	LISP_VALUE * result = (LISP_VALUE *)malloc(sizeof(LISP_VALUE));

	if (result == NULL) {
		fatalError("malloc() failed in createUndefinedValue()");
	}

	result->type = lispValueType_Undefined;
	result->value = 0;
	memset(result->name, 0, maxStringValueLength * sizeof(char));
	result->pair = NULL;
	result->closure = NULL;

	/* registerValueWithMemoryManager(result); */

	return result;
}

LISP_VALUE * createNumericValue(int value) {
	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Number;
	result->value = value;

	return result;
}

LISP_VALUE * createStringValue(char * str) {
	int len = strlen(str);

	if (len > 0 && str[0] == '"') {
		++str;
		--len;
	}

	if (len > 0 && str[len - 1] == '"') {
		--len;
	}

	if (len >= maxStringValueLength) {
		fprintf(stderr, "The string '%s' is too long to be a string value.", str);
		fatalError("createStringValue() : String too long");
	}

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_String;
	memcpy(result->name, str, len * sizeof(char));

	return result;
}

LISP_VALUE * createSymbolValue(char * value) {

	if (strlen(value) >= maxStringValueLength) {
		fprintf(stderr, "The string '%s' is too long to be a string value.", value);
		fatalError("createSymbolValue() : String too long");
	}

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Symbol;
	strcpy(result->name, value);

	return result;
}

LISP_VALUE * createPrimitiveOperator(char * value) {
	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_PrimitiveOperator;
	strcpy(result->name, value);

	return result;
}

LISP_VALUE * createClosure(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body, LISP_ENV * env) {
	LISP_CLOSURE * closure = (LISP_CLOSURE *)malloc(sizeof(LISP_CLOSURE));

	if (closure == NULL) {
		fatalError("malloc() failed in createClosure()");
	}

	closure->args = args;
	closure->body = body;
	closure->env = env;

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Closure;
	result->closure = closure;

	return result;
}

static void freeClosure(LISP_CLOSURE * closure) {

	/* if (closure->args != NULL) {
		freeVariableList(closure->args);
		closure->args = NULL;
	}

	if (closure->body != NULL) {
		freeExpression(closure->body);
		closure->body = NULL;
	}

	if (closure->env != NULL) {
		freeEnvironment(closure->env);
		closure->env = NULL;
	} */

	free(closure);
}

LISP_VALUE * createPair(LISP_VALUE * head, LISP_VALUE * tail) {
	LISP_PAIR * pair = (LISP_PAIR *)malloc(sizeof(LISP_PAIR));

	if (pair == NULL) {
		fatalError("malloc() failed in createPair()");
	}

	pair->head = head;
	pair->tail = tail;

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Pair;
	result->pair = pair;

	return result;
}

static void freePair(LISP_PAIR * pair) {

	if (pair->head != NULL) {
		/* freeValue(pair->head); */
		pair->head = NULL;
	}

	if (pair->tail != NULL) {
		/* freeValue(pair->tail); */
		pair->tail = NULL;
	}

	free(pair);
}

LISP_VALUE * createNull() {
	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Null;

	return result;
}

/* void freeNull(LISP_VALUE * value) {
	free(value);
} */

LISP_VALUE * cloneValue(LISP_VALUE * value) {

	switch (value->type) {
		case lispValueType_Number:
			return createNumericValue(value->value);

		case lispValueType_String:
			return createStringValue(value->name);

		case lispValueType_Symbol:
			return createSymbolValue(value->name);

		case lispValueType_PrimitiveOperator:
			return createPrimitiveOperator(value->name);

		case lispValueType_Closure:
			return createClosure(value->closure->args, value->closure->body, value->closure->env);

		case lispValueType_Pair:
			return createPair(value->pair->head, value->pair->tail);

		case lispValueType_Null:
			return createNull();

		default:
			fprintf(stderr, "Failed to clone value of type %d\n", value->type);
			fatalError("cloneValue() : Bad value type");
	}

	return NULL;
}

void freeValue(LISP_VALUE * value) {

	if (value->pair != NULL) {
		freePair(value->pair);
		value->pair = NULL;
	}

	if (value->closure != NULL) {
		freeClosure(value->closure);
		value->closure = NULL;
	}

	free(value);
}

// **** Expression struct creation functions ****

LISP_EXPR * createUndefinedExpression() {
	LISP_EXPR * result = (LISP_EXPR *)malloc(sizeof(LISP_EXPR));

	if (result == NULL) {
		fatalError("malloc() failed in createUndefinedExpression()");
	}

	result->type = lispExpressionType_Undefined;
	result->value = NULL;
	result->var = NULL;
	result->exprList = NULL;
	result->lambdaExpr = NULL;
	result->functionCall = NULL;
	result->expr = NULL;
	result->expr2 = NULL;
	result->varExprPairList = NULL;
	result->exprPairList = NULL;

	return result;
}

LISP_VAR_LIST_ELEMENT * createVariableListElement(LISP_VAR * var, LISP_VAR_LIST_ELEMENT * next) {
	LISP_VAR_LIST_ELEMENT * result = (LISP_VAR_LIST_ELEMENT *)malloc(sizeof(LISP_VAR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("malloc() failed in createVariableListElement()");
	}

	result->var = var;
	result->next = next;

	return result;
}

void freeVariableList(LISP_VAR_LIST_ELEMENT * varList) {

	if (varList != NULL) {

		if (varList->var != NULL) {
			/* printf("freeVariableList() : Freeing a variable named '%s'\n", varList->var->name); */
			freeVariable(varList->var);
			varList->var = NULL;
		}

		if (varList->next != NULL) {
			freeVariableList(varList->next);
			varList->next = NULL;
		}

		free(varList);
	}
}

LISP_EXPR_PAIR_LIST_ELEMENT * createExpressionPairListElement(LISP_EXPR * expr, LISP_EXPR * expr2, LISP_EXPR_PAIR_LIST_ELEMENT * next) {
	LISP_EXPR_PAIR_LIST_ELEMENT * result = (LISP_EXPR_PAIR_LIST_ELEMENT *)malloc(sizeof(LISP_EXPR_PAIR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("malloc() failed in createExpressionPairListElement()");
	}

	result->expr = expr;
	result->expr2 = expr2;
	result->next = next;

	return result;
}

void freeExpressionPairList(LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList) {

	if (exprPairList != NULL) {

		if (exprPairList->expr != NULL) {
			freeExpression(exprPairList->expr);
			exprPairList->expr = NULL;
		}

		if (exprPairList->expr2 != NULL) {
			freeExpression(exprPairList->expr2);
			exprPairList->expr2 = NULL;
		}

		if (exprPairList->next != NULL) {
			freeExpressionPairList(exprPairList->next);
			exprPairList->next = NULL;
		}

		free(exprPairList);
	}
}

LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body) {
	LISP_LAMBDA_EXPR * lambdaExpr = (LISP_LAMBDA_EXPR *)malloc(sizeof(LISP_LAMBDA_EXPR));

	if (lambdaExpr == NULL) {
		fatalError("malloc() failed in createLambdaExpression()");
	}

	lambdaExpr->args = args;
	lambdaExpr->body = body;

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_LambdaExpr;
	result->lambdaExpr = lambdaExpr;

	return result;
}

void freeLambdaExpression(LISP_LAMBDA_EXPR * lambdaExpr) {
	/* printf("Freeing LambdaExpression...\n"); */

	if (lambdaExpr->args != NULL) {
		freeVariableList(lambdaExpr->args);
		lambdaExpr->args = NULL;
	}

	if (lambdaExpr->body != NULL) {
		freeExpression(lambdaExpr->body);
		lambdaExpr->body = NULL;
	}

	free(lambdaExpr);
}

LISP_EXPR * createSetExpression(LISP_VAR * var, LISP_EXPR * expr) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_SetExpr;
	result->var = var;
	result->expr = expr;

	return result;
}

/* void freeSetExpression(LISP_EXPR * setExpr) {
} */

void freeFunctionCall(LISP_FUNCTION_CALL * functionCall) {
	freeExpression(functionCall->firstExpr);
	functionCall->firstExpr = NULL;
	freeExpressionList(functionCall->actualParamExprs);
	functionCall->actualParamExprs = NULL;
	free(functionCall);
}

void freeExpression(LISP_EXPR * expr) {

	if (expr->value != NULL) {
		freeValue(expr->value);
		expr->value = NULL;
	}

	/* if (expr->var != NULL) {
		freeVariable(expr->var);
		expr->var = NULL;
	} */

	/* if (expr->exprList != NULL) {
		freeExpressionList(expr->exprList);
		expr->exprList = NULL;
	} */

	if (expr->lambdaExpr != NULL) {
		freeLambdaExpression(expr->lambdaExpr);
		expr->lambdaExpr = NULL;
	}

	if (expr->functionCall != NULL) {
		freeFunctionCall(expr->functionCall);
		expr->functionCall = NULL;
	}

	/* if (expr->expr != NULL) {
		freeExpression(expr->expr);
		expr->expr = NULL;
	}

	if (expr->expr2 != NULL) {
		freeExpression(expr->expr2);
		expr->expr2 = NULL;
	} */

	/* if (expr->varExprPairList != NULL) {
		freeVarExprPairList(expr->varExprPairList);
		expr->varExprPairList = NULL;
	} */

	/* if (expr->exprPairList != NULL) {
		freeExprPairList(expr->exprPairList);
		expr->exprPairList = NULL;
	} */

	free(expr);
}

// **** Expression list struct creation functions ****

LISP_EXPR_LIST_ELEMENT * createExpressionListElement(LISP_EXPR * expr, LISP_EXPR_LIST_ELEMENT * next) {
	LISP_EXPR_LIST_ELEMENT * result = (LISP_EXPR_LIST_ELEMENT *)malloc(sizeof(LISP_EXPR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("malloc() failed in createExpressionListElement()");
	}

	result->expr = expr;
	result->next = next;

	return result;
}

void freeExpressionList(LISP_EXPR_LIST_ELEMENT * exprList) {

	while (exprList != NULL) {
		freeExpression(exprList->expr);
		exprList->expr = NULL; /* So we don't try to free it again */
		exprList = exprList->next;
	}
}

/* A variable is an Expression but not a Value. */

LISP_VAR * createVariable(char * name) {

	if (strlen(name) >= maxStringValueLength - 1) {
		fprintf(stderr, "createVariable() : The name '%s' is too long.\n", name);
		fatalError("createVariable() : String too long");
	}
	/* Ensure that name does not contain ( or ) */
	else if (strchr(name, '(') != NULL || strchr(name, ')')) {
		fprintf(stderr, "createVariable() : The name '%s' contains an illegal character: '(' or ')'.\n", name);
		fatalError("createVariable() : String contains an illegal character");
	}

	LISP_VAR * var = (LISP_VAR *)malloc(sizeof(LISP_VAR));

	if (var == NULL) {
		fatalError("malloc() failed in createVariable()");
	}

	memset(var->name, 0, maxStringValueLength);
	strcpy(var->name, name);

	return var;
}

LISP_EXPR * createExpressionFromVariable(LISP_VAR * var) {
	LISP_EXPR * expr = createUndefinedExpression();

	expr->type = lispExpressionType_Variable;
	expr->var = var;

	return expr;
}

void freeVariable(LISP_VAR * var) {
	free(var);
}

LISP_NAME_VALUE_LIST_ELEMENT * createNameValueListElement(char * name, LISP_VALUE * value, LISP_NAME_VALUE_LIST_ELEMENT * next) {
	LISP_NAME_VALUE_LIST_ELEMENT * nvle = (LISP_NAME_VALUE_LIST_ELEMENT * )malloc(sizeof(LISP_NAME_VALUE_LIST_ELEMENT));

	if (nvle == NULL) {
		fatalError("malloc() failed in createNameValueListElement()");
	}

	nvle->name = name;
	nvle->value = value; /* TODO? : Clone the value? */
	nvle->next = next;

	return nvle;
}

void freeNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle) {

	if (nvle != NULL) {
		/* Do not free name. */
		nvle->name = NULL;
		/* Do not free value. */
		nvle->value = NULL;
		freeNameValueList(nvle->next);
		nvle->next = NULL;
		free(nvle);
	}
}

LISP_ENV * createEnvironment(LISP_ENV * next) {
	LISP_ENV * env = (LISP_ENV *)malloc(sizeof(LISP_ENV));

	if (env == NULL) {
		fatalError("malloc() failed in createEnvironment()");
	}

	env->nameValueList = NULL;
	env->next = next;

	return env;
}

void freeEnvironment(LISP_ENV * env) {

	if (env != NULL) {

		if (env->nameValueList != NULL) {
			freeNameValueList(env->nameValueList);
			env->nameValueList = NULL;
		}

		/* If we free the entire list of environment frames,
		 * then freeing any frame will free the global environment.
		 */

		/* if (env->next != NULL) {
			freeEnvironment(env->next); */
		env->next = NULL;
		/* } */

		free(env);
	}
}

LISP_EXPR * createExpressionFromValue(LISP_VALUE * value) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Value;
	result->value = value;

	return result;
}

/* Misc DOM functions */

/* BOOL isValueCallable(LISP_VALUE * value) {
	return value->type == lispValueType_PrimitiveOperator || value->type == lispValueType_Closure;
} */

BOOL isList(LISP_VALUE * value) {

	switch (value->type) {
		case lispValueType_Null:
			return TRUE;

		case lispValueType_Pair:
			return isList(value->pair->tail);

		default:
			break;
	}

	return FALSE;
}

void printValue(LISP_VALUE * value) {

	if (isList(value) && value->type != lispValueType_Null) {
		char separator = '\0';

		printf("List: (");

		while (value->type != lispValueType_Null) {
			printf("%c", separator);
			printValue(value->pair->head);
			separator = ' ';
			value = value->pair->tail;
		}

		printf(")");

		return;
	}

	switch (value->type) {
		case lispValueType_Number:
			printf("%d", value->value);
			break;

		case lispValueType_String:
			printf("String: '%s'", value->name);
			break;

		case lispValueType_Symbol:
			printf("Symbol: '%s'", value->name);
			break;

		case lispValueType_PrimitiveOperator:
			printf("PrimitiveOperator: '%s'", value->name);
			break;

		case lispValueType_Closure:
			printf("<closure>");
			break;

		case lispValueType_Pair:
			printf("Pair: (");
			printValue(value->pair->head);
			printf(" . ");
			printValue(value->pair->tail);
			printf(")");
			break;

		case lispValueType_Null:
			printf("Null: ()");
			break;

		default:
			printf("<invalid value>");
			break;
	}
}

/* **** The End **** */
