/* atrocity/src/create-and-destroy.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "memory-manager.h"
#include "utilities.h"

/* **** Value struct creation functions **** */

int getNumCharsAllocatedToNameBufInValue(LISP_VALUE * value) {
	/* **** Deprecated **** */

	/* Return the number of chars, not necessarily the number of bytes. */

	/* In the future, we might do something like:
	value->numCharsAllocatedToNameBuf = ...;
	value->name = (char *)mmAlloc(value->numCharsAllocatedToNameBuf * sizeof(char));
	memset(value->name, 0, value->numCharsAllocatedToNameBuf * sizeof(char));
	*/
	/* return value->numCharsAllocatedToNameBuf; */

	return maxStringValueLength;
}

LISP_VALUE * createUndefinedValue() {
	/* printf("createUndefinedValue() : Begin\n"); */

	LISP_VALUE * result = (LISP_VALUE *)mmAlloc(sizeof(LISP_VALUE));

	/* if (result == NULL) {
		fatalError("mmAlloc() failed in createUndefinedValue()");
	} */

	result->type = lispValueType_Undefined;
	result->value = 0;
	memset(result->name, 0, maxStringValueLength * sizeof(char));
	result->pair = NULL;
	result->closure = NULL;
	result->continuationId = 0;
	result->continuationReturnValue = NULL;

	/* registerValueWithMemoryManager(result); */

	/* printf("createUndefinedValue() : End\n"); */

	return result;
}

LISP_VALUE * createNumericValue(int value) {
	/* printf("createNumericValue() : Begin\n"); */

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Number;
	result->value = value;

	/* printf("createNumericValue() : End\n"); */

	return result;
}

LISP_VALUE * createStringValue(char * str) {
	/* printf("createStringValue() : Begin\n"); */

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
	/* printf("Created string: <%s>\n", result->name); */

	/* printf("createStringValue() : End\n"); */

	return result;
}

LISP_VALUE * createSymbolValue(char * value) {
	/* printf("createSymbolValue() : Begin\n"); */

	if (strlen(value) >= maxStringValueLength) {
		fprintf(stderr, "The string '%s' is too long to be a string value.", value);
		fatalError("createSymbolValue() : String too long");
	}

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Symbol;
	strcpy(result->name, value);

	/* printf("createSymbolValue() : End\n"); */

	return result;
}

LISP_VALUE * createPrimitiveOperator(char * value) {
	/* printf("createPrimitiveOperator() : Begin\n"); */

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_PrimitiveOperator;
	strcpy(result->name, value);

	/* printf("createPrimitiveOperator() : End\n"); */

	return result;
}

LISP_VALUE * createClosure(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body, LISP_ENV * env) {
	/* printf("createClosure() : Begin\n"); */

	LISP_CLOSURE * closure = (LISP_CLOSURE *)mmAlloc(sizeof(LISP_CLOSURE));

	/* if (closure == NULL) {
		fatalError("mmAlloc() failed in createClosure()");
	} */

	closure->args = args;
	closure->body = body;
	closure->env = env;

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Closure;
	result->closure = closure;

	/* printf("createClosure() : End\n"); */

	return result;
}

/* static void freeClosure(LISP_CLOSURE * closure) {

	if (closure->args != NULL) {
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
	}

	mmFree(closure);
} */

/* TODO:
A thunk is a suspended computation; used to implement lazy evaluation in SASL.
A thunk is implemented as a closure thqt takes no arguments.

LISP_VALUE * createThunk(LISP_EXPR * body, LISP_ENV * env) {
	return createClosure(NULL, body, env);
}

void freeThunk(LISP_VALUE * value) {
	freeClosure(value);
}*/

LISP_VALUE * createPair(LISP_VALUE * head, LISP_VALUE * tail) {
	/* printf("createPair() : Begin\n"); */

	LISP_PAIR * pair = (LISP_PAIR *)mmAlloc(sizeof(LISP_PAIR));

	/* if (pair == NULL) {
		fatalError("mmAlloc() failed in createPair()");
	} */

	pair->head = head;
	pair->tail = tail;

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Pair;
	result->pair = pair;

	/* printf("createPair() : End\n"); */

	return result;
}

/* static void freePair(LISP_PAIR * pair) {

	if (pair->head != NULL) {
		/ * freeValue(pair->head); * /
		pair->head = NULL;
	}

	if (pair->tail != NULL) {
		/ * freeValue(pair->tail); * /
		pair->tail = NULL;
	}

	mmFree(pair);
} */

LISP_VALUE * createNull() {
	/* printf("createNull() : Begin\n"); */

	LISP_VALUE * result = createUndefinedValue();

	result->type = lispValueType_Null;

	/* printf("createNull() : End\n"); */

	return result;
}

/* void freeNull(LISP_VALUE * value) {
	mmFree(value);
} */

LISP_VALUE * cloneValue(LISP_VALUE * value) {
	/* printf("cloneValue() : Begin\n"); */

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

		case lispPseudoValueType_Continuation:
		case lispPseudoValueType_ContinuationReturn:
			/* fprintf(stderr, "Warning: cloneValue() returning the original value for Continuation or ContinuationReturn\n"); */
			return value;

		default:
			fprintf(stderr, "Failed to clone value of type %d\n", value->type);
			fatalError("cloneValue() : Bad value type");
	}

	return NULL;
}

void freeValue(LISP_VALUE * value) {
	/* printf("freeValue() : Begin\n"); */

	/* if (value->pair != NULL) {
		freePair(value->pair);
		value->pair = NULL;
	}

	if (value->closure != NULL) {
		freeClosure(value->closure);
		value->closure = NULL;
	}

	mmFree(value); */
}

// **** Expression struct creation functions ****

LISP_EXPR * createUndefinedExpression() {
	/* printf("createUndefinedExpression() : Begin\n"); */

	LISP_EXPR * result = (LISP_EXPR *)mmAlloc(sizeof(LISP_EXPR));

	/* if (result == NULL) {
		fatalError("mmAlloc() failed in createUndefinedExpression()");
	} */

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

	/* printf("createUndefinedExpression() : End\n"); */

	return result;
}

LISP_VAR_LIST_ELEMENT * createVariableListElement(LISP_VAR * var, LISP_VAR_LIST_ELEMENT * next) {
	/* printf("createVariableListElement() : Begin\n"); */

	failIf(var == NULL, "createVariableListElement() : var == NULL");
	failIf(var->name == NULL, "createVariableListElement() : var->name == NULL");

	/* LISP_VAR_LIST_ELEMENT * result = (LISP_VAR_LIST_ELEMENT *)mmAlloc(sizeof(LISP_VAR_LIST_ELEMENT)); */

	SCHEME_UNIVERSAL_TYPE * result = allocateStringAndCreateUniversalStruct(
		schemeStructType_VariableListElement,
		0,
		0,
		var->name,
		NULL,
		NULL,
		next
	);

	/* if (result == NULL) {
		fatalError("mmAlloc() failed in createVariableListElement()");
	} */

	/* result->var = var; / * Replace result->var->name with (universal) result->name * /
	result->next = next; */

	/* printf("createVariableListElement() : End\n"); */

	return result;
}

void freeVariableList(LISP_VAR_LIST_ELEMENT * varList) {

	/* if (varList != NULL) {

		if (varList->var != NULL) {
			/ * printf("freeVariableList() : Freeing a variable named '%s'\n", varList->var->name); * /
			freeVariable(varList->var);
			varList->var = NULL;
		}

		if (varList->next != NULL) {
			freeVariableList(varList->next);
			varList->next = NULL;
		}

		mmFree(varList);
	} */
}

LISP_EXPR_PAIR_LIST_ELEMENT * createExpressionPairListElement(LISP_EXPR * expr, LISP_EXPR * expr2, LISP_EXPR_PAIR_LIST_ELEMENT * next) {
	LISP_EXPR_PAIR_LIST_ELEMENT * result = (LISP_EXPR_PAIR_LIST_ELEMENT *)mmAlloc(sizeof(LISP_EXPR_PAIR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("mmAlloc() failed in createExpressionPairListElement()");
	}

	result->expr = expr;
	result->expr2 = expr2;
	result->next = next;

	return result;
}

void freeExpressionPairList(LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList) {

	/* if (exprPairList != NULL) {

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

		mmFree(exprPairList);
	} */
}

LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body) {
	LISP_LAMBDA_EXPR * lambdaExpr = (LISP_LAMBDA_EXPR *)mmAlloc(sizeof(LISP_LAMBDA_EXPR));

	if (lambdaExpr == NULL) {
		fatalError("mmAlloc() failed in createLambdaExpression()");
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

	/* if (lambdaExpr->args != NULL) {
		freeVariableList(lambdaExpr->args);
		lambdaExpr->args = NULL;
	}

	if (lambdaExpr->body != NULL) {
		freeExpression(lambdaExpr->body);
		lambdaExpr->body = NULL;
	}

	mmFree(lambdaExpr); */
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
	/* freeExpression(functionCall->firstExpr);
	functionCall->firstExpr = NULL;
	freeExpressionList(functionCall->actualParamExprs);
	functionCall->actualParamExprs = NULL;
	mmFree(functionCall); */
}

void freeExpression(LISP_EXPR * expr) {

	/* if (expr->value != NULL) {
		freeValue(expr->value);
		expr->value = NULL;
	}

	/ * if (expr->var != NULL) {
		freeVariable(expr->var);
		expr->var = NULL;
	} * /

	/ * if (expr->exprList != NULL) {
		freeExpressionList(expr->exprList);
		expr->exprList = NULL;
	} * /

	if (expr->lambdaExpr != NULL) {
		freeLambdaExpression(expr->lambdaExpr);
		expr->lambdaExpr = NULL;
	}

	if (expr->functionCall != NULL) {
		freeFunctionCall(expr->functionCall);
		expr->functionCall = NULL;
	}

	/ * if (expr->expr != NULL) {
		freeExpression(expr->expr);
		expr->expr = NULL;
	}

	if (expr->expr2 != NULL) {
		freeExpression(expr->expr2);
		expr->expr2 = NULL;
	} * /

	/ * if (expr->varExprPairList != NULL) {
		freeVarExprPairList(expr->varExprPairList);
		expr->varExprPairList = NULL;
	} * /

	/ * if (expr->exprPairList != NULL) {
		freeExprPairList(expr->exprPairList);
		expr->exprPairList = NULL;
	} * /

	mmFree(expr); */
}

// **** Expression list struct creation functions ****

LISP_EXPR_LIST_ELEMENT * createExpressionListElement(LISP_EXPR * expr, LISP_EXPR_LIST_ELEMENT * next) {
	LISP_EXPR_LIST_ELEMENT * result = (LISP_EXPR_LIST_ELEMENT *)mmAlloc(sizeof(LISP_EXPR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("mmAlloc() failed in createExpressionListElement()");
	}

	result->expr = expr;
	result->next = next;

	return result;
}

void freeExpressionList(LISP_EXPR_LIST_ELEMENT * exprList) {

	/* while (exprList != NULL) {
		freeExpression(exprList->expr);
		exprList->expr = NULL; / * So we don't try to free it again * /
		exprList = exprList->next;
	} */
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

	/* LISP_VAR * var = (LISP_VAR *)mmAlloc(sizeof(LISP_VAR));

	if (var == NULL) {
		fatalError("mmAlloc() failed in createVariable()");
	}

	memset(var->name, 0, maxStringValueLength);
	strcpy(var->name, name); */
	SCHEME_UNIVERSAL_TYPE * var = allocateStringAndCreateUniversalStruct(
		lispExpressionType_Variable,
		0,
		0,
		name,
		NULL,
		NULL,
		NULL
	);

	return var;
}

LISP_EXPR * createExpressionFromVariable(LISP_VAR * var) {
	LISP_EXPR * expr = createUndefinedExpression();

	expr->type = lispExpressionType_Variable;
	expr->var = var;

	return expr;
}

void freeVariable(LISP_VAR * var) {
	/* mmFree(var); */
}

LISP_NAME_VALUE_LIST_ELEMENT * createNameValueListElement(char * name, LISP_VALUE * value, LISP_NAME_VALUE_LIST_ELEMENT * next) {
	/* LISP_NAME_VALUE_LIST_ELEMENT * nvle = (LISP_NAME_VALUE_LIST_ELEMENT * )mmAlloc(sizeof(LISP_NAME_VALUE_LIST_ELEMENT));

	if (nvle == NULL) {
		fatalError("mmAlloc() failed in createNameValueListElement()");
	}

	nvle->name = name;
	nvle->value = value; / * TODO? : Clone the value? * /
	nvle->next = next; */

	SCHEME_UNIVERSAL_TYPE * nvle = allocateStringAndCreateUniversalStruct(
		schemeStructType_NameValueListElement,
		0,
		0,
		name,
		NULL,
		NULL,
		next
	);

	nvle->value = value; /* Temporary member */

	return nvle;
}

void freeNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle) {

	/* if (nvle != NULL) {
		/ * Do not free name. * /
		nvle->name = NULL;
		/ * Do not free value. * /
		nvle->value = NULL;
		freeNameValueList(nvle->next);
		nvle->next = NULL;
		mmFree(nvle);
	} */
}

LISP_ENV * createEnvironment(LISP_ENV * next) {
	/* LISP_ENV * env = (LISP_ENV *)mmAlloc(sizeof(LISP_ENV));

	if (env == NULL) {
		fatalError("mmAlloc() failed in createEnvironment()");
	}

	env->nameValueList = NULL;
	env->next = next; */
	SCHEME_UNIVERSAL_TYPE * env = createUniversalStruct(
		schemeStructType_Environment,
		0,
		0,
		NULL,
		NULL,
		NULL,
		next
	);

	return env;
}

void freeEnvironment(LISP_ENV * env) {

	/* if (env != NULL) {

		if (env->nameValueList != NULL) {
			freeNameValueList(env->nameValueList);
			env->nameValueList = NULL;
		}

		/ * If we free the entire list of environment frames,
		 * then freeing any frame will free the global environment.
		 * /

		/ * if (env->next != NULL) {
			freeEnvironment(env->next); * /
		env->next = NULL;
		/ * } * /

		mmFree(env);
	} */
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

	if (value == NULL) {
		printf("NULL");

		return;
	} else if (isList(value) /* && value->type != lispValueType_Null */) {
		char separator = '\0';

		printf("(");

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
			printf("\"%s\"", value->name);
			break;

		case lispValueType_Symbol:
			printf("'%s", value->name);
			break;

		case lispValueType_PrimitiveOperator:
			printf("%s", value->name);
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

		/* case lispValueType_Null:
			printf("()");
			break; */

		case lispPseudoValueType_Continuation:
			printf("<continuation; id %d>", value->continuationId);
			break;

		case lispPseudoValueType_ContinuationReturn:
			printf("<continuation return; id %d, value: ", value->continuationId);
			printValue(value->continuationReturnValue);
			printf(">");
			break;

		default:
			printf("<invalid value>");
			break;
	}
}

BOOL printValueToString(LISP_VALUE * value, char * buf, int bufsize) {
	/* Returns FALSE iff there is no more room to print in buf. */
	/* TODO: Use a StringBuilder */

	/* (?) It is assumed that the caller will zero-fill buf before calling this function. Or else:
	memset(buf, 0, bufsize * sizeof(char)); */

	if (isList(value) && value->type != lispValueType_Null) {
		char separator = '\0';
		int separatorLen = 0;

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf++ = '(';
		--bufsize;

		while (value->type != lispValueType_Null) {

			if (bufsize <= separatorLen) {
				*buf = '\0'; /* Null-terminate the string */
				return FALSE;
			}

			sprintf(buf, "%c", separator);
			buf += separatorLen;
			bufsize -= separatorLen;

			if (!printValueToString(value->pair->head, buf, bufsize)) {
				return FALSE;
			}

			const int len = strlen(buf);

			buf += len;
			bufsize -= len;

			separator = ' ';
			separatorLen = 1;
			value = value->pair->tail;
		}

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf = ')';

		return TRUE;
	} else if (value->type == lispValueType_Pair) {

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf++ = '(';
		--bufsize;

		if (!printValueToString(value->pair->head, buf, bufsize)) {
			return FALSE;
		}

		int len = strlen(buf);

		buf += len;
		bufsize -= len;

		if (bufsize <= 3) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf++ = ' ';
		*buf++ = '.';
		*buf++ = ' ';
		bufsize -= 3;

		if (!printValueToString(value->pair->tail, buf, bufsize)) {
			return FALSE;
		}

		len = strlen(buf);

		buf += len;
		bufsize -= len;

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf = ')';

		return TRUE;
	}

	char * strContinuation = "<contin>";
	char * strContinuationReturn = "<contRtn>";
	char * strClosure = "<closure>";
	char * strInvalid = "<invalid value>";
	const int maxPrintedIntegerLength = 10;
	int lenToAppend = 0;

	switch (value->type) {
		case lispValueType_Number:
			lenToAppend = maxPrintedIntegerLength;
			break;

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			lenToAppend = strlen(value->name);
			break;

		case lispValueType_Closure:
			lenToAppend = strlen(strClosure);
			break;

		case lispValueType_Null:
			lenToAppend = 2;
			break;

		case lispPseudoValueType_Continuation:
			lenToAppend = strlen(strContinuation);
			break;

		case lispPseudoValueType_ContinuationReturn:
			lenToAppend = strlen(strContinuationReturn);
			break;

		default:
			lenToAppend = strlen(strInvalid);
			break;
	}

	if (bufsize <= lenToAppend) {
		printf("Returning FALSE");
		*buf = '\0'; /* Null-terminate the string */
		return FALSE;
	}

	/* (listtostring '("abc" 123 "def")) -> TODO: BUG: Double quotes are not removed from string literals inside a (single-)quoted list */

	switch (value->type) {
		case lispValueType_Number:
			sprintf(buf, "%d", value->value);
			break;

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			strcpy(buf, value->name);
			break;

		case lispValueType_Closure:
			strcpy(buf, strClosure);
			break;

		case lispValueType_Null:
			strcpy(buf, "()");
			break;

		case lispPseudoValueType_Continuation:
			strcpy(buf, strContinuation);
			break;

		case lispPseudoValueType_ContinuationReturn:
			strcpy(buf, strContinuationReturn);
			break;

		default:
			strcpy(buf, strInvalid);
			break;
	}

	return TRUE;
}

/* BEGIN SCHEME_UNIVERSAL_TYPE */

SCHEME_UNIVERSAL_TYPE * createUniversalStruct(
	int type,
	int integerValue,
	int maxNameLength,
	char * name,
	SCHEME_UNIVERSAL_TYPE * value1,
	SCHEME_UNIVERSAL_TYPE * value2,
	SCHEME_UNIVERSAL_TYPE * next
) {
	SCHEME_UNIVERSAL_TYPE * result = (SCHEME_UNIVERSAL_TYPE *)mmAlloc(sizeof(SCHEME_UNIVERSAL_TYPE));

	result->mark = 0;
	result->type = type;
	result->integerValue = integerValue;
	result->maxNameLength = maxNameLength;
	result->name = name;
	result->value1 = value1;
	result->value2 = value2;
	result->next = next;

	result->value = NULL; /* Temporary */

	addItemToMemMgrRecords(result);

	return result;
}

/* If name != NULL then copy it, and set maxNameLength = strlen(name) + 1 */
/* If name == NULL and maxNameLength > 1 then mmAlloc(maxNameLength * sizeof(char)) and zero-fill it */
/* If name == NULL and maxNameLength <= 0 then set maxNameLength = the default maxStringValueLength; then mmAlloc and zero-fill */

SCHEME_UNIVERSAL_TYPE * allocateStringAndCreateUniversalStruct(
	int type,
	int integerValue,
	int maxNameLength,
	char * name,
	SCHEME_UNIVERSAL_TYPE * value1,
	SCHEME_UNIVERSAL_TYPE * value2,
	SCHEME_UNIVERSAL_TYPE * next
) {

	if (name != NULL) {
		const int len = strlen(name);

		if (maxNameLength <= len) {
			maxNameLength = len + 1;
		}
		/* This allows you to allocate a buffer longer than len + 1 chars if you wish */
	} else if (maxNameLength <= 0) {
		maxNameLength = maxStringValueLength;
	}

	char * buf = (char *)mmAlloc(maxNameLength * sizeof(char));

	memset(buf, 0, maxNameLength * sizeof(char));

	if (name != NULL) {
		strcpy(buf, name);
	}

	return createUniversalStruct(type, integerValue, maxNameLength, buf, value1, value2, next);
}

void freeUniversalStruct(SCHEME_UNIVERSAL_TYPE * expr) {

	if (expr->name != NULL) {
		mmFree(expr->name);
		expr->name = NULL;
	}

	if (expr->value1 != NULL) {
		freeUniversalStruct(expr->value1);
		expr->value1 = NULL;
	}

	if (expr->value2 != NULL) {
		freeUniversalStruct(expr->value2);
		expr->value2 = NULL;
	}

	if (expr->next != NULL) {
		freeUniversalStruct(expr->next);
		expr->next = NULL;
	}

	expr->value = NULL; /* Temporary member */

	mmFree(expr);
}

/* END SCHEME_UNIVERSAL_TYPE */

/* **** The End **** */
