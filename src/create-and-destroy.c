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

extern int numMallocsForExpressions;

/* **** Value struct creation functions **** */

int getNumCharsAllocatedToNameBufInValue(LISP_VALUE * value) {
	/* Return the number of chars, not necessarily the number of bytes. */

	return value->maxNameLength;
}

LISP_VALUE * createNumericValue(int value) {
	return createUniversalStruct(
		lispValueType_Number,
		value,
		0,
		NULL,
		NULL,
		NULL,
		NULL
	);
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

	SCHEME_UNIVERSAL_TYPE * result = allocateStringAndCreateUniversalStruct(
		lispValueType_String,
		0,
		0,
		str,
		NULL,
		NULL,
		NULL
	);

	result->name[len] = '\0'; /* Overwrite any closing double-quote */

	return result;
}

LISP_VALUE * createSymbolValue(char * value) {

	if (strlen(value) >= maxStringValueLength) {
		fprintf(stderr, "The string '%s' is too long to be a symbol value.", value);
		fatalError("createSymbolValue() : String too long");
	}

	return allocateStringAndCreateUniversalStruct(
		lispValueType_Symbol,
		0,
		0,
		value,
		NULL,
		NULL,
		NULL
	);
}

LISP_VALUE * createPrimitiveOperator(char * value) {
	return allocateStringAndCreateUniversalStruct(
		lispValueType_PrimitiveOperator,
		0,
		0,
		value,
		NULL,
		NULL,
		NULL
	);
}

LISP_VALUE * createClosure(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body, LISP_ENV * env) {
	SCHEME_UNIVERSAL_TYPE * closure = createUniversalStruct(
		lispValueType_Closure,
		0,
		0,
		NULL,
		args,
		env,
		NULL
	);

	closure->expr = body;

	return closure;
}

/* TODO:
A thunk is a suspended computation; used to implement lazy evaluation in SASL.
A thunk is implemented as a closure thqt takes no arguments.

LISP_VALUE * createThunk(LISP_EXPR * body, LISP_ENV * env) {
	return createClosure(NULL, body, env);
} */

LISP_VALUE * createPair(LISP_VALUE * head, LISP_VALUE * tail) {
	return createUniversalStruct(
		lispValueType_Pair,
		0,
		0,
		NULL,
		head,
		tail,
		NULL
	);
}

LISP_VALUE * createNull() {
	/* TODO: Just return NULL; */

	return createUniversalStruct(
		lispValueType_Null,
		0,
		0,
		NULL,
		NULL,
		NULL,
		NULL
	);
}

LISP_VALUE * cloneValue(LISP_VALUE * value) {

	switch (value->type) {
		case lispValueType_Number:
			return createNumericValue(getIntegerValueInValue(value));

		case lispValueType_String:
			return createStringValue(getNameInValue(value));

		case lispValueType_Symbol:
			return createSymbolValue(getNameInValue(value));

		case lispValueType_PrimitiveOperator:
			return createPrimitiveOperator(getNameInValue(value));

		case lispValueType_Closure:
			return createClosure(getArgsInClosure(getClosureInValue(value)), getBodyInClosure(getClosureInValue(value)), getEnvInClosure(getClosureInValue(value)));

		case lispValueType_Pair:
			return createPair(getHeadInPair(getPairInValue(value)), getTailInPair(getPairInValue(value)));

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

// **** Expression struct creation functions ****

static LISP_EXPR * createUndefinedExpression() {
	LISP_EXPR * result = (LISP_EXPR *)mmAlloc(sizeof(LISP_EXPR));

	++numMallocsForExpressions;

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
	failIf(var == NULL, "createVariableListElement() : var == NULL");
	failIf(var->name == NULL, "createVariableListElement() : var->name == NULL");

	SCHEME_UNIVERSAL_TYPE * result = allocateStringAndCreateUniversalStruct(
		schemeStructType_VariableListElement,
		0,
		0,
		var->name,
		NULL,
		NULL,
		next
	);

	return result;
}

LISP_EXPR_PAIR_LIST_ELEMENT * createExpressionPairListElement(LISP_EXPR * expr, LISP_EXPR * expr2, LISP_EXPR_PAIR_LIST_ELEMENT * next) {
	LISP_EXPR_PAIR_LIST_ELEMENT * result = createUniversalStruct(
		schemeStructType_ExpressionPairListElement,
		0,
		0,
		NULL,
		NULL,
		NULL,
		next
	);

	result->expr = expr;
	result->expr2 = expr2;

	return result;
}

LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body) {
	SCHEME_UNIVERSAL_TYPE * lambdaExpr = createUniversalStruct(
		schemeStructType_LambdaExpr,
		0,
		0,
		NULL,
		args,
		NULL,
		NULL
	);

	lambdaExpr->expr = body;

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_LambdaExpr;
	result->lambdaExpr = lambdaExpr;

	return result;
}

LISP_EXPR * createSetExpression(LISP_VAR * var, LISP_EXPR * expr) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_SetExpr;
	result->var = var;
	result->expr = expr;

	return result;
}

// **** Expression list struct creation functions ****

LISP_EXPR_LIST_ELEMENT * createExpressionListElement(LISP_EXPR * expr, LISP_EXPR_LIST_ELEMENT * next) {
	SCHEME_UNIVERSAL_TYPE * result = createUniversalStruct(
		schemeStructType_ExpressionListElement,
		0,
		0,
		NULL,
		NULL,
		NULL,
		next
	);

	result->expr = expr;

	return result;
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

LISP_NAME_VALUE_LIST_ELEMENT * createNameValueListElement(char * name, LISP_VALUE * value, LISP_NAME_VALUE_LIST_ELEMENT * next) {
	SCHEME_UNIVERSAL_TYPE * nvle = allocateStringAndCreateUniversalStruct(
		schemeStructType_NameValueListElement,
		0,
		0,
		name,
		value,
		NULL,
		next
	);

	return nvle;
}

LISP_ENV * createEnvironment(LISP_ENV * next) {
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

LISP_EXPR * createExpressionFromValue(LISP_VALUE * value) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Value;
	result->value = value;

	return result;
}

LISP_EXPR * createFunctionCallExpression(LISP_EXPR_LIST_ELEMENT * exprList) {
	SCHEME_UNIVERSAL_TYPE * functionCall = createUniversalStruct(
		schemeStructType_FunctionCall,
		0,
		0,
		NULL,
		exprList->next,
		NULL,
		NULL
	);

	functionCall->expr = exprList->expr;

	exprList->expr = NULL;
	exprList->next = NULL;
	/* mmFree(exprList); */

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_FunctionCall;
	result->functionCall = functionCall;

	return result;
}

LISP_EXPR * createLetExpression(int exprType, LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList, LISP_EXPR * expr) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = exprType;
	result->varExprPairList = varExprPairList;
	result->expr = expr;

	return result;
}

LISP_EXPR * createBeginExpression(LISP_EXPR_LIST_ELEMENT * exprList) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Begin;
	result->exprList = exprList;

	return result;
}

LISP_EXPR * createWhileExpression(LISP_EXPR * condition, LISP_EXPR * body) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_While;
	result->expr = condition;
	result->expr2 = body;

	return result;
}

LISP_EXPR * createCondExpression(LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Cond;
	result->exprPairList = exprPairList;

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
			return isList(getTailInPair(getPairInValue(value)));

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
			printValue(getHeadInPair(getPairInValue(value)));
			separator = ' ';
			value = getTailInPair(getPairInValue(value));
		}

		printf(")");

		return;
	}

	switch (value->type) {
		case lispValueType_Number:
			printf("%d", getIntegerValueInValue(value));
			break;

		case lispValueType_String:
			printf("\"%s\"", getNameInValue(value));
			break;

		case lispValueType_Symbol:
			printf("'%s", getNameInValue(value));
			break;

		case lispValueType_PrimitiveOperator:
			printf("%s", getNameInValue(value));
			break;

		case lispValueType_Closure:
			printf("<closure>");
			break;

		case lispValueType_Pair:
			printf("Pair: (");
			printValue(getHeadInPair(getPairInValue(value)));
			printf(" . ");
			printValue(getTailInPair(getPairInValue(value)));
			printf(")");
			break;

		/* case lispValueType_Null:
			printf("()");
			break; */

		case lispPseudoValueType_Continuation:
			printf("<continuation; id %d>", getContinuationIdInValue(value));
			break;

		case lispPseudoValueType_ContinuationReturn:
			printf("<continuation return; id %d, value: ", getContinuationIdInValue(value));
			printValue(getContinuationReturnValueInValue(value));
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

			if (!printValueToString(getHeadInPair(getPairInValue(value)), buf, bufsize)) {
				return FALSE;
			}

			const int len = strlen(buf);

			buf += len;
			bufsize -= len;

			separator = ' ';
			separatorLen = 1;
			value = getTailInPair(getPairInValue(value));
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

		if (!printValueToString(getHeadInPair(getPairInValue(value)), buf, bufsize)) {
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

		if (!printValueToString(getTailInPair(getPairInValue(value)), buf, bufsize)) {
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
			sprintf(buf, "%d", getIntegerValueInValue(value));
			break;

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			strcpy(buf, getNameInValue(value));
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

	result->expr = NULL; /* Temporary */
	result->expr2 = NULL; /* Temporary */

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

	expr->expr = NULL; /* Temporary member */
	expr->expr2 = NULL; /* Temporary member */

	mmFree(expr);
}

/* END SCHEME_UNIVERSAL_TYPE */

/* **** The End **** */
