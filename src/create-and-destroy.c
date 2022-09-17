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

/* BEGIN SCHEME_UNIVERSAL_TYPE */

static SCHEME_UNIVERSAL_TYPE * createUniversalStruct(
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
	result->value3 = NULL;
	result->next = next;

	addItemToMemMgrRecords(result);

	return result;
}

/* If name != NULL then copy it, and set maxNameLength = strlen(name) + 1 */
/* If name == NULL and maxNameLength > 1 then mmAlloc(maxNameLength * sizeof(char)) and zero-fill it */
/* If name == NULL and maxNameLength <= 0 then set maxNameLength = the default maxStringValueLength; then mmAlloc and zero-fill */

static SCHEME_UNIVERSAL_TYPE * allocateStringAndCreateUniversalStruct(
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

	if (expr->value3 != NULL) {
		freeUniversalStruct(expr->value3);
		expr->value3 = NULL;
	}

	if (expr->next != NULL) {
		freeUniversalStruct(expr->next);
		expr->next = NULL;
	}

	mmFree(expr);
}

/* END SCHEME_UNIVERSAL_TYPE */

/* **** Value struct creation functions **** */

/* int getNumCharsAllocatedToNameBufInValue(LISP_VALUE * value) {
	/ * Return the number of chars, not necessarily the number of bytes. * /

	return value->maxNameLength;
} */

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

	getBodyInClosure(closure) = body; /* I.e. closure->value3 = body; */

	return closure;
}

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

/* TODO:
A thunk is a suspended computation; used to implement lazy evaluation in SASL.
A thunk is implemented as a closure thqt takes no arguments.

LISP_VALUE * createThunk(LISP_EXPR * body, LISP_ENV * env) {
	return createUniversalStruct(
		lispValueType_Thunk,
		0,
		0,
		NULL,
		body,
		env,
		NULL
	);
}
*/

LISP_VALUE * createContinuation(int id) {
	return createUniversalStruct(
		lispPseudoValueType_Continuation,
		id,
		0,
		NULL,
		NULL,
		NULL,
		NULL
	);
}

LISP_VALUE * createContinuationReturn(int id, LISP_VALUE * value) {
	return createUniversalStruct(
		lispPseudoValueType_ContinuationReturn,
		id,
		0,
		NULL,
		value,
		NULL,
		NULL
	);
}

/* LISP_VALUE * cloneValue(LISP_VALUE * value) {

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
			return createClosure(getArgsInClosure(value), getBodyInClosure(value), getEnvInClosure(value));

		case lispValueType_Pair:
			return createPair(getHeadInPair(value), getTailInPair(value));

		case lispValueType_Null:
			return createNull();

		case lispPseudoValueType_Continuation:
		case lispPseudoValueType_ContinuationReturn:
			/ * fprintf(stderr, "Warning: cloneValue() returning the original value for Continuation or ContinuationReturn\n"); * /
			return value;

		default:
			fprintf(stderr, "Failed to clone value of type %d\n", value->type);
			fatalError("cloneValue() : Bad value type");
	}

	return NULL;
} */

// **** Expression struct creation functions ****

LISP_VAR_LIST_ELEMENT * createVariableListElement(LISP_VAR * var, LISP_VAR_LIST_ELEMENT * next) {
	failIf(var == NULL, "createVariableListElement() : var == NULL");
	failIf(var->name == NULL, "createVariableListElement() : var->name == NULL");

	return allocateStringAndCreateUniversalStruct(
		schemeStructType_VariableListElement,
		0,
		0,
		var->name,
		NULL,
		NULL,
		next
	);
}

LISP_EXPR_PAIR_LIST_ELEMENT * createExpressionPairListElement(LISP_EXPR * expr, LISP_EXPR * expr2, LISP_EXPR_PAIR_LIST_ELEMENT * next) {
	return createUniversalStruct(
		schemeStructType_ExpressionPairListElement,
		0,
		0,
		NULL,
		expr,
		expr2,
		next
	);
}

LISP_VAR_EXPR_PAIR_LIST_ELEMENT * createVariableExpressionPairListElement(char * buf, LISP_EXPR * expr, LISP_VAR_EXPR_PAIR_LIST_ELEMENT * next) {
	return allocateStringAndCreateUniversalStruct(
		schemeStructType_VariableExpressionPairListElement,
		0,
		0,
		buf,
		expr,
		NULL,
		next
	);
}

LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body) {
	return createUniversalStruct(
		lispExpressionType_LambdaExpr,
		0,
		0,
		NULL,
		args,
		body,
		NULL
	);
}

LISP_EXPR * createSetExpression(LISP_VAR * var, LISP_EXPR * expr) {
	return createUniversalStruct(
		lispExpressionType_SetExpr,
		0,
		0,
		NULL,
		var,
		expr,
		NULL
	);
}

// **** Expression list struct creation functions ****

LISP_EXPR_LIST_ELEMENT * createExpressionListElement(LISP_EXPR * expr, LISP_EXPR_LIST_ELEMENT * next) {
	return createUniversalStruct(
		schemeStructType_ExpressionListElement,
		0,
		0,
		NULL,
		expr,
		NULL,
		next
	);
}

/* A variable is an Expression but not a Value. */

LISP_VAR * createVariable(char * name) {

	/* Ensure that name does not contain ( or ) */
	if (strchr(name, '(') != NULL || strchr(name, ')')) {
		fprintf(stderr, "createVariable() : The name '%s' contains an illegal character: '(' or ')'.\n", name);
		fatalError("createVariable() : String contains an illegal character");
	}

	return allocateStringAndCreateUniversalStruct(
		lispExpressionType_Variable,
		0,
		0,
		name,
		NULL,
		NULL,
		NULL
	);
}

LISP_EXPR * createExpressionFromVariable(LISP_VAR * var) {
	return createUniversalStruct(
		lispExpressionType_Variable,
		0,
		0,
		NULL,
		var,
		NULL,
		NULL
	);
}

LISP_NAME_VALUE_LIST_ELEMENT * createNameValueListElement(char * name, LISP_VALUE * value, LISP_NAME_VALUE_LIST_ELEMENT * next) {
	return allocateStringAndCreateUniversalStruct(
		schemeStructType_NameValueListElement,
		0,
		0,
		name,
		value,
		NULL,
		next
	);
}

LISP_ENV * createEnvironment(LISP_ENV * next) {
	return createUniversalStruct(
		schemeStructType_Environment,
		0,
		0,
		NULL,
		NULL,
		NULL,
		next
	);
}

LISP_EXPR * createExpressionFromValue(LISP_VALUE * value) {
	return createUniversalStruct(
		lispExpressionType_Value,
		0,
		0,
		NULL,
		value,
		NULL,
		NULL
	);
}

LISP_EXPR * createFunctionCallExpression(LISP_EXPR_LIST_ELEMENT * exprList) {
	return createUniversalStruct(
		lispExpressionType_FunctionCall,
		0,
		0,
		NULL,
		exprList->next,
		getExprInExprList(exprList),
		NULL
	);
}

LISP_EXPR * createLetExpression(int exprType, LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList, LISP_EXPR * expr) {
	return createUniversalStruct(
		exprType,
		0,
		0,
		NULL,
		varExprPairList,
		expr,
		NULL
	);
}

LISP_EXPR * createBeginExpression(LISP_EXPR_LIST_ELEMENT * exprList) {
	return createUniversalStruct(
		lispExpressionType_Begin,
		0,
		0,
		NULL,
		exprList,
		NULL,
		NULL
	);
}

LISP_EXPR * createWhileExpression(LISP_EXPR * condition, LISP_EXPR * body) {
	return createUniversalStruct(
		lispExpressionType_While,
		0,
		0,
		NULL,
		body,
		condition,
		NULL
	);
}

LISP_EXPR * createCondExpression(LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList) {
	return createUniversalStruct(
		lispExpressionType_Cond,
		0,
		0,
		NULL,
		exprPairList,
		NULL,
		NULL
	);
}

LISP_EXPR * createDefineMacroExpression(char * dstBuf, LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * expr) {
	return allocateStringAndCreateUniversalStruct(
		lispExpressionType_Macro,
		0,
		0,
		dstBuf,
		args,
		expr,
		NULL
	);
}

SCHEME_UNIVERSAL_TYPE * createMacroListElement(LISP_EXPR * macro, SCHEME_UNIVERSAL_TYPE * macroList) {
	return createUniversalStruct(
		schemeStructType_MacroListElement,
		0,
		0,
		NULL,
		macro,
		NULL,
		macroList
	);
}

STRING_BUILDER_TYPE * createStringBuilder(int bufIncSize) {
	const int defaultBufferIncrementSize = 16;

	return createUniversalStruct(
		stringBuilderType,
		(bufIncSize > 0) ? bufIncSize : defaultBufferIncrementSize,
		0,
		NULL,
		NULL,
		NULL,
		NULL
	);
}

/* **** The End **** */
