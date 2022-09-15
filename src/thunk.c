/* atrocity/src/thunk.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "create-and-destroy.h"
#include "evaluate.h"
#include "memory-manager.h"

static BOOL isValueOrThunk(LISP_EXPR * expr) {
	return expr->type <= lispValueType_LastValue;
}

LISP_VALUE * exprToValueOrThunk(LISP_EXPR * expr, LISP_ENV * env) {

	if (isValueOrThunk(expr)) {
		return expr; /* expr is already a value or a thunk */
	} else if (expr->type == lispExpressionType_Value) {
		return getValueInExpr(expr);
	}

	return createThunk(expr, env);

	/* TEMP: return evaluate(expr, env); */
}

LISP_VALUE_LIST_ELEMENT * exprListToListOfValuesOrThunks(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env) {

	if (exprList == NULL) {
		return NULL;
	}

	LISP_VALUE * valueOrThunk = exprToValueOrThunk(getExprInExprList(exprList), env);

	LISP_VALUE_LIST_ELEMENT * next = exprListToListOfValuesOrThunks(exprList->next, env);

	return createValueListElement(valueOrThunk, next);
}

LISP_VALUE * dethunk(LISP_VALUE * value) {
	/* printf("dethunk BEGIN; value is %ld\n", value);
	printf("-> value type is %d\n", value->type); */

	if (value->type == lispPseudoValueType_EvaluatedThunk) {
		return value->next;
	} else if (value->type != lispValueType_Thunk) {
		/* printf("dethunk : Returning early\n"); */

		return value;
	}

	LISP_VALUE * result = value;

	while (result->type == lispValueType_Thunk) {
		/* printf("dethunk : Inside while loop\n"); */

		/* I.e. result = evaluate(result->body, result->env); */
		result = evaluate(result->value1, result->value2);
		/* printf("-> result type is %d\n", result->type); */
	}

	/* printf("dethunk : Exited while loop\n");
	printf("  value is %ld\n", value);
	printf("  result is %ld\n", result);
	printf("  result->type is %d\n", result->type);
	printf("  result->name is %ld\n", result->name); */

	failIf(value->name != NULL, "dethunk() : value->name != NULL");

	failIf(result->type == lispValueType_Symbol && result->name == NULL, "dethunk() : result is a symbol with a NULL name");

	/* 2022-09-15 : IInstead of copying, use the mem mgr
	to replace every reference to value with a reference to result. */

	/* const int numChanged = mmReplacePointer(value, result); */

	/* printf("Replaced %ld with %ld in %d place(s).\n", value, result, numChanged); */

	/* value->mark = result->mark;
	value->type = result->type;
	value->integerValue = result->integerValue;
	value->maxNameLength = result->maxNameLength;
	value->name = result->name;
	value->value1 = result->value1;
	value->value2 = result->value2;
	value->value3 = result->value3;
	value->next = result->next;

	if (result->name != NULL) {
		value->name = (char *)mmAlloc(value->maxNameLength * sizeof(char));
		memset(value->name, 0, value->maxNameLength * sizeof(char));
		strcpy(value->name, result->name);
	} */

	/* result->name = NULL; */

	/* printf("dethunk END\n"); */

	/* Turn value into an EvaluatedThunk that refers to its actual value */

	value->mark = 0;
	value->type = lispPseudoValueType_EvaluatedThunk;
	value->integerValue = 0;
	value->maxNameLength = 0;
	value->value1 = NULL;
	value->value2 = NULL;
	value->value3 = NULL;
	value->next = result;

	return result;
}

LISP_VALUE_LIST_ELEMENT * dethunkList(LISP_VALUE_LIST_ELEMENT * listOfValuesOrThunks) {

	if (listOfValuesOrThunks == NULL) {
		return NULL;
	}

	LISP_VALUE * dethunkedValue = dethunk(getValueInValueListElement(listOfValuesOrThunks));

	failIf(!isUnthunkedValue(dethunkedValue), "Value is not an unthunked value");

	LISP_VALUE_LIST_ELEMENT * next = dethunkList(listOfValuesOrThunks->next);

	return createValueListElement(dethunkedValue, next);
}

LISP_VALUE * deepDethunk(LISP_VALUE * value) {
	/* Warning: This will go into an infinite loop if called with a circular
	data structure. */

	if (value == NULL) {
		return NULL;
	}

	dethunk(value);
	/* value->mark = 1; */

	deepDethunk(value->value1);
	deepDethunk(value->value2);
	deepDethunk(value->value3);
	deepDethunk(value->next);

	return value;
}

/* **** The End **** */