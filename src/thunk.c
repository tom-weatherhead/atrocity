/* atrocity/src/thunk.c */

#include <stdlib.h>
/* #include <stdio.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "evaluate.h"

static BOOL isValueOrThunk(LISP_EXPR * expr) {
	return expr->type <= lispValueType_LastValue;
}

LISP_VALUE * exprToValueOrThunk(LISP_EXPR * expr, LISP_ENV * env) {

	if (isValueOrThunk(expr)) {
		return expr; /* expr is already a value or a thunk */
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

	if (value->type != lispValueType_Thunk) {
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

	/* printf("dethunk : Exited while loop\n"); */

	failIf(value->name != NULL, "dethunk() : value->name != NULL");

	value->mark = result->mark;
	value->type = result->type;
	value->integerValue = result->integerValue;
	value->maxNameLength = result->maxNameLength;
	value->name = result->name;
	value->value1 = result->value1;
	value->value2 = result->value2;
	value->value3 = result->value3;
	value->next = result->next;

	result->name = NULL;

	/* printf("dethunk END\n"); */

	return value;
}

LISP_VALUE_LIST_ELEMENT * dethunkList(LISP_VALUE_LIST_ELEMENT * listOfValuesOrThunks) {

	if (listOfValuesOrThunks == NULL) {
		return NULL;
	}

	LISP_VALUE * dethunkedValue = dethunk(getValueInValueListElement(listOfValuesOrThunks));
	LISP_VALUE_LIST_ELEMENT * next = dethunkList(listOfValuesOrThunks->next);

	return createValueListElement(dethunkedValue, next);
}

/* **** The End **** */
