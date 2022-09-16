/* atrocity/src/thunk.c */

/*
#include <stdlib.h>

#include "types.h"

#include "create-and-destroy.h"
#include "evaluate.h"

static BOOL isValueOrThunk(LISP_EXPR * expr) {
	return expr->type <= lispValueType_LastValue;
}

LISP_VALUE * exprToValueOrThunk(LISP_EXPR * expr, LISP_ENV * env) {

	if (isValueOrThunk(expr)) {
		return expr; / * expr is already a value or a thunk * /
	}

	return createThunk(expr, env);
}

LISP_VALUE * dethunk(LISP_VALUE * value) {
	LISP_VALUE * result = value;

	failIf(value == NULL, "dethunk() : value->name != NULL");

	while (result->type == lispValueType_Thunk) {
		/ * I.e. result = evaluate(result->body, result->env); * /
		result = evaluate(result->value1, result->value2);
	}

	failIf(value->name != NULL, "dethunk() : value->name != NULL");

	value->mark = result->mark;
	value->type = result->type;
	value->integerValue = result->integerValue;
	value->maxNameLength = result->maxNameLength;
	NO: value->name = result->name;
	value->value1 = result->value1;
	value->value2 = result->value2;
	value->value3 = result->value3;
	value->next = result->next;

	TODO: Create a copy of the string result->name

	return value;
} */

/* **** The End **** */
