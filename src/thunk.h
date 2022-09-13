/* atrocity/src/thunk.h */

LISP_VALUE * exprToValueOrThunk(LISP_EXPR * expr, LISP_ENV * env);
SCHEME_UNIVERSAL_TYPE * exprListToListOfValuesOrThunks(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env);

LISP_VALUE * dethunk(LISP_VALUE * value);
LISP_VALUE_LIST_ELEMENT * dethunkList(LISP_VALUE_LIST_ELEMENT * listOfValuesOrThunks);

/* **** The End **** */
