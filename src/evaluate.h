/* atrocity/src/evaluate.h */

BOOL isUnthunkedValue(SCHEME_UNIVERSAL_TYPE * ptr);

LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env);

/* **** The End **** */
