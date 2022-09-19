/* atrocity/src/evaluate.h */

BOOL isPrimop(char * str);
BOOL areValuesEqual(LISP_VALUE * v1, LISP_VALUE * v2);
LISP_VALUE * evaluate(LISP_EXPR * expr, LISP_ENV * env);

/* **** The End **** */
