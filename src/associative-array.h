/* atrocity/src/associative-array.h */

LISP_VALUE * aaCreate();
/* BOOL aaHas(LISP_VALUE * key); */
LISP_VALUE * aaGet(LISP_VALUE * aa, LISP_VALUE * key);
LISP_VALUE * aaSet(LISP_VALUE * aa, LISP_VALUE * key, LISP_VALUE * value);

/* **** The End **** */
