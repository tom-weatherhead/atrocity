/* atrocity/src/associative-array.h */

LISP_VALUE * aaCreate();
/* BOOL aaHas(LISP_VALUE * key); */
LISP_VALUE * aaGet(LISP_VALUE * key);
void aaSet(LISP_VALUE * key, LISP_VALUE * value);

/* **** The End **** */
