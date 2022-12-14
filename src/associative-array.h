/* atrocity/src/associative-array.h */

LISP_VALUE * createAssociativeArray();
/* BOOL aaHas(LISP_VALUE * aa, LISP_VALUE * key); */
LISP_VALUE * aaGet(LISP_VALUE * aa, LISP_VALUE * key);
LISP_VALUE * aaSet(LISP_VALUE * aa, LISP_VALUE * key, LISP_VALUE * value);
LISP_VALUE * aaSize(LISP_VALUE * aa);
LISP_VALUE * aaDeleteKey(LISP_VALUE * aa, LISP_VALUE * key);

/* **** The End **** */
