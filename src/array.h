/* atrocity/src/array.h */

LISP_VALUE * getArrayLength(LISP_VALUE * array);
LISP_VALUE * push(LISP_VALUE * array, LISP_VALUE * value);
LISP_VALUE * pop(LISP_VALUE * array);
LISP_VALUE * peek(LISP_VALUE * array);
LISP_VALUE * unshiftArray(LISP_VALUE * array, LISP_VALUE * value);
LISP_VALUE * shiftArray(LISP_VALUE * array);

/* **** The End **** */
