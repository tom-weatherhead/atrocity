/* atrocity/src/print.h */

void printValue(LISP_VALUE * value);
/* BOOL printValueToString(LISP_VALUE * value, char * buf, int bufsize); */
STRING_BUILDER_TYPE * printValueToString(
	STRING_BUILDER_TYPE * sb,
	LISP_VALUE * value,
	char * separatorBetweenListItems,
	BOOL printBracketsAroundList
);

/* **** The End **** */
