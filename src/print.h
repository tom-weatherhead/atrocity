/* atrocity/src/print.h */

void printValue(LISP_VALUE * value);

STRING_BUILDER_TYPE * printValueToStringEx(
	STRING_BUILDER_TYPE * sb,
	LISP_VALUE * value,
	char * separatorBetweenListItems,
	BOOL printBracketsAroundList
);
STRING_BUILDER_TYPE * printValueToString(STRING_BUILDER_TYPE * sb, LISP_VALUE * value);

STRING_BUILDER_TYPE * printExpressionToStringEx(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr, BOOL (*fnHandler)(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr));
STRING_BUILDER_TYPE * printExpressionToString(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr);

/* **** The End **** */
