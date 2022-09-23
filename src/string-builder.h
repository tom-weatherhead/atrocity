/* atrocity/src/string-builder.h */

BOOL isStringBuilderEmpty(STRING_BUILDER_TYPE * sb);
BOOL stringInBuilderIs(STRING_BUILDER_TYPE * sb, char * str);
void clearStringBuilder(STRING_BUILDER_TYPE * sb);
STRING_BUILDER_TYPE * appendToStringBuilder(STRING_BUILDER_TYPE * sb, char * strToAppend);
STRING_BUILDER_TYPE * appendCharToStringBuilder(STRING_BUILDER_TYPE * sb, char c);
STRING_BUILDER_TYPE * appendCharsToStringBuilder(STRING_BUILDER_TYPE * sb, char * src, int numChars);

/* **** The End **** */
