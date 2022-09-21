/* atrocity/src/string-builder.h */

BOOL isStringBuilderEmpty(STRING_BUILDER_TYPE * sb);
void clearStringBuilder(STRING_BUILDER_TYPE * sb);
STRING_BUILDER_TYPE * appendToStringBuilder(STRING_BUILDER_TYPE * sb, char * strToAppend);
STRING_BUILDER_TYPE * appendCharToStringBuilder(STRING_BUILDER_TYPE * sb, char c);
STRING_BUILDER_TYPE * appendCharsToStringBuilder(STRING_BUILDER_TYPE * sb, char * src, int numChars);

/* TODO:
STRING_BUILDER_TYPE * appendLineFromFileToStringBuilder(STRING_BUILDER_TYPE * sb, FILE * file); */

/* **** The End **** */
