/* atrocity/src/string-builder.h */

/* typedef struct {
	char * buf;
	/ * Use size_t ? * /
	int bufSize; / * Capacity (in chars) of chunk of memory pointed to by buf * /
	int bufIncSize;
} STRING_BUILDER_TYPE; */

/* STRING_BUILDER_TYPE * createString(char * buf, int bufSize);
STRING_BUILDER_TYPE * createStringBuilder(int bufIncSize); */
STRING_BUILDER_TYPE * appendToStringBuilder(STRING_BUILDER_TYPE * sb, char * strToAppend);
/* void freeString(STRING_BUILDER_TYPE * strObj); */

/* **** The End **** */
