/* atrocity/src/string-builder.h */

typedef struct {
	char * buf;
	/* Use size_t ? */
	int bufSize; /* Capacity (in chars) of chunk of memory pointed to by buf */
} STRING_BUILDER_TYPE;

STRING_BUILDER_TYPE * createString(char * buf, int bufSize);
STRING_BUILDER_TYPE * appendToString(STRING_BUILDER_TYPE * strObj, char * strToAppend);
void freeString(STRING_BUILDER_TYPE * strObj);

/* **** The End **** */
