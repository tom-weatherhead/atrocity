/* atrocity/src/string-builder.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "memory-manager.h"
#include "string-builder.h"

/* Functions */

/* TODO: Create a string or StringBuilder "class" with realloc() */

/* const int defaultBufferIncrementSize = 16; */

static int roundUpStringTypeBufferSize(int n, int bufIncSize) {
	return (n + bufIncSize - 1) / bufIncSize;
}

/* STRING_BUILDER_TYPE * createString(char * buf, int bufSize) {
	int bufstrlen = 1;

	if (buf != NULL) {
		bufstrlen = strlen(buf) + 1;
	}

	if (bufstrlen > bufSize) {
		bufSize = bufstrlen;
	}

	bufSize = roundUpStringTypeBufferSize(bufSize);

	STRING_BUILDER_TYPE * strObj = (STRING_BUILDER_TYPE *)mmAlloc(sizeof(STRING_BUILDER_TYPE));

	strObj->bufSize = bufSize;
	strObj->buf = (char *)mmAlloc(bufSize * sizeof(char));
	memset(strObj->buf, 0, bufSize);

	if (buf != NULL) {
		strcpy(strObj->buf, buf);
	}

	return strObj;
} */

STRING_BUILDER_TYPE * appendToStringBuilder(STRING_BUILDER_TYPE * sb, char * strToAppend) {
	const int newbufsize = roundUpStringTypeBufferSize(strlen(sb->name) + strlen(strToAppend) + 1, sb->integerValue);

	if (newbufsize > sb->maxNameLength) {
		/* Re-allocate */
		sb->name = (char *)mmRealloc(sb->name, newbufsize);
		memset(sb->name + sb->maxNameLength, 0, (sb->maxNameLength - newbufsize) * sizeof(char));
		sb->maxNameLength = newbufsize;
	}

	strcat(sb->name, strToAppend);

	return sb;
}

/* void freeString(STRING_BUILDER_TYPE * strObj) {
	mmFree(strObj->buf);
	strObj->buf = NULL;
	mmFree(strObj);
} */

/* END: Create a string or StringBuilder "class" with realloc() */

/* **** The End **** */
