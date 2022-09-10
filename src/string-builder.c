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

const int stringTypeBufferSizeIncrement = 16;

static int roundUpStringTypeBufferSize(int n) {
	return (n + stringTypeBufferSizeIncrement - 1) / stringTypeBufferSizeIncrement;
}

STRING_BUILDER_TYPE * createString(char * buf, int bufSize) {
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
}

STRING_BUILDER_TYPE * appendToString(STRING_BUILDER_TYPE * strObj, char * strToAppend) {
	const int newbufsize = roundUpStringTypeBufferSize(strlen(strObj->buf) + strlen(strToAppend) + 1);

	if (newbufsize > strObj->bufSize) {
		/* Re-allocate */
		strObj->buf = (char *)mmRealloc(strObj->buf, newbufsize);
		memset(strObj->buf + strObj->bufSize, 0, (strObj->bufSize - newbufsize) * sizeof(char));
		strObj->bufSize = newbufsize;
	}

	strcat(strObj->buf, strToAppend);

	return strObj;
}

void freeString(STRING_BUILDER_TYPE * strObj) {
	mmFree(strObj->buf);
	strObj->buf = NULL;
	mmFree(strObj);
}

/* END: Create a string or StringBuilder "class" with realloc() */

/* **** The End **** */
