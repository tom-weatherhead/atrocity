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
	return ((n + bufIncSize - 1) / bufIncSize) * bufIncSize;
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
	const int oldStrLen = (sb->name == NULL) ? 0 : strlen(sb->name);
	const int newbufsize = roundUpStringTypeBufferSize(oldStrLen + strlen(strToAppend) + 1, sb->integerValue);

	if (newbufsize > sb->maxNameLength) {
		char * newBuf = (char *)mmAlloc(newbufsize * sizeof(char));

		memset(newBuf, 0, newbufsize * sizeof(char));

		if (sb->name != NULL) {
			strcpy(newBuf, sb->name);
			mmFree(sb->name);
		}

		sb->name = newBuf;
		sb->maxNameLength = newbufsize;
	}

	if (sb->name != NULL) {
		strcat(sb->name, strToAppend);
	}

	return sb;
}

/* void freeString(STRING_BUILDER_TYPE * strObj) {
	mmFree(strObj->buf);
	strObj->buf = NULL;
	mmFree(strObj);
} */

/* END: Create a string or StringBuilder "class" with realloc() */

/* **** The End **** */
