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

static int roundUpStringTypeBufferSize(int n, int bufIncSize) {
	return ((n + bufIncSize - 1) / bufIncSize) * bufIncSize;
}

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

/* **** The End **** */
