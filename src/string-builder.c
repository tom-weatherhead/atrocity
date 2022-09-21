/* atrocity/src/string-builder.c */

#include <stdlib.h>
/* #include <stdio.h> */
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

BOOL isStringBuilderEmpty(STRING_BUILDER_TYPE * sb) {
	return sb == NULL || sb->name == NULL || sb->maxNameLength == 0 || strlen(sb->name) == 0;
}

void clearStringBuilder(STRING_BUILDER_TYPE * sb) {

	if (sb != NULL && sb->name != NULL && sb->maxNameLength > 0) {
		memset(sb->name, 0, sb->maxNameLength * sizeof(char));
	}
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

STRING_BUILDER_TYPE * appendCharToStringBuilder(STRING_BUILDER_TYPE * sb, char c) {
	char twoChars[2];

	twoChars[0] = c;
	twoChars[1] = '\0';

	return appendToStringBuilder(sb, twoChars);
}

STRING_BUILDER_TYPE * appendCharsToStringBuilder(STRING_BUILDER_TYPE * sb, char * src, int numChars) {
	char * buf = (char *)mmAlloc((numChars + 1) * sizeof(char));

	memcpy(buf, src, numChars * sizeof(char));
	buf[numChars] = '\0';

	appendToStringBuilder(sb, buf);

	mmFree(buf);

	return sb;
}

/* TODO:
STRING_BUILDER_TYPE * appendLineFromFileToStringBuilder(STRING_BUILDER_TYPE * sb, FILE * file) {} */

/* **** The End **** */
