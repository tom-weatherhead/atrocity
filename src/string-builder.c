/* atrocity/src/string-builder.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "memory-manager.h"
#include "string-builder.h"

/* Functions */

static int roundUpStringTypeBufferSize(int n, int bufIncSize) {
	failIf(bufIncSize <= 0, "StringBuilder roundUpStringTypeBufferSize() : bufIncSize <= 0");

	return ((n + bufIncSize - 1) / bufIncSize) * bufIncSize;
}

BOOL isStringBuilderEmpty(STRING_BUILDER_TYPE * sb) {
	return sb == NULL || sb->name == NULL || sb->maxNameLength == 0 || strlen(sb->name) == 0;
}

/* TODO: Use this in parser.c : */

BOOL stringInBuilderIs(STRING_BUILDER_TYPE * sb, char * str) {
	return sb != NULL && str != NULL && sb->name != NULL && !strcmp(sb->name, str);
}

void clearStringBuilder(STRING_BUILDER_TYPE * sb) {

	if (sb != NULL) {
		failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "clearStringBuilder() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (1)");
	}

	if (sb != NULL && sb->name != NULL && sb->maxNameLength > 0) {
		memset(sb->name, 0, sb->maxNameLength * sizeof(char));
	}

	if (sb != NULL) {
		failIf(getBufferSizeIncrementInStringBuilder(sb) <= 0, "clearStringBuilder() : getBufferSizeIncrementInStringBuilder(sb) <= 0 (2)");
	}
}

/* newMinimumSize must already include one for the terminating null char */

static void ensureStringBuilderSize(STRING_BUILDER_TYPE * sb, int newMinimumSize) {
	newMinimumSize = roundUpStringTypeBufferSize(newMinimumSize, getBufferSizeIncrementInStringBuilder(sb));

	if (newMinimumSize > sb->maxNameLength) {
		const int numBytes = newMinimumSize * sizeof(char);
		char * newBuf = (char *)mmAlloc(numBytes);

		memset(newBuf, 0, numBytes);

		if (sb->name != NULL) {
			strcpy(newBuf, sb->name);
			mmFree(sb->name);
		}

		sb->name = newBuf;
		sb->maxNameLength = newMinimumSize;
	}
}

STRING_BUILDER_TYPE * appendToStringBuilder(STRING_BUILDER_TYPE * sb, char * strToAppend) {
	failIf(sb == NULL, "appendToStringBuilder() : sb == NULL");
	failIf(strToAppend == NULL, "appendToStringBuilder() : strToAppend == NULL");

	const int oldStrLen = (sb->name == NULL) ? 0 : strlen(sb->name);

	/* const int newbufsize = roundUpStringTypeBufferSize(oldStrLen + strlen(strToAppend) + 1, getBufferSizeIncrementInStringBuilder(sb));

	if (newbufsize > sb->maxNameLength) {
		char * newBuf = (char *)mmAlloc(newbufsize * sizeof(char));

		memset(newBuf, 0, newbufsize * sizeof(char));

		if (sb->name != NULL) {
			strcpy(newBuf, sb->name);
			mmFree(sb->name);
		}

		sb->name = newBuf;
		sb->maxNameLength = newbufsize;
	} */

	/* printf("appendToStringBuilder() : ensureStringBuilderSize...\n"); */

	ensureStringBuilderSize(sb, oldStrLen + strlen(strToAppend) + 1);

	if (sb->name != NULL) {
		strcat(sb->name, strToAppend);
	}

	return sb;
}

STRING_BUILDER_TYPE * appendCharToStringBuilder(STRING_BUILDER_TYPE * sb, char c) {
	failIf(sb == NULL, "appendCharToStringBuilder() : sb == NULL");

	const int oldStrLen = (sb->name == NULL) ? 0 : strlen(sb->name);

	/* printf("appendCharToStringBuilder() : ensureStringBuilderSize...\n"); */

	ensureStringBuilderSize(sb, oldStrLen + 2);

	failIf(sb->name == NULL, "appendCharToStringBuilder() : sb->name == NULL");

	sb->name[oldStrLen] = c;
	sb->name[oldStrLen + 1] = '\0';

	return sb;

	/* char twoChars[2];

	twoChars[0] = c;
	twoChars[1] = '\0';

	return appendToStringBuilder(sb, twoChars); */
}

STRING_BUILDER_TYPE * appendCharsToStringBuilder(STRING_BUILDER_TYPE * sb, char * src, int numChars) {
	/* char * buf = (char *)mmAlloc((numChars + 1) * sizeof(char));

	memcpy(buf, src, numChars * sizeof(char));
	buf[numChars] = '\0';

	appendToStringBuilder(sb, buf);

	mmFree(buf); */

	failIf(sb == NULL, "appendCharsToStringBuilder() : sb == NULL");
	failIf(src == NULL, "appendCharsToStringBuilder() : src == NULL");

	const int oldStrLen = (sb->name == NULL) ? 0 : strlen(sb->name);

	/* printf("appendCharsToStringBuilder() : ensureStringBuilderSize...\n"); */

	ensureStringBuilderSize(sb, oldStrLen + numChars + 1);

	failIf(sb->name == NULL, "appendCharToStringBuilder() : sb->name == NULL");

	/* printf("appendCharsToStringBuilder() : memcpy...\n"); */

	memcpy(sb->name + oldStrLen, src, numChars * sizeof(char));
	sb->name[oldStrLen + numChars] = '\0';

	return sb;
}

/* **** The End **** */
