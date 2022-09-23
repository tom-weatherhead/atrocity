/* atrocity/src/string-builder.c */

#include <stdlib.h>
#include <string.h>

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

/* TODO: Use this in parser.c : */
BOOL stringInBuilderIs(STRING_BUILDER_TYPE * sb, char * str) {
	return sb != NULL && str != NULL && sb->name != NULL && !strcmp(sb->name, str);
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

/* TODO: Use this in input-output.c:
STRING_BUILDER_TYPE * appendLineFromFileToStringBuilder(STRING_BUILDER_TYPE * sb, FILE * file) {

	if (sb == NULL) {
		sb = createStringBuilder(0);
	}

	for (;;) {
		const int cn = fgetc(fp);

		if (cn == EOF) {
			break;
		}

		const char c = (char)cn;

		if (c == '\n') {
			break;
		}

		appendCharToStringBuilder(sb, c);
	}

	return sb;
} */

/* **** The End **** */
