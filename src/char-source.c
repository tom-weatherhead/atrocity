/* atrocity/src/char-source.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "char-source.h"
#include "memory-manager.h"
#include "utilities.h"

/* **** CharSource functions **** */

CharSource * createCharSource(char * str) {
	CharSource * cs = (CharSource *)mmAlloc(sizeof(CharSource));

	if (cs == NULL) {
		fatalError("mmAlloc() failed in createCharSource()");
	}

	/* TODO? : Clone the string? */
	cs->str = str;

	cs->len = strlen(str);
	cs->i = 0;

	return cs;
}

void freeCharSource(CharSource * cs) {
	cs->str = NULL; /* Note bene: We don't call free() here */
	mmFree(cs);
}

static BOOL isWhiteSpace(char c) {
	return c == ' ' || c == '\t' || c == '\n' ? TRUE : FALSE;
}

int getNextChar(CharSource * cs) {

	while (cs->i < cs->len) {
		const char c = cs->str[cs->i];

		++cs->i;

		if (!isWhiteSpace(c)) {
			return (int)c;
		}
	}

	return EOF;
}

void rewindOneChar(CharSource * cs) {

	if (cs->i > 0) {
		--cs->i;
	}
}

static BOOL isEOF(CharSource * cs) {
	return cs->i >= cs->len;
}

static void skipWhiteSpace(CharSource * cs) {

	while (cs->i < cs->len && isWhiteSpace(cs->str[cs->i])) {
		++cs->i;
	}
}

/* TODO: Modify getIdentifier to create or populate a StringBuilder */

int getIdentifier(CharSource * cs, char * dstBuf, int dstBufSize, BOOL * pIsSingleQuoted) {
	memset(dstBuf, 0, dstBufSize);

	skipWhiteSpace(cs);

	if (isEOF(cs)) {
		return 0;
	}

	const char firstChar = cs->str[cs->i];
	BOOL isSingleQuoted = FALSE;

	switch (firstChar) {
		case '(':
		case ')':
		/* case '\'': */
			memcpy(dstBuf, cs->str + cs->i++, 1);
			return 1;

		case '\'':
			isSingleQuoted = TRUE;

			if (pIsSingleQuoted == NULL) {
				fatalError("getIdentifier() found a single quote, but cannot notify caller");
			}

			*pIsSingleQuoted = TRUE;
			memcpy(dstBuf, cs->str + cs->i++, 1);
			return 1;

		default:
			break;
	}

	/* failIf(isSingleQuoted && pIsSingleQuoted == NULL);

	if (pIsSingleQuoted == NULL) {
		*pIsSingleQuoted = isSingleQuoted;
	} */

	const BOOL isString = (cs->str[cs->i] == '"') ? TRUE : FALSE;
	BOOL isStringClosed = FALSE;

	const int start = cs->i;

	if (isString || isSingleQuoted) {
		++cs->i;
	}

	while (cs->i < cs->len) {
		const char c = cs->str[cs->i];

		if (isString) {

			if (c == '"') {
				isStringClosed = TRUE;
				++cs->i;
				break;
			}
		} else if (isWhiteSpace(c) || c == '(' || c == ')' /* || c == '\0' */) {
			break;
		}

		++cs->i;
	}

	if (isString && !isStringClosed) {
		fatalError("getIdentifier() : String opened but not closed");
		return 0;
	}

	const int end = cs->i;
	const int len = end - start;
	const int lenToCopy = (dstBufSize - 1 < len) ? dstBufSize - 1 : len;

	memcpy(dstBuf, &cs->str[start], lenToCopy);
	/* Or: memcpy(dstBuf, cs->str + start, lenToCopy); */

	return lenToCopy;
}

BOOL consumeStr(CharSource * cs, char * str) {
	/* Consume str */
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fprintf(stderr, "consumeStr() : Error : Expected '%s', found EOF\n", str);
		fatalError("consumeStr() 1");
	} else if (strcmp(dstBuf, str)) {
		fprintf(stderr, "consumeStr() : Error : Expected '%s', found '%s'\n", str, dstBuf);
		fatalError("consumeStr() 2");
	}

	return TRUE;
}

/* **** The End **** */
