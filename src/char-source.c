/* atrocity/src/char-source.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "char-source.h"
#include "create-and-destroy.h"
#include "memory-manager.h"
#include "string-builder.h"
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

STRING_BUILDER_TYPE * getIdentifier(CharSource * cs, STRING_BUILDER_TYPE * sb, BOOL * pIsSingleQuoted) {

	if (sb == NULL) {
		sb = createStringBuilder(0);
	}

	skipWhiteSpace(cs);

	if (isEOF(cs)) {
		return sb;
	}

	const char firstChar = cs->str[cs->i];
	BOOL isSingleQuoted = FALSE;

	switch (firstChar) {
		case '(':
		case ')':
		/* case '\'': */
			/* printf("getIdentifier() : ( or ) : appendCharToStringBuilder...\n"); */
			appendCharToStringBuilder(sb, firstChar);
			cs->i++;
			return sb;

		case '\'':
			isSingleQuoted = TRUE;

			if (pIsSingleQuoted == NULL) {
				fatalError("getIdentifier() found a single quote, but cannot notify caller");
			}

			*pIsSingleQuoted = TRUE;
			/* printf("getIdentifier() : ' : appendCharToStringBuilder...\n"); */
			appendCharToStringBuilder(sb, firstChar);
			cs->i++;
			return sb;

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
	/* const int lenToCopy = (dstBufSize - 1 < len) ? dstBufSize - 1 : len; */

	/* memcpy(dstBuf, &cs->str[start], lenToCopy); */
	/* Or: memcpy(dstBuf, cs->str + start, lenToCopy); */

	return appendCharsToStringBuilder(sb, cs->str + start, len);
}

BOOL consumeStr(CharSource * cs, char * str) {
	STRING_BUILDER_TYPE * sb = getIdentifier(cs, NULL, NULL);

	if (isStringBuilderEmpty(sb)) {
		fprintf(stderr, "consumeStr() : Error : Expected '%s', found EOF\n", str);
		fatalError("consumeStr() 1");
	} else if (strcmp(sb->name, str)) {
		fprintf(stderr, "consumeStr() : Error : Expected '%s', found '%s'\n", str, sb->name);
		fatalError("consumeStr() 2");
	}

	return TRUE;
}

/* **** The End **** */
