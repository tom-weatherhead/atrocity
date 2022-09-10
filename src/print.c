/* atrocity/src/print.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "memory-manager.h"
#include "utilities.h"

void printValue(LISP_VALUE * value) {

	if (value == NULL) {
		printf("NULL");

		return;
	} else if (isList(value) /* && value->type != lispValueType_Null */) {
		char separator = '\0';

		printf("(");

		while (value->type != lispValueType_Null) {
			printf("%c", separator);
			printValue(getHeadInPair(getPairInValue(value)));
			separator = ' ';
			value = getTailInPair(getPairInValue(value));
		}

		printf(")");

		return;
	}

	switch (value->type) {
		case lispValueType_Number:
			printf("%d", getIntegerValueInValue(value));
			break;

		case lispValueType_String:
			printf("\"%s\"", getNameInValue(value));
			break;

		case lispValueType_Symbol:
			printf("'%s", getNameInValue(value));
			break;

		case lispValueType_PrimitiveOperator:
			printf("%s", getNameInValue(value));
			break;

		case lispValueType_Closure:
			printf("<closure>");
			break;

		case lispValueType_Pair:
			printf("Pair: (");
			printValue(getHeadInPair(getPairInValue(value)));
			printf(" . ");
			printValue(getTailInPair(getPairInValue(value)));
			printf(")");
			break;

		/* case lispValueType_Null:
			printf("()");
			break; */

		case lispPseudoValueType_Continuation:
			printf("<continuation; id %d>", getContinuationIdInValue(value));
			break;

		case lispPseudoValueType_ContinuationReturn:
			printf("<continuation return; id %d, value: ", getContinuationIdInValue(value));
			printValue(getContinuationReturnValueInValue(value));
			printf(">");
			break;

		default:
			printf("<invalid value>");
			break;
	}
}

/* STRING_BUILDER_TYPE * printValueToString(STRING_BUILDER_TYPE * sb, LISP_VALUE * value) {} */
BOOL printValueToString(LISP_VALUE * value, char * buf, int bufsize) {
	/* Returns FALSE iff there is no more room to print in buf. */
	/* TODO: Use a StringBuilder */

	/* (?) It is assumed that the caller will zero-fill buf before calling this function. Or else:
	memset(buf, 0, bufsize * sizeof(char)); */

	if (isList(value) && value->type != lispValueType_Null) {
		char separator = '\0';
		int separatorLen = 0;

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf++ = '(';
		--bufsize;

		while (value->type != lispValueType_Null) {

			if (bufsize <= separatorLen) {
				*buf = '\0'; /* Null-terminate the string */
				return FALSE;
			}

			sprintf(buf, "%c", separator);
			buf += separatorLen;
			bufsize -= separatorLen;

			if (!printValueToString(getHeadInPair(getPairInValue(value)), buf, bufsize)) {
				return FALSE;
			}

			const int len = strlen(buf);

			buf += len;
			bufsize -= len;

			separator = ' ';
			separatorLen = 1;
			value = getTailInPair(getPairInValue(value));
		}

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf = ')';

		return TRUE;
	} else if (value->type == lispValueType_Pair) {

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf++ = '(';
		--bufsize;

		if (!printValueToString(getHeadInPair(getPairInValue(value)), buf, bufsize)) {
			return FALSE;
		}

		int len = strlen(buf);

		buf += len;
		bufsize -= len;

		if (bufsize <= 3) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf++ = ' ';
		*buf++ = '.';
		*buf++ = ' ';
		bufsize -= 3;

		if (!printValueToString(getTailInPair(getPairInValue(value)), buf, bufsize)) {
			return FALSE;
		}

		len = strlen(buf);

		buf += len;
		bufsize -= len;

		if (bufsize <= 1) {
			*buf = '\0'; /* Null-terminate the string */
			return FALSE;
		}

		*buf = ')';

		return TRUE;
	}

	char * strContinuation = "<contin>";
	char * strContinuationReturn = "<contRtn>";
	char * strClosure = "<closure>";
	char * strInvalid = "<invalid value>";
	const int maxPrintedIntegerLength = 10;
	int lenToAppend = 0;

	switch (value->type) {
		case lispValueType_Number:
			lenToAppend = maxPrintedIntegerLength;
			break;

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			lenToAppend = strlen(value->name);
			break;

		case lispValueType_Closure:
			lenToAppend = strlen(strClosure);
			break;

		case lispValueType_Null:
			lenToAppend = 2;
			break;

		case lispPseudoValueType_Continuation:
			lenToAppend = strlen(strContinuation);
			break;

		case lispPseudoValueType_ContinuationReturn:
			lenToAppend = strlen(strContinuationReturn);
			break;

		default:
			lenToAppend = strlen(strInvalid);
			break;
	}

	if (bufsize <= lenToAppend) {
		printf("Returning FALSE");
		*buf = '\0'; /* Null-terminate the string */
		return FALSE;
	}

	/* (listtostring '("abc" 123 "def")) -> TODO: BUG: Double quotes are not removed from string literals inside a (single-)quoted list */

	switch (value->type) {
		case lispValueType_Number:
			sprintf(buf, "%d", getIntegerValueInValue(value));
			break;

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			strcpy(buf, getNameInValue(value));
			break;

		case lispValueType_Closure:
			strcpy(buf, strClosure);
			break;

		case lispValueType_Null:
			strcpy(buf, "()");
			break;

		case lispPseudoValueType_Continuation:
			strcpy(buf, strContinuation);
			break;

		case lispPseudoValueType_ContinuationReturn:
			strcpy(buf, strContinuationReturn);
			break;

		default:
			strcpy(buf, strInvalid);
			break;
	}

	return TRUE;
}

/* **** The End **** */
