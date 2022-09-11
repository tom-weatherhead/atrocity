/* atrocity/src/print.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "memory-manager.h"
#include "string-builder.h"
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

/* BOOL printValueToString(LISP_VALUE * value, char * buf, int bufsize) { */
STRING_BUILDER_TYPE * printValueToString(STRING_BUILDER_TYPE * sb, LISP_VALUE * value) {
	/* Returns FALSE iff there is no more room to print in buf. */
	/* TODO: Use a StringBuilder */

	/* (?) It is assumed that the caller will zero-fill buf before calling this function. Or else:
	memset(buf, 0, bufsize * sizeof(char)); */

	if (sb == NULL) {
		sb = createStringBuilder(0);
	}

	if (isList(value) && value->type != lispValueType_Null) {
		char * separator = "";

		appendToStringBuilder(sb, "(");

		while (value->type != lispValueType_Null) {
			appendToStringBuilder(sb, separator);

			printValueToString(sb, getHeadInPair(getPairInValue(value)));

			separator = " ";
			value = getTailInPair(getPairInValue(value));
		}

		appendToStringBuilder(sb, ")");

		return sb;
	} else if (value->type == lispValueType_Pair) {
		appendToStringBuilder(sb, "(");
		printValueToString(sb, getHeadInPair(getPairInValue(value)));
		appendToStringBuilder(sb, " . ");
		printValueToString(sb, getTailInPair(getPairInValue(value)));
		appendToStringBuilder(sb, ")");

		return sb;
	}

	char * strContinuation = "<contin>";
	char * strContinuationReturn = "<contRtn>";
	char * strClosure = "<closure>";
	char * strInvalid = "<invalid value>";
	/* const int maxPrintedIntegerLength = 10;
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
		*buf = '\0'; / * Null-terminate the string * /
		return sb; / * FALSE; * /
	} */

	/* (listtostring '("abc" 123 "def")) -> TODO: BUG: Double quotes are not removed from string literals inside a (single-)quoted list */
	char intSprintfBuffer[16];

	switch (value->type) {
		case lispValueType_Number:
			memset(intSprintfBuffer, 0, 16 * sizeof(char));
			sprintf(intSprintfBuffer, "%d", getIntegerValueInValue(value));
			appendToStringBuilder(sb, intSprintfBuffer);
			break;

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			appendToStringBuilder(sb, getNameInValue(value));
			break;

		case lispValueType_Closure:
			appendToStringBuilder(sb, strClosure);
			break;

		case lispValueType_Null:
			appendToStringBuilder(sb, "()");
			break;

		case lispPseudoValueType_Continuation:
			appendToStringBuilder(sb, strContinuation);
			break;

		case lispPseudoValueType_ContinuationReturn:
			appendToStringBuilder(sb, strContinuationReturn);
			break;

		default:
			appendToStringBuilder(sb, strInvalid);
			break;
	}

	return sb;
}

/* **** The End **** */
