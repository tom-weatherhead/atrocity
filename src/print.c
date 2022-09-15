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
#include "thunk.h"
#include "utilities.h"

void printValue(LISP_VALUE * value) {
	/* printf("printValue() : Value pointer is %ld\n", value);
	printf("printValue() : Value type is %d\n", value->type); */

	/* deepDethunk(value); Can cause seg fault */

	if (value == NULL) {
		printf("NULL");

		return;
	}

	value = dethunk(value);

	if (isList(value) /* && value->type != lispValueType_Null */) {
		/* printf("printValue() : Value isList\n"); */

		char separator = '\0';

		printf("(");

		while (value->type != lispValueType_Null) {
			/* printf("printValue() : value type is %d\n", value->type); */
			failIf(value->type != lispValueType_Pair, "printValue() : value type is not Pair");
			printf("%c", separator);
			failIf(getHeadInPair(value) == NULL, "printValue() : value head is NULL");
			printValue(getHeadInPair(value));
			separator = ' ';
			failIf(getTailInPair(value) == NULL, "printValue() : value tail is NULL");
			value = dethunk(getTailInPair(value));
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

			if (getNameInValue(value) == NULL) {
				fprintf(stderr, "**** Symbol %ld has a NULL name ****\n", value);
			}

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
			printValue(getHeadInPair(value));
			printf(" . ");
			printValue(getTailInPair(value));
			printf(")");
			break;

		/* case lispValueType_Null:
			printf("()");
			break; */

		case lispValueType_Thunk:
			printf("<thunk>");
			break;

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

/* TODO: Add params:
- char * separatorBetweenListItems
- BOOL printBracketsAroundList */
STRING_BUILDER_TYPE * printValueToString(STRING_BUILDER_TYPE * sb, LISP_VALUE * value, char * separatorBetweenListItems, BOOL printBracketsAroundList) {
	/* Returns FALSE iff there is no more room to print in buf. */
	/* TODO: Use a StringBuilder */

	/* (?) It is assumed that the caller will zero-fill buf before calling this function. Or else:
	memset(buf, 0, bufsize * sizeof(char)); */

	if (sb == NULL) {
		sb = createStringBuilder(0);
	}

	if (separatorBetweenListItems == NULL) {
		separatorBetweenListItems = " ";
	}

	if (isList(value) && value->type != lispValueType_Null) {
		char * separator = "";

		if (printBracketsAroundList) {
			appendToStringBuilder(sb, "(");
		}

		while (value->type != lispValueType_Null) {
			LISP_VALUE * head = dethunk(getHeadInPair(value));

			appendToStringBuilder(sb, separator);

			printValueToString(sb, head, separatorBetweenListItems, TRUE);

			separator = separatorBetweenListItems;
			value = dethunk(getTailInPair(value));
		}

		if (printBracketsAroundList) {
			appendToStringBuilder(sb, ")");
		}

		return sb;
	} else if (value->type == lispValueType_Pair) {
		appendToStringBuilder(sb, "(");
		printValueToString(sb, dethunk(getHeadInPair(value)), separatorBetweenListItems, printBracketsAroundList);
		appendToStringBuilder(sb, " . ");
		printValueToString(sb, dethunk(getTailInPair(value)), separatorBetweenListItems, printBracketsAroundList);
		appendToStringBuilder(sb, ")");

		return sb;
	}

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
			appendToStringBuilder(sb, "<closure>");
			break;

		case lispValueType_Null:
			appendToStringBuilder(sb, "()");
			break;

		case lispValueType_Thunk:
			appendToStringBuilder(sb, "<thunk>");
			break;

		case lispPseudoValueType_Continuation:
			appendToStringBuilder(sb, "<contin>");
			break;

		case lispPseudoValueType_ContinuationReturn:
			appendToStringBuilder(sb, "<contRtn>");
			break;

		default:
			appendToStringBuilder(sb, "<invalid value>");
			break;
	}

	return sb;
}

/* **** The End **** */
