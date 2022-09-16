/* atrocity/src/utilities.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "thunk.h"

/* Functions */

BOOL isStringAllWhitespace(char * str) {
	const int len = strlen(str);
	int i;

	for (i = 0; i < len; ++i) {

		/* if (isspace(str[i])) {} ? (after #include <ctype.h>) */
		if (str[i] != ' ' && str[i] != '\t' && str[i] != '\n') {
			return FALSE;
		}
	}

	return TRUE;
}

BOOL safeAtoi(char * str, int * ptrToInt) {
	const int len = strlen(str);
	int i = 0;

	if (len > 0 && str[0] == '-') {
		i = 1; /* The - may be a minus sign */
	}

	if (i == len) {
		return FALSE; /* str is just "" or "-" */
	}

	for (; i < len; ++i) {

		if (str[i] < '0' || str[i] > '9') {
			return FALSE;
		}
	}

	*ptrToInt = atoi(str);

	return TRUE;
}

char * fgets_wrapper(char * buffer, size_t buflen, FILE * fp) {
	/* From https://stackoverflow.com/questions/1694036/why-is-the-gets-function-so-dangerous-that-it-should-not-be-used */

	if (fgets(buffer, buflen, fp) != 0) {
		size_t len = strlen(buffer);

		if (len > 0 && buffer[len - 1] == '\n') {
			buffer[len - 1] = '\0';
		}

		return buffer;
	}

	return 0;
}

BOOL isStringInList(char * str, char * list[]) {
	int i;

	for (i = 0; list[i] != NULL; ++i) {

		if (!strcmp(str, list[i])) {
			return TRUE;
		}
	}

	return FALSE;
}

void fail(char * str, char * file, int line) {
	fprintf(stderr, "\nFatal error '%s' in file %s at line %d\n", str, file, line);
	exit(1);
}

/* Misc DOM functions */

/* BOOL isValueCallable(LISP_VALUE * value) {
	return value->type == lispValueType_PrimitiveOperator || value->type == lispValueType_Closure;
} */

BOOL isList(LISP_VALUE * value) {
	/* printf("isList() : Before dethunk : value is %ld\n", value); */

	value = dethunk(value);

	/* printf("isList() : After dethunk : value is %ld\n", value); */

	switch (value->type) {
		case lispValueType_Null:
			return TRUE;

		case lispValueType_Pair:
			failIf(getTailInPair(value) == value, "isList() : tail == self");
			return isList(getTailInPair(value));

		default:
			break;
	}

	return FALSE;
}

/* **** The End **** */
