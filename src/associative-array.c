/* atrocity/src/associative-array.c */

#include <stdlib.h>
/* #include <stdio.h>
#include <string.h>
/ * #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"

static int hashString(char * str) {
	int result = 0;

	while (str != NULL) {
		result *= 13 * (int)*str;
		result += 37;
	}

	return result;
}

static int hashKey(LISP_VALUE * key) {

	switch (key->type) {
		case lispValueType_Number:
			return getIntegerValueInValue(key);

		case lispValueType_String:
			return hashString(getNameInValue(key));

		default:
			fatalError("hashKey() : Key type is not hashable");
			return 0;
	}
}

LISP_VALUE * aaCreate() {
	/* int numBuckets = 256;
	LISP_VALUE ** buckets = (LISP_VALUE **)malloc(numBuckets * sizeof(LISP_VALUE *));

	memset(buckets, 0, numBuckets * sizeof(LISP_VALUE *)); */

	LISP_VALUE * result = createAssociativeArray();

	/* result->aux = buckets; */ /* typeof aux is void * */

	return result;
}

/* BOOL aaHas(LISP_VALUE * key); */

LISP_VALUE * aaGet(LISP_VALUE * key) {
	return NULL;
}

void aaSet(LISP_VALUE * key, LISP_VALUE * value) {
	return;
}

/* **** The End **** */
