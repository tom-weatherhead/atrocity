/* atrocity/src/associative-array.c */

#include <stdlib.h>
/* #include <stdio.h> */
#include <string.h>
/* #include <ctype.h> */
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

static BOOL areKeysEqual(LISP_VALUE * key1, LISP_VALUE * key2) {

	if (key1->type != key2->type) {
		return FALSE;
	}

	switch (key1->type) {
		case lispValueType_Number:
			return getIntegerValueInValue(key1) == getIntegerValueInValue(key2);

		case lispValueType_String:
			return !strcmp(getNameInValue(key1), getNameInValue(key2));

		default:
			fatalError("areKeysEqual() : Key type is not comparable");
			return FALSE;
	}
}

LISP_VALUE * aaCreate() {
	const int numBuckets = 256;

	return createAssociativeArray(numBuckets);
}

/* BOOL aaHas(LISP_VALUE * key); */

LISP_VALUE * aaGet(LISP_VALUE * aa, LISP_VALUE * key) {
	int hashValue = hashKey(key) % getIntegerValueInValue(aa);
	SCHEME_UNIVERSAL_TYPE * bucketPtr = ((SCHEME_UNIVERSAL_TYPE **)(aa->aux))[hashValue];

	while (bucketPtr != NULL) {

		if (areKeysEqual(key, getKeyInAssociativeArrayListElement(bucketPtr))) {
			return getValueInAssociativeArrayListElement(bucketPtr);
		}

		bucketPtr = bucketPtr->next;
	}

	return NULL;
}

LISP_VALUE * aaSet(LISP_VALUE * aa, LISP_VALUE * key, LISP_VALUE * value) {
	/* Update value if key is found; else insert */
	int hashValue = hashKey(key) % getIntegerValueInValue(aa);
	SCHEME_UNIVERSAL_TYPE ** buckets = (SCHEME_UNIVERSAL_TYPE **)(aa->aux);
	SCHEME_UNIVERSAL_TYPE * bucketPtr = buckets[hashValue];

	/* 1) Update value if key is found */

	while (bucketPtr != NULL) {

		if (areKeysEqual(key, getKeyInAssociativeArrayListElement(bucketPtr))) {
			getValueInAssociativeArrayListElement(bucketPtr) = value;

			return value;
		}

		bucketPtr = bucketPtr->next;
	}

	/* 2) Insert */
	buckets[hashValue] = createAssociativeArrayListElement(key, value, buckets[hashValue]);

	return value;
}

/* TODO: LISP_VALUE * resizeAA(LISP_VALUE * aa, int newNumBuckets); */

/* **** The End **** */
