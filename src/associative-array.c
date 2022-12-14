/* atrocity/src/associative-array.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "associative-array.h"
#include "create-and-destroy.h"
#include "evaluate.h"
#include "memory-manager.h"

/* For associative arrays (dictionaries) : */

/* External constants / variables */

extern LISP_VALUE * globalNullValue;

/* Local constants */

static const int maxNumItemsInAnyBucket = 16;

/* Functions */

static int hashString(char * str) {
	int result = 0;

	while (*str != '\0') {
		result *= 13 * (int)*str++;
		result += 37;
	}

	return result;
}

static int hashKey(LISP_VALUE * key) {

	switch (key->type) {
		case lispValueType_Number:
			return getIntegerValueInValue(key);

		case lispValueType_PrimitiveOperator:
		case lispValueType_String:
		case lispValueType_Symbol:
			return hashString(getNameInValue(key));

		case lispValueType_Pair:
			return hashKey(getHeadInPair(key)) * 37 + hashKey(getTailInPair(key)) + 17;

		case lispValueType_AssociativeArray:
		case lispValueType_Closure:
		case lispValueType_Null:
		case lispPseudoValueType_Continuation:
		case lispPseudoValueType_ContinuationReturn:
			return 0;

		default:
			fatalError("hashKey() : Key type is not hashable");
			return 0;
	}
}

LISP_VALUE * createAssociativeArray() {
	const int numBuckets = 256;

	return createAssociativeArrayEx(numBuckets);
}

static void resizeAA(LISP_VALUE * aa, int newNumBuckets) {
	printf("Resizing associative array...\n");

	SCHEME_UNIVERSAL_TYPE ** oldAux = (SCHEME_UNIVERSAL_TYPE **)(aa->aux);
	int oldNumBuckets = getNumBucketsInAssociativeArray(aa);

	getNumBucketsInAssociativeArray(aa) = newNumBuckets;
	aa->aux = (SCHEME_UNIVERSAL_TYPE **)mmAlloc(newNumBuckets * sizeof(SCHEME_UNIVERSAL_TYPE *));

	memset(aa->aux, 0, newNumBuckets * sizeof(SCHEME_UNIVERSAL_TYPE *));

	while (oldNumBuckets > 0) {
		SCHEME_UNIVERSAL_TYPE * bucketPtr = oldAux[oldNumBuckets--];

		while (bucketPtr != NULL) {
			/* TODO: Prevent this aaSet call from causing a resize within a resize */
			aaSet(aa, getKeyInAssociativeArrayListElement(bucketPtr), getValueInAssociativeArrayListElement(bucketPtr));
			bucketPtr = bucketPtr->next;
		}
	}

	printf("Done resizing associative array\n");
}

/* BOOL aaHas(LISP_VALUE * aa, LISP_VALUE * key) {
	return aaGet(aa, key) != NULL;
} */

LISP_VALUE * aaGet(LISP_VALUE * aa, LISP_VALUE * key) {
	const int hashValue = hashKey(key) % getNumBucketsInAssociativeArray(aa);
	SCHEME_UNIVERSAL_TYPE * bucketPtr = ((SCHEME_UNIVERSAL_TYPE **)(aa->aux))[hashValue];

	while (bucketPtr != NULL) {

		if (areValuesEqual(key, getKeyInAssociativeArrayListElement(bucketPtr))) {
			return getValueInAssociativeArrayListElement(bucketPtr);
		}

		bucketPtr = bucketPtr->next;
	}

	return globalNullValue;
}

LISP_VALUE * aaSet(LISP_VALUE * aa, LISP_VALUE * key, LISP_VALUE * value) {
	/* Update value if key is found; else insert */
	const int hashValue = hashKey(key) % getNumBucketsInAssociativeArray(aa);
	SCHEME_UNIVERSAL_TYPE ** buckets = (SCHEME_UNIVERSAL_TYPE **)(aa->aux);
	SCHEME_UNIVERSAL_TYPE * bucketPtr = buckets[hashValue];
	int numItemsInBucket = 0;

	/* 1) Update value if key is found */

	while (bucketPtr != NULL) {

		if (areValuesEqual(key, getKeyInAssociativeArrayListElement(bucketPtr))) {
			getValueInAssociativeArrayListElement(bucketPtr) = value;

			return value;
		}

		bucketPtr = bucketPtr->next;
		numItemsInBucket++;
	}

	/* 2) Insert */
	buckets[hashValue] = createAssociativeArrayListElement(key, value, buckets[hashValue]);

	if (numItemsInBucket > maxNumItemsInAnyBucket) {
		resizeAA(aa, 2 * getNumBucketsInAssociativeArray(aa));
	}

	/* If the number of items in this bucket becomes too large,
	then resize the AssociativeArray, which will rehash all keys */

	return value;
}

LISP_VALUE * aaSize(LISP_VALUE * aa) {
	const int numBuckets = getNumBucketsInAssociativeArray(aa);
	SCHEME_UNIVERSAL_TYPE ** buckets = (SCHEME_UNIVERSAL_TYPE **)(aa->aux);
	int i;
	int result = 0;


	for (i = 0; i < numBuckets; ++i) {
		SCHEME_UNIVERSAL_TYPE * bucketPtr;

		for (bucketPtr = buckets[i]; bucketPtr != NULL; bucketPtr = bucketPtr->next) {
			++result;
		}
	}

	return createNumericValue(result);
}

LISP_VALUE * aaDeleteKey(LISP_VALUE * aa, LISP_VALUE * key) {
	const int hashValue = hashKey(key) % getNumBucketsInAssociativeArray(aa);
	SCHEME_UNIVERSAL_TYPE ** buckets = (SCHEME_UNIVERSAL_TYPE **)(aa->aux);
	SCHEME_UNIVERSAL_TYPE * bucketPtr = buckets[hashValue];
	SCHEME_UNIVERSAL_TYPE * prevPtr = NULL;

	for (; bucketPtr != NULL; bucketPtr = bucketPtr->next) {

		if (areValuesEqual(key, getKeyInAssociativeArrayListElement(bucketPtr))) {
			break;
		}

		prevPtr = bucketPtr;
	}

	if (bucketPtr == NULL) {
		/* The key was not found. */
		return globalNullValue;
	}

	if (prevPtr == NULL) {
		buckets[hashValue] = bucketPtr->next;
	} else {
		prevPtr->next = bucketPtr->next;
	}

	return getValueInAssociativeArrayListElement(bucketPtr);
}

/* **** The End **** */
