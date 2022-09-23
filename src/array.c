/* atrocity/src/array.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

/* #include "array.h" */
#include "create-and-destroy.h"
#include "evaluate.h"
#include "memory-manager.h"

/* Arrays (ordered sequences of values) : */

/* TODO: "aslice", "a...", */

LISP_VALUE * getArrayLength(LISP_VALUE * array) {
	failIf(array == NULL, "getArrayLength() : array == NULL");
	failIf(array->type != lispValueType_Array, "getArrayLength() : array->type != lispValueType_Array");

	int length = 0;
	SCHEME_UNIVERSAL_TYPE * ptr = getHeadInArray(array);

	while (ptr != NULL) {
		++length;
		ptr = ptr->next;
	}

	return createNumericValue(length);
}

LISP_VALUE * push(LISP_VALUE * array, LISP_VALUE * value) {
	/* Append value to the end of the array */
	failIf(array == NULL, "push() : array == NULL");
	failIf(value == NULL, "push() : value == NULL");
	failIf(array->type != lispValueType_Array, "push() : array->type != lispValueType_Array");

	SCHEME_UNIVERSAL_TYPE * ptr = getHeadInArray(array);

	if (ptr == NULL) {
		getHeadInArray(array) = createArrayListElement(value, NULL);
	} else {

		while (ptr->next != NULL) {
			ptr = ptr->next;
		}

		ptr->next = createArrayListElement(value, NULL);
	}

	return value;
}

LISP_VALUE * pop(LISP_VALUE * array) {
	/* Remove value from the end of the array */
	failIf(array == NULL, "pop() : array == NULL");
	failIf(array->type != lispValueType_Array, "pop() : array->type != lispValueType_Array");

	SCHEME_UNIVERSAL_TYPE * ptr = getHeadInArray(array);

	failIf(ptr == NULL, "pop() : array is empty");

	if (ptr->next == NULL) {
		/* The array contains exactly one element */
		getHeadInArray(array) = NULL;
	} else {
		SCHEME_UNIVERSAL_TYPE * prevPtr = ptr;

		ptr = ptr->next;

		while (ptr->next != NULL) {
			prevPtr = ptr;
			ptr = ptr->next;
		}

		prevPtr->next = NULL;
	}

	return getValueInArrayListElement(ptr);
}

LISP_VALUE * peek(LISP_VALUE * array) {
	/* Return value at the end of the array */
	failIf(array == NULL, "peek() : array == NULL");
	failIf(array->type != lispValueType_Array, "peek() : array->type != lispValueType_Array");

	SCHEME_UNIVERSAL_TYPE * ptr = getHeadInArray(array);

	failIf(ptr == NULL, "peek() : array is empty");

	while (ptr->next != NULL) {
		ptr = ptr->next;
	}

	return getValueInArrayListElement(ptr);
}

LISP_VALUE * unshiftArray(LISP_VALUE * array, LISP_VALUE * value) {
	/* Prepend value to the beginning of the array */
	failIf(array == NULL, "unshiftArray() : array == NULL");
	failIf(value == NULL, "unshiftArray() : value == NULL");
	failIf(array->type != lispValueType_Array, "unshiftArray() : array->type != lispValueType_Array");

	getHeadInArray(array) = createArrayListElement(value, getHeadInArray(array));

	return value;
}

LISP_VALUE * shiftArray(LISP_VALUE * array) {
	/* Remove value from the beginning of the array */
	failIf(array == NULL, "shiftArray() : array == NULL");
	failIf(array->type != lispValueType_Array, "shiftArray() : array->type != lispValueType_Array");

	SCHEME_UNIVERSAL_TYPE * ptr = getHeadInArray(array);

	failIf(ptr == NULL, "shiftArray() : array is empty");

	getHeadInArray(array) = ptr->next;

	return getValueInArrayListElement(ptr);
}

static SCHEME_UNIVERSAL_TYPE * createShallowCopyOfValueList(SCHEME_UNIVERSAL_TYPE * srcPtr, int n) {

	if (srcPtr == NULL || n <= 0) {
		return NULL;
	} else {
		/* Create a shallow copy */
		LISP_VALUE * value = getValueInArrayListElement(srcPtr);
		SCHEME_UNIVERSAL_TYPE * next = createShallowCopyOfValueList(srcPtr->next, n - 1);

		return createArrayListElement(value, next);
	}
}

LISP_VALUE * sliceArray(LISP_VALUE * array, LISP_VALUE * start, LISP_VALUE * end) {
	/* From https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice :

	The slice() method returns a shallow copy of a portion of an array into a new array object selected from start to end (end not included) where start and end represent the index of items in that array. The original array will not be modified.
	*/

	failIf(array == NULL, "sliceArray() : array == NULL");
	failIf(array->type != lispValueType_Array, "sliceArray() : array->type != lispValueType_Array");

	failIf(start == NULL, "sliceArray() : start == NULL");
	failIf(start->type != lispValueType_Number, "sliceArray() : start->type != lispValueType_Number");
	failIf(getIntegerValueInValue(start) < 0, "sliceArray() : start value < 0");

	failIf(end == NULL, "sliceArray() : end == NULL");
	failIf(end->type != lispValueType_Number, "sliceArray() : end->type != lispValueType_Number");
	failIf(getIntegerValueInValue(end) < 0, "sliceArray() : end value < 0");

	/* I.e. Skip ${start} elements, then take ${end - start} elements */

	SCHEME_UNIVERSAL_TYPE * srcPtr = getHeadInArray(array);
	int n = getIntegerValueInValue(start);

	while (n > 0 && srcPtr != NULL) {
		srcPtr = srcPtr->next;
		--n;
	}

	LISP_VALUE * result = createArray();

	getHeadInArray(result) = createShallowCopyOfValueList(srcPtr, getIntegerValueInValue(end) - getIntegerValueInValue(start));

	return result;
}

/* **** The End **** */
