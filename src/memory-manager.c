/* atrocity/src/memory-manager.c */

/* A mark-and-sweep garbage collector. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"

/* External constants / variables */

extern LISP_VALUE * globalNullValue;
extern LISP_VALUE * globalTrueValue;

/* Local constants / variables */

static int numMallocs = 0;
static int numFrees = 0;

typedef struct MEMMGR_RECORD_STRUCT {
	SCHEME_UNIVERSAL_TYPE * item;
	struct MEMMGR_RECORD_STRUCT * next;
} MEMMGR_RECORD;

static MEMMGR_RECORD * memmgrRecords = NULL;

void * mmAlloc(int numBytes) {
	failIf(numBytes <= 0, "mmAlloc() : numBytes <= 0");

	void * result = malloc(numBytes);

	failIf(result == NULL, "mmAlloc() : malloc() returned NULL");

	++numMallocs;
	memset(result, 0, numBytes);

	return result;
}

void mmFree(void * ptr) {
	failIf(ptr == NULL, "mmFree() : ptr is NULL");

	if (ptr != NULL) {
		free(ptr);
		++numFrees;
	}
}

void mmPrintReport() {
	printf("\nMemory manager report:\n");
	printf("  Number of mallocs:  %d\n", numMallocs);
	printf("  Number of frees:    %d\n\n", numFrees);

	if (numFrees < numMallocs) {
		printf("  Number of leaks:    %d\n\n", numMallocs - numFrees);
	}
}

void addItemToMemMgrRecords(SCHEME_UNIVERSAL_TYPE * item) {
	MEMMGR_RECORD * mmRec = NULL;

	/* for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {
		/ * TODO: Speed this search up by using a tree instead of a list. * /

		if (mmRec->item == item) {
			/ * The item is already in the list. * /
			fprintf(stderr, "addItemToMemMgrRecords() : Attempted duplication of item of type %d\n", item->type);
			return;
		}
	} */

	mmRec = (MEMMGR_RECORD *)mmAlloc(sizeof(MEMMGR_RECORD));

	mmRec->item = item;
	mmRec->next = memmgrRecords;
	memmgrRecords = mmRec;
}

int mmReplacePointer(SCHEME_UNIVERSAL_TYPE * old, SCHEME_UNIVERSAL_TYPE * new) {
	MEMMGR_RECORD * mmRec;
	int count = 0;

	for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {

		if (mmRec->item->value1 == old) {
			mmRec->item->value1 = new;
			++count;
		}

		if (mmRec->item->value2== old) {
			mmRec->item->value2 = new;
			++count;
		}

		if (mmRec->item->value3 == old) {
			mmRec->item->value3 = new;
			++count;
		}
	}

	return count;
}

/* int getNumMemMgrRecords() {
	int n = 0;
	MEMMGR_RECORD * mmRec;

	for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {
		++n;
	}

	return n;
} */

static void clearMarks() {
	MEMMGR_RECORD * mmRec;

	for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {
		mmRec->item->mark = 0;
	}
}

static void setMarksInExprTree(SCHEME_UNIVERSAL_TYPE * expr) {

	if (expr == NULL || expr->mark == 1) {
		return;
	} else if (expr->mark != 0) {
		printf("mm error: expr->mark is %d\n", expr->mark);
		exit(1);
	}

	/* Do this recursively */

	expr->mark = 1;

	setMarksInExprTree(expr->value1);
	setMarksInExprTree(expr->value2);
	setMarksInExprTree(expr->value3);
	setMarksInExprTree(expr->next);
}

static int freeUnmarkedStructs() {
	MEMMGR_RECORD ** ppmmRec = &memmgrRecords;
	MEMMGR_RECORD * mmRec = *ppmmRec;
	int numFreed = 0;

	while (mmRec != NULL) {

		if (mmRec->item->mark == 0) {
			/* Free mmRec->item. Do not free recursively.
			Allow mmRec->item->name to be freed. */
			mmRec->item->value1 = NULL;
			mmRec->item->value2 = NULL;
			mmRec->item->value3 = NULL;
			mmRec->item->next = NULL;
			freeUniversalStruct(mmRec->item);

			/* Then free mmRec, preserving the integrity of the linked list */
			MEMMGR_RECORD * nextmmRec = mmRec->next;

			mmRec->item = NULL;
			mmRec->next = NULL;
			mmFree(mmRec);
			++numFreed;
			*ppmmRec = nextmmRec;
		} else {
			ppmmRec = &mmRec->next;
		}

		mmRec = *ppmmRec;
	}

	return numFreed;
}

int collectGarbage(SCHEME_UNIVERSAL_TYPE * exprTreesToMark[]) {
	int i;

	clearMarks();

	for (i = 0; exprTreesToMark[i] != NULL; ++i) {
		setMarksInExprTree(exprTreesToMark[i]);
	}

	return freeUnmarkedStructs();
}

int freeAllStructs() {
	globalNullValue = NULL;
	globalTrueValue = NULL;
	clearMarks();

	return freeUnmarkedStructs();
}

static BOOL isCyclicalHelper(SCHEME_UNIVERSAL_TYPE * ptr) {

	if (ptr == NULL) {
		return FALSE;
	} else if (ptr->mark != 0) {
		printf("Mark found; object type %d\n", ptr->type);
		return TRUE;
	}

	ptr->mark = 1;

	/* return isCyclicalHelper(ptr->value1) || isCyclicalHelper(ptr->value2) || isCyclicalHelper(ptr->value3) || isCyclicalHelper(ptr->next); */

	if (isCyclicalHelper(ptr->value1)) {
		printf("  Object of type %d : value1 is cyclical\n", ptr->type);
		return TRUE;
	} else if (isCyclicalHelper(ptr->value2)) {
		printf("  Object of type %d : value2 is cyclical\n", ptr->type);
		return TRUE;
	} else if (isCyclicalHelper(ptr->value3)) {
		printf("  Object of type %d : value3 is cyclical\n", ptr->type);
		return TRUE;
	} else if (isCyclicalHelper(ptr->next)) {
		printf("  Object of type %d : next is cyclical\n", ptr->type);
		return TRUE;
	} else {
		ptr->mark = 0;
		return FALSE;
	}
}

BOOL isCyclical(SCHEME_UNIVERSAL_TYPE * ptr) {
	clearMarks();

	return isCyclicalHelper(ptr);
}

/* **** The End **** */
