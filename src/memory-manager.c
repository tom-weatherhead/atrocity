/* atrocity/src/memory-manager.c */

/* A mark-and-sweep garbage collector. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"

static int numMallocs = 0;
/*static int numReallocs = 0; */
static int numFrees = 0;

void * mmAlloc(int numBytes) {
	failIf(numBytes <= 0, "mmAlloc() : numBytes <= 0");

	void * result = malloc(numBytes);

	failIf(result == NULL, "mmAlloc() : malloc() returned NULL");

	++numMallocs;
	memset(result, 0, numBytes);

	return result;
}

/* void * mmRealloc(void * ptr, int numBytes) {
	failIf(ptr == NULL, "mmRealloc() : ptr is NULL");

	void * result = realloc(ptr, numBytes);

	failIf(result == NULL, "mmRealloc() : realloc() returned NULL");

	++numReallocs;

	return result;
} */

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
	/* printf("  Number of reallocs: %d\n", numReallocs); */
	printf("  Number of frees:    %d\n\n", numFrees);

	if (numFrees < numMallocs) {
		printf("  Number of leaks:    %d\n\n", numMallocs - numFrees);
	}
}

/* **** BEGIN Memory manager version 1 **** */

typedef struct MEMMGR_RECORD_STRUCT {
	SCHEME_UNIVERSAL_TYPE * item;
	struct MEMMGR_RECORD_STRUCT * next;
} MEMMGR_RECORD;

static MEMMGR_RECORD * memmgrRecords = NULL;

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

	if (expr == NULL) {
		return;
	}

	/* printf("expr is %ld\n", expr); */

	if (expr->mark == 1) {
		return;
	} else if (expr->mark != 0) {
		printf("mm error: expr->mark is %d\n", expr->mark);
		exit(1);
	}

	/* Do this recursively */

	/* if (expr->type == schemeStructType_NameValueListElement) {
		printf("Marking... ");
	} */

	expr->mark = 1;

	/* if (expr->type == schemeStructType_NameValueListElement) {
		printf("Done.\n");
	}

	if (expr->value1 != NULL && expr->value1->type == schemeStructType_NameValueListElement) {
		printf("This type is %d; value1's type is %d\n", expr->type, expr->value1->type);
	}

	if (expr->value2 != NULL && expr->value2->type == schemeStructType_NameValueListElement) {
		printf("This type is %d; value2's type is %d\n", expr->type, expr->value2->type);
	}

	if (expr->value3 != NULL && expr->value3->type == schemeStructType_NameValueListElement) {
		printf("This type is %d; value3's type is %d\n", expr->type, expr->value3->type);
	} */

	if (expr->value1 == expr) {
		fprintf(stderr, "CIRCULAR: expr->value1 == expr; type is %d\n", expr->type);
		exit(1);
	} else if (expr->value2 == expr) {
		fprintf(stderr, "CIRCULAR: expr->value2 == expr; type is %d\n", expr->type);
		exit(1);
	} else if (expr->value3 == expr) {
		fprintf(stderr, "CIRCULAR: expr->value3 == expr; type is %d\n", expr->type);
		exit(1);
	} else if (expr->next == expr) {
		fprintf(stderr, "CIRCULAR: expr->next == expr; type is %d\n", expr->type);
		exit(1);
	}

	setMarksInExprTree(expr->value1);
	setMarksInExprTree(expr->value2);
	setMarksInExprTree(expr->value3);
	setMarksInExprTree(expr->next);

	/* if (expr->value1 != NULL) {
		setMarksInExprTree(expr->value1);
	}

	if (expr->value2 != NULL) {
		setMarksInExprTree(expr->value2);
	}

	if (expr->value3 != NULL) {
		setMarksInExprTree(expr->value3);
	} */

	/* if (expr->next != NULL) {
		/ * printf("Marking next in struct of type %d\n", expr->type);
		printf("  next's type is %d\n", expr->next->type);

		if (expr->type != schemeStructType_NameValueListElement) { * /
			setMarksInExprTree(expr->next);
		/ * } * /
	} */
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

	printf("Completed marking.\n");

	return freeUnmarkedStructs();
	/* return 0; */
}

void freeAllStructs() {
	clearMarks();
	freeUnmarkedStructs();
}

/* **** END Memory manager version 1 **** */

/* **** The End **** */
