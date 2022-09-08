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
static int numReallocs = 0;
static int numFrees = 0;

void * mmAlloc(int numBytes) {
	/* printf("mmAlloc() : Allocating %d bytes\n", numBytes); */

	failIf(numBytes <= 0, "mmAlloc() : numBytes <= 0");

	void * result = malloc(numBytes);

	failIf(result == NULL, "mmAlloc() : malloc() returned NULL");

	++numMallocs;
	memset(result, 0, numBytes);

	/* printf("mmAlloc() : Done.\n"); */

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
	/* printf("mmFree() : Begin.\n"); */

	failIf(ptr == NULL, "mmFree() : ptr is NULL");

	if (ptr != NULL) {
		free(ptr);
		++numFrees;
	} else {
		printf("mmFree() : ptr == NULL\n");
	}

	/* printf("mmFree() : Done.\n"); */
}

void mmPrintReport() {
	printf("\nMemory manager report:\n");
	printf("  Number of mallocs:  %d\n", numMallocs);
	printf("  Number of reallocs: %d\n", numReallocs);
	printf("  Number of frees:    %d\n\n", numFrees);

	if (numFrees < numMallocs) {
		printf("  Number of leaks:    %d\n\n", numMallocs - numFrees);
	}
}

/* **** BEGIN Memory manager version 1 **** */

typedef struct MEMMGR_RECORD_STRUCT {
	SCHEME_UNIVERSAL_TYPE * expr;
	struct MEMMGR_RECORD_STRUCT * next;
} MEMMGR_RECORD;

static MEMMGR_RECORD * memmgrRecords = NULL;

void addItemToMemMgrRecords(SCHEME_UNIVERSAL_TYPE * item) {
	MEMMGR_RECORD * mmRec = NULL;

	/* for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {
		/ * TODO: Speed this search up by using a tree instead of a list. * /

		if (mmRec->expr == item) {
			/ * The item is already in the list. * /
			fprintf(stderr, "addItemToMemMgrRecords() : Attempted duplication of item of type %d\n", item->type);
			return;
		}
	} */

	mmRec = (MEMMGR_RECORD *)mmAlloc(sizeof(MEMMGR_RECORD));

	++numMallocs;
	mmRec->expr = item;
	mmRec->next = memmgrRecords;
	memmgrRecords = mmRec;
}

int getNumMemMgrRecords() {
	int n = 0;
	MEMMGR_RECORD * mmRec;

	for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {
		++n;
	}

	return n;
}

void clearMarks() {
	MEMMGR_RECORD * mmRec;

	for (mmRec = memmgrRecords; mmRec != NULL; mmRec = mmRec->next) {
		mmRec->expr->mark = 0;
	}
}

void setMarksInExprTree(SCHEME_UNIVERSAL_TYPE * expr) {
	/* Do this recursively */
	expr->mark = 1;

	if (expr->value1 != NULL) {
		setMarksInExprTree(expr->value1);
	}

	if (expr->value2 != NULL) {
		setMarksInExprTree(expr->value2);
	}

	if (expr->next != NULL) {
		setMarksInExprTree(expr->next);
	}
}

void freeUnmarkedStructs() {
	MEMMGR_RECORD ** ppmmRec = &memmgrRecords;
	MEMMGR_RECORD * mmRec = *ppmmRec;

	while (mmRec != NULL) {

		if (mmRec->expr->mark == 0) {
			/* Free mmRec->expr. Do not free recursively.
			Allow mmRec->expr->name to be freed. */
			mmRec->expr->value1 = NULL;
			mmRec->expr->value2 = NULL;
			mmRec->expr->next = NULL;
			mmRec->expr->expr = NULL;
			mmRec->expr->expr2 = NULL;
			freeUniversalStruct(mmRec->expr);

			/* Then free mmRec, preserving the integrity of the linked list */
			MEMMGR_RECORD * nextmmRec = mmRec->next;

			mmRec->expr = NULL;
			mmRec->next = NULL;
			mmFree(mmRec);
			++numFrees;
			*ppmmRec = nextmmRec;
		} else {
			ppmmRec = &mmRec->next;
		}

		mmRec = *ppmmRec;
	}
}

void collectGarbage(SCHEME_UNIVERSAL_TYPE * exprTreesToMark[]) {
	int i;

	clearMarks();

	for (i = 0; exprTreesToMark[i] != NULL; ++i) {
		setMarksInExprTree(exprTreesToMark[i]);
	}

	freeUnmarkedStructs();
}

void freeAllStructs() {
	clearMarks();
	freeUnmarkedStructs();
}

/* **** END Memory manager version 1 **** */

/* **** The End **** */
