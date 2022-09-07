/* atrocity/src/memory-manager.c */

/* A mark-and-sweep garbage collector. */

#include <stdlib.h>
#include <stdio.h>
/* #include <string.h> */
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

/* #include "create-and-destroy.h" */

static int numMallocs = 0;
static int numReallocs = 0;
static int numFrees = 0;

void * mmAlloc(int numBytes) {
	void * result = malloc(numBytes);

	failIf(result == NULL, "mmAlloc() : malloc() returned NULL");

	++numMallocs;

	return result;
}

void * mmRealloc(void * ptr, int numBytes) {
	failIf(ptr == NULL, "mmRealloc() : ptr is NULL");

	void * result = realloc(ptr, numBytes);

	failIf(result == NULL, "mmRealloc() : realloc() returned NULL");

	++numReallocs;

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
	printf("  Number of reallocs: %d\n", numReallocs);
	printf("  Number of frees:    %d\n\n", numFrees);
}

/* **** The End **** */
