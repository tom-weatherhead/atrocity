/* atrocity/src/memory-manager.h */

void * mmAlloc(int numBytes);
/* void * mmRealloc(void * ptr, int numBytes); */
void mmFree(void * ptr);
void mmPrintReport();

void addItemToMemMgrRecords(SCHEME_UNIVERSAL_TYPE * item);
void freeAllStructs();

/* **** The End **** */
