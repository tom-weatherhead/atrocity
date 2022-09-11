/* atrocity/src/memory-manager.h */

void * mmAlloc(int numBytes);
void mmFree(void * ptr);
void mmPrintReport();

void addItemToMemMgrRecords(SCHEME_UNIVERSAL_TYPE * item);
int collectGarbage(SCHEME_UNIVERSAL_TYPE * exprTreesToMark[]);
int freeAllStructs();

/* **** The End **** */
