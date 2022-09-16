/* atrocity/src/memory-manager.h */

void * mmAlloc(int numBytes);
void mmFree(void * ptr);
void mmPrintReport();

int mmReplacePointer(SCHEME_UNIVERSAL_TYPE * old, SCHEME_UNIVERSAL_TYPE * new);

void addItemToMemMgrRecords(SCHEME_UNIVERSAL_TYPE * item);
int collectGarbage(SCHEME_UNIVERSAL_TYPE * exprTreesToMark[]);
int freeAllStructs();

BOOL isCyclical(SCHEME_UNIVERSAL_TYPE * ptr);

/* **** The End **** */
