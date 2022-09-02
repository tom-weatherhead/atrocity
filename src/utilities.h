/* atrocity/src/utilities.h */

BOOL isStringAllWhitespace(char * str);
BOOL safeAtoi(char * str, int * ptrToInt);
char * fgets_wrapper(char * buffer, size_t buflen, FILE * fp);
BOOL isStringInList(char * str, char * list[]);
void fatalError(char * str);

/* **** The End **** */
