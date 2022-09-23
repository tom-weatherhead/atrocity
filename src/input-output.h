/* atrocity/src/input-output.h */

STRING_BUILDER_TYPE * appendLineFromFileToStringBuilder(STRING_BUILDER_TYPE * sb, FILE * file);
void execScriptInFile(char * filename, LISP_ENV * globalEnv);
void readEvalPrintLoop();

/* **** The End **** */
