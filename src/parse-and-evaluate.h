/* atrocity/src/parse-and-evaluate.h */

LISP_VALUE * parseStringAndEvaluate(char * str, LISP_ENV * globalEnv);
void parseAndEvaluateEx(char * str, LISP_ENV * globalEnv, BOOL verbose);

/* **** The End **** */
