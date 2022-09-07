/* atrocity/src/parse-and-evaluate.h */

LISP_VALUE * parseStringAndEvaluate(char * str, LISP_ENV * globalEnv);
void parseAndEvaluateEx(char * str, LISP_ENV * globalEnv, BOOL verbose);
/* void parseAndEvaluate(char * str); */
/* void parseAndEvaluateStringList(char * strs[]); */

/* **** The End **** */
