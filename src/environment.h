/* atrocity/src/environment.h */

BOOL updateNameIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, char * name, LISP_VALUE * value);
BOOL updateIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, LISP_VAR * var, LISP_VALUE * value);
LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env);
void addNameToEnvironment(LISP_ENV * env, char * name, LISP_VALUE * value);
void addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value);
void setValueInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value);
/* LISP_ENV * composeEnvironment(LISP_VAR_LIST_ELEMENT * variableList, LISP_VALUE_LIST_ELEMENT * valueList, LISP_ENV * env); */
/* void printEnvironment(LISP_ENV * env); */
LISP_ENV * createGlobalEnvironment();
void freeGlobalEnvironment(LISP_ENV * globalEnv);

/* **** The End **** */
