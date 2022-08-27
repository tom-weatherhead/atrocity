/* atrocity/src/environment.h */

BOOL updateIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, LISP_VAR * var, LISP_VALUE * value);
LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env);
void addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value);
void setValueInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value);

/* **** The End **** */
