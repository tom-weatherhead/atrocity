/* atrocity/src/environment.h */

LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env);
void addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value);
void setValueInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value);

/* **** The End **** */
