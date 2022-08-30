/* atrocity/src/create-and-destroy.h */

int getNumCharsAllocatedToNameBufInValue(LISP_VALUE * value);

LISP_VAR_LIST_ELEMENT * createVariableListElement(LISP_VAR * var, LISP_VAR_LIST_ELEMENT * next);
LISP_EXPR_PAIR_LIST_ELEMENT * createExpressionPairListElement(LISP_EXPR * expr, LISP_EXPR * expr2, LISP_EXPR_PAIR_LIST_ELEMENT * next);

LISP_VALUE * createClosure(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body, LISP_ENV * env);
LISP_ENV * createEnvironment(LISP_ENV * next);
LISP_NAME_VALUE_LIST_ELEMENT * createNameValueListElement(char * name, LISP_VALUE * value, LISP_NAME_VALUE_LIST_ELEMENT * next);
LISP_VALUE * createNull();
LISP_VALUE * createNumericValue(int value);
LISP_VALUE * createPair(LISP_VALUE * head, LISP_VALUE * tail);
LISP_VALUE * createPrimitiveOperator(char * value);
LISP_VALUE * createStringValue(char * value);
LISP_VALUE * createSymbolValue(char * value);
LISP_VAR * createVariable(char * name);

LISP_VALUE * cloneValue(LISP_VALUE * value);

LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body);
LISP_EXPR * createSetExpression(LISP_VAR * var, LISP_EXPR * expr);
LISP_EXPR * createUndefinedExpression();

LISP_EXPR * createExpressionFromVariable(LISP_VAR * var);
LISP_EXPR * createExpressionFromValue(LISP_VALUE * value);

void freeValue(LISP_VALUE * value);

void freeEnvironment(LISP_ENV * env);
void freeExpression(LISP_EXPR * expr);
void freeExpressionList(LISP_EXPR_LIST_ELEMENT * exprList);
void freeExpressionPairList(LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList);
void freeVariable(LISP_VAR * var);
void freeVariableList(LISP_VAR_LIST_ELEMENT * varList);

BOOL isList(LISP_VALUE * value);
void printValue(LISP_VALUE * value);
BOOL printValueToString(LISP_VALUE * value, char * buf, int bufsize);

/* **** The End **** */
