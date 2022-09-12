/* atrocity/src/create-and-destroy.h */

/* int getNumCharsAllocatedToNameBufInValue(LISP_VALUE * value); */

/* Create values */
LISP_VALUE * createClosure(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body, LISP_ENV * env);
LISP_VALUE * createContinuation(int id);
LISP_VALUE * createContinuationReturn(int id, LISP_VALUE * value);
LISP_ENV * createEnvironment(LISP_ENV * next);
LISP_NAME_VALUE_LIST_ELEMENT * createNameValueListElement(char * name, LISP_VALUE * value, LISP_NAME_VALUE_LIST_ELEMENT * next);
LISP_VALUE * createNull();
LISP_VALUE * createNumericValue(int value);
LISP_VALUE * createPair(LISP_VALUE * head, LISP_VALUE * tail);
LISP_VALUE * createPrimitiveOperator(char * value);
LISP_VALUE * createStringValue(char * value);
LISP_VALUE * createSymbolValue(char * value);
LISP_VALUE * createThunk(LISP_EXPR * body, LISP_ENV * env);

LISP_VALUE * cloneValue(LISP_VALUE * value);

/* Create expressions */
LISP_EXPR * createBeginExpression(LISP_EXPR_LIST_ELEMENT * exprList);
LISP_EXPR * createCondExpression(LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList);
LISP_EXPR * createFunctionCallExpression(LISP_EXPR_LIST_ELEMENT * exprList);
LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body);
LISP_EXPR * createLetExpression(int exprType, LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList, LISP_EXPR * expr);
LISP_EXPR * createSetExpression(LISP_VAR * var, LISP_EXPR * expr);
LISP_VAR * createVariable(char * name);
LISP_EXPR * createWhileExpression(LISP_EXPR * condition, LISP_EXPR * body);

LISP_EXPR * createExpressionFromVariable(LISP_VAR * var);
LISP_EXPR * createExpressionFromValue(LISP_VALUE * value);

/* Create other stuff */
LISP_EXPR_LIST_ELEMENT * createExpressionListElement(LISP_EXPR * expr, LISP_EXPR_LIST_ELEMENT * next);
LISP_EXPR_PAIR_LIST_ELEMENT * createExpressionPairListElement(LISP_EXPR * expr, LISP_EXPR * expr2, LISP_EXPR_PAIR_LIST_ELEMENT * next);
LISP_VAR_EXPR_PAIR_LIST_ELEMENT * createVariableExpressionPairListElement(char * buf, LISP_EXPR * expr, LISP_VAR_EXPR_PAIR_LIST_ELEMENT * next);
LISP_VAR_LIST_ELEMENT * createVariableListElement(LISP_VAR * var, LISP_VAR_LIST_ELEMENT * next);

/* Free stuff */
/* void freeEnvironment(LISP_ENV * env); */

void freeUniversalStruct(SCHEME_UNIVERSAL_TYPE * expr);

/* DOM functions */
/* BOOL isList(LISP_VALUE * value);
void printValue(LISP_VALUE * value);
BOOL printValueToString(LISP_VALUE * value, char * buf, int bufsize); */

STRING_BUILDER_TYPE * createStringBuilder(int bufIncSize);

/* **** The End **** */
