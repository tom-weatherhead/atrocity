/* atrocity/src/types.h */

/* Preprocessor defines */

#if !defined(BOOL) && !defined(FALSE) && !defined(TRUE)
/* Our poor man's Boolean data type: */
#define BOOL int
#define FALSE 0
#define TRUE 1
#endif

#define maxStringValueLength 128

/* Type definitions */

/* Definitions of values and expressions: */
/* Every value is an expression. */
/* Every expression can be evaluated to a value. */

typedef struct SCHEME_UNIVERSAL_STRUCT {
	/* Contains nine permanent members. */

	int mark; /* All dynamically allocated structs must have this member */

	int type;
	/* TODO: Use a union? */
	int integerValue;

	int maxNameLength; /* Size (in chars) of the allocated buffer to which name points */
	char * name; /* Or use the char name[1]; trick at the end of the struct? */

	/* Pair */
	struct SCHEME_UNIVERSAL_STRUCT * value1;
	struct SCHEME_UNIVERSAL_STRUCT * value2;
	struct SCHEME_UNIVERSAL_STRUCT * value3;

	struct SCHEME_UNIVERSAL_STRUCT * next; /* To allow linked lists */
} SCHEME_UNIVERSAL_TYPE;

#define LISP_CLOSURE SCHEME_UNIVERSAL_TYPE
#define LISP_ENV SCHEME_UNIVERSAL_TYPE
#define LISP_EXPR SCHEME_UNIVERSAL_TYPE
#define LISP_EXPR_LIST_ELEMENT SCHEME_UNIVERSAL_TYPE
#define LISP_EXPR_PAIR_LIST_ELEMENT SCHEME_UNIVERSAL_TYPE
#define LISP_FUNCTION_CALL SCHEME_UNIVERSAL_TYPE
#define LISP_LAMBDA_EXPR SCHEME_UNIVERSAL_TYPE
#define LISP_NAME_VALUE_LIST_ELEMENT SCHEME_UNIVERSAL_TYPE
#define LISP_PAIR SCHEME_UNIVERSAL_TYPE
#define LISP_VALUE SCHEME_UNIVERSAL_TYPE
#define LISP_VAR SCHEME_UNIVERSAL_TYPE
#define LISP_VAR_EXPR_PAIR_LIST_ELEMENT SCHEME_UNIVERSAL_TYPE
#define LISP_VAR_LIST_ELEMENT SCHEME_UNIVERSAL_TYPE

#define getArgsInClosure(c) ((c)->value1)
#define getBodyInClosure(c) ((c)->value3)
#define getEnvInClosure(c) ((c)->value2)

#define getExprInExprList(el) ((el)->value1)

#define getFirstExprInFunctionCall(fc) ((fc)->value2)
#define getActualParamExprsInFunctionCall(fc) ((fc)->value1)

#define getArgsInLambdaExpr(le) ((le)->value1)
#define getBodyInLambdaExpr(le) ((le)->value2)

#define getHeadInPair(p) ((p)->value1)
#define getTailInPair(p) ((p)->value2)

#define getExprInPairListElement(ple) ((ple)->value1)
#define getExpr2InPairListElement(ple) ((ple)->value2)

#define getExprInVarExprPairListElement(veple) ((veple)->value1)

#define getIntegerValueInValue(v) ((v)->integerValue)
#define getNameInValue(v) ((v)->name)
#define getPairInValue(v) (v)
#define getClosureInValue(v) (v)
#define getContinuationIdInValue(v) ((v)->integerValue)
#define getContinuationReturnValueInValue(v) ((v)->value1)

#define getExprListInExpr(e) ((e)->value1)
/* #define getLambdaExprInExpr(e) ((e)->value1) */
#define getLambdaExprInExpr(e) (e)
/* #define getFunctionCallInExpr(e) ((e)->value1) */
#define getFunctionCallInExpr(e) (e)
#define getExprInExpr(e) ((e)->value2)
#define getExpr2InExpr(e) ((e)->value1)
#define getVarExprPairListInExpr(e) ((e)->value1)
#define getExprPairListInExpr(e) ((e)->value1)

#define getValueInExpr(e) ((e)->value1)
#define getVarInExpr(e) ((e)->value1)

/* The NameValueList is a crude dictionary of values. */

enum {
	lispValueType_Number, /* 0 */
	lispValueType_String,
	lispValueType_Symbol,
	lispValueType_PrimitiveOperator,
	lispValueType_Closure,
	lispValueType_Pair,
	lispValueType_Null, /* 6 */ /* TODO? Interpret the NULL pointer as a Null value? */
	/* lispValueType_Thunk, -> A suspended computation; used to implement lazy evaluation in SASL */
	/* lispValueType_Exception, */
	lispPseudoValueType_Continuation,
	lispPseudoValueType_ContinuationReturn,

	lispExpressionType_Value,
	lispExpressionType_Variable, /* 10 */
	lispExpressionType_LambdaExpr,
	lispExpressionType_FunctionCall,
	lispExpressionType_SetExpr,
	lispExpressionType_Let,
	lispExpressionType_LetStar,
	lispExpressionType_Letrec,
	lispExpressionType_Begin,
	lispExpressionType_While,
	lispExpressionType_Cons,
	lispExpressionType_Cond,
	lispExpressionType_Car,
	lispExpressionType_Cdr,
	lispExpressionType_CallCC,

	schemeStructType_Environment,
	schemeStructType_ExpressionListElement,
	schemeStructType_ExpressionPairListElement,
	/* schemeStructType_FunctionCall,
	schemeStructType_LambdaExpr, */
	schemeStructType_NameValueListElement,
	schemeStructType_VariableListElement,
	schemeStructType_VariableExpressionPairListElement
};

#define failIf(b, str) if (b) { fprintf(stderr, "Fatal error '%s' in file %s at line %d\n", str, __FILE__, __LINE__); exit(1); }

/* **** The End **** */
