/* atrocity/src/types.h */

/* Preprocessor defines */

#if !defined(BOOL) && !defined(FALSE) && !defined(TRUE)
/* Our poor man's Boolean data type: */
#define BOOL int
#define FALSE 0
#define TRUE 1
#endif

#define maxStringValueLength 128

/* Forward declarations of some structs */

struct LISP_EXPR_STRUCT;

/* Type definitions */

/* Definitions of values and expressions: */
/* Every value is an expression. */
/* Every expression can be evaluated to a value. */

/* TODO: Migrate the data model to this single structure: */
/* Currently, 2 structs are defined in this file. */
typedef struct SCHEME_UNIVERSAL_STRUCT {
	/* Contains eight permanent members. */

	int mark; /* All dynamically allocated structs must have this member */

	int type;
	/* TODO: Use a union? */
	int integerValue;

	int maxNameLength; /* Size (in chars) of the allocated buffer to which name points */
	char * name; /* Or use the char name[1]; trick at the end of the struct? */

	/* Pair */
	struct SCHEME_UNIVERSAL_STRUCT * value1;
	struct SCHEME_UNIVERSAL_STRUCT * value2;

	struct SCHEME_UNIVERSAL_STRUCT * next; /* To allow linked lists */

	/* BEGIN Temporary members */
	struct LISP_EXPR_STRUCT * expr;
	struct LISP_EXPR_STRUCT * expr2;
	/* END Temporary members */
} SCHEME_UNIVERSAL_TYPE;

#define LISP_CLOSURE SCHEME_UNIVERSAL_TYPE
#define LISP_ENV SCHEME_UNIVERSAL_TYPE
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
#define getBodyInClosure(c) ((c)->expr)
#define getEnvInClosure(c) ((c)->value2)

#define getFirstExprInFunctionCall(fc) ((fc)->expr)
#define getActualParamExprsInFunctionCall(fc) ((fc)->value1)

#define getArgsInLambdaExpr(le) ((le)->value1)
#define getBodyInLambdaExpr(le) ((le)->expr)

#define getHeadInPair(p) ((p)->value1)
#define getTailInPair(p) ((p)->value2)

#define getIntegerValueInValue(v) ((v)->integerValue)
#define getNameInValue(v) ((v)->name)
#define getPairInValue(v) (v)
#define getClosureInValue(v) (v)
#define getContinuationIdInValue(v) ((v)->integerValue)
#define getContinuationReturnValueInValue(v) ((v)->value1)

/* The NameValueList is a crude dictionary of values. */

typedef struct LISP_EXPR_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	int type;
	/* TODO: All of the pointers below will become pointers to SCHEME_UNIVERSAL_TYPE */
	SCHEME_UNIVERSAL_TYPE * value;
	SCHEME_UNIVERSAL_TYPE * var;
	SCHEME_UNIVERSAL_TYPE * exprList;
	SCHEME_UNIVERSAL_TYPE * lambdaExpr;
	SCHEME_UNIVERSAL_TYPE * functionCall;
	struct LISP_EXPR_STRUCT * expr; /* For e.g. set! */
	struct LISP_EXPR_STRUCT * expr2; /* For e.g. cons */
	SCHEME_UNIVERSAL_TYPE * varExprPairList; /* For let, let*, letrec */
	SCHEME_UNIVERSAL_TYPE * exprPairList;
} LISP_EXPR;

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

	lispExpressionType_Undefined,
	/* TODO: For each type of expression, determine which members
	of SCHEME_UNIVERSAL_TYPE will be used to hold what data.
	Will we need to add a value3 to SCHEME_UNIVERSAL_TYPE ? */
	lispExpressionType_Value,
	lispExpressionType_Variable, /* 11 */
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
	schemeStructType_FunctionCall,
	schemeStructType_LambdaExpr,
	schemeStructType_NameValueListElement,
	schemeStructType_VariableListElement,
	schemeStructType_VariableExpressionPairListElement
};

#define failIf(b, str) if (b) { fprintf(stderr, "Fatal error '%s' in file %s at line %d\n", str, __FILE__, __LINE__); exit(1); }

/* **** The End **** */
