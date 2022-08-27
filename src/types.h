/* atrocity/src/types.h */

/* Preprocessor defines */

#if !defined(BOOL) && !defined(FALSE) && !defined(TRUE)
/* Our poor man's Boolean data type: */
#define BOOL int
#define FALSE 0
#define TRUE 1
#endif

/* const int maxStringValueLength = 8; */
#define maxStringValueLength 16

/* Forward declarations of some structs */

struct LISP_PAIR_STRUCT;
struct LISP_VAR_LIST_ELEMENT_STRUCT;
struct LISP_EXPR_STRUCT;
struct LISP_EXPR_LIST_ELEMENT_STRUCT;

/* Type definitions */

/* Definitions of values and expressions: */
/* Every value is an expression. */
/* Every expression can be evaluated to a value. */

typedef struct {
	int mark; /* All dynamically allocated structs must have this member */

	int type;
	/* TODO: Use a union */
	int value;
	char name[maxStringValueLength]; /* Used by ValueTypes String and PrimitiveOperator */
	struct LISP_PAIR_STRUCT * pair;
	struct LISP_CLOSURE_STRUCT * closure;
} LISP_VALUE;

/* typedef struct {
} LISP_S_EXPR; */ /* A value */

/* The NameValueList is a crude dictionary of values. */

typedef struct LISP_NAME_VALUE_LIST_ELEMENT_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	char * name;
	LISP_VALUE * value;
	struct LISP_NAME_VALUE_LIST_ELEMENT_STRUCT * next;
} LISP_NAME_VALUE_LIST_ELEMENT;

typedef struct LISP_ENV_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_NAME_VALUE_LIST_ELEMENT * nameValueList;
	struct LISP_ENV_STRUCT * next;
} LISP_ENV;

typedef struct LISP_PAIR_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_VALUE * head;
	LISP_VALUE * tail;
} LISP_PAIR; /* A value. */

typedef struct LISP_CLOSURE_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	struct LISP_VAR_LIST_ELEMENT_STRUCT * args;
	struct LISP_EXPR_STRUCT * body;
	struct LISP_ENV_STRUCT * env;
} LISP_CLOSURE; /* A value. Closures are part of Scheme, not LISP. */

typedef struct {
	int mark; /* All dynamically allocated structs must have this member */

	char name[maxStringValueLength];
} LISP_VAR; /* An expression */

typedef struct LISP_VAR_LIST_ELEMENT_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_VAR * var;
	struct LISP_VAR_LIST_ELEMENT_STRUCT * next;
} LISP_VAR_LIST_ELEMENT;

typedef struct LISP_EXPR_PAIR_LIST_ELEMENT_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	struct LISP_EXPR_STRUCT * expr;
	struct LISP_EXPR_STRUCT * expr2;
	struct LISP_EXPR_PAIR_LIST_ELEMENT_STRUCT * next;
} LISP_EXPR_PAIR_LIST_ELEMENT;

typedef struct LISP_VAR_EXPR_PAIR_LIST_ELEMENT_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_VAR * var;
	struct LISP_EXPR_STRUCT * expr;
	struct LISP_VAR_EXPR_PAIR_LIST_ELEMENT_STRUCT * next;
} LISP_VAR_EXPR_PAIR_LIST_ELEMENT;

typedef struct LISP_EXPR_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	int type;
	LISP_VALUE * value;
	LISP_VAR * var;
	struct LISP_EXPR_LIST_ELEMENT_STRUCT * exprList;
	struct LISP_LAMBDA_EXPR_STRUCT * lambdaExpr;
	struct LISP_FUNCTION_CALL_STRUCT * functionCall;
	struct LISP_EXPR_STRUCT * expr; /* For e.g. set! */
	struct LISP_EXPR_STRUCT * expr2; /* For e.g. cons */
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList; /* For let, let*, letrec */
	LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList;
} LISP_EXPR;

typedef struct LISP_EXPR_LIST_ELEMENT_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_EXPR * expr;
	struct LISP_EXPR_LIST_ELEMENT_STRUCT * next;
} LISP_EXPR_LIST_ELEMENT;

typedef struct LISP_LAMBDA_EXPR_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_VAR_LIST_ELEMENT * args;
	LISP_EXPR * body;
} LISP_LAMBDA_EXPR; /* An expression */

typedef struct LISP_FUNCTION_CALL_STRUCT {
	int mark; /* All dynamically allocated structs must have this member */

	LISP_EXPR * firstExpr;
	LISP_EXPR_LIST_ELEMENT * actualParamExprs;
} LISP_FUNCTION_CALL; /* An expression */

enum {
	lispValueType_Undefined,
	lispValueType_Number,
	lispValueType_String,
	lispValueType_Symbol,
	lispValueType_PrimitiveOperator,
	lispValueType_Closure,
	lispValueType_Pair,
	lispValueType_Null,
	lispExpressionType_Undefined,
	lispExpressionType_Value,
	lispExpressionType_Variable,
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
	lispExpressionType_CallCC
};

/* **** The End **** */
