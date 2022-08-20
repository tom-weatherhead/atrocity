/* atrocity/src/parser.c */

// **** Parsing (recursive descent - a real half-assed parser) ****

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

/* Function prototypes */

LISP_EXPR * createUndefinedExpression();
LISP_VAR * createVariable(char * name);
LISP_EXPR * createSetExpression(LISP_VAR * var, LISP_EXPR * expr);
LISP_EXPR * createLambdaExpression(LISP_VAR_LIST_ELEMENT * args, LISP_EXPR * body);
LISP_VAR_LIST_ELEMENT * createVariableListElement(LISP_VAR * var, LISP_VAR_LIST_ELEMENT * next);
LISP_EXPR * createExpressionFromVariable(LISP_VAR * var);
LISP_EXPR * createExpressionFromValue(LISP_VALUE * value);
LISP_VALUE * createPrimitiveOperator(char * value);
LISP_VALUE * createNumericValue(int value);

/* Forward references */

LISP_EXPR * parseExpression(CharSource * cs);
LISP_EXPR_LIST_ELEMENT * parseExpressionList(CharSource * cs);

/* Functions */

BOOL safeAtoi(char * str, int * ptrToInt) {
	const int len = strlen(str);
	int i = 0;

	if (len > 0 && str[0] == '-') {
		i = 1; /* The - may be a minus sign */
	}

	if (i == len) {
		return FALSE; /* str is just "" or "-" */
	}

	for (; i < len; ++i) {

		if (str[i] < '0' || str[i] > '9') {
			return FALSE;
		}
	}

	*ptrToInt = atoi(str);
	return TRUE;
}

LISP_EXPR * parseFunctionCallExpression(CharSource * cs) {
	LISP_EXPR_LIST_ELEMENT * exprList = parseExpressionList(cs);

	LISP_FUNCTION_CALL * functionCall = (LISP_FUNCTION_CALL *)malloc(sizeof(LISP_FUNCTION_CALL));

	functionCall->firstExpr = exprList->expr;
	functionCall->actualParamExprs = exprList->next;

	exprList->expr = NULL;
	exprList->next = NULL;
	free(exprList);

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_FunctionCall;
	result->functionCall = functionCall;

	return result;
}

LISP_VAR_LIST_ELEMENT * parseVariableList(CharSource * cs) {
	const int dstBufSize = 8;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseVariableList() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* End of list */
	}

	printf("parseVariableList() : Creating the variable '%s'\n", dstBuf);

	LISP_VAR * var = createVariable(dstBuf);
	LISP_VAR_LIST_ELEMENT * next = parseVariableList(cs);

	return createVariableListElement(var, next);
}

LISP_EXPR * parseLambdaExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	/* Consume ( */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseLambdaExpression() : Error : Expected (, found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseLambdaExpression() : Error : Expected (, found '%s'\n", dstBuf);
		return NULL;
	}

	/* Parse variable list and consume ) */
	LISP_VAR_LIST_ELEMENT * args = parseVariableList(cs);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	/* Consume ) */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseLambdaExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseLambdaExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	return createLambdaExpression(args, expr);
}

LISP_EXPR * parseSetExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	/* Parse variable */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseSetExpression() : Error : Expected variable, found EOF\n");
		return NULL;
	} else if (!strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected variable, found '('\n");
		return NULL;
	} else if (!strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected variable, found ')'\n");
		return NULL;
	}

	LISP_VAR * var = createVariable(dstBuf);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	/* Consume ) */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseSetExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	return createSetExpression(var, expr);
}

LISP_VAR_EXPR_PAIR_LIST_ELEMENT * parseVarExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found EOF\n");
		return NULL;
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* The end of the list */
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found '%s'\n", dstBuf);
		return NULL;
	}

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected variable, found EOF\n");
		return NULL;
	}

	LISP_VAR * var = createVariable(dstBuf);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	/* Consume ) */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseSetExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * next = parseVarExpressionPairList(cs);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * result = (LISP_VAR_EXPR_PAIR_LIST_ELEMENT *)malloc(sizeof(LISP_VAR_EXPR_PAIR_LIST_ELEMENT));

	result->var = var;
	result->expr = expr;
	result->next = next;

	return result;
}

LISP_EXPR * parseLetExpression(CharSource * cs, int exprType) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	/* Consume ( */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseLetExpression() : Error : Expected (, found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseLetExpression() : Error : Expected (, found '%s'\n", dstBuf);
		return NULL;
	}

	/* Parse the variable-expression pair list */
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = parseVarExpressionPairList(cs);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	/* Consume ) */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseLetExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseLetExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	LISP_EXPR * result = createUndefinedExpression();

	result->type = exprType;
	result->varExprPairList = varExprPairList;
	result->expr = expr;

	return result;
}

LISP_EXPR * parseConsExpression(CharSource * cs) {
	/* Parse head expression */
	LISP_EXPR * head = parseExpression(cs);

	/* Parse tail expression */
	LISP_EXPR * tail = parseExpression(cs);

	/* Consume ) */
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseConsExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseConsExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Cons;
	result->expr = head;
	result->expr2 = tail;

	return result;
}

LISP_EXPR * parseCarExpression(CharSource * cs) {
	LISP_EXPR * expr = parseExpression(cs);

	/* Consume ) */
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseCarExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseCarExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Car;
	result->expr = expr;

	return result;
}

LISP_EXPR * parseCdrExpression(CharSource * cs) {
	LISP_EXPR * expr = parseExpression(cs);

	/* Consume ) */
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseCdrExpression() : Error : Expected ), found EOF\n");
		return NULL;
	} else if (strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseCdrExpression() : Error : Expected ), found '%s'\n", dstBuf);
		return NULL;
	}

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Cdr;
	result->expr = expr;

	return result;
}

LISP_EXPR * parseBeginExpression(CharSource * cs) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Begin;
	result->exprList = parseExpressionList(cs);

	return result;
}

/* LISP_EXPR * parseCallCCExpression(CharSource * cs) {
} */

LISP_EXPR * parseBracketedExpression(CharSource * cs) {
	/* TODO: letrec
	 * and maybe: print call/cc
	 */

	const int dstBufSize = 8;
	char dstBuf[dstBufSize];
	const int csRewindPoint = cs->i;

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseBracketedExpression() : Error : Expected an expression or keyword, found EOF\n");
		return NULL;
	} else if (!strcmp(dstBuf, "lambda")) {
		return parseLambdaExpression(cs);
	} else if (!strcmp(dstBuf, "set!")) {
		return parseSetExpression(cs);
	} else if (!strcmp(dstBuf, "cons")) {
		/* Should cons, car, and cdr be implemented as primops? */
		return parseConsExpression(cs);
	} else if (!strcmp(dstBuf, "car")) {
		return parseCarExpression(cs);
	} else if (!strcmp(dstBuf, "cdr")) {
		return parseCdrExpression(cs);
	} else if (!strcmp(dstBuf, "begin")) {
		return parseBeginExpression(cs);
	} /* else if (!strcmp(dstBuf, "call/cc")) {
		return parseCallCCExpression(cs);
	} */ else if (!strcmp(dstBuf, "let")) {
		return parseLetExpression(cs, lispExpressionType_Let);
	} else if (!strcmp(dstBuf, "let*")) {
		return parseLetExpression(cs, lispExpressionType_LetStar);
	} else if (!strcmp(dstBuf, "letrec")) {
		return parseLetExpression(cs, lispExpressionType_Letrec);
	} else {
		cs->i = csRewindPoint;
		return parseFunctionCallExpression(cs);
	}
}

/* Parse a list of expressions */

LISP_EXPR_LIST_ELEMENT * parseExpressionList(CharSource * cs) {
	/* We are parsing a bracketed list of expressions. */
	/* Assume that the opening bracket has already been consumed. */

	const int csRewindPoint = cs->i;

	/* if (c < 0) { */
	const int c = getNextChar(cs);

	if (c == EOF) {
		fprintf(stderr, "parseExpressionList() : Error : Expected an expression list, found EOF\n");
		return NULL;
	}
	/* } */

	if (c == ')') {
		return NULL; /* End of the list */
	}

	cs->i = csRewindPoint;

	LISP_EXPR * expr = parseExpression(cs);
	LISP_EXPR_LIST_ELEMENT * next = parseExpressionList(cs);

	LISP_EXPR_LIST_ELEMENT * result = (LISP_EXPR_LIST_ELEMENT *)malloc(sizeof(LISP_EXPR_LIST_ELEMENT));

	result->expr = expr;
	result->next = next;

	return result;
}

/* Parse an expression */

LISP_EXPR * parseExpression(CharSource * cs) {

	/* if (c >= 0) {
		rewindOneChar(cs);
	} */

	/* Be careful to not assume that sizeof(char) is always 1. */
	const int dstBufSize = 8;
	char dstBuf[dstBufSize];
	int dstBufAsInt = 0;

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseExpression() : Error : Expected an expression, found EOF\n");
		return NULL;
	} else if (safeAtoi(dstBuf, &dstBufAsInt)) {
		/* printf("Converted the string '%s' to the integer %d\n", dstBuf, dstBufAsInt); */
		return createExpressionFromValue(createNumericValue(dstBufAsInt));
	} else if (
		!strcmp(dstBuf, "+") ||
		!strcmp(dstBuf, "-") ||
		!strcmp(dstBuf, "*") ||
		!strcmp(dstBuf, "/") ||
		!strcmp(dstBuf, "%") ||
		!strcmp(dstBuf, "<") ||
		!strcmp(dstBuf, ">") ||
		!strcmp(dstBuf, "=") || /* For all value types, not just numbers */
		!strcmp(dstBuf, "<=") ||
		!strcmp(dstBuf, ">=") ||
		!strcmp(dstBuf, "!=") || /* For all value types, not just numbers */
		!strcmp(dstBuf, "if") ||
		!strcmp(dstBuf, "null?") ||
		!strcmp(dstBuf, "number?") ||
		!strcmp(dstBuf, "string?") ||
		!strcmp(dstBuf, "symbol?") ||
		!strcmp(dstBuf, "pair?") ||
		!strcmp(dstBuf, "primop?") ||
		!strcmp(dstBuf, "closure?") ||
		/* Not yet implemented: */
		!strcmp(dstBuf, "cons") ||
		!strcmp(dstBuf, "car") ||
		!strcmp(dstBuf, "cdr") ||
		!strcmp(dstBuf, "call/cc")
	) {
		/* TODO: print call/cc */
		/* printf("parseExpression() : Creating expression from PrimitiveOperator '%s'\n", dstBuf); */
		return createExpressionFromValue(createPrimitiveOperator(dstBuf));
	} else if (!strcmp(dstBuf, "(")) {
		return parseBracketedExpression(cs);
	} else if (!strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseExpression() : Error: Unexpected ')'\n");
		return NULL;
	} else {
		/* printf("parseExpression() : Creating the variable '%s'\n", dstBuf); */
		return createExpressionFromVariable(createVariable(dstBuf));
	}
}

/* **** The End **** */
