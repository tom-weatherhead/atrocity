/* atrocity/src/parser.c */

/* **** Parsing (recursive descent - a real half-assed parser) **** */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"
#include "parser.h"

/* Function prototypes */

/* Forward references */

static LISP_EXPR_LIST_ELEMENT * parseExpressionList(CharSource * cs);
static LISP_VALUE * createQuotedList(CharSource * cs);

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

	if (functionCall == NULL) {
		fatalError("malloc() failed in parseFunctionCallExpression()");
	}

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
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseVariableList() : Error : Expected ), found EOF\n");
		fatalError("parseVariableList() : Expected ), found EOF");
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* End of list */
	}

	/* printf("parseVariableList() : Creating the variable '%s'\n", dstBuf); */

	LISP_VAR * var = createVariable(dstBuf);
	LISP_VAR_LIST_ELEMENT * next = parseVariableList(cs);

	return createVariableListElement(var, next);
}

LISP_EXPR * parseLambdaExpression(CharSource * cs) {

	if (!consumeStr(cs, "(")) {
		fatalError("parseLambdaExpression() : Expected (");
	}

	/* Parse variable list and consume ) */
	LISP_VAR_LIST_ELEMENT * args = parseVariableList(cs);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseLambdaExpression() : Expected )");
	}

	return createLambdaExpression(args, expr);
}

LISP_EXPR * parseSetExpression(CharSource * cs) {
	/* printf("parseSetExpression() : begin\n"); */

	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	/* Parse variable */

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseSetExpression() : Error : Expected variable, found EOF\n");
		fatalError("parseSetExpression() : Expected variable, found EOF");
	} else if (!strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected variable, found '('\n");
		fatalError("parseSetExpression() : Expected variable, found '('");
	} else if (!strcmp(dstBuf, ")")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected variable, found ')'\n");
		fatalError("parseSetExpression() : Expected variable, found ')'");
	}

	LISP_VAR * var = createVariable(dstBuf);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fprintf(stderr, "parseSetExpression() : Error : Expected ')'\n");
		fatalError("parseSetExpression() : Expected ')'");
	}

	/* printf("Calling createSetExpression()...\n"); */

	return createSetExpression(var, expr);
}

LISP_VAR_EXPR_PAIR_LIST_ELEMENT * parseVarExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found EOF\n");
		fatalError("parseVarExpressionPairList() : Expected ( or ), found EOF");
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* The end of the list */
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found '%s'\n", dstBuf);
		fatalError("parseVarExpressionPairList() : Expected ( or )");
	}

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected variable, found EOF\n");
		fatalError("parseVarExpressionPairList() : Expected variable, found EOF");
	}

	LISP_VAR * var = createVariable(dstBuf);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseVarExpressionPairList() : Expected )");
	}

	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * next = parseVarExpressionPairList(cs);
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * result = (LISP_VAR_EXPR_PAIR_LIST_ELEMENT *)malloc(sizeof(LISP_VAR_EXPR_PAIR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("malloc() failed in parseVarExpressionPairList()");
	}

	result->var = var;
	result->expr = expr;
	result->next = next;

	return result;
}

LISP_EXPR * parseLetExpression(CharSource * cs, int exprType) {

	if (!consumeStr(cs, "(")) {
		fatalError("parseLetExpression() : Expected (");
	}

	/* Parse the variable-expression pair list */
	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * varExprPairList = parseVarExpressionPairList(cs);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseLetExpression() : Expected )");
	}

	LISP_EXPR * result = createUndefinedExpression();

	result->type = exprType;
	result->varExprPairList = varExprPairList;
	result->expr = expr;

	return result;
}

LISP_EXPR * parseBeginExpression(CharSource * cs) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Begin;
	result->exprList = parseExpressionList(cs);

	return result;
}

LISP_EXPR * parseWhileExpression(CharSource * cs) {
	LISP_EXPR * condition = parseExpression(cs);
	LISP_EXPR * body = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseWhileExpression() : Expected )");
	}

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_While;
	result->expr = condition;
	result->expr2 = body;

	return result;
}

LISP_EXPR_PAIR_LIST_ELEMENT * parseExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseExpressionPairList() : Error : Expected ( or ), found EOF\n");
		fatalError("parseExpressionPairList() : Error : Expected ( or ), found EOF");
		return NULL;
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* End of list */
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseExpressionPairList() : Error : Expected ( or ), found '%s'\n", dstBuf);
		fatalError("parseExpressionPairList() : Error : Expected ( or )");
		return NULL;
	}

	LISP_EXPR * expr = parseExpression(cs);
	LISP_EXPR * expr2 = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseExpressionPairList() : Encountered an unexpected ')'");
		return NULL;
	}

	LISP_EXPR_PAIR_LIST_ELEMENT * next = parseExpressionPairList(cs);

	return createExpressionPairListElement(expr, expr2, next);
}

LISP_EXPR * parseCondExpression(CharSource * cs) {
	LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList = parseExpressionPairList(cs);

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Cond;
	result->exprPairList = exprPairList;

	return result;
}

LISP_EXPR * parseBracketedExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	const int csRewindPoint = cs->i;

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseBracketedExpression() : Error : Expected an expression or keyword, found EOF\n");
		fatalError("parseBracketedExpression() : Error : Expected an expression or keyword, found EOF");
		return NULL;
	} else if (!strcmp(dstBuf, "lambda")) {
		return parseLambdaExpression(cs);
	} else if (!strcmp(dstBuf, "set!") || !strcmp(dstBuf, "set")) {
		return parseSetExpression(cs);
	} else if (!strcmp(dstBuf, "begin")) {
		return parseBeginExpression(cs);
	} else if (!strcmp(dstBuf, "while")) {
		return parseWhileExpression(cs);
	} else if (!strcmp(dstBuf, "cond")) {
		return parseCondExpression(cs);
	} else if (!strcmp(dstBuf, "let")) {
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

static LISP_EXPR_LIST_ELEMENT * parseExpressionList(CharSource * cs) {
	/* We are parsing a bracketed list of expressions. */
	/* Assume that the opening bracket has already been consumed. */

	const int csRewindPoint = cs->i;

	const int c = getNextChar(cs);

	if (c == EOF) {
		fprintf(stderr, "parseExpressionList() : Error : Expected an expression list, found EOF\n");
		fatalError("parseExpressionList() : Error : Expected an expression list, found EOF");
		return NULL;
	}

	if (c == ')') {
		return NULL; /* End of the list */
	}

	cs->i = csRewindPoint;

	LISP_EXPR * expr = parseExpression(cs);
	LISP_EXPR_LIST_ELEMENT * next = parseExpressionList(cs);

	LISP_EXPR_LIST_ELEMENT * result = (LISP_EXPR_LIST_ELEMENT *)malloc(sizeof(LISP_EXPR_LIST_ELEMENT));

	if (result == NULL) {
		fatalError("malloc() failed in parseExpressionList()");
	}

	result->expr = expr;
	result->next = next;

	return result;
}

static LISP_VALUE * createQuotedValue(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	int dstBufAsInt = 0;

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "createQuotedValue() : Error : Expected a literal value, found EOF\n");
		fatalError("createQuotedValue() : Error : Expected a literal value, found EOF");
		return NULL;
	} else if (!strcmp(dstBuf, "(")) {
		return createQuotedList(cs);
	} else if (safeAtoi(dstBuf, &dstBufAsInt)) {
		/* printf("Converted the string '%s' to the integer %d\n", dstBuf, dstBufAsInt); */
		return createNumericValue(dstBufAsInt);
	} else {
		/* fprintf(stderr, "createQuotedValue() : Error : Expected a literal value, found '%s'\n", dstBuf);
		return NULL; */
		/* printf("createSymbolValue: '%s'\n", dstBuf); */

		return createSymbolValue(dstBuf);
	}
}

static LISP_VALUE * createQuotedList(CharSource * cs) {
	const int csRewindPoint = cs->i;
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "createQuotedList() : Error : Expected a literal value, found EOF\n");
		fatalError("createQuotedList() : Error : Expected a literal value, found EOF");
		return NULL;
	} else if (!strcmp(dstBuf, ")")) {
		return createNull();
	} else {
		cs->i = csRewindPoint;

		LISP_VALUE * head = createQuotedValue(cs);
		LISP_VALUE * tail = createQuotedList(cs);

		return createPair(head, tail);
	}
}

/* Parse an expression */

LISP_EXPR * parseExpression(CharSource * cs) {
	/* Be careful to not assume that sizeof(char) is always 1. */
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	int dstBufAsInt = 0;

	if (getIdentifier(cs, dstBuf, dstBufSize) == 0) {
		fprintf(stderr, "parseExpression() : Error : Expected an expression, found EOF\n");
		fatalError("parseExpression() : Expected an expression, found EOF");
		return NULL;
	}

	/* printf("parseExpression() : dstBuf is '%s'\n", dstBuf); */

	if (safeAtoi(dstBuf, &dstBufAsInt)) {
		return createExpressionFromValue(createNumericValue(dstBufAsInt));
	} else if (!strcmp(dstBuf, "'")) {
		return createExpressionFromValue(createQuotedValue(cs));
	} else if (strlen(dstBuf) >= 2 && dstBuf[0] == '"' && dstBuf[strlen(dstBuf) - 1] == '"') {
		return createExpressionFromValue(createStringValue(dstBuf));
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
		!strcmp(dstBuf, "list?") ||
		!strcmp(dstBuf, "primop?") ||
		!strcmp(dstBuf, "closure?") ||
		!strcmp(dstBuf, "print") ||
		!strcmp(dstBuf, "list") ||
		!strcmp(dstBuf, "random") ||
		!strcmp(dstBuf, "cons") ||
		!strcmp(dstBuf, "car") ||
		!strcmp(dstBuf, "cdr") ||
		!strcmp(dstBuf, "throw") ||
		!strcmp(dstBuf, "call/cc")
		/* Not yet implemented: */
		/* || !strcmp(dstBuf, "listtostring") ||
		!strcmp(dstBuf, "rplaca") ||
		!strcmp(dstBuf, "rplacd") ||
		!strcmp(dstBuf, "quote") ||
		!strcmp(dstBuf, "floor") */
	) {
		return createExpressionFromValue(createPrimitiveOperator(dstBuf));
	} else if (!strcmp(dstBuf, "(")) {
		return parseBracketedExpression(cs);
	} else if (!strcmp(dstBuf, ")")) {
		fatalError("parseExpression() : Encountered an unexpected ')'");
		return NULL;
	} else {
		return createExpressionFromVariable(createVariable(dstBuf));
	}
}

/* **** The End **** */
