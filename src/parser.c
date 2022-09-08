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
#include "memory-manager.h"
#include "parser.h"
#include "utilities.h"

/* Function prototypes */

/* Forward references */

static LISP_EXPR_LIST_ELEMENT * parseExpressionList(CharSource * cs);
static LISP_VALUE * createQuotedList(CharSource * cs);

/* Constants */

static char * primops[] = {
	"+", "-", "*", "/", "%", "<", ">", "<=", ">=",
	"=", /* For all value types, not just numbers */
	"!=", /* For all value types, not just numbers */
	"closure?", "list?", "null?", "number?", "pair?", "primop?", "string?", "symbol?",
	"cons", "car", "cdr", "list", "listtostring", "rplaca", "rplacd",
	"if", "print", "random", "throw", "call/cc", "and", "or", "??",
	/* Not yet implemented: "quote", "floor" */
	NULL
};

/* Functions */

static LISP_EXPR * parseFunctionCallExpression(CharSource * cs) {
	LISP_EXPR_LIST_ELEMENT * exprList = parseExpressionList(cs);

	SCHEME_UNIVERSAL_TYPE * functionCall = createUniversalStruct(
		schemeStructType_FunctionCall,
		0,
		0,
		NULL,
		exprList->next,
		NULL,
		NULL
	);

	functionCall->expr = exprList->expr;

	exprList->expr = NULL;
	exprList->next = NULL;
	/* mmFree(exprList); */

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_FunctionCall;
	result->functionCall = functionCall;

	return result;
}

static LISP_VAR_LIST_ELEMENT * parseVariableList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fprintf(stderr, "parseVariableList() : Error : Expected ), found EOF\n");
		fatalError("parseVariableList() : Expected ), found EOF");
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* End of list */
	}

	LISP_VAR * var = createVariable(dstBuf);
	LISP_VAR_LIST_ELEMENT * next = parseVariableList(cs);

	return createVariableListElement(var, next);
}

static LISP_EXPR * parseLambdaExpression(CharSource * cs) {

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

static LISP_EXPR * parseSetExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	/* Parse variable */

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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

	return createSetExpression(var, expr);
}

static LISP_VAR_EXPR_PAIR_LIST_ELEMENT * parseVarExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found EOF\n");
		fatalError("parseVarExpressionPairList() : Expected ( or ), found EOF");
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* The end of the list */
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found '%s'\n", dstBuf);
		fatalError("parseVarExpressionPairList() : Expected ( or )");
	}

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected variable, found EOF\n");
		fatalError("parseVarExpressionPairList() : Expected variable, found EOF");
	}

	/* LISP_VAR * var = createVariable(dstBuf); */

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseVarExpressionPairList() : Expected )");
	}

	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * next = parseVarExpressionPairList(cs);
	SCHEME_UNIVERSAL_TYPE * result = allocateStringAndCreateUniversalStruct(
		schemeStructType_VariableExpressionPairListElement,
		0,
		0,
		dstBuf,
		NULL,
		NULL,
		next
	);

	result->expr = expr;

	return result;
}

static LISP_EXPR * parseLetExpression(CharSource * cs, int exprType) {

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

static LISP_EXPR * parseBeginExpression(CharSource * cs) {
	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Begin;
	result->exprList = parseExpressionList(cs);

	return result;
}

static LISP_EXPR * parseWhileExpression(CharSource * cs) {
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

static LISP_EXPR_PAIR_LIST_ELEMENT * parseExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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

static LISP_EXPR * parseCondExpression(CharSource * cs) {
	LISP_EXPR_PAIR_LIST_ELEMENT * exprPairList = parseExpressionPairList(cs);

	LISP_EXPR * result = createUndefinedExpression();

	result->type = lispExpressionType_Cond;
	result->exprPairList = exprPairList;

	return result;
}

static LISP_EXPR * parseBracketedExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	const int csRewindPoint = cs->i;

	/* 'if' is currently handled as a primop */

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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
	LISP_EXPR_LIST_ELEMENT * result = createExpressionListElement(expr, next);

	return result;
}

static LISP_VALUE * createQuotedValue(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	int dstBufAsInt = 0;

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fprintf(stderr, "createQuotedValue() : Error : Expected a literal value, found EOF\n");
		fatalError("createQuotedValue() : Error : Expected a literal value, found EOF");
		return NULL;
	} else if (!strcmp(dstBuf, "(")) {
		return createQuotedList(cs);
	} else if (safeAtoi(dstBuf, &dstBufAsInt)) {
		return createNumericValue(dstBufAsInt);
	} else {
		return createSymbolValue(dstBuf);
	}
}

static LISP_VALUE * createQuotedList(CharSource * cs) {
	const int csRewindPoint = cs->i;
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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
	/* printf("parseExpression: Begin\n"); */

	/* Be careful to not assume that sizeof(char) is always 1. */
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	int dstBufAsInt = 0;
	BOOL isSingleQuoted = FALSE;

	if (getIdentifier(cs, dstBuf, dstBufSize, &isSingleQuoted) == 0) {
		fprintf(stderr, "parseExpression() : Error : Expected an expression, found EOF\n");
		fatalError("parseExpression() : Expected an expression, found EOF");
		return NULL;
	}

	/* printf("parseExpression() : dstBuf is '%s'\n", dstBuf); */

	if (safeAtoi(dstBuf, &dstBufAsInt)) {
		/* printf("parseExpression() : safeAtoi() returned true\n"); */

		return createExpressionFromValue(createNumericValue(dstBufAsInt));
	/* } else if (!strcmp(dstBuf, "'")) { */
	} else if (isSingleQuoted) {
		/* printf("parseExpression() : isSingleQuoted\n"); */

		/* In the future, we will count quoted brackets, and keep track of
		the current quote state (i.e. is quoted or is not quoted)
		-> Or let the functions createQuotedValue() / createQuotedValue() match the quoted brackets (i.e. via recursive descent parsing) ?
		We can keep track of state data in the CharSource, if necessary. */
		return createExpressionFromValue(createQuotedValue(cs));
	} else if (strlen(dstBuf) >= 2 && dstBuf[0] == '"' && dstBuf[strlen(dstBuf) - 1] == '"') {
		/* printf("parseExpression() : Calling createStringValue and createExpressionFromValue\n"); */

		return createExpressionFromValue(createStringValue(dstBuf));
	} else if (isStringInList(dstBuf, primops)) {
		/* printf("parseExpression() : Calling createPrimitiveOperator and createExpressionFromValue\n"); */

		return createExpressionFromValue(createPrimitiveOperator(dstBuf));
	} else if (!strcmp(dstBuf, "(")) {
		/* printf("parseExpression() : Calling parseBracketedExpression()\n"); */

		return parseBracketedExpression(cs);
	} else if (!strcmp(dstBuf, ")")) {
		fatalError("parseExpression() : Encountered an unexpected ')'");
		return NULL;
	} else {
		/* printf("parseExpression() : Calling createVariable and createExpressionFromVariable\n"); */

		return createExpressionFromVariable(createVariable(dstBuf));
	}
}

/* **** The End **** */
