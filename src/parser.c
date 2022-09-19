/* atrocity/src/parser.c */

/* **** Parsing (recursive descent - a real half-assed parser) **** */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

#include "associative-array.h"
#include "evaluate.h"
#include "create-and-destroy.h"
#include "memory-manager.h"
#include "parser.h"
#include "utilities.h"

/* Function prototypes */

/* Forward references */

static LISP_EXPR_LIST_ELEMENT * parseExpressionList(CharSource * cs);
static LISP_VALUE * createQuotedList(CharSource * cs);

/* Constants */

/* Functions */

static LISP_EXPR * parseFunctionCallExpression(CharSource * cs) {
	LISP_EXPR_LIST_ELEMENT * exprList = parseExpressionList(cs);

	return createFunctionCallExpression(exprList);
}

static LISP_VAR_LIST_ELEMENT * parseVariableList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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
		fatalError("parseSetExpression() : Expected variable, found EOF");
	} else if (!strcmp(dstBuf, "(")) {
		fatalError("parseSetExpression() : Expected variable, found '('");
	} else if (!strcmp(dstBuf, ")")) {
		fatalError("parseSetExpression() : Expected variable, found ')'");
	}

	LISP_VAR * var = createVariable(dstBuf);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseSetExpression() : Expected ')'");
	}

	return createSetExpression(var, expr);
}

static LISP_VAR_EXPR_PAIR_LIST_ELEMENT * parseVarExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fatalError("parseVarExpressionPairList() : Expected ( or ), found EOF");
	} else if (!strcmp(dstBuf, ")")) {
		return NULL; /* The end of the list */
	} else if (strcmp(dstBuf, "(")) {
		fprintf(stderr, "parseVarExpressionPairList() : Error : Expected ( or ), found '%s'\n", dstBuf);
		fatalError("parseVarExpressionPairList() : Expected ( or )");
	}

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fatalError("parseVarExpressionPairList() : Expected variable, found EOF");
	}

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseVarExpressionPairList() : Expected )");
	}

	LISP_VAR_EXPR_PAIR_LIST_ELEMENT * next = parseVarExpressionPairList(cs);

	return createVariableExpressionPairListElement(dstBuf, expr, next);
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

	return createLetExpression(exprType, varExprPairList, expr);
}

static LISP_EXPR * parseBeginExpression(CharSource * cs) {
	return createBeginExpression(parseExpressionList(cs));
}

static LISP_EXPR * parseWhileExpression(CharSource * cs) {
	LISP_EXPR * condition = parseExpression(cs);
	LISP_EXPR * body = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseWhileExpression() : Expected )");
	}

	return createWhileExpression(condition, body);
}

static LISP_EXPR_PAIR_LIST_ELEMENT * parseExpressionPairList(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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

	return createCondExpression(exprPairList);
}

static LISP_EXPR * parseDefineMacroExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];

	/* Parse the name of the macro */

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
		fatalError("parseDefineMacroExpression() : Expected macro name, found EOF");
	} else if (!strcmp(dstBuf, "(")) {
		fatalError("parseDefineMacroExpression() : Expected macro name, found '('");
	} else if (!strcmp(dstBuf, ")")) {
		fatalError("parseDefineMacroExpression() : Expected macro name, found ')'");
	}

	if (!consumeStr(cs, "(")) {
		fatalError("parseDefineMacroExpression() : Expected (");
	}

	/* Parse variable list and consume ) */
	LISP_VAR_LIST_ELEMENT * args = parseVariableList(cs);

	/* Parse expression */
	LISP_EXPR * expr = parseExpression(cs);

	if (!consumeStr(cs, ")")) {
		fatalError("parseLambdaExpression() : Expected )");
	}

	return createDefineMacroExpression(dstBuf, args, expr);
}

static LISP_EXPR * parseBracketedExpression(CharSource * cs) {
	const int dstBufSize = maxStringValueLength;
	char dstBuf[dstBufSize];
	const int csRewindPoint = cs->i;

	/* 'if' is currently handled as a primop */

	if (getIdentifier(cs, dstBuf, dstBufSize, NULL) == 0) {
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
	} else if (!strcmp(dstBuf, "define-macro")) {
		return parseDefineMacroExpression(cs);
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
	BOOL isSingleQuoted = FALSE;

	if (getIdentifier(cs, dstBuf, dstBufSize, &isSingleQuoted) == 0) {
		fatalError("parseExpression() : Expected an expression, found EOF");
		return NULL;
	}

	if (safeAtoi(dstBuf, &dstBufAsInt)) {
		return createExpressionFromValue(createNumericValue(dstBufAsInt));
	} else if (isSingleQuoted) {
		/* In the future, we will count quoted brackets, and keep track of
		the current quote state (i.e. is quoted or is not quoted)
		-> Or let the functions createQuotedValue() / createQuotedValue() match the quoted brackets (i.e. via recursive descent parsing) ?
		We can keep track of state data in the CharSource, if necessary. */
		return createExpressionFromValue(createQuotedValue(cs));
	} else if (strlen(dstBuf) >= 2 && dstBuf[0] == '"' && dstBuf[strlen(dstBuf) - 1] == '"') {
		return createExpressionFromValue(createStringValue(dstBuf));
	} else if (isPrimop(dstBuf)) {
		return createExpressionFromValue(createPrimitiveOperator(dstBuf));
	} else if (!strcmp(dstBuf, "[]")) {
		return createExpressionFromValue(createArray());
	} else if (!strcmp(dstBuf, "{}")) {
		return createExpressionFromValue(createAssociativeArray());
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
