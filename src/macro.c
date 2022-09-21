/* atrocity/src/macro.c */

#include <stdlib.h>
/* #include <stdio.h> */

#include "types.h"

#include "char-source.h"
#include "create-and-destroy.h"
#include "environment.h"
#include "evaluate.h"
#include "parser.h"
#include "print.h"
#include "string-builder.h"

/* External constants / variables */

/* Forward references */

static void sExpressionToStringForReparse(STRING_BUILDER_TYPE * sb, LISP_VALUE * sexpression);

/* Functions */

static BOOL isQuotedConstantWithApostrophe(LISP_EXPR * expr) {
	return expr->type == lispExpressionType_QuotedConstantWithApostrophe;
}

static BOOL handlerApostrophesToQuoteKeywords(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr) {

	if (expr->type == lispExpressionType_QuotedConstantWithApostrophe) {
		appendToStringBuilder(sb, "(quote ");
		printExpressionToStringEx(sb, getValueInApostropheQuotedExpr(expr), handlerApostrophesToQuoteKeywords);
		appendToStringBuilder(sb, ")");

		return TRUE;
	}

	return FALSE;
}

static LISP_VALUE * expressionToSExpression(LISP_EXPR * expr, LISP_ENV * env) {
	STRING_BUILDER_TYPE * sb = createStringBuilder(0);

	if (isQuotedConstantWithApostrophe(expr)) {
		printExpressionToString(sb, expr);
	} else {
		appendToStringBuilder(sb, "'");
		printExpressionToStringEx(sb, expr, handlerApostrophesToQuoteKeywords);
	}

	CharSource * cs = createCharSource(getStringInStringBuilder(sb));
	LISP_EXPR * parserResult = parseExpression(cs);

	failIf(!isQuotedConstantWithApostrophe(parserResult), "MacroDefinition.ExpressionToSExpression() : quotedConstStr did not parse to a quoted constant with apostrophe");

	LISP_VALUE * value = evaluate(parserResult, env);

	freeCharSource(cs);

	return value;
}

static void sExpressionListToStringWithoutBracketsForReparse(STRING_BUILDER_TYPE * sb, LISP_VALUE * l) {
	/* failIf(l->type != lispValueType_Pair, "macro: sExpressionListToStringWithoutBracketsForReparse() : typeof l is not Pair"); */

	sExpressionToStringForReparse(sb, getHeadInPair(l));

	if (getTailInPair(l)->type == lispValueType_Null) {
		return;
	}
	// else if (l.Tail is Thunk)
	// {
	// 	return string.Format("{0} {1}", headAsString, this.sExpressionToStringForReparse(l.Tail));
	// }
	else if (getTailInPair(l)->type == lispValueType_Pair) {
		appendToStringBuilder(sb, " ");
		sExpressionListToStringWithoutBracketsForReparse(sb, getTailInPair(l));
	} else {
		// Tail is a symbol, an integer literal, a string, a closure, etc.
		appendToStringBuilder(sb, " . ");
		sExpressionToStringForReparse(sb, getTailInPair(l));
	}
}

static void sExpressionToStringForReparse(STRING_BUILDER_TYPE * sb, LISP_VALUE * sexpression) {
	// Convert the first level of quote keywords to apostrophes; e.g.:
	// (quote foo) -> "'foo"
	// (quote (quote foo)) -> "'(quote foo)"
	// ((quote foo) (quote bar)) -> "('foo 'bar)"

	/* TODO after isQuotedConstantWithQuoteKeyword() has been implemented: */
	/* if (sexpression->type == lispValueType_QuotedConstantWithQuoteKeyword) {
		appendToStringBuilder(sb, "'");
		printValueToString(sb, getValueInQuoteQuotedExpr(sexpression), NULL, FALSE);
	} else */ /* if (isList(sexpression)) { */
	if (sexpression->type == lispValueType_Pair) {
		appendToStringBuilder(sb, "(");
		sExpressionListToStringWithoutBracketsForReparse(sb, sexpression);
		appendToStringBuilder(sb, ")");
	} else {
		printValueToString(sb, sexpression, NULL, FALSE);
	}
}

static LISP_VALUE_LIST_ELEMENT * exprListToSExpressionList(LISP_EXPR_LIST_ELEMENT * exprList, LISP_ENV * env) {

	if (exprList == NULL) {
		return NULL;
	}

	LISP_VALUE * value = expressionToSExpression(getExprInExprList(exprList), env);
	LISP_VALUE_LIST_ELEMENT * next = exprListToSExpressionList(exprList->next, env);

	return createValueListElement(value, next);
}

/* static int getLinkedListLength(SCHEME_UNIVERSAL_TYPE * ptr) {

	if (ptr == NULL) {
		return 0;
	}

	return getLinkedListLength(ptr->next) + 1;
} */

LISP_VALUE * invokeMacro(SCHEME_UNIVERSAL_TYPE * macro, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	failIf(macro->type != lispExpressionType_Macro, "invokeMacro() : Macro is not a macro");

	LISP_VALUE_LIST_ELEMENT * valueList = exprListToSExpressionList(actualParamExprs, env);

	/* printf("invokeMacro() : Num formal params: %d\n", getLinkedListLength(getArgsInMacro(macro)));
	printf("invokeMacro() : Num actual params: %d\n", getLinkedListLength(valueList)); */

	LISP_ENV * rhoPrime = composeEnvironment(getArgsInMacro(macro), valueList, env);

	LISP_VALUE * substitutedBody = evaluate(getBodyInMacro(macro), rhoPrime);

	STRING_BUILDER_TYPE * sb = createStringBuilder(0);

	sExpressionToStringForReparse(sb, substitutedBody);

	CharSource * cs = createCharSource(sb->name);
	LISP_EXPR * parseTree = parseExpression(cs);

	freeCharSource(cs);

	return evaluate(parseTree, env);
}

/* **** The End **** */
