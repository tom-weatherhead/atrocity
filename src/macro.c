/* atrocity/src/macro.c */

#include <stdlib.h>

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

static BOOL handlerApostrophesToQuoteKeywords(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr) {

	if (expr->type == lispValueType_QuotedConstantWithApostrophe) {
		appendToStringBuilder(sb, "(quote ");
		printExpressionToStringEx(sb, getValueInApostropheQuotedValue(expr), handlerApostrophesToQuoteKeywords);
		appendToStringBuilder(sb, ")");

		return TRUE;
	}

	return FALSE;
}

static LISP_VALUE * expressionToSExpression(LISP_EXPR * expr, LISP_ENV * env) {
	STRING_BUILDER_TYPE * sb = createStringBuilder(0);

	if (expr->type == lispValueType_QuotedConstantWithApostrophe) {
		printExpressionToString(sb, expr);
	} else {
		appendToStringBuilder(sb, "'");
		printExpressionToStringEx(sb, expr, handlerApostrophesToQuoteKeywords);
	}

	CharSource * cs = createCharSource(getStringInStringBuilder(sb));
	LISP_EXPR * parserResult = parseExpression(cs);

	failIf(parserResult->type != lispValueType_QuotedConstantWithApostrophe, "MacroDefinition.ExpressionToSExpression() : quotedConstStr did not parse to a quoted constant with apostrophe");

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

	if (sexpression->type == lispValueType_QuotedConstantWithQuoteKeyword) {
		appendToStringBuilder(sb, "'");
		printValueToString(sb, getValueInQuoteQuotedValue(sexpression));
	} else if (sexpression->type == lispValueType_Pair) {
		appendToStringBuilder(sb, "(");
		sExpressionListToStringWithoutBracketsForReparse(sb, sexpression);
		appendToStringBuilder(sb, ")");
	} else {
		printValueToString(sb, sexpression);
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

LISP_VALUE * invokeMacro(SCHEME_UNIVERSAL_TYPE * macro, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	failIf(macro->type != lispExpressionType_Macro, "invokeMacro() : Macro is not a macro");

	LISP_VALUE_LIST_ELEMENT * valueList = exprListToSExpressionList(actualParamExprs, env);

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
