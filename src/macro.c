/* atrocity/src/macro.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"
#include "create-and-destroy.h"
#include "environment.h"
#include "evaluate.h"
#include "parser.h"
#include "print.h"
#include "string-builder.h"
#include "utilities.h"

/* External constants / variables */

/* extern LISP_VALUE * globalNullValue; */

/* Forward references */

/* static void objectToString_ApostrophesToQuoteKeywords(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr, BOOL (*fnHandler)(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr)); */

static void sExpressionToStringForReparse(STRING_BUILDER_TYPE * sb, LISP_VALUE * sexpression);

/* Functions */

static BOOL isQuotedConstantWithApostrophe(LISP_EXPR * expr) {
	return expr->type == lispExpressionType_QuotedConstantWithApostrophe;
}

/* Typescript:

// public override string ToString() {
// 	return string.Format("(define-macro {0} {1} {2})", MacroName, ArgList, Body);
// }

// private sExpressionListToString_ApostrophesToQuoteKeywords(l: SExpressionList): string {
// 	const headAsString = this.objectToString_ApostrophesToQuoteKeywords(l.head);
//
// 	if (isNullSExpression(l.tail)) {
// 		return headAsString;
// 		// } else if (isThunk(l.tail)) {
// 		// 	return `${headAsString} ${l.tail}`;
// 	} else if (isSExpressionList(l.tail)) {
// 		return `${headAsString} ${this.sExpressionListToString_ApostrophesToQuoteKeywords(
// 			l.tail
// 		)}`;
// 	} else {
// 		// Tail is a symbol, an integer literal, a string, a closure, etc.
// 		return `${headAsString} . ${this.objectToString_ApostrophesToQuoteKeywords(l.tail)}`;
// 	}
// }

public objectToString_ApostrophesToQuoteKeywords(expr: unknown): string {
	if (isFunctionDefinition<ISExpression>(expr)) {
		return `(define ${expr.functionName} (${expr.argList
			.map((a) => a.name)
			.join(' ')}) ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else if (isIfUsage<ISExpression>(expr)) {
		return `(if ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.condition
		)} ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.ifBody
		)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.elseBody)})`;
	} else if (isWhileUsage<ISExpression>(expr)) {
		return `(while ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.condition
		)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else if (isSetUsage<ISExpression>(expr)) {
		return `(set ${expr.variableName} ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isBeginUsage<ISExpression>(expr)) {
		return `(begin ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.firstExpression
		)} ${expr.expressionList
			.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
			.join(' ')})`;
	} else if (isCondUsage<ISExpression>(expr)) {
		const exprPairListString = expr.exprPairList
			.map(
				([expr1, expr2]: [IExpression<ISExpression>, IExpression<ISExpression>]) =>
					`(${this.objectToString_ApostrophesToQuoteKeywords(
						expr1
					)} ${this.objectToString_ApostrophesToQuoteKeywords(expr2)})`
			)
			.join(' ');

		return `(cond ${exprPairListString})`;
	} else if (isLetUsage<ISExpression>(expr)) {
		const bindingsString = expr.bindings
			.map(
				([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
					`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
			)
			.join(' ');

		return `(let (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isLetStarUsage<ISExpression>(expr)) {
		const bindingsString = expr.bindings
			.map(
				([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
					`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
			)
			.join(' ');

		return `(let* (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isOperatorUsage<ISExpression>(expr) / * && !(expr is Scheme.PrimOp) * /) {
		if (expr.expressionList.length === 0) {
			return `(${expr.operatorName})`;
		}

		const exprListString = expr.expressionList
			.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
			.join(' ');

		return `(${expr.operatorName} ${exprListString})`;
	} else if (isQuotedConstantWithApostrophe(expr)) {
		return `(quote ${expr.sexpression})`;
	}
	// 	/ *
	// else if (expr is SExpressionList) // Not an IExpression<ISExpression>
	// {
	// 	return string.Format("({0})", SExpressionListToString_ApostrophesToQuoteKeywords((SExpressionList)expr));
	// }
	// 	 * /
	else if (isMacroDefinition(expr)) {
		return `(define-macro ${expr.macroName} ${
			expr.argList
		} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else if (isLambdaExpression(expr)) {
		return `(lambda ${expr.argList} ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.body
		)})`;
	} else if (isEvaluableExpression(expr)) {
		const feAsString = this.objectToString_ApostrophesToQuoteKeywords(expr.firstExpression);

		if (expr.expressionList.length == 0) {
			return `(${feAsString})`;
		}

		return `(${feAsString} ${expr.expressionList
			.map((x) => this.objectToString_ApostrophesToQuoteKeywords(x))
			.join(' ')})`;
	} else if (isLetRecUsage<ISExpression>(expr)) {
		const fnBindingAsString = ([v, expr2]: [
			IVariable<ISExpression>,
			IExpression<ISExpression>
		]) => `(${v} ${this.objectToString_ApostrophesToQuoteKeywords(expr2)})`;
		const bindingsAsString = expr.bindings.map(fnBindingAsString).join(' ');

		return `(letrec (${bindingsAsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isCallCCUsage(expr)) {
		return `(call/cc ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else {
		return `${expr}`;
	}
} */

static BOOL handlerApostrophesToQuoteKeywords(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr) {

	if (expr->type == lispExpressionType_QuotedConstantWithApostrophe) {
		appendToStringBuilder(sb, "(quote ");
		printExpressionToStringEx(sb, getValueInApostropheQuotedExpr(expr), handlerApostrophesToQuoteKeywords);
		appendToStringBuilder(sb, ")");

		return TRUE;
	}

	return FALSE;
}

/* TODO: This function can replace printExpressionToString() in print.c */
/* static void objectToString_ApostrophesToQuoteKeywords(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr, BOOL (*fnHandler)(STRING_BUILDER_TYPE * sb, LISP_EXPR * expr)) {
	LISP_EXPR_LIST_ELEMENT * exprList;

	if (fnHandler != NULL && (*fnHandler)(sb, expr)) {
		return;
	}

	switch (expr->type) {
		case lispExpressionType_Begin:
			appendToStringBuilder(sb, "(begin");

			for (exprList = getExprListInBeginExpr(expr); exprList != NULL; exprList = exprList->next) {
				appendToStringBuilder(sb, " ");
				objectToString_ApostrophesToQuoteKeywords(sb, getExprInExprList(exprList), fnHandler);
			}

			appendToStringBuilder(sb, ")");
			break;

		case lispExpressionType_FunctionCall:
			appendToStringBuilder(sb, "(");
			objectToString_ApostrophesToQuoteKeywords(sb, getFirstExprInFunctionCall(expr), fnHandler);

			for (exprList = getActualParamExprsInFunctionCall(expr); exprList != NULL; exprList = exprList->next) {
				appendToStringBuilder(sb, " ");
				objectToString_ApostrophesToQuoteKeywords(sb, getExprInExprList(exprList), fnHandler);
			}

			appendToStringBuilder(sb, ")");
			break;

		case lispExpressionType_QuotedConstantWithApostrophe:
			fatalError("objectToString_ApostrophesToQuoteKeywords() : Handler was not called.");
			/ * appendToStringBuilder(sb, "(quote ");
			objectToString_ApostrophesToQuoteKeywords(sb, getValueInApostropheQuotedExpr(expr));
			appendToStringBuilder(sb, ")"); * /
			break;

		case lispExpressionType_SetExpr:
			appendToStringBuilder(sb, "(set! ");
			appendToStringBuilder(sb, getVarInSetExpr(expr)->name);
			appendToStringBuilder(sb, " ");
			objectToString_ApostrophesToQuoteKeywords(sb, getExprInSetExpr(expr), fnHandler);
			appendToStringBuilder(sb, ")");
			break;

		case lispExpressionType_Value:
			printValueToString(sb, getValueInExpr(expr), NULL, FALSE);
			break;

		case lispExpressionType_Variable:
			appendToStringBuilder(sb, getVarInExpr(expr)->name);
			break;

		case lispExpressionType_While:
			appendToStringBuilder(sb, "(while ");
			objectToString_ApostrophesToQuoteKeywords(sb, getExprInExpr(expr), fnHandler);
			appendToStringBuilder(sb, " ");
			objectToString_ApostrophesToQuoteKeywords(sb, getExpr2InExpr(expr), fnHandler);
			appendToStringBuilder(sb, ")");
			break;

		/ * lispExpressionType_Value
		lispExpressionType_Variable, * /
		case lispExpressionType_LambdaExpr:
		case lispExpressionType_Let:
		case lispExpressionType_LetStar:
		case lispExpressionType_Letrec:
		case lispExpressionType_Cond:
		case lispExpressionType_Macro:
		default:
			fprintf(stderr, "macro: objectToString_ApostrophesToQuoteKeywords() : expr type is %d\n", expr->type);
			fprintf(stderr, "macro: objectToString_ApostrophesToQuoteKeywords() : lispExpressionType_SetExpr is %d\n", lispExpressionType_SetExpr);
			fatalError("macro: objectToString_ApostrophesToQuoteKeywords() : Unsupported expr type");
			break;

		/ * default:
			printExpressionToString(sb, expr);
			break; * /
	}

	/ * if (isFunctionDefinition<ISExpression>(expr)) {
		return `(define ${expr.functionName} (${expr.argList
			.map((a) => a.name)
			.join(' ')}) ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else if (isIfUsage<ISExpression>(expr)) {
		return `(if ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.condition
		)} ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.ifBody
		)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.elseBody)})`;
	} else if (isCondUsage<ISExpression>(expr)) {
		const exprPairListString = expr.exprPairList
			.map(
				([expr1, expr2]: [IExpression<ISExpression>, IExpression<ISExpression>]) =>
					`(${this.objectToString_ApostrophesToQuoteKeywords(
						expr1
					)} ${this.objectToString_ApostrophesToQuoteKeywords(expr2)})`
			)
			.join(' ');

		return `(cond ${exprPairListString})`;
	} else if (isLetUsage<ISExpression>(expr)) {
		const bindingsString = expr.bindings
			.map(
				([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
					`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
			)
			.join(' ');

		return `(let (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isLetStarUsage<ISExpression>(expr)) {
		const bindingsString = expr.bindings
			.map(
				([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
					`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
			)
			.join(' ');

		return `(let* (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isOperatorUsage<ISExpression>(expr) / * && !(expr is Scheme.PrimOp) * /) {
		if (expr.expressionList.length === 0) {
			return `(${expr.operatorName})`;
		}

		const exprListString = expr.expressionList
			.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
			.join(' ');

		return `(${expr.operatorName} ${exprListString})`;
	} else if (isQuotedConstantWithApostrophe(expr)) {
		return `(quote ${expr.sexpression})`;
	}
	// 	/ *
	// else if (expr is SExpressionList) // Not an IExpression<ISExpression>
	// {
	// 	return string.Format("({0})", SExpressionListToString_ApostrophesToQuoteKeywords((SExpressionList)expr));
	// }
	// 	 * /
	else if (isMacroDefinition(expr)) {
		return `(define-macro ${expr.macroName} ${
			expr.argList
		} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else if (isLambdaExpression(expr)) {
		return `(lambda ${expr.argList} ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.body
		)})`;
	} else if (isLetRecUsage<ISExpression>(expr)) {
		const fnBindingAsString = ([v, expr2]: [
			IVariable<ISExpression>,
			IExpression<ISExpression>
		]) => `(${v} ${this.objectToString_ApostrophesToQuoteKeywords(expr2)})`;
		const bindingsAsString = expr.bindings.map(fnBindingAsString).join(' ');

		return `(letrec (${bindingsAsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
			expr.expression
		)})`;
	} else if (isCallCCUsage(expr)) {
		return `(call/cc ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
	} else {
		printExpressionToString(sb, expr);
	} * /
} */

/* private expressionToSExpression(
	expr: IExpression<ISExpression>,
	globalInfo: IGlobalInfo<ISExpression>
): ISExpression {
	if (typeof globalInfo.tokenizer === 'undefined') {
		throw new Error('expressionToSExpression() : this.tokenizer is undefined.');
	} else if (typeof globalInfo.parser === 'undefined') {
		throw new Error('expressionToSExpression() : this.parser is undefined.');
	}

	let quotedConstStr: string;

	if (isQuotedConstantWithApostrophe(expr)) {
		quotedConstStr = expr.toString();
	} else {
		quotedConstStr = "'" + this.objectToString_ApostrophesToQuoteKeywords(expr);
	}

	let parserResult: unknown;

	try {
		parserResult = globalInfo.parser.parse(globalInfo.tokenizer.tokenize(quotedConstStr));
	} catch (ex) {
		throw new Error(`Error while parsing ${quotedConstStr} : ${ex}`);
	}

	if (!isQuotedConstantWithApostrophe(parserResult)) {
		throw new Error(
			`MacroDefinition.ExpressionToSExpression() : The following did not parse to a quoted constant with apostrophe: ${quotedConstStr}`
		);
	}

	// var quotedConst = (QuotedConstantWithApostrophe)parserResult;

	return parserResult.evaluate(globalInfo);
} */

static LISP_VALUE * expressionToSExpression(LISP_EXPR * expr, LISP_ENV * env) {
	STRING_BUILDER_TYPE * sb = createStringBuilder(0);

	if (isQuotedConstantWithApostrophe(expr)) {
		printExpressionToString(sb, expr);
	} else {
		appendToStringBuilder(sb, "'");
		printExpressionToStringEx(sb, expr, handlerApostrophesToQuoteKeywords);
	}

	/* let parserResult: unknown;

	try {
		parserResult = globalInfo.parser.parse(globalInfo.tokenizer.tokenize(quotedConstStr));
	} catch (ex) {
		throw new Error(`Error while parsing ${quotedConstStr} : ${ex}`);
	} */
	/* printf("expressionToSExpression() : Parsing '%s'\n", getStringInStringBuilder(sb)); */

	CharSource * cs = createCharSource(getStringInStringBuilder(sb));
	LISP_EXPR * parserResult = parseExpression(cs);

	/* if (parserResult != NULL) {
		printf("expressionToSExpression() : parserResult->type is %d\n", parserResult->type);
	} */

	failIf(!isQuotedConstantWithApostrophe(parserResult), "MacroDefinition.ExpressionToSExpression() : quotedConstStr did not parse to a quoted constant with apostrophe");

	LISP_VALUE * value = evaluate(parserResult, env);

	freeCharSource(cs);

	return value;
}

/* private sExpressionListToStringWithoutBracketsForReparse(l: SExpressionList): string {
	const headAsString = this.sExpressionToStringForReparse(l.head);

	if (isNullSExpression(l.tail)) {
		return headAsString;
	}
	// else if (l.Tail is Thunk)
	// {
	// 	return string.Format("{0} {1}", headAsString, this.sExpressionToStringForReparse(l.Tail));
	// }
	else if (isSExpressionList(l.tail)) {
		return `${headAsString} ${this.sExpressionListToStringWithoutBracketsForReparse(
			l.tail
		)}`;
	} else {
		// Tail is a symbol, an integer literal, a string, a closure, etc.
		return `${headAsString} . ${this.sExpressionToStringForReparse(l.tail)}`;
	}
}

private sExpressionToStringForReparse(sexpression: ISExpression): string {
	// Convert the first level of quote keywords to apostrophes; e.g.:
	// (quote foo) -> "'foo"
	// (quote (quote foo)) -> "'(quote foo)"
	// ((quote foo) (quote bar)) -> "('foo 'bar)"

	// var qc = sexpression as QuotedConstantWithQuoteKeyword;

	// if (qc != null)
	if (isQuotedConstantWithQuoteKeyword(sexpression)) {
		return "'" + sexpression.sexpression.toString();
	}

	// var l = sexpression as SExpressionList;

	if (!isSExpressionList(sexpression)) {
		return sexpression.toString();
	} else {
		/ *
	else if (l.Head.ToString() == "quote")
	{
		var l2 = l.Tail as SExpressionList;

		if (l2 != null && l2.Length == 1)
		{
			return "'" + l2.Head.ToString();
		}
		else
		{
			return "'" + l.Tail.ToString();
		}
	}
		 * /
		return `(${this.sExpressionListToStringWithoutBracketsForReparse(sexpression)})`;
	}
} */

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

	/* TODO after isQuotedConstantWithQuoteKeyword() has been implemented:
	if (isQuotedConstantWithQuoteKeyword(sexpression)) {
		appendToStringBuilder(sb, "'");
		appendToStringBuilder(sb, sexpression.sexpression);
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
