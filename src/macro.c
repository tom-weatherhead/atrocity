/* atrocity/src/macro.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"
#include "environment.h"
#include "evaluate.h"
#include "print.h"
#include "utilities.h"

/* External constants / variables */

/* extern LISP_VALUE * globalNullValue; */

/* Functions */

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
}

private expressionToSExpression(
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
}

private sExpressionListToStringWithoutBracketsForReparse(l: SExpressionList): string {
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

/* void sExpressionListToStringWithoutBracketsForReparse(STRING_BUILDER_TYPE * sb, LISP_VALUE * sexpression) {
	;
}

void sExpressionToStringForReparse(STRING_BUILDER_TYPE * sb, LISP_VALUE * sexpression) {

	if (isQuotedConstantWithQuoteKeyword(sexpression)) {
		appendToStringBuilder(sb, "'");
		appendToStringBuilder(sb, sexpression.sexpression.toString());
	} else if (!isSExpressionList(sexpression)) {
		appendToStringBuilder(sb, sexpression.toString());
	} else {
		appendToStringBuilder(sb, "(");
		sExpressionListToStringWithoutBracketsForReparse(sb, sexpression);
		appendToStringBuilder(sb, ")");
	}
} */

LISP_VALUE * invokeMacro(SCHEME_UNIVERSAL_TYPE * macro, LISP_EXPR_LIST_ELEMENT * actualParamExprs, LISP_ENV * env) {
	/* return globalNullValue; */

	return createNumericValue(1337);

	/* C:
	LISP_VALUE_LIST_ELEMENT * valueList = unevaluatedArguments.map((expr) => expressionToSExpression(expr);

	LISP_ENV * rhoPrime = composeEnvironment(getArgsInMacro(macro), valueList, env);

	LISP_VALUE * substitutedBody = evaluate(getBodyInMacro(macro), rhoPrime);

	STRING_BUILDER_TYPE * sb = createStringBuilder(0);

	sExpressionToStringForReparse(sb, substitutedBody);

	CharSource * cs = createCharSource(sb->name);
	LISP_EXPR * parseTree = parseExpression(cs);

	freeCharSource(cs);

	return evaluate(parseTree, env);
	*/

	/* Typescript:
	const rhoPrime = new EnvironmentFrame<ISExpression>(localEnvironment);

	rhoPrime.compose(
		this.argList,
		unevaluatedArguments.map((expr) => this.expressionToSExpression(expr, globalInfo))
	);

	const substitutedBody = this.body.evaluate(globalInfo, rhoPrime);
	const substitutedBodyAsString = this.sExpressionToStringForReparse(substitutedBody);
	let parserResult: unknown;

	try {
		parserResult = globalInfo.parser.parse(
			globalInfo.tokenizer.tokenize(substitutedBodyAsString)
		);
	} catch (ex) {
		throw new Error(`Error while parsing ${substitutedBodyAsString} : ${ex}`);
	}

	const exprParsed = parserResult as IExpression<ISExpression>;

	return exprParsed.evaluate(globalInfo, localEnvironment);
	*/
}

/* **** The End **** */
