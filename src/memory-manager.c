/* atrocity/src/memory-manager.c */

/* A mark-and-sweep garbage collector. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"

#include "create-and-destroy.h"

typedef struct POINTER_LIST_ELEMENT_STRUCT {
	void * ptr;
	struct POINTER_LIST_ELEMENT_STRUCT * next;
} POINTER_LIST_ELEMENT;

typedef struct {
	int mark; /* All dynamically allocated structs must have this member */
} MARKED_STRUCT;

/* E.g.: */
POINTER_LIST_ELEMENT * lispValues = NULL;

POINTER_LIST_ELEMENT * addPointerToList(POINTER_LIST_ELEMENT * list, void * ptr) {
	POINTER_LIST_ELEMENT * ple = (POINTER_LIST_ELEMENT *)malloc(sizeof(POINTER_LIST_ELEMENT));

	ple->ptr = ptr;
	ple->next = list;

	return ple;
}

/*
void clearMarks() {

	for each (v in lispValues) {
		/ * ((LISP_VALUE *)v->ptr)->mark = 0; * /
		((MARKED_STRUCT *)v->ptr)->mark = 0;
	}
}
 */

/*
registerValueWithMemoryManager(LISP_VALUE * value) {
	Add value to the list lispValues;
	lispValues = addPointerToList(lispValues, value);
}


pair
void setMarksIn() {
	;
}

closure
void setMarksIn() {
	;
}

var
void setMarksIn() {
	;
}

varList
void setMarksIn() {
	;
}

varExprPair
void setMarksIn() {
	;
}

expr
void setMarksIn() {
	;
}

exprList
void setMarksIn() {
	;
}

lambdaExpr
void setMarksIn() {
	;
}

functionCall
void setMarksIn() {
	;
}
 */

/*
void setMarksInLispValue(LISP_VALUE * value) {
	;
}

void setMarksInLispEnv(LISP_ENV * env) {

	for (; env != NULL; env = env->next) {
		for each (nvle in env->nameValueList) {
			/ * There is nothing to mark in nvle->name * /
			setMarksInLispValue(nvle->value);
		}
	}
}

void setMarks(LISP_ENV * globalEnv) {
	setMarksInLispEnv(globalEnv);
}

void collectGarbage(LISP_ENV * globalEnv) {
	clearMarks();
	setMarks(globalEnv);
	freeUnmarkedStructs();
}
 */

/* **** The End **** */
