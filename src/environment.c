/* atrocity/src/environment.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "create-and-destroy.h"

/* Functions */

static LISP_VALUE * lookupVariableInNameValueList(LISP_VAR * var, LISP_NAME_VALUE_LIST_ELEMENT * nvle) {

	while (nvle != NULL) {
		/* printf("  Comparing var name '%s' to '%s'...\n", var->name, nvle->name); */

		if (!strcmp(nvle->name, var->name)) {
			/* printf("  Match\n"); */
			return nvle->value;
		}

		nvle = nvle->next;
	}

	return NULL;
}

LISP_VALUE * lookupVariableInEnvironment(LISP_VAR * var, LISP_ENV * env) {
	LISP_VALUE * value = NULL;

	while (env != NULL) {
		/* printf("lookupVariableInEnvironment: Looking for '%s' in nameValueList\n", var->name); */
		value = lookupVariableInNameValueList(var, env->nameValueList);

		if (value != NULL) {
			break;
		}

		/* printf("lookupVariableInEnvironment: Moving to the next env frame\n");
		printf("  env->nameValueList is %ld\n", (long)env->nameValueList);
		printf("  env is %ld\n", (long)env);
		printf("  env->next is %ld\n", (long)env->next); */

		if (env == env->next) {
			fprintf(stderr, "lookupVariableInEnvironment: env == env->next; breaking...\n");
			break;
		}

		env = env->next;
	}

	/* printf("lookupVariableInEnvironment: Looked up var '%s', returning value %ld\n", var->name, value); */

	return value;
}

BOOL updateIfFoundInNameValueList(LISP_NAME_VALUE_LIST_ELEMENT * nvle, LISP_VAR * var, LISP_VALUE * value) {

	while (nvle != NULL) {

		if (!strcmp(nvle->name, var->name)) {
			nvle->value = value;
			return TRUE;
		}

		nvle = nvle->next;
	}

	return FALSE;
}

static BOOL updateIfFoundInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {


	while (env != NULL) {

		if (updateIfFoundInNameValueList(env->nameValueList, var, value)) {
			return TRUE;
		}

		env = env->next;
	}

	return FALSE;
}

void addToEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {

	if (lookupVariableInEnvironment(var, env) != NULL) {
		fprintf(stderr, "addToEnvironment() : The variable '%s' already exists in this environment. Update not yet implemented.", var->name);
		return;
	}

	env->nameValueList = createNameValueListElement(var->name, value, env->nameValueList);
}

void setValueInEnvironment(LISP_ENV * env, LISP_VAR * var, LISP_VALUE * value) {

	if (!updateIfFoundInEnvironment(env, var, value)) {

		while (env->next != NULL) {
			env = env->next;
		}

		/* Now env == globalEnv */

		addToEnvironment(env, var, value);
		/* Or -> addToEnvironment(globalEnv, var, value); */
	}
}

/*
public addBubbleDown(key: IVariable<T>, value: T): void {
	// I.e. update the key's value in this frame or in any frame below it.
	// !!! -> If the key is not found, add the key and value to the global env.

	// console.log(`EnvironmentFrame<T>.AddBubbleDown() : var is ${key.name}; value is ${value}`);

	// if (value === undefined) {
	// 	console.log('Warning in EnvironmentFrame.addBubbleDown() : The value being added is falsy.');
	// }

	if (!this.dictionaryContainsKey(key) && typeof this.next !== 'undefined') {
		this.next.addBubbleDown(key, value); // Bubble down towards the global environment.
	} else {
		// Bug fix: Before 2013/12/04, the "else" above was absent, and the code below was executed unconditionally.
		// Console.WriteLine("AddBubbleDown: The new value of {0} in {1} environment frame is {2}",
		// 	key, (next != null) ? "a local" : "the global", value);
		this.add(key, value);
	}
}
*/

void printEnvironment(LISP_ENV * env) {
	int i = 0;

	printf("printEnvironment:\n");

	while (env != NULL) {
		LISP_NAME_VALUE_LIST_ELEMENT * nvle = env->nameValueList;
		int j = 0;

		printf("  Frame %d:\n", i++);

		while (nvle != NULL) {
			printf("    Value %d: %s = ", j++, nvle->name);
			printValue(nvle->value);
			printf("\n");
			nvle = nvle->next;
		}

		env = env->next;
	}

	printf("End of printEnvironment\n");
}

/* **** The End **** */
