/* atrocity/src/main.c - Started on 2022-08-15 */

/* cd into the src directory. Then: */
/* To compile and link: $ make */
/* To run tests: $ make test */
/* To remove all build products: $ make clean */
/* To do all of the above: $ make clean && make && make test */
/* To run a script: E.g. $ ./atrocity ../scripts/test001.scm */
/* To enter the read-eval-print loop: $ ./atrocity */

/* TODO: Add this stuff:
Dot (i.e. '.'; e.g. (cons 1 2) -> (1 . 2) : A pair, but not a list.)
QuoteKeyword (e.g. for (quote 1 2 3))
Real (i.e. floating-point) numbers? Then sin cos tan atan2 pow ln exp
floor
string< (string-is-less-than)
tostring ?
stringtolist ?
stringtosymbol ?
Default values for formal parameters
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
/* #include <ctype.h> */
/* #include <assert.h> */

#include "types.h"

#include "char-source.h"
#include "input-output.h"
#include "memory-manager.h"
#include "parse-and-evaluate.h"
#include "tests.h"

/* **** The Main MoFo **** */

int main(int argc, char * argv[]) {
	BOOL enableTests = FALSE;
	BOOL enableVersion = FALSE;
	char * filename = NULL;
	int i;

	/* Use the current time to seed the random number generator: */
	srand(time(NULL));

	for (i = 1; i < argc; ++i) {
		/* printf("argv[%d] = %s\n", i, argv[i]); */

		if (!strcmp(argv[i], "-t")) {
			enableTests = TRUE;
		} else if (!strcmp(argv[i], "-v")) {
			enableVersion = TRUE;
		} else if (filename == NULL && argv[i][0] != '-') {
			filename = argv[i];
		}
	}

	if (enableVersion) {
		printf("\nAtrocity version 0.0.0\n");
	} else if (enableTests) {
		runTests();
	} else if (filename != NULL) {
		execScriptInFile(filename, NULL);
	} else {
		readEvalPrintLoop();
	}

	const int numFreed = freeAllStructs();

	printf("gc final: %d block(s) of memory freed.\n", numFreed);

	mmPrintReport();

	return 0; /* Zero (as a Unix exit code) means success. */
}

/* **** The End **** */
