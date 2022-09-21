/* atrocity/src/char-source.h */

typedef struct {
	char * str;
	int len;
	int i;
} CharSource;

CharSource * createCharSource(char * str);
void freeCharSource(CharSource * cs);
int getNextChar(CharSource * cs);
void rewindOneChar(CharSource * cs);
STRING_BUILDER_TYPE * getIdentifier(CharSource * cs, STRING_BUILDER_TYPE * sb, BOOL * pIsSingleQuoted);
BOOL consumeStr(CharSource * cs, char * str);

/* **** The End **** */
