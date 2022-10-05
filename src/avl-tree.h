/* atrocity/src/avl-tree.h */

/* typedef AVL_TREE_KEY_TYPE ?;
typedef AVL_TREE_VALUE_TYPE ?; */

BINARY_TREE_NODE_TYPE * avlTreeInsertKey(char * key, BINARY_TREE_NODE_TYPE * node);
BINARY_TREE_NODE_TYPE * avlTreeInsertKeyAndValue(char * key, SCHEME_UNIVERSAL_TYPE * value, BINARY_TREE_NODE_TYPE * node);

BINARY_TREE_NODE_TYPE * avlTreeDelete(char * key, BINARY_TREE_NODE_TYPE * node);

void avlTreeInOrderTraversal(BINARY_TREE_NODE_TYPE * node);

BOOL isKeyInAvlTree(char * key, BINARY_TREE_NODE_TYPE * node);
SCHEME_UNIVERSAL_TYPE * lookupValueInAvlTree(char * key, BINARY_TREE_NODE_TYPE * node, SCHEME_UNIVERSAL_TYPE * defaultValue);

int getAvlTreePopulation(BINARY_TREE_NODE_TYPE * node);

/* **** The End **** */
