/* atrocity/src/avl-tree.c */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"

#include "create-and-destroy.h"
#include "print.h"

/* AVL height-balanced tree */

/* This tree can be used as either a set of strings
or a dictionary with string-type keys and any type of values */

static BOOL isKeyLessThan(char * key1, char * key2) {
	return strcmp(key1, key2) < 0;
}

static BINARY_TREE_NODE_TYPE * createBinaryTreeLeaf() {
	return NULL;
}

static BOOL isBinaryTreeLeaf(BINARY_TREE_NODE_TYPE * node) {
	return node == NULL;
}

static int treeHeight(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return 0;
	}

	/* const int lheight = treeHeight(getLeftSubtree(node));
	const int rheight = treeHeight(getRightSubtree(node));

	return ((lheight > rheight) ? lheight : rheight) + 1; */

	return getHeightInBinaryTree(node);
}

static int calculateTreeHeightAndVerifyBalance(BINARY_TREE_NODE_TYPE * node, int depth, char * failMsg) {

	if (isBinaryTreeLeaf(node)) {
		return 0;
	}

	if (failMsg == NULL) {
		failMsg = "calculateTreeHeightAndVerifyBalance() : AVL tree is not balanced";
	}

	const int lheight = calculateTreeHeightAndVerifyBalance(getLeftSubtree(node), depth + 1, failMsg);
	const int rheight = calculateTreeHeightAndVerifyBalance(getRightSubtree(node), depth + 1, failMsg);
	const int balance = abs(lheight - rheight);

	if (balance > 1) {
		fprintf(stderr, "calculateTreeHeightAndVerifyBalance: Imbalance at depth %d: lheight = %d; rheight = %d\n", depth, lheight, rheight);
	}

	failIf(balance > 1, failMsg);

	const int height = ((lheight > rheight) ? lheight : rheight) + 1;

	/* Test 2022-10-05 */
	failIf(height != getHeightInBinaryTree(node), "calculateTreeHeightAndVerifyBalance: Calculated node height != stored node height");

	/* getHeightInBinaryTree(node) = height; */

	return height;
}

/*   A            B
 *  / \          / \
 * a   B   ->   A   c
 *    / \      / \
 *   b   c    a   b
 *
fun RotateLeft Lf = Lf
	| RotateLeft (Br(_,_,Lf)) = Lf  (* Error *)
	| RotateLeft (Br((k1,_,x1),ltree,(Br((k2,_,x2),rltree,rrtree)))) =
		NewNode(k2,x2,NewNode(k1,x1,ltree,rltree),rrtree); */

static BINARY_TREE_NODE_TYPE * rotateLeft(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeLeaf();
	}

	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);

	failIf(isBinaryTreeLeaf(rtree), "rotateLeft() : RightSubtree is a leaf node");

	char * key1 = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value1 = getValueInBinaryTree(node);
	char * key2 = getKeyInBinaryTree(rtree);
	SCHEME_UNIVERSAL_TYPE * value2 = getValueInBinaryTree(rtree);
	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);
	BINARY_TREE_NODE_TYPE * rltree = getLeftSubtree(rtree);
	BINARY_TREE_NODE_TYPE * rrtree = getRightSubtree(rtree);

	BINARY_TREE_NODE_TYPE * newLtree = createBinaryTreeNode(key1, value1, ltree, rltree);

	return createBinaryTreeNode(key2, value2, newLtree, rrtree);
}

/*     B        A
 *    / \      / \
 *   A   c -> a   B
 *  / \          / \
 * a   b        b   c
 *
fun RotateRight Lf = Lf
	| RotateRight (Br(_,Lf,_)) = Lf  (* Error *)
	| RotateRight (Br((k1,_,x1),(Br((k2,_,x2),lltree,lrtree)),rtree)) =
		NewNode(k2,x2,lltree,NewNode(k1,x1,lrtree,rtree)); */

static BINARY_TREE_NODE_TYPE * rotateRight(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeLeaf();
	}

	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);

	failIf(isBinaryTreeLeaf(ltree), "rotateRight() : LeftSubtree is a leaf node");

	char * key1 = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value1 = getValueInBinaryTree(node);
	char * key2 = getKeyInBinaryTree(ltree);
	SCHEME_UNIVERSAL_TYPE * value2 = getValueInBinaryTree(ltree);
	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);
	BINARY_TREE_NODE_TYPE * lltree = getLeftSubtree(ltree);
	BINARY_TREE_NODE_TYPE * lrtree = getRightSubtree(ltree);

	BINARY_TREE_NODE_TYPE * newRtree = createBinaryTreeNode(key1, value1, lrtree, rtree);

	return createBinaryTreeNode(key2, value2, lltree, newRtree);
}

/* fun RotateRightLeft Lf = Lf
	| RotateRightLeft (Br((k,_,x),ltree,rtree)) =
		RotateLeft(NewNode(k,x,ltree,RotateRight(rtree))); */

static BINARY_TREE_NODE_TYPE * rotateRightLeft(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeLeaf();
	}

	char * key = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value = getValueInBinaryTree(node);
	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);
	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);

	return rotateLeft(createBinaryTreeNode(key, value, ltree, rotateRight(rtree)));
}

/* fun RotateLeftRight Lf = Lf
	| RotateLeftRight (Br((k,_,x),ltree,rtree)) =
		RotateRight(NewNode(k,x,RotateLeft(ltree),rtree)); */

static BINARY_TREE_NODE_TYPE * rotateLeftRight(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeLeaf();
	}

	char * key = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value = getValueInBinaryTree(node);
	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);
	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);

	return rotateRight(createBinaryTreeNode(key, value, rotateLeft(ltree), rtree));
}

/* In Br( (k, 1, x), Lf, Lf ), the 1 appears to be the subtree height.

fun AVLInsert (k, x, Lf) = Br( (k, 1, x), Lf, Lf )
  | AVLInsert (k, x, Br((k1, h1, x1), ltree, rtree)) =
		if Order.less(k,k1) then  (* x goes into ltree *)
      let
		    val newltree = AVLInsert(k,x,ltree)
				val newtree = NewNode(k1,x1,newltree,rtree)
      in
		    if TreeHeight(newltree) <= TreeHeight(rtree) + 1 then
				  newtree
        else if Order.less(k,NodeKey(newltree)) then
		      RotateRight(newtree)
				else
          RotateLeftRight(newtree)
		  end
    else if Order.less(k1,k) then
		  let
				val newrtree = AVLInsert(k,x,rtree)
        val newtree = NewNode(k1,x1,ltree,newrtree)
		  in
				if TreeHeight(newrtree) <= TreeHeight(ltree) + 1 then
          newtree
		    else if Order.less(NodeKey(newrtree),k) then
				  RotateLeft(newtree)
        else
		      RotateRightLeft(newtree)
      end
		else
			Br((k1, h1, x), ltree, rtree); (* Just replace the contents *) */

static BINARY_TREE_NODE_TYPE * avlTreeInsertHelper(char * key, SCHEME_UNIVERSAL_TYPE * value, BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeNode(key, value, createBinaryTreeLeaf(), createBinaryTreeLeaf());
	}

	char * key1 = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value1 = getValueInBinaryTree(node);
	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);
	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);

	if (isKeyLessThan(key, key1)) {
		BINARY_TREE_NODE_TYPE * newltree = avlTreeInsertHelper(key, value, ltree);
		BINARY_TREE_NODE_TYPE * newtree = createBinaryTreeNode(key1, value1, newltree, rtree);

		if (treeHeight(newltree) <= treeHeight(rtree) + 1) {
			return newtree;
		} else if (isKeyLessThan(key, getKeyInBinaryTree(newltree))) {
			return rotateRight(newtree);
		} else {
			return rotateLeftRight(newtree);
		}
	} else if (isKeyLessThan(key1, key)) {
		BINARY_TREE_NODE_TYPE * newrtree = avlTreeInsertHelper(key, value, rtree);
		BINARY_TREE_NODE_TYPE * newtree = createBinaryTreeNode(key1, value1, ltree, newrtree);

		if (treeHeight(newrtree) <= treeHeight(ltree) + 1) {
			return newtree;
		} else if (isKeyLessThan(getKeyInBinaryTree(newrtree), key)) {
			return rotateLeft(newtree);
		} else {
			return rotateRightLeft(newtree);
		}
	} else {
		/* Just replace the contents: Replace value1 with value */
		return createBinaryTreeNode(key1, value, ltree, rtree);
	}
}

BINARY_TREE_NODE_TYPE * avlTreeInsertKey(char * key, BINARY_TREE_NODE_TYPE * node) {
	calculateTreeHeightAndVerifyBalance(node, 0, "AVL pre-Insert key: Balance check failed");

	BINARY_TREE_NODE_TYPE * result = avlTreeInsertHelper(key, NULL, node);

	calculateTreeHeightAndVerifyBalance(result, 0, "AVL post-Insert key: Balance check failed");

	return result;
}

BINARY_TREE_NODE_TYPE * avlTreeInsertKeyAndValue(char * key, SCHEME_UNIVERSAL_TYPE * value, BINARY_TREE_NODE_TYPE * node) {
	calculateTreeHeightAndVerifyBalance(node, 0, "AVL pre-Insert key and value: Balance check failed");

	BINARY_TREE_NODE_TYPE * result = avlTreeInsertHelper(key, value, node);

	calculateTreeHeightAndVerifyBalance(result, 0, "AVL post-Insert key and value: Balance check failed");

	return result;
}

/* fun SimpleBalance Lf = Lf
  | SimpleBalance (Br((k,h,x),ltree,rtree)) =
		let
			val tree = Br((k,h,x),ltree,rtree)
			val lh = TreeHeight( ltree )
			val rh = TreeHeight( rtree )
		in
			if lh > rh + 1 then
				RotateRight( tree )
			else if rh > lh + 1 then
				RotateLeft( tree )
			else
				tree
		end */

static BINARY_TREE_NODE_TYPE * simpleBalance(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeLeaf();
	}

	char * key = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value = getValueInBinaryTree(node);
	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);
	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);
	const int lh = treeHeight(ltree);
	const int rh = treeHeight(rtree);

	if (lh > rh + 1) {
		/* ThAW 2022-10-04 BEGIN INSERT 1 */
		const int llh = treeHeight(getLeftSubtree(ltree));
		const int rlh = treeHeight(getRightSubtree(ltree));

		if (llh < rlh) {
			node = createBinaryTreeNode(key, value, rotateLeft(ltree), rtree);
		}
		/* ThAW 2022-10-04 END INSERT 1 */

		return rotateRight(node);
	} else if (rh > lh + 1) {
		/* ThAW 2022-10-04 BEGIN INSERT 2:
		Sometimes we may want to rotateRight(getRightSubtree(node))
		before we do the rotateLeft(node) below. E.g. when:
		treeHeight(getLeftSubtree(node)) == n
		treeHeight(getLeftSubtree(getRightSubtree(node))) == n + 2
		treeHeight(getRightSubtree(getRightSubtree(node))) == n + 1
		*/
		const int lrh = treeHeight(getLeftSubtree(rtree));
		const int rrh = treeHeight(getRightSubtree(rtree));

		if (lrh > rrh) {
			node = createBinaryTreeNode(key, value, ltree, rotateRight(rtree));
		}
		/* ThAW 2022-10-04 END INSERT 2 */

		return rotateLeft(node);
	} else {
		return node;
	}
}

/* fun AVLDelete (_, Lf) = Lf
  | AVLDelete (k, (Br((k1,h1,x1),ltree,rtree))) =
		if Order.less(k,k1) then
      SimpleBalance( NewNode( k1, x1, AVLDelete( k, ltree ), rtree ) )
		else if Order.less(k1,k) then
      SimpleBalance( NewNode( k1, x1, ltree, AVLDelete( k, rtree ) ) )
		else if IsEmpty(ltree) then
      rtree
		else if IsEmpty(rtree) then
      ltree
		else if TreeHeight(ltree) < TreeHeight(rtree) then
      AVLDelete( k, RotateLeft( Br((k1,h1,x1),ltree,rtree) ) )
		else
      AVLDelete( k, RotateRight( Br((k1,h1,x1),ltree,rtree) ) ); */

static BINARY_TREE_NODE_TYPE * avlTreeDeleteHelper(char * key, BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return createBinaryTreeLeaf();
	}

	char * key1 = getKeyInBinaryTree(node);
	SCHEME_UNIVERSAL_TYPE * value1 = getValueInBinaryTree(node);
	BINARY_TREE_NODE_TYPE * ltree = getLeftSubtree(node);
	BINARY_TREE_NODE_TYPE * rtree = getRightSubtree(node);

	if (isKeyLessThan(key, key1)) {
		return simpleBalance(createBinaryTreeNode(key1, value1, avlTreeDeleteHelper(key, ltree), rtree));
	} else if (isKeyLessThan(key1, key)) {
		return simpleBalance(createBinaryTreeNode(key1, value1, ltree, avlTreeDeleteHelper(key, rtree)));
	} else if (isBinaryTreeLeaf(ltree)) {
		return rtree;
	} else if (isBinaryTreeLeaf(rtree)) {
		return ltree;
	} else if (treeHeight(ltree) < treeHeight(rtree)) {
		return avlTreeDeleteHelper(key, rotateLeft(node));
	} else {
		return avlTreeDeleteHelper(key, rotateRight(node));
	}
}

BINARY_TREE_NODE_TYPE * avlTreeDelete(char * key, BINARY_TREE_NODE_TYPE * node) {
	printf("\nDeleting key '%s'...\n", key);

	calculateTreeHeightAndVerifyBalance(node, 0, "AVL pre-Delete: Balance check failed");

	BINARY_TREE_NODE_TYPE * result = avlTreeDeleteHelper(key, node);

	calculateTreeHeightAndVerifyBalance(result, 0, "AVL post-Delete: Balance check failed");

	return result;
}

/* fun AVLVerifyTree( Lf ) = true
  | AVLVerifyTree( Br( (k, h, _), ltree, rtree ) ) =
		let
      val lh = TreeHeight( ltree )
		  val rh = TreeHeight( rtree )
    in
		  h >= lh + 1 andalso h >= rh + 1 andalso
      ( (h = lh + 1 andalso rh + 1 >= lh) orelse
		    (h = rh + 1 andalso lh + 1 >= rh) ) andalso
			( IsEmpty(ltree) orelse Order.less(NodeKey(ltree),k) ) andalso
			( IsEmpty(rtree) orelse Order.less(k,NodeKey(rtree)) ) andalso
      AVLVerifyTree( ltree ) andalso AVLVerifyTree( rtree )
		end
 */

void avlTreeInOrderTraversal(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return;
	}

	avlTreeInOrderTraversal(getLeftSubtree(node));

	char * valueAsString = "<null>";
	SCHEME_UNIVERSAL_TYPE * value = getValueInBinaryTree(node);
	STRING_BUILDER_TYPE * sb = NULL;

	if (value != NULL) {
		sb = printValueToString(NULL, value);
		valueAsString = sb->name;
	}

	printf("Key: '%s' -> Value: '%s'\n", getKeyInBinaryTree(node), valueAsString);

	avlTreeInOrderTraversal(getRightSubtree(node));
}

BOOL isKeyInAvlTree(char * key, BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return FALSE;
	}

	/* Binary search */
	const int comparison = strcmp(key, getKeyInBinaryTree(node));

	if (comparison < 0) {
		return isKeyInAvlTree(key, getLeftSubtree(node));
	} else if (comparison > 0) {
		return isKeyInAvlTree(key, getRightSubtree(node));
	} else {
		return TRUE;
	}
}

SCHEME_UNIVERSAL_TYPE * lookupValueInAvlTree(char * key, BINARY_TREE_NODE_TYPE * node, SCHEME_UNIVERSAL_TYPE * defaultValue) {

	if (isBinaryTreeLeaf(node)) {
		return defaultValue;
	}

	/* Binary search */
	const int comparison = strcmp(key, getKeyInBinaryTree(node));

	if (comparison < 0) {
		return lookupValueInAvlTree(key, getLeftSubtree(node), defaultValue);
	} else if (comparison > 0) {
		return lookupValueInAvlTree(key, getRightSubtree(node), defaultValue);
	} else {
		return getValueInBinaryTree(node);
	}
}

int getAvlTreePopulation(BINARY_TREE_NODE_TYPE * node) {

	if (isBinaryTreeLeaf(node)) {
		return 0;
	}

	return getAvlTreePopulation(getLeftSubtree(node)) + getAvlTreePopulation(getRightSubtree(node)) + 1;
}

/* **** The End **** */
