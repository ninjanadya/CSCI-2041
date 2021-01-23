(*
Nadya Postolaki
CSCI 2041
Lab 4
*)

(*
1) Check case if BST is empty, return empty BST/raise BadEmptyBst error

2) Check case if key < root, if yes and BST leftsubtree empty, return BST/raise BadEmptyBst error
	OR
Check case if key > root, if yes and BST rightsubtree empty, return BST/raise BadEmptyBst error

3) Check case if key < root/current, if yes, look in leftsubtree and recurse through match statement
	OR 
Check case if key > root/current, if yes, look in rightsubtree and recurse through match statement


*)

let rec bstDelete tree key=
	match tree with
		BstEmpty -> BstEmpty| (*If empty tree, then it returns an empty tree*)
		
		BstNode(otherKey, BstEmpty, BstEmpty) -> BstEmpty|

		BstNode(otherKey, leftSubtree, rightSubtree) when key < otherKey -> (*two children*)
			BstNode(otherKey, (bstDelete leftSubtree key), rightSubtree) |

		BstNode(otherKey, leftSubtree, rightSubtree) when key > otherKey ->
			BstNode(otherKey, leftSubtree, (bstDelete rightSubtree key)) |

		BstNode(otherKey, BstEmpty, rightSubtree) -> 
			if 	key != otherKey		
				then BstNode(otherKey, BstEmpty, (bstDelete rightSubtree key))  (*one child*)
			else
				rightSubtree
			|
		BstNode(otherKey, leftSubtree, BstEmpty) -> 
			if key != otherKey			
				then BstNode(otherKey, (bstDelete leftSubtree key), BstEmpty)   (*one child*)
			else
				leftSubtree
			|
		BstNode(otherKey, leftSubtree, rightSubtree) ->
			let newKey = bstMaxKey leftSubtree in
			BstNode(newKey, (bstDelete leftSubtree newKey), rightSubtree);;





