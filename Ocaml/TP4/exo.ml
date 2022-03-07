
								(*exercice 1*)
type bintree=
    |Empty
    |Node of int*bintree*bintree;;
   
let example_tree= Node(1,Node(2,Node(4,Empty,Empty),Node(5,Node(7,Empty,Empty),Node(8,Empty,Empty))),Node(3,Empty,Node(6,Node(9,Empty,Empty),Empty)));;
 (***********************************************************************************)
							 (*exercice 2*)
 (*question 1*)
let rec bintree_count_nodes node=
		match node with
		|Empty-> 0
		| Node(_,l,r)-> 1+bintree_count_nodes l + bintree_count_nodes r;;

 (*question 2*)
let rec bintree_count_leaves node= match node with
	|Empty->0
	|Node(_,Empty,Empty)->1
	|Node(_,l,r)->bintree_count_leaves l +bintree_count_leaves r;;

 (*question 3*)
let rec  bintree_count_internal_nodes node=match node with
	| Empty-> 0
	| Node(_,Empty,Empty) ->0
	|Node(_,l,r)->1+bintree_count_internal_nodes l +bintree_count_internal_nodes r;;

 (*question 4*)
let rec bintree_count_right node= match node with
	|Empty  -> 0
	|Node(_,l,Empty) ->bintree_count_right l 
	|Node(_,l,r) ->1+bintree_count_right l +bintree_count_right r;;

 (*question 5*)		
let rec bintree_count_left  node= match node with
	|Empty  -> 0
	|Node(_,Empty,r) ->bintree_count_right r
	|Node(_,l,r) ->1+bintree_count_right l +bintree_count_right r;;


(***************************************************************************)
						(*exrcice 3*)
 (*question 1*)

let max a b=if a>b then a else b;;


let rec bintree_height node=match node with
	|Empty-> 0
	
	|Node(_,r,l)->1+ max (bintree_height r) (bintree_height l );;



 (*question 2*)
let rec bintree_is_mirror tree1 tree2= match tree1,tree2 with
	| (Empty,Empty) -> true
	| (Empty,Node(_,_,_))-> false
	| (Node(_,_,_),Empty)-> false
	| Node(_,l,r),Node(_,l1,r1)-> bintree_is_mirror l r1 && bintree_is_mirror r l1;;




 (*question 3*)
let  bintree_is_symmetric node= match node with
		|Empty ->true
		|Node(_,l,r)-> bintree_is_mirror l r;; 

(**********************************************************************************)
								(*exercice 4*)

(*question 1*)
let rec bintree_collect_nodes node=match node with
	| Empty-> []
	 |Node (v, t1, t2) -> v ::((bintree_collect_nodes t1) @ (bintree_collect_nodes t2));;

(*question 2*)
let rec bintree_collect_leaves node= match node with
	| Empty -> []
	| Node(v,Empty,Empty) -> [v]
	|Node(_,r,l)->((bintree_collect_leaves r) @ (bintree_collect_leaves l));;

(*question 3*) 
let rec  bintree_collect_internal_nodes node= match node with
	| Empty->[]
	| Node(v,Empty,Empty)->[]
	|Node(v,r,l)->v::( (bintree_collect_internal_nodes r)@( bintree_collect_internal_nodes l));;

(*question 3*)
let rec bintree_collect_level node n=match node  ,n with
	|Empty ,n ->[]
	|Node(v,l,r), 1->[v]
	|Node(v,l,r),n->((bintree_collect_level l (n-1))@(bintree_collect_level r (n-1)));;

(*question 4*)

let rec bintree_collect_canopy node= match node with
		|Empty->[]
		|Node(v,Node(v1,Empty,Empty),Node(v2,Empty,Empty))->[0;1]
		|Node(v,Node(v1,Empty,Empty),r)->[0]@(bintree_collect_canopy r)
		|Node(v,l,Node(v1,Empty,Empty))->[1]@(bintree_collect_canopy l) 
		|Node(_,l,r)->(bintree_collect_canopy l)@(bintree_collect_canopy r);;
(*********************************************************************)
						(*exercice 5*)

(*question 1*)


let  rec bintree_pre node =match node with
		|Empty ->[]
		|Node(v,l,r)->[v]@(bintree_pre l )@ (bintree_pre r );;

bintree_pre example_tree;;
(*question 2*)
let rec bintree_in node=match node with
		| Empty -> []
		|Node(v,l,r)->(bintree_in l )@ [v]@(bintree_in r );;


bintree_in example_tree;;

(*question 3*)

let rec bintree_post node=match node with
		| Empty -> []
		|Node(v,l,r)->(bintree_pre l )@ (bintree_pre r )@[v];;


bintree_post example_tree;;


(**************************************************************************************)
									(*exercice 6*)

(*question 1*)


let rec bintree_insert node value=match node with 
	|Empty->Node(value,Empty,Empty)
	|Node(v,l,r)->if v<value then Node(v,l,bintree_insert r value )
					else Node(v,bintree_insert l value ,r);;

 

let rec is_sorted root= match root with
	|[]|[_]->true
	|t::t1::q->t<=t1 && is_sorted q;;
let bintree_search node=is_sorted (bintree_in node);;

(*********************************************************************************************)
							(*exercice 7*)
(*question 1*)
let rec bintree_double node=match node with
	| Empty ->Empty
	|Node(v,l,r)->Node(2*v,bintree_double l,bintree_double r);;

							 
(*question 2*)
let rec bintree_apply f node=match node with
	| Empty ->Empty
	|Node(v,l,r)->Node(f v,bintree_apply f l,bintree_apply f r);;

 let f n= 2*n;;

(*question 3*)
let rec bintree_rotate node=match node with
		|Empty->node
		| Node(v,l,r)->Node(v,bintree_rotate r,bintree_rotate l);;
 
 (*question 4*)
 let rec sum node=match node with
 	|Empty->0
 	|Node(v,l,r)->v+sum l+sum r;;

 let rec bintree_sum_subtree node= match node with
 	|Empty->node
 	|Node(v,l,r)->Node(v+sum l+sum r ,bintree_sum_subtree l,bintree_sum_subtree r);;



bintree_sum_subtree example_tree;;