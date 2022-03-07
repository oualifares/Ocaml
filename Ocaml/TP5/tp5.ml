(* Aounallah Salma TP5*)

(*les parenthese à la fin en caml sert à rien, on peut les enlever
 *map ca parcours toute la liste et ca applique la fct sur chaque element
 *filter ca parcours la liste et ca renvoie une liste 
 dans laquelle il y a just les element pour laquelles ton prédicats est vrai	
*)

(*============================= Excercie1 ==============================*)

let sqr x = x * x;;
let my_list = [3; 12; 3; 40; 6; 4; 6; 0];;

(*============================= question2 ==============================*)

let f_sum f a b =
	f a + f b;;

(*============================= question3 ==============================*)

let new_f_sum f a =
	fun b -> f a + f b;;

(*============================= question4 ==============================*)

let f1 f a =
	fun b -> a * b;;
	
let f2 f =
	f 1 + 2;;
	
let f3 f a=
	f a + f 0;;
	
let f4 f a= (*les parenthese à la fin en caml sert à rien, on peut les enlever*)
	f a + f 0;;
	
let f5 f =
	f (fun a -> a + 3) +1;;
	
(*============================= question5 ==============================*)

let list_carre = List.map sqr my_list;;

(*============================= question6 ==============================*)

let list_doube =  List.map (( * ) 2) my_list;;

(*============================= question7-8 ==============================*)

let make_list make n =
	let rec make_list_aux n lst =
		if n = 0 then
			lst
		else
			make_list_aux (n-1) (lst @ [make ()])
	in make_list_aux n [];;

(*============================= question9 ==============================*)

let list_random = make_list (function ()-> Random.bool ()) 64;;

(*============================= question10 ==============================*)

let list_int = make_list (function ()-> Random.int 101) 16;;


(*============================= Excercie2 ==============================*)

let entiers = [2; 5; 7; 3; 12; 4; 9; 2; 11];;
let animaux = ["Wombat"; "aXolotl"; "pangolin"; "suricate"; "paresseuX"; "quoKKa"; "lemurien"];;

(*============================= question1 ==============================*)

(*1er méthode*)
let rec longueur_chaine_list lst =
	match lst with
		|[]->[]
		|str :: lst' -> (String.length str) :: (longueur_chaine_list lst');;

(*2ème methode*)
let list_length = List.map (function a -> String.length a) animaux;;

(*============================= question2 ==============================*)

let list_maj = List.map (function a -> String.uppercase a) animaux;;

(*============================= question3 ==============================*)

let list_min = List.filter (function a -> (a = String.lowercase a)) animaux;;

(*============================= question4 ==============================*)

let list_paire = List.filter (function a -> (String.length a mod 2 = 0)) animaux;;

(*============================= question5 ==============================*)

let liste = List.map (function a -> 
				if a mod 2 = 0 then
					(a, "pair")
				else
					(a,"impair")
			) entiers;;

(*============================= question6 ==============================*)

let list_n = List.map (function a -> (make_list (function b -> a) a)) entiers;;

(*============================= question7 ==============================*)

let list_get = List.exists (function a -> String.get a 0 = 's') animaux;;

(*============================= question8 ==============================*)

let list_2mod5 = List.for_all (function a -> String.length a mod 5 = 2) animaux;;

(*============================= Excercie3 ==============================*)
(*============================= question1 ==============================*)

let sum l = List.fold_left (fun x y -> x + y) 0 l

(*============================= question2 ==============================*)

let size l = List.length l

(*============================= question3 ==============================*)

let last l = List.nth l (size l - 1)

(*============================= question4 ==============================*)

let nb_occ e l = List.fold_left (fun x y -> if y = e then x + 1 else x) 0 l

(*============================= question5 ==============================*)

let max_list l = List.fold_left (fun x y -> if y > x then y else x) (List.hd l) l

(*============================= question6 ==============================*)

let average l = (List.fold_left (fun x y -> x + y) 0 l) / List.length l

(*============================= Excercie4 ==============================*)
(*============================= question1 ==============================*)

let rec my_for_all p l = 
	if l = [] then [] 
	else [p (List.hd l)] @ my_for_all p (List.tl l)

(*============================= question2/3 ==============================*)

let my_for_all2 p l = List.fold_left (fun x y -> x @ [p y]) [] l

(*============================= question4 ==============================*)

let my_for_all3 p l = List.fold_right (fun x y -> [p x] @ y) l []

(*============================= question5 ==============================*)

let my_exists p l = List.exists (fun a -> (p a) = true) l

(*============================= question6 ==============================*)

let rec none p l =
	if l = [] then true
	else
		if p (List.hd l) = true then false
		else none p (List.tl l)

(*============================= question7 ==============================*)

let not_all p l = List.for_all p l

(*============================= question8 ==============================*)

let rec ordered p l = 
	if List.tl l = [] then true 
	else 
		if p (List.hd l) (List.hd (List.tl l)) = false then false 
		else ordered p (List.tl l);; 

(*============================= question9 ==============================*)

let filter2 p l1 l2 = 
	let rec aux_filter2 p l1 l2 lst = 
		if l1 = [] || l2 = [] then lst 
		else 
			if p (List.hd l1) (List.hd l2) = false then aux_filter2 p (List.tl l1) (List.tl l2) lst 
			else lst @ [((List.hd l1),(List.hd l2))] 
	in aux_filter2 p l1 l2 [];; 

(*============================= Excercie5 ==============================*)

let rec insert x l = match l with
    | [] -> [[x]]
    | h::t -> (x::l) :: (List.map (fun y -> h::y) (insert x t));;
    

let rec perm l = match l with
    | h::t -> List.flatten( List.map (insert h) (perm t))
    | _ -> [l];;

(*============================= Excercie6 ==============================*)

type bintree = Empty 
			| Node of int * bintree * bintree;;

let example_tree =
	Node(1,
		Node(2,
			Node(4, Empty, Empty),
		Node(5, Node(7, Empty, Empty), Node(8, Empty, Empty))),
		Node(3, Empty, Node(6, Node(9, Empty, Empty), Empty)));;

(*============================= question1 ==============================*)

let rec tree_map p a =
	match a with
			| Empty -> Empty
			| Node(r, Empty, Empty)-> Node(p r, Empty , Empty)
			| Node(r, g, d)-> Node(p r ,tree_map p g, tree_map p d);;

(*============================= question2 ==============================*)

let rec fold_map f a x =
	match a with
			| Empty -> x
			| Node(r, g, d) -> f r (fold_map f g x) (fold_map f d x);;

(*============================= question3-a ==============================*)
 
let bintree_count_internal_nodes t =
	fold_map (fun a b c -> 1 + b + c) t 0;; 

(*============================= question3-b ==============================*)

let bintree_collect_internal_nodes t =
	fold_map (fun a b c -> [a] @ b @ c) t [];; 
