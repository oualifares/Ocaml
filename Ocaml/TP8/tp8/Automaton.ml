(**********************************************************************)
(***************************** Exercice 1 *****************************)
(**********************************************************************)

(* 1.1. *)
(* val interval : int -> int -> int list = <fun> *)

let interval a b = 
	let rec aux a b lst=
		if a > b then lst
		else aux (a+1) b (lst @ [a])
	in aux a b [];;
(*
	let rec interval a b = 
		if a > b then []
		else a :: (interval (a+1) b)
*)

(* 1.2. *)
(* val string_of_list : 'a list -> ('a -> string) -> string = <fun> *)

let string_of_list lst f =
	String.concat "" (List.map f lst);;

(* 1.3. *)
(* val compose_iter : ('a -> 'a) -> 'a -> int -> 'a list = <fun> *)

let rec compose_iter f x n =
	if n = 0 then [x]
	else x :: (compose_iter f (f x) (n-1))

(* 1.4. *)
(* val is_prefix_lists : 'a list -> 'a list -> bool = <fun> *)

let rec is_prefix_lists pref lst =
	if pref = [] then
		true
	else
		if (lst = []) || ((List.hd pref) <> (List.hd lst)) then
			false
		else
			is_prefix_lists (List.tl pref) (List.tl lst)

(* 1.5. *)
(* val is_factor_lists : 'a list -> 'a list -> bool = <fun> *)

let rec is_factor_lists fact lst =
	if is_prefix_lists fact lst then
		true
	else
		if lst = [] then
			false
		else
			is_factor_lists fact (List.tl lst)

(* 1.6. *)
(* val is_subword_lists : 'a list -> 'a list -> bool = <fun> *)

let rec is_subword_lists buf lst =
	if buf = [] then
		true
	else
		if lst = [] then
			false
		else
			if (List.hd buf) = (List.hd lst) then
				is_subword_lists (List.tl buf) (List.tl lst)
			else
				is_subword_lists buf (List.tl lst)

(* 1.7. *)
(* val is_duplicate_free : 'a list -> bool = <fun> *)

let is_duplicate_free lst =
	let rec is_duplicate_free_aux lst buf =
		if lst = [] then
			true
		else
			if is_factor_lists [List.hd lst] buf then
				false
			else
				is_duplicate_free_aux (List.tl lst) ([List.hd lst] @ buf)
	in is_duplicate_free_aux lst []


(**********************************************************************)
(***************************** Exercice 2 *****************************)
(**********************************************************************)

type 'a automaton = {
    ribbon : int -> 'a;
    evol : 'a * 'a * 'a -> 'a;
    void : 'a
}

(* 2.1. *)
(* val create : ('a * 'a * 'a -> 'a) -> 'a -> 'a automaton = <fun> *)

let create evol valeur =
	{ribbon = (fun i -> valeur); 
	evol = evol; 
	void = valeur} ;;

(* 2.2. *)
(* val get_value : 'a automaton -> int -> 'a = <fun> *)

let get_value automate i = automate.ribbon i

(* 2.3. *)
(* val set_value : 'a automaton -> int -> 'a -> 'a automaton = <fun> *)

let set_value automate i v =
	{automate with ribbon = (fun j -> if j = i then v else (automate.ribbon j))};;

(**********************************************************************)
(***************************** Exercice 3 *****************************)
(**********************************************************************)

type bunch = int * int

(* 3.1. *)
(* val get_bunch_values : 'a automaton -> bunch -> 'a list = <fun> *)

let rec get_bunch_values aut (a,b) =
	List.map (fun i -> get_value aut i) (interval a b)


(* 3.2. *)
(* val to_string : 'a automaton -> bunch -> ('a -> string) -> string
    = <fun> *)
let to_string aut bun f =
    string_of_list (get_bunch_values aut bun) f

(* 3.3. *)
(* val has_factor : 'a automaton -> bunch -> 'a list -> bool = <fun> *)

let has_factor aut bun lst =
	is_factor_lists lst (get_bunch_values aut bun)

(* 3.4. *)
(* val has_subword : 'a automaton -> bunch -> 'a list -> bool = <fun> *)

let has_subword aut bun lst =
	is_subword_lists lst (get_bunch_values aut bun)

(**********************************************************************)
(***************************** Exercice 4 *****************************)
(**********************************************************************)

(* 4.1. *)
(* val shift : 'a automaton -> int -> 'a automaton = <fun> *)

let shift aut k =
	{ribbon = (fun i -> aut.ribbon (i+k)); evol = aut.evol; void = aut.void}

(* 4.2. *)
(* val mirror : 'a automaton -> 'a automaton = <fun> *)

let mirror aut =
	{ribbon = (fun i -> aut.ribbon (i * (-1))); evol = aut.evol; void = aut.void}

(* 4.3. *)
(* val map : 'a automaton -> ('a -> 'a) -> 'a automaton = <fun> *)

let map aut f =
	{ribbon = (fun i -> f (aut.ribbon i)); evol = aut.evol; void = aut.void}	

(* 4.4. *)
(* val evolution : 'a automaton -> 'a automaton = <fun> *)

let evolution aut =
	{ribbon = (fun i -> aut.evol ((aut.ribbon (i-1)),(aut.ribbon i),(aut.ribbon (i+1)))); evol = aut.evol; void = aut.void}

(* 4.5. *)
(* val evolutions : 'a automaton -> int -> 'a automaton list = <fun> *)

let rec evolutions aut n =
	if n < 0 then
		[]
	else
		[aut] @ evolutions (evolution aut) (n-1)

(* 4.6. *)
(* val evolutions_bunch : 'a automaton -> bunch -> int -> 'a list list
    = <fun> *)

let evolutions_bunch aut b n =
	List.map (fun a -> get_bunch_values a b) (evolutions aut n)

(* 4.7. *)
(* val is_resurgent : 'a automaton -> bunch -> int -> bool *)

(**********************************************************************)
(***************************** Exercice 5 *****************************)
(**********************************************************************)

(* 5.1. *)
(* val sierpinski : int automaton
    = {ribbon = <fun>; evol = <fun>; void = 0} *)

let sierpinski =
	{ribbon = (fun i -> 0); evol = (fun (a,b,c) -> (a + b + c) mod 2); void = 0}

(* 5.2. *)
(* Type wb. *)
(* val chaos : wb automaton
    = {ribbon = <fun>; evol = <fun>; void = White} *)

type wb = Black | White

let chaos =
	{ribbon = (fun i -> White);
	evol = (fun (a,b,c) -> match (a,b,c) with
								| (Black,Black,_) -> White
								| (Black,White,Black) -> White
								| (Black,White,White) -> Black
								| (White,Black,_) -> Black
								| (White,White,Black) -> Black
								| (White,White,White) -> White
			);
	void = White}

(**********************************************************************)
(***************************** Exercice 6 *****************************)
(**********************************************************************)

(* 6.1. *)

(* 6.2. *)
