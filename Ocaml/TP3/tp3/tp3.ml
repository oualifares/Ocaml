let rec integers_1 n = if n<0 then [] else n :: integers_1(n-1);;
integers_1 12 ;;
(*le probleme cest que le resultat
 est inversé on obtients une liste de n à 0 *)
let rec integers_2 n = if n < 0 then [] else integers_1(n-1) @ [n];;
integers_2 10 ;;

let integers_3 n = List.rev (integers_1 n);;
integers_3 10 ;;

exception Empty_list ;;


let three_or_more l =     
	let rec aux l acc = 
		if (acc >= 3 )then true
		else if (l = [])  then false
		else aux (List.tl l) (acc+1)
	in aux l 0;;

let size l = 
		let rec aux l acc =
		if ( l = []) then acc
		else aux (List.tl l) (acc +1)
	in aux l 0 ;;

let last l =  
	if l = [] then raise Empty_list
	 else  List.hd  ( List.rev (l) ) ;;

let rec is_increasing l =
	if l = [] then true
	else if (List.tl l) = [] then true
	else if(List.hd l ) > (List.hd(List.tl l)) then false
	else is_increasing (List.tl l);;
is_increasing [1;2;3;4];;
let even_odd l = 
    if l = [] then raise Empty_list
    else
        let rec is_even l = (
            if l = [] then true
            else 
                if List.hd l mod 2 != 0 
                    then false
                else is_odd (List.tl l))
        and is_odd l = (
            if l = [] then true
            else 
                if List.hd l mod 2 = 0 
                    then false
                else is_even (List.tl l))
        in (is_even l);; 

even_odd [2;3;4;5];;

let rec find e l = 
    if l = [] then false
    else
        if List.hd l = e
            then true
        else find e (List.tl l);;
find 5 [2;3;4;5];;

let rec member e l = 
    if l = [] then []
    else
        if List.hd l = e
            then l
        else member e (List.tl l);;
member 4 [2;3;4;5];;

let rec member_last e l = 
    if l = [] then []
    else
        if List.hd l = e && find e (List.tl l) = false
            then l
        else member_last e (List.tl l);;

member_last 2 [2;3;2;2;4;5];;

let rec nb_occ e l = 
    if find e (member e l) = false
        then 0
    else 
        if List.hd l = e
            then 1 + nb_occ e (List.tl l)
        else nb_occ e (List.tl l);;

nb_occ 2 [2;3;2;2;2;4;5];;

let rec nth n l = 
    if n = 1
        then List.hd l
    else nth (n-1) (List.tl l);;

nth 3 [1; 2; 3; 4; 3; 5], nth 3 [2; 4; 6];;

let average l = 
    let rec sum l = (
        if l = []
            then 0.
        else List.hd l +. sum (List.tl l)
    )
    in (sum l) /. float_of_int (size l);;

average [5.; 8.5; 11.5; 15.]


let list_copy l = 
	if l = [] then raise Empty_list
	else let rec aux l l1=
		if l = [] then ((List.rev) l1)
		else aux (List.tl l) (List.hd l :: l1)
	in aux l [];;

let rec random_list n max = 
	if n = 0 then []
	else (Random.int max) :: random_list (n-1) max ;;
let l = random_list 10 2;;

let rec reverse l = 
    if l = []
        then []
    else
        reverse (List.tl l) @ [List.hd l];;

reverse l;;
let rec flatten_list l = 
    if l = []
        then []
    else
        List.hd l @ flatten_list (List.tl l);;

flatten_list [[1; 2]; []; [3; 4; 5]; [6]];;

let rec without_duplicates l =
    if l = []
        then []
    else if find (List.hd l) (List.tl l) = false
        then [List.hd l] @ without_duplicates (List.tl l)
    else without_duplicates (List.tl l);;

without_duplicates [0; 0;1; 2; 3; 3; 3; 3; 4; 5; 5; 6; 8; 8];;

let frequences l =
    let rec frequence l elements freq= (
        if elements = []
            then freq
        else
            frequence l (List.tl elements) (freq @ [((List.hd elements), (nb_occ (List.hd elements) l))])
    )
    in (frequence l (without_duplicates l) []);;

frequences l;; 


let rec f_split l  = 
    if l=[] then [],[]
    else if List.tl l=[] then ([List.hd l],[])
    else
 
        let  x = List.hd l and y= List.hd (List.tl l) and q=List.tl (List.tl l) in 
          let (a, b) = f_split q
        in (x::a, y::b);;


let rec f_merge  l1 l2 =  

    if l1=[] then l2
     else if l2=[] then l1
    else 
        let t= List.hd l1 and t'= List.hd l2 and q=List.tl l1 and q'=List.tl l2 in
        if t<t' then t::(f_merge q l2)
        else t':: f_merge l1 q';;
let rec fusion_sort l=
    if l=[] then []
    else if List.tl l=[] then [List.hd l]
    else let (a,b)= f_split l in f_merge (fusion_sort a) (fusion_sort b)


;;
	




