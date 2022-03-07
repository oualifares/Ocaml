(*avec entier*)
let averge x y z = (x+y+z)/3 ;;
averge 5 5 5 ;;

(*avec flottant*)

let averge x y z = (x+. y+. z)/. 3.;;
averge 5. 5. 5.5 ;;

 (*question2*)
let implies a b = 
 	if (a = true && b = false)then false else true ;;

implies true true ;;
implies true false ;;
implies false true ;;
implies false false ;;

(*question3*)
let inv (a,b) = (b,a) ;;
inv (5,4);;
inv("igm","uge");;

let inv (a,b)= 
	(snd(a,b),fst(a,b));;
inv (51,04);;
inv("ratp","sncf");;

(*question4*)
let inv2 (a,b) =
	(snd(a,b+0),fst(a+0,b));;
inv2 (6 ,5);;
(*question5*)
let f_one () = 1;;
f_one ();;

(*****Question6	*******)
let m = 3;; (*renvoie 3*)
let f x = x;; (*renvoie la valeur de x*)
let g x = x + m;; (*renvoie la valeur de x + 3*)
f 4;; (*affiche 4*)
g 4;; (*affiche 7 car on fait 4 + 3*)
let m = 5;; (*renvoie 5*)
g 4;; (* affiche toujours 7 car la fonction (let m = 5;;) est apres la fonction g donc g fait appel a la
    	fonction m qu'in a declaré avant g et pas celle d'apres *)
f m;; (*affiche 5*)