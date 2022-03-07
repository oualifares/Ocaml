(*============================= factorial ==============================*)

let rec fact n = 
	if (n=0) || (n=1) then  1 
  	else n * fact (n-1);;

(*============================= fibonacci ==============================*)

let rec fibo n = 
	if (n=0) then 0
	else if(n=1) then 1  
  	else fibo(n-1) + fibo (n-2);;

(*============================= plus grand diviseur ==============================*)

let rec pgcd m n  = 
	if (m=0) then n
	else if(m>n) then pgcd n m  
  	else pgcd (n mod m) m;;

(*============================= Fonction d’Ackermann-Péter ==============================*)

let rec ackermann m n  = 
	if (m=0) then n+1
	else if((m>=1) && (n=0)) then ackermann (m-1) 1 
  	else if((m>=1) && (n>=1)) then ackermann (m-1) (ackermann m (n-1))
  	else -1;;

(*============================= Coefficient binomiaux ==============================*)

let rec binom n k = 
	if (k=0) then 1
	else if(n<k) then 0 
  	else (binom (n-1) (k-1)) + (binom (n-1) k);; 

(*============================= Fonctions mutuellement récursives ==============================*)

let rec is_even = function n -> 
		if(n = 0) then true
		else is_odd (n-1)
	and is_odd = function n -> 
		if(n = 0) then false
		else is_even (n-1);;

(*============================= Récursivité terminale 1/2 ==============================*)

let rec aux n acc= 
	if (n=0) || (n=1) then acc 
  	else aux (n-1) (n*acc);;

(*============================= Récursivité terminale 2/2 ==============================*)

let rec aux n acc1 acc2= 
	if (n=0) then acc1
	else if(n=1) then acc2
  	else aux (n-1) acc2 (acc2+acc1);;

(*============================= Conjecture de Syracuse ==============================*)

let rec syracuse n =
	if(n = 1) then 0
	else if(n mod 2 = 0) then 1 + syracuse (n/2)
	else 1 + syracuse (n*3+1);;

(*============================= Exponentielle ==============================*)

let rec exp x n = 
	if n = 0 then 1
	else x * exp x (n-1);; 

let  exp1 x n = 
	let rec aux_exp x n acc = 
	if n = 0 then acc
	else aux_exp x (n-1) (x*acc)
	in aux_exp x n 1;; 	

let rec fast_exp x n =
	if n = 0 then 1
	else if n mod 2 = 0 then let z = fast_exp x (n/2) in let res = z * z in res
	else  let z = fast_exp x (n/2) in let res = z * z * x in res;;

let exp2 x n =
	let rec aux_ex x n cmpt acc = 
	if n = 0 then (acc , cmpt)
    else aux_ex x (n-1) (cmpt + 1) (x*acc)
	in aux_ex x n 0 1;;  

(*============================= Sommes doubles ==============================*)

let rec sum n  = 
	if n <= 1  then 1
	else n + sum (n-1);;

let rec sum1 n m =
	if m <	n then 0
	else if m == n then n
	else m + sum1 n (m-1);;

let rec sum2 n m =
	if m <	n then 0
	else if m == n then m
	else n + sum2 (n+1) m;;

let rec summ n m = 
	if m <	n then 0
	else if m == n then m
	else
	 	let s = summ(n+1) m in let z = n + s in let  res = z + summ n s in res ;;
 	
