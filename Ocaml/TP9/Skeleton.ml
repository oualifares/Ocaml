(* point type definition *)
type point = float * float;;

(* Question 1 *)
(* float -> float -> point *)
let make_point a b :point =  (a,b);;
   

(* Question 2 *)
(* point -> float *)
let point_x : point -> float = fun (x, _) -> x;;

(* point -> float *)
let point_y : point -> float = fun (_, y) -> y;;


(* Question 3 *)
(* point -> point -> bool *)
let point_domination (p1 : point) (p2 : point) =
			if (point_x p1 >= point_x p2) &&  (point_y p1 >= point_y p2) then true 
			else false;; 

(* -------------------------------------------------------------------------- *)

(* rectangle type definition *)
type rectangle = point * point;;


(* Question 4 *)
(* point -> point -> rectangle *)
let make_rectangle (p1 : point) (p2 : point) : rectangle = (p1, p2);;

(* Question 5 *)
(* rectangle -> point *)
let rectangle_lower_left : rectangle -> point = fun ((p1, p2), (_, _)) -> (p1, p2);;

(* rectangle -> point *)
(* rectangle_upper_right *)
let rectangle_upper_right : rectangle -> point = fun ((_, _), (p1, p2)) -> (p1, p2);;

(* Question 6 *)
(* rectangle -> float *)
let rectangle_width : rectangle -> float = fun ((p1, _), (p3, _)) -> p3 -. p1;;

(* rectangle -> float *)
(* rectangle_height *)
let rectangle_height : rectangle -> float = fun ((_, p1), (_, p3)) -> p3 -. p1;; 

(* Question 7 *)
(* rectangle -> point -> bool *)
(* rectangle_contains_point *)

let rectangle_contains_point (r : rectangle ) (p : point) 
			=  if (point_x p <= point_x (rectangle_upper_right r)) && (point_x (rectangle_lower_left r) <= point_x p ) 
			&& (point_y p <= point_y (rectangle_upper_right r)) && ( point_y (rectangle_lower_left r) <= point_y p) then true 
			else false ;;
(* Question 8 *)
(* rectangle -> point list -> point list *)
(* rectangle_contains_points *)

let rectangle_contained_points   (r : rectangle ) lst = 
		List.filter (fun x -> rectangle_contains_point r x = true ) lst ;;

(* -------------------------------------------------------------------------- *)

(* quadtree type definition *)
type quadtree = | Leaf of point list * rectangle
                | Node of quadtree * quadtree * quadtree * quadtree * rectangle;;


(* Question 9 *)
(* rectangle -> rectangle * rectangle * rectangle * rectangle  *)
(* rectangle_split_four *)

let rectangle_split_four (r : rectangle)=
    let p = rectangle_lower_left r and p2 = rectangle_upper_right r in
        let r = make_rectangle (point_x p, (point_y p2) /. 2.) ((point_x p2) /. 2., (point_y p2))
        and r2 = make_rectangle ((point_x p2) /. 2., (point_y p2) /. 2.) (point_x p2, point_y p2)
        and r3 = make_rectangle ((point_x p2) /. 2., point_y p) (point_x p2, (point_y p2) /. 2.)
        and r4 = make_rectangle (point_x p, point_y p) ((point_x p2) /. 2., (point_y p2) /. 2.)
    in 
        (r,r2,r3,r4)

(* Question 10 *)
(* ’a list -> ’a *)
(* smallest *)

let smallest l =
    List.fold_left (fun x y -> if compare x y = -1 then x else y) (hd l) l

(* ’a list -> ’a *)
(* largest *)

let largest l =
    List.fold_left (fun acc x -> if compare acc x = 1 then acc else x) (hd l) l

(* Question 11 *)
(* point list -> rectangle *)
(* enclosing_rectangle *)

let enclosing_rectangle (l) =
    make_rectangle (smallest l) (largest l)

(* Question 12 *)
(* point list -> int -> quadtree *)
(* quadtree_make *)

let quadtree_make points n = 
    let rec aux points rect =
        let (r,r2,r3,r4) = rectangle_split_four(rect) in 
            if length points <= n then  
                Leaf(points,rect)
            else
                Node(aux (rectangle_contains_points r points) r, aux (rectangle_contains_points r2 points) r2, aux (rectangle_contains_points r3 points) r3, aux (rectangle_contains_points r4 points) r4, rect)
    in aux points (enclosing_rectangle points);;

(* Question 13 *)
(* quadtree -> int  *)
(* quadtree_count *)


(* Question 14 *)
(* quadtree -> int list *)
(* quadtree_signature *)


(* -------------------------------------------------------------------------- *)


(* Question 15 *)
(* quadtree -> point list  *)
(* quadtree_all_points *)


(* rectangle -> rectangle -> bool *)
let rectangle_contains_rectangle r1 r2 =
    rectangle_contains_point r1 (rectangle_lower_left r2) &&
    rectangle_contains_point r1 (rectangle_upper_right r2);;

(* rectangle -> rectangle -> bool *)
let rectangle_disjoint (p1,p2) (p3,p4) =
    point_domination p3 p2 || point_domination p1 p4;;

(* rectangle -> rectangle -> rectangle *)
let rectangle_intersection ((p1,p2),(p3,p4))=
    let ll_x_max = largest [point_x p1; point_x p3] in
    let ll_y_max = largest [point_y p1; point_y p3] in
    let ur_x_min = smallest [point_x p2; point_x p4] in
    let ur_y_min = smallest [point_y p2; point_y p4] in
    let ll = make_point ll_x_max ll_y_max in
    let ur = make_point ur_x_min ur_y_min in
        make_rectangle ll ur;;


(* Question 16 *)
(* rectangle -> quadtree -> point list  *)
(* rectangle_query *)



(* -------------------------------------------------------------------------- *)

(* Question 17 *)
(* quadtree -> int -> point -> quadtree *)
(* quadtree_insert *)


(* Question 18 *)
(* quadtree -> point -> quadtree *)
(* quadtree_delete *)
