#load "graphics.cma";;

open Graphics;;

(* Dimensions of the graphical window. *)
let w = 512 and h = 512

(* Type for the representation of functional images. *)
type picture  = int * int -> Graphics.color

(* render f : draw the image `f` on the graphical window. *)
let render (f : picture) =
    open_graph (Printf.sprintf " %dx%d" w h);
    auto_synchronize false;
    for x = 0 to w - 1 do
        for y = 0 to h - 1 do
            set_color (f (x, y));
            plot x y
        done;
        set_color background
    done;
    synchronize ();
    (wait_next_event [Button_down; Key_pressed]) |> ignore; 
    close_graph ()

let from_polar (rho, theta) =
    (int_of_float (rho *. cos theta), int_of_float (rho *. sin theta)) 

let to_polar (x, y) =
    let distance_to_origin (x, y) = sqrt (float_of_int (x * x + y * y)) in
    (distance_to_origin (x, y), atan2 (float_of_int y) (float_of_int x))


let diagonal graph : picture  =
	(fun (x, y) -> 
		if x = y then
			graph
		else
			background);;


(*let diagonal (graph : picture) (x, y)=
		if x = y then
			graph
		else
			background;;*)


let square cote col (x, y) =
	if 0 <= x && x < cote && 0 <= y && y < cote then
		col
	else
		background;;


let rectangle col largeur haut (x, y) =
	if 0 <= x && x < largeur && 0 <= y && y < haut then
		col
	else
		background;;


let disk rayon col (x, y) =
	if (x * x) + (y * y) <= rayon * rayon then
		col
	else
		background;;



let circle rayon col (x, y) =
	if (x * x) + (y * y) = rayon * rayon then
		col
	else
		background;;

let move (img : picture) (delta_x, delta_y) = 
	(fun (x, y) -> img (x + delta_x, y + delta_y));;


let vertical_symetry img : picture =
	(fun (x, y) -> img (-x, y));;


let honrizontal_symetry img =
	(fun (x, y) -> img (x, -y));;





let antidiagonal = move (vertical_symetry (diagonal green)) (-512, 0);;



let v_lines n =
	(fun (x, y) -> 
		if y mod n = 0 then
			black
		else
			background);;

let v_stripes n =
	(fun (x, y) -> 
		if y mod (2 * n) <= n then
			black
		else
			background);;


let chessboard n =
	(fun (x, y) -> 
		if (y mod (2 * n) <= n && x mod (2 * n) <= n) || (y mod (2 * n) >= n && x mod (2 * n) >= n)then
			black
		else
			background);;




let concentric col rayon =
	(fun (x, y) -> 
		if ((x * x) + (y * y)) mod ( 2* (rayon * rayon)) <= rayon * rayon then
			col
		else
			background);;



let compose_two img1 img2 =
	(fun (x, y) -> 
		let pic1 = img1 (x, y) in
			let pic2 = img2 (x, y) in
				if pic1 = background then
					pic2
				else if pic2 = background then
					pic1
				else 
					pic2);;


let mickey = compose_two 
				(compose_two (move (disk 25 black) (-100, -100)) (move (disk 25 black) (-200, -100))) 
				(move (disk 50 black) (-150, -50));;


let rec compose lst_img = 
	(fun (x, y) -> 
	if lst_img = [] then 
		background
	else
		compose_two (List.hd lst_img) (compose (List.tl lst_img)) (x, y));;



let mickey1 = 
		let ear = move (disk 25 black) (-100, -100)
			and ear2 = move (disk 25 black) (-200, -100)
			and head = (move (disk 50 black) (-150, -50))
			and head2 = (move (disk 45 (Graphics.rgb 191 191 191)) (-150, -50))
			and eye1 = move (disk 5 black) (-135, -60)
			and eye2 = move (disk 5 black) (-165, -60)
			and nose = (move (disk 10 black) (-150, -45))
		in [ear; ear2; head; head2; eye1; eye2; nose];;

(render (compose mickey1));;






