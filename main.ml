open Raylib

let n = 10
let squaresize = 1
(*pixel per case*)
let ppc = 50
type vector = float * float

let grads = Array.init (n+1) 
	(fun _ -> Array.init (n+1) 
		(fun _ -> (Random.float 1.0, Random.float  1.0))
	)

let print_vec (a,b) = Printf.printf "(%0.2f, %0.2f)\n" a b

let print_array =
	Array.iter (Array.iter print_vec)

let (+$) (a,b) (c,d) = (a +. c, b +. d)
let (-$) (a,b) (c,d) = (a -. c, b -. d)
let ( *$) t (a,b) = (a *. t, b *. t)

let ps (a,b) (x,y) =
		a *. x +. b *. y


(*6t5-15t4+10t3*)
let f t =
	6.0 *. t**5.0 -. 15.0*.t**4.0 +. 10.0 *. t **3.0

(* polynôme obtenu manuellement en voulant des dérivées nulles au bord 
et atteindre les valeurs aussi au bord *)
let f t = 
	3.0 *. t**2.0  -. 2.0 *. t**3.0
let inter u v t =
	u +. (v-.u) *. f t

let perlin (x,y) =
	let i,j = x |> floor |> int_of_float, y |> floor |> int_of_float in
	(*Printf.printf "%d %d\n" i j ;*)
	let pos = x -. floor x, y -. floor y in
	let infl = 
	[| ps grads.(i).(j) pos; ps grads.(i+1).(j) (pos -$ (1.0,0.0));
		ps grads.(i).(j+1) (pos -$ (0.0,1.0)); ps grads.(i+1).(j+1) (pos -$ (1.0,1.0))
	|]
	in
	let u,v = pos in
	let xtop = inter infl.(0) infl.(1) u in
	let xbot = inter infl.(2) infl.(3) u in
	inter xtop xbot v


let iof = int_of_float 
let foi = float_of_int
let  rec loop () = 
	if window_should_close () then close_window ()
	else begin
		begin_drawing ();
		(*clear_background Color.black;*)
		for k = 0 to 1 do
		for i = 0 to ppc*(n-1) do
			for j = 0 to ppc*(n-1) do
				let v = perlin (foi i /. foi ppc, foi j /. foi ppc) in 
				draw_rectangle (i*squaresize) (j*squaresize) squaresize squaresize 
					(color_from_hsv (v*.360.0) 0.7 1.0)
			done
		done
		done;
		take_screenshot "result.png";
		end_drawing ();
		close_window ();
	end

let () = 
	Random.self_init ();
	init_window (ppc * (n-1) * squaresize) (ppc * (n-1) * squaresize) "grré";
	loop ();
