open Raylib

(* taille d'un pixel *)
let squaresize = 2
let w = 1000
let h = 1000

type vec = float * float
type perlin = {
	grads : vec array array;
	n : int
}

let foi = float_of_int
let (-$) (a,b) (c,d) = (a -. c, b -. d)
let ( *$) q (a,b) = (q*.a, q*.b)
let ps (a,b) (x,y) = a *. x +. b *. y

let createPerlin (freq:int) : perlin =
	(*Random.self_init ();*)
	let n = freq in
	{
		grads = Array.init (n+1) 
		(fun _ -> Array.init (n+1) 
			(fun _ -> Random.float 2.0 -. 1.0, Random.float  2.0 -. 1.0 )
		);
		n = n
	}



(*polynôme recommandé*)
(*6t5-15t4+10t3*)
let f t = 
	(6.0 *. t**2.0 -. 15.0*.t +. 10.0) *. t**3.0
	(* polynôme obtenu manuellement en voulant des dérivées nulles au bord  et atteindre les valeurs aussi au bord *)
	(*3.0 *. t**2.0  -. 2.0 *. t**3.0*)

let inter u v t = u +. (v-.u) *. f t 

let perlin (p:perlin) pos =
	(* lieu sur la grille*)
	let x,y = foi p.n *$ pos in
	let i,j = x |> floor |> int_of_float, y  |> floor |> int_of_float in
	let pos = x  -. floor x, y -. floor y in
	let infl = 
	[| ps p.grads.(i).(j) pos; ps p.grads.(i+1).(j) (pos -$ (1.0,0.0));
		ps p.grads.(i).(j+1) (pos -$ (0.0,1.0)); ps p.grads.(i+1).(j+1) (pos -$ (1.0,1.0))
	|]
	in
	let u,v = pos in
	let xtop = inter infl.(0) infl.(1) u in
	let xbot = inter infl.(2) infl.(3) u in
	let ret = inter xtop xbot v in
	ret 

let rec loop perlgros perlpetit = 
	if window_should_close () then (
		take_screenshot "result.png";
		close_window ()
	)
	else begin
		begin_drawing ();
		clear_background Color.black;
		let maxi = ref 0.0 in 
		let mini = ref 1.0 in
		for _ = 0 to squaresize+1 do
		for i = 0 to w/squaresize - 1 do
			for j = 0 to h/squaresize - 1 do
				let pos = (foi (i*squaresize) /. foi w, foi (j*squaresize) /. foi h) in 
				let v1 = perlin perlgros pos in 
				let v2 = perlin perlpetit pos in
				let v = 0.6 *. v1 +. 0.4*.v2 in
				let v = (v +. 1.0) /. 2.0 in
				draw_rectangle (i*squaresize) (j*squaresize) squaresize squaresize 
					(if v < 0.5 then Color.black
					else if v < 0.6 then color_from_hsv 0.0 1.0 0.5
					else Color.red)
			done
		done
		done;
		end_drawing ();
		loop perlgros perlpetit
	end

let () = 
	Random.self_init ();
	init_window w h  "grré";
	set_target_fps 60;
	if is_window_ready () then
		let perlgros = createPerlin 10 in
		let perlpetit = createPerlin 20 in
		loop perlgros perlpetit
	else 
		failwith "rr"
