(* LATE: Late Days used: 1, Late Days from previous assignments: 0 *)
(* Exercises A *)
(* A.1 *)
type point = { x : float; y : float }
type segment = { startp : point; endp : point }

let midpoint_segment { startp; endp } =
	let x = (startp.x +. endp.x) /. 2. in
    let y = (startp.y +. endp.y) /. 2. in
        { x; y }
let segment_length { startp; endp } =
    sqrt ((startp.x -. endp.x) ** 2. +. (startp.y -. endp.y) ** 2.)

let print_point { x; y } = Printf.printf "(%g, %g)" x y

let make_point x y = { x; y }
let get_coords { x; y } = (x, y)
let make_segment startp endp = { startp; endp }
let get_points { startp; endp } = (startp, endp)

(* A.2 *)
type rectangle = {ll: point; ur: point}
let rectangle_lower_segment {ll; ur} = 
	let startp = ll in
	let endp = make_point ur.x ll.y in
	make_segment startp endp
let rectangle_upper_segment {ll; ur} = 
	let startp = make_point ll.x ur.y in
	let endp = ur in
	make_segment startp endp
let rectangle_left_segment {ll; ur} = 
	let startp = ll in
	let endp = make_point ll.x ur.y in
	make_segment startp endp
let rectangle_right_segment {ll; ur} = 
	let startp = make_point ur.x ll.y in
	let endp = ur in
	make_segment startp endp

let rectangle_perimeter rect =
	segment_length (rectangle_lower_segment rect) +.
	segment_length (rectangle_upper_segment rect) +.
	segment_length (rectangle_left_segment rect) +.
	segment_length (rectangle_right_segment rect)
	
let rectangle_area rect =
	segment_length (rectangle_lower_segment rect) *.
	segment_length (rectangle_left_segment rect)

type rectangle2 = {lx: float; ly: float; ux: float; uy: float}
let rectangle_lower_segment2 { lx; ly; ux; uy } = 
	let startp = make_point lx ly in
	let endp = make_point ux ly in
	make_segment startp endp
let rectangle_upper_segment2 { lx; ly; ux; uy } = 
	let startp = make_point lx uy in
	let endp = make_point ux uy in
	make_segment startp endp
let rectangle_left_segment2	{ lx; ly; ux; uy } = 
	let startp = make_point lx ly in
	let endp = make_point lx uy in
	make_segment startp endp
let rectangle_right_segment2 { lx; ly; ux; uy } = 
	let startp = make_point ux ly in
	let endp = make_point ux uy in
	make_segment startp endp

let rectangle_perimeter2 rect =
	segment_length (rectangle_lower_segment2 rect) +.
	segment_length (rectangle_upper_segment2 rect) +.
	segment_length (rectangle_left_segment2 rect) +.
	segment_length (rectangle_right_segment2 rect)
	
let rectangle_area2 rect =
	segment_length (rectangle_lower_segment2 rect) *.
	segment_length (rectangle_left_segment2 rect)

let make_rectangle ll ur = { ll; ur }

let make_rectangle2 lx ly ux uy = { lx; ly; ux; uy }

(* A.3 *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x _ -> x)
let second z = z (fun _ y -> y)

(*  
1. verify first (make_pair x y) yields x:

desugar first (make_pair x y) to first (fun m -> m x y)
then first (fun m -> m x y) desugar to (fun x y -> x) (fun m -> m x y) 
this then then becomes (fun m -> m x y) (fun x y -> x) to (fun x y -> x) x y
finally (fun x y -> x) x y evaluates to x


2. Now evaluate second (make_pair 1 2)
Now let us evaluate evaluate second (make_pair 1 2)
	evaluate make_pair 1 2 
		1 -> 1
		2 -> 2
	evaluate make_pair, desugar to fun x y -> fun m -> m x y
    apply 1, 2 to fun x y -> fun m -> m x y
      	substitute 1 for x and then 2 for y in fun m -> m x y
        	fun m -> m 1 2
	evaluate second, desugar to fun z -> z (fun x y -> y)
  	then apply fun m -> m 1 2 to fun z -> z (fun x y -> y)
		substitute (fun m -> m 1 2) for z in z (fun x y -> y)
			-> (fun m -> m 1 2) (fun x y -> y)
		evaluate (fun m -> m 1 2) (fun x y -> y)
			evaluate (fun x y -> y) -> (fun x y -> y)
			evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
			apply (fun m -> m 1 2) to (fun x y -> y)
				substitute (fun x y -> y) for m in (m 1 2)
					-> (fun x y -> y) 1 2
				evaluate (fun x y -> y) 1 2
					evaluate 1 -> 1
					evaluate 2 -> 2
					evaluate (fun x y -> y) -> (fun x y -> y)
					apply (fun x y -> y) to 1, 2
						substitute 1 for x, 2 for y in (y)
							-> 2
						result: 2
					result: 2
				result: 2
			result: 2
		result: 2
	result: 2

*)

(* A.4 *)
let rec pow a b = 
	if b = 0 then 1
	else a * pow a (b - 1)

let rec int_log a b = 
	if b mod a <> 0 then 0
	else 1 + int_log a (b / a)

let make_pairi a b = (pow 2 a) * (pow 3 b)
let firsti z = int_log 2 z
let secondi z = int_log 3 z

(* A.5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev u =
	match u with
	| () :: t -> t
	| _ -> invalid_arg "cannot take previous unary of 0"

let rec integer_to_unary n =
	if n = 0 then zero
	else succ (integer_to_unary (n - 1))

let rec unary_to_integer u =
	if is_zero u then 0
	else 1 + unary_to_integer (prev u)

let rec unary_add u1 u2 =
	if is_zero u2 then u1
	else unary_add (succ u1) (prev u2)


type nat = Zero | Succ of nat
let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' u = 
	match u with
	| Succ u -> u
	| Zero -> invalid_arg "cannot take prev of 0"

(* The other definitions do not have to change other than obvious name changes because the core 
functions they call serve the same purposes.
Here are the obvious name changes *)
let rec integer_to_unary' n =
	if n = 0 then zero'
	else succ' (integer_to_unary' (n - 1))

let rec unary_to_integer' u =
	if is_zero' u then 0
	else 1 + unary_to_integer' (prev' u)

let rec unary_add' u1 u2 =
	if is_zero' u2 then u1
	else unary_add' (succ' u1) (prev' u2)

(* A.6 *)
let zerof = fun _ -> fun z -> z
  (* or equivalently: let zerof = fun s z -> z *)
  (* or equivalently: let zerof s z = z *)

let add1 n = fun s -> fun z -> s (n s z)
  (* or equivalently: let add1 n = fun s z -> s (n s z) *)
  (* or equivalently: let add1 n s z = s (n s z) *)

let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = (m s) ((n s) z)
let church_to_integer n = (n (fun z -> z + 1)) 0

(* A.7 *)
(* 
zerof : 'a -> 'b -> 'b can be desugared to 'a -> ('b -> 'b)
one : ('a -> 'b) -> 'a -> 'b can be desugared to ('a -> 'b) -> ('a -> 'b)
Then let us desugar church_to_integer : ((int -> int) -> int -> 'c) -> 'c to
((int -> int) -> (int -> 'c)) -> 'c

Let us apply church_to_integer to zerof, substitute zerof for n:
church_to_integer zerof = zerof (fun x -> x + 1) 0
Now we know the numeral zerof must match the type ((int -> int) -> (int -> 'c)). 
This forces the first argument of zerof, originally of type 'a, to be (int -> int). So we can set 'a = int -> int
Then the second argument, originally of type 'b, must be int to match the function type signature. So we set 'b = int.
Thus zerof becomes: zerof : (int -> int) -> int -> int and the result of church_to_integer zerof is an int

Let us apply church_to_integer to one, substitute one for n:
church_to_integer one = one (fun x -> x + 1) 0
Now we know the numeral one must match the type ((int -> int) -> (int -> 'c)).
This forces the parameter for one (originall type ('a -> 'b)) to match (int -> int). So we set 'a = int and 'b = int.
Then the second argument of type a' becomes type int aswell
Thus one becomes: one : (int -> int) -> int -> int and the result of church_to_integer one is an int
*)


(* Exercises B *)
(* B.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch = function
	| Mobile (l, _) -> l
let right_branch = function
	| Mobile (_, r) -> r
let branch_length = function
	| Weight (l, _) -> l
	| Structure (l, _) -> l
let branch_structure = function
	| Weight (_, w) -> `Weight w
	| Structure (_, m) -> `Structure m

let rec branch_weight1 = function
	| Weight (_, w) -> w
	| Structure (_, m) -> total_weight1 m
and total_weight1 = function
	| Mobile (l, r) -> (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b = 
	match branch_structure b with
		| `Weight w -> w
		| `Structure m -> total_weight2 m
and total_weight2 m = (branch_weight2 (left_branch m)) + (branch_weight2 (right_branch m))

let rec is_balanced m = 
	let is_balanced_branch b = 
		match branch_structure b with
		| `Weight _ -> true
		| `Structure m -> is_balanced m
	in let l = left_branch m
	and r = right_branch m 
	in ((branch_length l * branch_weight1 l) == (branch_length r * branch_weight1 r))
	&& is_balanced_branch l && is_balanced_branch r

type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = { left; right }
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l m = Branch' (l, Structure' m)
let left_branch' m = m.left
let right_branch' m = m.right
let branch_length' = function
	| Branch' (l, _) -> l
let branch_structure' = function
	| Branch' (_, Weight' (w)) -> `Weight w
	| Branch' (_, Structure' (m)) -> `Structure m

let rec branch_weight' b = 
	match branch_structure' b with
		| `Weight w -> w
		| `Structure m -> total_weight' m
and total_weight' m = (branch_weight' (left_branch' m)) + (branch_weight' (right_branch' m))

let rec is_balanced' m = 
	let is_balanced_branch' b = 
		match branch_structure' b with
		| `Weight _ -> true
		| `Structure m -> is_balanced' m
	in let l = left_branch' m
	and r = right_branch' m 
	in ((branch_length' l * branch_weight' l) == (branch_length' r * branch_weight' r))
	&& is_balanced_branch' l && is_balanced_branch' r

(* B.2 *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree (Tree(d)) = 
	let rec square = function
		| [] -> []
		| Sub h :: t -> Sub (square_tree h) :: (square t)
		| Num h :: t -> Num (h * h) :: (square t)
in Tree (square d)


let rec square_tree' (Tree (d)) = 
	let square = function
	| Sub t -> Sub (square_tree' t)
	| Num n -> Num (n * n)
in Tree (List.map square d)
	
(* B.3 *)
let rec tree_map f (Tree d) = 
	let f = function
	| Sub t -> Sub (tree_map f t)
	| Num n -> Num (f n)
in Tree (List.map f d)

(* Exercises C *)
(* C.1 *)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

let rec simplify expr =
	let e = simplify1 expr in
		if expr = e
		then expr
		else simplify e
	and simplify1 = function
		| Add (Int 0, a) -> simplify a
		| Add (Int a, Int b) -> Int (a + b)
		| Add (a, Int 0) -> simplify a
		| Add (a, b) -> Add (simplify a, simplify b)
		| Mul (_, Int 0) -> Int 0
		| Mul (Int 0, _) -> Int 0
		| Mul (Int 1, a) -> simplify a
		| Mul (a, Int 1) -> simplify a
		| Mul (Int a, Int b) -> Int (a * b)
		| Mul (a, b) -> Mul (simplify a, simplify b)
		| Pow (_, 0) -> Int 1
		| Pow (a, 1) -> simplify a
		| Pow (Int a, b) -> Int (pow a b)
		| Pow (a, b) -> Pow (simplify a, b)
		| Int a -> Int a
		| Var a -> Var a

(* C.2 *)
let rec derivative var expr =
	let e = simplify expr in
	let d = deriv var e in
	  simplify d
	and deriv var = function
		| Int _ -> Int 0
		| Var e when e = var -> Int 1
		| Var _ -> Int 0
		| Add (a, b) -> Add (deriv var a, deriv var b)
		| Mul (a, b) -> Add(Mul(deriv var a, b), Mul(a, deriv var b))
		| Pow (a, b) -> Mul(Mul(Int b, Pow (a, b - 1)), deriv var a)
