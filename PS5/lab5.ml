(* LATE: Late Days used: 1, Late Days from previous assignments: 1 *)
(* A exercises *)
(* A.1 *)
let fibonacci n =
    let curr = ref 0
    and next = ref 1
    and fib = ref n in
    begin
        while !fib > 0 do
            let temp = ref !next in
            next := !temp + !curr;
            curr := !temp;
            fib := !fib - 1;
        done;
        !curr
    end

let fibonacci2 n =
    let curr = ref 0
    and next = ref 1 in
    begin
        for _ = n downto 1 do
            let temp = ref !next in
            next := !temp + !curr;
            curr := !temp;
        done;
        !curr
    end

(* A.2 *)
let selection_sort arr =
    let n = Array.length arr in
    if n < 2 then () else
        for i = 0 to n - 2 do
            let min_index = ref i in
            for j = i + 1 to n - 1 do
                if arr.(j) < arr.(!min_index) then min_index := j;
            done;
            if !min_index <> i then begin
                let temp = arr.(i) in
                arr.(i) <- arr.(!min_index);
                arr.(!min_index) <- temp;
            end
        done

(* Exercises B *)
(* B.1 *)
let meters_per_foot = 0.3048

let get_meters len =
    match len with
        | `Meter m -> m
        | `Foot f -> f *. meters_per_foot
        | `Inch i -> i *. meters_per_foot /. 12.


let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.2 *)
let grams_per_slug = 14593.903203 

let get_grams mass =
    match mass with
    | `Gram g -> g
    | `Kilo k -> k *. 1000.0
    | `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds time =
  match time with
  | `Second s -> s
  | `Minute m -> m *. 60.0
  | `Hour h -> h *. 3600.0
  | `Day d -> d *. 86400.0

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* B.3 *)
let unit_add a b =
    match (a, b) with
    | (`Length l1, `Length l2) -> `Length (length_add l1 l2)
    | (`Mass m1, `Mass m2) -> `Mass (mass_add m1 m2)
    | (`Time t1, `Time t2) -> `Time (time_add t1 t2)
    | _ -> failwith "units are incompatible"
  
(* we do not get into a combinatorial explosion when adding more unit classes because
the add operation is defined per unit class, so when a new unit class is introduced, 
we can just add a new clause to handle that class. Since we don't match across classes so the growth of
combinations is linear with the number of unit classes. *)

(* Exercises C *)
(* C.1 *)
let rec make_gram g =
    let is_compatible other =
        match other#unit_type with
            | `Gram -> true
            | `Slug -> true
            | _ -> false
    in
        object
        method get_grams = g
        method get_slugs = g /. grams_per_slug
        method unit_type = `Gram
        method compatible other = is_compatible other
        method add other = 
            if is_compatible other then
            make_gram (g +. other#get_grams)
            else
            failwith "units are incompatible"
        end


(* C.2 *)
(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
    object
      method value = i
      method show = string_of_int i
      method is_zero = i = 0
      method is_number = true
      method evaluate _ _ = make_number i  (* must evaluate to an object *)
      method derive _ = make_number 0  (* derivative of a number is 0 *)
    end
  
(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
    object
        method value = failwith "variable has no numerical value"
        method show  = v
        method is_zero = false
        method is_number = false
        method evaluate v' n =
        if v = v'
            then make_number n
            else make_variable v
        method derive v' =
        if v = v'
            then make_number 1  (* d/dx(x) = 1 *)
            else make_number 0  (* d/dx(y) = 0 *)
    end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
    match () with
        | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
        | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
        | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
            make_number (expr1#value + expr2#value)
        | _ ->  (* create a new object representing the sum *)
            object
                method value = failwith "sum expression has no numerical value"
                method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
                method is_zero = false
                method is_number = false
                method evaluate v n =
                    make_sum (expr1#evaluate v n) (expr2#evaluate v n)
                method derive v =
                    make_sum (expr1#derive v) (expr2#derive v)
            end

(* Evaluate a message-passing expression with a number
    substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* C.2a *)
let rec make_product expr1 expr2 =
    match () with
        | _ when expr1#is_zero || expr2#is_zero -> make_number 0
        | _ when expr1#is_number && expr1#value = 1 -> expr2
        | _ when expr2#is_number && expr2#value = 1 -> expr1
        | _ when expr1#is_number && expr2#is_number ->
            make_number (expr1#value * expr2#value)
        | _ ->
            object
                method value = failwith "product expression has no numerical value"
                method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
                method is_zero = false
                method is_number = false
                method evaluate v n =
                    make_product (expr1#evaluate v n) (expr2#evaluate v n)
                method derive v =
                    make_sum (make_product (differentiate expr1 v) expr2) (make_product expr1 (differentiate expr2 v))
            end

(* C.2b *)
(* i *)
(* 
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* ii *)
(* 
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* iii *)
(* 
- : string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + (x * (y * y)))))"
*)

(* iv *)
(*
- : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
*)

(* v *)
(* 
- : string = "558"
*)

(* vi *)
(* 
- : string = "396"
*)
