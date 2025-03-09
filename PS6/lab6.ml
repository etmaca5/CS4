(* LATE: Late Days used: 2, Late Days from previous assignments: 3 *)
(* Part A exercises *)

(* A.1 *)
(*
    FRAME 0 (initial environment)
        parent: none
        bindings:
          - : [primitive function -]
          * : [primitive function *]
      
    FUNCTION 0 (fun n -> let rec iter m r = ... in iter n 1)
        env: FRAME 0
        params: n
        body: let rec iter m r = if m = 0 then r else iter (m - 1) (r * m) in iter n 1
    
    FRAME 1 (let factorial = FUNCTION 0 in ...)
        parent: FRAME 0
        bindings:
            factorial : FUNCTION 0
      
    FRAME 2 (factorial applied to 3)
        parent: FRAME 0
        bindings:
            n : 3
    
    FUNCTION 1 (rec iter m r = if m = 0 then r else iter (m - 1) (r * m))
        env: FRAME 2
        params: m, r
        body: if m = 0 then r else iter (m - 1) (r * m)
    
    FRAME 3 (let rec iter = FUNCTION 1 in ...)
        parent: FRAME 2
        bindings:
            iter : FUNCTION 1
      
    FRAME 4 (iter applied to n, 1)
        parent: FRAME 3
        bindings:
            m : 3
            r : 1
      
    FRAME 5 (iter applied to (m - 1), (r * m))
        parent: FRAME 3
        bindings:
            m : 2
            r : 3
      
    FRAME 6 (iter applied to (m - 1), (r * m))
        parent: FRAME 3
        bindings:
            m : 1
            r : 6
      
    FRAME 7 (iter applied to (m - 1), (r * m))
        parent: FRAME 3
        bindings:
            m : 0
            r : 6
*)

(* A.2 *)
let factorial =
    let f = ref (fun _ -> 0) in
    begin 
        f := fun n ->
        match n with
            | 0 -> 1
            | _ -> n * (!f (n-1));
    end;
    !f

(* Exercises B *)
(* B.1 *)
(* B1a *)
exception Stat_error of string

let make_stat_1 () =
    let sum = ref 0.0 in
    let sumsq = ref 0.0 in
    let n = ref 0 in
    object
        method append x =
            sum := !sum +. x;
            sumsq := !sumsq +. (x *. x);
            n := !n + 1
        
        method clear =
            sum := 0.0;
            sumsq := 0.0;
            n := 0
            
        method mean =
            if !n = 0 then raise (Stat_error "need at least one value for mean")
            else !sum /. float_of_int !n
        
        method variance =
            if !n = 0 then raise (Stat_error "need at least one value for variance")
            else !sumsq /. float_of_int !n -. (!sum /. float_of_int !n) ** 2.0
        
        method stdev =
            if !n = 0 then raise (Stat_error "need at least one value for stdev")
            else sqrt (!sumsq /. float_of_int !n -. (!sum /. float_of_int !n) ** 2.0)
    end

(* B2b *)
let make_stat_2 () =
    let sum = ref 0.0 in
    let sumsq = ref 0.0 in
    let n = ref 0 in
    object (self)
        method append x =
            sum := !sum +. x;
            sumsq := !sumsq +. (x *. x);
            n := !n + 1
        
        method clear =
            sum := 0.0;
            sumsq := 0.0;
            n := 0
        
        method private _variance =
            !sumsq /. float_of_int !n -. (!sum /. float_of_int !n) ** 2.0
        
        method mean =
            if !n = 0 then raise (Stat_error "need at least one value for mean")
            else !sum /. float_of_int !n
        
        method variance =
            if !n = 0 then raise (Stat_error "need at least one value for variance")
            else self#_variance
        
        method stdev =
            if !n = 0 then raise (Stat_error "need at least one value for stdev")
            else sqrt (self#_variance)
    end

(* B.2 *)
class stat =
    let sum = ref 0.0 in
    let sumsq = ref 0.0 in
    let n = ref 0 in
    object (self)
        method append x =
            sum := !sum +. x;
            sumsq := !sumsq +. (x *. x);
            n := !n + 1
        
        method clear =
            sum := 0.0;
            sumsq := 0.0;
            n := 0
        
        method private _variance =
            !sumsq /. float_of_int !n -. (!sum /. float_of_int !n) ** 2.0
        
        method mean =
            if !n = 0 then raise (Stat_error "need at least one value for mean")
            else !sum /. float_of_int !n
        
        method variance =
            if !n = 0 then raise (Stat_error "need at least one value for variance")
            else self#_variance
        
        method stdev =
            if !n = 0 then raise (Stat_error "need at least one value for stdev")
            else sqrt (self#_variance)
    end

(* Exercises C *)
(* C.1 *)

module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (* Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf

    let is_empty = function
        | Leaf -> true
        | _ -> false

    let make_node x l r =
        let rank_left = match l with
            | Leaf -> 0
            | Node(rank, _, _, _) -> rank
        in
        let rank_right = match r with
            | Leaf -> 0
            | Node(rank, _, _, _) -> rank
        in
        if rank_left >= rank_right then
            Node(rank_right + 1, x, l, r)
        else
            Node(rank_left + 1, x, r, l)

    let rec merge h1 h2 =
        match (h1, h2) with
        | (Leaf, h) -> h
        | (h, Leaf) -> h
        | (Node(_, x1, l1, r1), Node(_, x2, l2, r2)) ->
            if x1 <= x2 then
                make_node x1 l1 (merge r1 h2)
            else
                make_node x2 l2 (merge h1 r2)

    let insert h x =
        merge h (Node(1, x, Leaf, Leaf))

    let find_min = function
        | Leaf -> raise Empty
        | Node(_, x, _, _) -> x

    let delete_min = function
        | Leaf -> raise Empty
        | Node(_, _, l, r) -> merge l r

    let rec from_list = function
        | [] -> Leaf
        | h :: t -> insert (from_list t) h
  end

let heap_sort lst =
    let q = PriorityQueue.from_list lst in
    let rec iter q out =
        if PriorityQueue.is_empty q then List.rev out
        else iter (PriorityQueue.delete_min q) (PriorityQueue.find_min q :: out)
    in
    iter q []

(* C.2 *)

(* Signature for ordered objects *)
module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

(* Functor to create priority queues for any ordered type *)
module MakePriorityQueue (Elt : ORDERED_TYPE)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty

    type elem = Elt.t

    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf

    let is_empty = function
        | Leaf -> true
        | _ -> false

    let make_node x l r =
        let rank_left = match l with
            | Leaf -> 0
            | Node(rank, _, _, _) -> rank
        in
        let rank_right = match r with
            | Leaf -> 0
            | Node(rank, _, _, _) -> rank
        in
        if rank_left >= rank_right then
            Node(rank_right + 1, x, l, r)
        else
            Node(rank_left + 1, x, r, l)

    let rec merge h1 h2 =
        match (h1, h2) with
        | (Leaf, h) -> h
        | (h, Leaf) -> h
        | (Node(_, x1, l1, r1), Node(_, x2, l2, r2)) ->
            if Elt.compare x1 x2 <= 0 then
                make_node x1 l1 (merge r1 h2)
            else
                make_node x2 l2 (merge h1 r2)

    let insert h x =
        merge h (Node(1, x, Leaf, Leaf))

    let find_min = function
        | Leaf -> raise Empty
        | Node(_, x, _, _) -> x

    let delete_min = function
        | Leaf -> raise Empty
        | Node(_, _, l, r) -> merge l r

    let rec from_list = function
        | [] -> Leaf
        | h :: t -> insert (from_list t) h

  end

module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
    let q = StringPQ.from_list lst in
    let rec iter q out =
        if StringPQ.is_empty q then List.rev out
        else iter (StringPQ.delete_min q) (StringPQ.find_min q :: out)
    in
    iter q []

(* Exercises D *)
(* D.1a *)
let y =
    fun f ->
      (fun z -> z (`Roll z))
      (fun (`Roll w) -> f (fun x -> w (`Roll w) x))


(* let rec sum lst =
    match lst with
    | [] -> 0
    | h::t -> h + sum t *)

let almost_sum self lst = 
	match lst with
	| [] -> 0
	| h :: t -> h + self t

let sum = y almost_sum

(* D.1b *)
(* let factorial n =
    let rec iter n r =
      if n = 0
        then r
        else iter (n - 1) (n * r)
    in
      iter n 1 *)

let factorial2 n =
    let almost_iter self (n, r) =
        if n = 0
        then r
        else self (n - 1, n * r)
    in
    let iter = y almost_iter in
    iter (n, 1)


(* D.2 *)
type 'a contents = Unevaluated of (unit -> 'a) | Evaluated of 'a
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Unevaluated e)

let force lzy =
    match !lzy with
    | Evaluated v -> v
    | Unevaluated f -> 
        let result = f () in
        lzy := Evaluated result;
        result