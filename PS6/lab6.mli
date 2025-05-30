(* lab6.mli: Interface file for lab 6. *)

(* B.1 *)

exception Stat_error of string

val make_stat_1 :
  unit ->
  < append : float -> unit; clear : unit; mean : float; stdev : float;
    variance : float >

val make_stat_2 :
  unit ->
  < append : float -> unit; clear : unit; mean : float; stdev : float;
    variance : float >

(* B.2 *)

class stat :
  object
    method append : float -> unit
    method clear : unit
    method mean : float
    method stdev : float
    method variance : float
  end

(* C.1 *)

module type PRIORITY_QUEUE =
  sig
    exception Empty
    type elem
    type t
    val empty : t
    val is_empty : t -> bool
    val insert : t -> elem -> t
    val find_min : t -> elem
    val delete_min : t -> t
    val from_list : elem list -> t
  end

module PriorityQueue :
  sig
    exception Empty
    type elem = int
    type t
    val empty : t
    val is_empty : t -> bool
    val insert : t -> elem -> t
    val find_min : t -> elem
    val delete_min : t -> t
    val from_list : elem list -> t
  end

val heap_sort : PriorityQueue.elem list -> PriorityQueue.elem list

(* C.2 *)

module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module MakePriorityQueue :
  functor (Elt : ORDERED_TYPE) ->
    sig
      exception Empty
      type elem = Elt.t
      type t
      val empty : t
      val is_empty : t -> bool
      val insert : t -> elem -> t
      val find_min : t -> elem
      val delete_min : t -> t
      val from_list : elem list -> t
    end

module OrderedString :
  sig
    type t = string
    val compare : t -> t -> int
  end

module StringPQ :
  sig
    exception Empty
    type elem = OrderedString.t
    type t = MakePriorityQueue(OrderedString).t
    val empty : t
    val is_empty : t -> bool
    val insert : t -> elem -> t
    val find_min : t -> elem
    val delete_min : t -> t
    val from_list : elem list -> t
  end

val heap_sort_2 : StringPQ.elem list -> StringPQ.elem list

val sum : int list -> int
val factorial2 : int -> int

type 'a lazy_t  (* abstract! *)
val make_lazy : (unit -> 'a) -> 'a lazy_t
val force : 'a lazy_t -> 'a

