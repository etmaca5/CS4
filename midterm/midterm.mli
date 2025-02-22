(* CS 4, Winter 2025. Interface file for the midterm exam. *)

(* Part B. *)

val split : int -> 'a list -> 'a list * 'a list

val group : int -> 'a list -> 'a list list

val bubble : 'a list -> ('a -> 'a -> bool) -> 'a list
val bubble2 : 'a list -> ('a -> 'a -> bool) -> 'a list * bool
val bubble_sort : 'a list -> ('a -> 'a -> bool) -> 'a list

val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list
val scan_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(* Part C. *)

type tree =
    Leaf
  | Node2 of tree * int * tree
  | Node3 of tree * int * tree * int * tree

val tree_search : int -> tree -> bool

type insertion =
  | NoSplit of tree
  | Split of tree * int * tree

val insert_helper : int -> tree -> insertion

val tree_insert : int -> tree -> tree

(* Utility functions (not graded). *)

val tree_of_list : int list -> tree
val tree_to_string : tree -> string
val print_tree_to_file : string -> tree -> unit
