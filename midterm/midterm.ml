(* Etienne Casanova *)
(* ecasanov@caltech.edu *)
(* A exercises *)
(* A.1 *)
(* The worst case asymptotic time complexity is O(log(n)) for this function
as the argument n is halved at each recursive call
(either 1 + (n - 1)/ 2 or n / 2) until the input reaches 0 (note: 1 / 2 = 0).
This occurs for both the even and the odd cases, and is the behavior
of of O(log(n)) time complexity.
*)

(* A.2 *)
(* The worst case asymptotic time complexity is O(n) for this function as if
the input n is odd we will recursively call the function with n-2 which is 
also odd, so we are decreasing the input by 2 each time fod odd inputs,
which has a time complexity of O(n) since n / 2 recursive calls. 
For the even case, we are dividing the input by 2 each time, which has a time
complexity of O(log(n)) so we take the worst case of O(n).
*)

(* A.3 *)
(* The worst case asymptotic time complexity is O(log(n)^2) for this function
as the tail recursion function iter doubles b until it is past n, then it
doubles a and b is reset to 1. It continues to do this doubling of b 
in the inner loop until a reaches past n. Thus there are log(n) iterations
of b reaching n, then those are repeated log(n) times for the outer loop 
(getting a to reach n). Since they both are initialized to 1, the time 
complexity is O(log(n)^2).
*)

(* A.4 *)
(* First we know that append takes O(n) where n is the length of the first
list l1. This is because it appends each element one by one to the second
list l2 (doing constant work for each element)

Now let us evaluate concat_r.
We take the first element of the list (h) and append it to the 
rest of the recursive calls:
append [1;2;3] (concat_r [[4;5;6]; [7;8;9]])
then we evaluate concat_r [[4;5;6]; [7;8;9]], which becomes
append [4;5;6] (concat_r [[7;8;9]]) ->
concat_r [[7;8;9]] -> append [7;8;9] [] -> [7;8;9]
then we evaluate append [4;5;6] [7;8;9] -> [4;5;6;7;8;9]
then we evaluate append [1;2;3] [4;5;6;7;8;9] -> [1;2;3;4;5;6;7;8;9]

Let m be the length of the inner lists and n be the number of inner lists.
We take n calls to the append function and each call takes O(m) time, thus
the overall time complexity is O(m * n) for concat_r. For the example above
this is O(3 * 3) as shown in the number of calls to append and concat_r.
The question assumes m = n so the time complexity is O(n^2).


Now let us evaluate concat_l
We start with acc = []
then for the first iteration, we get: 
iter (append [] [1;2;3]) [[4;5;6]; [7;8;9]]
Then we evaluate the second iteration:
iter (append [1;2;3] [4;5;6]) [[7;8;9]]
Then we evaluate the third iteration:
Third iteration: acc = [1;2;3;4;5;6].
iter (append [1;2;3;4;5;6] [7;8;9]) []
Then we evaluate the fourth iteration, which evalutes to:
[1;2;3;4;5;6;7;8;9]

Notice that for the third iteration we append 6 elements to the list,
from the left side, so the time complexity is more than concat_r.
Specifically, on the ith iteration the acc list has length (i - 1) * n
and this occurs for the n - 1 iterations. The size of this list determines
the time complexity of the append function, which is O(n). So the total time
complexity the number of inner lists m (which is the number of recursive 
calls) times the size of acc because each element in acc is appended, thus:
sum (i - 1) * n from i=1 to i=n-1 = O(n^2).
Thus the time complexity of concat_l is O(m * n^2).
The question assumes m = n so the time complexity is O(n^3).
*)


(* Exercises B *)
(* B.1 *)
let split n lst =
    let rec iter i acc l =
      if i = 0 then (List.rev acc, l)
      else
        match l with
        | [] -> failwith "split: not enough elements in list"
        | h :: t -> iter (i - 1) (h :: acc) t
    in
    iter n [] lst

(* B.2 *)
let rec group n lst =
    if lst = [] then []
    else
        let (first, rest) = split n lst in
        first :: group n rest
  
(*
This is an example of structural recursion because each recursive
call is made on a sublist of the original list and no other list
apart from the original input are created. The recursion is thus based
only on the structure of the input list.
*)

(* B.3 *)
(* B.3a *)
let rec bubble lst cmp =
    match lst with
    | [] -> []
    | [x] -> [x]
    | x :: y :: rest ->
        if cmp x y then
            x :: bubble (y :: rest) cmp
        else
            y :: bubble (x :: rest) cmp

(* B.3b *)
let rec bubble2 lst cmp =
    match lst with
    | [] -> ([], false)
    | [x] -> ([x], false)
    | x :: y :: rest ->
        if cmp x y then
          let (bubbl_lst, swapped) = bubble2 (y :: rest) cmp in
          (x :: bubbl_lst, swapped)
        else
          let (bubbl_lst, _) = bubble2 (x :: rest) cmp in
          (y :: bubbl_lst, true)

(* B.3c *)
let rec bubble_sort lst cmp =
    let (lst', swapped) = bubble2 lst cmp in
    if swapped then bubble_sort lst' cmp else lst'

(* B.4 *)
(* B.4a *)
let span f lst =
    let rec iter acc l =
        match l with
        | [] -> (List.rev acc, [])
        | h :: t ->
            if f h then iter (h :: acc) t
            else (List.rev acc, l)
    in
    iter [] lst

(* B.4b *)
let rec group_by f lst =
    match lst with
    | [] -> []
    | h :: t ->
        let (group, rest) = span (fun x -> f h x) t in
        (h :: group) :: group_by f rest

(* B.4c *)
let rec scan_left f init lst =
    match lst with
    | [] -> [init]
    | h :: t ->
        init :: scan_left f (f init h) t

(* B.4d *)
let rec unfold f s =
    match f s with
    | None -> []
    | Some (first, rest) -> first :: unfold f rest

(* Exercises C *)
(* C.1 *)
type tree =
  | Leaf
  | Node2 of tree * int * tree   (* left tree, value, right tree *)
  | Node3 of tree * int * tree * int * tree
    (* left tree, left value, middle tree, right value, right tree *)

let tree_search x tr =
    let rec iter t = 
        match t with
        | Leaf -> false
        | Node2 (l, data, r) ->
            if data = x then true
            else if data > x then iter l
            else iter r
        | Node3 (l, data1, m, data2, r) ->
            if data1 = x || data2 = x then true
            else if data1 > x then iter l
            else if data2 < x then iter r
            else iter m
    in iter tr

(* C.2 *)
type insertion =
  | NoSplit of tree              (* we didn't have to split a node *)
  | Split of tree * int * tree   (* we had to split a node *)

let rec insert_helper i t =
  match t with

    (* Base cases. *)

    | Leaf -> NoSplit (Node2 (Leaf, i, Leaf))

    | Node2 (_, j, _) when i = j -> NoSplit t  (* i is already in tree *)
    | Node3 (_, j, _, k, _) when i = j || i = k -> NoSplit t  (* ditto *)

    | Node2 (Leaf, j, Leaf) when i < j ->
        (* add i to tree; change 2-node to 3-node *)
        NoSplit (Node3 (Leaf, i, Leaf, j, Leaf))
    | Node2 (Leaf, j, Leaf) ->   (* i > j *)
        NoSplit (Node3 (Leaf, j, Leaf, i, Leaf))

    | Node3 (Leaf, j, Leaf, k, Leaf) when i < j ->
        (* split; watch the order! *)
        Split (Node2 (Leaf, i, Leaf), j, Node2 (Leaf, k, Leaf))
    | Node3 (Leaf, j, Leaf, k, Leaf) when i > j && i < k ->
        Split (Node2 (Leaf, j, Leaf), i, Node2 (Leaf, k, Leaf))
    | Node3 (Leaf, j, Leaf, k, Leaf) ->   (* i > k *)
        Split (Node2 (Leaf, j, Leaf), k, Node2 (Leaf, i, Leaf))

    (* Recursive cases. *)

    | Node2 (t1, j, t2) when i < j ->  (* insert into t1 *)
        begin
          match insert_helper i t1 with
            | NoSplit t1' -> NoSplit (Node2 (t1', j, t2))
            | Split (t1a, i', t1b) -> NoSplit (Node3 (t1a, i', t1b, j, t2))
        end

    | Node2 (t1, j, t2) ->  (* i > j; insert into t2 *)
        begin
            match insert_helper i t2 with
            | NoSplit t2' -> NoSplit (Node2 (t1, j, t2'))
            | Split (t2a, i', t2b) -> NoSplit (Node3 (t1, j, t2a, i', t2b))
        end

    | Node3 (t1, j, t2, k, t3) when i < j ->  (* insert into t1 *)
        begin
          match insert_helper i t1 with
            | NoSplit t1' -> NoSplit (Node3 (t1', j, t2, k, t3))
            | Split (t1a, i', t1b) ->  (* split nodes *)
                Split (Node2 (t1a, i', t1b), j, Node2 (t2, k, t3))
        end

    | Node3 (t1, j, t2, k, t3) when i > j && i < k ->  (* insert into t2 *)
        begin
            match insert_helper i t2 with
            | NoSplit t2' -> NoSplit (Node3 (t1, j, t2', k, t3))
            | Split (t2a, i', t2b) ->  (* split nodes *)
                Split (Node2 (t1, j, t2a), i', Node2 (t2b, k, t3))
        end

    | Node3 (t1, j, t2, k, t3) ->  (* i > k; insert into t3 *)
        begin
            match insert_helper i t3 with
            | NoSplit t3' -> NoSplit (Node3 (t1, j, t2, k, t3'))
            | Split (t3a, i', t3b) ->  (* split nodes *)
                Split (Node2 (t1, j, t2), k, Node2 (t3a, i', t3b))
        end

let tree_insert i t =
  match insert_helper i t with
    | NoSplit t' -> t'
    | Split (t1, j, t2) -> Node2 (t1, j, t2)

exception Unbalanced

(* Return the depth of a tree if balanced.
    * If not, raise an Unbalanced exception. *)
let rec tree_depth tree =
    match tree with
    | Leaf -> 0
    | Node2 (t1, _, t2) ->
        let d1 = tree_depth t1 in
        let d2 = tree_depth t2 in
            if d1 <> d2
            then raise Unbalanced
            else d1 + 1
    | Node3 (t1, _, t2, _, t3) ->
        let d1 = tree_depth t1 in
        let d2 = tree_depth t2 in
            if d1 <> d2
            then raise Unbalanced
            else
                let d3 = tree_depth t3 in
                if d1 <> d3
                then raise Unbalanced
                else d1 + 1
    
(* Return true if a tree is balanced.  2-3 trees should always be balanced. *)
let tree_balanced t =
    try let _ = tree_depth t in true
    with Unbalanced -> false

(* Convert a list to a tree. *)
let tree_of_list lst =
    let rec iter tree rest =
    match rest with
        | [] -> tree
        | h :: t -> iter (tree_insert h tree) t
    in
    iter Leaf lst

(* Convert a tree to a string in "dot" format. *)
let tree_to_string t =
    let sp = Printf.sprintf in
    let rec iter t i =
      match t with
        | Leaf -> (sp "  %d[shape=\"point\",width=0.2]\n" i, i + 1)
        | Node2 (t1, c, t2) ->
            let (t1s, j) = iter t1 (i + 1) in
            let (t2s, k) = iter t2 j in
            let curr =
              sp "  %d[label=\"%d\"]\n" i c ^
              sp "  %d -> %d\n" i (i + 1) ^
              sp "  %d -> %d\n" i j in
            let body = curr ^ t1s ^ t2s in
              (body, k)
        | Node3 (t1, c1, t2, c2, t3) ->
            let (t1s, j) = iter t1 (i + 1) in
            let (t2s, k) = iter t2 j in
            let (t3s, l) = iter t3 k in
            let curr =
              sp "  %d[label=\"%d %d\"]\n" i c1 c2 ^
              sp "  %d -> %d\n" i (i + 1) ^
              sp "  %d -> %d\n" i j ^
              sp "  %d -> %d\n" i k in
            let body = curr ^ t1s ^ t2s ^ t3s in
              (body, l)
    in
    let header = "digraph tree {\n" in
    let (body, _) = iter t 0 in
      header ^ body ^ "}\n"
  
(* Print a tree to a file in "dot" format. *)
let print_tree_to_file filename t =
    let outfile = open_out (filename ^ ".dot") in
      begin
        Printf.fprintf outfile "%s" (tree_to_string t);
        close_out outfile
      end
