open OUnit2
open Midterm

(* 
 * Utilities. 
 *)

let assert_false msg x = assert_bool msg (not x)
let assert_true msg x = assert_bool msg x
let assert_not_equal msg x y = assert_bool msg (not (x = y))

let assert_raises_failure msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Failure _ -> true
       | _ -> false)

let assert_raises_invalid_arg msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Invalid_argument _ -> true
       | _ -> false)

(* Print a list of integers. *)
let print_int_list lst =
  let rec aux lst =
    match lst with
      | [] -> ""
      | h :: t -> ";" ^ string_of_int h ^ aux t
    in
  let s = 
    match lst with
      | [] -> "[]"
      | h :: t -> "[" ^ string_of_int h ^ aux t ^ "]"
  in
    Printf.printf "%s\n" s

(*
 * Sample trees for part C.
 *)

let tree0 = Leaf

let tree1 = Node2 (Leaf, 4, Leaf)

let tree2 = Node3 (Leaf, 4, Leaf, 6, Leaf)

let tree3 = 
  Node2 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 6, Leaf))

let tree4 = 
  Node2 (Node2 (Leaf, 3, Leaf), 4, Node3 (Leaf, 5, Leaf, 6, Leaf))

let tree5 = 
  Node3 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6, Node2 (Leaf, 7, Leaf))

let tree6 =
  Node3 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6,
    Node3 (Leaf, 7, Leaf, 9, Leaf))

let tree7 =
  Node3 (Node3 (Leaf, 1, Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6,
    Node3 (Leaf, 7, Leaf, 9, Leaf))

let tree7a =
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf), 60,
    Node3 (Leaf, 70, Leaf, 90, Leaf))

let tree7b = 
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40,
    Node3 (Leaf, 50, Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf))

let tree7c = 
  Node2
    (Node2 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf)), 51,
       Node2 (Node2 (Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf)))

let tree8 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node2 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 9, Leaf)))

let tree9 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node2 (Leaf, 7, Leaf), 9,
      Node2 (Leaf, 10, Leaf)))

let tree10 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf), 9,
      Node2 (Leaf, 10, Leaf)))

let tree11 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf), 9,
      Node3 (Leaf, 10, Leaf, 11, Leaf)))

let tree12 =
  Node3 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node2 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf)), 9,
      Node2 (Node2 (Leaf, 10, Leaf), 11, Node2 (Leaf, 12, Leaf)))

let test_tree_insert i t =
  match insert_helper i t with
    | NoSplit t' -> t'
    | Split (t1, j, t2) -> Node2 (t1, j, t2)

(*
 * The tests.
 *)

let all_tests = "all" >:::
[ 

  (* No tests for part A. *)

  (* ---------------------------------------------------------------------- 
   * Part B.
   * ---------------------------------------------------------------------- *)

  "Problem B.1: split" >:: (fun _ ->
     assert_equal ~msg:"split 1"
       (split 4 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])
       ([1; 2; 3; 4], [5; 6; 7; 8; 9; 10]);

     assert_equal ~msg:"split 2"
       (split 0 [1; 2; 3])
       ([], [1; 2; 3]);

     assert_equal ~msg:"split 3"
       (split 1 [1; 2; 3])
       ([1], [2; 3]);

     assert_equal ~msg:"split 4"
       (split 2 [1; 2; 3])
       ([1; 2], [3]);

     assert_equal ~msg:"split 5"
       (split 3 [1; 2; 3])
       ([1; 2; 3], []);

     assert_raises_failure "split 6"
       (fun () -> split 4 [1; 2; 3]);
  );

  "Problem B.2: group" >:: (fun _ ->
     assert_equal ~msg:"group 1"
       (group 3 [])
       [];

     assert_equal ~msg:"group 2"
       (group 3 [1;2;3])
       [[1; 2; 3]];

     assert_equal ~msg:"group 3"
       (group 3 [1;2;3;4;5;6])
       [[1; 2; 3]; [4; 5; 6]];

     assert_equal ~msg:"group 4"
       (group 3 [1;2;3;4;5;6;7;8;9])
       [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]];

     assert_raises_failure "group 5"
       (fun () -> group 3 [1;2;3;4;5;6;7;8]);
  );

  "Problem B.3.a: bubble" >:: (fun _ ->
     assert_equal ~msg:"bubble 1"
       (bubble [] (<=))
       [];

     assert_equal ~msg:"bubble 2"
       (bubble [1] (<=))
       [1];

     assert_equal ~msg:"bubble 3"
       (bubble [1; 2] (<=))
       [1; 2];

     assert_equal ~msg:"bubble 4"
       (bubble [1; 1] (<=))
       [1; 1];

     assert_equal ~msg:"bubble 5"
       (bubble [1; 2; 3] (<=))
       [1; 2; 3];

     assert_equal ~msg:"bubble 6"
       (bubble [2; 1] (<=))
       [1; 2];

     assert_equal ~msg:"bubble 7"
       (bubble [3; 1; 2] (<=) )
       [1; 2; 3];

     assert_equal ~msg:"bubble 8"
       (bubble [3; 2; 1] (<=))
       [2; 1; 3];

     assert_equal ~msg:"bubble 9"
       (bubble [5; 1; 3; 2; 4] (<=))
       [1; 3; 2; 4; 5];

     assert_equal ~msg:"bubble 10"
       (bubble [1; 3; 2; 4; 5] (<=))
       [1; 2; 3; 4; 5];

     assert_equal ~msg:"bubble 11"
       (bubble [5; 4; 3; 2; 1] (<=))
       [4; 3; 2; 1; 5];

     assert_equal ~msg:"bubble 12"
       (bubble [4; 3; 2; 1; 5] (<=))
       [3; 2; 1; 4; 5];

     assert_equal ~msg:"bubble 13"
       (bubble [3; 2; 1; 4; 5] (<=))
       [2; 1; 3; 4; 5];

     assert_equal ~msg:"bubble 14"
       (bubble [2; 1; 3; 4; 5] (<=))
       [1; 2; 3; 4; 5];

     assert_equal ~msg:"bubble 15"
       (bubble [1; 2; 3; 4; 5] (<=))
       [1; 2; 3; 4; 5];
  );

  "Problem B.3.b: bubble2" >:: (fun _ ->

     assert_equal ~msg:"bubble2 1"
       (bubble2 [] (<=))
       ([], false);

     assert_equal ~msg:"bubble2 2"
       (bubble2 [1] (<=))
       ([1], false);

     assert_equal ~msg:"bubble2 3"
       (bubble2 [1; 2] (<=))
       ([1; 2], false);

     assert_equal ~msg:"bubble2 4"
       (bubble2 [1; 2; 3] (<=))
       ([1; 2; 3], false);

     assert_equal ~msg:"bubble2 5"
       (bubble2 [2; 1] (<=))
       ([1; 2], true);

     assert_equal ~msg:"bubble2 6"
       (bubble2 [3; 1; 2] (<=))
       ([1; 2; 3], true);

     assert_equal ~msg:"bubble2 7"
       (bubble2 [3; 2; 1] (<=))
       ([2; 1; 3], true);

     assert_equal ~msg:"bubble2 8"
       (bubble2 [1; 3; 2; 4; 5] (<=))
       ([1; 2; 3; 4; 5], true);

     assert_equal ~msg:"bubble2 9"
       (bubble2 [5; 1; 3; 2; 4] (<=))
       ([1; 3; 2; 4; 5], true);

     assert_equal ~msg:"bubble2 10"
       (bubble2 [1; 3; 2; 4; 5] (<=))
       ([1; 2; 3; 4; 5], true);

     assert_equal ~msg:"bubble2 11"
       (bubble2 [5; 4; 3; 2; 1] (<=))
       ([4; 3; 2; 1; 5], true);

     assert_equal ~msg:"bubble2 12"
       (bubble2 [4; 3; 2; 1; 5] (<=))
       ([3; 2; 1; 4; 5], true);

     assert_equal ~msg:"bubble2 13"
       (bubble2 [3; 2; 1; 4; 5] (<=))
       ([2; 1; 3; 4; 5], true);

     assert_equal ~msg:"bubble2 14"
       (bubble2 [2; 1; 3; 4; 5] (<=))
       ([1; 2; 3; 4; 5], true);

     assert_equal ~msg:"bubble2 15"
       (bubble2 [1; 2; 3; 4; 5] (<=))
       ([1; 2; 3; 4; 5], false);

     assert_equal ~msg:"bubble2 16"
       (bubble2 [1; 1] (<=))
       ([1; 1], false);

     assert_equal ~msg:"bubble2 17"
       (bubble2 [1; 1; 1; 1; 1; 1; 1] (<=))
       ([1; 1; 1; 1; 1; 1; 1], false);

     assert_equal ~msg:"bubble2 18"
       (bubble2 [1; 1; 2; 2; 3; 3; 4; 4; 5; 5] (<=))
       ([1; 1; 2; 2; 3; 3; 4; 4; 5; 5], false);
  );

  "Problem B.3.c: bubble_sort" >:: (fun _ ->

     assert_equal ~msg:"bubble_sort 1"
       (bubble_sort [] (<=))
       [];

     assert_equal ~msg:"bubble_sort 2"
       (bubble_sort [1] (<=))
       [1];

     assert_equal ~msg:"bubble_sort 3"
       (bubble_sort [1; 2] (<=))
       [1; 2];

     assert_equal ~msg:"bubble_sort 4"
       (bubble_sort [2; 1] (<=))
       [1; 2];

     assert_equal ~msg:"bubble_sort 5"
       (bubble_sort [5; 1; 2; 3; 4] (<=))
       [1; 2; 3; 4; 5];

     assert_equal ~msg:"bubble_sort 6"
       (bubble_sort [5; 1; 3; 2; 4] (<=))
       [1; 2; 3; 4; 5];

     assert_equal ~msg:"bubble_sort 7"
       (bubble_sort [5; 4; 3; 2; 1] (<=))
       [1; 2; 3; 4; 5];

     assert_equal ~msg:"bubble_sort 8"
       (bubble_sort [5; 1; 3; 2; 3; 4; 2; 1; 5] (<=))
       [1; 1; 2; 2; 3; 3; 4; 5; 5];

     assert_equal ~msg:"bubble_sort 9"
       (bubble_sort [1; 1] (<=))
       [1; 1];

     assert_equal ~msg:"bubble_sort 10"
       (bubble_sort [1; 1; 1; 1; 1; 1; 1] (<=))
       [1; 1; 1; 1; 1; 1; 1];

     assert_equal ~msg:"bubble_sort 11"
       (bubble_sort [1; 2; 3; 4; 5; 4; 3; 2; 1] (<=))
       [1; 1; 2; 2; 3; 3; 4; 4; 5];
  );

  "Problem B.4.a: span" >:: (fun _ ->
     assert_equal ~msg:"span 1"
       (span (fun x -> x < 3) [1;2;3;4;1;2;3;4])
       ([1; 2], [3; 4; 1; 2; 3; 4]);

     assert_equal ~msg:"span 2"
       (span (fun x -> x < 9) [1;2;3])
       ([1; 2; 3], []);

     assert_equal ~msg:"span 3"
       (span (fun x -> x < 0) [1;2;3])
       ([], [1; 2; 3]);

     assert_equal ~msg:"span 4"
       (span (fun x -> x < 2) [])
       ([], []);
  );

  "Problem B.4.b: group_by" >:: (fun _ ->
     assert_equal ~msg:"group_by 1"
       (group_by (=) [])
       [];

     assert_equal ~msg:"group_by 2"
       (group_by (=) [1])
       [[1]];

     assert_equal ~msg:"group_by 3"
       (group_by (=) [1;1;1;1;2;3;3;4;4;4;5])
       [[1;1;1;1];[2];[3;3];[4;4;4];[5]];

     assert_equal ~msg:"group_by 4"
       (group_by (<>) [1; 1; 1; 2; 3; 1; 4; 4; 5])
       [[1]; [1]; [1; 2; 3]; [1; 4; 4; 5]];

     assert_equal ~msg:"group_by 5"
       (group_by (>) [1; 3; 5; 1; 4; 2; 6; 5; 4])
       [[1]; [3]; [5; 1; 4; 2]; [6; 5; 4]];
  );

  "Problem B.4.c: scan_left" >:: (fun _ ->
     assert_equal ~msg:"scan_left 1"
       (scan_left (+) 0 [1;2;3;4;5])
       [0; 1; 3; 6; 10; 15];

     assert_equal ~msg:"scan_left 2"
       (scan_left ( * ) 1 [1;2;3;4;5])
       [1; 1; 2; 6; 24; 120];

     assert_equal ~msg:"scan_left 3"
       (scan_left (+) 42 [])
       [42];

     assert_equal ~msg:"scan_left 4"
       (scan_left (-) 100 [1;2;3;4])
       [100; 99; 97; 94; 90];
  );

  "Problem B.4.d: unfold" >:: (fun _ ->
     let range m n =
       unfold
         (fun x -> if x > n then None else Some (x, x + 1))
         m
     in
     let fibs n =
       unfold
         (fun (x, y) -> if x > n then None else Some (x, (y, x + y)))
         (0, 1)
     in
       begin
         assert_equal ~msg:"unfold 1"        
           (range 10 0)
           [];

         assert_equal ~msg:"unfold 2"
           (range 0 0)
           [0];

         assert_equal ~msg:"unfold 3"
           (range 0 10)
           [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10];

         assert_equal ~msg:"unfold 4"
           (range 5 10)
           [5; 6; 7; 8; 9; 10];

         assert_equal ~msg:"unfold 5"
           (fibs (-1))
           [];

         assert_equal ~msg:"unfold 6"
           (fibs 0)
           [0];

         assert_equal ~msg:"unfold 7"
           (fibs 10)
           [0; 1; 1; 2; 3; 5; 8];

         assert_equal ~msg:"unfold 8"
           (fibs 100)
           [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89];

         assert_equal ~msg:"unfold 9"
           (fibs 1000)
           [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987];
       end
  );

  (* ---------------------------------------------------------------------- 
   * Part C.
   * ---------------------------------------------------------------------- *)

  "Problem C.1: tree_search" >:: (fun c ->
    assert_bool "test0" (not (tree_search 4 tree0));

    assert_bool "test1" (tree_search 4 tree1);

    assert_bool "test2" (tree_search 4 tree2);
    assert_bool "test2" (tree_search 6 tree2);

    assert_bool "test3" (tree_search 4 tree3);
    assert_bool "test3" (tree_search 6 tree3);
    assert_bool "test3" (tree_search 3 tree3);

    assert_bool "test4" (tree_search 4 tree4);
    assert_bool "test4" (tree_search 6 tree4);
    assert_bool "test4" (tree_search 3 tree4);
    assert_bool "test4" (tree_search 5 tree4);

    assert_bool "test5" (tree_search 4 tree5);
    assert_bool "test5" (tree_search 6 tree5);
    assert_bool "test5" (tree_search 3 tree5);
    assert_bool "test5" (tree_search 5 tree5);
    assert_bool "test5" (tree_search 7 tree5);

    assert_bool "test6" (tree_search 4 tree6);
    assert_bool "test6" (tree_search 6 tree6);
    assert_bool "test6" (tree_search 3 tree6);
    assert_bool "test6" (tree_search 5 tree6);
    assert_bool "test6" (tree_search 7 tree6);
    assert_bool "test6" (tree_search 9 tree6);

    assert_bool "test7" (tree_search 4 tree7);
    assert_bool "test7" (tree_search 6 tree7);
    assert_bool "test7" (tree_search 3 tree7);
    assert_bool "test7" (tree_search 5 tree7);
    assert_bool "test7" (tree_search 7 tree7);
    assert_bool "test7" (tree_search 9 tree7);
    assert_bool "test7" (tree_search 1 tree7);

    assert_bool "test8" (tree_search 4 tree8);
    assert_bool "test8" (tree_search 6 tree8);
    assert_bool "test8" (tree_search 3 tree8);
    assert_bool "test8" (tree_search 5 tree8);
    assert_bool "test8" (tree_search 7 tree8);
    assert_bool "test8" (tree_search 9 tree8);
    assert_bool "test8" (tree_search 1 tree8);
    assert_bool "test8" (tree_search 2 tree8);

    assert_bool "test9" (tree_search 4 tree9);
    assert_bool "test9" (tree_search 6 tree9);
    assert_bool "test9" (tree_search 3 tree9);
    assert_bool "test9" (tree_search 5 tree9);
    assert_bool "test9" (tree_search 7 tree9);
    assert_bool "test9" (tree_search 9 tree9);
    assert_bool "test9" (tree_search 1 tree9);
    assert_bool "test9" (tree_search 2 tree9);
    assert_bool "test9" (tree_search 10 tree9);

    assert_bool "test10" (tree_search 4 tree10);
    assert_bool "test10" (tree_search 6 tree10);
    assert_bool "test10" (tree_search 3 tree10);
    assert_bool "test10" (tree_search 5 tree10);
    assert_bool "test10" (tree_search 7 tree10);
    assert_bool "test10" (tree_search 9 tree10);
    assert_bool "test10" (tree_search 1 tree10);
    assert_bool "test10" (tree_search 2 tree10);
    assert_bool "test10" (tree_search 10 tree10);
    assert_bool "test10" (tree_search 8 tree10);
  );

  "Problem C.2: insert_helper" >:: (fun c ->
    assert_equal ~msg:"insert_helper 1" (test_tree_insert 4 tree0) tree1;
    assert_equal ~msg:"insert_helper 2" (test_tree_insert 6 tree1) tree2;
    assert_equal ~msg:"insert_helper 3" (test_tree_insert 3 tree2) tree3;
    assert_equal ~msg:"insert_helper 4" (test_tree_insert 5 tree3) tree4;
    assert_equal ~msg:"insert_helper 5" (test_tree_insert 7 tree4) tree5;
    assert_equal ~msg:"insert_helper 6" (test_tree_insert 9 tree5) tree6;
    assert_equal ~msg:"insert_helper 7" (test_tree_insert 1 tree6) tree7;
    assert_equal ~msg:"insert_helper 8" (test_tree_insert 2 tree7) tree8;
    assert_equal ~msg:"insert_helper 9" (test_tree_insert 10 tree8) tree9;
    assert_equal ~msg:"insert_helper 10" (test_tree_insert 8 tree9) tree10;
    assert_equal ~msg:"insert_helper 11" (test_tree_insert 11 tree10) tree11;
    assert_equal ~msg:"insert_helper 12" (test_tree_insert 12 tree11) tree12;
    assert_equal ~msg:"insert_helper 13" (test_tree_insert 52 tree7a) tree7b;
    assert_equal ~msg:"insert_helper 14" (test_tree_insert 51 tree7b) tree7c;
  );

]

let _ = run_test_tt_main all_tests

