(* Tests for lab3.ml *)

open OUnit2
open Lab3

let check_within msg f1 f2 prec =
  assert_bool msg (cmp_float ~epsilon:prec f1 f2)

let square n = n * n

(* Compare two lists for "set equality" i.e. that they have the
 * same elements.  Here we require that no list have extra duplicates
 * of existing elements that the other list doesn't have. *)
let set_equal lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

let eps = 1.0e-8

let all_tests = "all" >:::
[ 
(*** Problem A.0 ***)

  "range" >:: (fun _ ->
    assert_equal ~msg:"range 1"
      (range 1 10) [1;2;3;4;5;6;7;8;9;10];
    assert_equal ~msg:"range 2"
      (range 1 1) [1];
    assert_equal ~msg:"range 3"
      (range 1 0) [];
  );

  "irange" >:: (fun _ ->
    assert_equal ~msg:"irange 1"
      (irange 1 10) [1;2;3;4;5;6;7;8;9;10];
    assert_equal ~msg:"irange 2"
      (irange 1 1) [1];
    assert_equal ~msg:"irange 3"
      (irange 1 0) [];
  );

(*** Problem A.1 ***)

  "last_sublist" >:: (fun _ ->
    assert_raises (Invalid_argument "last_sublist: empty list") 
      (fun () -> last_sublist []);
    assert_equal ~msg:"last_sublist 1"
      (last_sublist [1]) [1];
    assert_equal ~msg:"last_sublist 2"
      (last_sublist [1; 2; 3; 4; 5]) [5]
  );

(*** Problem A.2 ***)

  "reverse" >:: (fun _ ->
    assert_equal ~msg:"reverse 1"
      (reverse []) [];
    assert_equal ~msg:"reverse 2"
      (reverse [1; 2; 3; 4; 5]) [5; 4; 3; 2; 1];
    assert_equal ~msg:"reverse 3"
      (reverse [[1; 4]; [9]; [16; 25]]) [[16; 25]; [9]; [1; 4]]
  );

(*** Problem A.3 ***)

  "square_list" >:: (fun _ ->
    assert_equal ~msg:"square_list 1"
      (square_list []) [];
    assert_equal ~msg:"square_list 2"
      (square_list [2]) [4];
    assert_equal ~msg:"square_list 3"
      (square_list [-2]) [4];
    assert_equal ~msg:"square_list 4"
      (square_list [-2; 2; -2]) [4; 4; 4];
    assert_equal ~msg:"square_list 5"
      (square_list [-3; 2; -1; 0]) [9; 4; 1; 0];
    assert_equal ~msg:"square_list 6"
      (square_list [1; 2; 3; 4]) [1; 4; 9; 16];
    assert_equal ~msg:"square_list 7"
      (square_list [1; 2; 3; 4; 5]) [1; 4; 9; 16; 25];
  );

  "square_list2" >:: (fun _ ->
    assert_equal ~msg:"square_list2 1"
      (square_list2 []) [];
    assert_equal ~msg:"square_list2 2"
      (square_list2 [2]) [4];
    assert_equal ~msg:"square_list2 3"
      (square_list2 [-2]) [4];
    assert_equal ~msg:"square_list2 4"
      (square_list2 [-2; 2; -2]) [4; 4; 4];
    assert_equal ~msg:"square_list2 5"
      (square_list2 [-3; 2; -1; 0]) [9; 4; 1; 0];
    assert_equal ~msg:"square_list2 6"
      (square_list2 [1; 2; 3; 4]) [1; 4; 9; 16];
    assert_equal ~msg:"square_list2 7"
      (square_list2 [1; 2; 3; 4; 5]) [1; 4; 9; 16; 25];
  );

(*** Problem A.4: no tests ***)

(*** Problem A.5 ***)

  "count_negative_numbers" >:: (fun _ ->
    assert_equal ~msg:"count_negative_numbers 1"
      (count_negative_numbers []) 0;
    assert_equal ~msg:"count_negative_numbers 2"
      (count_negative_numbers [-2; -1; 0; 1; 2]) 2;
    assert_equal ~msg:"count_negative_numbers 3"
      (count_negative_numbers [1; 2; 3; 4; 5]) 0;
    assert_equal ~msg:"count_negative_numbers 4"
      (count_negative_numbers [-2; 0; -2; 0; -1; 1]) 3
  );

(*** Problem A.6 ***)

  "power_of_two_list" >:: (fun _ ->
    assert_equal ~msg:"power_of_two_list 1"
      (power_of_two_list 0) [];
    assert_equal ~msg:"power_of_two_list 2"
      (power_of_two_list 1) [1];
    assert_equal ~msg:"power_of_two_list 3"
      (power_of_two_list 5) [1; 2; 4; 8; 16]
  );

(*** Problem A.7 ***)

  "prefix_sum" >:: (fun _ ->
    assert_equal ~msg:"prefix_sum 1"
      (prefix_sum []) [];
    assert_equal ~msg:"prefix_sum 2"
      (prefix_sum [1]) [1];
    assert_equal ~msg:"prefix_sum 3"
      (prefix_sum [1; 1; 1; 1]) [1; 2; 3; 4];
    assert_equal ~msg:"prefix_sum 4"
      (prefix_sum [1; -1; 1; -1]) [1; 0; 1; 0];
    assert_equal ~msg:"prefix_sum 5"
      (prefix_sum [1; 2; 3; 4]) [1; 3; 6; 10]
  );

(*** Problem A.8 ***)

  "deep_reverse" >:: (fun _ ->
    assert_equal ~msg:"deep_reverse 1"
      (deep_reverse []) [];
    assert_equal ~msg:"deep_reverse 2"
      (deep_reverse [[1; 1]; [2; 2]]) [[2; 2]; [1; 1]];
    assert_equal ~msg:"deep_reverse 3"
      (deep_reverse [[1; 2]; [3; 4]]) [[4; 3]; [2; 1]];
    assert_equal ~msg:"deep_reverse 4"
      (deep_reverse [[[1; 2]; [3; 4]]; [[5; 6]; [7; 8]]])
      [[[7; 8]; [5; 6]]; [[3; 4]; [1; 2]]]
   );

(*** Problem A.9 ***)

  "deep_reverse_nested" >:: (fun _ ->
    assert_equal ~msg:"deep_reverse_nested 1"
      (deep_reverse_nested (Value 10)) (Value 10);
    assert_equal ~msg:"deep_reverse_nested 2"
      (deep_reverse_nested 
        (List [Value 10; Value 20; Value 30; Value 40])) 
      (List [Value 40; Value 30; Value 20; Value 10]);
    assert_equal ~msg:"deep_reverse_nested 3"
      (deep_reverse_nested 
        (List [List [Value 10; Value 20]; List [Value 30; Value 40]])) 
      (List [List [Value 40; Value 30]; List [Value 20; Value 10]]);
    assert_equal ~msg:"deep_reverse_nested 4"
      (deep_reverse_nested 
        (List [Value 10; List [Value 20; Value 30]])) 
      (List [List [Value 30; Value 20]; Value 10]);
    assert_equal ~msg:"deep_reverse_nested 5"
      (deep_reverse_nested 
        (List [List [Value 10; Value 20]; Value 30]))
      (List [Value 30; List [Value 20; Value 10]]);
    assert_equal ~msg:"deep_reverse_nested 6"
      (deep_reverse_nested 
        (List [Value 10; List [Value 20; List [Value 30; Value 40]; Value 50]; Value 60])) 
      (List [Value 60; List [Value 50; List [Value 40; Value 30]; Value 20]; Value 10])
  );

(*** Problem B.1 ***)

  "quicksort" >:: (fun _ ->
     assert_equal ~msg:"quicksort 1"
       (quicksort (<) []) [];
     assert_equal ~msg:"quicksort 2"
       (quicksort (<) [1]) [1];
     assert_equal ~msg:"quicksort 3"
       (quicksort (<) [1;2;3;4;5]) [1;2;3;4;5];
     assert_equal ~msg:"quicksort 4"
       (quicksort (<) [5;4;3;2;1;1;2;3;4;5]) [1;1;2;2;3;3;4;4;5;5];
     assert_equal ~msg:"quicksort 5"
       (quicksort (>) [5;4;3;2;1;1;2;3;4;5]) [5;5;4;4;3;3;2;2;1;1]
  );

(*** Problem B.4 ***)

  "insertion_sort" >:: (fun _ ->
     assert_equal ~msg:"insertion sort 1"
       (insertion_sort (<) []) [];
     assert_equal ~msg:"insertion sort 2"
       (insertion_sort (<) [1]) [1];
     assert_equal ~msg:"insertion sort 3"
       (insertion_sort (<) [1;2;3;4;5]) [1;2;3;4;5];
     assert_equal ~msg:"insertion sort 4"
       (insertion_sort (<) [5;4;3;2;1;1;2;3;4;5]) [1;1;2;2;3;3;4;4;5;5];
     assert_equal ~msg:"insertion sort 5"
       (insertion_sort (>) [5;4;3;2;1;1;2;3;4;5]) [5;5;4;4;3;3;2;2;1;1]
  );

(*** Problem C.1 ***)

  "subsets" >:: (fun _ ->
     assert_equal ~msg:"subsets 1"
       (subsets []) [[]];
     assert_bool "subsets 2"
       (set_equal (subsets [1]) [[];[1]]);
     assert_bool "subsets 3"
       (set_equal (subsets [1;2;3]) [[];[3];[2];[2;3];[1];[1;3];[1;2];[1;2;3]]);
  );

(*** Problem C.2 ***)

  "map, append, length" >:: (fun _ ->
     assert_equal ~msg:"map 1"
       (map square [1;2;3;4;5]) [1;4;9;16;25];
     assert_equal ~msg:"map 2"
       (map square []) [];
     assert_equal ~msg:"append 1"
       (append [] []) [];
     assert_equal ~msg:"append 2"
       (append [1] [2]) [1;2];
     assert_equal ~msg:"append 3"
       (append [1;2;3;4;5] []) [1;2;3;4;5];
     assert_equal ~msg:"append 4"
       (append [] [1;2;3;4;5]) [1;2;3;4;5];
     assert_equal ~msg:"append 5"
       (append [1;2;3;4;5] [6;7;8;9;10]) [1;2;3;4;5;6;7;8;9;10];
     assert_equal ~msg:"length 1"
       (length []) 0;
     assert_equal ~msg:"length 2"
       (length [1;2;3;4;5]) 5
  );

  "accumulate_n" >:: (fun _ ->
     assert_equal ~msg:"accumulate_n 1"
       (accumulate_n (+) 0 [[];[];[]]) [];
     assert_equal ~msg:"accumulate_n 2"
       (accumulate_n (+) 0 [[1;2;3];[4;5;6];[7;8;9];[10;11;12]])
       [22;26;30];
     assert_equal ~msg:"accumulate_n 3"
       (accumulate_n ( * ) 1 [[2;3];[4;5]]) [8;15]
  );

  "dot_product, matrix_times_vector, matrix_times_matrix" >:: (fun _ ->
     assert_equal ~msg:"dot_product 1"
       (dot_product [] []) 0;
     assert_equal ~msg:"dot_product 2"
       (dot_product [1;2;3] [4;5;6]) 32;
     assert_equal ~msg:"matrix_times_vector 1"
       (matrix_times_vector [[1;0];[0;1]] [10;20]) [10;20];
     assert_equal ~msg:"matrix_times_vector 2"
       (matrix_times_vector [[1;2];[3;4]] [-2;3]) [4;6];
     assert_equal ~msg:"transpose 1"
       (transpose [[1;2];[3;4]]) [[1;3];[2;4]];
     assert_equal ~msg:"transpose 2"
       (transpose [[1;2;3];[4;5;6]]) [[1;4];[2;5];[3;6]];
     assert_equal ~msg:"matrix_times_matrix 1"
       (matrix_times_matrix [[1;0];[0;1]] [[1;2];[3;4]]) 
       [[1;2];[3;4]];
     assert_equal ~msg:"matrix_times_matrix 2"
       (matrix_times_matrix [[1;2];[3;4]] [[1;2];[3;4]]) 
       [[7;10];[15;22]];
     assert_equal ~msg:"matrix_times_matrix 3"
       (matrix_times_matrix [[1;2;3];[4;5;6]] [[1;2];[3;4];[5;6]])
       [[22;28];[49;64]]
  );

]

let _ = run_test_tt_main all_tests

