(* Tests for lab2.ml *)

open OUnit2
open Lab2
open Num

let assert_float_equal ~msg ~expected ~got dx =
  if abs_float (expected -. got) > dx then
    Printf.printf "\n%s: expected (%.10g) but got (%.10g)\n%!"
      msg expected got

let square n = n * n
let cube n = n * n * n
let inc n = n + 1
let step1 n = n + 1
let step2 n = n + 2

let ni = num_of_int
let squaren n = n */ n
let cuben n = n */ n */ n
let step1n n = n +/ (ni 1)
let step2n n = n +/ (ni 2)
let pi = atan 1.0 *. 4.

let rec range = function 1 -> [1] | n -> range (n-1) @ [n]

let all_tests = "all" >:::
[ 
  "fast_expt" >:: (fun _ -> 
     assert_equal (fast_expt 2 0) 1;
     assert_equal (fast_expt 2 5) 32;
     assert_equal (fast_expt 2 16) 65536;
     assert_equal (fast_expt 3 4) 81;
     assert_equal (fast_expt 14 5) 537824
  );

  "ifast_expt" >:: (fun _ -> 
     assert_equal (ifast_expt 2 0) 1;
     assert_equal (ifast_expt 2 5) 32;
     assert_equal (ifast_expt 2 16) 65536;
     assert_equal (ifast_expt 3 4) 81;
     assert_equal (ifast_expt 14 5) 537824
  );

  "fast_mult" >:: (fun _ ->
     assert_equal (fast_mult 2 0) 0;
     assert_equal (fast_mult 2 1) 2;
     assert_equal (fast_mult 2 7) 14;
     assert_equal (fast_mult 2 16) 32;
     assert_equal (fast_mult 3 4) 12;
     assert_equal (fast_mult 31 43) 1333
  );

  "ifast_mult" >:: (fun _ ->
     assert_equal (ifast_mult 2 0) 0;
     assert_equal (ifast_mult 2 1) 2;
     assert_equal (ifast_mult 2 7) 14;
     assert_equal (ifast_mult 2 16) 32;
     assert_equal (ifast_mult 3 4) 12;
     assert_equal (ifast_mult 31 43) 1333
  );

  "isum" >:: (fun _ ->
     assert_equal (isum squaren (ni 10) step1n (ni 0)) (ni 0);
     assert_equal (isum squaren (ni 4) step1n (ni 4)) (ni 16);
     assert_equal (isum squaren (ni 0) step1n (ni 10)) (ni 385)
  );

  "product_rec" >:: (fun _ ->
     assert_equal (product_rec squaren (ni 10) step1n (ni 1)) (ni 1);
     assert_equal (product_rec squaren (ni 1) step1n (ni 5)) (ni 14400);
     assert_equal (product_rec cuben (ni 1) step2n (ni 5)) (ni 3375)
  );

  "factorial_rec" >:: (fun _ ->
     assert_equal (factorial_rec (ni 0)) (ni 1);
     assert_equal (factorial_rec (ni 5)) (ni 120);
     assert_equal (factorial_rec (ni 10)) (ni 3628800);
  );

  "product_iter" >:: (fun _ ->
     assert_equal (product_iter squaren (ni 10) step1n (ni 1)) (ni 1);
     assert_equal (product_iter squaren (ni 1) step1n (ni 5)) (ni 14400);
     assert_equal (product_iter cuben (ni 1) step2n (ni 5)) (ni 3375)
  );

  "factorial_iter" >:: (fun _ ->
     assert_equal (factorial_iter (ni 0)) (ni 1);
     assert_equal (factorial_iter (ni 5)) (ni 120);
     assert_equal (factorial_iter (ni 10)) (ni 3628800);
  );

  "pi_approx" >:: (fun _ ->
     assert_bool "pi approximation out of range" 
       (cmp_float ~epsilon:0.001 pi pi_approx)
  );

(*
  "accumulate_rec" >:: (fun _ ->
     assert_equal 0 0  (* TODO *)
  );

  "accumulate_iter" >:: (fun _ ->
     assert_equal 0 0  (* TODO *)
  );
*)

  "sum" >:: (fun _ ->
     assert_equal (sum squaren (ni 10) step1n (ni 1)) (ni 0);
     assert_equal (sum squaren (ni 1) step1n (ni 5)) (ni 55);
     assert_equal (sum cuben (ni 1) step2n (ni 5)) (ni 153)
  );

  "product" >:: (fun _ ->
     assert_equal (product squaren (ni 10) step1n (ni 1)) (ni 1);
     assert_equal (product squaren (ni 1) step1n (ni 5)) (ni 14400);
     assert_equal (product cuben (ni 1) step2n (ni 5)) (ni 3375)
  );

  "compose" >:: (fun _ ->
     assert_equal ((compose square step1) 6) 49;
     assert_equal ((compose step1 square) 6) 37;
     assert_equal ((compose cube step2) 6) 512;
     assert_equal ((compose step2 cube) 6) 218
  );

  "repeated" >:: (fun _ ->
     assert_equal ((repeated square 2) 5) 625;
     assert_equal ((repeated cube 3) 2) 134217728
  );

  "smooth" >:: (fun _ ->
     assert_float_equal ~msg:"smooth 1"
       ~expected:0.0
       ~got:(smooth 0.1 sin 0.0)
       0.00001;
     assert_float_equal ~msg:"smooth 2"
       ~expected:0.0995009158
       ~got:(smooth 0.1 sin 0.1)
       0.00001;
     assert_float_equal ~msg:"smooth 3"
       ~expected:0.8386684181
       ~got:(smooth 0.1 sin 1.0)
       0.00001;
     assert_float_equal ~msg:"smooth 4"
       ~expected:0.1406499999
       ~got:(smooth 0.1 sin 3.0)
       0.00001;
     assert_float_equal ~msg:"smooth 5"
       ~expected:0.9966694435
       ~got:(smooth 0.1 sin (pi /. 2.0))
       0.00001;
  );

  "nsmoothed" >:: (fun _ ->
     assert_float_equal ~msg:"nsmoothed 1"
       ~expected:0.0
       ~got:(nsmoothed 0.1 sin 0 0.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 2"
       ~expected:0.8414709848
       ~got:(nsmoothed 0.1 sin 0 1.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 3"
       ~expected:0.9092974268
       ~got:(nsmoothed 0.1 sin 0 2.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 4"
       ~expected:1.0
       ~got:(nsmoothed 0.1 sin 0 (pi /. 2.0))
       0.00001;
     assert_float_equal ~msg:"nsmoothed 5"
       ~expected:(-0.5440211109)
       ~got:(nsmoothed 0.1 sin 0 10.0)
       0.00001;

     assert_float_equal ~msg:"nsmoothed 6"
       ~expected:0.0
       ~got:(nsmoothed 0.1 sin 1 0.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 7"
       ~expected:0.8386684182
       ~got:(nsmoothed 0.1 sin 1 1.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 8"
       ~expected:0.9062689604
       ~got:(nsmoothed 0.1 sin 1 2.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 9"
       ~expected:0.9966694435
       ~got:(nsmoothed 0.1 sin 1 (pi /. 2.0))
       0.00001;
     assert_float_equal ~msg:"nsmoothed 10"
       ~expected:(-0.5422092179)
       ~got:(nsmoothed 0.1 sin 1 10.0)
       0.00001;

     assert_float_equal ~msg:"nsmoothed 11"
       ~expected:0.0
       ~got:(nsmoothed 0.1 sin 10 0.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 12"
       ~expected:0.8138616443
       ~got:(nsmoothed 0.1 sin 10 1.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 13"
       ~expected:0.8794626461
       ~got:(nsmoothed 0.1 sin 10 2.0)
       0.00001;
     assert_float_equal ~msg:"nsmoothed 14"
       ~expected:0.9671891949
       ~got:(nsmoothed 0.1 sin 10 (pi /. 2.0))
       0.00001;
     assert_float_equal ~msg:"nsmoothed 15"
       ~expected:(-0.5261713402)
       ~got:(nsmoothed 0.1 sin 10 10.0)
       0.00001;
  );

  "is_prime" >:: (fun _ ->
     assert_equal (List.filter is_prime (range 100))
       [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 
        59; 61; 67; 71; 73; 79; 83; 89; 97];
     assert_equal (List.map is_prime [-2; -1; 0; 1]) 
       [false; false; false; false]
  );

  "smallest_prime_factor" >:: (fun _ ->
     let r = [4; 6; 8; 9; 10; 12; 14; 15; 16; 18; 20; 21; 22; 24; 25; 
              26; 27; 28; 30; 32; 33; 34; 35; 36; 38; 39; 40; 42; 44; 
              45; 46; 48; 49; 50; 51; 52; 54; 55; 56; 57; 58; 60; 62; 
              63; 64; 65; 66; 68; 69; 70; 72; 74; 75; 76; 77; 78; 80; 
              81; 82; 84; 85; 86; 87; 88; 90; 91; 92; 93; 94; 95; 96; 
              98; 99; 100] in
     assert_equal (List.map smallest_prime_factor r) 
       [2; 2; 2; 3; 2; 2; 2; 3; 2; 2; 2; 3; 2; 2; 5; 2; 3; 2; 2; 2; 3; 
        2; 5; 2; 2; 3; 2; 2; 2; 3; 2; 2; 7; 2; 3; 2; 2; 5; 2; 3; 2; 2; 
        2; 3; 2; 5; 2; 2; 3; 2; 2; 2; 3; 2; 7; 2; 2; 3; 2; 2; 5; 2; 3; 
        2; 2; 7; 2; 3; 2; 5; 2; 2; 3; 2];
     assert_equal (smallest_prime_factor 42009217) 641
  )
]

let _ = run_test_tt_main all_tests

