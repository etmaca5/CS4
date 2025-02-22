(* Tests for lab4.ml *)

open OUnit2
open Lab4

let check_within msg f1 f2 prec =
  assert_bool msg (cmp_float ~epsilon:prec f1 f2)

let check_within_points msg p1 p2 prec =
  let (x1, y1) = get_coords p1 in
  let (x2, y2) = get_coords p2 in
    check_within msg x1 x2 prec;
    check_within msg y1 y2 prec

let eps = 1.0e-8

let p1 = make_point 0.0 0.0
let p2 = make_point 10.0 0.0
let p3 = make_point 10.0 10.0
let s1 = make_segment p1 p2
let s2 = make_segment p2 p3
let s3 = make_segment p3 p1

let square n = n * n

(* Compare two lists for "set equality" i.e. that they have the
 * same elements.  Here we require that no list have extra duplicates
 * of existing elements that the other list doesn't have. *)
let set_equal lst1 lst2 =
  List.sort compare lst1 = List.sort compare lst2

(* Test cases for mobile problem. *)

let m0 = 
  make_mobile 
    (make_weight 1 1) 
    (make_weight 1 1)

let m1 = 
  make_mobile
    (make_weight 3 4)
    (make_structure 
      4
      (make_mobile
        (make_weight 1 2)
        (make_weight 2 1)))

let m2 =
  make_mobile
    (make_weight 1 400)
    (make_structure 
      10
      (make_mobile
        (make_weight 100 1)
        (make_weight 1 200)))
  
let m3 =
  make_mobile
    (make_weight 1 (total_weight1 m2))
    (make_structure 1 m2)

let m0' = 
  make_mobile'
    (make_weight' 1 1) 
    (make_weight' 1 1)

let m1' = 
  make_mobile'
    (make_weight' 3 4)
    (make_structure' 
      4
      (make_mobile'
        (make_weight' 1 2)
        (make_weight' 2 1)))

let m2' =
  make_mobile'
    (make_weight' 1 400)
    (make_structure' 
      10
      (make_mobile'
        (make_weight' 100 1)
        (make_weight' 1 200)))
  
let m3' =
  make_mobile'
    (make_weight' 1 (total_weight' m2'))
    (make_structure' 1 m2')

(* Test cases for tree problems *)

let tree1 = Tree 
  [Num 10; 
   Sub (Tree [Num 20; 
              Sub (Tree [Num 42; Sub (Tree []); Num 12]); 
              Sub (Tree []);
              Sub (Tree [Num 13; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 2; Num 3])]
  
let tree2 = Tree
  [Num 100; 
   Sub (Tree [Num 400; 
              Sub (Tree [Num 1764; Sub (Tree []); Num 144]); 
              Sub (Tree []);
              Sub (Tree [Num 169; Sub (Tree [])])]);
   Sub (Tree []);
   Sub (Tree [Num 1; Num 4; Num 9])]

let square_tree'' t = tree_map square t 

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d

let all_tests = "all" >:::
[ 
  (*** Part A. ***)

  (* A.1 *)

  "make_point/get_coords" >:: (fun _ -> 
     let p = make_point 1.0 3.4 in
     let (x, y) = get_coords p in
       begin
         check_within "get_coords 1: invalid x accessor for points" x 1.0 eps;
         check_within "get_coords 2: invalid y accessor for points" y 3.4 eps
       end
  );

  "make_segment/get_points" >:: (fun _ -> 
     let p1 = make_point 1.0 3.4 in
     let p2 = make_point (-1.0) 43.0 in
     let s = make_segment p1 p2 in
     let (p1', p2') = get_points s in
       begin
         check_within_points "get_points 1: invalid startp accessor for segments" p1' p1 eps;
         check_within_points "get_points 2: invalid endp accessor for segments" p2' p2 eps
       end
  );

  "segment_length" >:: (fun _ -> 
     check_within "segment_length 1" (segment_length s1) 10.0 eps;
     check_within "segment_length 2" (segment_length s2) 10.0 eps;
     check_within "segment_length 3" (segment_length s3) 14.14213 0.00001
  );

  "midpoint_segment" >:: (fun _ ->
     let (x, y) = get_coords (midpoint_segment s3) in
       check_within "midpoint_segment 1: x coord of midpoint of segment s3" x 5.0 eps;
       check_within "midpoint_segment 2: y coord of midpoint of segment s3" y 5.0 eps
  );

  (* A.2 *)
  "make_rectangle" >:: (fun _ ->
     let p1  = make_point (-1.0) 3.4 in
     let p2  = make_point 1.0 43.0 in
     let r   = make_rectangle p1 p2 in
     let rl  = rectangle_lower_segment r in
     let ru  = rectangle_upper_segment r in
     let rlf = rectangle_left_segment  r in
     let rrt = rectangle_right_segment r in
     let rp  = rectangle_perimeter r in
     let ra  = rectangle_area r in
       begin
         check_within "make_rectangle 1" (segment_length rl) 2.0 eps;
         check_within "make_rectangle 2" (segment_length ru) 2.0 eps;
         check_within "make_rectangle 3" (segment_length rlf) 39.6 eps;
         check_within "make_rectangle 4" (segment_length rrt) 39.6 eps;
         check_within "make_rectangle 5" rp 83.2 eps;
         check_within "make_rectangle 6" ra 79.2 eps;
       end
  );

  "make_rectangle2" >:: (fun _ ->
     let x1  = -1.0 in
     let x2  = 1.0 in
     let y1  = 3.4 in
     let y2  = 43.0 in
     let r   = make_rectangle2 x1 y1 x2 y2 in
     let rl  = rectangle_lower_segment2 r in
     let ru  = rectangle_upper_segment2 r in
     let rlf = rectangle_left_segment2  r in
     let rrt = rectangle_right_segment2 r in
     let rp  = rectangle_perimeter2 r in
     let ra  = rectangle_area2 r in
       begin
         check_within "make_rectangle2 1" (segment_length rl) 2.0 eps;
         check_within "make_rectangle2 2" (segment_length ru) 2.0 eps;
         check_within "make_rectangle2 3" (segment_length rlf) 39.6 eps;
         check_within "make_rectangle2 4" (segment_length rrt) 39.6 eps;
         check_within "make_rectangle2 5" rp 83.2 eps;
         check_within "make_rectangle2 6" ra 79.2 eps;
       end
  );

  (* A.3: no tests *)

  (* A.4 *)

  "integer pairs" >:: (fun _ ->
     assert_equal ~msg:"integer pairs 1" (pow 5 7) 78125;
     assert_equal ~msg:"integer pairs 2" (pow 7 5) 16807;
     assert_equal ~msg:"integer pairs 3" (int_log 5 78125) 7;
     assert_equal ~msg:"integer pairs 4" (int_log 7 16807) 5;
     assert_equal ~msg:"integer pairs 5" (make_pairi 5 7) 69984;
     assert_equal ~msg:"integer pairs 6" (firsti (make_pairi 5 7)) 5;
     assert_equal ~msg:"integer pairs 7" (secondi (make_pairi 5 7)) 7;
     assert_equal ~msg:"integer pairs 8" (make_pairi 7 5) 31104;
     assert_equal ~msg:"integer pairs 9" (firsti (make_pairi 7 5)) 7;
     assert_equal ~msg:"integer pairs 10" (secondi (make_pairi 7 5)) 5
  );

  (* A.5 *)

  "unary integers" >:: (fun _ ->
     assert_equal ~msg:"unary_integers 1"
       (prev [()]) [];
     assert_equal ~msg:"unary_integers 2"
       (prev [(); (); (); (); ()]) [(); (); (); ()];
     assert_equal ~msg:"unary_integers 3"
       (integer_to_unary 0) [];
     assert_equal ~msg:"unary_integers 4"
       (integer_to_unary 1) [()];
     assert_equal ~msg:"unary_integers 5"
       (integer_to_unary 10) [(); (); (); (); (); (); (); (); (); ()];
     assert_equal ~msg:"unary_integers 6"
       (unary_to_integer [(); (); (); (); (); (); (); (); ()]) 9;
     assert_equal ~msg:"unary_integers 7"
       (unary_to_integer [(); (); ()]) 3;
     assert_equal ~msg:"unary_integers 8"
       (unary_to_integer [()]) 1;
     assert_equal ~msg:"unary_integers 9"
       (unary_to_integer []) 0;
     assert_equal ~msg:"unary_integers 10"
       (unary_add [(); (); ()] []) [(); (); ()];
     assert_equal ~msg:"unary_integers 11"
       (unary_add [] [(); (); ()]) [(); (); ()];
     assert_equal ~msg:"unary_integers 12"
       (unary_add [(); ()] [(); (); ()]) [(); (); (); (); ()];
     assert_equal ~msg:"unary_integers 13"
       (unary_to_integer 
         (unary_add (integer_to_unary 1001) (integer_to_unary 65535))) 66536;
  );

  "unary integers 2" >:: (fun _ ->
     assert_equal ~msg:"unary integers 2"
       (prev' (Succ Zero)) Zero;
     assert_equal ~msg:"unary integers 2"
       (prev' (Succ (Succ (Succ (Succ (Succ Zero)))))) 
         (Succ (Succ (Succ (Succ Zero))));
     assert_equal ~msg:"unary integers 2"
       (integer_to_unary' 0) Zero;
     assert_equal ~msg:"unary integers 2"
       (integer_to_unary' 1) (Succ Zero);
     assert_equal ~msg:"unary integers 2"
       (integer_to_unary' 10) 
       (Succ (Succ (Succ (Succ (Succ 
         (Succ (Succ (Succ (Succ (Succ Zero))))))))));
     assert_equal ~msg:"unary integers 2"
       (unary_to_integer' 
         (Succ (Succ (Succ (Succ (Succ 
           (Succ (Succ (Succ (Succ Zero)))))))))) 9;
     assert_equal ~msg:"unary integers 2"
       (unary_to_integer' (Succ (Succ (Succ Zero)))) 3;
     assert_equal ~msg:"unary integers 2"
       (unary_to_integer' (Succ Zero)) 1;
     assert_equal ~msg:"unary integers 2"
       (unary_to_integer' Zero) 0;
     assert_equal ~msg:"unary integers 2"
       (unary_add' (Succ (Succ (Succ Zero))) Zero) 
         (Succ (Succ (Succ Zero)));
     assert_equal ~msg:"unary integers 2"
       (unary_add' Zero (Succ (Succ (Succ Zero)))) 
         (Succ (Succ (Succ Zero)));
     assert_equal ~msg:"unary integers 2"
       (unary_add' (Succ (Succ Zero)) (Succ (Succ (Succ Zero)))) 
         (Succ (Succ (Succ (Succ (Succ Zero)))));
     assert_equal ~msg:"unary integers 2"
       (unary_to_integer'
         (unary_add' (integer_to_unary' 1001) (integer_to_unary' 65535))) 66536;
  );

  (* A.6: no tests *)

  (* A.7: no tests *)

  (*** Part B. ***)

  (* MAYBE-TODO: add tests for:
   * left_branch, right_branch, branch_length, branch_structure
   * branch_weight1, branch_weight2
   *
   * Also all primed functions: left_branch' etc.
   * (same tests)
   *)

  "is_balanced" >:: (fun _ ->
     assert_bool  "is_balanced 1" (is_balanced m0);
     assert_bool  "is_balanced 2" (is_balanced m1);
     assert_bool  "is_balanced 3" (not (is_balanced m2));
     assert_bool  "is_balanced 4" (not (is_balanced m3));
     assert_equal ~msg:"is_balanced 5" (total_weight1 m0) 2;
     assert_equal ~msg:"is_balanced 6" (total_weight1 m1) 7;
     assert_equal ~msg:"is_balanced 7" (total_weight1 m2) 601;
     assert_equal ~msg:"is_balanced 8" (total_weight1 m3) 1202;
     assert_equal ~msg:"is_balanced 9" (total_weight2 m0) 2;
     assert_equal ~msg:"is_balanced 10" (total_weight2 m1) 7;
     assert_equal ~msg:"is_balanced 11" (total_weight2 m2) 601;
     assert_equal ~msg:"is_balanced 12" (total_weight2 m3) 1202
  );

  "is_balanced'" >:: (fun _ ->
     assert_bool  "is_balanced' 1" (is_balanced' m0');
     assert_bool  "is_balanced' 2" (is_balanced' m1');
     assert_bool  "is_balanced' 3" (not (is_balanced' m2'));
     assert_bool  "is_balanced' 4" (not (is_balanced' m3'));
     assert_equal ~msg:"is_balanced' 5" (total_weight' m0') 2;
     assert_equal ~msg:"is_balanced' 6" (total_weight' m1') 7;
     assert_equal ~msg:"is_balanced' 7" (total_weight' m2') 601;
     assert_equal ~msg:"is_balanced' 8" (total_weight' m3') 1202;
  );

  "square_tree" >:: (fun _ ->
     assert_equal ~msg:"square_tree 1" (square_tree (Tree [])) (Tree []);
     assert_equal ~msg:"square_tree 2" (square_tree' (Tree [])) (Tree []);
     assert_equal ~msg:"square_tree 3" (square_tree tree1) tree2;
     assert_equal ~msg:"square_tree 4" (square_tree' tree1) tree2
  );

  "square_tree''" >:: (fun _ ->
     let square_tree'' tree = tree_map (fun n -> n * n) tree in
       begin
         assert_equal ~msg:"square_tree'' 1" (square_tree'' (Tree [])) (Tree []);
         assert_equal ~msg:"square_tree'' 2" (square_tree'' tree1) tree2;
         assert_equal ~msg:"square_tree'' 3" (square_tree'' tree1) (square_tree tree1);
         assert_equal ~msg:"square_tree'' 4" (square_tree'' tree2) (square_tree tree2)
       end
  );

  (*** Part C. ***)

  "simplify" >:: (fun _ ->
     assert_equal ~msg:"simplify 1" (simplify (Int 42)) (Int 42);
     assert_equal ~msg:"simplify 2" (simplify (Var "x")) (Var "x");
     assert_equal ~msg:"simplify 3" (simplify (Add (Int 32, Int 41))) (Int 73);
     assert_equal ~msg:"simplify 4" (simplify (Add (Add (Int 1, Int 2), Add (Int 3, Int 4))))
                  (Int 10);
     assert_equal ~msg:"simplify 5" (simplify (Add (Mul (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 14);
     assert_equal ~msg:"simplify 6" (simplify (Mul (Mul (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 24);
     assert_equal ~msg:"simplify 7" (simplify (Mul (Add (Int 1, Int 2), Mul (Int 3, Int 4))))
                  (Int 36);
     assert_equal ~msg:"simplify 8" (simplify (Pow (Int 0, 0))) (Int 1);
     assert_equal ~msg:"simplify 9" (simplify (Pow (Int 10, 2))) (Int 100);
     assert_equal ~msg:"simplify 10" (simplify (Pow (Add (Int 1, Int 2), 2))) (Int 9);
     assert_equal ~msg:"simplify 11" (simplify (Add (Var "x", Int 0))) (Var "x");
     assert_equal ~msg:"simplify 12" (simplify (Add (Int 0, Var "x"))) (Var "x");
     assert_equal ~msg:"simplify 13" (simplify (Mul (Int 0, Var "y"))) (Int 0);
     assert_equal ~msg:"simplify 14" (simplify (Mul (Var "y", Int 0))) (Int 0);
     assert_equal ~msg:"simplify 15" (simplify (Mul (Int 1, Var "z"))) (Var "z");
     assert_equal ~msg:"simplify 16" (simplify (Mul (Var "z", Int 1))) (Var "z");
     assert_equal ~msg:"simplify 17" (simplify (Pow (Var "x", 0))) (Int 1);
     assert_equal ~msg:"simplify 18" (simplify (Pow (Var "x", 1))) (Var "x");
     assert_equal ~msg:"simplify 19" (simplify (Pow (Add (Var "x", Int 0), 1))) (Var "x");
     assert_equal ~msg:"simplify 20"
       (simplify (Add (Add (Var "x", Int 0), Mul (Var "y", Int 0)))) 
       (Var "x")
  );

  "derivative" >:: (fun _ ->
     assert_equal ~msg:"derivative 1" (derivative "x" (Int 10)) (Int 0);
     assert_equal ~msg:"derivative 2" (derivative "x" (Var "x")) (Int 1);
     assert_equal ~msg:"derivative 3" (derivative "x" (Var "y")) (Int 0);
     assert_equal ~msg:"derivative 4" (derivative "x" (Add (Var "x", Var "x"))) (Int 2);
     assert_equal ~msg:"derivative 5" (derivative "x" (Add (Add (Var "x", Var "x"), Var "x"))) 
                  (Int 3);
     assert_equal ~msg:"derivative 6" (derivative "x" (Mul (Var "x", Int 42))) (Int 42);
     assert_equal ~msg:"derivative 7" (derivative "x" (Mul (Var "x", Var "y"))) (Var "y");
     assert_equal ~msg:"derivative 8" (derivative "z" (Mul (Var "x", Var "y"))) (Int 0);
     assert_equal ~msg:"derivative 9" 
       (derivative "x" (Mul (Pow (Var "x", 2), Mul (Int 3, Var "x"))))
       (Add (Mul (Mul (Int 2, Var "x"), Mul (Int 3, Var "x")),
             Mul (Pow (Var "x", 2), Int 3)));
     assert_equal ~msg:"derivative 10" (derivative "x" (Pow (Var "y", 1))) (Int 0);
     assert_equal ~msg:"derivative 11" (derivative "x" (Pow (Var "x", 1))) (Int 1);
     assert_equal ~msg:"derivative 12" (derivative "x" (Pow (Var "x", 2))) 
                  (Mul ((Int 2), (Var "x")));
     assert_equal ~msg:"derivative 13" (derivative "x" (Pow (Mul (Int 3, Var "x"), 3)))
                  (Mul 
                    (Mul 
                      (Int 3, 
                        Pow (Mul (Int 3, Var "x"), 2)),
                      Int 3));
     assert_equal ~msg:"derivative 14" (derivative "x" (Add (Mul (Int 4, Pow (Var "x", 3)), 
                                        Mul (Int 6, Pow (Var "x", 2)))))
                  (Add (Mul (Int 4, Mul (Int 3, Pow (Var "x", 2))),
                        Mul (Int 6, Mul (Int 2, Var "x"))))

  );
]

let _ = run_test_tt_main all_tests

