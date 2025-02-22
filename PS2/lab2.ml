(* Exercises A*)
(* A.0 *)
(* The function divides by n by 2 each step so it will take log_2(n) steps to complete. Thus the time complexity is O(log(n))
The function uses tail recusion so each recursive call doesn't add any stack space. Thus the space complexity is O(1) since 
we don't need any additional stack frames since there are no pending operations.
*)

(* A.1 *)
(* The space complexity of the function is O(n) because the function will first evaluate fib(n - 1) before evaluating
fib(n - 2), so fib(n - 1) has at most n pending operations and then the same for fib(n - 2). Thus the space complexity is
O(n). This is different because from the time complexity because the function evaluates fib(n - 1) recursively until we reach
the base case so we decrease n by 1 each time until n < 2 (so size n stack), then we evaluate all the fib(n - 2) and we can use the stack
space from the previous calls since they have already been evaluated. So there are at most n pending
operations at once versus the time complexity which includes every single operation.
*)

(* TODO: check if this is correct *)
(* A.2 *) 
(* 1. When sine 12.15 is evaluated we first get 4.5 then 1.5 then 0.5 then 0.16 then 0.05, so we call p 5 times 
starting with the call to 0.05 
2. We decrease input size angle by a factor of 3 each time (until we reach 0.1) so the time complexity is O(log_3(angle)) or O(log(angle)).
The same goes for the space complexity because the recursive calls are nested so we add to the stack for each call. Thus the 
space complexity is O(log_3(angle)) or O(log(angle)).
*)

(* A.3a *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
    | 0 -> 1
    | _ when is_even n -> square (fast_expt b (n / 2))
    | _ -> b * fast_expt b (n - 1)
 

 (* A.3b *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter n b a = 
    match n with
    | 0 -> a
    | _ when is_even n -> iter (n / 2) (square b) a
    | _ -> iter (n - 1) b (a * b)
  in iter n b 1

(* A.4 *)
let rec fast_mult a b =
  let is_even m = m mod 2 = 0 in
  let double m = 2 * m in
  let halve m = m / 2 in
    match b with
    | 0 -> 0
    | _ when is_even b -> fast_mult (double a) (halve b)
    | _ -> a + fast_mult a (b - 1)

(* A.5 *)
let ifast_mult b n =
  let is_even m = m mod 2 = 0 in
  let double m = 2 * m in
  let half m = m / 2 in
  let rec iter n b a = 
    match b with
    | 0 -> a
    | _ when is_even b -> iter (double n) (half b) a 
    | _ -> iter n (b - 1) (n + a)
  in iter n b 0

(* A.6 *)
(* The most amount of recursive calls we will need to evaluate foo f (n / 2) is log_2(n) because we divide n in half each time.
Then this occurs first for the first foo then for the second foo so the total so we have a binary tree of depth log_2(n) and 
so the amount of recursive calls is 2^log_2(n) = n. Thus the runtime complexity is O(n). 
For the space complexity the first foo operation will evaluate first so we only need O(log(n)) stack space to store 
(since the depth is log_2(n)) and its operations all complete before the second foo is evaluated. 
So, we will at most have log(n) pending operations at once. 
Thus the space complexity is O(log(n)).
(Note: this is because f can compute in constant space and time.)
*)

(* A.7 *)
(* 1.This is a linear recursive function because we have to recursively call last_two each time to define the new tuple, so we 
build recursive calls in this way until we reach the base case where we start to build the tuple.

2. The time complexity is O(n) because we have n recursive calls that take constant time and since the function is linear
recursive the space complexity is also O(n).
*)

(* Exercises B *)

(* B.1a *)
(*
(fun x y -> x * (2 + y)) 20 (2 * 4)
*)

(* B.1b *)
(*
(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
*)

(* B.1c *)
(*
(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1 
*)

(* B.1d *)
(*
(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1 
*)

(* B.2 *)
(*

Desugar first step:
(fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
    
Desugar the next let-in to:
(fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
    
Desugar the final let-in to:
(fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)

evaluate (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
  (2 * 10) -> 20
  (3 + 4) -> 7
  evaluate fun x y -> (fun y -> (fun z -> x * y * z) 22) 14
    14 -> 14
    evaluate fun y -> (fun z -> x * y * z) 22
      22 -> 22
      evaluate fun z -> x * y * z
      apply fun z -> x * y * z to 22
        substitute 22 in for z in x * y * z -> x * y * 22
        evaluate x * y * 22 -> x * y * 22
    apply fun y -> x * y * 22 to 14
      substitute 14 in for y in x * y * 22 -> x * 14 * 22
  apply fun x y -> x * 14 * 22 to 20 7
    substitute 20 for x and 7 for y in x * 14 * 22 -> 20 * 14 * 22 - y doesn't exist so we don't substitute it in
  evaluate 20 * 14 * 22
    20 * 14 -> 280
    22 -> 22
    280 * 22 -> 6160
  result is 6160

*)

(* B.3 *)
(* 
We can desugar the function to:
(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

This doesn't work because x and y are not defined when they are to be applied / substituted in to the function (not nested),
we can see this in the desugaring.
To fix this we can change the 'and' to 'let __ in' that way the values are defined seperately and can be applied to one another in a nested way.
The correct code is:

let x = 10 in
let y = x * 2 in
let z = y + 3
in x + y + z

The desugaring would then be:

(fun x -> (fun y -> (fun z -> x + y + z) (y + 3)) (x * 2)) 10

This would allow the y and the z to be evaluated correctly in order.
*)


(* Exercises C *)

open Num
let ni = num_of_int     (* convert int -> num *)

(* C.1 *)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (term a +/ result)
  in
    iter a (ni 0)

(* C.2 *)
let rec product_rec term a next b =
  if a >/ b
     then (ni 1)
     else term a */ (product_rec term (next a) next b)

let product_iter term a next b =
  let rec iter a result =
    if a >/ b
        then result
        else iter (next a) (term a */ result)
  in
    iter a (ni 1)

let factorial_rec n = product_rec (fun x -> x) (ni 1) (fun n -> n +/ (ni 1)) (n)

let factorial_iter n = product_iter (fun x -> x) (ni 1) (fun n -> n +/ (ni 1)) (n)

let pi_product n = product_iter (fun x -> (x */ (x +/ (ni 2))) // ((x +/ (ni 1)) */ (x +/ (ni 1)))) (ni 2) (fun n -> n +/ (ni 2)) (n)

let pi_approx = float_of_num ((pi_product (ni 1000)) */ (ni 4))

(* C.3 *)
let rec accumulate_rec combiner null_value term a next b =
  if a >/ b
     then null_value
     else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (combiner (term a) result)
  in
    iter a null_value

let sum term a next b = accumulate_iter ( +/ ) (ni 0) term a next b

let product term a next b = accumulate_iter ( */ ) (ni 1) term a next b

(* C.4 *)
let compose f g = fun x -> f (g x)

(* C.5 *)
let rec repeated f n =
  if n = 0 then
    fun x -> x
  else compose f (repeated f (n - 1))

(* C.6 *)
let smooth dx f x = (f (x +. dx) +. f (x -. dx) +. f x) /. 3.

let nsmoothed dx f n x = (repeated (smooth dx) n f) x

(* Exercises D *)
(* D.1 *)
let is_prime p = 
  if p < 2 then false
  else 
    let check_prime b = p mod b = 0 in
    let rec iter num =
      match num with
      | _ when num < 2 -> true
      | _ when check_prime num -> false
      | _ -> iter (num - 1)
    in iter (int_of_float (sqrt (float_of_int p)))

(* D.2 *)
let smallest_prime_factor x =
  if x < 2 || is_prime x then failwith "invalid_arg"
  else
    let is_divisible b = x mod b = 0 in
    let rec iter num =
      match num with
      | _ when is_prime num && is_divisible num -> num
      | _ -> iter (num + 1)
    in iter 2