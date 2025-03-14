(* LATE: Late Days used: 1, Late Days from previous assignments: 5 *)
(* klotski.ml: core functionality of the Klotski game. *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with Not_found ->  (* new piece; create a new piece set *)
                let cs = LocSet.singleton (r, c) in
                let p' = CharMap.add ch cs p in
                  iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b =
  let solved_loc = LocSet.of_list [(3, 1); (3, 2); (4, 1); (4, 2)] in
    CharMap.exists (fun _ piece -> LocSet.equal piece solved_loc) b.pieces

let compare b1 b2 = 
  let unoccupied_equal = LocSet.compare b1.unoccupied b2.unoccupied in
  if unoccupied_equal <> 0 
    then unoccupied_equal
  else
    let pieces1 = LocSetSet.of_list (List.map snd (CharMap.bindings b1.pieces)) in
    let pieces2 = LocSetSet.of_list (List.map snd (CharMap.bindings b2.pieces)) in
    LocSetSet.compare pieces1 pieces2

let remove c ({ pieces = p; unoccupied = u } as b) = 
  if CharMap.mem c p 
    then {pieces = CharMap.remove c p; unoccupied = LocSet.union u (CharMap.find c p)}
  else 
    b

let add (c, p) { pieces = ps; unoccupied = u } = 
  if LocSet.subset p u && not (CharMap.mem c ps)
    then Some { pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p }
  else 
    None

let make_move (c, d, i) b =
  if i < 1 || not (CharMap.mem c b.pieces) 
    then None
  else
    let piece = CharMap.find c b.pieces in
    let shift (r, c) = 
      match d with
      | Up -> (r - 1, c)
      | Down -> (r + 1, c)
      | Left -> (r, c - 1)
      | Right -> (r, c + 1)
    in
    let rec check_path piece dist =
      if dist = 0 
        then Some piece
      else
        let new_piece = LocSet.map shift piece in
        let new_locs = LocSet.diff new_piece piece in
        if LocSet.subset new_locs b.unoccupied then
          check_path new_piece (dist - 1)
        else
          None
    in
    match check_path piece i with
    | None -> None
    | Some new_piece ->
        let board_wo_piece = remove c b in
        add (c, new_piece) board_wo_piece


let next b =
  let pieces = CharMap.bindings b.pieces in  
  let find_moves (c, _) =
    List.concat_map (fun d ->
      List.fold_left (fun out i ->
        match make_move (c, d, i) b with
        | None -> out
        | Some new_board -> new_board :: out
      ) [] [1; 2; 3; 4]
    ) [Up; Down; Left; Right]
  in
  List.concat_map find_moves pieces

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let _test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

