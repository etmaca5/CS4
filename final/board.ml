open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc
    
    let knight_moves = [
      (1, -2); (1, 2); (2, -1); (2, 1);
      (-2, -1); (-2, 1); (-1, -2); (-1, 2)
    ]

    let init_reachable nrows ncols =
      let reachable = S.make nrows ncols in
      let reachable_loc_count (row, col) =
        List.fold_left (fun count (x, y) ->
          if ok_loc nrows ncols (row + x, col + y) 
            then count + 1 
          else count
        ) 0 knight_moves
      in
      let rec iter row col grid =
        if row >= nrows 
          then grid
        else if col >= ncols 
          then iter (row + 1) 0 grid
        else
          let count = reachable_loc_count (row, col) in
          let new_grid = S.set grid (row, col) count in
          iter row (col + 1) new_grid
      in
      iter 0 0 reachable

    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    let get_loc_counts_from_loc board loc = 
      if not (check_bounds board loc)
        then bad_loc "get_loc_counts_from_loc" "location off board" loc
      else
        let (row, col) = loc in
        List.fold_left (fun counts (x, y) ->
          let new_loc = (row + x, col + y) in
          match S.get board.reachable new_loc with
          | Some count -> (new_loc, count) :: counts
          | None -> counts
        ) [] knight_moves
    
    let check_new_valid_move board loc =
      let last_loc = get_last board in
      let (last_row, last_col) = last_loc in
      let (row, col) = loc in
      List.exists (fun (x, y) ->
        last_row + x = row && last_col + y = col
      ) knight_moves

    let place board loc = 
      if not (check_bounds board loc)
        then bad_loc "place" "location off board" loc
      else if S.has_loc board.indices loc
        then bad_loc "place" "location is occupied" loc
      else if board.placed <> [] && not (check_new_valid_move board loc)
        then bad_loc "place" "loc is not a knight's move" loc
      else
        let new_reachable =
          List.fold_left (fun counts (x, y) ->
            let (row, col) = loc in
            let new_row = row + x
            and new_col = col + y in
            match S.get counts (new_row, new_col) with
              | None -> counts
              | Some count -> S.set counts (new_row, new_col) (count - 1)
          ) (S.remove board.reachable loc) knight_moves
        in
        { board with
          placed = loc :: board.placed;
          last_index = board.last_index + 1;
          indices = S.set board.indices loc (board.last_index + 1);
          reachable = new_reachable
        }

    let undo board = 
      match board.placed with
      | [] -> board
      | last_loc :: rest ->
          let (row, col) = last_loc in
          let new_indices = S.remove board.indices last_loc in
          let reachable_count = List.fold_left (fun count (x, y) ->
            let new_loc = (row + x, col + y) in
            if check_bounds board new_loc && 
               not (S.has_loc new_indices new_loc) 
              then count + 1
            else
              count
          ) 0 knight_moves in
          let reach_w_loc = S.set board.reachable last_loc reachable_count in
          let new_reachable = List.fold_left (fun counts (x, y) ->
            let new_loc = (row + x, col + y) in
            if check_bounds board new_loc &&
               not (S.has_loc new_indices new_loc)
              then match S.get counts new_loc with
              | Some count -> S.set counts new_loc (count + 1)
              | None -> counts
            else
              counts
          ) reach_w_loc knight_moves in
          { board with
            placed = rest;
            last_index = board.last_index - 1;
            indices = new_indices;
            reachable = new_reachable
          }

    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end
