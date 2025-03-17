open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions go here. *)

    let search nrows ncols start_row start_col print =
      let board = B.make nrows ncols in      
      let rec solve curr_board =
        if B.is_solved curr_board
          then (curr_board, B.get_placed curr_board)
        else
          let moves = B.get_loc_counts_from_loc curr_board 
                      (B.get_last curr_board) in
          if moves = [] 
            then raise Solution_not_found
          else
            let min_count = List.fold_left 
              (fun min_so_far (_, count) -> min min_so_far count) 
              max_int moves in
            let best_moves = List.filter 
              (fun (_, count) -> count = min_count) 
              moves in
            let idx = Random.int (List.length best_moves) in
            let (next_loc, _) = List.nth best_moves idx in
            let next_board = B.place curr_board next_loc in
            solve next_board
      in
      try
        let initial_board = B.place board (start_row, start_col) in
        let (final_board, placed) = solve initial_board in
        if print then
          P.print_board final_board false;
        Some placed
      with
      | Invalid_argument _ -> None
      | Solution_not_found -> None
  end

