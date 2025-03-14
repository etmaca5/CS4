(* klotski_interact.ml: Playing Klotski interactively. *)

open Lab7
open Klotski_boards
open Klotski

(* Read a move from a 3-character string. *)
let read_move s =
  let char_to_int c =
    let j = int_of_char c - int_of_char '0' in
      if j < 1 || j > 4
        then None
        else Some j
  in
  let char_to_dir d =
    match d with
      | 'l' -> Some Left
      | 'r' -> Some Right
      | 'u' -> Some Up
      | 'd' -> Some Down
      | _ -> None
  in
  let check_char c =
    if c < 'a' || c > 'z'
      then None
      else Some c
  in
    if String.length s <> 3
      then None
      else
        let c = check_char s.[0] in
        let d = char_to_dir s.[1] in
        let i = char_to_int s.[2] in
          match (c, d, i) with
            | (Some c', Some d', Some i') -> Some (c', d', i')
            | _ -> None

let rec interact b =
  try
    Printf.printf "\n%s\n" (show b);
    if is_solved b
      then Printf.printf "The board is solved!\n"
      else
        begin
          Printf.printf "Enter move: ";
          let line = read_line () in
            match read_move line with
              | None -> 
                  begin
                    Printf.printf "\nERROR: invalid move.\n";
                    interact b
                  end
              | Some move ->
                  begin
                    match make_move move b with
                      | None -> 
                          begin
                            Printf.printf "Invalid move; try again.\n";
                            interact b
                          end
                      | Some b' -> interact b'
                  end
        end
  with End_of_file ->
    begin
      Printf.printf "  \n";  (* an ugly hack to make exiting look clean *)
      exit 0
    end

let usage progname =
  begin
    Printf.printf "usage: %s n\n%!" progname;
    Printf.printf "    (n : an int between 0 and 7)\n%!"
  end

let _ =
  let args = Sys.argv in
    match args with
      | [| _; n |] -> 
        begin
          try 
            let n' = int_of_string n in
            let b = read boards.(n') in
              interact b
          with
            | Failure msg
            | Invalid_argument msg ->
              begin
                Printf.eprintf "ERROR: %s\n%!" msg;
                usage args.(0);
                exit 1
              end
        end
      | _ -> 
        begin
          usage args.(0);
          exit 1
        end

