(* klotski_solve.ml: Klotski board solver *)

open Lab7
open Klotski_boards
open Klotski
open Gsearch

module DFS = Search(Stack)
module BFS = Search(Queue)
module K_DFS = DFS(Klotski)
module K_BFS = BFS(Klotski)

let search_and_print search show b =
  let h = search b in
    begin
      print_newline ();
      print_string (show h);
      Printf.printf "Solution in %d moves.\n\n" (List.length h - 1)
    end

let usage progname =
  begin
    Printf.printf "usage: %s [-dfs] n\n%!" progname;
    Printf.printf "    (n : an int between 0 and 7)\n%!"
  end

let _ =
  let args = Sys.argv in
    match args with
      | [| _; "-dfs"; n |] ->
          let n' = int_of_string n in
          let b = read boards.(n') in
            search_and_print K_DFS.search K_DFS.show_history b
      | [| _; n |] -> 
        begin
          try 
            let n' = int_of_string n in
            let b = read boards.(n') in
              search_and_print K_BFS.search K_BFS.show_history b
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
