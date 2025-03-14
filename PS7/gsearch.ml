(* LATE: Late Days used: 1, Late Days from previous assignments: 5 *)
(* gsearch.ml: generic searching over a domain *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let search init = 
      let storage = S.create () in
      let history = [init] in
      S.push history storage;
      let visited = DS.empty in
      let rec search' storage visited =
        if S.is_empty storage
          then raise Not_found
        else
          let curr_h = S.pop storage in
          let curr_b = List.hd curr_h in 
          if DS.mem curr_b visited
            then search' storage visited
          else
            if D.is_solved curr_b
              then curr_h
            else
              let visited = DS.add curr_b visited in
              let children = D.next curr_b in
              List.iter (fun child -> S.push (child :: curr_h) storage) children; 
              search' storage visited
      in search' storage visited


    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

