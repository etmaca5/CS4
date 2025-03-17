module Loc =
  struct
    type t = int * int

    let compare = Stdlib.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = 
      if nrows <= 0 || ncols <= 0 
        then invalid_arg 
          (Printf.sprintf 
            "make: invalid arguments: nrows = %d, ncols = %d" nrows ncols)
      else
        Array.make_matrix nrows ncols (-1)

    let get data (row, col) = 
      if row < 0 || col < 0 || row >= Array.length data ||
         (Array.length data > 0 && col >= Array.length data.(0))
        then None
      else if data.(row).(col) = -1
        then None
      else 
        Some data.(row).(col)


    let set data (row, col) i = 
      if i < 0 then
        invalid_arg "set: negative argument"
      else if row < 0 || row >= Array.length data || col < 0 ||
              (Array.length data > 0 && col >= Array.length data.(0))
        then invalid_arg 
          (Printf.sprintf "set: invalid location: (%d, %d)" row col)
      else begin
        data.(row).(col) <- i;
        data
      end

    let has_loc data (row, col) =
      match get data (row, col) with
      | None -> false
      | Some _ -> true

    let remove data (row, col) =
      if has_loc data (row, col)
        then (
          data.(row).(col) <- -1;
          data)
      else 
        data
  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)
module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols = 
      if nrows <= 0 || ncols <= 0 
        then invalid_arg (Printf.sprintf 
              "make: invalid arguments: nrows = %d, ncols = %d" nrows ncols)
      else
        { contents = LocMap.empty; nrows = nrows; ncols = ncols }
    
    let get data (row, col) = 
      if row < 0 || col < 0 || row >= data.nrows || col >= data.ncols
        then None
      else
        LocMap.find_opt (row, col) data.contents
    let set data (row, col) i = 
      if i < 0 
        then invalid_arg "set: negative argument"
      else if row < 0 || row >= data.nrows || col < 0 || col >= data.ncols 
        then invalid_arg 
          (Printf.sprintf "set: invalid location: (%d, %d)" row col)
      else
        { data with contents = LocMap.add (row, col) i data.contents }

    let has_loc data (row, col) = 
      match get data (row, col) with
      | None -> false
      | Some _ -> true

    let remove data (row, col) =
      if has_loc data (row, col)
        then { data with contents = LocMap.remove (row, col) data.contents }
      else 
        data
  end

