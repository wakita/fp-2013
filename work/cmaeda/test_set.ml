(* Start a fresh OCaml interpreter and enter
 *   #use "test_set.ml"
 * to run this script
 *)

open MySet
open Printf

module type INTEGER = sig
  type t = int
  val lt : t -> t -> bool
end

module Integer : INTEGER = struct
  type t = int
  let lt = (<)
end

module ISet = MySet.Make(Integer)

let rec sprint s =
  printf "{ %s }\n"
    (String.concat ", "
       (List.map string_of_int (ISet.to_list s)));;

open ISet

let _ =
  let s = empty in
  sprint s;
  sprint (insert 2 (insert 0 (insert 1 s)))
