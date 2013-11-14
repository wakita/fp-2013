(* Start a fresh OCaml interpreter and enter
 *   #use "test_vector.ml";;
 *)

open Vector;;
open Printf;;

let vprint v =
  printf "[| %s |]\n"
    (String.concat "; "
       (List.map string_of_int (to_list v)));;

let v = vector [0; 1; 2; 3] in
vprint v;
vprint (set v 2 ((get v 2) * 10));
vprint v;
vprint (append v v);
vprint (append v (set v 2 ((get v 2) * 10)))
