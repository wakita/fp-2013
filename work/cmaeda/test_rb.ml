open RedBlack
open Printf

let tprint t =
  printf "{ %s }\n"
    (String.concat ", " (List.map string_of_int (to_list t)))

let _ =
  let s = insert 1 (insert 2 (insert 3 (insert 4 empty))) in
  tprint s
