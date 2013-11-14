type c = R | B (* colors *)
type 'a t = E | T of c * 'a t * 'a * 'a t

let empty = E

let rec to_list = function
  | E -> []
  | T(_, t1, y, t2) -> (to_list t1) @ [y] @ (to_list t2)

let rec member x = function
  | E -> false
  | T(_, t1, y, t2) ->
      if x < y then member x t1
      else if y < x then member x t2
      else true

let balance = function
  | (B, T(R, T(R, a, x, b), y, c), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
  | (B, T(R, a, x, T(R, b, y, c)), z, d) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
  | (B, a, x, T(R, T(R, b, y, c), z, d)) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
  | (B, a, x, T(R, b, y, T(R, c, z, d))) -> T(R, T(B, a, x, b), y, T(B, c, z, d))
  | (c, t1, x, t2) -> T(c, t1, x, t2)

let rec insert x s =
  let rec ins = function
    | E -> T(R, E, x, E)
    | T(c, t1, y, t2) ->
        if x < y then balance(c, ins t1, y, t2)
        else if y < x then balance(c, t1, y, ins t2)
        else s in
  match ins s with
    | T(_, t1, y, t2) -> T(B, t1, y, t2)
    | _ -> failwith "It is guaranteed that this case should not happen"
