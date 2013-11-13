exception Subscript

type 'a t = 'a MyStack.t

(* val vector : 'a list -> 'a t *)
let rec vector = function
  | [] -> MyStack.empty
  | x :: xs -> MyStack.cons x (vector xs)

let rec to_list v =
  if MyStack.isEmpty v then []
  else (MyStack.head v) :: (to_list (MyStack.tail v))

(* If you get tired of writing "MyStack.***", probably you want to "open" MyStack package and simply write "***" instead of "MyStack.***" *)

open MyStack

(* val get : 'a t -> int -> 'a *)
let rec get v i =
  if isEmpty v then raise Subscript
  else if i = 0 then head v else get (tail v) (i - 1)

(* val set : 'a t -> int -> 'a -> 'a t *)
let rec set v i y =
  if isEmpty v then raise Subscript
  else
    if i = 0 then cons y (tail v)
    else cons (head v) (set (tail v) (i - 1) y)

let rec append v1 v2 =
  if isEmpty v1 then v2
  else cons (head v1) (append (tail v1) v2)

let rec suffixes v =
  if isEmpty v then []
  else v :: (suffixes (tail v))
