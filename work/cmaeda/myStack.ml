exception Empty
exception Subscript

(*
type 'a t = 'a list

let empty = []
let isEmpty = function [] -> true | _ -> false
let cons x s = x :: s
let head = function (hd :: _) -> hd | _ -> raise Empty
let tail = function (_ :: tl) -> tl | _ -> raise Empty
 *)

type 'a t = Nil | Cons of 'a * 'a t

let empty = Nil
let isEmpty = function
  | Nil -> true
  | _ -> false

let cons x s = Cons (x, s)

let head = function
  | Cons (hd, _) -> hd
  | _ -> raise Empty

let tail = function
  | Cons (_, tl) -> tl
  | _ -> raise Empty

let rec update xs i y = match xs with
  | Nil -> raise Subscript
  | Cons(x, xs) ->
      if i = 0 then Cons(y, xs) else update xs (i - 1) y
