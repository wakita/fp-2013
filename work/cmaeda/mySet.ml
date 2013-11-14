module type Ordered =
sig
  type t
  val lt : t -> t -> bool
end

module type S =
sig
  type elem
  type t
  val empty : t
  val to_list : t -> elem list
  val member : elem -> t -> bool
  val insert : elem -> t -> t
end

module Make (Ord : Ordered) =
struct
  type elem = Ord.t
  type t = E | T of t * elem * t

  let empty = E

  let rec to_list = function
    | E -> []
    | T(s1, e, s2) -> (to_list s1) @ [e] @ (to_list s2)

  let rec member x = function
    | E -> false
    | T(s1, e, s2) ->
        if Ord.lt x e then member x s1
        else if Ord.lt e x then member x s2
        else true

  let rec insert x = function
    | E -> T(E, x, E)
    | T(s1, e, s2) as s ->
        if Ord.lt x e then T(insert x s1, e, s2)
        else if Ord.lt e x then T(s1, e, insert x s2)
        else s
end
