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

module Make (Ord : Ordered) : S with type elem = Ord.t
