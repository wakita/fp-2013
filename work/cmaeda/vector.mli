type 'a t

val vector : 'a list -> 'a t
val to_list : 'a t -> 'a list

val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
val append : 'a t -> 'a t -> 'a t
val suffixes : 'a t -> 'a t list
