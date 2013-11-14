type 'a t
val empty : 'a t
val to_list : 'a t -> 'a list
val member : 'a -> 'a t -> bool
val insert : 'a -> 'a t -> 'a t
