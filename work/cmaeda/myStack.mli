type 'a t (* The type "MyStack.t" represents our implementation of Stack *)

val empty : 'a t
val isEmpty : 'a t -> bool
val cons : 'a -> 'a t -> 'a t
val head : 'a t -> 'a
val tail : 'a t -> 'a t
