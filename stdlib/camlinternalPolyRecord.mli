type hash = int
type +'a t
type fields = (hash * Obj.t) array

val get : 'a t -> hash -> 'o

val set : 'a t -> hash -> 'o -> unit

val copy_with : fields -> 'a t option -> 'a t

val create : fields -> 'a t

