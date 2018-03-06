type e = float
type vector
type point


val x_v : vector
val y_v : vector
val z_v : vector


val len : vector -> e
val neg : vector -> vector
val dot : vector -> vector -> e
val add : vector -> vector -> vector
val add3 : vector -> vector -> vector -> vector
val decr : vector -> vector -> vector
val mult : vector -> vector -> vector
val dist : point -> point -> e
val direction : point -> point -> vector 
val scale : vector -> e -> vector
val move : point -> vector -> point
val norm : vector -> vector
val cross : vector -> vector -> vector
val set_max : e -> vector -> vector

val e_dists : point -> point -> (e * e * e)
val map : (e -> e) -> vector -> vector 

val x : vector -> e
val y : vector -> e
val z : vector -> e

val px : point -> e
val py : point -> e
val pz : point -> e

val unpack_v : vector -> e list
val unpack_p : point -> e list

val vector_c : e list -> vector
val point_c : e list -> point

val print_v : vector -> unit
val print_p : point -> unit

val up_v : vector
val zero_v : vector