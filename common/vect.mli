type e = float
type vector
type point


val x_v : vector
val y_v : vector
val z_v : vector


val len : vector -> e
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

val unpack_v : vector -> e list
val unpack_p : point -> e list

val vector_c : e list -> vector
val point_c : e list -> point

val up_v : vector
val zero_v : vector