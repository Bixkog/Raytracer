type origin = Vect.point
type direction = Vect.vector

type t = {center : origin; target : direction}

let create_ray p d = {center = p; target = Vect.norm d}