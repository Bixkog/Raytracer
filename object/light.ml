type light = {
		center : Vect.point;
		color : Vect.vector;
		intensity : float;
		weakening : float
	}

let create center color intensity weakening = 
	{
		center = center;
		color = color;
		intensity = intensity;
		weakening = weakening
	}

let intens l hp = 
	let dist = Vect.dist l.center hp in
	l.weakening /. (dist *. dist) *. l.intensity