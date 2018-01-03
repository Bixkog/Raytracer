type light = {
		center : Vect.point;
		color : Vect.vector;
		intensity : float;
	}

let create center color intensity = 
	{
		center = center;
		color = color;
		intensity = intensity;
	}