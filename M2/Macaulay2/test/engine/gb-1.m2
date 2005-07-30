-- Easy GB computations

homog = m -> isHomogeneous m
both = m -> true
algs = { (Inhomogeneous,both), (Homogeneous,homog), (Sugarless,both), (Homogeneous2,homog), (Faugere,both) }

scan(algs, (a,c) -> (
	  alg = a;
	  crit = c;
	  gbi = m -> gb(m, Algorithm=>alg);
	  syzi = m -> syz(m, Algorithm=>alg);
     	  << "-- algorithm: " << alg << endl;
     	  input "gb-1.aux"
	  ))
