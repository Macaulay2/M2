-- Easy GB computations

homog = m -> isHomogeneous m
both = m -> true
algs = { (Inhomogeneous,both), (Homogeneous,homog), (Sugarless,both), (Homogeneous2,homog), (Faugere,both) }

algs = { (Inhomogeneous,both) }
algs = { (Homogeneous,homog) }
algs = { (Sugarless,both) }
algs = { (Homogeneous2,homog) }
algs = { (Faugere,both) } -- 6/19/06: these do not sort or auto reduce, so answer is harder to check.

algs = { (Inhomogeneous,both), (Homogeneous,homog), (Sugarless,both), (Homogeneous2,homog) }

time scan(algs, (a,c) -> (
	  alg = a;
	  crit = c;
	  gbi = m -> gb(m, Algorithm=>alg);
	  syzi = m -> syz(m, Algorithm=>alg);
     	  << "-- algorithm: " << alg << endl;
     	  input "gb-1.aux"
	  ))
