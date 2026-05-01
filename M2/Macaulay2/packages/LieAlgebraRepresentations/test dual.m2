	    sl6 = simpleLieAlgebra("A",5)
	    Std = standardRepresentation(sl6);
	    rho1 = exteriorPower(2,Std);
	    rho2 = exteriorPower(4,Std);
	    rho3 = dual rho1;
	    isomorphismOfRepresentations(rho2, rho3)
