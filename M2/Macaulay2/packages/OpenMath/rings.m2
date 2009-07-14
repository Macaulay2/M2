toOpenMath Ring := R -> (
	if 		R === ZZ then OMS("setname1", "Z")
	else if R === QQ then OMS("setname1", "Q")
	else error concatenation("Cannot handle ring ", toString R)
)

toOpenMath InexactFieldFamily := R -> (
	if 		R === CC then OMS("setname1", "C")
	else if R === RR then OMS("setname1", "R")
	else error concatenation("Cannot handle ring ", toString R)
)

toOpenMath PolynomialRing := R -> (
	vars := apply(gens R, i->OMV(toString(i)));
	
	OMA("polyd1", "poly_ring_d_named", {
		prepend(toOpenMath(coefficientRing(R)), vars)
	})
)

toOpenMath RingElement := p -> (
	ring := toOpenMath(class(p));
	terms := OMA("polyd1", "SDMP", 
		apply(listForm(p), i->OMA("polyd1", "term", { i#0, i#1 }))
	);

	OMA("polyd1", "DMP", {ring, terms})
)


R = ZZ[x,y]
hi = x^3+ 17*x^2*y - 1
print toOpenMath R
