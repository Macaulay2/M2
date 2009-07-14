--- To OpenMath ---
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


--- From OpenMath ---
