-- self-explanatory
polynomialsWithSameSupportsAndRationalCoefficients = listOfPolynomials -> (
    p := first listOfPolynomials;
    S := QQ[gens ring p];
    apply(listOfPolynomials, p -> (
    	    (mons, coeffs) := coefficients p;    
    	    monsQQ := matrix apply(
		first \ exponents \ flatten entries mons,
		e -> {product apply(e, gens S, (ei, gi) -> gi^ei)}
		);
    	    coeffsQQ := matrix{for i from 1 to numcols mons list 1_QQ};
    	    (coeffsQQ*monsQQ)_(0,0)
	    )
    )
)
-- needed something less error-prone than random QQ (used for random specializations, eg. for mixed volumes
randomRationalNumber = () -> random(-50,50)/random(1,10)

