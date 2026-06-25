--- status: Draft
--- author(s): Gregory G. Smith
--- notes:

doc ///
    Key
        isSkewAffineRing
        (isSkewAffineRing, QuotientRing)
        (isSkewAffineRing, PolynomialRing)
        (isSkewAffineRing, Ring)
    Headline
        whether something is an affine ring containing skew-commuting variables
    Usage
        isSkewAffineRing R
    Inputs 
        R:Ring
    Outputs
	    :Boolean
	        true if @TT "R"@ is a quotient of a polynomial ring over a field
	        that contains at least one skew-commutative variable and false
	        otherwise.
    Description
        Text
            Typical examples of skew commutative rings over a field.
        Example
            kk = ZZ/101
            R = kk[a..d, SkewCommutative => true]
            isSkewAffineRing R
            isSkewAffineRing (R / ideal {a*b})
        Text
            This function enforces that only skew-commutative rings are skew affine.
        Example
            kk = ZZ/101
            isSkewAffineRing(kk[a..c])
            isSkewAffineRing kk
    SeeAlso
        isAffineRing
        coefficientRing
        isField
        ambient
///

