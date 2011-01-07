createdGFs = new MutableHashTable;
getGF = (p,n) -> (
	q := p^n;
	r := null;
	if (createdGFs#?q) then (
		--print ("Fetching GF("|toString q|")");
		createdGFs#q
	) else (
		--print ("Creating GF("|toString q|")");
		if n === 1 then r = GF(p) else r = GF(p,n);
		createdGFs#q = r;
		r
	)
);
setGF = (q, F) -> (
	createdGFs#q = F;
);

--- From OpenMath ---
-- Symbols to try and remember how elements of a particular field are represented.
protect PowerOfPrimitiveElement
protect FieldByPolyVector
protect OpenMathPrefEltRepr

--- To OpenMath ---
protect PowerOfPrimElt
toOpenMathFFelt = idCheck(t -> (
	F := class t;
	if (F#?OpenMathPrefEltRepr and F#OpenMathPrefEltRepr == PowerOfPrimitiveElement) then (
		--print "This field prefers PowerOfPrimitiveElement";
		toOpenMathFFElt_PowerOfPrimElt t
	) else if (F#?OpenMathPrefEltRepr and F#OpenMathPrefEltRepr == FieldByPolyVector) then (
		--print "This field prefers FieldByPolyVector";
		toOpenMathFFElt_Vector t
	) else if (class class t === GaloisField) and ((class t).order < 10000) then
		toOpenMathFFElt_PowerOfPrimElt t
	else
		toOpenMathFFElt_Vector t
))
