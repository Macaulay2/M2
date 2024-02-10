debugPrintMap = method()
debugPrintMap(RingMap) := f -> (
    a := gens source f;
    for i from 0 to (length a)-1 do(
	elt := f(a_i);
	print("maps "|toString(a_i)|" to "|toString(elt));
	);    
    );

debugPrintAllMaps = method()
debugPrintAllMaps(Subring) := subR -> (
    pres := makePresRing(subR);
    print("--------------------------------");
    print("-- PresRing map info dump:");
    print("--------------------------------");
    print("-- sagbiInclusion:");
    debugPrintMap (pres#"sagbiInclusion");
    print("-- projectionAmbient:");
    debugPrintMap (pres#"projectionAmbient");
    print("-- inclusionAmbient:");
    debugPrintMap (pres#"inclusionAmbient");
    print("-- substitution:");
    debugPrintMap (pres#"substitution");
    print("-- fullSubstitution:");
    debugPrintMap (pres#"fullSubstitution");
    print("--------------------------------");
    print("-- End PresRing map info dump.");
    print("--------------------------------");
    );

-- Shortens each row of (net p) to a maximum of strWidth and puts "..." after 
-- the end of the middle row. Intended for use with big polynomials. 
shortenNet = (p, strWidth) -> (
    S := unstack net p;
    middle := (floor((length S)-1)/2);
    
    maxlen := max apply(S, x -> length x);
    
    if maxlen <= strWidth then(
	return stack S;
	);
    
    shortened := for i from 0 to (length S)-1 list(
	maxInd := strWidth;
	-- only put "..." after the middle row
	if i == middle then (
	    (S#i)_(0,strWidth - 3)|"..."
	    ) else(
	    (S#i)_(0,strWidth - 3)
	    )
	);
    if #shortened == 2 then(
	shortened = {""}|shortened; 
	);    
    stack shortened
    );

-- Nicely prints the generators of subR. Can handle large polynomials.
debugPrintGens = method(Options => {StrWidth => 125})
debugPrintGens(Subring) := o -> subR -> (
    colon := stack({"  ",": ","  "});
    G := genVars subR;
    for i from 0 to (numcols G)-1 do(
	var := G_(0,i);
	p := (subR#"presentation"#"fullSubstitution")(var);
	pnet := ({" "}|(unstack net var));
	fmt := ((stack pnet)|colon);
	final := fmt|(shortenNet(p, o.StrWidth)); 
	print(final);
	);
    );

-- Given a one row matrix of polynomials, nicely prints the entries
debugPrintMat = method(Options => {StrWidth => 125})
debugPrintMat(Matrix) := o -> M -> (
    colon := stack({"  ",": ","  "});
    for i from 0 to (numcols M)-1 do(
	p := M_(0,i);
	pnet := ({" "}|{toString(i)}|{" "});
	fmt := ((stack pnet)|colon);
	final := fmt|(shortenNet(p, o.StrWidth)); 
	print(final);
	);
    );
