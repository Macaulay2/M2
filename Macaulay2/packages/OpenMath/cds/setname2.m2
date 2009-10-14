-- done: Zm, GFp, GFpn, QuotientField
-- not done: A, Boolean, H

--- From OpenMath ---
OMSEvaluators#"setname2" = new MutableHashTable;
OMSEvaluators#"setname2"#"Zm" = (args, attrs) -> (
	ZZ/(fromOpenMath args#0)
);
OMSEvaluators#"setname2"#"GFp" = (args, attrs) -> (
	getGF(fromOpenMath args#0, 1)
);
OMSEvaluators#"setname2"#"GFpn" = (args, attrs) -> (
	a := apply(args, fromOpenMath);
	getGF(a#0, a#1)
);
OMSEvaluators#"setname2"#"QuotientField" = (args, attrs) -> (
	frac fromOpenMath args#0
);


--- To OpenMath ---
toOpenMath QuotientRing := autoCreateIDCheck(R -> (
	if isField(R) and (ambient(R) === ZZ) then
		OMA("setname2", "GFp", { OMI(char(R)) })
	else
		(theOMerror = concatenate("Cannot convert quotient ring ", toString(R), " to OpenMath"); error("whoops"))
))
toOpenMath GaloisField := autoCreateIDCheck(F -> (
	setGF(F.order, F);
	OMA("setname2", "GFpn", { OMI(F.char), OMI(F.degree) })
))