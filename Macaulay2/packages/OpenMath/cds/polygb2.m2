-- done:  in, in_radical, 
-- not done: extended_in, minimal_groebner_element

-- rooz: See my comments at polygb1. We should use DMPL more often.
-- rooz: My implementation here:
-- rooz: * in(pol, DMPL)
-- rooz: * in_radical(pol, DMPL)

inOrInRad := (args, rad) -> (
	if #args >= 4 then (
		--The proper variant
		theOMerror = "I strongly dislike polygb2.in and polygb2.in_radical as defined in the CD. 
			Please give me a polynomial as 1st argument and a DMPL as 2nd argument, that'd much better.";
		error("Whoops");
	);

	if #args =!= 2 then	(
		theOMerror = "Expecting number of arguments to polygb2.in and in_radical to be 2 or 4."; 
		error("whoops");
	);

	--The second one will be a DMP, but we're only interested in the SDMP.
	pols := fromOpenMath(args#1);
	R := class(pols#0);
	if not isOMAOf(args#0, "polyd1", "DMP") then
		(theOMerror = "Expecting 1st argument of polygb2.in and in_radical  to be an OMA of polyd1.DMP"; error("whoops"));

	pol := evalSDMP(((args#0).children)#2, R);
	r := pol % (if rad then ideal(pols) else radical(ideal(pols)));
	0 == r
)

OMSEvaluators#"polygb2" = new MutableHashTable;
OMSEvaluators#"polygb2"#"in" = (args, attrs) -> inOrInRad(args, false);
OMSEvaluators#"polygb2"#"in_radical" = (args, attrs) -> inOrInRad(args, true);

