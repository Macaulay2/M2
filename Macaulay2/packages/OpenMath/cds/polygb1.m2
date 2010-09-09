-- "done":  groebner, groebner_basis, reduce
-- not done: completely_reduced, , groebnered, 

----- From the Content Dictionary on "groebner":
-- The groebner basis (reduced, minimal) of a set of polynomials, with
-- respect to a given ordering. First argument is  a list of
-- variables, the second is an ordering, the
-- third is a list of polynomials. A program that can compute
-- the basis is required to return a "groebner_basis" object.
--
-- rooz: I dislike that. 
-- rooz: As a better alternative, I'll just assume I have one argument: a list of DMPs or a DMPL
-- rooz: Similarly, the groebner_basis 's I return have only one argument: a DMPL
-- rooz: Similarly, for "reduce" I'm looking for a 1st argument that's a polynomial, and a 2nd argument


--- From OpenMath ---
OMSEvaluators#"polygb1" = new MutableHashTable;
OMSEvaluators#"polygb1"#"groebner" = (args, attrs) -> (
	if #args === 3 then (
		--The proper variant
		theOMerror = "I strongly dislike polygb1.groebner as defined in the CD. 
			Please give me a DMPL as argument, that's much better.";
		error("whoops");
	);
	
	if #args =!= 1 then
		(theOMerror = "Expecting number of arguments to polygb1.groebner to be 1 or 3."; error("whoops"));

	--The proper variant.
	pols := fromOpenMath(args#0);
	gb(ideal(pols))
)

OMSEvaluators#"polygb1"#"reduce" = (args, attrs) -> (
	if #args === 4 then
		--The proper variant
		return OME("I strongly dislike polygb1.reduce as defined in the CD. 
			Please give me a polynomial as 1st argument and a DMPL as 2nd argument, that'd much better.");
	
	if #args =!= 2 then
		(theOMerror = "Expecting number of arguments to polygb1.reduce to be 2 or 4."; error("whoops"));

	--The second one will be a DMP, but we're only interested in the SDMP.
	--
	pols := fromOpenMath(args#1);
	R := class(pols#0);
	if not isOMAOf(args#0, "polyd1", "DMP") then
		(theOMerror = "Expecting 1st argument of polygb1.reduce to be an OMA of polyd1.DMP"; error("whoops"));
		
	pol := evalSDMP(((args#0).children)#2, R);
	pol % ideal(pols)
)



--- To OpenMath ---
toOpenMath GroebnerBasis := x -> (
	m := entries(gens(x));
	m = m#0;
	
	OMA("polygb1", "groebner_basis", 
		{ toOpenMathDMPL(m) }
	)
)
