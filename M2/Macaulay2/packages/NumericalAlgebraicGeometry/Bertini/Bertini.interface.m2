-- Bertini interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2

toBertiniOptions = method()
toBertiniOptions OptionTable := OptionTable => o -> (
    opt := {
	TRACKTOLBEFOREEG=>o.CorrectorTolerance,
	TRACKTOLDURINGEG=>o.CorrectorTolerance*o.EndZoneFactor,
	FINALTOL=>o.CorrectorTolerance*o.EndZoneFactor	
	};
    new OptionTable from opt -- TODO: write all options
    )
solveBertini = method(TypicalValue => List)
solveBertini (List,OptionTable) := List => (F,o) -> (
    R := ring first F;
    coeffR := coefficientRing R;
    if not(
	instance(ring 1_coeffR, ComplexField) 
	or instance(ring 1_coeffR, RealField)
	or coeffR===QQ or coeffR ===ZZ
	) then error "expected coefficients that can be converted to complex numbers";  
    sols := bertiniZeroDimSolve(F,toBertiniOptions o); 
    sols    
    )

trackHomotopyBertini = method(TypicalValue => List)
trackHomotopyBertini (List,RingElement,List,OptionTable) := List => (F,t,solS,o) -> (
    bertiniTrackHomotopy(F,t,solS,toBertiniOptions o)
    )

trackBertini = method(TypicalValue => List)
trackBertini (List,List,List,OptionTable) := List => (S,T,solS,o) -> (
    R := ring first S;
    K := coefficientRing R;
    t := symbol t;
    Rt := K(monoid[gens R, t]);     
    t = last gens Rt;
    H := apply(#S, i->o#(NumericalAlgebraicGeometry$gamma)*t^(o#(NumericalAlgebraicGeometry$tDegree))*sub(S#i,Rt)+(1-t)^(o#(NumericalAlgebraicGeometry$tDegree))*sub(T#i,Rt));
    bertiniTrackHomotopy(H,t,solS,toBertiniOptions o)
    )
trackBertini (PolySystem,PolySystem,List,OptionTable) := List => (S,T,solS,o) -> trackBertini (equations S, equations T, solS, o)