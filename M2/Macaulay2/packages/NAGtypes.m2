-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
     "NAGtypes",
     Version => "1.3.0.2",
     Date => "August, 2010",
     Headline => "Common types used in Numerical Algebraic Geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => true 
     )

export {
     -- software 	  
     "PHCPACK",
     -- witness set
     "WitnessSet", "witnessSet", "equations", "slice", "points", "Equations", "Slice", "Points", 
     -- point (solution)
     "Point", "point", "coordinates",
     "Coordinates", "SolutionStatus", "LastT", "RCondition", "NumberOfSteps",
     "Regular", "Singular", "Infinity", "MinStepFailure"
     }

Point = new Type of MutableHashTable 
WitnessSet = new Type of MutableHashTable 

-----------------------------------------------------------------------
-- POINT = {
--   Coordinates => List of CC,
--   NumberOfSteps => ZZ, -- number of steps made while tracking the path
--   SolutionStatus => {RegularSolution, SingularSolution, Infinity, MinStepFailure}
--   LastT => RR in [0,1]
--   RCondition => reverse condition number of the Jacobian
--   }
Point.synonym = "point"
point = method()
point List := s -> new Point from {Coordinates=>first s} | drop(s,1)
net Point := p -> (
     if not p.?SolutionStatus or p.SolutionStatus === Regular then net p.Coordinates 
     else if p.SolutionStatus === Singular then net "[S,t=" | net p.LastT | net "]"
     else if p.SolutionStatus === MinStepFailure then net "[M,t=" | net p.LastT | net "]"
     else if p.SolutionStatus === Infinity then net "[I,t=" | net p.LastT | net "]"
     else error "the point is corrupted"
     ) 
coordinates = method()
coordinates Point := p -> p.Coordinates
status Point := o -> p -> p.SolutionStatus
matrix Point := o -> p -> matrix {coordinates p}
-----------------------------------------------------------------------
-- WITNESS SET = {
--   Equations,            -- an ideal  
--   Slice,                -- a list of linear equations OR a matrix (of their coefficients)
--   Points	           -- a list of points (in the format of the output of solveSystem/track) 
--   }
-- caveat: we assume that #Equations = dim(Slice)   
WitnessSet.synonym = "witness set"
WitnessSet.Tolerance = 1e-6;
dim WitnessSet := W -> ( if class W.Slice === List then #W.Slice 
     else if class W.Slice === Matrix then numrows W.Slice 
     else error "ill-formed slice in WitnessSet" )
codim WitnessSet := W -> numgens ring W - dim W
ring WitnessSet := W -> ring W.Equations
degree WitnessSet := W -> #W.Points
ideal WitnessSet := W -> W.Equations
net WitnessSet := W -> "(dim=" | net dim W |",deg="| net degree W | ")" 
witnessSet = method()
witnessSet (Ideal,Ideal,List) := (I,S,P) -> new WitnessSet from { Equations => I, Slice => S_*, Points => VerticalList P}
witnessSet (Ideal,Matrix,List) := (I,S,P) -> new WitnessSet from { Equations => I, Slice => S, Points => VerticalList P}
witnessSet Ideal := I -> (
     n := numgens ring I;
     d := dim I;
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM = promote(SM,ring I);
     S := ideal(SM * transpose vars ring I + random(CC^d,CC^1));
     RM := (randomUnitaryMatrix numgens I)^(toList(0..n-d-1));
     RM = promote(RM,ring I);
     P := solveSystem(flatten entries (RM * transpose gens I) | S_*);
     PP := select(P, p->norm sub(gens I, matrix p) < 1e-5);
     witnessSet(I,S,PP/first)
     )
points = method() -- strips all info except coordinates, returns a doubly-nested list
points WitnessSet := (W) -> apply(W.Points, coordinates)
equations = method() -- returns list of equations
equations WitnessSet := (W) -> (W.Equations)_*
slice = method() -- returns linear equations for the slice (in both cases)   
slice WitnessSet := (W) -> ( if class W.Slice === List then W.Slice
     else if class W.Slice === Matrix then sliceEquations(W.Slice, ring W)
     else error "ill-formed slice in WitnessSet" )

-- extra functions

generalEquations = method()
generalEquations WitnessSet := (W) -> (
     -- change the equations to be general change of vars, if not a CI
     -- the output is a new witness set, with the same points and slice.
     R := ring W;
     n := numgens R;
     d := dim W;
     ngens := numgens ideal W;
     if ngens === n-d then W
     else (
	  -- take random combinations of the equations
	  neweqns := (generators ideal W) * random(R^ngens, R^(n-d));
	  witnessSet(ideal neweqns, ideal W.Slice, W.Points))
     )

addSlackVariables = method()
addSlackVariables WitnessSet := (W) -> (
     -- creates a new system of polynomials, in variables:
     -- old set of variables, and zz1, ..., zzd, where
     -- d is the dimension of W.
     R := ring W;
     n := numgens R;
     d := dim W; -- this will be the number of slack variables to add
     W1 := generalEquations W;
     -- Add in new variables zz1, ..., zzd,
     --  this changes the equations, the slice, and the points
     slackvars := apply(d, i->getSymbol("zz"|toString (i+1)));
     newR := (coefficientRing R)[gens R, slackvars];
     newvars := (vars newR)_{n..n+d-1};
     -- new slice:
     newSlice := apply(d, i -> sub(W1.Slice#i,newR) + newR_(n + i));
     -- add a linear matrix 
     A := random(newR^(d),newR^(n-d));
     AZ := transpose newvars * A;
     newEqns := (sub(gens ideal W1, newR) + AZ) | newvars;
     -- new points
     zeros := toList apply(d, i -> 0_(coefficientRing R));
     newPoints := apply(W1.Points, pt -> join(pt,zeros));
     witnessSet(ideal newEqns, ideal newSlice, newPoints)
     )

beginDocumentation()

document {
     Key => PHCPACK,
     Headline => "use PHCpack for homotopy continuation",
     "Available at ", TT "http://www.math.uic.edu/~jan/download.html"
     }
document {
     Key => {Coordinates, SolutionStatus, LastT, RCondition, NumberOfSteps},
     Headline => "various attributes of a Point"
     }
endPackage "NAGtypes" 
