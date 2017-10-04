-- Types of Ambient spaces 

-- AFFINE
AffineSpace = new Type of Ambient
net AffineSpace := A -> net "A^" | net dim A
affineSpace = method()
affineSpace Ring :=  R -> new AffineSpace from { 
    "coordinate ring"=> R
    }
affineSpace(Ring,ZZ,Symbol) := (C,n,x) -> affineSpace( C[x_1..x_n] )

ring AffineSpace := A -> A#"coordinate ring"

dim AffineSpace := A -> dim ring A

field = method()
field AffineSpace := A -> coefficientRing ring A
 
randomSlicingVariety(AffineSpace,ZZ) := (A,k) -> ( -- k = codim 
    R := ring A;
    n := dim A;
    slicingVariety( A, rationalMap transpose(vars R * random(R^n,R^k) - matrix {toList (k:1_R)}) )
    )

-- hack!!! for WitnessSet
slicingVariety WitnessSet := W -> slicingVariety(affineSpace ring W, rationalMap transpose matrix{slice W})


-- PROJECTIVE
ProjectiveSpace = new Type of Ambient
net ProjectiveSpace := A -> net "P^" | net dim A
projectiveSpace = method()
projectiveSpace Ring :=  R -> new ProjectiveSpace from { 
    "coordinate ring"=> R
    }
projectiveSpace(Ring,ZZ,Symbol) := (C,n,x) -> projectiveSpace( C[x_0,x_1..x_n] )

ring ProjectiveSpace := A -> A#"coordinate ring"

dim ProjectiveSpace := A -> dim ring A - 1 

field ProjectiveSpace := A -> coefficientRing ring A
 
randomSlicingVariety(ProjectiveSpace,ZZ) := (A,k) -> ( -- k = codim 
    R := ring A;
    n := dim A;
    slicingVariety( A, rationalMap transpose(vars R * random(R^(n+1),R^k)) )
    )

