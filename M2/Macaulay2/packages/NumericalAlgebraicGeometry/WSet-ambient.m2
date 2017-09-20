-- Types of Ambient spaces 
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
    slicingVariety( A, transpose(vars R * random(R^n,R^k) - matrix {toList (k:1_R)}) )
    )
