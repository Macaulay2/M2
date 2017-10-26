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

-- MULTI-AFFINE
MultiAffineSpace = new Type of Ambient
net MultiAffineSpace := A -> net "A^" | net dim A
multiAffineSpace = method()
multiAffineSpace Ring :=  R -> (
    assert all(gens R, x->all(degree x, a->a>=0) and max degree x == 1);
    new MultiAffineSpace from { 
    	"coordinate ring"=> R
    	}
    )

standardWeightVector = (i,n) -> (d := new MutableList from (n:0); d#i=1; toList d)
multiAffineSpace(Ring,List,Symbol) := (C,N,x) -> multiAffineSpace( C[
	splice apply(#N,i->x_(i,1)..x_(i,N#i)), 
	Degrees=>flatten apply(#N,i->toList(N#i:standardWeightVector(i,#N)))
	] )

ring MultiAffineSpace := A -> A#"coordinate ring"

dim MultiAffineSpace := A -> degree product gens ring A -- multidimension

field MultiAffineSpace := A -> coefficientRing ring A

variables = method() 
variables (ZZ, MultiAffineSpace) := (i,A) -> ( -- coordinates of the i-th piece
    R := ring A;
    basis(
    	standardWeightVector(i,degreeLength R),
    	R
	)
    )

randomSlicingVariety(MultiAffineSpace,List) := (A,K) -> ( -- K = list of codimensions 
    assert(all(K,k->class k===ZZ and k>=0) and sum K>0);
    R := ring A;
    N := dim A;
    M := transpose matrix {flatten apply(#N,i->(
	    n := N#i; 
	    k := K#i;
	    if k==0 then {}
	    else flatten entries(variables(i,A) * random(R^n,R^k) - matrix {toList (k:1_R)})
	    ))};
    slicingVariety( A, rationalMap M )
    )
