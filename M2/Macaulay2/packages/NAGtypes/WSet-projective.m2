-- Witness set for a projective variety 
-- (in fact subscheme of PP^n that may be neither irreducible nor non-reduced)
-- should store 
-- (1) a projective SlicingVariety S
-- (2) a finite collection of (W,H) where
--   W is a WSet in the corresponding affine chart w.r.t. toA(S)
--   H is a linear form in (homogeneous coordinates) H=1
-- toA is a method giving rational maps that establish 
-- a correspondence between the points in P^n\{H=0} and {H=1}\subset A^{n+1}

PWSet = new Type of WSet
PWSet.synonym = "projective witness set"
texMath PWSet := x -> texMath net x
net PWSet := PW -> "[dim="| dim PW | " deg="| degree PW |" in " | net ambient PW | "]"

makePWSet = method(TypicalValue=>PWSet, 
    Options=>{Tolerance=>0.000001} -- used to determine whether the point is "at infinity": see toChart
    )
makePWSet (PolySystem,SlicingVariety,List) := o -> (F,S,pts) ->
  new PWSet from {
      "equations" => F,
      "slice" => S,
      Points => pts,
      "charts" => {},
      Tolerance => o.Tolerance
      }

dim PWSet := PW -> codim slicingVariety PW 
codim PWSet := {} >> o -> PW -> dim slicingVariety PW
ambient PWSet := PW -> ambient slicingVariety PW
degree PWSet := PW -> #PW.Points
points PWSet := PW -> PW.Points
slicingVariety PWSet := PW -> PW#"slice"

addChart = method()
addChart (PWSet, List, RingElement) := (PW,pts,H) -> PW#"charts" = PW#"charts" | {
    (witnessSet(PW#"equations", polySystem (map slicingVariety PW || matrix{{H-1}}), pts), H)
    }  

addPointsToChart = method()
addPointsToChart (PWSet, List, RingElement) := (PW,pts,H) -> (
    charts := PW#"charts";
    c := position(charts,WH->last WH==H); 
    if c === null then error "uknown chart";
    W := first charts#c; 
    PW.Points = PW.Points | pts;
    W.Points = W.Points | pts;
    )  

toChart = method()
toChart(PWSet,Point,RingElement) := (PW,p,H) -> (
    a := sub(H,matrix p);
    if abs a < PW.Tolerance then infinity else point{ apply(coordinates p,x->x/a) }
    )  

-- OUT: list of "affine" points 
toChart(PWSet,RingElement) := (PW,H) -> for p in points PW list (
    p' := toChart(PW,p,H);
    if p' === infinity then continue
    else p' 
    ) 

-- assume nothing goes wrong (no points are close to H'=0)!!! 
moveSlicingVariety(PWSet,SlicingVariety) := (PW,S) -> (
    PW' := makePWSet(PW#"equations", S, {});
    H' := random(1, ring ambient S);
    addChart(PW',toChart(PW',H'),H'); 
    for WH in PW#"charts" do (
	(W,H) := WH;
	W' := moveSlicingVariety(W,slicingVariety(ambient W, rationalMap transpose matrix{drop(slice W,-1)|{H'-1}}));
	addPointsToChart(PW',points W',H');
	);
    PW'
    ) 
    
TEST ///
    errorDepth = 0;
    debug needsPackage "NumericalAlgebraicGeometry"
    debug NAGtypes
    R = CC[x,y,z]
    A = projectiveSpace R
    S = slicingVariety(A,rationalMap matrix{{x+2*y+3*z}})
    F = polySystem {x*y*z}
    PW = makePWSet(F,S,{point{{0_CC,3,-2}},point{{3,0_CC,-1}},point{{2,-1,0_CC}}})    
    addChart(PW,toChart(PW,x+2*y),x+2*y);
    addChart(PW, {toChart(PW,last points PW,x)}, x);
    peek PW
    S' = slicingVariety(A,rationalMap matrix{{x+y+z}})
    PW' = moveSlicingVariety(PW,S')
    peek PW'
    points PW'/matrix/clean_0.0001
///
