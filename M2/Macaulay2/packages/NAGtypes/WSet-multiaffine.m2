-- multi-affine and multi-projective witness sets 

MultiSlicingVariety = new Type of SlicingVariety
MultiSlicingVariety.synonym = "multi-affine slice"
multiSlicingVariety = method()
multiSlicingVariety(Ambient,List) := (A,MM) -> new MultiSlicingVariety from {"ambient"=>A, "maps"=>MM}
codim MultiSlicingVariety :=  {} >> o -> S -> rank@@target\S#"maps"
texMath MultiSlicingVariety := x -> texMath net x
net MultiSlicingVariety := S -> net "slice of codim " | net codim S
map MultiSlicingVariety := o -> S -> transpose matrix{S#"maps"/transpose}

randomSlicingVariety(MultiAffineSpace,List) := (A,K) -> ( -- K = list of codimensions 
    assert(all(K,k->class k===ZZ and k>=0) and sum K>0);
    R := ring A;
    N := dim A;
    M := apply(#N,i->(
	    n := N#i; 
	    k := K#i;
	    if k==0 then map(R^0,R^1,0)
	    else (variables(i,A) * random(R^n,R^k) - matrix {toList (k:1_R)})
	    ));
    multiSlicingVariety( A, M/rationalMap )
    )

MultiAffineWSet = new Type of WSet
multiAffineWSet = method(TypicalValue=>MultiAffineWSet)
multiAffineWSet (PolySystem,SlicingVariety,List) := (F,S,pts) -> (
      R := ring F;
      -- assert isHomogeneous F.PolyMap; --!!! 
      new MultiAffineWSet from {
      	  "ambient" => multiAffineSpace R,
	  "equations" => F, 
      	  "slice" => S,
      	  "points" => pts
      	  }
      )
dim MultiAffineWSet := W -> codim W#"slice"
degree MultiAffineWSet := W -> # points W 

net MultiAffineWSet := W -> net "multiAffineWSet(dim=" | net dim W | ",deg=" | net degree W | ")" 

slicingVariety MultiAffineWSet := W -> W#"slice" 

-- this returns points "downstairs"
points MultiAffineWSet := W -> W#"points"

----------------------------------------------------------
-- Witness collection for a multi-projective variety 
-- should store a collection of multi-affine witness sets

WCollection = new Type of MutableHashTable
WCollection.synonym = "multi-projective witness set collection"

net WCollection := W -> if #W#"witnesses">0 then
"[dim="| dim W | " deg="| degree W |" in " | net ambient W | "]" else
"uninitialized WCollection"

wCollection = method(TypicalValue=>WCollection, 
    Options=>{Tolerance=>0.000001} -- used to determine whether the point is "at infinity": see toChart
    )
wCollection(Ambient,PolySystem) := o -> (A,F) ->
  new WCollection from {
      "ambient" => A,
      "equations" => F,
      "witnesses" => new MutableHashTable from {},
      Tolerance => o.Tolerance
      }

dim WCollection := W -> if #W#"witnesses">0 then dim (W_(first multidimensions W)) else error "WCollection not initialized"  
codim WCollection := {} >> o -> W ->  if #W#"witnesses">0 then codim (W_(first multidimensions W)) else error "WCollection not initialized"  
ambient WCollection := W -> W#"ambient"
witnessKeys = method()
witnessKeys WCollection := W -> keys W#"witnesses"
multidimensions = method()
multidimensions WCollection := W -> first \ witnessKeys W
WCollection _ Sequence := (W,d) -> if member(d,witnessKeys W) then W#"witnesses"#d else null
WCollection _ List := (W,d) ->  (
    r := select(witnessKeys W, k -> first k == d);
    if #r>0 then W_(first r) 
    else null
    ) 

-- degree WCollection := W -> #W.Points
-- points WCollection := W -> W.Points

addWSet = method()
addWSet (WCollection, List, MultiSlicingVariety, List) := (W,H,S,pts) -> W#"witnesses"#(codim S,H,S) = 
    multiAffineWSet(W#"equations", 
	multiSlicingVariety(multiAffineSpace ring ambient W,
	    apply(#H, i->rationalMap transpose matrix {flatten entries S#"maps"#i | {H#i}})), 
	pts)

toChart(WCollection,AbstractPoint,List) := (W,p,H) -> (
    N := dim ambient W;
    assert(#N==#H);
    A := for h in H list sub(h,matrix p);
    if any(A/abs) < W.Tolerance then infinity else point{ 
	c := coordinates p;
	c' := {};
	scan(#N, i->(
	    	n := N#i;	
	    	t := take(c,n); 
	    	c = drop(c,n);
	    	c' = c' | apply(t,x->x/A#i);
	    	));
	c'
	}
    )  

    
TEST ///
debug needsPackage "NAGtypes"
errorDepth = 2
A = multiProjectiveSpace(CC_53,{1,1},x)
use ring A 
-- multi=homogenized parabola y-z^2=0 where y=x_(0,0) and z=x_(1,0)
F = polySystem {x_(0,0)*x_(1,1)^2-x_(1,0)^2*x_(0,1)}
H = {x_(1,1)-1,x_(0,1)-1}
S = multiSlicingVariety(A, {rationalMap matrix{{x_(0,0)-1}}, rationalMap map((ring A)^0,(ring A)^1,0)})
pts = {point{{1,1,1,1}},point{{1,1,-1,1}}}
W = wCollection(A,F) 
addWSet(W,H,S,pts)
peek W
dim W
W_{1,0}
assert(W_{0,1} === null)
///
