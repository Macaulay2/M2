---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2015 Alvise Trevisan and Alexander I. Suciu
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------

newPackage(
	"ToricTopology",
    	Version => "1.0", 
    	Date => "September 1, 2015",
    	Authors => {
    		{Name => "Alvise Trevisan", Email => "a.trevisan@enpicom.com", HomePage => "http://www.enpicom.com"},
    		{Name => "Alexander I. Suciu", Email => "a.suciu@neu.edu"}
		},  
        Keywords => {"Toric Geometry"},
     	PackageImports => { "OldChainComplexes", "SimplicialComplexes" },
    	Headline => "toric topology"
    	)

protect QTMSimplicialComplex
protect QTMCharacteristicMatrix
protect QTMDimension
		
export{
	"SmallCover", "QuasiToricManifold",
	"isValidChar",
	"smallCover", "quasiToricManifold",
	"cohomologyRing",
	"chern", "stiefelWhitney",
	"bettiSmallCover", "bettiQTM",
	"realProjectiveSpace", "hessenbergVariety", "complexProjectiveSpace",
	"QTMSimplicialComplex", "QTMCharacteristicMatrix", "QTMDimension"
}

-- type definitions --
SmallCover = new Type of HashTable
SmallCover.synonym = "small cover"
QuasiToricManifold = new Type of HashTable
QuasiToricManifold.synonym = "quasi-toric manifold"

-- constructors --
-- note: if not mod 2, first reduce
smallCover = method(TypicalValue => SmallCover)
smallCover(SimplicialComplex,Matrix) := SmallCover => (sc,chi) -> (
	chimod2 := sub(chi,ZZ/2);
	if not isValidChar(sc,chi) then error "expected characteristic matrix";
	new SmallCover from {
		QTMSimplicialComplex => sc,
		QTMCharacteristicMatrix => chimod2,
		QTMDimension => rank( target (chi) )
	}
) 

quasiToricManifold = method(TypicalValue => QuasiToricManifold)
quasiToricManifold(SimplicialComplex,Matrix) := QuasiToricManifold => (sc,chi) -> (
	if not isValidChar(sc,chi) then error "expected characteristic matrix";
	new QuasiToricManifold from { 
		QTMSimplicialComplex => sc, 
		QTMCharacteristicMatrix => chi,
		QTMDimension => 2*rank( target (chi) )
	}
) 
	 

-- methods --

-- check whether a matrix is characteristic for a given simplicial complex 
isValidChar = method(TypicalValue=>Boolean);
isValidChar(SimplicialComplex,Matrix) := Boolean => (sc,chi) -> (
	flag := true;
	mins := listMinors(sc,chi);
	for i in mins do if (i!=1 and i!=-1) then flag=false;
	flag
)

cohomologyRing = method(TypicalValue=>QuotientRing,Options=>true)
-- cohomology ring over the integers mod 2 of a small cover
cohomologyRing(SmallCover) := QuotientRing => {CoefficientRing=>ZZ/2} >> opts -> (N) -> (
	if not opts.CoefficientRing===ZZ/2 then error "Expected ZZ/2 as coefficient ring";
	sc := N.QTMSimplicialComplex; 
	chi := N.QTMCharacteristicMatrix;
	S := (opts.CoefficientRing)[(entries(vars(ring sc)))_0];
	newgens:={};
	scan( (entries(gens(ideal sc)))_0, i->newgens=append(newgens,sub(i,S)) );
	I := ideal( newgens );
	J := ideal ((vars S)*(transpose chi));
	S/(I+J)
)

-- cohomology ring over the integers of a quasi-toric manifold
cohomologyRing(QuasiToricManifold) := QuotientRing =>  {CoefficientRing=>ZZ} >> opts -> (M) -> (
	sc := M.QTMSimplicialComplex;
	chi := M.QTMCharacteristicMatrix;
	C := opts.CoefficientRing;
	S := C[(entries(vars(ring sc)))_0];
	newgens:={};
	scan( (entries(gens(ideal sc)))_0, i->newgens=append(newgens,sub(i,S)) );
	I := ideal( newgens );
	J := ideal ((vars S)*(transpose chi));
	S/(I+J)
)

chern = method(TypicalValue=>List,Options=>{CoefficientRing=>ZZ})
-- Chern classes of a quasi-toric manifold
chern(QuasiToricManifold) := List => opts -> (M) -> (
	T := cohomologyRing(M,CoefficientRing=>opts.CoefficientRing);
	c := 1;
	scan ((entries(vars(ambient T)))_0, i -> c = c*(1+i));
	n := numgens target(M.QTMCharacteristicMatrix);
	chernclasses := {};
	for i in 1..n do chernclasses=append(chernclasses,part(i,sub(c,T)));
	chernclasses
)

stiefelWhitney = method(TypicalValue=>List)
-- Stiefel-Whitney classes of a small cover
stiefelWhitney(SmallCover) := List => (N) -> (
	T := cohomologyRing(N);
	w := 1;
	scan ((entries(vars(ambient T)))_0, i -> w = w*(1+i));
	n := numgens target(N.QTMCharacteristicMatrix);
	swclasses := {};
	for i in 1..n do swclasses=append(swclasses,part(i,sub(w,T)));
	swclasses
)

bettiSmallCover = method()
-- k-th betti number of a small cover
bettiSmallCover(ZZ,SmallCover) := ZZ => (k,N) -> (
	sc := N.QTMSimplicialComplex;
	chi := N.QTMCharacteristicMatrix;
	n := numgens(target(chi));
	ind := drop( subsets(toList(1..n)), 1);
	cclist := {};
	scan(ind, I-> cclist=append(cclist,chainComplex(subComplex(sc,supportChi(chi,I) ) ) ) );
	b := 0;
	scan(cclist, cc -> b = b + rank( HH_(k-1)( cc) ) );
	b
)

-- all the betti numbers up to n of an n-dimensional small cover
bettiSmallCover(SmallCover) := List => (N) -> (
	b := {1};
	for i in 1..(N.QTMDimension) do b=append(b, bettiSmallCover(i,N));
	b
)

bettiQTM = method()
-- k-th betti number of a quasi-toric manifold
bettiQTM(ZZ,QuasiToricManifold) := ZZ => (k, M) -> (
	if ((k < 0) or (k > M.QTMDimension) or (k % 2 == 1)) then (
		0
	) 
	else (
		coho := cohomologyRing M;
		(((coefficients numerator reduceHilbert hilbertSeries coho)_1)_0)_(sub(k/2,ZZ))
	)
)

-- all the betti numbers up to 2n of an 2n-dimensional quasi-toric manifold
bettiQTM(QuasiToricManifold) := List => (M) -> (
	b := {};
	for i in 1..(M.QTMDimension) do b = append(b, bettiQTM(i, M));
	b
)

-- Sample small covers --

realProjectiveSpace = method(TypicalValue=>SmallCover)
-- n-dimensional real projective space
realProjectiveSpace(ZZ) := SmallCover => (n) -> (
	smallCover(projectiveSpace(n, ZZ/2))
)


hessenbergVariety = method(TypicalValue=>SmallCover)
-- Hessenberg variety associated to the (dual of the) n-dimensional permutahedron
hessenbergVariety(ZZ) := SmallCover => (n) -> (
    smallCover(permutahedronDual(n),chiHessenberg(n))
)


-- Sample quasi-toric manifolds
complexProjectiveSpace = method(TypicalValue=>QuasiToricManifold)
-- n-dimensional complex projective space
complexProjectiveSpace(ZZ) := QuasiToricManifold => (n) -> (
	quasiToricManifold(projectiveSpace(n, ZZ))
)


-- Helper functions --
projectiveSpace = (n, base) -> (
	I := id_(base^n);
	l := {};
	scan(n, i -> l=append(l, {1}));
	ones := matrix(l);
	R := base[vars(0..n)];
	varlist := (entries(vars(R)))_0;
	maxmissingface := sub(varlist_0 ,R);
	scan(1..n, i -> maxmissingface = maxmissingface * sub(varlist_i, R) );
	K := simplicialComplex(monomialIdeal(maxmissingface));
	(K,I|ones)
)

listMinors = (sc,chi) -> (
	listminors:={};
	for mon in facets(sc) do (
		listminors=append(listminors,determinant(submatrix(chi,indices(mon))));
	);
	listminors
)

subComplex = (sc, V) -> (
            varlist := (entries(vars(ring sc)))_0;
            mV := sub(varlist_(V_0-1),ring sc); 
			scan(drop(V,1),i->mV=mV*sub(varlist_(i-1),ring(sc)));
			candidates := {};
			for k in (0..(length V)) do (
				candidates = join(candidates, faces(k,sc));
			);       
			k:=0;    
			lis := {};         
            while k!= length(candidates) do (
	        	if (denominator(sub(mV, ring sc)/(candidates_k)))==1 then (
	            	lis=append(lis,candidates_k);
                   	candidates=drop(candidates,{k,k});
                   	k=k-1;
            	); 
	            k=k+1;
		    );
	        simplicialComplex(lis)
);

-- given a char matrix chi (n rows, m cols) and a subset I={i_1, .., i_n} of [n]
-- returns the support of chi_I = chi_{i_1} + ... + chi_{i_n} 	  
supportChi = (chi, I) -> (
                cI := {};
                m := numgens(source(chi));
                n := numgens(target(chi));
                for i in (1..m) do cI=append(cI,0);
                scan( I, i-> cI = entries((transpose chi)_(i-1)) + cI );
                fincI := {};                 
                scan(cI, i -> fincI = append(fincI,sub(sub(i,ZZ/2),ZZ))) ;
                supp:={};
                j:=1;
                for j in (1..m) do (
                        if (fincI_(j-1) != 0) then supp=append(supp,j);
                );
                supp
);


simplicialIntToMon = (sc) -> (
			  p := max( flatten( sc ) );
			  R := ZZ[vars(0..p-1)];
			  e := {};
			  for i in (1..p) do e=append(e,0);
			  lis := {};
			  for i in (0..length(sc)-1) do (
			  		lis = append(lis,new MutableList from e); 
			  		for j in sc#i do (
			  				lis#i#(j-1)=1; 
			  				); 
			    	);
			  lismon := {};
			  for i in lis do lismon=append(lismon,R_(toList(i)));
			  simplicialComplex( lismon )
			  );
              
-- returns the characteristic matrix for the Hessenberg variety sitting on the dual of the n-dimensional permutahedron
chiHessenberg = (n) -> (
	-- finds the char matrix
	col1s := {};
	chisimplex := id_((ZZ/2)^n)|(transpose (matrix {apply(n,i->1)} ));
	columns := new MutableHashTable;
	i :=0;
	for maxl in subsets(n+1,n) do (
		columns#maxl = chisimplex_{i};
		i=i+1;
	);
	
    vertices := drop(drop(subsets(n+1),1),-1);
	for vert in vertices do (
		if not member(vert, subsets(n+1,n)) then (
			supersets := {};
			scan(subsets(n+1,n), i -> (if (not(i==vert) and isSubset(set(vert),set(i))) then 
				supersets= append(supersets,i) ) );
			col := 0;
			scan(supersets, i -> col = col+ columns#i);	
			columns#vert = col;
		);
	);
	
	--finally computes the char matrix
	chi := columns#(vertices#0);
	for i in 1..(length(vertices)-1) do (
		chi = chi | columns#(vertices#i);
	);
	
    chi
);

permsimplices = (lis) -> (
    resl :={};
    for fac in lis do (
        if length(last(fac))==1 then return lis
        else (
            tmplis := {};
            for sub in subsets(last(fac),length(last(fac))-1) do (
                tmplis = append(tmplis, append(fac, sub));
            );
            resl = join(resl, tmplis);
        );
    );
    return permsimplices(resl);
)

-- returns the simplicial complex dual to the n-dimensional permutahedron
permutahedronDual = (n) -> (
	vertices := drop(drop(subsets(n+1),1),-1);
	hashgen := {};
	for i in 1..length(vertices) do (
		hashgen = append( hashgen, {vertices#(i-1),i});
	); 
	vhash := hashTable(hashgen); 
    
	psimplices := {}; 
	for i in 0..n do (
		psimplices = append(psimplices,{(subsets(n+1,n))#i}); 
	);
	simplices := {};
	for permsimplex in permsimplices(psimplices) do (
		simplex := {};
		for sub in permsimplex do ( 
			simplex = append(simplex, vhash#sub);
		);
		simplices = append(simplices, simplex);
	);
	simplicialIntToMon(simplices)
)

-------------------
-- Documentation --
-------------------

beginDocumentation()

doc ///
    Key
        ToricTopology
    Headline
        homological computations in toric topology
    Description
        Text 
            ToricTopology is a package for computing with quasi-toric manifolds and small covers.
            
	    A quasi-toric manifold (or small cover) is entirely determined by a pair consisting of a simplicial complex K and a matrix chi which is characteristic for K.
   
  	    If K has n vertices, we can think of its k-faces as sets of integers between 1 and n.
            A matrix chi is characteristic for K if all maximal minors of chi indexed by the facets of  K have determinant equal to 1 or -1.

///


doc ///
  Key
    SmallCover
  Headline
    the class of all small covers 
  Description
   Text
       A small cover is represented by a simplicial complex K and matrix which is characteristic for K.
  SeeAlso
    QuasiToricManifold
///

doc ///
  Key
    QuasiToricManifold
  Headline
    the class of all quasi-toric manifolds
  Description
   Text
       A quasi-toric manifold is represented by a simplicial complex K and matrix which is characteristic for K.
  SeeAlso
   SmallCover
///

doc ///
  Key
    isValidChar
    (isValidChar,SimplicialComplex,Matrix)
  Headline
    Whether a matrix is characteristic for a simplicial complex
  Usage
    isValidChar(K,chi)
  Inputs
    K:SimplicialComplex
    chi:Matrix
  Outputs
    :Boolean
  Description
   Text
        Checks whether chi is characteristic for K.
  SeeAlso
     
///


doc ///
  Key
    smallCover
    (smallCover,SimplicialComplex,Matrix)
  Headline
    Create a small cover
  Usage
    smallCover(K,chi)
  Inputs
    K:SimplicialComplex
    chi:Matrix
  Outputs
    :SmallCover
  Description
   Text
        Create a small cover over K with characteristic matrix chi.
        If chi is not characteristic for K, then an error is returned.
        The entries of chi are automatically converted to ZZ/2 entries, if they not already so.
  SeeAlso
     
///

doc ///
  Key
    quasiToricManifold
    (quasiToricManifold,SimplicialComplex,Matrix)
  Headline
    Create a quasi-toric manifold
  Usage
    quasiToricManifold(K,chi)
  Inputs
    K:SimplicialComplex
    chi:Matrix
  Outputs
    :QuasiToricManifold
  Description
   Text
        Create a quasi-toric manifold over K with characteristic matrix chi.
        If chi is not characteristic for K, an error is returned.
  SeeAlso
///


doc ///
  Key
    cohomologyRing
    (cohomologyRing,SmallCover)
    (cohomologyRing,QuasiToricManifold)
  Headline
    Compute the cohomology ring of a small cover or quasi-toric manifold
  Usage
    cohomologyRing(N)
    cohomologyRing(M)
  Inputs
    N:SmallCover
    M:QuasiToricManifold
  Outputs
    :QuotientRing
  Description
   Text
        Compute the cohomology ring of a small cover (over ZZ/2) or quasi-toric manifold (over ZZ). 
  SeeAlso
     
///


doc ///
  Key
    stiefelWhitney
    (stiefelWhitney,SmallCover)
  Headline
    Compute the Stiefel-Whitney classes of a small cover
  Usage
    stiefelWhitney(N)
  Inputs
    N:SmallCover
  Outputs
    :List
  Description
   Text
        Compute the Stiefel-Whitney classes of a small cover.
        The output is a list of elements in the cohomology ring of N.
  SeeAlso
    cohomologyRing
///

doc ///
  Key
    chern
    (chern,QuasiToricManifold)
  Headline
    Compute the Chern classes of a quasi-toric manifold
  Usage
    chern(M)
  Inputs
    M:QuasiToricManifold
  Outputs
    :List
  Description
   Text
        Compute the Chern classes of a quasi-toric manifold.
        The output is a list of elements in the cohomology ring of M.
  SeeAlso
    cohomologyRing
///


doc ///
  Key
    bettiSmallCover
    (bettiSmallCover,ZZ,SmallCover)
    (bettiSmallCover,SmallCover)
  Headline
    Compute the betti numbers of a small cover
  Usage
    bettiSmallCover(k,N)
    bettiSmallCover(N)
  Inputs
    k:ZZ
    N:SmallCover
  Outputs
    :ZZ
    :List
  Description
   Text
        Compute the betti numbers of a small cover.
        If a dimension k is specified, then only the k-th betti number of N is computed.
        If no dimension is specified, all the betti numbers between 0 and the dimension of N are computed.
  SeeAlso
     
///

doc ///
  Key
    bettiQTM
    (bettiQTM,ZZ,QuasiToricManifold)
    (bettiQTM,QuasiToricManifold)
  Headline
    Compute the betti numbers of a quasi-toric manifold
  Usage
    bettiQTM(k,M)
    bettiQTM(M)
  Inputs
    k:ZZ
    M:QuasiToricManifold
  Outputs
    :ZZ
    :List
  Description
   Text
        Compute the betti numbers of a quasi-toric manifold.
        If a dimension k is specified, then only the k-th betti number of M is computed.
        If no dimension is specified, all the betti numbers between 0 and the dimension of M are computed.
  SeeAlso
     
///

doc ///
  Key
    realProjectiveSpace
    (realProjectiveSpace,ZZ)
  Headline
    Real projective space of dimension n
  Usage
    realProjectiveSpace(n)
  Inputs
    n:ZZ
  Outputs
    :SmallCover
  Description
   Text
        Real projective space of dimension n, as small cover.
  SeeAlso
    complexProjectiveSpace
///

doc ///
  Key
    complexProjectiveSpace
    (complexProjectiveSpace,ZZ)
  Headline
    Complex projective space of dimension n
  Usage
    complexProjectiveSpace(n)
  Inputs
    n:ZZ
  Outputs
    :QuasiToricManifold
  Description
   Text
        Complex projective space of dimension n, as small cover.
  SeeAlso
    realProjectiveSpace
///


doc ///
  Key
    hessenbergVariety
    (hessenbergVariety,ZZ)
  Headline
    Hessenberg variety asscoiated to the n-permutahedron 
  Usage
    hessenbergVariety(n)
  Inputs
    n:ZZ
  Outputs
    :SmallCover
  Description
   Text
       Hessenberg variety asscoiated to the n-permutahedron, as small cover.
  SeeAlso
    
///
