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
    		{Name => "Alvise Trevisan", Email => "a.trevisan@enpicom.com"},
    		{Name => "Alexander I. Suciu", Email => "a.suciu@neu.edu"}
		},  
    	Headline => "Toric topology"
    	)

protect QTMSimplicialComplex
protect QTMCharacteristicMatrix
protect QTMDimension
		
export{
	SmallCover,QuasiToricManifold,
	isValidChar,
	smallCover, quasiToricManifold,
	cohomologyRing,
	chern, stiefelWhitney,
	bettiSmallCover, bettiQTM,
	realProjectiveSpace, complexProjectiveSpace,
	QTMSimplicialComplex, QTMCharacteristicMatrix, QTMDimension
}

needsPackage "SimplicialComplexes"

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

isValidChar = method(TypicalValue=>Boolean);
isValidChar(SimplicialComplex,Matrix) := Boolean => (sc,chi) -> (
	flag := true;
	mins := listMinors(sc,chi);
	for i in mins do if (i!=1 and i!=-1) then flag=false;
	flag
)

cohomologyRing = method(TypicalValue=>QuotientRing,Options=>true)
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

bettiSmallCover(SmallCover) := List => (N) -> (
	b := {1};
	for i in 1..(N.QTMDimension) do b=append(b, bettiSmallCover(i,N));
	b
)

bettiQTM = method()
bettiQTM(ZZ,QuasiToricManifold) := ZZ => (k, M) -> (
	if ((k < 0) or (k > M.QTMDimension) or (k % 2 == 1)) then (
		0
	) 
	else (
		coho := cohomologyRing M;
		(((coefficients numerator reduceHilbert hilbertSeries coho)_1)_0)_(sub(k/2,ZZ))
	)
)

bettiQTM(QuasiToricManifold) := List => (M) -> (
	b := {};
	for i in 1..(M.QTMDimension) do b = append(b, bettiQTM(i, M));
	b
)

realProjectiveSpace = method(TypicalValue=>SmallCover)
realProjectiveSpace(ZZ) := SmallCover => (n) -> (
	smallCover(projectiveSpace(n, ZZ/2))
)

complexProjectiveSpace = method(TypicalValue=>QuasiToricManifold)
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
	for mon in (entries(facets(sc)))_0 do (
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
				candidates = join(candidates,(entries(faces(k,sc)))_0);
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
