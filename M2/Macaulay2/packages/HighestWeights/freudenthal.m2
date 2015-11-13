--------------------------------------------------------------------------------
-- Copyright 2014  Federico Galetto
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

export {"decomposeWeightsList"}

--THROUGHOUT HW stands for Highest Weight

--References for some of the functions appearing here:
--DeGraaf = W.A. DeGraaf - Lie Algebras: Theory and Algorithms
--Moody-Patera = R.V. Moody and J. Patera - Fast recursion formula
--               for weight multiplicities

--INPUT
--R: root system
--w: a weight of R
--OUTPUT
--true, if w is dominant
--false, otherwise
isDominant = method(TypicalValue => Boolean)
isDominant (RootSystem,Weight) := Boolean => (R,w) -> all ( 1 .. rank R , i -> eval(R,w,i) >= 0 )

--INPUT
--R: a root system
--mu: a weight of R
--OUTPUT
--the unique dominant weight of R conjugate to mu
--[DeGraaf, p. 319]
conjugateDominantWeight = method(TypicalValue => Weight)
conjugateDominantWeight (RootSystem,Weight) := Weight => (R,mu) -> (
     w := mu;
     while not isDominant(R,w) do (
	 for i from 1 to rank(R) do (
	     if eval(R,w,i)<0 then (
		 w = reflect(R,i,w);
		 break;
		 );
	     );
	 );
     return w;
     )

--INPUT
--R: a root system
--w: a weight of R
--OUTPUT
--returns the squared norm of w+rho
--where rho is half the sum of all positive roots of R
plusRhoNorm = (R,w) -> (
     rho := halfSumOfRoots(R);
     scalarProduct(R,w+rho,w+rho)
     )

--INPUT
--R: a root system
--X: a list of weights of R
--OUTPUT
--a list of the weights w in X sorted by decreasing value
--of the squared norm of w+half the sum of positive roots of R
sortLayer = (R,X) -> (
     --pair the norm with the index of the weight in X
     --so I can distinguish two weights with the same norm
     Y := apply(#X,i->(plusRhoNorm(R,X_i),i));
     --rsort works lexicographically so the pairs are sorted
     --by decreasing values of the first entry i.e. the norm
     Y = rsort Y;
     --get the weights in X using the order from the second entry of Y
     apply(Y,i->X_(last i))
     )

--INPUT
--R: a root system
--l: a weight of R
--OUTPUT
--if l is dominant, returns a list of the dominant weights <l
--the weights are computed in layers of the form l-(n pos. roots)
--the output list is sorted using sortLayer
--[DeGraaf, p. 332]
--if l is not dominant, returns {}
dominantWeightsOfHWModule = (R,l) -> (
     if isDominant(R,l)==false then return {};
     posRoots := elements positiveRoots(R);
     previousLayer := {};
     --the weights are computed in layers starting from l
     nextLayer := {l};
     --this list will be the output
     dominantWeights := {};
     while nextLayer!={} do (
	  previousLayer = nextLayer;
	  --the new layer comes from the previous one subtracting positive roots
	  nextLayer = {};
	  --all the previous layers together form the output
	  dominantWeights = dominantWeights | previousLayer;
	  for u in previousLayer do (
	       for r in posRoots do (
		    w := u - r;
		    --drop the non dominant or repeated weights
		    if (isDominant(R,w) and not member(w,dominantWeights)) then (
			 nextLayer=append(nextLayer,w);
			 );
		    );
	       );
	  --drop duplicate weights obtained in this pass
	  nextLayer = unique nextLayer;
	  );
     --sort the weights
     return sortLayer(R,dominantWeights);
     )

--INPUT
--R: a root system
--X: a set of simple roots of R
--w: a weight of R
--OUTPUT
--returns the stabilizer of w in the Weyl group of the sub root system
--corresponding to the simple roots in the set X
--[DeGraaf, p. 340]
weightStabilizer = (R,X,w) -> (
     e := select(elements X,i->scalarProduct(R,simpleRoot(R,i),w)==0);
     parabolic(R,set e)
     )

--INPUT
--R: root system
--T: a set of simple roots of R
--OUTPUT
--returns a list of special representatives xi for the orbits of the action
--of \hat{W_T} on the roots of R
--[Moody-Patera, Prop. 3]
specialOrbitRepresentatives = (R,T) -> (
     l := for r in elements(positiveRoots(R)) list (
	  skipRoot := false;
	  for i in elements(T) do (
	       if eval(R,r,i)<0 then (
		    skipRoot = true;
		    break;
		    );
	       );
	  if skipRoot==true then (
	       continue
	       );
	  r
	  );
     return l;
     )

--INPUT
--R: root system
--T: a set of simple roots of R
--xi: a positive root of R, xi should be the special representative for an
--orbit of the action of \hat{W_T} on R
--OUTPUT
--the cardinality of the orbit of xi under said action
--[Moody-Patera, Prop. 3]
--I call S the stabilizer of R in W_T (it's W_S in the paper)
--the index [W_T:W_S] is computed using the formula
--|W_T|/|W_S|=(|W|/|W_S|)/(|W|/|W_T|) and these cardinalities are computed
--using the poincareSeries function of WeylGroups
orbitSize = (R,T,xi) -> (
     x := local x;
     ZZ[x];
     sizeWmodT := sub(poincareSeries(R,T,x),x=>1);
     S := weightStabilizer(R,T,xi);
     sizeWmodS := sub(poincareSeries(R,S,x),x=>1);
     if isRoot(R,T,xi) then (
	  return (sizeWmodS//sizeWmodT);
	  )
     else (
	  return (2*sizeWmodS//sizeWmodT);
	  );
     )

--INPUT
--R: root system
--lambda: a dominant weight
--mu: a dominant weight
--orbits: a HashTable, the keys are the special representatives of the action
--of the stabilizer of mu on R and the values are the orbit sizes
--mult: a HashTable, the keys are dominant weights, the values are the
--multiplicities of those weights in the HW module with HW lambda
--OUTPUT
--the multiplicity of mu in the HW module with HW lambda
--this uses the modified Freudenthal's formula
--[DeGraaf, p. 339] or [Moody-Patera, eqn. 2]
freudenthal = (R,lambda,mu,orbits,mult) -> (
     num := 0;
     for gamma in keys(orbits) do (
	  tempSum := 0;
	  v := mu + gamma;
	  w := conjugateDominantWeight(R,v);
	  while (mult#?w) do (
	       tempSum = tempSum + scalarProduct(R,v,gamma)*mult#w;
	       v = v + gamma;
	       w = conjugateDominantWeight(R,v);
	       );
	  num = num + orbits#gamma*tempSum;
	  );
     den := plusRhoNorm(R,lambda)-plusRhoNorm(R,mu);
     return lift(num/den,ZZ);
     )

--INPUT
--R: root system
--lambda: a dominant weight of R
--OUTPUT
--the character of the HW module with HW lambda
--[DeGraaf, p. 339]
characterOfHWModule = method(TypicalValue => Tally)
characterOfHWModule (RootSystem,Weight) := Tally => (R,lambda) -> (
     if isDominant(R,lambda)==false then (
	  error "expected the second argument to be a dominant weight"
	  );
     L := dominantWeightsOfHWModule(R,lambda);
     --this hash table saves previously computed multiplicities
     mult := new MutableHashTable;
     mult#(lambda) = 1;
     stabilizers := new MutableHashTable;
     for nu in drop(L,1) do (
	  T := weightStabilizer(R,set(1..rank(R)),nu);
	  if not stabilizers#?T then (
	       x := specialOrbitRepresentatives(R,T);
	       y := apply(x,xi->orbitSize(R,T,xi));
	       stabilizers#T = hashTable pack(2,mingle{x,y});
	       );
	  mult#nu = freudenthal(R,lambda,nu,stabilizers#T,mult);
	  );
     return new Tally from mult;
     )

--------------------------------------------------------------------------------
----------FROM HERE ON I PUT FUNCTIONS NEEDED TO DECOMPOSE CHARACTERS-----------
--------------------------------------------------------------------------------
--INPUT
--z: an integer
--T: a VirtualTally
--OUTPUT
--returns a new VirtualTally with the same keys as T
--and values those of T multiplied by z
--COMMENT
--this is just a technical function defined to multiply a
--VirtualTally by an integer
--It's used in the next function
ZZ * VirtualTally := (z,T) -> new VirtualTally from apply(keys T,k->{k,z*T#k})

--INPUT
--R: root system
--T: a Tally of the dominant weights of a module over the Lie group of type R
--(i.e. T contains the dominant character as dom.weight => multiplicity)
--OUTPUT
--a Tally whose keys are the HW of the modules in the decomposition of T
--and the values are the multiplicities of those HW modules
--COMMENTS
--works by stripping one by one the characters of HW modules 
--appearing in the tally starting with the one that has the
--largest plusRhoNorm
decomposeCharacter = (R,T) -> (
     V := new VirtualTally from T;
     emptyVirtualTally := new VirtualTally from {};
     listOfHWTerms := while V =!= emptyVirtualTally list (
	  w := first sortLayer(R,keys V);
	  W := new VirtualTally from characterOfHWModule(R,w);
	  c := V#w;
	  V = V - c*W;
	  (w,c)
	  );
     return new Tally from hashTable(listOfHWTerms);
     )

--INPUT
--D: a Dynkin type
--L: a List of weights of R each written as a list of integers
--(they are converted into weights for WeylGroups internally)
--OUTPUT
--a Tally whose keys are the HW of the modules in the decomposition of L
--and the values are the multiplicities of those HW modules
--COMMENTS
--Unlike the function above this accepts weights as simple lists and converts
--them internally to the vector format of WeylGroups
--Also here non dominant weights are allowed (and automatically discarded)
--The weights are also returned as simple lists
--This is the only function that the main package will call from this file
decomposeWeightsList = (D,L) -> (
     R := rootSystem(D);
     W := apply(L,w->weight(R,w));
     T := tally select(W,w->isDominant(R,w));
     V := decomposeCharacter(R,T);
     dec := new Tally from applyPairs(V,(k,v)->(entries k,v));
     --check that multiplicities are non negative
     if any(values dec,i->i<0) then
     	 error"Decomposition returned a highest weight module with negative multiplicity! Please double check your weights.";
     return dec;
     )