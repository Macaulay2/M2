-- Copyright 2017-2018: Paolo Lella.
-- You may redistribute this file under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2
-- of the License, or any later version.

newPackage("SLnEquivariantMatrices",
           Version => "1.0", 
     	   Date => "April 10, 2018",
     	   Authors => {
	               {Name => "Ada Boralevi", 
			   Email => "ada.boralevi@polito.it",
			       HomePage => "http://www.adaboralevi.com"},
	               {Name => "Daniele Faenzi",
			   Email => "daniele.faenzi@u-bourgogne.fr",
			       HomePage => "http://dfaenzi.perso.math.cnrs.fr"},
		       {Name => "Paolo Lella",
			   Email => "paolo.lella@polimi.it",
			       HomePage => "http://www.paololella.it"}
		      },
	   Keywords => {"Representation Theory"},
	   PackageImports => {"Varieties"},
     	   Headline => "file ancillary to the paper \"A construction of equivariant bundles on the space of symmetric forms\""
     	  )
     
export {
        "slIrreducibleRepresentationsTensorProduct",
   	"slEquivariantConstantRankMatrix",
   	"slEquivariantVectorBundle",
	"sl2EquivariantConstantRankMatrix",
	"sl2EquivariantVectorBundle"
       }


--------------------------------------------------------------------------------
--
-- FUNCTIONS
--
--------------------------------------------------------------------------------
   
slIrreducibleRepresentationsTensorProduct = method(TypicalValue=>List)
slIrreducibleRepresentationsTensorProduct (ZZ,ZZ,ZZ) := (n,a,b) -> (
    
    if n <= 0 then error "\targument 1 : expected a positive integer";
    if a <= 0 then error "\targument 2 : expected a positive integer";
    if b <= 0 then error "\targument 3 : expected a positive integer";
    
    v := getSymbol "v";
    w := getSymbol "w";
    R := QQ(monoid[v_0..v_n]);
    
    return slIrreducibleRepresentationsTensorProduct (R,a,b);
)

slIrreducibleRepresentationsTensorProduct (PolynomialRing,ZZ,ZZ) := (R,a,b) -> (
    n := numgens R - 1;
    if n <= 0 then error "\targument 1 : expected a ring with at least 2 variables";
    if a <= 0 then error "\targument 2 : expected a positive integer";
    if b <= 0 then error "\targument 3 : expected a positive integer";
    
    RotimesR := R**R;
    L := getSymbol "L";
    C := QQ(monoid[L_0..L_n]);
    S := C[gens RotimesR];
        
    fromStoRotimesR := map(RotimesR,S,gens RotimesR);
    fromRotimesRtoS := map(S,RotimesR,gens S);   
    firstFactor := map(S,R,for i from 0 to n list S_i);
    secondFactor := map(S,R,for i from 0 to n list S_(n+i+1));
        
    Va := rsort flatten entries basis(a,R);
    Vb := rsort flatten entries basis(b,R);
    eigenValues := {};
    eigenSpaces := new MutableList from {};    
    for f in Va do
    (
    	for g in Vb do
	(
	    fg := (firstFactor f)*(secondFactor g);
	    e := leadCoefficient((slActionH fg)//fg);
	    p := position(eigenValues, x -> x == e);
	    if p === null then
	    (
	    	eigenValues = append(eigenValues,e);
		eigenSpaces = append(eigenSpaces,{fromStoRotimesR fg});	
	    )
	    else
	    (
	       eigenSpaces#p = append(eigenSpaces#p,fromStoRotimesR fg);
	    );
	);	
    );
    l := weightOrder (C,eigenValues);
    (eigenValues,eigenSpaces) = sortEigenspaces(eigenValues, eigenSpaces, l);
    dimensions := new MutableList from for e in eigenSpaces list #e;
    irreducible := {};
    maxWeightPos := 0;
    while maxWeightPos < #eigenSpaces do
    (
       maxWeightVector := {};
       currentEigenSpace := eigenSpaces#maxWeightPos;
       if (d := #currentEigenSpace) > 1 then
       (
       	   A := QQ[Variables=>d];
	   ARotimesR := A[gens RotimesR];
       	   fromAtoARotimesR := map(ARotimesR,A);
       	   fromRotimesRtoARotimesR := map(ARotimesR,RotimesR, gens ARotimesR);
       	   fromARotimesRtoRotimesR := map(RotimesR,ARotimesR, gens RotimesR);
       	   genericElement := sum(for i from 0 to d-1 list A_i*fromRotimesRtoARotimesR(currentEigenSpace#i));
	   toVanish := {};
	   for i from 0 to n-1 do
	   (
	       for j from i+1 to n do
	       (
		   toVanish = toVanish | for t in terms slActionE(n,i,j,genericElement) list leadCoefficient t;
	       );        
	   );
           toVanish = rsort (trim ideal toVanish)_*;
	   parameters := gens A;
	   eliminable := {};
	   for eq in toVanish do
	   (
	       parameters = delete(leadMonomial eq, parameters);
	       eliminable = append(eliminable, leadMonomial eq => (leadTerm eq-eq)/(leadCoefficient eq)); 
	   );
           genericElement = sub(genericElement,eliminable);
	   for par in parameters do
	   (
	       specializationMap := map(RotimesR,ARotimesR,gens RotimesR | (for a in gens A list if a == par then 1 else 0)); 
    	       maxWeightVector = append(maxWeightVector, (eigenValues#maxWeightPos,specializationMap genericElement));
	   );
       )
       else
       (
	   maxWeightVector = append(maxWeightVector, (eigenValues#maxWeightPos, currentEigenSpace#0));
       );
       	      
       for highestWeightVector in maxWeightVector do
       (
       	   newRepresentationWeights := {};
       	   newRepresentationBases := new MutableList from {};
	   queue := {highestWeightVector};
       	   control := 0;
       	   while #queue > 0 do
       	   (
       	       head := first queue;
	       queue = delete(head,queue);
	       vect := head#1;
	       weight := head#0;
	   
	       j := position(newRepresentationWeights, x -> x == weight);
	       pos := position(eigenValues, x -> x == weight);
	       if j === null then
	       (
	       	   newRepresentationWeights = append(newRepresentationWeights,weight);
	       	   newRepresentationBases = append(newRepresentationBases,{vect});
	       	   dimensions#pos = dimensions#pos-1;	      
	       )
               else
	       (
		   currentDim := numgens trim ideal (newRepresentationBases#j);
	       	   newDim := numgens trim (ideal (newRepresentationBases#j) + ideal(vect));
	       	   if newDim > currentDim then 
	       	   (
		       newRepresentationBases#j = append(newRepresentationBases#j,vect);
		       dimensions#pos = dimensions#pos-1;
	       	   );
	       );
	   
	       for i from 0 to n-1 do
	       (
	       	   for j from i+1 to n do
	       	   (
		       newWeight := weight + C_j - C_i;
		       if (newV := slActionE(n,j,i,vect)) != 0 then
		       (
		       	   newPair := (newWeight,(trim ideal newV)_0);
			   if not member(newPair,queue) then queue = append(queue, newPair);	   
		       );
	       	   );    
	       );
       	   ); 
       	   (newRepresentationWeights,newRepresentationBases) = sortEigenspaces(newRepresentationWeights,newRepresentationBases,l);
           irreducible = append(irreducible,flatten for i from 0 to #newRepresentationBases-1 list rsort flatten entries gens trim ideal newRepresentationBases#i);
	);
	while maxWeightPos < #eigenSpaces and dimensions#maxWeightPos == 0 do maxWeightPos=maxWeightPos+1;       
    	
    );    
    return irreducible;
)

slEquivariantConstantRankMatrix = method(TypicalValue => Matrix, Options => {CoefficientRing => QQ})
slEquivariantConstantRankMatrix (ZZ,ZZ,ZZ) := opts -> (n,d,m) -> (
    if n <= 0 then error "\targument 1 : expected a positive integer";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected a integer greater than 1";
    
    v := getSymbol "v";
    R := QQ(monoid[v_0..v_n]);

    return slEquivariantConstantRankMatrix(R,d,m,CoefficientRing=>opts.CoefficientRing);
)

slEquivariantConstantRankMatrix (PolynomialRing,ZZ,ZZ) := opts -> (R,d,m) -> (
    n := numgens R - 1;
    if n <= 0 then error "\targument 1 : expected a ring with at least 2 variables";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected a integer greater than 1";
    
    RotimesR := QQ(monoid[gens (R**R)]);
    firstFactor := map(RotimesR,R, for i from 0 to n list RotimesR_i);
    projFirstFactor := map(R,RotimesR, gens R | toList(n+1:0));
    secondFactor := map(RotimesR,R, for i from 0 to n list RotimesR_(n+i+1));
    V := rsort flatten entries basis(d,R);
    W := rsort flatten entries basis((m-1)*d,R); 
    
    L := getSymbol "L";
    C := QQ(monoid[L_0..L_n]);
       
    highestWeight := (m*d-1)*C_0 + C_1;
    highestWeightVector := (firstFactor V#0)*(secondFactor W#1) - (firstFactor V#1)*(secondFactor W#0);
    irreducibleRepresentationWeights := {};
    irreducibleRepresentationVectors := new MutableList from {};
    queue := {(highestWeight,highestWeightVector)};
    while #queue > 0 do
    (
       	head := first queue;
	queue = delete(head,queue);
	vect := head#1;
	weight := head#0;
	   
	j := position(irreducibleRepresentationWeights, x -> x == weight);
	if j === null then
	(
	    irreducibleRepresentationWeights = append(irreducibleRepresentationWeights,weight);
	    irreducibleRepresentationVectors = append(irreducibleRepresentationVectors,{vect});
       	)
        else
	(
	    currentDim := numgens trim ideal (irreducibleRepresentationVectors#j);
	    newDim := numgens trim (ideal (irreducibleRepresentationVectors#j) + ideal(vect));
	    if newDim > currentDim then 
	    (
	       	irreducibleRepresentationVectors#j = append(irreducibleRepresentationVectors#j,vect);
	    );
        );
	   
	for i from 0 to n-1 do
	(
	    for j from i+1 to n do
	    (
	    	newWeight := weight + C_j - C_i;
		if (newV := slActionE(n,j,i,vect)) != 0 then
		(
		    newPair := (newWeight,(trim ideal newV)_0);
		    if not member(newPair,queue) then queue = append(queue, newPair);	   
		);
	    );    
	);
    ); 
    (irreducibleRepresentationWeights,irreducibleRepresentationVectors) = sortEigenspaces(irreducibleRepresentationWeights,irreducibleRepresentationVectors,weightOrder (C,irreducibleRepresentationWeights));
    irreducibleRepresentationBasis := flatten for i from 0 to #irreducibleRepresentationVectors-1 list irreducibleRepresentationVectors#i;
    
    N := #V-1;
    x := getSymbol "x";
    X := QQ(monoid[x_0..x_N]);
     
    M := {};
    for vw in irreducibleRepresentationBasis do
    (
       column := {};
       for w in W do
       (
	   c := vw//(secondFactor w);
	   if c == 0 then
	   (
	       column = column | {0_X};	   
	   )	 
           else
	   (
	       column = column | {(leadCoefficient c)*X_(position(V, x-> x == (projFirstFactor leadMonomial c)))};	   
	   );	
       );
       M = M | {column};	    
   );
   
   M = transpose matrix M;
   if opts.CoefficientRing =!= QQ then
   (
       newX := opts.CoefficientRing(monoid[gens X]);
       phi := map(newX,X);
       M = matrix for i in entries M list for j in i list phi j; 
       M = M**newX^{1};   
   );

   return M;
)

slEquivariantConstantRankMatrix (ZZ,ZZ,ZZ,PolynomialRing) := opts -> (n,d,m,X) -> (
    if n <= 0 then error "\targument 1 : expected a positive integer";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected a integer greater than 1";
    if numgens X != (N := binomial(n+d,n)) then error ("\targument 4 : expected a polynomial ring with " | toString(N) | " variables");

    M := slEquivariantConstantRankMatrix (n,d,m);
    phi := map(X,ring M,gens X);
    return (matrix for r in entries M list for c in r list phi c)**X^{1};
)

slEquivariantConstantRankMatrix (PolynomialRing,ZZ,ZZ,PolynomialRing) := opts -> (R,d,m,X) -> (
    n := numgens R - 1;
    if n <= 0 then error "\targument 1 : expected a ring with at least 2 variables";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected a integer greater than 1";
    if numgens X != (N := binomial(n+d,n)) then error ("\targument 4 : expected a polynomial ring with " | toString(N) | " variables");

    M := slEquivariantConstantRankMatrix (R,d,m);
    phi := map(X,ring M,gens X);
    return (matrix for r in entries M list for c in r list phi c)**X^{1};
)

slEquivariantVectorBundle = method(TypicalValue => CoherentSheaf, Options => {CoefficientRing => QQ})
slEquivariantVectorBundle (ZZ,ZZ,ZZ) := opts -> (n,d,m) -> (
    if n <= 0 then error "\targument 1 : expected a positive integer";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected a integer greater than 1";
    
    return sheaf minimalPresentation ker slEquivariantConstantRankMatrix(n,d,m,CoefficientRing=>opts.CoefficientRing);  
)

slEquivariantVectorBundle (PolynomialRing,ZZ,ZZ) := opts -> (R,d,m) -> (
    n := numgens R - 1;
    if n <= 0 then error "\targument 1 : expected a ring with at least 2 variables";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected a integer greater than 1";
    
    return sheaf minimalPresentation ker slEquivariantConstantRankMatrix(R,d,m,CoefficientRing=>opts.CoefficientRing);  
)

slEquivariantVectorBundle (ZZ,ZZ,ZZ,PolynomialRing) := opts -> (n,d,m,X) -> (
    if n <= 0 then error "\targument 1 : expected a positive integer";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected an integer greater than 1";
    if numgens X != (N := binomial(n+d,n)) then error ("\targument 4 : expected a polynomial ring with " | toString(N) | " variables");

    return sheaf minimalPresentation ker slEquivariantConstantRankMatrix(n,d,m,X);  
)

slEquivariantVectorBundle (PolynomialRing,ZZ,ZZ,PolynomialRing) := opts -> (R,d,m,X) -> (
    n := numgens R - 1;
    if n <= 0 then error "\targument 1 : expected a ring with at least 2 variables";
    if d <= 0 then error "\targument 2 : expected a positive integer";
    if m <= 1 then error "\targument 3 : expected an integer greater than 1";
    if numgens X != (N := binomial(n+d,n)) then error ("\targument 4 : expected a polynomial ring with " | toString(N) | " variables");

    return sheaf minimalPresentation ker slEquivariantConstantRankMatrix(R,d,m,X);  
)

sl2EquivariantConstantRankMatrix = method(TypicalValue => Matrix, Options => {CoefficientRing => QQ})
sl2EquivariantConstantRankMatrix (ZZ,ZZ) := opts -> (d,m) -> (
    if d <= 0 then error "\targument 1 : expected a positive integer";
    if m <= 1 then error "\targument 2 : expected a integer greater than 1";
    
    x := getSymbol "x";
    R := opts.CoefficientRing(monoid[x_0..x_d]);

    return sl2EquivariantConstantRankMatrix(R,m);
)

sl2EquivariantConstantRankMatrix (PolynomialRing,ZZ) := opts -> (R,m) -> (
    if m <= 1 then error "\targument 2 : expected a integer greater than 1";
    d := numgens R - 1;
    
    mutableM := mutableMatrix(R,(m-1)*d+1,m*d-1);
    for j from 0 to m*d-2 do 
    (
	coeff := for i from max(0,j+1-d) to min(j+1,(m-1)*d) list matrixCoefficient(i+1,j+1,d,m);
	gcdCoeff := gcd coeff;
	h := 0;
	for i from max(0,j+1-d) to min(j+1,(m-1)*d) do
	(
	    mutableM_(i,j) = (coeff#h//gcdCoeff)*R_(j-i+1);
	    h = h+1;
	);	
    );

    return (matrix mutableM)**R^{-1};
)

sl2EquivariantVectorBundle = method(TypicalValue => CoherentSheaf, Options => {CoefficientRing => QQ})
sl2EquivariantVectorBundle (ZZ,ZZ) := opts -> (d,m) -> (
    if d <= 0 then error "\targument 1 : expected a positive integer";
    if m <= 1 then error "\targument 2 : expected a integer greater than 1";

    return sheaf minimalPresentation ker sl2EquivariantConstantRankMatrix (d,m,CoefficientRing => opts.CoefficientRing);    
)

sl2EquivariantVectorBundle (PolynomialRing,ZZ) := opts -> (R,m) -> (
    if m <= 1 then error "\targument 2 : expected a integer greater than 1";  
    
    return sheaf minimalPresentation ker sl2EquivariantConstantRankMatrix(R,m);
)

--------------------------------------------------------------------------------
--
-- AUXILIARY FUNCTIONS (unexported)
--
--------------------------------------------------------------------------------
slActionE = method(TypicalValue=>RingElement)
slActionE (ZZ,ZZ,ZZ,RingElement) := (n,i,j,v) -> (
    R := ring v;
    if first degree v == 0 then
    (
	return 1_R; 	  
    )
    else
    (
	return sum(for t in terms v list R_i*(diff_(R_j) t) + R_(n+i+1)*(diff_(R_(n+j+1)) t) );
    );
)


slActionH = method(TypicalValue=>RingElement)
slActionH (ZZ,ZZ,ZZ,RingElement) := (n,i,j,v) ->  (
    return slActionE(n,j,i,slActionE(n,i,j,v)) - slActionE(n,i,j,slActionE(n,j,i,v));
)

slActionH RingElement := v -> (
    R := ring v;
    C := coefficientRing R;
    n := numgens C - 1;
    
    if first degree v == 0 then
    (
        return 1_R; 	  
    )
    else
    (
    	return sum (for t in terms v list (sum (for i from 0 to n list (degree(R_i,t)+degree(R_(i+n+1),t))*C_i))*t);	
    ); 
)

weightOrder = method(TypicalValue => RingMap, Options => {Height=>8})
weightOrder (PolynomialRing,List) := opts -> (C,L) -> (
    
    n := numgens C;
    if n == 2 then return map(QQ,C,{1,-1});
    
    W := rsort for i from 1 to n-1 list random(QQ,Height=>opts.Height);
    if #(unique W) < #W then return weightOrder(C,L,Height=>opts.Height);
    
    wn := -sum W;
    W = W | {wn};
    
    eval := map(QQ,C,W);
    evalL := unique for l in L list eval l;
    if #evalL < #L then return weightOrder(C,L,Height=>2*opts.Height);
    
    return eval;
)

sortEigenspaces = method(TypicalValue => Sequence)
sortEigenspaces (List, MutableList, RingMap) := (eigenValues,eigenSpaces,eval) -> (
    
    orderedEigenValues := {};
    orderedEigenSpaces := {};
    for i from 0 to #eigenValues-1 do
    (
    	pos := 0;
	while pos < #orderedEigenValues and eval eigenValues#i < eval orderedEigenValues#pos do pos = pos+1;
	orderedEigenValues = insert(pos,eigenValues#i,orderedEigenValues);	
	orderedEigenSpaces = insert(pos,eigenSpaces#i,orderedEigenSpaces);	
    );
    return (orderedEigenValues,orderedEigenSpaces);
)

sortEigenspaces (List, List, RingMap) := (eigenValues,eigenSpaces,eval) -> (
    
    orderedEigenValues := {};
    orderedEigenSpaces := {};
    for i from 0 to #eigenValues-1 do
    (
    	pos := 0;
	while pos < #orderedEigenValues and eval eigenValues#i < eval orderedEigenValues#pos do pos = pos+1;
	orderedEigenValues = insert(pos,eigenValues#i,orderedEigenValues);	
	orderedEigenSpaces = insert(pos,eigenSpaces#i,orderedEigenSpaces);	
    );
    return (orderedEigenValues,orderedEigenSpaces);
)


matrixCoefficient = method(TypicalValue => ZZ)
matrixCoefficient(ZZ,ZZ,ZZ,ZZ) := (i,j,d,m) -> (

    if i == 1 then return -product(for k from 2 to j list d-k+1);
    
    if j > ceiling((m*d-1)/2) then return -matrixCoefficient((m-1)*d+2-i,m*d-j,d,m);
      
    output := -product(for k from max(1,j-d) to i-2 list (m-1)*d-k);
    output = output*product(for k from 1 to j-i+1 list d-k+1);
    output = output*(binomial(j-1,i-1)*m - binomial(j,i-1));
    return output;        
)

--****************************************************************************--
-- DOCUMENTATION FOR PACKAGE
--****************************************************************************--
beginDocumentation()

doc ///
    Key
    	SLnEquivariantMatrices
    Headline
    	Ancillary file to the paper "A construction of equivariant bundles on the space of symmetric forms"
    Description
    	Text
	    In the paper "A construction of equivariant bundles on the space of symmetric forms" (@HREF "https://arxiv.org"@),
	    the authors construct stable vector bundles on the space $\PP(S^d\CC^{n+1})$ of symmetric forms of degree $d$
	    in $n + 1$ variables which are equivariant for the action of $SL_{n+1}(\CC)$ ,and admit an equivariant free
	    resolution of length 2.
	    
	    Take two integers $d \ge 1$ and $m \ge 2$ and a vector spave $V = \CC^{n+1}$. For $n=2$, we have
	    
	    $S^dV \otimes S^{(m-1)d}V = S^{md}V \oplus S^{md-2}V \oplus S^{md-4}V \oplus \dots$,
	     
	    while for $n > 1$,
	    
	    $S^dV \otimes S^{(m-1)d}V = S^{md}V \oplus V_{(md-2)\lambda_1+\lambda_2} \oplus V_{(md-4)\lambda_1+2\lambda_2} \oplus \dots$,
		
	    where $\lambda_1$ and $\lambda_2$ are the two greatest fundamental weights of the Lie group $SL_{n+1}(\bf C)$ and $V_{i\lambda_1+j\lambda_2}$ is the irreducible representation of highest weight $i\lambda_1+j\lambda_2$.
	    
	    The projection of the tensor product onto the second summand induces a $SL_{2}(\CC)$-equivariant morphism
	    
	    $\Phi: S^{md-2}V \otimes O_{\PP(S^dV)} \to S^{(m-1)d}V \otimes O_{\PP(S^dV)}(1)$
	    
	    or a $SL_{n+1}(\CC)$-equivariant morphism
	    
	    $\Phi: V_{(md-2)\lambda_1 + \lambda_2} \otimes O_{\PP(S^dV)} \to S^{(m-1)d}V \otimes O_{\PP(S^dV)}(1)$
	    
	    with constant co-rank 1, and thus gives an exact sequence of vector bundles on $\PP(S^dV)$:
	    	    
	    $0 \to W_{2,d,m} \to S^{md-2}V \otimes \mathcal{O}_{\PP(S^dV)} \to S^{(m-1)d}V \otimes \mathcal{O}_{\PP(S^dV)}(1) \to \mathcal{O}_{\PP(S^dV)}(m) \to 0$,
	    
	    $0 \to W_{n,d,m} \to V_{(md-2)\lambda_1 + \lambda_2} \otimes \mathcal{O}_{\PP(S^dV)} \to S^{(m-1)d}V \otimes \mathcal{O}_{\PP(S^dV)}(1) \to \mathcal{O}_{\PP(S^dV)}(m) \to 0$.
	    
	    The package allows to compute 
	    
	    (1) the decomposition into irreducible $SL_{n+1}(\bf C)$-representations of the
	    tensor product of two symmetric powers $S^a\CC^{n+1}$ and $S^b\CC^{n+1}$;
	    
	    (2) the matrix representing the morphism $\Phi$;	    
	    
	    (3) the vector bundle $W_{n,d,m}$.
///

doc /// 
    Key
	slIrreducibleRepresentationsTensorProduct
	(slIrreducibleRepresentationsTensorProduct, ZZ, ZZ, ZZ)
	(slIrreducibleRepresentationsTensorProduct, PolynomialRing, ZZ, ZZ)
    Headline 
    	computes the irreducible SL-subrepresentations of the tensor product of two symmetric products
    Usage
    	D = slIrreducibleRepresentationsTensorProduct(n,a,b)
	D = slIrreducibleRepresentationsTensorProduct(R,a,b)	 
    Inputs
    	n:ZZ
	    positive
	R:PolynomialRing 	    
	a:ZZ
	    positive
	b:ZZ
	    positive
    Outputs 
    	D:List
	    of irreducible representations
    Description
        Text 
	    This function computes the decomposition in irreducible $SL(n+1)$-representations of the tensor product $S^aV \otimes S^bV$, where $V = <v_0,\ldots,v_n>$ and $a \leq b$.
	    
	    If $n = 1$, the decomposition is
	    
	    $S^aV \otimes S^bV = S^{a+b}V \oplus S^{a+b-2}V \oplus S^{a+b-4}V \oplus \dots \oplus S^{b-a}V$,
	    
	    while if $n > 1$, the decomposition is
	    
	    $S^aV \otimes S^bV = S^{a+b}V \oplus V_{(a+b-2)\lambda_1 + \lambda_2} \oplus V_{(a+b-4)\lambda_1 + 2\lambda_2} \oplus \dots \oplus V_{(b-a)\lambda_1 + a\lambda_2}$,
	    
	    where $\lambda_1$ and $\lambda_2$ are the two greatest fundamental weights of the Lie group $SL(n+1)$ and $V_{i\lambda_1+j\lambda_2}$ is the irreducible representation of highest weight $i\lambda_1+j\lambda_2$.
	Example
	    n = 2
	    a = 1, b = 2
	    D = slIrreducibleRepresentationsTensorProduct(n,a,b);
	    #D
	    D#0
	    D#1
	Text
    	    If a polynomial ring {\tt R} is given, then {\tt n = numgens R - 1} and $V = <R_0,\ldots,R_n>$.
	Example
	    R = QQ[x_0,x_1,x_2]; 
	    a = 2, b = 3
	    D = slIrreducibleRepresentationsTensorProduct(R,a,b);
	    #D
	    D#0
	    D#1
	    D#2
///

doc /// 
    Key
   	slEquivariantConstantRankMatrix
	(slEquivariantConstantRankMatrix, ZZ, ZZ, ZZ)
	(slEquivariantConstantRankMatrix, PolynomialRing, ZZ, ZZ)
	(slEquivariantConstantRankMatrix, ZZ, ZZ, ZZ, PolynomialRing)
	(slEquivariantConstantRankMatrix, PolynomialRing, ZZ, ZZ, PolynomialRing) 
    Headline 
    	computes a SL-equivariant constant rank matrix
    Usage
    	M = slEquivariantConstantRankMatrix(n,d,m)
	M = slEquivariantConstantRankMatrix(n,d,m,CoefficientRing=>C)
    	M = slEquivariantConstantRankMatrix(R,d,m)
	M = slEquivariantConstantRankMatrix(n,d,m,X)
	M = slEquivariantConstantRankMatrix(R,d,m,X)
    Inputs
    	n:ZZ
	    positive
	R:PolynomialRing 
	    with at least two variables
	d:ZZ
	    positive
	m:ZZ
	    at least 2
	X:PolynomialRing 
	    with {\tt binomial(n+d,n)} or {\tt binomial(numgens R-1+d,numgens R-1)} variables
	C:Ring 
    Outputs  
    	M:Matrix  
    Description
        Text 
	    This function returns a constant rank matrix of linear forms. For $n=1$, the matrix describes the morphism
	    
	    $\Phi: S^{md-2}V \otimes O_{\PP^d} \to S^{(m-1)d}V \otimes O_{\PP^d)}(1)$
	    
	    given by the projection
	    
	    $S^dV \otimes S^{(m-1)d}V \to S^{md-2}V$
	    
	    of the irreducible $SL(2)$-subrepresentation of highest weight $md-2$, where $\PP^d = \PP(S^dV)$ as $V=<v_0,v_1>$.
	
	    For $n>1$, the matrix describes the morphism	    
	    
	    $\Phi: V_{(md-2)\lambda_1 + \lambda_2} \otimes O_{\PP(S^dV)} \to S^{(m-1)d}V \otimes O_{\PP(S^dV)}(1)$
	    
	    given by the projection
	    
	    $S^dV \otimes S^{(m-1)d}V \to V_{(md-2)\lambda_1 + \lambda_2}$
	    
	    of the irreducible $SL(n+1)$-subrepresentation $V_{(md-2)\lambda_1 + \lambda_2}$ of highest weight 
	    $(md-2)\lambda_1 + \lambda_2 = (md-1)L_1 + L_2$ in the tensor product $S^dV \otimes S^{(m-1)d}V$, where $V = \CC^{n+1}$ and
	    $\lambda_1$ and $\lambda_2$ are the two greatest fundamental weights of the Lie group $SL(n+1)$.
	Example
	    n = 1, d = 3, m = 3 
	    M = slEquivariantConstantRankMatrix(n,d,m)
	Text
	    By default, @TO slEquivariantConstantRankMatrix@ defines the matrix over a polynomial ring with rational coefficients. 
	    The optional argument @TO CoefficientRing@ allows one to change the coefficient ring.
	Example
	    n = 1, d = 3, m = 3 
	    M = slEquivariantConstantRankMatrix(n,d,m,CoefficientRing=>ZZ/10007)
	Text
	    If the first argument is a polynomial ring {\tt R}, then {\tt n = numgens R-1}.
	Example
	    R = QQ[y_0,y_1];
	    d = 2, m = 3 
	    M = slEquivariantConstantRankMatrix(R,d,m)
	Text
	    If the last argument is polynomial ring {\tt X} (and {\tt X} has the same number of variables of the coordinate ring of $\PP(S^d\CC^{n+1})$),
	    then the matrix is defined over the polynomial ring {\tt X}.
	Example
	    n = 1, d = 3, m = 3 
	    X = ZZ/7[z_0,z_1,z_2,z_3];
	    M = slEquivariantConstantRankMatrix(n,d,m,X)
	    R = QQ[y_0,y_1];
	    d = 3, m = 2 
	    M = slEquivariantConstantRankMatrix(R,d,m,X)
///

doc /// 
    Key
    	slEquivariantVectorBundle
	(slEquivariantVectorBundle, ZZ, ZZ, ZZ)
	(slEquivariantVectorBundle, ZZ, ZZ, ZZ, PolynomialRing) 
	(slEquivariantVectorBundle, PolynomialRing, ZZ, ZZ)
	(slEquivariantVectorBundle, PolynomialRing, ZZ, ZZ, PolynomialRing) 
    Headline 
    	computes a SL-equivariant vector bundle over some projective space
    Usage
    	W = slEquivariantVectorBundle(n,d,m)
	W = slEquivariantVectorBundle(n,d,m,CoefficientRing=>C)
    	W = slEquivariantVectorBundle(R,d,m)
    	W = slEquivariantVectorBundle(n,d,m,X)
    	W = slEquivariantVectorBundle(R,d,m,X)
    Inputs
    	n:ZZ
	    positive
	R:PolynomialRing 
	    with at least two variables
	d:ZZ
	    positive
	m:ZZ
	    at least 2
	X:PolynomialRing 
	    with {\tt binomial(n+d,n)} or {\tt binomial(numgens R-1+d,numgens R-1)} variables
	C:Ring 
    Outputs 
    	W:CoherentSheaf
	    vector bundle
    Description
        Text 
    	    For $n=1$, this function returns the kernel of the matrix describing the morphism
	    
	    $\Phi: S^{md-2}V \otimes O_{\PP^d} \to S^{(m-1)d}V \otimes O_{\PP^d)}(1)$
	    
	    given by the projection
	    
	    $S^dV \otimes S^{(m-1)d}V \to S^{md-2}V$
	    
	    of the irreducible $SL(2)$-subrepresentation of highest weight $md-2$, where $\PP^d = \PP(S^dV)$ as $V=<v_0,v_1>$,
	    while for $n>1$, the function returns the kernel of the matrix describing the morphism   
	    
	    $\Phi: V_{(md-2)\lambda_1 + \lambda_2} \otimes O_{\PP(S^dV)} \to S^{(m-1)d}V \otimes O_{\PP(S^dV)}(1)$
	    
	    given by the projection
	    
	    $S^dV \otimes S^{(m-1)d}V \to V_{(md-2)\lambda_1 + \lambda_2}$
	    
	    of the irreducible $SL(n+1)$-subrepresentation $V_{(md-2)\lambda_1 + \lambda_2}$ of highest weight 
	    $(md-2)\lambda_1 + \lambda_2 = (md-1)L_1 + L_2$ in the tensor product $S^dV \otimes S^{(m-1)d}V$, where $V = \CC^{n+1}$ and
	    $\lambda_1$ and $\lambda_2$ are the two greatest fundamental weights of the Lie group $SL(n+1)$. 
	    
	    In the paper {\em A construction of equivariant bundles on the space of symmetric forms}, it is proved that the matrix $\Phi$ has constant co-rank 1, 
	    so that the kernel $W = ker \Phi$ turns out to be a vector bundle.	
	Example
	    n = 1, d = 3, m = 3 
	    W = slEquivariantVectorBundle(n,d,m)
	Text
	    By default, @TO slEquivariantVectorBundle@ defines the vector bundle over a projective space whose coordinate ring has rational coefficients. 
	    The optional argument @TO CoefficientRing@ allows one to change the coefficient ring.
	Example
	    n = 1, d = 3, m = 3 
	    W = slEquivariantVectorBundle(n,d,m,CoefficientRing=>ZZ/10007)
	Text
	    If the first argument is a polynomial ring {\tt R}, then {\tt n = numgens R-1}.
	Example
	    R = QQ[y_0,y_1];
	    d = 2, m = 3 
	    W = slEquivariantVectorBundle(R,d,m)
	Text
	    If the last argument is polynomial ring {\tt X} (and {\tt X} has the same number of variables of the coordinate ring of $\PP(S^d\CC^{n+1})$),
	    then the vector bundle is defined over the projective space {\tt Proj(X)}.
	Example
	    n = 1, d = 3, m = 3 
	    X = ZZ/7[z_0,z_1,z_2,z_3];
	    W = slEquivariantVectorBundle(n,d,m,X)
	    R = QQ[y_0,y_1];
	    d = 3, m = 2 
	    W = slEquivariantVectorBundle(R,d,m,X)
///

doc /// 
    Key
   	sl2EquivariantConstantRankMatrix
	(sl2EquivariantConstantRankMatrix, ZZ, ZZ)
	(sl2EquivariantConstantRankMatrix, PolynomialRing, ZZ)
    Headline 
    	computes a SL(2)-equivariant constant rank matrix
    Usage
    	M = sl2EquivariantConstantRankMatrix(d,m)
	M = sl2EquivariantConstantRankMatrix(d,m,CoefficientRing=>C)
    	M = sl2EquivariantConstantRankMatrix(R,m)
    Inputs
	R:PolynomialRing 
	    with at least two variables
	d:ZZ
	    positive
	m:ZZ
	    at least 2
	C:Ring 
    Outputs  
    	M:Matrix  
    Description
        Text 
	    This function returns a constant rank matrix of linear forms. The matrix describes the morphism
	    
	    $\Phi: S^{md-2}V \otimes O_{\PP^d} \to S^{(m-1)d}V \otimes O_{\PP^d)}(1)$
	    
	    given by the projection
	    
	    $S^dV \otimes S^{(m-1)d}V \to S^{md-2}V$
	    
	    of the irreducible $SL(2)$-subrepresentation of highest weight $md-2$, where $\PP^d = \PP(S^dV)$ as $V=<v_0,v_1>$.
    	    In the paper {\em A construction of equivariant bundles on the space of symmetric forms}, the entries of the matrix $\Phi$ 
	    are explicitly described.
	Example
	    d = 4, m = 3 
	    M = sl2EquivariantConstantRankMatrix(d,m)
	Text
	    By default, @TO sl2EquivariantConstantRankMatrix@ defines the matrix over a polynomial ring with rational coefficients. 
	    The optional argument @TO CoefficientRing@ allows one to change the coefficient ring.
	Example
	    d = 4, m = 3 
	    M = sl2EquivariantConstantRankMatrix(d,m,CoefficientRing=>ZZ/10007)
	Text
	    If the first argument is a polynomial ring {\tt R}, then {\tt d = numgens R-1}.
	Example
	    R = QQ[y_0..y_4];
	    m = 3 
	    M = sl2EquivariantConstantRankMatrix(R,m)
///

doc /// 
    Key
    	sl2EquivariantVectorBundle
	(sl2EquivariantVectorBundle, ZZ, ZZ)
	(sl2EquivariantVectorBundle, PolynomialRing, ZZ)
    Headline 
    	computes a SL(2)-equivariant vector bundle over some projective space
    Usage
    	W = sl2EquivariantVectorBundle(d,m)
	W = sl2EquivariantVectorBundle(d,m,CoefficientRing=>C)
    	W = sl2EquivariantVectorBundle(R,m)
    Inputs
	R:PolynomialRing 
	    with at least two variables
	d:ZZ
	    positive
	m:ZZ
	    at least 2
	C:Ring 
    Outputs 
    	W:CoherentSheaf
	    vector bundle
    Description
        Text 
    	    This function returns the kernel of the matrix describing the morphism
	    
	    $\Phi: S^{md-2}V \otimes O_{\PP^d} \to S^{(m-1)d}V \otimes O_{\PP^d)}(1)$
	    
	    given by the projection
	    
	    $S^dV \otimes S^{(m-1)d}V \to S^{md-2}V$
	    
	    of the irreducible $SL(2)$-subrepresentation of highest weight $md-2$, where $\PP^d = \PP(S^dV)$ as $V=<v_0,v_1>$.
	    
	    In the paper {\em A construction of equivariant bundles on the space of symmetric forms}, it is proved that the matrix $\Phi$ has constant co-rank 1, 
	    so that the kernel $W = ker \Phi$ turns out to be a vector bundle, and the entries of the matrix $\Phi$ are explicitly described.	
	Example
	    d = 3, m = 2 
	    W = sl2EquivariantVectorBundle(d,m)
	Text
	    By default, @TO slEquivariantVectorBundle@ defines the vector bundle over a projective space whose coordinate ring has rational coefficients. 
	    The optional argument @TO CoefficientRing@ allows one to change the coefficient ring.
	Example
	    d = 3, m = 2 
	    W = sl2EquivariantVectorBundle(d,m,CoefficientRing=>ZZ/10007)
	Text
	    If the first argument is a polynomial ring {\tt R}, then {\tt d = numgens R-1}.
	Example
	    R = QQ[y_0..y_3];
	    m = 2
	    W = sl2EquivariantVectorBundle(R,m)
///

doc ///
    Key
        [slEquivariantConstantRankMatrix,CoefficientRing]
    Headline
        name for optional argument
    Usage
	M = slEquivariantConstantRankMatrix(n,m,d,CoefficientRing=>C)
    Description
      	Text
       	    This is an option to tell @TO slEquivariantConstantRankMatrix@ to define the matrix
	    over a polynomial ring with coefficients in the ring {\tt C}.
///

doc ///
    Key
        [slEquivariantVectorBundle,CoefficientRing]
    Headline
        name for optional argument
    Usage
	W = slEquivariantVectorBundle(n,m,d,CoefficientRing=>C)
    Description
      	Text
       	    This is an option to tell @TO slEquivariantVectorBundle@ to define the vector bundle
	    over a projective space whose coordinate ring has coefficients in the ring {\tt C}.
///

doc ///
    Key
        [sl2EquivariantConstantRankMatrix,CoefficientRing]
    Headline
        name for optional argument
    Usage
	M = sl2EquivariantConstantRankMatrix(m,d,CoefficientRing=>C)
    Description
      	Text
       	    This is an option to tell @TO slEquivariantConstantRankMatrix@ to define the matrix
	    over a polynomial ring with coefficients in the ring {\tt C}.
///

doc ///
    Key
        [sl2EquivariantVectorBundle,CoefficientRing]
    Headline
        name for optional argument
    Usage
	W = sl2EquivariantVectorBundle(m,d,CoefficientRing=>C)
    Description
      	Text
       	    This is an option to tell @TO slEquivariantVectorBundle@ to define the vector bundle
	    over a projective space whose coordinate ring has coefficients in the ring {\tt C}.
///

--****************************************************************************--
-- TESTS FOR PACKAGE
--****************************************************************************--
TEST ///
n = 1, a = 3, b = 6
D = slIrreducibleRepresentationsTensorProduct(n,a,b);
assert(#D == a+1)
///

TEST ///
n = 1, a = 2, b = 4
D = slIrreducibleRepresentationsTensorProduct(n,a,b);
assert(#(D#0) == a+b+1)
assert(#(D#1) == a+b-1)
///

TEST ///
n = 2, d = 3, m = 2
M = slEquivariantConstantRankMatrix(n,d,m);
assert(rank M == numRows M - 1)
///

TEST ///
n = 1, d = 3, m = 3
W = slEquivariantVectorBundle(n,d,m);
assert(rank W == d-1)
///

TEST ///
d = 4, m = 3 
M = sl2EquivariantConstantRankMatrix(d,m);
assert(rank M == numRows M - 1)
///

TEST ///
d = 4, m = 3
W = sl2EquivariantVectorBundle(d,m);
assert(rank W == d-1)
///

--restart
--installPackage "SLnEquivariantMatrices"
--loadPackage "SLnEquivariantMatrices"
--viewHelp

