-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-


DiagonalAction = new Type of GroupAction

-------------------------------------------
--- DiagonalAction methods -------------------
-------------------------------------------

diagonalAction = method()

diagonalAction (Matrix, Matrix, List, PolynomialRing) := DiagonalAction => (W1, W2, d, R) -> (
    if not isField coefficientRing R then (
	error "diagonalAction: Expected the last argument to be a polynomial ring over a field."
	);
    if ring W1 =!= ZZ or ring W2 =!= ZZ then (
	error "diagonalAction: Expected the first and second arguments to be matrices of integer weights."
	);
    if numColumns W1 =!= dim R or numColumns W2 =!= dim R then (
	error "diagonalAction: Expected the number of columns of each matrix to equal the dimension of the polynomial ring."
	);
    if numRows W2 =!= #d then (
	error "diagonalAction: Expected the number of rows of the second argument to equal the size of the list."
	);
    if any(d, j -> not instance(j, ZZ) or j <= 0) then (
	error "diagonalAction: Expected the second argument to be a list of positive integers."
	);
    p := char R;
    if p > 0 and any(d, j -> j%p == 0) then (
	error "diagonalAction: Diagonal action is not defined when the characteristic divides the order of one of the cyclic factors."
	);    
    r := numRows W1;
    g := numRows W2;
    z := getSymbol "z";
    C := ZZ[Variables=> r + g,VariableBaseName=>z,
	MonomialOrder=> {GroupLex => r,GroupLex => g},
	Inverses=>true];
    new DiagonalAction from {
	cache => new CacheTable,
	(symbol cyclicFactors) => d,
	(symbol degreesRing) => C monoid degreesRing R,
	(symbol numgens) => g, 
	(symbol ring) => R, 
	(symbol rank) => r,
	(symbol weights) => (W1, W2)
	}
    )

diagonalAction (Matrix, List, PolynomialRing) := DiagonalAction => (W, d, R) -> (
    if ring W =!= ZZ then (
	error "diagonalAction: Expected the first argument to be a matrix of integer weights."
	);
    r := numRows W - #d;
    if r < 0 then (
	error "diagonalAction: The number of rows of the matrix cannot be smaller than the size of the list."
	); 
    W1 := W^(apply(r, i -> i));
    W2 := W^(apply(#d, i -> r + i));
    diagonalAction(W1, W2, d, R)
    )

diagonalAction (Matrix, PolynomialRing) := DiagonalAction => (W, R) -> diagonalAction(W, {}, R)


-------------------------------------------

net DiagonalAction := D -> (
    torus := "";
    cyclicGroups := "";
    r := D.rank;
    g := D.numgens;
    local weightMatrix;
    if r > 0 then (
	torus = (expression coefficientRing D.ring)^(expression "*");
	if r > 1 then torus = (expression ("("|net torus|")"))^(expression r)
	);
    if g > 0 then (
	cyclicGroups = cyclicGroups|horizontalJoin apply(g, i -> (
		if i == g - 1 then (net ZZ|"/"|net D.cyclicFactors#i)
		else (net ZZ|"/"|net D.cyclicFactors#i|" x ")
		)
	    );
	if r > 0 then (
	    torus = net torus|" x ";
	    weightMatrix = D.weights
	    )
	else weightMatrix = last D.weights
	)
    else weightMatrix = first D.weights;
    stack {(net D.ring)|" <- "|net torus|net cyclicGroups|" via ","", net weightMatrix}
    )

cyclicFactors = method()

cyclicFactors DiagonalAction := List => D -> D.cyclicFactors

degreesRing DiagonalAction := Ring => D -> D.degreesRing

numgens DiagonalAction := ZZ => D -> D.numgens

rank DiagonalAction := ZZ => D -> D.rank

weights = method()

weights DiagonalAction := Matrix => D -> D.weights



equivariantHilbertSeries = method(Options => {Order => infinity}, TypicalValue => Divide)

equivariantHilbertSeries DiagonalAction := op -> T -> (
    if unique degrees ring T =!= {{1}} then
    error "Only implemented for standard graded polynomial rings";
    ord := op.Order;
    if ord === infinity then (
	equivariantHilbertRational(T)
	)
    else (
	equivariantHilbertPartial(T,ord-1)
	)
    )

equivariantHilbertRational = T -> (
    n := dim T;
    W1 := first weights T;
    W2 := last weights T;
    d := cyclicFactors T;
    if not zero W2 then (
    	W2 = matrix apply(entries W2,d,(row,m)->apply(row,i->i%m));
    	);
    W := W1 || W2;
    R := degreesRing T;
    C := coefficientRing R;
    p := pairs tally entries transpose W;
    den := Product apply(sort apply(p, (w,e) -> {1 - C_w * R_0,e}), t -> Power t);
    Divide{1,den}
)

equivariantHilbertPartial = (T, d) -> (
    if not T.cache.?equivariantHilbert then (
	T.cache.equivariantHilbert = 1_(degreesRing T);
	);
    currentDeg := first degree T.cache.equivariantHilbert;
    (M,C) := coefficients T.cache.equivariantHilbert;
    if (d > currentDeg) then (
	R := degreesRing T;
	r := rank T;
	g := numgens T;
	cf := cyclicFactors T;
    	den := value denominator equivariantHilbertSeries T;
    	denDeg := first degree den;
	B := last coefficients den;
	if cf =!= {} then (
	    CR := coefficientRing R;
	    phi := map(CR,R);
	    CRab := ZZ[Variables=>g];
	    CRab = CRab / ideal apply(g,i -> CRab_i^(cf_i)-1);
	    psi := map(CRab,CR,toList(r:1)|(gens CRab));
	    psi' := map(CR,CRab,apply(g, i-> CR_(r+i)));
      	    (m,c) := coefficients(phi B,Variables=>apply(g, i-> CR_(r+i)));
	    m = psi' psi m;
	    B = m*c;
	    );
	for i from currentDeg+1 to d do (
	    p := -sum(1..min(i,denDeg),k -> C_(i-k,0)*B_(k,0) );
	    if cf =!= {} then (
	    	(m,c) = coefficients(phi p,Variables=>apply(g, i-> CR_(r+i)));
	    	m = psi' psi m;
	    	p = (m*c)_(0,0);
	    	);
	    M = M | matrix{{R_0^i}};
	    C = C || matrix{{p}};
	    );
	);
    q := first flatten entries (M_{0..d}*C^{0..d});
    T.cache.equivariantHilbert = q
    )
