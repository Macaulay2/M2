-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

-------------------------------------------
--- LinearlyReductiveAction methods -------
-------------------------------------------
LinearlyReductiveAction = new Type of GroupAction  

linearlyReductiveAction = method()

linearlyReductiveAction (Ideal, Matrix, PolynomialRing) :=
linearlyReductiveAction (Ideal, Matrix, QuotientRing) := LinearlyReductiveAction => (A, M, Q) -> (
    R := ambient Q;
    if not isField coefficientRing R then (error "linearlyReductiveAction: Expected the third argument to be a polynomial ring over a field.");
    if (numColumns M =!= numRows M) or (numRows M =!= #(gens R)) then (error "linearlyReductiveAction: Matrix size does not match polynomial ring.");
    if coefficientRing ring A =!= coefficientRing R then (error "linearlyReductiveAction: Group and polynomial ring not defined over same field.");
    new LinearlyReductiveAction from {
	cache => new CacheTable,
	(symbol groupIdeal) => A, 
	(symbol actionMatrix) => M, 
	(symbol ring) => Q
	}
    )


-------------------------------------------

net LinearlyReductiveAction := V -> (
    stack {(net V.ring)|" <- "|(net ring V.groupIdeal)|"/"|(net V.groupIdeal)|" via ",
	"", net V.actionMatrix}
    )

actionMatrix = method()

actionMatrix LinearlyReductiveAction := Matrix => V -> V.actionMatrix

groupIdeal = method()

groupIdeal LinearlyReductiveAction := Ideal => V -> V.groupIdeal


---------------------------------------------

hilbertIdeal = method(Options => {
	DegreeLimit => {},
	SubringLimit => infinity
	})

hilbertIdeal LinearlyReductiveAction := Ideal => opts -> V -> (
    if opts.DegreeLimit === {} and opts.SubringLimit === infinity and V.cache#?hilbertIdeal then (
	return V.cache#hilbertIdeal;
	);
    A := groupIdeal V;
    M := actionMatrix V;
    R := ambient ring V;
    U := ideal ring V;
    if (numColumns M =!= numRows M) or (numRows M =!= #(gens R)) then print "Matrix size does not match polynomial ring";
    n := #(gens R);
    K := coefficientRing(R);
    l := #(gens ring M);
    
    x := local x, y := local y, z := local z;
    S := K[z_1..z_l, x_1..x_n, y_1..y_n, MonomialOrder=>Eliminate l];
    M' := sub(M, apply(l, i -> (ring M)_i => z_(i+1)));
    A' := sub(A, apply(l, i -> (ring M)_i => z_(i+1)));
    Ux' := sub(U, apply(n, i -> R_i => x_(i+1)));
    Uy' := sub(U, apply(n, i -> R_i => y_(i+1)));
    
    J' := apply(n, i -> y_(i+1) - sum(n, j -> M'_(j,i) * x_(j+1)));
    J := A' + ideal(J') + Ux' + Uy';
    if opts.DegreeLimit === {} and opts.SubringLimit === infinity then (
	I := eliminate(apply(l, i -> z_(i+1)),J);
	) else (
	I = ideal selectInSubring(1,
	    gens gb(J,DegreeLimit=>opts.DegreeLimit,SubringLimit=>opts.SubringLimit)
	    );
	);
    II := sub(I, apply(n, i -> y_(i+1) => 0));
    
    II = trim(sub(II, join(apply(n, i -> x_(i+1) => (ring V)_i),apply(n, i -> y_(i+1) => 0), apply(l, i -> z_(i+1) => 0))));

    if opts.DegreeLimit === {} and opts.SubringLimit === infinity then (
	V.cache#hilbertIdeal = II;
	);

    return II;
    )




