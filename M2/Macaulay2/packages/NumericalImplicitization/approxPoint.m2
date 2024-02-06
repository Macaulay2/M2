
-- Find approximate point on variety

realPoint = method(Options => {Tolerance => 1e-10, Iterations => 1000, Initial => "random"})
realPoint Ideal := Matrix => opts -> I -> (
	p := optimizeNelderMead(I, opts);
	if norm sub(gens I, p) > opts.Tolerance then (
		if debugLevel > 0 then print "Starting gradient descent";
		p = lineSearch(I, p, opts);
	);
	point p
)

-- Nelder-Mead(/downhill simplex/amoeba) method

optimizeNelderMead = method(Options => {Tolerance => 1e-6, Iterations => 200, Initial => "standard"})
optimizeNelderMead (FunctionClosure, List) := List => opts -> (F, V) -> (
	-- Inputs: F = nonnegative real-valued function, V = full-dim simplex given as (vertices, values)
	-- Output: (real) zero of F (approximate up to opts.Tolerance)
	n := #V - 1;
	counter := 0;
	V = sort(V, last);
	while counter < opts.Iterations do (
		if debugLevel > 1 then print("Using points: " | toString(V/first/entries/first));
		if V#0#1 < opts.Tolerance then (
			if debugLevel > 0 then print "Found solution to within tolerance";
			break;
		);
		centroid := (1/n)*sum drop(V/first, -1);
		reflected := 2*centroid - V#-1#0; -- alpha = 1
		fR := F(reflected);
		if V#0#1 <= fR and fR < V#-2#1 then (
			if debugLevel > 1 then print("Reflection: " | toString first entries reflected);
			V = append(drop(V, -1), {reflected, fR});
		) else if fR < V#0#1 then (
			expanded := 2*reflected - centroid; -- gamma = 2
			if debugLevel > 1 then print("Expansion: " | toString first entries expanded);
			fE := F(expanded);
			V = prepend(if fE < fR then {expanded, fE} else {reflected, fR}, drop(V, -1));
		) else (
			contracted := (1/2)*(V#-1#0 + centroid); -- rho = 1/2
			fC := F(contracted);
			if fC < V#-1#1 then (
				if debugLevel > 1 then print("Contraction: " | toString first entries contracted);
				V = append(drop(V, -1), {contracted, fC});
			) else (
				if debugLevel > 1 then print "Shrink";
				V = {V#0#0} | apply(#V-1, i -> (1/2)*(V#(i+1)#0 + V#0#0)); -- sigma = 1/2
				V = apply(V, v -> {v, F(v)});
			);
		);
		V = sort(V, last);
		counter = counter+1;
		if debugLevel > 0 and counter % 100 == 0 then print("Completed " | toString counter | " iterations");
		if V#-1#1 - V#0#1 < (opts.Tolerance)^(1.5) and V#0#1 > opts.Tolerance then (
			if debugLevel > 0 then print "Stuck in local minimum";
			break;
		);
	);
	V
)
optimizeNelderMead Ideal := Matrix => opts -> I -> (
	n := dim ring I;
	k := if instance(coefficientRing ring I, ComplexField) then CC else RR;
	V := if instance(opts.Initial, List) then opts.Initial
		else if opts.Initial === "standard" then simplex n
		else if opts.Initial === "continue" then I.cache#"nelderMeadSimplex"
		else entries(random(k^(n+1), k^n));
	F := x -> norm sub(gens I, x); -- x -> sub(sos, x);
	if not opts.Initial === "continue" then V = apply(V, v -> {matrix{v}, F(matrix{v})});
	V = optimizeNelderMead(F, V, opts);
	if debugLevel > 0 and V#0#1 > opts.Tolerance then print "Solution not found within tolerance. Try running this function again with Initial => \"continue\", or alternatively use lineSearch";
	I.cache#"nelderMeadSimplex" = V;
	V#0#0
)

simplex = method(Options => { CoefficientRing => RR })
simplex (ZZ, Number, Number) := List => opts -> (n, a, s) -> (
	k := opts.CoefficientRing;
	v0 := if instance(a, List) then a else a*toList(n:1_k);
	A := entries (matrix{{n:0_k}} || s*id_(k^n));
	A/(v -> v + v0)
)
simplex ZZ := List => opts -> n -> simplex(n, 0, 1, opts)

sort (List, Function) := opts -> (L, f) -> (
	H := hashTable(identity, apply(L, l -> f(l) => l));
	deepSplice join apply(sort keys H, k -> H#k)
)

lineSearch = method(Options => options realPoint)
lineSearch (Ideal, Matrix) := Matrix => opts -> (I, s) -> (
	sos := sum(I_*, g -> g^2);
	grad := diff(vars ring I, sos);
	F := x -> sub(sos, x);
	y := s;
	for i from 1 to opts.Iterations do (
		v := sub(grad, y);
		m := norm v;
		p := -(1/m)*v; -- descent direction
		t := (1/2)*m;
		alpha := 2; -- initial (max) step size
		(F0, F1) := (F(y), F(y + alpha*p));
		while F1 > F0 - alpha*t and alpha > opts.Tolerance do ( -- Armijo-Goldstein condition
			alpha = (1/2)*alpha; -- backtracking
			F1 = F(y + alpha*p);
		);
		if debugLevel > 0 then print("Old: " | toString(F0) | ", new: " | toString(F1));
		y = y + alpha*p;
		if F1 < opts.Tolerance then (
			if debugLevel > 0 then print "Found solution to within tolerance";
			break;
		);
		if debugLevel > 0 and i % 25 == 0 then print("Completed " | toString i | " iterations");
	);
	y
)
