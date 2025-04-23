--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

needs "gb.m2"
-- TODO: needs "genmat.m2" but would introduce a cycle
needs "matrix1.m2"
needs "quotring.m2"

pivots = method()

pivots Matrix := (p) -> (			    -- I wish this could be in the engine
     R := ring p;
     f := leadTerm matrix {{1_R},{1_R}};
     dir := if f_(0,0) == 1 then Down else Up;
     opt := Reverse => dir === Up;
     cols := entries transpose p;
     for j from 0 to #cols-1 list (
	  i := position(cols#j, e -> e != 0, opt);
	  if i === null then continue;
	  (i,j)))

smithNormalForm = method(
     Options => {
	  ChangeMatrix => {true, true},		    -- target, source
     	  KeepZeroes => true
	  }
     )
smithNormalForm Matrix := o -> A -> (
    (D',P,Q) := preSmithNormalForm(A,o);
    D := mutableMatrix D';
    -- get into a diagonal matrix
    n := min(numRows D, numColumns D);
    start'i := 0;
    done := false;
    P' := mutableMatrix map((ring D')^(numRows D'));
    Q' := mutableMatrix map((ring D')^(numColumns D'));
    while not done do (
	pos'i := position(start'i..(n-2),j->D_(j+1,j+1)!=0 and D_(j+1,j+1) % D_(j,j) != 0);	
	if pos'i === null then (
	    if start'i > 0 then start'i = 0 else done = true
	    ) else (
	    i := pos'i + start'i;
	    start'i = i + 1;
	    a := D_(i,i);
	    b := D_(i+1,i+1);
	    (g,e,f) := gcdCoefficients(a,b);
	    D_(i,i) = g;
	    D_(i+1,i+1) = lcm(a,b);
	    rowAdd(P',i+1,e,i);
	    rowAdd(P',i,1-a//g,i+1);
	    rowAdd(P',i+1,-1,i);
	    rowAdd(Q',i,f,i+1);
	    rowAdd(Q',i+1,(a//g-1)*b//g,i);
    )
	);-- end while 
    P'' := matrix P';
    Q'' := matrix Q';
    if P =!= null then P = P'' * P;
    if Q =!= null then Q = Q * transpose Q'';
    unsequence nonnull (P'' * D' * transpose Q'', P, Q)
)

preSmithNormalForm = method(
     Options => {
	  ChangeMatrix => {true, true},		    -- target, source
     	  KeepZeroes => true
	  }
     )
preSmithNormalForm Matrix := o -> (f) -> (
     pruneSyz := m -> generators gb(syz m, Syzygies=>false, ChangeMatrix=>false);
     if not isFreeModule source f or not isFreeModule target f then error "expected map between free modules";
     (tchg,schg,keepz) := (o.ChangeMatrix#0, o.ChangeMatrix#1,o.KeepZeroes);
     (tmat,smat) := (null,null);	-- null represents the identity, lazily
     (tzer,szer) := (null,null);	-- null represents zero, lazily
     R := ring f;
     R' := R[MonomialOrder => Position => Up];
     f' := promote(f,R');
     g := f';
     op := false;	       -- whether we are working on the transposed side
     count := 0;
     while true do (
	  flag := if op then tchg else schg;
	  G := gb(g, ChangeMatrix => flag, Syzygies => flag);
	  h := generators G;
	  if debugLevel > 100 then (
	       << "-- count = " << count << endl;
	       if op then (
		    << "-- lead terms in rows    (gb    ) : " << transpose leadTerm h << endl;
		    << "-- lead terms in columns (not gb) : " << leadTerm transpose h << endl;
		    )
	       else (
		    << "-- lead terms in columns (gb    ) : " << leadTerm h << endl;
		    << "-- lead terms in rows    (not gb) : " << transpose leadTerm transpose h << endl;
		    ));
     	  if count > 0 and h == g then break;	  
	  if op then (
	       if tchg then (
	       	    chg := getChangeMatrix G;
	       	    zer := pruneSyz G;
		    if keepz then (
			 if tmat === null
			 then (tmat,tzer) = (chg,zer)
			 else (tmat,tzer) = (tmat * chg, tmat * zer | tzer ))
		    else (
			 if tmat === null
			 then tmat = chg
			 else tmat = tmat * chg)))
	  else (
	       if schg then (
	       	    chg = getChangeMatrix G;
	       	    zer = pruneSyz G;
		    if keepz then (
			 if smat === null
			 then (smat, szer) = (chg, zer)
			 else (smat, szer) = (smat * chg, smat * zer | szer ))
		    else (
			 if smat === null
			 then smat = chg
			 else smat = smat * chg)));
	  g = transpose h;
	  op = not op;
	  count = count + 1;
	  ); -- end while
     if op then g = transpose g;
     if tchg then (
	  if tmat === null
	  then tchange := id_(target f')
	  else (
     	       tmat = transpose tmat;
	       if keepz then (
     	       	    tzer = transpose tzer;
	       	    tchange = tmat || tzer;
	       	    g = g || map(target tzer, source g,0))
	       else tchange = tmat));
     if schg then (
	  if smat === null
	  then schange := id_(source f')
	  else (
	       if keepz then (
	       	    schange = smat | szer;
	       	    g = g | map(target g, source szer, 0))
	       else schange = smat));
     g = lift(g,R);
     -- D == P f Q ;
     D := if not keepz then map(R^(numgens target g),,g) else (
	 m := numgens target f;
	 n := numgens source f;
	 m' := numgens target g;
	 n' := numgens source g;
	 (map(R^m',R^n',g) | map(R^m',R^(n-n'),0)) || map(R^(m-m'),R^n,0) 
	 );			    -- this makes D homogeneous, if possible
     P := if tchg then map(target D, target f, lift(tchange,R));
     Q := if schg then map(source f, source D, lift(schange,R));
     ( D, P, Q )
     )

 
complementOkay = method()     -- modeled after isAffineRing, but allows ZZ, too
complementOkay Ring := R -> isField R or R === ZZ
complementOkay PolynomialRing := R -> (
     -- complement works over skew-commutative rings, so we don't insist on commutativity
     (options R).WeylAlgebra === {} and not (options R).Inverses and complementOkay coefficientRing R
     )
complementOkay QuotientRing := R -> isField R or complementOkay ambient R

complement = method()
complement Matrix := Matrix => (f) -> (
     if not isHomogeneous f then error "complement: expected homogeneous matrix";
     if not isFreeModule source f or not isFreeModule target f then error "expected map between free modules";
     R := ring f;
     if isField R then (
	  h := transpose syz transpose f;
	  id_(target h) // h)
     else if R === ZZ then (
	  (g,ch) := smithNormalForm(f,ChangeMatrix=>{true,false});
	  m := numgens target g;
	  piv := select(pivots g,(i,j) -> abs g_(i,j) === 1);
	  rows' := first \ piv;
	  submatrix'(id_(ZZ^m),rows') // ch				    -- would be faster if gb provided inverse change matrices!!!
	  )
     else if complementOkay R then (
	  (R',F) := flattenRing R; -- we flatten because otherwise we might get the degree map wrong, spoiling homogeneity
	  f' := F f;
	  map(target f,,R ** complement (map(coefficientRing R', R')) f')
	  )
     else if instance(R,QuotientRing) then map(target f,,R ** complement lift(f,ambient R))
     else error "complement: expected matrix over affine ring or finitely generated ZZ-algebra")

-----------------------------------------------------------------------------
-- mingens and trim
-----------------------------------------------------------------------------

-- the method is declared in gb.m2
-- TODO: the strategies should be separated
mingens Ideal  := Matrix => opts -> I -> mingens(module I, opts)
mingens Module := Matrix => opts -> M -> M.cache.mingens ??= if isFreeModule M then generators M else tryHooks(
    (mingens, Module), (opts, M), (opts, M) -> (
	if opts.Strategy === null then opts = opts ++ { Strategy => Complement };
 	  mingb := m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);
	  zr := f -> if f === null or f == 0 then null else f;
	  F := ambient M;
	  epi := g -> -1 === rawGBContains(g, rawIdentity(raw F,0));
	  if M.?generators then (
	       if M.?relations then (
		    if opts.Strategy === Complement and isHomogeneous M and isAffineRing ring M then (
			 c := mingens mingb (M.generators|M.relations);
			 c * complement(M.relations // c))
		    else (
	  	    	 tot := mingb(M.generators|M.relations);
		    	 rel := mingb(M.relations);
		    	 mingens mingb (mingens tot % rel)))
	       else mingens mingb M.generators)
	  else (
	       if M.?relations then (
		    if opts.Strategy === Complement and isHomogeneous M.relations then (
			 complement M.relations)
		    else mingens mingb (id_F % mingb(M.relations)))
	       else id_F)))

trim = method (Options => { Strategy => null -* TODO: add DegreeLimit => {} *-})
trim Ring         := Ring => o -> identity
trim QuotientRing := Ring => o -> R -> quotient trim(ideal presentation R, o)

-- TODO: the strategies should be separated
trim Ideal  := Ideal  => opts -> I -> ideal trim(module I, opts)
trim Module := Module => opts -> M -> M.cache#(symbol trim => opts) ??= if isFreeModule M then M else tryHooks(
    (trim, Module), (opts, M), (opts, M) -> (
	if opts.Strategy === null then opts = opts ++ { Strategy => Complement };
	  -- we preserve the ambient free module of which M is subquotient and try to minimize the generators and relations
	  --   without computing an entire gb
	  -- does using "complement" as in "mingens Module" above offer a benefit?
 	  mingb := m -> gb (m, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false);
	  zr := f -> if f === null or f == 0 then null else f;
	  F := ambient M;
	  epi := g -> -1 === rawGBContains(g, rawIdentity(raw F,0));
     	  if not M.?generators then M = subquotient(ambient M,id_(ambient M),if M.?relations then M.relations);
	  N := (
	       if M.?relations then (
		    if opts.Strategy === Complement then (
			 gns := mingens(M,opts);
			 rlns := mingens(image M.relations,opts);
			 gns' := mingb gns;
			 gns = if not epi raw gns' then mingens gns';
			 if gns === M.generators and rlns === M.relations then M
			 else subquotient(F, gns, zr rlns))
		    else if opts.Strategy === Inhomogeneous then (
	  	    	 tot := mingb(M.generators|M.relations);
		    	 rel := mingb(M.relations);
			 if tot === M.generators and rel === M.relations 
			 then M
			 else subquotient(F, if not epi raw tot then mingens mingb (mingens tot % rel), zr mingens rel ))
		    else error "trim: unrecognized Strategy option")
	       else (
		    if opts.Strategy === Complement then (
			 g := mingens(M,opts);
			 if g === M.generators then M else (
			      M' := image g;
			      -- The code below was intended to give M' a cheap Groebner basis,
     			      -- but we comment out it, because even if Syzygies is set to false, ChangeMatrix might have been true, and we don't record that somehow.
			      -- Mgc := M.generators.cache;
			      -- M'gc := M'.generators.cache;
			      -- scan(pairs Mgc, (k,v) -> (
			      -- 		if instance(k,GroebnerBasisOptions) then (
			      -- 		     if not k.Syzygies then M'gc#k = v
			      -- 		     else (
			      -- 			  k' := new GroebnerBasisOptions from {
			      -- 			       HardDegreeLimit => k.HardDegreeLimit,
			      -- 			       Syzygies => false,
			      -- 			       SyzygyRows => 0
			      -- 			       };
			      -- 			  M'gc#k' = v; -- the gb, v, may actually have syzygies, but they won't be accessed
			      -- 			  )
			      -- 		     );
			      -- 		));
			      M'))
		    else if opts.Strategy === Inhomogeneous then (
	  	    	 tot = mingb M.generators;
		    	 subquotient(F, if not epi raw tot then mingens tot, ))
		    else error "trim: unrecognized Strategy option"));
	  if N.?generators and epi raw mingb N.generators then N = subquotient(ambient N,,if N.?relations then N.relations);
	  -- TODO: make into a separate hook
	  if ring M === ZZ then (
	       LLLBases := needsPackage "LLLBases";
	       LLL := value LLLBases.Dictionary#"LLL";
	       N = subquotient(F, if N.?generators then compress LLL N.generators, if N.?relations then compress LLL N.relations);
	       );
	  N.cache.trim = N;
	  N))

trimPID := M -> if M.?relations then (if M.?generators then trimPID image generators M else ambient M) / trimPID image relations M else if not M.?generators then M else (
    f := presentation M;
    (g,ch) := smithNormalForm(f, ChangeMatrix => {true, false});
    isunit := r -> r != 0 and degree r === {0};
    rows := select(min(rank source g,rank target g),i->isunit g_(i,i));
    rows = rows | toList(rank target f..<rank target g); -- temporary fix for #3017
    ch = submatrix'(ch,rows,);
    p:=id_(target ch)//ch;
    q:=generators M*p;
    d:=diagonalMatrix apply(rank source q,i->lcm apply(entries (q_i),a->if a==0 then 1 else 1/leadCoefficient a));
    image (q*d)
    )
addHook((trim, Module), Strategy => "PID",
    (opts, M) -> (
	R := ring M;
	if instance(R,PolynomialRing) and numgens R === 1 and isField coefficientRing R and not isHomogeneous M then trimPID M
	)
    )

-----------------------------------------------------------------------------

syz Matrix := Matrix => opts -> (f) -> (
    c := runHooks((syz, Matrix), (opts, f));
    if c =!= null then return c;
    error "syz: no strategy implemented for this type of matrix")

addHook((syz, Matrix), Strategy => Default, (opts, f) -> (
     if not isFreeModule target f or not isFreeModule source f
     then error "expected map between free modules";
     if ring f === ZZ or not isHomogeneous f
     then syz gb (f, opts, Syzygies=>true)
     else mingens image syz gb (f, opts, Syzygies=>true)
     ))

modulo = method(
     Options => {
     	  -- DegreeLimit => {}
	  }
     )
modulo(Matrix,Nothing) := Matrix => options -> (m,null) -> syz(m,options)
modulo(Nothing,Matrix) := Matrix => options -> (null,n) -> n
modulo(Matrix,Matrix)  := Matrix => options -> (m,n) -> (
     (P,L) := (target m, source m);
     if P =!= target n then error "expected maps with the same target";
     if not isFreeModule P or not isFreeModule L or not isFreeModule source n
     then error "expected maps between free modules";
     dm := degree m;
     if not all(dm,zero) then (
	  R := ring P;
	  H := R^{ dm };
	  m = map(target m ** H, source m, m, Degree => apply(dm,i->0) );
	  n = n ** H;
	  );
     h := m|n;
     if debugLevel == 11 and isHomogeneous m and isHomogeneous n and not isHomogeneous h then error "modulo: homogeneity lost";
     f := syz(h, options, SyzygyRows => numgens L);
     if target f =!= L 
     then map(L,source f,f)			    -- it can happen that L has a Schreier order, and we want to preserve that exactly
     else f)

-- TODO: make this work between non-free modules
quotientRemainder'(Matrix, Matrix) := Matrix => (f, g) -> (
    L := source f;
    M := target f;
    N := target g;
    if L =!= source g then error "expected maps with the same source";
    if not all({L, M, N}, isFreeModule) then error "expected maps between free modules";
    dual \ quotientRemainder(dual f, dual g))

quotientRemainder(Matrix,Matrix) := Matrix => (f,g) -> (
     L := source f;					    -- result may not be well defined if L is not free
     M := target f;
     N := source g;
     if M =!= target g then error "expected maps with the same target";
     if M.?generators then (
	  M = cokernel presentation M;	    -- this doesn't change the cover
	  );
     f = matrix f;
     g = matrix g;
     G := (
	  if M.?relations 
	  then gb(g | presentation M, ChangeMatrix => true, SyzygyRows => rank source g)
	  else gb(g,                  ChangeMatrix => true)
	  );
     (rem,quo,cplt) := rawGBMatrixLift(raw G, raw f);
     (
	  map(N, L, quo, Degree => degree f - degree g),
	  map(M, L, rem)
     ))

--------------------------------------------------------------------------------
-- factoring of a matrix through another

-- Note: when f and g are endomorphisms, the sources and targets all agree,
-- so we need both functions quotient and quotient' to distinguish them.

-- factor matrices with same targets
Matrix // Matrix := Matrix => (f, g) -> quotient(f, g)
Number // Matrix := RingElement // Matrix := Matrix => (r, g) -> map(source g, target g, map(target g, cover target g, r) // g)
Matrix // Number := Matrix // RingElement := Matrix => (f, r) -> map(target f, source f, f // map(target f, cover target f, r))

-- factor matrices with same sources
Matrix \\ Matrix := Matrix => (g, f) -> quotient'(f, g)
-- commented because they don't seem very meaningful
--Matrix \\ Number := Matrix \\ RingElement := Matrix => (g, r) -> map(source g, target g, g \\ map(cover source g, source g, r))
--Number \\ Matrix := RingElement \\ Matrix := Matrix => (r, f) -> map(target g, source g, map(cover source g, source g, r) \\ f)

quotient(Matrix, Matrix) := Matrix => opts -> (f, g) -> (
    -- given f: A-->C and g: B-->C, then find (f//g): A-->B such that g o (f//g) + r = f
    if target f != target g then error "quotient: expected maps with the same target";
    c := runHooks((quotient, Matrix, Matrix), (opts, f, g), Strategy => opts.Strategy);
    if c =!= null then c else error "quotient: no method implemented for this type of input")

addHook((quotient, Matrix, Matrix), Strategy => Default,
    -- Note: this strategy only works if the remainder is zero, i.e.:
    -- homomorphism' f % image Hom(source f, g) == 0
    (opts, f, g) -> (
	opts = new OptionTable from {
	    DegreeLimit       => opts.DegreeLimit,
	    MinimalGenerators => opts.MinimalGenerators };
	map(source g, source f, homomorphism(homomorphism'(f, opts) // Hom(source f, g, opts)))))

-- FIXME: this is still causing unreasonable slow downs, e.g. for (large m) // (scalar)
addHook((quotient, Matrix, Matrix), Strategy => "Reflexive", (opts, f, g) -> if f == 0 or isFreeModule source f then (
     L := source f;	     -- result may not be well-defined if L is not free
     M := target f;
     N := source g;
     if M.?generators then (
	  M = cokernel presentation M;	    -- this doesn't change the cover
	  );
     -- now M is a quotient module, without explicit generators
     f' := matrix f;
     g' := matrix g;
     G := (
	  if g.cache#?"gb for quotient"
	  then g.cache#"gb for quotient"
	  else g.cache#"gb for quotient" = (
	       if M.?relations 
	       then gb(g' | relations M, ChangeMatrix => true, SyzygyRows => rank source g')
	       else gb(g',               ChangeMatrix => true)));
     map(N, L, f' // G, 
	  Degree => degree f' - degree g'  -- set the degree in the engine instead
	  )))

addHook((quotient, Matrix, Matrix), Strategy => InexactField, (opts, f, g) ->
    if instance(ring target f, InexactField)
    -- TODO: c.f. https://github.com/Macaulay2/M2/issues/3252
    and numRows g === numColumns g
    and isFreeModule source g
    and isFreeModule source f
    then solve(g, f))

addHook((quotient, Matrix, Matrix), Strategy => ZZ, (opts, f, g) ->
    if isQuotientOf(ZZ, ring target f)
    and isFreeModule source g
    and isFreeModule source f
    then solve(g, f))

quotient'(Matrix, Matrix) := Matrix => opts -> (f, g) -> (
    -- given f: A-->C and g: A-->B, then finds (g\\f): B-->C such that (g\\f) o g + r = f
    if source f != source g then error "quotient': expected maps with the same source";
    c := runHooks((quotient', Matrix, Matrix), (opts, f, g), Strategy => opts.Strategy);
    if c =!= null then c else error "quotient': no method implemented for this type of input")

addHook((quotient', Matrix, Matrix), Strategy => Default,
    -- Note: this strategy only works if the remainder is zero, i.e.:
    -- homomorphism' f % image Hom(g, target f) == 0
    (opts, f, g) -> (
	opts = new OptionTable from {
	    DegreeLimit       => opts.DegreeLimit,
	    MinimalGenerators => opts.MinimalGenerators };
	map(target f, target g, homomorphism(homomorphism'(f, opts) // Hom(g, target f, opts)))))

addHook((quotient', Matrix, Matrix), Strategy => "Reflexive",
    (opts, f, g) -> if all({source f, target f, source g, target g}, isFreeModule) then dual quotient(dual f, dual g, opts))

--------------------

remainder'(Matrix,Matrix) := Matrix => (f,g) -> (
     if not isFreeModule source f or not isFreeModule source g
     or not isFreeModule source g or not isFreeModule source g then error "expected maps between free modules";
     dual remainder(dual f, dual g))
remainder(Matrix,Matrix) := Matrix % Matrix := Matrix => (n,m) -> (
     if target m =!= target n then error "expected matrices with the same target";
     if not isFreeModule source n or not isFreeModule source m then error "expected maps from free modules";
     if not isQuotientModule target m then error "expected maps to a quotient module";
     c := runHooks((remainder, Matrix, Matrix), (n, m));
     if c =!= null then c else error "remainder: no method implemented for this type of input")
addHook((remainder, Matrix, Matrix), Strategy => Default, (n, m) -> n % gb image m)

Matrix % Module := Matrix => (f,M) -> f % gb M

RingElement % Matrix := (r,f) -> (
     if isFreeModule target f and numgens target f === 1 
     then ((r * id_(target f)) % f)_(0,0)
     else error "expected target of matrix to be free, and of rank 1"
     )

RingElement % Ideal := (r,I) -> (
     R := ring I;
     if ring r =!= R then error "expected ring element and ideal for the same ring";
     if r == 0 then return r;
     r % if isHomogeneous I and heft R =!= null then gb(I, DegreeLimit => degree r) else gb I)
Number % Ideal := (r,I) -> (
     R := ring I;
     r = promote(r,R);
     if r == 0 then return r;
     r % if isHomogeneous I and heft R =!= null then gb(I,DegreeLimit => toList (degreeLength R : 0)) else gb I)
isMember(RingElement, Ideal) :=
isMember(Number,      Ideal) := (r, I) -> zero(r % I)

Matrix % RingElement := (f,r) -> f % (r * id_(target f))    -- this could be sped up: compute gb matrix {{r}} first, tensor with id matrix, force gb, etc

-------------------------------------
-- index number of a ring variable --
-------------------------------------
index = method()
index RingElement := f -> rawIndexIfVariable raw f

degree (RingElement, RingElement) := (x,f) -> (
     i := index x;
     if i === null then error "expected a variable";
     wt := splice {i:0,1};
     (lo,hi) := weightRange(wt,f);
     hi
     )

indices RingElement := (f) -> rawIndices raw f
indices Matrix := (f) -> rawIndices raw f

support = method()
support RingElement :=
support Matrix      := f -> apply(try rawIndices raw f else {}, i -> (ring f)_i)
support Ideal       := I -> support generators I
--------------------
-- homogenization --
--------------------
homogenize = method()

listZ := v -> (
     if not all(v,i -> class i === ZZ) then error "expected list of integers";
     )

homogCheck := (R, f, v, wts) -> (
     if R =!= ring v then error "homogenization requires variable in the same ring";
     listZ wts;
     if degreeLength R =!= 1 then error "homogenization requires degrees of length 1";
     -- if # wts != numgens ring f then error "homogenization weight vector has incorrect length";
     i := index v;
     w := wts#i;
     if not all(wts, a -> a%w == 0) then error "weight of homogenization variable doesn't divide the others";
     i)

homogenize(RingElement, RingElement, List) := RingElement => (f,v,wts) -> (
     R := ring f;
     wts = splice wts;
     i := homogCheck(R,f,v,wts);
     new R from rawHomogenize(raw f, i, wts))

homogenize(Matrix, RingElement, List) := Matrix => (f,v,wts) -> (
     R := ring f;
     wts = splice wts;
     -- why did we have this?
     -- wts = apply(wts, i -> if instance(i,InfiniteNumber) then 0 else i);
     i := homogCheck(R,f,v,wts);
     if debugLevel > 10 then << (new FunctionApplication from {rawHomogenize, (f.RawMatrix, index v, wts)}) << endl;
     map(target f, source f, rawHomogenize(f.RawMatrix, i, wts)))

homogenize(RingElement, RingElement) :=
homogenize(Matrix, RingElement) := Matrix => (f,n) -> (
     R := ring f;
     if degreeLength R =!= 1 then error "homogenization requires degrees of length 1";
     wts := apply(generators(R, CoefficientRing=>ZZ), x -> first degree x);
     homogenize(f,n,wts))

homogenize(Module,RingElement) := Module => (M,z) -> (
     if isFreeModule M then M
     else subquotient(
	  if M.?generators then homogenize(generators gb M.generators,z),
	  if M.?relations then homogenize(generators gb M.relations,z)))

homogenize(Ideal,RingElement) := Ideal => (I,z) -> ideal homogenize(module I, z)

homogenize(Module,RingElement,List) := Module => (M,z,wts) -> (
     if isFreeModule M then M
     else subquotient(
	  if M.?generators then homogenize(M.generators,z,wts),
	  if M.?relations then homogenize(M.relations,z,wts)))

homogenize(Vector, RingElement, List) := Vector => (f,v,wts) -> (
     p := homogenize(f#0,v,wts);
     new target p from {p})
homogenize(Vector, RingElement) := Vector => (f,v) -> (
     p := homogenize(f#0,v);
     new target p from {p})

listOfVars := method()
listOfVars(Ring,Thing) := (R,x) -> error("expected 'Variables=>' argument to be a List, Sequence, integer, or RingElement")
listOfVars(Ring,Nothing) := (R,x) -> toList(0 .. numgens R-1)
listOfVars(Ring,List) := (R,x) -> (
     vrs := splice x;
     if #vrs === 0 then return vrs;
     types := unique apply(vrs,class);
     if #types != 1 then error "expected a list or sequence of integers or variables in the same ring";
     if first types =!= ZZ then vrs = index \ vrs;
     if any(vrs,i -> i === null) then error "expected a list of variables";
     vrs
     )
listOfVars(Ring,Sequence) := (R,x) -> listOfVars(R,toList x)
listOfVars(Ring,ZZ) := (R,x) -> (
     if x < 0 or x >= numgens R then
         error("expected an integer in the range 0 .. "|numgens R-1)
     else {x})
listOfVars(Ring,RingElement) := (R,x) -> (
     if class x === R 
     then {index x}
     else error "expected a ring element of the same ring")

coefficients = method(Options => {Variables => null, Monomials => null})
coefficients(RingElement) := o -> (f) -> coefficients(matrix{{f}},o)
coefficients(Matrix) := o -> (f) -> (
     if not isFreeModule target f then error "expected target to be a free module";
     --if numgens target f =!= 1 then error "expected target to be a free module of rank 1";
     m := raw f;
     vrs := listOfVars(ring f,o.Variables);
     rawmonoms := if o.Monomials === null then
                    rawMonomials(vrs,m)
	          else if class o.Monomials === Matrix then
	            raw o.Monomials
	       else if class o.Monomials === List then raw matrix{o.Monomials}
	       else if class o.Monomials === Sequence then raw matrix{toList o.Monomials}
	       else error "expected 'Monomials=>' argument to be a list, sequence, or matrix";
     --monoms := map(target f,,rawTensor(rawIdentity(raw target f,0),rawmonoms));
     monoms := map(target f,,rawmonoms);
     (monoms,map(source monoms,source f,rawCoefficients(vrs,rawmonoms,m)))
     )

--coefficients(Matrix) := coefficients(RingElement) := (m) -> coefficients(toList (0 .. numgens ring m - 1), m)
--coefficients(List, RingElement) := (v,m) -> coefficients(v,matrix{{m}})
--coefficients(Sequence, RingElement) := (v,m) -> coefficients(toList v,matrix{{m}})
--coefficients(ZZ, Matrix) := coefficients(ZZ, RingElement) := (v,m) -> coefficients({v},m)
--coefficients(Sequence, Matrix) := (vrs,f) -> coefficients(toList vrs,f)
--coefficients(List, Matrix) := (vrs,f) -> (
--     m := raw f;
--     vrs = splice vrs;
--     types := unique apply(vrs,class);
--     if #types != 1 then error "expected a list or sequence of integers or variables in the same ring";
--     R := first types;
--     if R =!= ZZ then vrs = index \ vrs;
--     if any(vrs,i -> i === null) then error "expected a list of variables";
--     monoms := map(target f,,rawTensor(rawIdentity(raw target f,0),rawMonomials(vrs, m)));
--     (monoms,map(source monoms,source f,rawCoefficients(vrs,raw monoms,m))))

monomials = method(Options => {Variables => null})
monomials(RingElement) := o -> (f) -> monomials(matrix{{f}},o)
monomials(Matrix) := o -> (f) -> (
     vrs := listOfVars(ring f,o.Variables);
     map(target f,,rawMonomials(vrs, raw f))
     )

--monomials(Matrix) := monomials(RingElement) := (m) -> monomials(toList (0 .. numgens ring m - 1), m)
--monomials(List, RingElement) := (v,m) -> monomials(v,matrix{{m}})
--monomials(Sequence, RingElement) := (v,m) -> monomials(toList v,matrix{{m}})
--monomials(ZZ, Matrix) := monomials(ZZ, RingElement) := (v,m) -> monomials({v},m)
--monomials(Sequence, Matrix) := (vrs,f) -> monomials(toList vrs,f)
--monomials(List, Matrix) := (vrs,f) -> (
--     m := raw f;
--     vrs = splice vrs;
--     types := unique apply(vrs,class);
--     if #types != 1 then error "expected a list or sequence of integers or variables in the same ring";
--     R := first types;
--     if R =!= ZZ then vrs = index \ vrs;
--     if any(vrs,i -> i === null) then error "expected a list of variables";
--     map(target f,,rawMonomials(vrs, m)))

-- old way, not versatile enough:
--terms RingElement := f -> (
--     (m,c) := flatten \ entries \ coefficients f;
--     apply(m,c,times))

terms RingElement := f -> terms(coefficientRing ring f, f)
terms(Ring,RingElement) := (k,f) -> (
     R := ring f;
     rawR := raw R;
     (cc,mm) := rawPairs(raw k, raw f);
     toList apply(cc, mm, (c,m) -> new R from rawTerm(rawR, c, m)))

sortColumns Matrix := o -> (f) -> toList rawSortColumns(
	  raw f,
	  if o.DegreeOrder === Ascending then 1 else
	  if o.DegreeOrder === Descending then -1 else
	  if o.DegreeOrder === null then 0 else
	  error "expected DegreeOrder option value to be Ascending, Descending, or null",
	  if o.MonomialOrder === Ascending then 1 else
	  if o.MonomialOrder === Descending then -1 else
	  error "expected MonomialOrder option value to be Ascending or Descending")

sort Matrix := o -> (f) -> f_(sortColumns(f,o))
rsort Matrix := o -> (f) -> f_(reverse sortColumns(f,o))

-----------------------------
-- Matrix utility routines --
-----------------------------

selectInSubring = method()

selectInSubring(ZZ, Matrix) := Matrix => (i,m) -> (
     p := rawEliminateVariables(i,m.RawMatrix);
     submatrix(m, p))

divideByVariable = method()

divideByVariable(Matrix, RingElement) := Matrix => (m,v) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     i := index v;
     if i === null then error "expected a generator of the ring";
     (m1,topdegree) := rawDivideByVariable(m.RawMatrix, i, -1);
     (map(ring m, m1), topdegree))

divideByVariable(Matrix, RingElement, ZZ) := Matrix => (m,v,d) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     i := index v;
     if i === null then error "expected a generator of the ring";
     (m1,topdegree) := rawDivideByVariable(m.RawMatrix, i, d);
     (map(ring m, m1), topdegree))

compress = method()

compress Matrix := Matrix => (m) -> map(target m,, rawMatrixCompress m.RawMatrix)

diagonalMatrix = method(TypicalValue => Matrix)
diagonalMatrix(RingFamily,ZZ,ZZ,List) := (R,m,n,vals) -> diagonalMatrix(default R,m,n,vals)
diagonalMatrix(Ring,ZZ,ZZ,List) := (R,m,n,vals) -> (
     if #vals > m or #vals > n then error "too many values for specified matrix size";
     f := mutableMatrix(R,m,n);
     for i from 0 to min(m,n,#vals)-1 do f_(i,i) = vals#i;
     matrix f)
diagonalMatrix(ZZ,ZZ,List) := (m,n,vals) -> diagonalMatrix(commonRing vals,m,n,vals)
diagonalMatrix(RingFamily, List) := (R,v) -> diagonalMatrix(default R, v)
diagonalMatrix(Ring, List) := (R,v) -> diagonalMatrix(R,#v,#v,v)
diagonalMatrix List := v -> diagonalMatrix(#v,#v,v)
diagonalMatrix Matrix := (m) -> (			    -- this one is silly
     R := ring m;
     nrows := numgens target m;
     if nrows === 0 then
       error "expected at least one row";
     if nrows > 1 then m = flatten m;
     a := numgens source m;
     m1 := mutableMatrix(R,a,a);
     for i from 0 to a-1 do m1_(i,i) = m_(0,i);
     matrix m1)

newCoordinateSystem = method()

newCoordinateSystem(PolynomialRing, Matrix) := (S,x) -> (
  -- x should be a one row matrix of linear forms
  -- S should be a ring, with the same number of variables as ring x.
  -- MES will document this and maybe change its name
  R := ring x;
  if numgens R != numgens S 
  then error "newCoordinateSystem requires input rings to have the same number of variables";
     -- probably should also check:
     -- (a) entries of 'x' are linear and independent
     -- (b) what if R,S, are quotient rings
  toS := map(S,R,vars S);
  m := contract(transpose vars R, x);
  n := complement m | m;
  ( map(S,R,vars S * toS n), map(R,S,vars R * n^(-1)) )
  )

lift(Ideal,RingElement) := Ideal => opts -> (I,S) -> (
     -- provisional, just for quotient rings
     T := ring I;
     if T === S then I
     else ideal lift(I.generators,S,opts) + ideal presentation(S,T));

-- computes the ideal of p x p permanents of the matrix M
permanents = method()
permanents(ZZ,Matrix) := Ideal => (p,M) -> (
     r:=numgens target M;
     c:=numgens source M;
     xxX := symbol xxX;
     R1:=ZZ/2[xxX_(1,1)..xxX_(r,c)];
     M1:= transpose genericMatrix(R1,xxX_(1,1),c,r);
     D1:= minors(p,M1);
     R2:=ZZ[xxX_(1,1)..xxX_(r,c)];
     D1=substitute(D1,R2);
     F := map(ring M, R2,flatten entries M);
     F D1)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
