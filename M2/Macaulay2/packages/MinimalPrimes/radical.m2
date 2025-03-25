-- Computing the radical of an ideal
-- Will be part of the PrimaryDecomposition package (most likely)

-- used in rad
flatt2 = (I, m) -> (
    -- First create a new ring with correct order
    local ones;
    local mm;
    local F;
    R := ring I;
    n := numgens R;
    vars1 := support m;
    d := #vars1;
    vars2 := support ((product gens R)//m);
    RU := (coefficientRing R) monoid([vars2, vars1,
	    MonomialOrder => ProductOrder{n - d, d},
	    MonomialSize  => 16]);
    J := substitute(I, RU);
    -- Collect lead coefficients of GB
    JG := J;
    leads := leadTerm(1, gens gb J);
    --(mons,cs) = coefficients(toList(0..n-d-1),leads);
    (mons, cs) := coefficients(leads, Variables => toList(0..n-d-1));
    monsid := trim ideal select(flatten entries mons, f -> f != 1);
    monsid = substitute(monsid, R);
    --monset = set flatten entries gens monsid;
    monset := new MutableHashTable;
    scan(flatten entries gens monsid, m -> monset#m = {});
    monslist := flatten entries substitute(mons, R);
    p := positions(monslist, f -> monset#?f);
    cs = transpose cs;
    scan(p, i -> monset#(monslist#i) = substitute(ideal(compress transpose (cs_{i})),R));
    monset)

-- used in radical00
getMinimalPoly = method()
getMinimalPoly(Ideal, RingElement, RingElement) := (I, u, x) -> (
    ux := u * x;
    elimvars := select(gens ring I, y -> ux % y != 0);
    J := eliminate(I, elimvars);
    d := min apply(numgens J, i -> degree(x, J_i));
    --error "getminpoly";
    fs := select(1, flatten entries gens J, f -> degree(x,f) === d);
    fs#0)

-- used in radical00
getSeparablePart = method()
getSeparablePart(RingElement, RingElement, RingElement) := (f, u, x) -> (
    product select(last \ factors f, g1 -> degree(x, g1) > 0))

-- used in rad
radical00 = method()
radical00(Ideal, RingElement) := (I, u) -> (
    -- For each variable not in u, compute the
    -- squarefree part (separable part)
    v := select(gens ring I, x -> u % x != 0);
    newelems := {};
    scan(v, x -> (
	    -- there are THREE problems here!
	    -- (a) use linear algebra
	    -- (b) char p
	    -- (c) f might not be the smallest eqn in var v_i.
	    --<< "about to get minpoly " << toString I << " u = " << toString u << " x = " << toString x << endl;
	    f := getMinimalPoly(I, u, x);
	    g := getSeparablePart(f, u, x);
	    if f != g then newelems = append(newelems, g)));
    --error "in radical00";
    if #newelems > 0 then I = I + ideal newelems;
    I)

-----------------------------------------------------------------------------
-- Radical of ideals
-----------------------------------------------------------------------------
-- Based on the Macaulay (classic) scripts written by D. Eisenbud.
-- Translated from Macaulay to Macaulay2 by M. Stillman:
--   radical --> radical
--   unmixed_radical --> radical(I,Unmixed=>true)

radical = method(
    Options => {
	CompleteIntersection => null,
	Strategy             => null,
	Unmixed              => false
	}
    )

-- Helper for radical
radical Ideal := Ideal => opts -> I -> (
    -- TODO: what does the Unmixed boolean add to strategy?
    strategy := if opts.Unmixed then Unmixed else opts.Strategy;
    key := (radical, Ideal);

    computation := (cacheValue symbol radical) (I -> runHooks(key, (opts, I), Strategy => strategy));
    C := computation I;

    if C =!= null then C else if strategy === null
    then error("no applicable method for ", toString key)
    else error("assumptions for radical strategy ", toString strategy, " are not met"))

--------------------------------------------------------------------
-- strategies for radical
--------------------------------------------------------------------

algorithms#(radical, Ideal) = new MutableHashTable from {
    Unmixed => (opts, I) -> if opts.Unmixed then unmixedradical I else radical1 I,

    Decompose => (opts, I) -> (
	C := minimalPrimes I;
	if #C === 0 then ideal 1_(ring I) else intersect C),

    CompleteIntersection => (opts, I) -> (
	-- unmixed radical, another Eisenbud-Huneke-Vasconcelos method
	-- to compute radical(m), given a max regular sequence n contained in m.
	if not instance(CI := opts.CompleteIntersection, Ideal)
	or not ring I === ring CI
	then return null;
	c := numgens CI;  -- we assume that this is a complete intersection...
	K := CI : minors(c, jacobian CI);  -- maybe work mod CI?
	K : (K : I)), -- do these mod K?

    Monomial => (opts, I) -> (
	if not isMonomialIdeal I
	then return null;
	cast := if instance(I, MonomialIdeal) then monomialIdeal else ideal;
	cast newMonomialIdeal(ring I, rawRadical raw monomialIdeal I)),
    }

-- Installing hooks for radical(Ideal)
scan({Unmixed, Decompose, CompleteIntersection, Monomial}, strategy ->
    addHook(key := (radical, Ideal), algorithms#key#strategy, Strategy => strategy))

--------------------------------------------------------------------
-- helper routines for radical algorithms
--------------------------------------------------------------------

unmixedradical = I -> (
    -- First lift I to a polynomial ring...
    A := ring I;
    f := presentation A;
    B := ring f;
    I = lift(I, B);
    if I != ideal 1_B and I.generators =!= 0 then (
	c := codim I;
	size := 1;
	R := A;
	while size <= c do (
	    R = B/I;
	    dR := jacobian R;
	    J := minors(size,dR);
	    --
	    g1 := leadTerm generators gb presentation R;
	    g1 = g1 | lift(generators leadTerm J, B);
	    --
	    if c < codim ideal g1 then size = size + 1 else (
		-- we would like the next line to read:
		-- I = annihilator J;
		I = ideal syz(transpose mingens J, SyzygyRows=>1);
		I = lift(I, B));
	    );
	);
    trim(I * A))

radical1 = I0 -> (
    -- possibly massage input, by removing obvious extraneous powers?
    -- at least of the monomials in the ideal?
    R := ring I0;
    I := removeLowestDimension I0;
    J := unmixedradical saturate(I0, I);
    if I == ideal 1_R then J else intersect(J, radical1 I))

-- TODO: add this one
-- MES: I grabbed this code from another file
zerodimRadical = I -> (
    -- assumption: dim I is 0
    if dim I != 0 then error "expected zero dimensional ideal";
    X := gens ring I;
    trim ideal gens gb(I + sum apply(#X, i -> eliminate(drop(X, {i, i}), I))))

-- TODO: add this one
rad = method()
rad Ideal := (Iorig) -> (
    -- returns the radical of Iorig
    (I,F,G) := flattenRing(Iorig,Result=>(,,));
    R := ring I;
    n := numgens I;
    radI := ideal(1_R);
    while codim I <= n do (
	u := independentSets(I,Limit=>1);
	u = if #u === 0 then 1_R else first u;
	--<< " size(u) = " << # support u << endl;
	J := radical00(I,u);
	h := flatt2(J,u);
	h = (intersect values h)_0;
	radJ := saturate(J,h);
	radI = intersect(radI,radJ);
	if u === 1 then break;
	h = flatt2(I,u);
	h = (intersect values h)_0;
	if h != 1 then h = product \\ last \ factors h;
	I = I + ideal(h));
    if ring I =!= ring Iorig then radI = G radI;
    trim radI)

rad(Ideal,ZZ) := (Iorig, codimlimit) -> (
    -- returns the radical of Iorig
    (I,F,G) := flattenRing(Iorig,Result=>(,,));
    R := ring I;
    n := numgens I;
    radI := ideal(1_R);
    c := codim I;
    --<< "R0 = " << toExternalString ring I << endl << flush;
    --<< "J0 = " << toString I << endl << flush;
    while codim I <= c + codimlimit do (
	u := independentSets(I, Limit => 1);
	u = if #u === 0 then 1_R else first u;
	--<< " size(u) = " << # support u << endl;
	J := radical00(I, u);
	h := flatt2(J, u);
	h = (intersect values h)_0;
	radJ := saturate(J, h);
	radI = intersect(radI, radJ);
	if u === 1 then break;
	h = flatt2(I, u);
	h = (intersect values h)_0;
	if h != 1 then h = product \\ last \ factors h;
	I = I + ideal h);
    if ring I =!= ring Iorig then radI = G radI;
    trim radI)

------------------------------
-- Radical containment -------
------------------------------

-- Determine whether g is in radical I
radicalContainment = method(TypicalValue => Boolean, Options => { Strategy => "Rabinowitsch" })
-- Returns the first index i such that I_i is not in the radical of J,
-- and null, if none
-- another way to do something almost identical: select(1, I_*, radFcn J)
radicalContainment(Ideal, Ideal)       := Boolean => opts -> (I, J) -> (rad := radFcn J; position(I_*, g -> not rad g))
-- Returns true if g is in the radical of I.
-- Assumption: I is in a monomial order for which you are happy to compute GB's.
--radicalContainment(RingElement, Ideal) := (g, I) -> opts -> (radFcn I) g
radicalContainment(RingElement, Ideal) := Boolean => opts -> (g, I) -> (
     R := ring I;
     if ring g =!= R then error "Expected same ring";
     if isHomogeneous I and opts.Strategy == "Kollar" then (
	  -- Radical membership test for homogeneous ideals
	  -- Based on Theorem 1.5 in https://www.jstor.org/stable/pdf/1990996.pdf
	  -- should assume degrees are > 2
	  degs := reverse sort((flatten entries mingens I)/degree/sum);
	  n := min(#support I, #degs);
	  degs = drop(degs_{0..<n}, -1) | {last degs};
	  if debugLevel > 0 then print("Upper bound of " | toString(product degs) | " for radical");
	  for i to floor(log_2 product degs) do (
	       if debugLevel > 0 then print("Testing power " | toString(2^(i+1)));
	       g = g^2 % I;
	       if g == 0 then return true;
	  );
	  false
     ) else (
	  n = numgens R;
	  S := (coefficientRing R) (monoid[Variables=>n+1,MonomialSize=>16]);
	  mapto := map(S,R,submatrix(vars S,{0..n-1}));
	  I = mapto I;
	  g = mapto g;
	  J := I + ideal(g*S_n-1);
	  1_S % J == 0_S
     )
)

-- helper function for 'radicalContainment'
radFcn = (cacheValue "RadicalContainmentFunction") (I -> (
    R := ring I;
    n := numgens R;
    S := (coefficientRing R) (monoid[Variables => n + 1, MonomialSize => 16]);
    mapto := map(S, R, submatrix(vars S, {0..n-1}));
    I = mapto I;
    -- here is a GB of I!
    A := S/I;
    g -> (g1 := promote(mapto g, A); g1 == 0 or ideal(g1 * A_n - 1) == 1)))

--------------------------------------------------------------------
----- Development section
--------------------------------------------------------------------

end--

restart
debug MinimalPrimes

R = QQ[x,y,z]/(x^2+y^2+z^2)
I = ideal"x,y"
rad I

R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
time rad I
time intersect decompose I

R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    s*u-b*v,
    t*v-s*w,
    v*x-u*y,
    w*y-v*z)
time rad I
time intersect decompose I
time decompose I

R = ZZ/32003[a,b,c,d,e,f,g,h]
I = ideal(
    h+f+e-d-a,
    2*f*b+2*e*c+2*d*a-2*a^2-a-1,
    3*f*b^2+3*e*c^2-3*d*a^2-d+3*a^3+3*a^2+4*a,
    6*g*e*b-6*d*a^2-3*d*a-d+6*a^3+6*a^2+4*a,
    4*f*b^3+4*e*c^3+4*d*a^3+4*d*a-4*a^4-6*a^3-10*a^2-a-1,
    8*g*e*c*b+8*d*a^3+4*d*a^2+4*d*a-8*a^4-12*a^3-14*a^2-3*a-1,
    12*g*e*b^2+12*d*a^3+12*d*a^2+8*d*a-12*a^4-18*a^3-14*a^2-a-1,
    -24*d*a^3-24*d*a^2-8*d*a+24*a^4+36*a^3+26*a^2+7*a+1)
time decompose I
time rad I -- not good yet

R = ZZ/32003[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z, MonomialSize=>8]
I = ideal(
    -a*b-a*d+2*a*h,
    a*d-b*d-c*f-2*a*h+2*b*h+2*c*k,
    a*b-a*d-2*b*h+2*d*h-2*c*k+2*f*k+2*g*l,
    a*c-2*c*s-a*t+2*b*t,
    a*c-c*s-2*a*t+b*t,
    -d-3*s+4*u,
    -f-3*t+4*v,
    -g+4*w,
    -a+2*x,
    -b^2-c^2+2*b*x+2*c*y,
    -d^2-f^2-g^2+2*d*x+2*f*y+2*g*z)
time decompose I
time rad I -- not good yet

R0 = (ZZ/32003)[w_(0,0), w_(0,1), w_(0,2), v, u, z, Degrees => {6:1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {3:1}, GRevLex => {1}, GRevLex => {2:1}, Position => Up}, DegreeRank => 1]
J0 = ideal(w_(0,0)*u-v^2-2*v*z-z^2,w_(0,2)^2+3378*w_(0,0)*v+3378*w_(0,0)*z+8595*w_(0,1)*v-9157*w_(0,1)*u+8595*w_(0,1)*z+8990*w_(0,2)*v+502*w_(0,2)*u+13486*w_(0,2)*z-14160*v^2-7666*v*u+6065*v*z+911*u^2+6866*u*z-14748*z^2,w_(0,1)*w_(0,2)-2296*w_(0,0)*v-2296*w_(0,0)*z+10183*w_(0,1)*v-10093*w_(0,1)*u+12431*w_(0,1)*z-15664*w_(0,2)*v+1481*w_(0,2)*u-15867*w_(0,2)*z+13406*v^2+2557*v*u-3813*v*z+13277*u^2+11829*u*z+6482*z^2,w_(0,0)*w_(0,2)+6955*w_(0,0)*v+9203*w_(0,0)*z+4020*w_(0,1)*v+8014*w_(0,1)*u+4020*w_(0,1)*z-2330*w_(0,2)*v-4665*w_(0,2)*u-2329*w_(0,2)*z+2106*v^2-15232*v*u+5834*v*z+5406*u^2+4443*u*z+5976*z^2,w_(0,1)^2+1511*w_(0,0)*v+1511*w_(0,0)*z-887*w_(0,1)*v-1020*w_(0,1)*u-1293*w_(0,1)*z-13362*w_(0,2)*v+5279*w_(0,2)*u-13362*w_(0,2)*z-2150*v^2+807*v*u-1705*v*z+8408*u^2-15068*u*z+9651*z^2,w_(0,0)*w_(0,1)+14147*w_(0,0)*v+13944*w_(0,0)*z+2558*w_(0,1)*v+5113*w_(0,1)*u+2559*w_(0,1)*z+213*w_(0,2)*v+426*w_(0,2)*u+213*w_(0,2)*z+14843*v^2+7373*v*u+3383*v*z+5350*u^2-11815*u*z-11663*z^2,w_(0,0)^2-214*w_(0,0)*v-212*w_(0,0)*z-12*w_(0,1)*v-24*w_(0,1)*u-12*w_(0,1)*z-w_(0,2)*v-2*w_(0,2)*u-w_(0,2)*z-518*v^2-184*v*u-1062*v*z-25*u^2-244*u*z-543*z^2,u^3-u*z^2,v*u^2-v*z^2+u^2*z-z^3,-w_(0,0)*z^2+v^2*u+2*v*u*z+u^2*z+u*z^2-z^3,-214*w_(0,0)*v*z^2-212*w_(0,0)*z^3-12*w_(0,1)*v*z^2-24*w_(0,1)*u*z^2-12*w_(0,1)*z^3-w_(0,2)*v*z^2-2*w_(0,2)*u*z^2-w_(0,2)*z^3+v^4+4*v^3*z-512*v^2*z^2-184*v*u*z^2-1058*v*z^3-26*u^2*z^2-244*u*z^3-541*z^4,12778*w_(0,0)*v*z^2+12517*w_(0,0)*z^3+708*w_(0,1)*v*z^2+1415*w_(0,1)*u*z^2+708*w_(0,1)*z^3+11078*w_(0,2)*v^2*u+15899*w_(0,2)*v*u^2-9847*w_(0,2)*v*u*z+59*w_(0,2)*v*z^2+8411*w_(0,2)*u^3-5026*w_(0,2)*u^2*z+11196*w_(0,2)*u*z^2+59*w_(0,2)*z^3-1277*v^4-11258*v^3*u-5108*v^3*z-9055*v^2*u^2+946*v^2*u*z-9103*v^2*z^2+11814*v*u^3-3556*v*u^2*z+14519*v*u*z^2-6512*v*z^3+8502*u^4+8105*u^3*z+10968*u^2*z^2+5855*u*z^3-1501*z^4,2393*w_(0,0)*v*z^2+2607*w_(0,0)*z^3+121*w_(0,1)*v*z^2+228*w_(0,1)*u*z^2+121*w_(0,1)*z^3-4923*w_(0,2)*v^2*u+1232*w_(0,2)*v*u^2-9846*w_(0,2)*v*u*z+10*w_(0,2)*v*z^2-4923*w_(0,2)*u^3-3691*w_(0,2)*u^2*z-4904*w_(0,2)*u*z^2+10*w_(0,2)*z^3+12274*v^4-1038*v^3*u-14910*v^3*z+6619*v^2*u^2-4649*v^2*u*z+14818*v^2*z^2-15939*v*u^3-2607*v*u^2*z-4344*v*u*z^2-4158*v*z^3-6315*u^4-3854*u^3*z+9218*u^2*z^2-133*u*z^3-13943*z^4,9597*w_(0,0)*v*z^2+12583*w_(0,0)*z^3-6552*w_(0,1)*v*z^2-13104*w_(0,1)*u*z^2-6552*w_(0,1)*z^3-546*w_(0,2)*v*z^2+w_(0,2)*u^3-1093*w_(0,2)*u*z^2-546*w_(0,2)*z^3+546*v^4+1571*v^3*u+2184*v^3*z+2819*v^2*u*z+8475*v^2*z^2-804*v*u^2*z-3530*v*u*z^2-810*v*z^3+15109*u^2*z^2-5535*u*z^3-4661*z^4)
time decompose J0 -- this time, this one is very bad
time rad J0 -- and this one is very good

R0 = (ZZ/32003)[w_(0,0), w_(0,1), w_(0,2), w_(0,3), w_(0,4), w_(0,5), v, u, z, Degrees => {9:1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, GRevLex => {6:1}, GRevLex => {1}, GRevLex => {2:1}, Position => Up}, DegreeRank => 1]
J0 = ideal(w_(0,2)*v+9428*w_(0,2)*z-6100*w_(0,3)*v+7354*w_(0,3)*u-6312*w_(0,3)*z+1840*w_(0,4)*v+8112*w_(0,4)*u+1840*w_(0,4)*z+6100*w_(0,5)*u-10532*w_(0,5)*z-3247*v*z+13997*u*z+14070*z^2,w_(0,1)*z-15633*w_(0,2)*z-498*w_(0,3)*v+9803*w_(0,3)*u+15078*w_(0,3)*z-3827*w_(0,4)*v-11798*w_(0,4)*u-3827*w_(0,4)*z+498*w_(0,5)*u+3646*w_(0,5)*z-563*v*z+334*u*z+9246*z^2,w_(0,1)*u+12802*w_(0,2)*u-11891*w_(0,2)*z-7319*w_(0,3)*v-8378*w_(0,3)*u+6728*w_(0,3)*z+1264*w_(0,4)*v-10334*w_(0,4)*u+1264*w_(0,4)*z-4756*w_(0,5)*v+4741*w_(0,5)*u-6147*w_(0,5)*z+623*v^2-11023*v*u+12718*v*z+9112*u^2-8759*u*z-13856*z^2,w_(0,1)*v-w_(0,2)*u+5868*w_(0,2)*z-12083*w_(0,3)*v+3765*w_(0,3)*u-1469*w_(0,3)*z+13219*w_(0,4)*v+9702*w_(0,4)*u+13219*w_(0,4)*z+12083*w_(0,5)*u+4653*w_(0,5)*z-2810*v*z+10445*u*z+13715*z^2,w_(0,0)*z+9765*w_(0,2)*z+12581*w_(0,3)*v-13568*w_(0,3)*u-13609*w_(0,3)*z-9392*w_(0,4)*v+2096*w_(0,4)*u-9392*w_(0,4)*z-12581*w_(0,5)*u-8299*w_(0,5)*z+3373*v*z-10779*u*z+9043*z^2,w_(0,0)*u-v^2-2*v*z-z^2,w_(0,0)*v+12802*w_(0,2)*u+10347*w_(0,2)*z+12103*w_(0,3)*v+5190*w_(0,3)*u-11666*w_(0,3)*z+10656*w_(0,4)*v-12430*w_(0,4)*u+10656*w_(0,4)*z-4756*w_(0,5)*v-14681*w_(0,5)*u+2152*w_(0,5)*z+623*v^2-11023*v*u+9346*v*z+9112*u^2+2020*u*z+9105*z^2,w_(0,5)^2+15447*w_(0,2)*u-15064*w_(0,2)*z-5900*w_(0,3)*v+7864*w_(0,3)*u+5296*w_(0,3)*z+2354*w_(0,4)*v-6631*w_(0,4)*u+8493*w_(0,4)*z+1559*w_(0,5)*v+4095*w_(0,5)*u-3080*w_(0,5)*z+8124*v^2+4156*v*u+15167*v*z+9462*u^2-4081*u*z+4687*z^2,w_(0,4)*w_(0,5)-4893*w_(0,2)*u-7303*w_(0,2)*z+4388*w_(0,3)*v-14001*w_(0,3)*u-15376*w_(0,3)*z+10434*w_(0,4)*v+6352*w_(0,4)*u-13392*w_(0,4)*z+1462*w_(0,5)*v+6691*w_(0,5)*u-320*w_(0,5)*z+10453*v^2+8164*v*u-7097*v*z+14362*u^2-3443*u*z+5824*z^2,w_(0,3)*w_(0,5)+4583*w_(0,2)*u+11704*w_(0,2)*z+15151*w_(0,3)*v+936*w_(0,3)*u-2505*w_(0,3)*z+13918*w_(0,4)*v-3035*w_(0,4)*u+1206*w_(0,4)*z+4703*w_(0,5)*v+954*w_(0,5)*u-6710*w_(0,5)*z-3364*v^2-15337*v*u-4273*v*z-9965*u^2-1583*u*z-9705*z^2,w_(0,2)*w_(0,5)-2871*w_(0,2)*u+98*w_(0,2)*z-4040*w_(0,3)*v-936*w_(0,3)*u+6560*w_(0,3)*z-8857*w_(0,4)*v+50*w_(0,4)*u-14930*w_(0,4)*z+10638*w_(0,5)*v-1035*w_(0,5)*u-10770*w_(0,5)*z+14782*v^2+2268*v*u-9743*v*z-9527*u^2+1701*u*z-7994*z^2,w_(0,1)*w_(0,5)+2493*w_(0,2)*u+7064*w_(0,2)*z+2756*w_(0,3)*v+4383*w_(0,3)*u+11875*w_(0,3)*z+9675*w_(0,4)*v+3006*w_(0,4)*u+10221*w_(0,4)*z-12286*w_(0,5)*v-15247*w_(0,5)*u-5803*w_(0,5)*z-15601*v^2-15612*v*u-9085*v*z+13580*u^2+2256*u*z+2869*z^2,w_(0,0)*w_(0,5)-6095*w_(0,2)*u+3886*w_(0,2)*z+9877*w_(0,3)*v+1498*w_(0,3)*u+7566*w_(0,3)*z+793*w_(0,4)*v-13128*w_(0,4)*u+793*w_(0,4)*z-609*w_(0,5)*v+13572*w_(0,5)*u-689*w_(0,5)*z-2500*v^2+13405*v*u-15*v*z-15575*u^2-3850*u*z+1503*z^2,w_(0,4)^2+2493*w_(0,2)*u+2237*w_(0,2)*z+8396*w_(0,3)*v+9175*w_(0,3)*u-2784*w_(0,3)*z-5847*w_(0,4)*v+15793*w_(0,4)*u-6449*w_(0,4)*z-12370*w_(0,5)*v+11116*w_(0,5)*u-10951*w_(0,5)*z-15601*v^2-15612*v*u-9127*v*z+13580*u^2-333*u*z-7059*z^2,w_(0,3)*w_(0,4)-4890*w_(0,2)*u-6730*w_(0,2)*z-9898*w_(0,3)*v+898*w_(0,3)*u+4322*w_(0,3)*z-49*w_(0,4)*v+7144*w_(0,4)*u-4564*w_(0,4)*z+3060*w_(0,5)*v-6580*w_(0,5)*u+13591*w_(0,5)*z+1606*v^2+12857*v*u+6897*v*z-2759*u^2+15411*u*z+11748*z^2,w_(0,2)*w_(0,4)-6095*w_(0,2)*u-7125*w_(0,2)*z+5427*w_(0,3)*v-7483*w_(0,3)*u+11208*w_(0,3)*z-832*w_(0,4)*v+13346*w_(0,4)*u-728*w_(0,4)*z-609*w_(0,5)*v-14065*w_(0,5)*u-2901*w_(0,5)*z-2500*v^2+13405*v*u+10619*v*z-15575*u^2-12972*u*z-7801*z^2,w_(0,1)*w_(0,4)-10213*w_(0,2)*z-4503*w_(0,3)*v-13981*w_(0,3)*u+8262*w_(0,3)*z+3927*w_(0,4)*v+1973*w_(0,4)*u+3915*w_(0,4)*z-w_(0,5)*v+4503*w_(0,5)*u+1397*w_(0,5)*z-15984*v*z+2266*u*z-8730*z^2,w_(0,0)*w_(0,4)+8104*w_(0,2)*z-9352*w_(0,3)*v+9787*w_(0,3)*u+10502*w_(0,3)*z+6428*w_(0,4)*v-4023*w_(0,4)*u+6429*w_(0,4)*z+9351*w_(0,5)*u+7523*w_(0,5)*z-2885*v*z+635*u*z+13251*z^2,w_(0,3)^2-15809*w_(0,2)*u+8153*w_(0,2)*z+1149*w_(0,3)*v-15556*w_(0,3)*u+7803*w_(0,3)*z+655*w_(0,4)*v-2143*w_(0,4)*u+1860*w_(0,4)*z-14976*w_(0,5)*v+11885*w_(0,5)*u+9441*w_(0,5)*z-7036*v^2+5335*v*u+6413*v*z-2573*u^2-4217*u*z-12654*z^2,w_(0,2)*w_(0,3)-6641*w_(0,2)*u-14611*w_(0,2)*z+14*w_(0,3)*v+1591*w_(0,3)*u-5001*w_(0,3)*z+15957*w_(0,4)*v-12599*w_(0,4)*u-15372*w_(0,4)*z+12325*w_(0,5)*v+11851*w_(0,5)*u+4957*w_(0,5)*z-13598*v^2-14767*v*u+6290*v*z-13305*u^2+143*u*z+1580*z^2,w_(0,1)*w_(0,3)-6095*w_(0,2)*u-2059*w_(0,2)*z-1342*w_(0,3)*v-7379*w_(0,3)*u+15137*w_(0,3)*z+15278*w_(0,4)*v+4640*w_(0,4)*u+15219*w_(0,4)*z-621*w_(0,5)*v-7296*w_(0,5)*u-595*w_(0,5)*z-2500*v^2+13405*v*u-11119*v*z-15575*u^2+5992*u*z-279*z^2,w_(0,0)*w_(0,3)+13692*w_(0,2)*z+12596*w_(0,3)*v-8944*w_(0,3)*u-8640*w_(0,3)*z-14280*w_(0,4)*v+8346*w_(0,4)*u-14280*w_(0,4)*z-w_(0,5)*v-12608*w_(0,5)*u+7702*w_(0,5)*z+1250*v*z-4992*u*z-5323*z^2,w_(0,2)^2-14163*w_(0,2)*z+158*w_(0,3)*v-8233*w_(0,3)*u+15729*w_(0,3)*z+6253*w_(0,4)*v-10485*w_(0,4)*u+6239*w_(0,4)*z-w_(0,5)*v-158*w_(0,5)*u-8843*w_(0,5)*z+1045*v*z-3690*u*z+5332*z^2,w_(0,1)*w_(0,2)-8191*w_(0,2)*z+12588*w_(0,3)*v-651*w_(0,3)*u-12564*w_(0,3)*z+9610*w_(0,4)*v+6435*w_(0,4)*u+9611*w_(0,4)*z-12589*w_(0,5)*u-6630*w_(0,5)*z-6870*v*z-15774*u*z-12316*z^2,w_(0,0)*w_(0,2)-4739*w_(0,2)*z-15164*w_(0,3)*v+4760*w_(0,3)*u-239*w_(0,3)*z-14811*w_(0,4)*v+542*w_(0,4)*u-14811*w_(0,4)*z+15164*w_(0,5)*u-13325*w_(0,5)*z+1455*v*z+13761*u*z+12173*z^2,w_(0,1)^2-4739*w_(0,2)*z-15164*w_(0,3)*v+4760*w_(0,3)*u-239*w_(0,3)*z-14811*w_(0,4)*v+542*w_(0,4)*u-14811*w_(0,4)*z+15164*w_(0,5)*u-13325*w_(0,5)*z+1455*v*z+13761*u*z+12173*z^2,w_(0,0)*w_(0,1)-6943*w_(0,2)*z-5602*w_(0,3)*v-2449*w_(0,3)*u+10613*w_(0,3)*z+5667*w_(0,4)*v-12093*w_(0,4)*u+5667*w_(0,4)*z+5602*w_(0,5)*u-14178*w_(0,5)*z-2684*v*z+13663*u*z+4824*z^2,w_(0,0)^2-w_(0,2)*u+12473*w_(0,2)*z+6841*w_(0,3)*v-4867*w_(0,3)*u-4785*w_(0,3)*z-13219*w_(0,4)*v-4192*w_(0,4)*u-13219*w_(0,4)*z-6841*w_(0,5)*u-15405*w_(0,5)*z-6746*v*z-10445*u*z+13918*z^2,u^3-u*z^2,v*u^2-v*z^2+u^2*z-z^3,6000*w_(0,3)*v*z+9917*w_(0,3)*u*z+6000*w_(0,3)*z^2+14765*w_(0,4)*v^2-14900*w_(0,4)*v*u-2473*w_(0,4)*v*z-655*w_(0,4)*u^2+3360*w_(0,4)*u*z+14765*w_(0,4)*z^2-9141*w_(0,5)*v^2-12538*w_(0,5)*v*u+5929*w_(0,5)*v*z+1645*w_(0,5)*u^2+3268*w_(0,5)*u*z+15070*w_(0,5)*z^2+3535*v^3-3543*v^2*u+6807*v^2*z-14272*v*u^2-3289*v*u*z-9652*v*z^2+5630*u^3+15772*u^2*z-7022*u*z^2-12924*z^3,-1940*w_(0,3)*v*z+10021*w_(0,3)*u*z-1940*w_(0,3)*z^2+1510*w_(0,4)*v^2-7484*w_(0,4)*v*u+3019*w_(0,4)*v*z+5072*w_(0,4)*u^2-1180*w_(0,4)*u*z+1509*w_(0,4)*z^2+7936*w_(0,5)*v^2+3897*w_(0,5)*v*u+4643*w_(0,5)*v*z-9145*w_(0,5)*u^2-1746*w_(0,5)*u*z-3293*w_(0,5)*z^2-15191*v^3+12068*v^2*u+2749*v^2*z-5858*v*u^2-13155*v*u*z+4612*v*z^2+8077*u^3+7426*u^2*z-2400*u*z^2-13328*z^3,-4194*w_(0,3)*v*z-7759*w_(0,3)*u*z-4194*w_(0,3)*z^2+424*w_(0,4)*v^2-9462*w_(0,4)*v*u+860*w_(0,4)*v*z-2108*w_(0,4)*u^2+15086*w_(0,4)*u*z+436*w_(0,4)*z^2+5371*w_(0,5)*v^2-1192*w_(0,5)*v*u+11051*w_(0,5)*v*z-7484*w_(0,5)*u^2+4012*w_(0,5)*u*z+5680*w_(0,5)*z^2+11583*v^3+15934*v^2*u-10306*v^2*z+556*v*u^2+3353*v*u*z-15662*v*z^2+10530*u^3+13222*u^2*z+7884*u*z^2+6227*z^3)
numgens J0
codim J0
numgens R0
time rad J0 -- very quick
time decompose J0

restart
loadPackage "PrimaryDecomposition"
debug PrimaryDecomposition
kk = ZZ/32003 -- from Greuel-Laplagne-Seelisch arXiv:0904.3561v1
S = kk[x,y]
I = ideal"55x8+66y2x9+837x2y6-75y4x2-70y6-97y7x2"
J = ideal jacobian I
rad1 = time rad J
rad2 = time intersect decompose J
rad1 == rad2 -- NO!!  So probably rad has a bug?
rad J == intersect decompose J
(gens J) % rad2
-- I think that rad2 might not be correct?
Slex = kk[x,y,MonomialOrder=>{1,1}]
J1 = sub(J,Slex)
rad1 = sub(rad1,Slex)
rad2 = sub(rad2,Slex)
see ideal gens gb J1
see ideal gens gb rad1
see ideal gens gb rad2

F0 = (ideal selectInSubring(1,gens gb J1))_0
F1 = (ideal selectInSubring(1,gens gb rad1))_0
F2 = (ideal selectInSubring(1,gens gb rad2))_0
saturate(J1,rad1)
saturate(J1,rad2)
decompose rad1
netList (oo/(i -> gens gb i))
decompose rad2
netList (oo/(i -> gens gb i))

rad3 = J : det jacobian J
rad3 == intersect decompose rad3
rad3 == rad1
(gens rad1) % rad2
(gens rad2) % rad1
degree rad1
degree rad2
rad1 : rad2
rad2 : rad1
decompose J
netList oo

(decompose rad1)/(i -> ideal gens gb i)
netList oo
(decompose rad2)/(i -> ideal gens gb i)
netList oo
rad1 == intersect decompose rad1
rad2 == intersect decompose rad2

---------------------------------------------------
-- New tests as of 6/10/2010

restart
load "radical.m2"


R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
codim I
U = first independentSets I
H = flatt2(I,U)
peek H
F = (intersect values H)_0
C1 = saturate(I,F)
I : C1
