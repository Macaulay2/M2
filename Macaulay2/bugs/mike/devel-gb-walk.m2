needsPackage "FourierMotzkin"
load "gfanInterface.m2"

bd = (inL,L) -> (
  M := flatten apply(#inL, i -> (
    f := inL#i;
    g := L#i;
    m = first exponents(f);
    apply(exponents(g-f), e -> m-e)));
  transpose matrix unique select(M, v -> any(v, i -> i < 0)))

interm = (w,ing,g) -> (
     -- take all terms c*x^e from g s.t. log(ing) - e is a scalar multiple of w.
     isScalarMult := (e,f) -> (
	  minors(2,matrix{e,f}) == 0
	  );
     e := first exponents ing;
     eg := exponents(g);
     p := positions(eg, f -> isScalarMult(e-f,w));
     sum (terms g)_p
     )

inwmon = (w,inL,L) -> ideal apply(#L, i -> interm(w,inL#i,L#i))
     -- w is an exponent vector
     -- take all of the terms u of an element g of L st log(in(g)-u)
     -- is a (rational) multiple of w.

-- These are normalized differences (u is normalized)
dmon = method()
dmon List := (u) -> (
     a := position(u, ui -> ui != 0);
     u = u/(u#a);
     {join({a, sum u},-reverse u), u}
     )

findwtvec = (inL,L) -> (
     apply(#inL, i -> (
	       first fourierMotzkin groebnerCone({inL#i},{L#i})
	       ))
     )
computeNextW = (w,inL,L) -> (
     -- does the case grevlex --> lex
     S = entries transpose bd(inL, L);
     Sp = partition(s -> position(s, i -> i != 0), S);
     Sp1 = hashTable apply(pairs Sp, (i,L) -> (i,select(L, s -> s#i < 0)));
     Sp2 = hashTable apply(pairs Sp1, (i,L) -> (
	       -- sort L, normalize elements
	       (i,sort(L/dmon))
	       ));
     -- remove all elements less than or equal to w (if w not null)
     if w =!= null then (
	  wmon = dmon w;
	  Sp2orig = Sp2;
	  Sp2 = hashTable apply(pairs Sp2, (i,L) -> (i, if i < wmon_0_0 then {}
		    else if i > wmon_0_0 then L
		    else select(L, d -> wmon < d)))
	  );
     q = select(keys Sp2, k -> #Sp2#k > 0);
     if #q == 0 
     then null
     else (
	  p = max q;
	  Sp2#p#0#1
	  )
     )

groebnerWalktoLex = method()
groebnerWalktoLex(Ideal,Ring) := (I,Rlast) -> (
     R := ring I;
     w = null;
     G = gb I;
     L = flatten entries gens G;
     inL = L/leadTerm;
     while (w = computeNextW(w,inL,L)) =!= null do (
	  << "-- start loop w = " << w << endl;
	  inwG = substitute(inwmon(w,inL,L), Rlast);
	  << "   inwG = " << inwG << endl;
	  G1 = gens gb inwG;
	  inG1 = substitute(leadTerm G1,R);
	  G1 = substitute(G1,R);
	  G1 = substitute(gens gb inwG,R);
	  << "   G1   = " << netList flatten entries G1 << endl;
	  << "   inG1 = " << netList flatten entries inG1 << endl;
	  G2 = G1 - (G1 % G);
	  L = flatten entries G2;
	  inL = (flatten entries inG1)/leadTerm;
	  tau = weightVector(inL,L);
	  R = (coefficientRing R)[gens R, MonomialOrder=>{Weights=>tau,Lex}];
	  G = forceGB substitute(G2, R);
	  L = flatten entries gens G;
	  -- this should be the same as inL above:
	  inL2 = apply(inL, g -> substitute(g,R));
	  inL = L/leadTerm;
	  if monomialIdeal inL != monomialIdeal inL2
	  then error "two initial ideals are different";	  
	  );
     forceGB substitute(gens G,Rlast)
     )
     
end
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
kk = ZZ/101
kk = QQ
R = kk[x,y,z]
R = kk[x,y,z,MonomialOrder=>Lex]
R = kk[x,y,z,t,MonomialOrder=>Lex]
I = ideal"16+3x3+16x2z+14x2y3,6+y3z+17x2z2+7xy2z2+13x3z2"
gbTrace=3
time gens gb I;
time gens gb(homogenize(I,t));

restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
--ord1 = matrix"1,1,1;0,0,-1;0,-1,0"
--ord2 = id_(ZZ^3)
kk = QQ
R = kk[x,y,z]
I = ideal"16+3x3+16x2z+14x2y3,6+y3z+17x2z2+7xy2z2+13x3z2"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
groebnerWalktoLex(I,Rlex)
gens oo

L = flatten entries gens gb I
inL = L/leadMonomial
computeNextW(null,inL,L)

groebnerCone(L/leadMonomial, L)
S = bd(L/leadMonomial, L)
S = entries transpose S

-- We want the elements of S which are > 0 wrt ord1, and < 0 wrt ord2.
-- Actually, all the elements we have are > 0 wrt ord1, by construction...
Sp = partition(s -> position(s, i -> i != 0), S)
Sp1 = hashTable apply(pairs Sp, (i,L) -> (i,select(L, s -> s#i < 0)))
Sp2 = hashTable apply(pairs Sp1, (i,L) -> (i,apply(L, s -> s/(s#0))))
xloc = maxPosition(Sp2#0/sum)
Sp2#0#xloc

L1 = L/(g -> interm(o44, g, leadTerm g))
Rlex = (coefficientRing R)[gens R, MonomialOrder=>Lex]
L1 = L1/(g -> substitute(g,Rlex))
G1 = flatten entries substitute(gens gb ideal L1, R)
gbL = gb ideal L
H = G1 - apply(G1, g -> g % gbL)
netList oo
max sum(o26_0)
apply(oo#0, L -> apply(L,plus))

groebnerCone(H, G1/leadMonomial)
fourierMotzkin fourierMotzkin oo

Rw = kk[gens R, MonomialOrder=>{Weights=>{6,3,2},Lex=>3}]
substitute(I,Rw)
I1 = ideal gens gb oo
I2 = substitute(ideal H,Rw)
leadTerm I1
leadTerm gens I2

-- findWeightvector: towards target order
--   

-- Steps: input: rings R, Rlex, many of form Rw
-- Input: ideal I in an Rw or R which is a reduced GB
-- Output: ideal J, 

-- This matrix causes problems with fourierMotzkin...
matrix {{0, -1, 0, 0, -1, 0, -1, -2, 0, -1, 0, -2, 0, -2, -1, -1, 0, -1, 0, -2, -1, -2, -1, 0, -1, 0, 0, -1, -1, 0, 0, -1, -2, 0, -1, 0, -1, -1, 0, 0, 0, 0, 0, -1, 0,
       0, -1, 0, 0, 0, 0, -1, -2, -1, 0, 0, 0, 0, -2, -1, 0, -1, -2, 0, 0, -1, -2, -1, 0, -2, -1, 0, -2, -1, -1, 0, -1, 0, -2, -1, -1, 0, -2, -1, 0, -1, 0, -1, 0, -2, -1, 0,
       -2, -1, 0, -2, -1, 0, -1, 0, 0, -1, -1, 0, -2, 0, -1, 0, 0, -2, -3, -1, 0, -2, -1, -1, 0, 0, -3, 0, -2, 0, -1, 0, -2, 0, -1, 0, -1, 0, 0, 1, 0, 1, 0, -1, 1, 1, 0, 0,
       -1, 0, 1, 0, 1, 2, 2, 2, 0, 2, 2, 2, 1, 1, 0, 2, 1, 0, 2, 1, 1, 2, 0, 1, 1, 2, 0, 2, 1, 2, 0, 1, 1, 2, 0, 1, 1, 2, 2, 0, 1, 2, 1, 2, 0, 2, 1, 2, 2}, {5, 7, 6, 5, 7, 6,
       8, 10, 8, 7, 6, 9, 7, 10, 9, 10, 5, 7, 6, 9, 8, 10, 9, 8, 10, 9, 6, 9, 10, 9, 10, 8, 10, 8, 10, 9, 11, 9, 9, 10, 11, 8, 9, 11, 11, 11, 11, 11, 9, 10, 9, 11, 13, 12,
       11, 6, 9, 10, 13, 12, 11, 13, 15, 13, 9, 11, 13, 12, 11, 14, 13, 12, 15, 14, 15, 9, 11, 10, 13, 12, 13, 12, 15, 14, 13, 15, 14, 10, 10, 13, 12, 11, 14, 13, 12, 15, 14,
       13, 15, 14, 15, 12, 13, 12, 15, 13, 15, 14, 15, 13, 15, 13, 12, 15, 14, 15, 14, 15, 15, 12, 15, 13, 15, 15, 15, 13, 15, 15, 15, 15, 15, -6, -3, -3, -2, 1, -1, -3, 0,
       1, 1, 1, 0, 0, 0, -12, -6, -6, -2, -4, -6, -5, -3, -2, 0, -2, -5, -2, -4, -1, 0, -6, -2, -3, -2, -3, 0, -2, 0, -1, -2, -3, -2, -3, 0, -1, 0, -1, -3, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0}, {-2, -2, -2, 0, -1, -1, -2, -3, -3, 0, 0, -1, -1, -2, -2, -3, 2, 1, 1, 0, 0, -1, -1, -1, -2, -2, 2, 0, -1, -1, -2, 2, 1, 1, 0, 0, -1, 2, 1, 0, -1, 3, 2,
       1, 0, 1, 3, 2, -3, -4, -2, -3, -4, -4, -4, 2, -1, -2, -3, -3, -3, -4, -5, -5, 0, -1, -2, -2, -2, -3, -3, -3, -4, -4, -5, 1, 0, 0, -1, -1, -2, -2, -3, -3, -3, -4, -4,
       2, 1, 0, 0, 0, -1, -1, -1, -2, -2, -2, -3, -3, -4, 1, 0, 0, -1, -1, -2, -2, -3, 2, 1, 1, 1, 0, 0, -1, -1, -2, 2, 2, 1, 1, 0, -1, 2, 2, 1, 0, 2, 1, 2, -1, -1, -2, -1,
       -3, -3, 0, -2, -3, -1, -2, -2, 0, -1, -2, -4, -3, -5, -5, -2, -3, -4, -5, -6, -6, -1, -3, -3, -5, -6, 0, -2, -2, -3, -3, -4, -4, -5, -5, -1, -1, -2, -2, -3, -3, -4,
       -4, -1, -2, -3, -4, -2, -3, 0, -2, 0, -1, 0}}

W = matrix {{2, 0, 2, 2, 1, 0, 2, 2, 2, 0, 3, 3, 0, 0, -1, 1, -1, -1, -1, 0, 0, -1}, 
        {-10, -5, -9, -9, -8, -5, -10, -15, -13, -5, -15, -15, -3, 3, 3, -1, 0, 6, 3, 2, 0, 0}, 
	{3, 2, 1, 0, -2, -2, -1, 5, 4, 2, -1, -2, -1, 1, 0, 1, 1, 1, 0, 1, 1, 0}}
loadPackage "FourierMotzkin"
fourierMotzkin W
