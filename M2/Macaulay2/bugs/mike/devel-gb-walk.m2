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

leadTerm(List,GroebnerBasis) := (w,G) -> (
     time L := flatten entries generators G;
     inL := flatten entries leadTerm G;
     time matrix {apply(#L, i -> interm(w,inL#i,L#i))}
     )

leadTerm(List,List,List) := (w,inL,L) -> (
     matrix {apply(#L, i -> interm(w,inL#i,L#i))}
     )
-------------------------------------------------------

bd = (inL,L) -> (
  M := flatten apply(#inL, i -> (
    f := inL#i;
    g := L#i;
    m = first exponents(f);
    apply(exponents(g-f), e -> m-e)));
  transpose matrix unique select(M, v -> any(v, i -> i < 0)))


-- These are normalized differences (u is normalized)
dmon = method()
dmon List := (u) -> (
     a := position(u, ui -> ui != 0);
     u = u/(u#a);
     {join({a, sum u},-reverse u), u}
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
		    else select(L, d -> wmon > d)))
	  );
     q = select(keys Sp2, k -> #Sp2#k > 0);
     if #q == 0 
     then null
     else (
	  p = min q;
	  Sp2#p#-1#1
	  )
     )

-- The following is NOT YET FUNCTIONAL!
-- version using engine version of markedGB's
groebnerWalktoLex = method()
groebnerWalktoLex(Ideal,Ring) := (I,Rlast) -> (
     R := ring I;
     ww = null;
     inL := substitute(leadTerm gens gb I, Rlast);
     L := substitute(gens gb I, Rlast);
     -- From here on, all computations are in Rlast
     G = markedGB(inL,L);
     while (time ww = computeNextW(ww,flatten entries leadTerm G, time flatten entries gens G)) =!= null do (
	  << "-- start loop ww = " << ww << endl;
	  time inwG = leadTerm(ww, G);
	  H = gens gb inwG; -- in target ring
	  inH = leadTerm H;
	  H' = H - (time (H % G)) * 1_Rlast;
	  time G = markedGB(inH,H'); -- this autoreduces the marked GB
	  );
     time forceGB gens G -- This one is an actual GB in Rlast order...
     )

-- The following is NOT YET FUNCTIONAL!
-- version using engine version of markedGB's
groebnerWalktoLex = method()
groebnerWalktoLex(Ideal,Ring) := (I,Rlast) -> (
     R := ring I;
     ww = null;
     inL := substitute(leadTerm gens gb I, Rlast);
     L := substitute(gens gb I, Rlast);
     -- From here on, all computations are in Rlast
     G = markedGB(inL,L);
     L = flatten entries gens G;
     inL = flatten entries leadTerm G;
     while (time ww = computeNextW(ww,inL, L)) =!= null do (
	  << "-- start loop ww = " << ww << endl;
	  time inwG = leadTerm(ww, inL,L);
	  H = gens gb inwG; -- in target ring
	  inH = leadTerm H;
	  H' = H - (time (H % G)) * 1_Rlast;
	  time G = markedGB(inH,H'); -- this autoreduces the marked GB
	  time L = flatten entries gens G;
	  time inL = flatten entries leadTerm G;
	  );
     time forceGB gens G -- This one is an actual GB in Rlast order...
     )
     
end
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
-- test of marked GB reduction
R = ZZ/101[a..e, MonomialOrder=>Lex]
I = ideal(a*b-c, b*c-d, c*d-e^2)
inL = matrix{{c*d,c^2,b*c,a*b,b*e^2,a*d^2}}
L = matrix{{c*d-e^2, c^2-a*d, b*c-d, a*b-c, b*e^2-d^2, a*d^2-c*e^2}}
G = markedGB(inL,L)
(a*b^3) % G
leadTerm G
gens G

findEdgeMonomialOrder {1,0,-3/2,5}
kk = ZZ/101
kk = QQ
R = kk[x,y,z]
R = kk[x,y,z,MonomialOrder=>Lex]
R = kk[x,y,z,t,MonomialOrder=>Lex]
I = ideal"16+3x3+16x2z+14x2y3,6+y3z+17x2z2+7xy2z2+13x3z2"
gbTrace=3
time gens gb I;
time gens gb(homogenize(I,t));

-- Example from paper
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
kk = QQ
R = kk[x,y]
I = ideal"x2-y3,x3-y2-x"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
groebnerWalktoLex(I,Rlex)
gens oo

-- Example 6.1 from paper (ISSAC 1997 challenge)
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
kk = QQ
R = kk[x,y,z,w]
I = ideal"
        -2w2+9wx+8x2+9wy+9xy+6y2-7wz-3xz-7yz-6z2-4w+8x+4y+8z+2,
        3w2-5wx+4x2-3wy+2xy+9y2-6wz-2xz+6yz+7z2+9w+7x+5y+7z+5,
        7w2+5wx+2x2+3wy+9xy-4y2-5wz-7xz-5yz-4z2-5w+4x+6y-9z+2,
        8w2+5wx+5x2-4wy+2xy+7y2+2wz-7xz-8yz+7z2+3w-7x-7y-8z+8
     "
Rlex = kk[x,y,z,w,MonomialOrder=>Lex]
time groebnerWalktoLex(I,Rlex)
gens oo

-- Example 6.3 from paper
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
kk = QQ
R = kk[x,y,z]
I = ideal"15+10x2y2+13yz+14xy2z+8x2yz2+11xy3z2,
          5+4xy+8y2,
	  16x3+19y+4x2y"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
time groebnerWalktoLex(I,Rlex)
gens oo
Ilex = substitute(I,Rlex)
gbTrace=3
time gens gb Ilex

restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
--ord1 = matrix"1,1,1;0,0,-1;0,-1,0"
--ord2 = id_(ZZ^3)
kk = QQ
kk = ZZ/32003
R = kk[x,y,z]
I = ideal"16+3x3+16x2z+14x2y3,6+y3z+17x2z2+7xy2z2+13x3z2"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
time groebnerWalktoLex(I,Rlex)
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
