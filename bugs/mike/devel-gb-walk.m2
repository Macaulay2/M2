debug Core
parallelLeadTerms = method()
parallelLeadTerms(List,GroebnerBasis) := (w,G) -> (
     map(ring G, rawGBGetParallelLeadTerms(raw G,w))
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
	  result := Sp2#p#-1#1;
	  gcdresult := gcd result;
	  apply(result, x -> lift(x/gcdresult,ZZ))
	  )
     )

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
     redtime = 0.0;
     cptime = 0.0;
     recordW = {};
     while (time ww = computeNextW(ww,inL, L)) =!= null do (
	  << "-- start loop ww = " << ww << endl;
	  recordW = append(recordW,ww);
	  inwG = parallelLeadTerms(ww,G);
	  H = gens gb inwG; -- in target ring
	  inH = leadTerm H;
	  t := timing(H' = H - (H % G));
	  redtime = redtime + t#0;
	  time G = markedGB(inH,H'); -- this autoreduces the marked GB
	  t = timing(L = flatten entries gens G);
	  cptime = cptime + t#0;
	  inL = flatten entries leadTerm G;
	  );
     << "reduction time = " << redtime << endl;
     << "copy time      = " << cptime << endl;
     time forceGB gens G -- This one is an actual GB in Rlast order...
     )

groebnerWalktoLex(Ideal,Ring,List) := (I,Rlast,WW) -> (
     -- WW should be a list of ww vectors, as produced in the algorithm (over char p, e.g.)
     R := ring I;
     ww = null;
     inL := substitute(leadTerm gens gb I, Rlast);
     L := substitute(gens gb I, Rlast);
     -- From here on, all computations are in Rlast
     G = markedGB(inL,L);
     redtime = 0.0;
     scan(WW, ww -> (
	  << "-- start loop ww = " << ww << endl;
	  inwG = parallelLeadTerms(ww,G);
	  H = gens gb inwG; -- in target ring
	  inH = leadTerm H;
	  t := timing(H' = H - (H % G));
	  redtime = redtime + t#0;
	  time G = markedGB(inH,H'); -- this autoreduces the marked GB
	  ));
     << "reduction time = " << redtime << endl;
     time forceGB gens G -- This one is an actual GB in Rlast order...
     )
     
end
restart
load "/Users/mike/src/M2-trunk/bugs/mike/devel-gb-walk.m2"
-- test of marked GB reduction
R1 = ZZ/101[a..e]
I = ideal"a4b+a3c3+a2d5+ae7"
R2 = ZZ/101[a..e,MonomialOrder=>Lex]
groebnerWalktoLex(I,R2)
R = ZZ/101[a..e, MonomialOrder=>Lex]
I = ideal(a*b-c, b*c-d, c*d-e^2)
inL = matrix{{c*d,c^2,b*c,a*b,b*e^2,a*d^2}}
L = matrix{{c*d-e^2, c^2-a*d, b*c-d, a*b-c, b*e^2-d^2, a*d^2-c*e^2}}
G = markedGB(inL,L)
parallelLeadTerms({0,0,1,1,-2},G)
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
load "/Users/mike/src/M2-trunk/bugs/mike/devel-gb-walk.m2"
kk = QQ
R = kk[x,y]
I = ideal"x2-y3,x3-y2-x"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
groebnerWalktoLex(I,Rlex)
gens oo
groebnerWalktoLex(I,Rlex,recordW)
gens oo

-- Modified Example from paper
restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
kk = QQ
--kk = ZZ/101
R = kk[x,y,z]
I = ideal"3x2-y3-z2,2x3-y2-x+z3"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
gens gb substitute(I,Rlex)
groebnerWalktoLex(I,Rlex)
gens oo
groebnerWalktoLex(I,Rlex,recordW)
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
groebnerWalktoLex(I,Rlex,recordW)
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
time groebnerWalktoLex(I,Rlex,recordW)
ooo == gens oo

Ilex = substitute(I,Rlex)
gbTrace=3
time gens gb Ilex

restart
load "/Users/mike/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
--ord1 = matrix"1,1,1;0,0,-1;0,-1,0"
--ord2 = id_(ZZ^3)
kk = ZZ/32003
R = kk[x,y,z]
I = ideal"16+3x3+16x2z+14x2y3,6+y3z+17x2z2+7xy2z2+13x3z2"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
time groebnerWalktoLex(I,Rlex)

kk = QQ
R = kk[x,y,z]
I = ideal"16+3x3+16x2z+14x2y3,6+y3z+17x2z2+7xy2z2+13x3z2"
Rlex = kk[x,y,z,MonomialOrder=>Lex]
time groebnerWalktoLex(I,Rlex,recordW)
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

-- Example -- slow/ker8 --------------------------
-- We see if there is any chance that this works:
restart
R = ZZ/2003[x11,x12,x13,x21,x22,x23,x31,x32,x33,
            y11,y12,y13,y21,y22,y23,y31,y32,y33, MonomialSize=>8];
load "/Users/mike/src/M2/Macaulay2/bugs/mike/devel-gb-walk.m2"
f = map(R,R,{x11*y11+x12*y21, x11*y12+x12*y22, 
      x11*y13+x12*y23, x21*y11+x22*y21, x21*y12+x22*y22, 
      x21*y13+x22*y23, x31*y11+x32*y21,
      x31*y12+x32*y22, x31*y13+x32*y23,
      x13*y11+x12*y21+x13*y21+x11*y31+x12*y31+x13*y31,
      x13*y12+x12*y22+x13*y22+x11*y32+x12*y32+x13*y32,
      x13*y13+x12*y23+x13*y23+x11*y33+x12*y33+x13*y33,
      x23*y11+x22*y21+x23*y21+x21*y31+x22*y31+x23*y31,
      x23*y12+x22*y22+x23*y22+x21*y32+x22*y32+x23*y32,
      x23*y13+x22*y23+x23*y23+x21*y33+x22*y33+x23*y33,
      x33*y11+x32*y21+x33*y21+x31*y31+x32*y31+x33*y31,
      x33*y12+x32*y22+x33*y22+x31*y32+x32*y32+x33*y32,
      x33*y13+x32*y23+x33*y23+x31*y33+x32*y33+x33*y33});

S = ZZ/32003[gens R,X_1..X_18]
M = substitute(f.matrix,S)
I = ideal apply(18, i -> M_(0,i) - X_(i+1)^3)
gens gb I
Slex = (coefficientRing S)[gens S, MonomialOrder=>Lex]
J = groebnerWalktoLex(I,Slex);
--------------------------------------------------