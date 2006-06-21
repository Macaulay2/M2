-- from email 6/19/06 attachment name: dyer
basering = QQ -- problem: Groebner bases not completely configured over reals!
basering = ZZ/32003 -- problem: Groebner bases not completely configured over reals!
R = basering[A,B,C,alpha,beta]
numtruevars = 3;
-- A, B, C variables (three of them, that's why numtruevars = 3)
-- alpha, beta non-zero complex numbers

expds = {1_R};
expd = {1_R, B};
L = {A, A + beta*B, C + alpha*B, C}
Mmat = matrix{{alpha*A + alpha*beta*B + beta*C, -A*C}, {A*C,  A*B*C}};

-- input: a matrix Mmat and an ordered list L of factors of det(Mmat)
-- try to factor Mmat as U_0 V_1 U_1 V_2 U_2 \cdots V_k U_k
-- det(U_l) = 1
-- and V_l is the diagonal matrix with L_l first spot and 1 elsewhere
factorable = (L,Mmat) -> (
  m := rank target Mmat;
  k := #L;
  n := #generators R;
  indexset := flatten apply(2, i ->
	flatten apply(expd, ii ->
	flatten apply(m, iii ->
	flatten apply(m, iiii -> (i,ii,iii+1,iiii+1)))));
  indexset = indexset | flatten apply(k-1, i ->
	flatten apply(expds, ii ->
	flatten apply(m, iii ->
	flatten apply(m, iiii -> (i+2,ii,iii+1,iiii+1)))));
  S = basering[a_1..a_n, apply(indexset, i -> c_i)]; -- make it global
  phi := map(S,R,{a_1..a_n});
  I = ideal(0_S);
  P := 1_S;
  MM := submatrix(id_(S^m), {1..m-1});
  ZM := matrix apply(m-1, i-> {0_S});
  i := 0;
  while (i < 2) do (
    XX := sum apply(expd,
	j -> phi(j)*(transpose genericMatrix(S,c_(i,j,1,1),m,m)));
    I = I + ideal((det XX) - 1_S);
    P = P * XX * ((matrix{{phi(L_i)}} || ZM) | MM);
    i = i + 1;
  );
  while (i < k) do (
    XX := sum apply(expds,
	j -> phi(j)*(transpose genericMatrix(S,c_(i,j,1,1),m,m)));
    I = I + ideal((det XX) - 1_S);
    P = P * XX * ((matrix{{phi(L_i)}} || ZM) | MM);
    i = i + 1;
  );
  XX = sum apply(expds,
	j -> phi(j)*(transpose genericMatrix(S,c_(k,j,1,1),m,m)));
  I = I + ideal((det XX) - 1_S);
  P = P * XX - phi(Mmat);
  eqns := flatten entries flatten P;
  psi := map(S,S, apply(numtruevars, i->0_S) |
	apply(dim S - numtruevars, i->(gens S)_(i+numtruevars)));
  I = I + sum apply(eqns, i -> viljnucoeffs(numtruevars,i,psi));
  I
--print "Wait just a bit (hours????) -- almost have the final answer: ";
 -- I == ideal(1_S) -- this takes too long!?
)

viljnucoeffs = (m, tempoly,psi) -> (
  J := ideal (0_S);
  while (tempoly != 0_S) do (
   tempterm := leadMonomial tempoly;
   tempexp := product set apply(m, i -> a_(i+1)^((flatten exponents tempterm)_i));
   partp := tempoly - (tempoly % (tempexp));
   partp = psi(partp//(tempexp));
   J = J + ideal(partp);
   tempoly = tempoly - tempexp*partp;
  );
  J
)

end
restart
load "7-iswanson-3hours.m2"
I = factorable({A, A + beta*B, alpha*B+C,C}, Mmat);
betti I -- 166 gens
numgens ring I -- 33 variables

G = flatten entries gens I;
G/(g -> part(0,g))
tally(G/support)
I1 = ideal select(flatten entries gens I, f -> size f <= 2);
transpose gens I1
C = decompose I1;
gbTrace=3
gens gb I1;
codim I1
I1 = monomialIdeal select(flatten entries gens I, f -> size f <= 1);
C = primaryDecomposition I1;
J1 = I + ideal(C_0);
J1 = ideal compress gens sub(I, apply(flatten entries gens C_-1, x -> x => 0));
numgens J1
tally apply(flatten entries gens J1, size)
J1a = ideal select(flatten entries gens J1, f -> size f <= 2);
J11 = trim J1;


--this next line should be about 3 hours?
isSubset(ideal(1_S), I)

I0 = substitute(I, a_5=>0);
I0 = ideal compress gens I0;
betti I0
