-- urgency: moderate
-----------------------------------------------------------------------------
-- Date: Wed, 14 Jun 2006 14:02:39 -0700 (PDT)
-- From: Irena Swanson <iswanson@reed.edu>
-- To: dan@math.uiuc.edu
-- cc: iswanson@reed.edu
-- Subject: Macaulay2 bus error
-- 
-- Dear Dan,
-- I used to run a version of the attached file at NMSU and it compiled 
-- (Linux on some PC) but I haven't been able to run it successfully here at 
-- Reed.  (It works if expd = {1_R}, but not with expd = {1_R, B}, the latter 
-- needs to allocate many more variables so it takes a lot of time.)
-- Anyway, if I load the attached file into M2, after < 3 hours it returns
-- "Process M2 bus error".  Jim Fix, a computer scientist here, thought that 
-- the programmers of M2 should know as this may be some memory allocation
-- problem for the Mac installation (Dual 2 GHz PowerPC G5, 2 GB DDR SDRAM,
-- OS X 10.4.2).  I tried to figure out M2's version on my computer, and this 
-- is what I got:
-- 
-- Macaulay 2, version
-- --Copyright 1993-2001, D. R. Grayson and M. E. Stillman
-- --Singular-Factory 2.0.5, copyright 1993-2001, G.-M. Greuel, et al.
-- --Singular-Libfac 2.0.4, copyright 1996-2001, M. Messollen
-- 
-- i1 : version
-- 
-- o1 = HashTable{architecture => Power Macintosh            }
--                 compile node name => c009h081.acad.reed.edu
--                 compile time => May 23 2006 11:04:05
--                 compiler => gcc 3.3
--                 dumpdata => false
--                 factory version => 2.0.5
--                 gc version => 6.2 alpha 4
--                 gmp version => 4.2.1
--                 libfac version => 2.0.4
--                 operating system => Darwin
--                 operating system release => 8.2.0
--                 VERSION =>
-- 
-- o1 : HashTable
-- 
-- Does it seem that I don't have a version?  I can do primary decomposition 
-- and radicals, so this can't be too old.  It was installed late last 
-- summer.
-- 
-- Irena
-----------------------------------------------------------------------------
-- Date: Thu, 15 Jun 2006 13:55:08 -0700 (PDT)
-- From: Irena Swanson <iswanson@reed.edu>
-- To: Michael Stillman <mike@math.cornell.edu>
-- cc: Dan Grayson <dan@math.uiuc.edu>
-- Subject: Re: Macaulay2 bus error
-- In-Reply-To: <2C3EC814-F516-4BEA-8F99-DEAB6E1C84AA@math.cornell.edu>
-- 
-- 
-- Thanks! I implemented this version, it's up and running.  It was helpful 
-- in telling me of a syntax error in one line (I had (exponents tempterm)_i 
-- and expected a number; need (flatten exponents tempterm)_i).  Now I am 
-- running my long problem.  I'll let you know if it works.
-- 
-- Irena
-----------------------------------------------------------------------------
basering = QQ -- problem: Groebner bases not completely configured over reals!
R = basering[A,B,C,alpha,beta]
numtruevars = 3;
-- A, B, C variables (three of them, that's why numtruevars = 3)
-- alpha, beta non-zero complex numbers

expd = {1_R};
expd = {1_R, B};
L = {A, A + beta*B, C + alpha*B, C}
Mmat = matrix{{alpha*A + alpha*beta*B + beta*C, -A*C}, {A*C,  A*B*C}};

-- input: natural number D
-- construct a list of all monomials in R of degree at most D
allmonomsatmostD = (D) -> (
  e := 1;
  expd := {1_R};
  while (e <= D) do (
    expd = expd | (entries generators (ideal (R_0..R_(numtruevars-1)))^e)_0;
    e = e+1;
  );
  expd
)

--expd = allmonomsatmostD(1)

-- input: a matrix Mmat and an ordered list L of factors of det(Mmat)
-- try to factor Mmat as U_0 V_1 U_1 V_2 U_2 \cdots V_k U_k
-- det(U_l) = 1
-- and V_l is the diagonal matrix with L_l first spot and 1 elsewhere
factorable = (L,Mmat) -> (
  m := rank target Mmat;
  k := #L;
  n := #generators R;
  indexset := flatten apply(k+1, i ->
	flatten apply(expd, ii ->
	flatten apply(m, iii ->
	flatten apply(m, iiii -> (i,ii,iii+1,iiii+1)))));
  S = basering[a_1..a_n, apply(indexset, i -> x_i)]; -- make it global
  phi := map(S,R,{a_1..a_n});
  I = ideal(0_S);
  P := 1_S;
  MM := submatrix(id_(S^m), {1..m-1});
  ZM := matrix apply(m-1, i-> {0_S});
  i := 0;
  while (i < k) do (
    XX := sum apply(expd,
	j -> phi(j)*(transpose genericMatrix(S,x_(i,j,1,1),m,m)));
    I = I + ideal((det XX) - 1_S);
    P = P * XX * ((matrix{{phi(L_i)}} || ZM) | MM);
    i = i + 1;
  );
  XX = sum apply(expd,
	j -> phi(j)*(transpose genericMatrix(S,x_(k,j,1,1),m,m)));
  I = I + ideal((det XX) - 1_S);
  P = P * XX - phi(Mmat);
  eqns := flatten entries flatten P;
  psi := map(S,S, apply(numtruevars, i->0_S) |
	apply(dim S - numtruevars, i->(gens S)_(i+numtruevars)));
  I = I + sum apply(eqns, i -> viljnucoeffs(numtruevars,i,psi));
  << "-- I has " << numgens I << " generators in a ring with " << numgens ring I << " variables" << endl;
  << "--computing gb I ..." << endl;
  gb I;							    -- this is slow, presumably
  << "--done computing gb I" << endl;
  isSubset(ideal(1_S), I)
--print "Wait just a bit (hours????) -- almost have the final answer: ";
 -- I == ideal(1_S) -- this takes too long!?
)

viljnucoeffs = (m, tempoly,psi) -> (
  n := 0;
  << "--viljnucoeffs ... " << endl;
  J := ideal (0_S);
  while (tempoly != 0_S) do (
   tempterm := leadMonomial tempoly;
   tempexp := product set apply(m, i -> a_(i+1)^((flatten exponents tempterm)_i));
   partp := tempoly - (tempoly % (tempexp));
   partp = psi(partp//(tempexp));
   J = J + ideal(partp);
   tempoly = tempoly - tempexp*partp;
   n = n+1;
   << "--viljnucoeffs pass " << n << " complete ... " << endl;
  );
  << "--viljnucoeffs done" << endl;
  J
)

gbTrace = 3

end
load "7-swanson.m2"
factorable(L, Mmat)
