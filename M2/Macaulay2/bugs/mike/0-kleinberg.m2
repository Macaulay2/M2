-- From an email 3/13/10
--	From: 	Ankur Moitra <moitra@MIT.EDU>
--	Subject: 	Re: using Macaulay 2 on Intel-based Macintosh running OS 10.4.11?
--	Date: 	March 13, 2010 10:04:05 PM EST
--	To: 	Michael Stillman <mike@math.cornell.edu>
--	Cc: 	Ankur Moitra <moitra@mit.edu>, Robert Kleinberg <rdk@cs.cornell.edu>, Daniel R. Grayson <dan@math.uiuc.edu>

(1) --------------------------------------

kk = ZZ/109

S = kk[a,b,c,d,w,x,y,z]

(2) --------------------------------------

S = CC[a,b,c,d,w,x,y,z]

------------------------------------------

A = matrix{{a}, {a^2 + w^2}, {a^3 + 3*a*w^2}, {a^4 + 6*a^2*w^2 + 3*w^4}, {a^5 + 10*a^3*w^2 + 15*a*w^4}, {a^6 + 15*a^4*w^2 + 45*a^2*w^4 + 15*w^6}}

B = matrix{{b}, {b^2 + x^2}, {b^3 + 3*b*x^2}, {b^4 + 6*b^2*x^2 + 3*x^4}, {b^5 + 10*b^3*x^2 + 15*b*x^4}, {b^6 + 15*b^4*x^2 + 45*b^2*x^4 + 15*x^6}}

C = matrix{{c}, {c^2 + y^2}, {c^3 + 3*c*y^2}, {c^4 + 6*c^2*y^2 + 3*y^4}, {c^5 + 10*c^3*y^2 + 15*c*y^4}, {c^6 + 15*c^4*y^2 + 45*c^2*y^4 + 15*y^6}}

D = matrix{{d}, {d^2 + z^2}, {d^3 + 3*d*z^2}, {d^4 + 6*d^2*z^2 + 3*z^4}, {d^5 + 10*d^3*z^2 + 15*d*z^4}, {d^6 + 15*d^4*z^2 + 45*d^2*z^4 + 15*z^6}}

M = A|B|C|D

J = intersect(ideal(a-b,w-x), ideal(a-c,w-y), ideal(a-d,w-z), ideal(b-c,x-y), ideal(b-d,x-z), ideal(c-d,y-z))
J = intersect(ideal(a-b,w^2-x^2), ideal(a-c,w^2-y^2), ideal(a-d,w^2-z^2), ideal(b-c,x^2-y^2), ideal(b-d,x^2-z^2), ideal(c-d,y^2-z^2),
     ideal(a,w^2), ideal(b,x^2), ideal(c,y^2), ideal(d,z^2))

I = minors(4, M);

P = radical I

Q = radical J

P == Q

% We know that V(I) contains V(J), we want to know if V(I) == V(J). Maybe we'll get lucky, and radical J might be a subset of I

% So I also tried Q intersect I == Q instead of P == Q in the last line

end
------ MES attempt to understand ideal below:
-- have I, J from above, char 109
-- 
time L = gens gb(I, Algorithm=>LinearAlgebra);
time G = forceGB L;
J_0^5 % G
L = ideal L;

radical monomialIdeal leadTerm G
radical monomialIdeal leadTerm J

-- facts to notice.  
-- (1a) J is already a radical ideal
-- (1b) radical of J in QQ[vars] is the same as the radical of J in CC[vars] (same generating set).
--
-- For the rest, doing it over the finite field above
-- (2) radical monomialIdeal leadTerm J != radical monomialIdeal leadTerm G
--  and so I and J do not have the same radical
-- lemma: rad(I) == rad(J) => rad(in(I)) == rad(in(J))

-- what other components are there?
independentSets monomialIdeal leadTerm G
-- one is {a, b, c, w, x, y}

S1 = kk[d,z,a,b,c,w,x,y,MonomialOrder=>Eliminate 2]
I1 = sub(I,S1);
betti I1
gbTrace=3
G = gb(I1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial, StopBeforeComputation=>true);
G = gb(I1, StopBeforeComputation=>true);
gb(I1, DegreeLimit=>16);
H = gens oo;
H = flatten entries H;
H/size
factor H_10

time gens gb(I1, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);

I = minors(3, M_{0,1,2});
codim I
degree I
J = intersect(ideal(a-b,w^2-x^2), ideal(a-c,w^2-y^2), ideal(b-c,x^2-y^2), ideal(a,w^2), ideal(b,x^2), ideal(c,y^2))
I1 = I : J;
betti I1
codim I1
degree I1
I2 = I : I1;
I2 == J
I == intersect(I1, J)
I1a = I1 : J;
I1b = I1a : J;
I1c = I1b : J;
betti I1c
I1c == I1b -- true
I1b == I1a -- false
see I1c
I1d = saturate(I1c, a);
I1d == I1c -- true

Isat = saturate(I, J_0);
codim Isat
degree Isat
betti Isat

Isat = trim Isat;
S1 = kk[a,b,c,w,x,y,d,z,MonomialOrder=>Eliminate 3]
I1 = sub(Isat,S1);

time gens gb I1;
