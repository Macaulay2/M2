-- from
load "~/src/M2/Macaulay2/packages/Macaulay2Doc/test/isSubset.m2"
-- fixed in June.

-- from email (google group):
-- [christopherleedouglas@gmail.com: [Macaulay2] Comparing Ideals in Polynomial Rings]
--	Date: 	April 30, 2009 9:17:33 PM EDT


end

R = ZZ[a,b,c,d,e,f,g,h]
I = ideal"
    6ac-d,
    105bc-e,
    g3+21bc-f,
    g2"
gbTrace=15
gb I
gb(I, MaxReductionCount=>1)

transpose gens gb I

R = ZZ[x,y,z,w,v,t]
I = ideal"10x2t+z2,100xy2+w3"
gbTrace=15
gens gb I
F = poly"50x2y2t+v4"
F % I

-- possible problem:

Computing a GB:
current gb:  g1, g2, ..., gn
doing an spair(gi,gj)
  get to the exact monomial gk, but with a lower initial coeff
    (Q: which elements gk could this happen to?)
    swap to change gk, and continue reducing the new one.
  POSSIBLE PROBLEM: We could be missing the spair(new gk, some gl)
    because spair(old gk,gl) was already computed.
  Can this happen??
  
  First: the only gk that can match mmust be in the same degree.
  However, it could have been done before the last set of spairs were
    created.
    If so: there is an spair (ga,gb), with ga in the current (sugar) degree.
    then, all pairs in this sugar degree were made (including all for ga.
    Could some of these spairs have been computed before the one that gets to this exact monomial?
      It seems possible to me.
      doing (gc,gd), reduces down an h with same monomial as ga.
        (h,ga) --> (h',ga'), and ga is replaced by ga'.
	Could (ga,ge) could have been done already, where (ga',ge) is now (ga,ge) divided by an integer?
  
g1, g2, g3, g4.  

g4 created, and then degree ends.  How can the degree stay the same?
The only spairs that are created the second time around, are those that don't change the sugar degree.
i.e. (ga,gb), where ga has the current sugar degree, and therefore in(gb) divides in(ga) (not counting coeffs).

But why would ga not have been reduced further?  It can't be because of gaps, since otherwise the current spair (ga,gb) 
would also be in higher degree.  So when can this happen?

It seems that it can only happen when a swap has occured in this degree already.

R = ZZ[x,y,z,u,v]
I = ideal"6x,z2,105y3,z10+15y3"
gbTrace=15
gb I


R = ZZ[a,b,c,d,e,f,g,x,y]
M = matrix"
  0,0,0,10,15,20;
  24a,0,21a,12a2,15a2,18a2;
  0,ab,b2,b3,b3,b3" ** R
leadTerm M
gbTrace=15
gens gb M

M = matrix"
  0,a,b,c,f,g;
  0,24x,12x2,9x2y,7xy,0;
  d,d2,d5,d5,e4,e3x"
leadTerm M  
gbTrace=15
gens gb M

-- This one misses an spair, but it seems
-- that the answer is OK?
M = matrix"
        0,0,a,b,c,0;
        0,24x,12x2,9x2y,7xy,0;
        d,d2,d5,d5,e4,e3x"
