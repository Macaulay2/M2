R = QQ[a,b,c]
f = R_{1,2,3}
S = R/(a^3+b^3+c^3)
g = R_{1,2,3}
h = S_{1,2,3}
assert( f == g )
assert( class f === R )
assert( class g === R )
assert( class h === S )

--Date: Fri, 16 Apr 2004 11:43:07 +0000 (UTC)
--From: Ben Richert <brichert@polymail.cpunix.calpoly.edu>
--To: dan@math.uiuc.edu
--Subject: possible M2 bug?
--Content-Type: TEXT/PLAIN; charset=US-ASCII
--
--Dan,
--
--I came across the following:
--
--Macaulay 2, version 0.9.2
----Copyright 1993-2001, D. R. Grayson and M. E. Stillman
----Singular-Factory 1.3c, copyright 1993-2001, G.-M. Greuel, et al.
----Singular-Libfac 0.3.3, copyright 1996-2001, M. Messollen
--
--i1 : r=QQ[A,B,a,b,n][p]
--
--o1 = r
--
--o1 : PolynomialRing
--
--i2 : n*p^3+(A-B)*(p+1)^3+(b-a)*(p+2)^3+(p+3)^3*((p+2)*(p+1)/2-3*n+B)
--
--Process M2 segmentation fault
--
--It's mad about the (p+2)*(p+1)/2, so if you switch to 1/2*(p+2)*(p+1) 
--things work fine. Just thought I'd let you know.
--
--Ben Richert

R = QQ[A,B,a,b,n][p]
f = n*p^3+(A-B)*(p+1)^3+(b-a)*(p+2)^3+(p+3)^3*((p+2)*(p+1)/2-3*n+B)
f = n*p^3+(A-B)*(p+1)^3+(b-a)*(p+2)^3+(p+3)^3*((p+2)*(p+1)//2-3*n+B)

R = QQ[x]
x/2
x//2
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test rings.out"
-- End:
