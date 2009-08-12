-- This one appears to be fixed: 10/26/08
-- however, it would be nice if 'codim' worked over ZZ..

A=ZZ[x1,x2,x3,x4,x5,x6]
I=ideal(x1*x3*x4,x1*x3*x5,x6*x4*x5,x6*x3*x4,x2*x3*x5,x2*x4*x5,x6*x1*x5,x6*x2*x3,x1*x2*x4,x6*x1*x2)
E = Ext^3(A^1/I,A)
J = ann E
J == I
codim I
end

-- Email from Uli Walther: 11 Feb 2008:
Hi there,

as far as I can see there is a serious bug in M2 version 1.0 in the
integer arithmetic part.

To wit:

i1 : A=ZZ[x1,x2,x3,x4,x5,x6]
i2 : I=ideal(x1*x3*x4,x1*x3*x5,x6*x4*x5,x6*x3*x4,x2*x3*x5,x2*x4*x5,x6*x1*x5,x6*x2*x3,x1*x2*x4,x6*x1*x2)
i3 : ann Ext^3(A^1/I,A)

returns
o3 = ideal (2x4*x5*x6, 2x1*x5*x6, 2x3*x4*x6, 2x2*x3*x6, 2x1*x2*x6,
2x2*x4*x5, 2x2*x3*x5, 2x1*x3*x5, 2x1*x3*x4, 2x1*x2*x4)

this is not correct since the annihilator of this Ext should
at least include I. (Listed is 2*I.)

Also, for some reasons I can't get an answer to the question what M2
thinks the dimension of this Ext is equal to. In 0.95 and 0.98 this
dimension was returned as "7", also wrong (and even wrong if you believe
in the annihilator above). 0.92 had all answers correct.

Looking more carefully, the resolution of I is not right.
The problem is that the last map in the
resolution should be (x_1,...,x_n,2). Apparently M2 thinks 2 is a unit in
ZZ. (This also causes it to think that Ext^4=0, which it's not unless you
tensor with a field of a characteristic different from 2 as Ext^4 = ZZ/2.)

I had done these computations in 0.92 and then Anurag tried to replicate
them and found what is described here.

I is the ideal of the real projective plane triangulated minimally.

I haven't tried 1.09.

Cheers, and have fun with it!

uli.

***********************************
Uli Walther
Department of Mathematics
Purdue University
150 North University Street
West Lafayette, IN  47907-2067

Tel: 765-494-1959
walther@math.purdue.edu

