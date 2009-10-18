-- From: schreyer@msri.org (Frank-Olaf Schreyer)
-- Subject: bug
-- To: dan@math.uiuc.edu (Dan Grayson),         mike@math.cornell.edu (Michael E. Stillman)
-- Date: Fri, 31 Mar 2000 19:14:52 -0800 (PST)
-- Reply-To: schreyer@msri.org
-- 
-- Dear Dan and Mike,
-- this seams to be a serious bug.
-- Yours Frank.

n=6
kk=ZZ/101;
E=kk[y_0..y_n,SkewCommutative=>true]
A=matrix{{y_1*y_6},{y_2*y_5},{y_3*y_4}}
n=matrix{{y_1*y_2*y_4,y_2*y_3*y_5,y_3*y_4*y_6,y_4*y_5*y_0,y_5*y_6*y_1,y_6*y_0*y_2,y_0*y_1*y_3},
         {y_3*y_5*y_6,y_4*y_6*y_0,y_5*y_0*y_1,y_6*y_1*y_2,y_0*y_2*y_3,y_1*y_3*y_4,y_2*y_4*y_5}}
betti(fn=res(image n,LengthLimit=>4))
betti(fnt=res(image transpose n,LengthLimit=>4))
bb = betti(fc=res(image fnt.dd_2,LengthLimit=>2))
cc = betti(fc=res(ker fnt.dd_1,LengthLimit=>2))
assert ( bb === cc )


n=6
kk=ZZ/101;
E=kk[y_0..y_n,SkewCommutative=>true]
n=matrix{{y_1*y_2*y_4},
         {y_3*y_5*y_6}}
betti(fnt=res(image transpose n,LengthLimit=>4))
M = fnt.dd_2;
gbTrace = 3
gens gb M
gens gb (M, Strategy => Inhomogeneous)
--gens gb (M)
--bb = betti syz (M) -- wrong if return cmp;
bb = betti syz (M) -- wrong if return cmp;
bb = betti syz (M, Strategy=>Inhomogeneous) -- wrong if return cmp;
M1 = map(E^(- degrees target M), E^(- degrees source M), M);
gens gb M1
bb1 = betti syz M1 -- wrong if return cmp;
cc = betti (fnt.dd_3)
assert ( bb === cc)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test resskew.out"
-- End:
