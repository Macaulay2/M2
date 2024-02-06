-- Test of multi-homogeneity.
-- This should include: hilbert functions, gb's, syzygies, resolutions
--   basis, homogeneity (?), ...

R1 = ZZ/101[a..d,Degrees=>{{1,-1},{1,0},{2,-2},{3,4}}]
R2 = ZZ/101[a..d,MonomialOrder=>Eliminate 2,Degrees=>{{1,-1},{1,0},{2,-2},{3,4}}]
--R3 = ZZ/101[a..f,Degrees=>Multi]

A = ZZ/101[x,y,z,Degrees=>{1,1,2}]
X = Proj A
singularLocus X
--status: We need to design an algorithm for computing the singular locus of a weighted projective space
--status:    The current way works only when the variables all have degree 1.
assert( ideal singularLocus X != 1 )

-- Date: Tue, 13 Mar 2007 17:40:08 +0000
-- From: Yang-Hui He <yang-hui.he@merton.ox.ac.uk>
-- To: "Daniel R. Grayson" <dan@math.uiuc.edu>
-- Subject: Proj
-- 
-- Hi Dan,
-- 
-- 
-- Could I ask another Macaulay2 question regarding weighted projective spaces?
-- 
-- I know, for example, that the sextic in WP_{1,1,2,2} is a K3 surface.
-- 
-- So I can define:
-- 
R = ZZ/101[x_0..x_3, Degrees => {1,1,2,2}];
-- k3 = Proj(R / random(6,R));
S = R/(x_0^6 + x_1^6 + x_2^3 + x_3^3)
k3 = Proj S
-- 
-- Then, I can define the cotangent bundle as:
-- 
cot = cotangentSheaf k3;
-- 
-- strangely, rank cot  does not give me 2, nor do the HH^i of cot  give 
-- the expected values.

p = presentation module cot

assert( isHomogeneous module cot )

rank cot
assert( rank cot == 2 )

apply(5, n -> rank HH^n cot)

F = presentation S

(vars S) * (jacobian F ** S)

--- cot2 = exteriorPower_2 cot;
--- apply(5, n -> rank HH^n cot2)

-- 
-- I tried some other weighted projective varieties, they all seem to give 
-- non-sensical results.  What am I doing wrong?
-- 
-- Thanks a lot again in advance!
-- 
-- Yang.
-- 



end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test testmulti.out"
-- End:
