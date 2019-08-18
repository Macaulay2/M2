-- This takes a little too long for inclusion in the package: 120 seconds

-- On Aug 17, 2010, at 4:03 AM, Olivier Debarre wrote:

-- Anyway, the computation (below) you did for us was to compute the Euler
-- characteristic of the structure sheaf O_Z of the subvariety Z of Gr(6,4)
-- defined as the zero locus of a section s of the globally generated rank-20
-- vector bundle wedge(3,S^*), provided that Z have the expected dimension 24-20=4
-- (which happens for instance when s is general, in which case Z is also smooth),
-- in which case there is a resolution of O_Z given by the Koszul complex, whose
-- terms are the wedge(i, wedge(3,S)), i = 0..20.

-- Hence your computation shows that chi(O_Z)=3. On the other hand, the canonical
-- class of Z is trivial (by adjunction) and there is a classification of smooth
-- projective varieties with trivial canonical class (Beauville and Bogomolov)
-- which says that Z has a finite etale cover Y (of degree m) which is a product
-- of abelian varieties (with chi=0), Calabi-Yau varieties (with chi=2) and
-- irreducible symplectic varieties (with chi=1+dim/2). Since chi(O_Y)=m
-- chi(O_Z)=3m, it is not difficult to deduce that m=1 and that Z itself must be
-- irreducible symplectic.

loadPackage "Schubert2"
G = flagBundle {6,4}
(S,Q) = G.Bundles
E = exteriorPower_3 S
time apply(0 .. 20, i -> chi exteriorPower_i E)
assert( oo == (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1) )
time integral ( chern_20 E * (chern_1 G.TangentBundle)^4 )
assert( oo == 14520000 )

end

-- Mike starting to look at this
time apply(0 .. 20, i -> time chi exteriorPower_i E)

-- Here is the next case, which we can't do yet:

loadPackage "Schubert2"
G = flagBundle {13,7}
(S,Q) = G.Bundles
(S,Q) = (dual Q, dual S)
E = exteriorPower_4 S
for i from 1 to 35 do (
    << i << " : " << chi exteriorPower_i E << endl << flush
)
