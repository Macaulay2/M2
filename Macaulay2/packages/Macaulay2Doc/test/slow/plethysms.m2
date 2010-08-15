     -- this computation proves a theorem for Claire Voisin and Olivier Debarre
     -- it takes a little too long for inclusion in the package: 120 seconds
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
