     -- this computation proves a theorem for Claire Voisin and Olivier Debarre
     -- it takes a little too long for inclusion in the package: 120 seconds
loadPackage "Schubert2"
G = flagBundle {6,4}
(S,Q) = G.Bundles
E = exteriorPower_3 S
apply(0 .. 20, i -> chi exteriorPower_i E)
assert( oo == (1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1) )
integral ( chern_20 E * (chern_1 G.TangentBundle)^4 )
assert( oo == 14520000 )
