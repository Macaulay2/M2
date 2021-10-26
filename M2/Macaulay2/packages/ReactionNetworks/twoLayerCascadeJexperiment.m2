restart
needsPackage "ReactionNetworks"
needsPackage "MonodromySolver"

J = twoLayerCascadeJ()

createRing(J, QQ)
FJ = join(subRandomInitVals J, subRandomReactionRates J)
I = ideal FJ
degree I
dim sub(I, QQ[J.ConcentrationRates])

J' = sub(twoLayerCascadeJ(), {"S_0" => "S_1", "S_1" => "S_2"})
J2 = glue(J,J')
R = createRing(J2, QQ)

F = join(subRandomInitVals J2, subRandomReactionRates J2)
I = ideal F
netList flatten entries mingens I
degree I
dim sub(I, QQ[J2.ConcentrationRates]) --positive dimension???

J2.ConcentrationRates



