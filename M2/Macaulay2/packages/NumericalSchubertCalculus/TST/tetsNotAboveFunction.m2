needsPackage "NumericalSchubertCalculus"
load("../notAbove.m2")

l = {2,1} --in G(4,8)
NotAboveLambda(l,4,8)

l = {2,2} --in G(4,8)
NotAboveLambda(l,4,8)
l = {4,2,2,1}
NotAboveLambda(l,5,11)
l = {7,1,1,1}
NotAboveLambda(l,4,11)

end
restart
