debug Core
R = ZZ/101[a..d]
I = ideal(a^2-b*c, a*b-c*d)
gens gb I
G = map(R, rawMGB raw gens I)
set flatten entries leadTerm G  === set flatten entries leadTerm gb I

restart
debug Core
loadPackage "ExampleIdeals"
I = cyclicRoots(5, ZZ/101)
gens gb I
G = map(R, rawMGB raw gens I)
set flatten entries leadTerm G  === set flatten entries leadTerm gb I

