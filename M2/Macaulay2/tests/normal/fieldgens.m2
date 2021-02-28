-- tell Thomas Kahle <tom111@gmx.de> if we break this in the future (for a good reason)
fieldgens = (K,F) -> if K === F then {} else for x in gens last K.baseRings list promote(x,K)
F = QQ
K = toField ( QQ[x]/(x^2+1) )
assert( class x === K )
assert( fieldgens (K,F) === { x } )
assert( fieldgens (F,F) === {   } )
