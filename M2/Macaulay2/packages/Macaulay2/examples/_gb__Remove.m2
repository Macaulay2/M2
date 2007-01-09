R = ZZ[a]/(a^2-3)[x,y]
F = y^2-x*(x-1)*(x-a)
J = ideal(diff(x,F),diff(y,F),F)
gens gb J
peek J.generators.cache
gbRemove J
peek J.generators.cache
