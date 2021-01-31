R = ZZ [x, y, z, C, D]
foo = map(R^{{0}}, R^{{0}}, {{x^4*C^25-x^3*y*C^25}})
bar = (coefficients(foo,Variables => {C,D}))#1;
degrees bar
assert not isHomogeneous bar				    -- this worked in 0.9.97
