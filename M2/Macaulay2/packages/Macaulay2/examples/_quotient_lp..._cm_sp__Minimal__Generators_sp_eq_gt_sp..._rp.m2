S=ZZ/101[a,b]
i=ideal(a^4,b^4)
quotient(i, a^3+b^3)
quotient(i, a^3+b^3, MinimalGenerators=>false)
