-- Test of marked GB's

R = ZZ/101[a..d]
I = ideal(a^2-b*c-1, b^3-a, c*d-a^3, a*b-c*d)
gens gb I
d^3 % I

Rlex = ZZ/101[a..d, MonomialOrder=>Lex]
J = substitute(I,Rlex)
gens gb J
d^3 % J

I1 = substitute(gens gb I, Rlex)
inI1 = substitute(leadTerm gens gb I, Rlex)
G = markedGB(inI1,I1)
gens G
leadTerm G
assert(numgens source gens G == 7)
assert((d^3 % G) * 1_Rlex == -20*b + 40*c + 21)
matrix{{(d^3 % G) }} * 1_Rlex
