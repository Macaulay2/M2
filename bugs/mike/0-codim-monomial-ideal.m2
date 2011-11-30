This shows there is a problem in computing the codimension of a monomial
ideal when the coefficient ring has quotient elements.

A=QQ[a,b]
I=ideal (b)
S=A/I
R=S[x]
M=image matrix{{a*x}}
i = rank M
(R',f) = flattenRing R
j = rank f M
assert( i == j )
