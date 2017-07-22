--Associated Graded ring very basic test
restart
R=ZZ/23[x]
I=ideal(x)
A=associatedGradedRing I
S=ring ideal A
assert(dim S==2)
assert(codim A==1)
N=normalCone I
s=ring ideal N
assert(dim s==2)
assert(codim N==1)
