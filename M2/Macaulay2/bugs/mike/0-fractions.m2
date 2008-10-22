gbTrace=10
R = frac (ZZ/101[a,b]) [x]   
gens gb ideal (a*x^3-13, b*x^2-12)  -- ok now

R = frac (ZZ[a,b]) [x]
gens gb ideal (a*x^3-13, b*x^2-12)  -- ok

R = frac (QQ[a,b]) [x]
gens gb ideal (a*x^3-13, b*x^2-12)  -- ok now

R = frac (ZZ/101[a,b,c]/(c)) [x]
gens gb ideal (a*x^3-13, b*x^2-12) -- ok

end
gives this:


oops

o9 = | -13/-13 |

             1       1
o9 : Matrix R  <--- R


gens gb ideal (a*x^3-13, b*x^2-12)  -- WORSE

debug Core
A = QQ[a,b]
B = frac A
R = A [x]


use A
new B from rawFraction(raw B, raw(12*a), raw(12*b-1/2*a))

f = 12*a
g = (12*b-1/2*a)
ring f
rawAssociateDivisor f
rawContent f

f/g
cf = rawContent raw f
cg = rawContent raw g
rawRemoveContent raw g
rawRemoveContent raw f
cf/cg
ccf = new QQ from cf
ccg = new QQ from cg
ccf/ccg

gcd((2*3*5)/(7^2*11*13), (2*3*13)/7^3*11)
factor oo
