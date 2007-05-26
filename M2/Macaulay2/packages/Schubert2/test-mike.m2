-- Mike's demo
loadPackage "Schubert2"
R = QQ[c1,c2,c3,c4,Degrees=>{1,2,3,4},MonomialOrder=>GRevLex=>{1,2,3,4}]
X = abstractVariety(4,R)
F = abstractSheaf(X, Rank => 4, ChernClass => 1+c1+c2+c3+c4)
segre F
parts oo
chern F
parts oo
ch F
parts oo
netList toList oo
a = logg chern F
expp a
todd a
parts oo
netList toList oo
det F
schur({2},F)
restart
loadPackage "Schubert2"
R = QQ[c3,c2,c1,Degrees=>{3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(4,ideal vars R)
X = abstractVariety(3, A)
F = abstractSheaf(X, Rank => 3, ChernClass => 1+c1+c2+c3)
F2 = wedge(2,F)
F3 = wedge(3,F)
symm(1,F)
symm(2,F)
symm(3,F)
G = F**F**F
H = wedge(20,G)
time symm(20,G)
chern H
ch H
chern F
segre F
segre(3,F)
parts segre F
netList toList parts segre F

TEST /// -- segre
  X = abstractVariety(3, use (QQ[c1,c2,c3,Degrees=>{1,2,3},MonomialOrder=>GRevLex=>{1,2,3}]))
  F = abstractSheaf(X, Rank => 3,ChernClass => 1+c1+c2+c3)
  assert(chern F == 1+c1+c2+c3)
  assert(toString segre F == "c1^3-2*c1*c2+c3+c1^2-c2+c1+1")
  assert(segre(3,F) == c1^3-2*c1*c2+c3)
  netList segre(0,3,F)
///

restart
loadPackage "Schubert2"
R = QQ[c4,c3,c2,c1,Degrees=>{4,3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(10,ideal vars R)
X = abstractVariety(9, A)
F = abstractSheaf(X, Rank => 4, ChernClass => 1+c1+c2+c3+c4)
F2 = wedge(2,F)
F3 = wedge(3,F)
symm(1,F)
symm(2,F)
symm(3,F)
G = F**F**F
H = wedge(20,G)
time symm(20,G)
chern H
ch H
chern F
segre F
segre(3,F)
parts segre F
netList toList parts segre F
       
-- test wedge, symm for small cases
restart
R = QQ[a,b,c,d,e1,e2,e3,e4,t,MonomialOrder=>Eliminate 4]
I = ideal(e1-(a+b+c+d), e2-(a*b+a*c+a*d+b*c+b*d+c*d), e3-(a*b*c+b*c*d+a*c*d+a*b*d),e4-a*b*c*d)
F = product(1..4, i -> (1+R_(i-1)*t))
F % I
F = product apply(subsets(0..3,2), x -> (1 + (R_(x#0) + R_(x#1)) * t))
F % I
coefficients(oo,Variables=>t)

F = product apply(subsets(0..3,3), x -> (1 + sum(#x, i -> R_(x#i)) * t))
F % I

F = product apply(flatten for i from 0 to 3 list for j from i to 3 list {i,j}, x -> (
	  (1 + sum(#x, i -> R_(x#i))*t)))
F % I

-- Testing schur functions of bundles
restart
loadPackage "Schubert2"
R = QQ[c4,c3,c2,c1,Degrees=>{4,3,2,1},MonomialOrder=>RevLex,Global=>false]
A = R/truncate(5,ideal vars R)
X = abstractVariety(4, A)
F = abstractSheaf(X, Rank => 4, ChernClass => 1+c1+c2+c3+c4)
ch F

-- Check that wedge 2, symm 2, work correctly
S2F = ch symm(2,F)
W2F = ch wedge(2,F)
R = symmRing 4
jacobiTrudi({2},R)
ch wedge(2,F) == (ch F)^2 - ch symm(2,F)
ch wedge(2,F) == (ch F)^2 - ch symm(2,F)
ch schur({2},F) == ch symm(2,F)
ch schur({1,1},F) == ch wedge(2,F)

-- Now let's try 3 parts
debug SchurRings
R = symmRing 4
S = R.Schur
s
s_{1}^3 -- s_3+2*s_(2,1)+s_(1,1,1)
S3 = ch symm(3,F)
W3 = ch wedge(3,F)

ch schur({2,1},F)
L21 = ((ch F)*(ch wedge(2,F)) - W3)
oo == ooo
0 == (ch F)^3 - S3 - W3 - 2*L21
ch wedge(3,F) == ch schur({1,1,1},F)
ch symm(3,F) == ch schur({3},F)
