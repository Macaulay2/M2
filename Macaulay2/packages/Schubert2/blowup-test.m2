load "./blowup.m2"

-- blow up of P5 along the Veronese
A = QQ[z]/z^6
P = z^5
B = QQ[h]/h^3
P5 = abstractVariety(5, A)
integral A := coefficient_P
P2 = abstractVariety(2, B)
p = h^2
integral B := coefficient_p
P5.TangentBundle = abstractSheaf(P5, Rank=>5, ChernClass=>(1+z)^6)
P2.TangentBundle = abstractSheaf(P2, Rank=>2, ChernClass=>(1+h)^3)
iupper = map(B,A,{2*h})
ilower = map(A^1, A^1/(z^3), {{4*z^3}})
(Ytilde,Xtilde,jlower) = blowup(P2,P5,iupper,ilower)
ctop tangentBundle Ytilde
assert( oo == 12 * P )

-- blow up a point on P^2
A = QQ[z]/z^3
B = QQ[h]/(h)
P2 = abstractVariety(2, A)
pt = abstractVariety(0, B)
P2.TangentBundle = abstractSheaf(P2, Rank=>5, ChernClass=>(1+z)^3)
pt.TangentBundle = abstractSheaf(pt, Rank=>0, ChernClass=>1_B)
iupper = map(B,A)
ilower = map(A^1, A^1/(z), {{z^2}})
(Ytilde,Xtilde,jlower) = blowup(pt,P2,iupper,ilower)
tangentBundle Ytilde
ctop tangentBundle Ytilde
assert ( oo == 0)

end

load "./blowup.m2"
-- blowup of P5 along the Veronese
P5 = projectiveSpace'(5, VariableName => symbol k)
P2 = projectiveSpace'(2, VariableName => symbol h)
use P5
chern tangentBundle P5
A = intersectionRing P5
B = intersectionRing P2
iupper = map(B,A,{-2*h,4*h^2, -8*h^3, 16*h^4, -32*h^5, 2*h})
isWellDefined iupper -- gives cryptic  error message
J5 = ideal first flattenRing A
A[h]/(2*h-k)
ideal 
use ring J5
sub(J5, {k => 2*k})
intersectionRing P5

sub(sub(ideal {-k, k^2, -k^3, k^4, -k^5, k}, {k=>2*k}), 
