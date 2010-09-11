S = QQ[x,y]
assert( (A = coker matrix{{x^3,y^3}}) === cokernel map(S^1,S^{{-3},{-3}},{{x^3, y^3}}) )
assert( (hilbertFunction(-5,Ext^2(A,S))) === 2 )

end

print generateAssertions ///
S = QQ[x,y]
A = coker matrix{{x^3,y^3}}
hilbertFunction(-5,Ext^2(A,S))
///
