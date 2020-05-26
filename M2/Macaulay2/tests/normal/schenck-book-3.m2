needsPackage "gfanInterface"
R=QQ[x,y]
assert( (I=ideal(x^7-1, y-x^5)) === ideal map((R)^1,(R)^{{-7},{-5}},{{x^7-1, -x^5+y}}) );
assert( (gfan(I)) === {new MarkedPolynomialList from {{y^7,x},{y^7-1,-y^3+x}},new MarkedPolynomialList from {{x^3,x^2*y,y^3},{x^3-y^2,x^2*y-1,y^3-x}},new MarkedPolynomialList from {{x^5,x^2*y,y^2},{x^5-y,x^2*y-1,-x^3+y^2}},new MarkedPolynomialList from {{x^7,y},{x^7-1,-x^5+y}}} );
end

print generateAssertions ///
needsPackage "gfanInterface"
R=QQ[x,y]
I=ideal(x^7-1, y-x^5)
gfan(I)
///
