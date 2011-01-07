needsPackage "gfanInterface"
R=QQ[x,y]
assert( (I=ideal(x^7-1, y-x^5)) === ideal(x^7-1,-x^5+y) )
assert( (gfan(I)) === ({{y^7,x},{y^3,x^2*y,x^3},{y^2,x^2*y,x^5},{y,x^7}},{{y^7-1,-y^3+x},{y^3-x,x^2*y-1,x^3-y^2},{-x^3+y^2,x^2*y-1,x^5-y},{-x^5+y,x^7-1}}) )
end

print generateAssertions ///
needsPackage "gfanInterface"
R=QQ[x,y]
I=ideal(x^7-1, y-x^5)
gfan(I)
///
