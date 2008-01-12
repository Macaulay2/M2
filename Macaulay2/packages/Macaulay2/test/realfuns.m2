eps = 2.^-50
chk = (x,y) -> abs(x-y) < eps
assert chk ( integrate(sin,0,pi/2), 1 )
