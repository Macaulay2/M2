R = QQ[a,b,t,x,y,z];
f = x^7 * y^11;
diff(x,f)
diff(y,f)
diff(x^2,f)
diff(x*y,f)
diff(y^2,f)
diff(x+y,f)
diff(x^2+x*y+y^2,f)
m = matrix {{x^3, x^4},{x^5,x^6}}
diff(x,m)
diff(x^2,m)
diff(matrix {{x,x^2,x^3,x^4}}, m)
diff(matrix {{x,x^2},{x^3,x^4}}, m)
diff(matrix {{x},{y}}, matrix {{x^2, x*y, y^2}})
v = matrix {{x,y}}
diff(v ** transpose v, 3*x^2 + 5*x*y + 11*y^2)
f = x^3 + y^3 + z^3 - t*x*y*z
v = matrix {{x,y,z}}
det diff(transpose v * v, f)
contract(x,m)
contract(x^2,m)
contract(matrix {{x,x^2,x^3,x^4}}, m)
contract(matrix {{x,x^2},{x^3,x^4}}, m)
f
v3 = symmetricPower(3,matrix{{x,y,z}})
contract(v3, f)
f = a * x^3 + b * x^2 * y + y^3
g = b * x^3 + a * x * y^2 + y^3
n = matrix {{f,g}} ** symmetricPower(2,matrix {{x,y}})
M = contract(transpose symmetricPower(5,matrix {{x,y}}), n)
det M
diff'(m, matrix {{x,x^2,x^3,x^4}})
diff'(m, matrix {{x,x^2},{x^3,x^4}})
contract'(m, matrix {{x,x^2,x^3,x^4}})
contract'(m, matrix {{x,x^2},{x^3,x^4}})
