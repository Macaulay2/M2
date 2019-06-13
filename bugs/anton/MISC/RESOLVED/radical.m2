R = QQ[z0, z1, z2, z3, z4, z5, w0, w1, w2, w3, w4, w5]
I' = ideal (z0 + w0 - 1, z1 + w1 - 1, z2 + w2 - 1, z3 + w3 - 1, z4 + w4 - 1, z5 + w5 - 1,
    z0*z1*z2*z3*z5 - 1, 
    -z1^2*z3^2*w0*w5 + w1*w3, 
    -z0*z5*w1*w2*w4 + w0*w5, 
    -z0*z5*w2*w3*w4 + w0*w5, 
    w2^2*w4^2 - z2*z4*w0*w5)
I = saturate(I',product(gens R))
dec = decompose I'
end
restart
load "radical.m2"
dim I
degree I
assert(radical I == I)
assert(I == last dec)
radical last dec -- takes forever
