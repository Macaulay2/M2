R = degreesRing 1
t = R_0
h = 1-t^2-2*t^5-t^6+2*t^7+3*t^8-2*t^10
assert( h % (1-t) == 0 )
assert( (h // (1-t)) * (1-t) == h )
