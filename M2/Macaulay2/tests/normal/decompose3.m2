R = QQ [a, b, c, d, e]
S = R [w_0, w_1, w_2, Join=>false]
J = ideal(b*w_0-c*w_1+d*w_2,a*w_0-b*w_1+c*w_2)
decompose J						    -- oops -- fixed
assert( intersect oo == J )
