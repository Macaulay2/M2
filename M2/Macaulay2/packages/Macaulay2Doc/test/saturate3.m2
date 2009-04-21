-- bug reported by Paolo Aluffi <aluffi@mail.math.fsu.edu>

QQ[e1,e2,e3,e4,e5,e6];
I=ideal(e1*e2*e3*e4+e1*e2*e3*e5+e1*e2*e3*e6+e1*e2*e4*e6+e1*e2*e5*e6
     +e1*e3*e4*e5+e1*e3*e5*e6+e1*e4*e5*e6+e2*e3*e4*e5+e2*e3*e4*e6
     +e2*e4*e5*e6+e3*e4*e5*e6);
J=ideal jacobian I;
m=numgens J;
R:=(ring J)[t_0 .. t_(m-1),Join=>false];
II:=substitute(J,R);
JJ:=ideal apply(0..(m-2), i -> apply((i+1)..(m-1), j -> (II_i*t_j-II_j*t_i)));

time saturate(JJ,II_0,Strategy => Bayer)
--stdio:10:6:(1):[0]: error: expected argument 2 to be an integer
--     -- used 4.3 seconds

R=QQ[x];
assert( saturate(ideal(0_R),0_R,Strategy => Iterate) == 1 )

assert( saturate(ideal(0_R),0_R,Strategy => Eliminate) == 1 )
--stdio:14:9:(1):[1]: error: expected a variable of the ring

assert( saturate(ideal(0_R),0_R,Strategy => Bayer) == 1 )
--stdio:15:9:(1):[1]: error: expected degree to be an integer or list of integers
