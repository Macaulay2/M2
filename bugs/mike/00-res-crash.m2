kk = ZZ/101
A = kk[a..d]
I = ideal"a2,b2,c2,d2"
J = super basis(3,I)
B = kk[x_0..x_15]
phi = map(A,B,J)
L = trim ker phi
C = B/L
M = coker vars C
gbTrace=10
res(M, LengthLimit=>5)  -- ouch!! crash  CRASHES on gcc 4.7.2, macosx, debug or optimized
betti oo

res(M, LengthLimit=>5, Strategy=>2)  -- ouch!! crash  CRASHES on gcc 4.7.2, macosx, debug or optimized
end

p $rbp
p *((void **)$)

# see d/debug.c for these trap routines, variables.
p trapcount 
b IM2_res_make

kk = ZZ/101
B = kk[x_0..x_15]
L=ideal(x_14^2-x_11*x_15,x_11*x_14-x_10*x_15,x_7*x_14-x_6*x_15,x_3*x_14-x_2*x_15,x_13^2-x_7*x_15,x_11*x_13-x_9*x_15,x_10*x_13-x_9*x_14,x_9*x_13-x_6*x_14,x_7*x_13-x_5*x_15,x_6*x_13-x_5*x_14,x_3*x_13-x_1*x_15,x_2*x_13-x_1*x_14,x_12^2-x_3*x_15,x_11*x_12-x_8*x_15,x_10*x_12-x_8*x_14,x_9*x_12-x_8*x_13,x_8*x_12-x_2*x_14,x_7*x_12-x_4*x_15,x_6*x_12-x_4*x_14,x_5*x_12-x_4*x_13,x_4*x_12-x_1*x_13,x_3*x_12-x_0*x_15,x_2*x_12-x_0*x_14,x_1*x_12-x_0*x_13,x_11^2-x_10*x_14,x_7*x_11-x_6*x_14,x_3*x_11-x_2*x_14,x_7*x_10-x_6*x_11,x_3*x_10-x_2*x_11,x_9^2-x_6*x_10,x_7*x_9-x_5*x_11,x_6*x_9-x_5*x_10,x_3*x_9-x_1*x_11,x_2*x_9-x_1*x_10,x_8^2-x_2*x_10,x_7*x_8-x_4*x_11,x_6*x_8-x_4*x_10,x_5*x_8-x_4*x_9,x_4*x_8-x_1*x_9,x_3*x_8-x_0*x_11,x_2*x_8-x_0*x_10,x_1*x_8-x_0*x_9,x_7^2-x_5*x_13,x_3*x_7-x_1*x_13,x_6^2-x_5*x_9,x_3*x_6-x_2*x_7,x_2*x_6-x_1*x_9,x_3*x_5-x_1*x_7,x_2*x_5-x_1*x_6,x_4^2-x_1*x_5,x_3*x_4-x_0*x_7,x_2*x_4-x_0*x_6,x_1*x_4-x_0*x_5,x_3^2-x_0*x_12,x_2^2-x_0*x_8,x_1^2-x_0*x_4)
C = B/L
M = coker vars C
res(M, LengthLimit=>5)  -- ouch!! crash  CRASHES on gcc 4.7.2, macosx, debug or optimized

-----------------------------
-- from rex, jan 2013

restart
gbTrace=1
R=QQ[t_1..t_16];

rel=matrix{{t_16*t_1-t_15*t_4-t_8*t_12+t_9*t_11,
        t_16*t_2-t_15*t_5+t_7*t_12-t_9*t_10,
        t_16*t_3-t_15*t_6-t_7*t_11+t_8*t_10,
        t_14*t_7-t_13*t_10-t_2*t_6+t_3*t_5,
        t_14*t_8-t_13*t_11+t_1*t_6-t_3*t_4,
        t_14*t_9-t_13*t_12-t_1*t_5+t_2*t_4,
        t_10*t_1+t_11*t_2+t_12*t_3+t_14*t_15,
        t_7*t_1+t_8*t_2+t_9*t_3+t_13*t_15,
        t_7*t_4+t_8*t_5+t_9*t_6+t_13*t_16,
        t_10*t_4+t_11*t_5+t_12*t_6+t_14*t_16
        }};
R=R/ideal(rel);


use R;

I=ideal apply(toList(1..16),i->(t_i))
d1 = koszul(0,gens I);

d2 = koszul(1,gens I);

M = prune HH(d1,d2);

resM=resolution(M,LengthLimit=>7,Strategy=>0)

-- MES attempts:
--time res (coker vars R, LengthLimit=>4);
restart
gbTrace=1
R=ZZ/32003[t_1..t_16, MonomialSize=>8];
rel=matrix{{t_16*t_1-t_15*t_4-t_8*t_12+t_9*t_11,
        t_16*t_2-t_15*t_5+t_7*t_12-t_9*t_10,
        t_16*t_3-t_15*t_6-t_7*t_11+t_8*t_10,
        t_14*t_7-t_13*t_10-t_2*t_6+t_3*t_5,
        t_14*t_8-t_13*t_11+t_1*t_6-t_3*t_4,
        t_14*t_9-t_13*t_12-t_1*t_5+t_2*t_4,
        t_10*t_1+t_11*t_2+t_12*t_3+t_14*t_15,
        t_7*t_1+t_8*t_2+t_9*t_3+t_13*t_15,
        t_7*t_4+t_8*t_5+t_9*t_6+t_13*t_16,
        t_10*t_4+t_11*t_5+t_12*t_6+t_14*t_16
        }};
R=R/ideal(rel);

time res (coker vars R, LengthLimit=>5);  -- ok
time res (coker vars R, LengthLimit=>6);  -- ok, 76 sec on Mike's rMBP
time res (coker vars R, LengthLimit=>7);  -- crashes at about 700 MB... ok after fix.



                                                                                         