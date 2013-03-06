-- -*- M2 -*-

restart
  A = ZZ[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13]
  f0 = ((x3 * x0) - (1))
  f1 = ((x1 * x3) - (((3) * (x5 ^ 2)) + (((2) * (x9 * x5)) + (x10 - (x6 * x4)))))
  f2 = ((x2 * x3) - (-((x5 ^ 3)) + ((x10 * x5) + (((2) * x12) - (x7 * x4)))))
  f3 = (x11 - ((x1 ^ 2) + (((x6 * x1) - x9) - ((2) * x5))))
  f4 = (x8 - (((-((x1 + x6)) * x11) - x2) - x7))
  f5 = (x3 - (((2) * x4) + ((x6 * x5) + x7)))
  f6 = (((x4 ^ 2) + ((x6 * (x5 * x4)) + (x7 * x4))) - ((x5 ^ 3) + ((x9 * (x5^ 2)) + ((x10 * x5) + x12))))
  f7 = (((((x8 ^ 2) + ((x6 * (x11 * x8)) + (x7 * x8))) - ((x11 ^ 3) + ((x9 * (x11 ^ 2)) + ((x10 * x11) + x12)))) * x13) - (1))
  I = ideal(f0,f1,f2,f3,f4,f5,f6,f7)
  assert( 1 % I == 0 )

-- compute the GB of I

end

-- working on this bug: 9 Feb 2013, MES
-- issue: it goes off into la la land.

see I
J = trim I
see J

F0 = I_0
F1 = I_1
F2 = I_2
F3 = - I_3
F4 = I_4
F5 = - I_5
F6 = F2 + I_6
F7 = I_7
J = ideal(F0,F1,F2,F3,F4,F5,F6,F7)
see J
trim J
see oo

gbTrace=15
I = ideal I_*
gens gb I;
