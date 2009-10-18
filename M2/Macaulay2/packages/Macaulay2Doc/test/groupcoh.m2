-- Sarah Wasserman

R = ZZ/2[e_2 .. e_5, x_1, y_1, u_1, x_4, x_5,
     Degrees => {
	  {1,0},{1,0},{1,0},{1,0},
	  {1,1},{1,1},{1,2},{1,1},{1,1}
     	  }
     ]/(
     e_2^2 - 1,
     e_2*e_3 - e_3,
     e_2*e_4 - e_4,
     e_2*e_5 - e_5,
     e_3^2,
     e_4^2,
     e_5^2,
     e_3*e_4,
     e_3*e_5,
     e_4*e_5,
     e_2*x_4 - x_4 - y_1*e_4,
     e_2*x_5 - x_5 - y_1*e_5,
     e_3*y_1,
     e_3*x_4,
     e_3*x_5,
     e_4*x_1,
     e_4*x_4 - (x_1 + y_1)*(1 + e_2),
     e_4*x_5 - x_1*e_3,
     e_5*x_1 - y_1*e_5,
     e_5*x_4 - x_1*e_3,
     e_5*x_5 - x_1*(1 + e_2),
     x_1^2 - x_1*y_1,
     x_1*x_4,
     y_1*x_5 - x_1*x_5,
     x_4*x_5,
     x_4^2 - y_1*(x_1+y_1),
     x_5^2 - x_1^2
     )

f = poincare R 
T = ring f

g = ( f
     // (1 - T_0)
     // (1 - T_0)
     // (1 - T_0)
     // (1 - T_0)
     // (1 - T_0*T_1)
     // (1 - T_0*T_1)
     // (1 - T_0*T_1)
     -- // (1 - T_0*T_1)
     -- // (1 - T_0*T_1^2)
     )

g = substitute(g, T_0 => 1)

print ((expression g) / (expression (1 - T_1) * (expression (1 - T_1^2))))

assert( g == -T_1^2+4*T_1+5 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test groupcoh.out"
-- End:
