-- Mayr-Meyer ideals
-- these have double exponential regularity in number of variables

mayr0 = (deg) -> (
    zdeg := z^deg;
    matrix {{S_0 * e_0 * zdeg - F_0 * e_0 * (a_0)^deg,
             S_0 * f_0 * zdeg - F_0 * f_0 * (b_0)^deg,
             S_0 * g_0 * zdeg - F_0 * g_0 * (c_0)^deg,
             S_0 * h_0 * zdeg - F_0 * h_0 * (d_0)^deg}})

mayri = (i) -> 
    matrix {{S_i * z - S_(i-1) * e_(i-1),
             F_(i-1) * e_(i-1) - S_(i-1) * f_(i-1),
             F_(i-1) * f_(i-1) * a_(i-1) - F_(i-1) * g_(i-1) * d_(i-1),
             S_(i-1) * h_(i-1) - F_i * z,
             S_(i-1) * g_(i-1) - F_(i-1) * h_(i-1),
             S_(i-1) * g_(i-1) - S_(i-1) * f_(i-1),
             F_(i-1) * f_(i-1) * e_i * (a_(i-1) * z - a_i * c_(i-1)),
             F_(i-1) * f_(i-1) * f_i * (a_(i-1) * z - b_i * c_(i-1)),
	     F_(i-1) * f_(i-1) * g_i * (a_(i-1) * z - c_i * c_(i-1)),
             F_(i-1) * f_(i-1) * h_i * (a_(i-1) * z - d_i * c_(i-1))}}

mayr = method()
mayr(ZZ,ZZ,Ring) := (n,deg,kk) -> (
     R = kk[S_0 .. S_n, F_0 .. F_n,
	       a_0 .. a_n, b_0 .. b_n, c_0 .. c_n, d_0 .. d_n,
	       e_0 .. e_n, f_0 .. f_n, g_0 .. g_n, h_0 .. h_n,
               z];
     mats = apply(n, i -> mayri (i+1));
     mats = prepend(mayr0 deg, mats);
     ideal matrix {mats})

end
restart
loadPackage "ExampleIdeals"
I = mayr(3,2,ZZ/101)
gbTrace=1
time gens gb I;
time gens gb(I,Algorithm=>Homogeneous2);

{* -- singular code
ring r = 101,(S_0, S_1, S_2, S_3, F_0, F_1, F_2, F_3, a_0, a_1, a_2, a_3, b_0, b_1, b_2, b_3, c_0, c_1, c_2, c_3, d_0, d_1, d_2, d_3, e_0, e_1, e_2, e_3, f_0, f_1, f_2, f_3, g_0, g_1, g_2, g_3, h_0, h_1, h_2, h_3, z), dp;
ideal i =    -F_0*a_0^2*e_0+S_0*e_0*z^2,
   -F_0*b_0^2*f_0+S_0*f_0*z^2,
   -F_0*c_0^2*g_0+S_0*g_0*z^2,
   -F_0*d_0^2*h_0+S_0*h_0*z^2,
   -S_0*e_0+S_1*z,
   F_0*e_0-S_0*f_0,
   F_0*a_0*f_0-F_0*d_0*g_0,
   S_0*h_0-F_1*z,
   S_0*g_0-F_0*h_0,
   -S_0*f_0+S_0*g_0,
   -F_0*a_1*c_0*e_1*f_0+F_0*a_0*e_1*f_0*z,
   -F_0*b_1*c_0*f_0*f_1+F_0*a_0*f_0*f_1*z,
   -F_0*c_0*c_1*f_0*g_1+F_0*a_0*f_0*g_1*z,
   -F_0*c_0*d_1*f_0*h_1+F_0*a_0*f_0*h_1*z,
   -S_1*e_1+S_2*z,
   F_1*e_1-S_1*f_1,
   F_1*a_1*f_1-F_1*d_1*g_1,
   S_1*h_1-F_2*z,
   S_1*g_1-F_1*h_1,
   -S_1*f_1+S_1*g_1,
   -F_1*a_2*c_1*e_2*f_1+F_1*a_1*e_2*f_1*z,
   -F_1*b_2*c_1*f_1*f_2+F_1*a_1*f_1*f_2*z,
   -F_1*c_1*c_2*f_1*g_2+F_1*a_1*f_1*g_2*z,
   -F_1*c_1*d_2*f_1*h_2+F_1*a_1*f_1*h_2*z,
   -S_2*e_2+S_3*z,
   F_2*e_2-S_2*f_2,
   F_2*a_2*f_2-F_2*d_2*g_2,
   S_2*h_2-F_3*z,
   S_2*g_2-F_2*h_2,
   -S_2*f_2+S_2*g_2,
   -F_2*a_3*c_2*e_3*f_2+F_2*a_2*e_3*f_2*z,
   -F_2*b_3*c_2*f_2*f_3+F_2*a_2*f_2*f_3*z,
   -F_2*c_2*c_3*f_2*g_3+F_2*a_2*f_2*g_3*z,
   -F_2*c_2*d_3*f_2*h_3+F_2*a_2*f_2*h_3*z;
rtimer=1;
ideal j=groebner(i);
*}

load "gb-mayr.m2"
gbTrace=1
I = mayr(4,2,ZZ/101);
betti I
numgens ring I
time syz gens I;

