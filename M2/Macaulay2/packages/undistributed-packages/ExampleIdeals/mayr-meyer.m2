-- Mayr-Meyer ideals
-- these have double exponential regularity in number of variables

mayr = method()
mayr(ZZ,ZZ,Ring) := (n,deg,kk) -> (
     (S,F,a,b,c,d,e,f,g,h,z) := ("S","F","a","b","c","d","e","f","g","h","z")/getSymbol;
     R := kk[S_0 .. S_n, F_0 .. F_n,
	       a_0 .. a_n, b_0 .. b_n, c_0 .. c_n, d_0 .. d_n,
	       e_0 .. e_n, f_0 .. f_n, g_0 .. g_n, h_0 .. h_n,
               z, MonomialSize=>8];
     (S,F,a,b,c,d,e,f,g,h,z) = (S,F,a,b,c,d,e,f,g,h,z)/value;
     mayr0 := (deg) -> (
	 zdeg := z^deg;
	 matrix {{S_0 * e_0 * zdeg - F_0 * e_0 * (a_0)^deg,
		  S_0 * f_0 * zdeg - F_0 * f_0 * (b_0)^deg,
		  S_0 * g_0 * zdeg - F_0 * g_0 * (c_0)^deg,
		  S_0 * h_0 * zdeg - F_0 * h_0 * (d_0)^deg}});
     mayri := (i) -> 
	 matrix {{S_i * z - S_(i-1) * e_(i-1),
		  F_(i-1) * e_(i-1) - S_(i-1) * f_(i-1),
		  F_(i-1) * f_(i-1) * a_(i-1) - F_(i-1) * g_(i-1) * d_(i-1),
		  S_(i-1) * h_(i-1) - F_i * z,
		  S_(i-1) * g_(i-1) - F_(i-1) * h_(i-1),
		  S_(i-1) * g_(i-1) - S_(i-1) * f_(i-1),
		  F_(i-1) * f_(i-1) * e_i * (a_(i-1) * z - a_i * c_(i-1)),
		  F_(i-1) * f_(i-1) * f_i * (a_(i-1) * z - b_i * c_(i-1)),
		  F_(i-1) * f_(i-1) * g_i * (a_(i-1) * z - c_i * c_(i-1)),
		  F_(i-1) * f_(i-1) * h_i * (a_(i-1) * z - d_i * c_(i-1))}};
     mats := apply(n, i -> mayri (i+1));
     mats = prepend(mayr0 deg, mats);
     ideal matrix {mats})

-- Code from Irena Swanson:
-- create the Mayr-Meyer ideal in 8n+2 variables, exponent d, base field F
-- variables: b_(i,j), c_(i,j): i = 0..n-2, j = 1..4; s, f stand for s_0, f_0
-- other s_i, f_i rewritten in terms of other variables
-- n >= 1
mayrMeyer = (F,n,d) -> (
 if (n < 1) then n = 1;
 (s,f,c,b) := ("s","f","c","b")/getSymbol;
 R := F[s,f,c_(0,1)..c_(n-1,4),b_(0,1)..b_(n-1,4),MonomialSize=>8];
 (s,f,c,b) = (s,f,c,b)/value;
 -- level 0
 l0 := ideal apply(4, i -> (s - f*b_(0,i+1)^d)*c_(0,i+1));
 -- level 1
 l11 := ideal(f*c_(0,1)-s*c_(0,2),
       f*c_(0,4)-s*c_(0,3),
       s*(c_(0,2)-c_(0,3)),
       f*(c_(0,2)*b_(0,1)-c_(0,3)*b_(0,4)));
 if (n < 2) then
       return (l0 + l11 + ideal(f*c_(0,2)*(b_(0,2)-b_(0,3))));
 l12 := ideal apply(4, i ->
	f*c_(0,2)*c_(1,i+1)*(b_(0,2)-b_(1,i+1)*b_(0,3)));
 -- first four of levels 2 .. n:
 ff := sum apply(n-1, i -> s*(product apply(i, j -> c_(j,1)) *
       ideal(c_(i,4)*c_(i+1,1)-c_(i,1)*c_(i+1,2),
             c_(i,4)*c_(i+1,4)-c_(i,1)*c_(i+1,3),
             c_(i,1)*(c_(i+1,3)-c_(i+1,2)),
             c_(i,4)*(c_(i+1,2)*b_(i+1,1)-c_(i+1,3)*b_(i+1,4)))));
 -- last four of levels 2 .. n-1:
 -- the if below due to M2 outputting the 0 ideal in ZZ rather than in R
 if (n == 2) then (
   lf := ideal(0_R);
 ) else (
   lf = sum apply(n-2, i -> (product apply(i, j -> c_(j,1))) *
       s * c_(i,4)*c_(i+1,2) *
       ideal apply(4, j ->
	c_(i+2,j+1)*(b_(i+1,2)-b_(i+2,j+1)*b_(i+1,3))));
 );
 -- last level n generator:
 ln := (product apply(n-2, j -> c_(j,1))) *
       ideal(s * c_(n-2,4)*c_(n-1,2)*(b_(n-1,2)-b_(n-1,3)));
 l0 + l11 + l12 + ff + lf + ln
)

end
restart
loadPackage "ExampleIdeals"
debug ExampleIdeals
I = mayr(3,2,ZZ/101)
mayrMeyer(ZZ/101,3,2)
gbTrace=1
time gens gb I;
time gens gb(I,Algorithm=>Homogeneous2);

-* -- singular code
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
timer=1;
ideal j=groebner(i);
*-
-* -- Magma
R<S_0, S_1, S_2, S_3, F_0, F_1, F_2, F_3, a_0, a_1, a_2, a_3, b_0, b_1, b_2, b_3, c_0, c_1, c_2, c_3, d_0, d_1, d_2, d_3, e_0, e_1, e_2, e_3, f_0, f_1, f_2, f_3, g_0, g_1, g_2, g_3, h_0, h_1, h_2, h_3, z> := PolynomialRing(GF(101),41,"grevlex");
I := ideal<R |  -F_0*a_0^2*e_0+S_0*e_0*z^2,
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
   -F_2*c_2*d_3*f_2*h_3+F_2*a_2*f_2*h_3*z>;
I;   
time J := GroebnerBasis(I);
*-

gbTrace=1
I = mayr(4,2,ZZ/101);
betti I
numgens ring I
time syz gens I;

