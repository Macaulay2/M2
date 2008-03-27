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
mayr(3,2,ZZ/101)
I = mayr(4,2,ZZ/101)
time gens gb I;

load "gb-mayr.m2"
gbTrace=1
I = mayr(4,2,ZZ/101);
betti I
numgens ring I
time syz gens I;

eg1 = () -> (
     print "benchmark gb-mayr-3-2-char101-nvars41-gb3266";
     I = mayr(3,2,ZZ/101);
     time gens gb I;
     )

eg2 = () -> (
     print "benchmark gb-mayr-4-2-char101-nvars51-gb??";
     I = mayr(4,2,ZZ/101);
     time gens gb I;
     )

----------------------------
doBench = (which,bens) -> (
  if instance(which,ZZ) 
    then (bens#which)()
    else scan(#bens, i -> 
       run ("M2 --silent -e which=" 
	    | i | " " 
	    | currentFileName 
	    | /// -e "exit(0)"///)))

--gbTrace=1
--doBench(which, {eg1,eg2})
end
load "gb-mayr.m2"
gbTrace=1
I = mayr(4,2,ZZ/101);
betti I
numgens ring I
time syz gens I;

