
fastmingens := (m) -> (
     sendgg(ggPush m, -- the matrix
	  ggPush 0, -- collect syzygyies?
	  ggPush 0, -- how many rows
	  ggPush 0, -- 0 for now
	  gggb);
     g := newHandle();
     sendgg(ggPush g, 
	  ggPush {}, -- degree bound
	  ggPush {-1, -- generator limit
	       -1,    -- syzygy imit
	       -1,    -- pair limit
	       -1,    -- codim limit (not used)
	       1,     -- stop at mingens?
	       -1,    -- subring limit
	       0});   -- strategy
     sendgg ggcalc;
     sendgg(ggPush g, 
	    gggetmingens,
            ggdup, ggPush 1, ggPush (-1), ggsortcolumns, ggsubmatrix);
     getMatrix ring m)
     
fastgb = (m) -> (
     sendgg(ggPush m, -- the matrix
	  ggPush 0, -- collect syzygyies?
	  ggPush 0, -- how many rows
	  ggPush 0, -- 0 for now
	  gggb);
     g := newHandle();
     sendgg(ggPush g, 
	  ggPush {}, -- degree bound
	  ggPush {-1, -- generator limit
	       -1,    -- syzygy limit
	       -1,    -- pair limit
	       -1,    -- codim limit (not used)
	       0,     -- stop at mingens?
	       -1,    -- subring limit
	       0});   -- strategy
     sendgg ggcalc;
     sendgg(ggPush g, gggetgb);
     getMatrix ring m)

-- This is much faster than (I:f), even with the first line.
fastquotient = (I,f) -> (
     -- I should be a matrix
     m := f | I;
     m = m ** (ring I)^{degree f};
     sendgg(ggPush m, 
	  ggPush 1, 	 
	  ggPush 1,
	  ggPush 0,
	  gggb);
     g := newHandle();
     sendgg(ggPush g, ggPush {}, ggPush {-1,-1,-1,-1,0,-1,0});
     sendgg ggcalc;
     sendgg(ggPush g, gggetsyz);
     fastmingens getMatrix ring m)

fastequal := (m,n) -> (
     sendgg(ggPush m, ggPush n, ggisequal);
     eePopInt() =!= 0)


quotMin = (I, facs, F) -> (
     J := fastquotient(I,F);
     if #facs > 1 then (
	  i := 0;
     	  while i < #facs and #facs > 1 do (
	       fac1 := drop(facs,{i,i});
	       G := product fac1;
	       J1 := fastquotient(I,G);
	       if fastequal(J,J1)
	       then (
	      	    facs = fac1;
	      	    F = G;
	      	    )
	       else i = i+1;
     	       );
	  );
     {J,facs,F}
     )
-- Sort the list polynomials in increasing degree order
sortByDegree = (facs) -> (
     x := apply(facs, f -> {degree f, f});
     x = sort x;
     apply(x, i -> i#1))

minSatPPD = (I, facs) -> (
     R := ring I;
     facs = sortByDegree facs;
     F0 := product facs;
     ret := quotMin(gens I,facs,F0);
     facs0 := ret#1;
     F := 1_R;   -- will be s.t. sat(I,F0) = I:F
     Iprev := gens I;
     while not fastequal(ret#0, Iprev) do (
	  F = F * ret#2;
	  Iprev = ret#0;
	  ret = quotMin toSequence ret;
	  );
     {ideal ret#0, F0, F, facs0}
     )

TEST ///
R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
F = x*y*z
time fastgb gens I
time gb I
time fastquotient(gens I,F)
time (I:F)
time (quotMin(gens I,{x,y,z},F))
time (minSatPPD(I,{x,y,z}))
time (minSatPPDold(I,{x,y,z}))

I1 = I + ideal(v*w*y, u*v*x, s*v*y, s*t*y, b*s*x, x*y*z, s*t*v, u*v*w, 
     b*s*t, b*s*u, u*w*x, b*u*x, b*t*x, b*t*u, t*w*z)
time (minSatPPD(I1,{b,t}))
time (minSatPPDold(I1,{b,t}))

///
