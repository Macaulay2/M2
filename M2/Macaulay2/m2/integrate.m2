--		Copyright 1993-1999 by Daniel R. Grayson


simpson := (f,a,b,k) -> (
     count := 0;
     oldf := f;
     f = x -> (
	  count = count + 1;
	  oldf x);
     h := (b-a)/(2*k);
     ff := i -> f(a + i*h);
     int := h/3 (
	  ff 0 
	  + 4 sum(k  , j -> ff(2*j+1)) 
	  + 2 sum(k-1, j -> ff(2*j+2))
	  + ff(2*k)
	  );
     << count << " function evaluations\n";
     int
     )


xx := {
     ,					  -- n==0
     ,					  -- n==1
     {
	  -.577350269189626,
	  .577350269189626},		  -- n==2
     {
	  -.774596669241483,
	  0,
	  .774596669241483},		  -- n==3
     {
	  -.861136311594053,
	  -.339981043584856,
	  .339981043584856,
	  .861136311594053
	  },				  -- n==4
     {
	  -.906179845938664,
	  -.538469310105683,
	  0,
	  .538469310105683,
	  .906179845938664
	  },				  -- n==5
     {
	  -.932469514203152,
	  -.661209386466265,
	  -.238619186083197,
	  .238619186083197,
	  .661209386466265,
	  .932469514203152
	  }				  -- n==6
     }

ww := {
     ,					  -- n==0
     ,					  -- n==1
     {1,1},				  -- n==2
     {5/9.,8/9.,5/9.},			  -- n==3
     {
	  .347854845137454,
	  .652145154862546,
	  .652145154862546,
	  .347854845137454
	  },				  -- n==4
     {
	  .236926885056189,
	  .478628670499366,
	  .568888888888889,
	  .478628670499366,
	  .236926885056189,
	  },				  -- n==5
     {
	  .171324492379170,
	  .360761573048139,
	  .467913934572691,
	  .467913934572691,
	  .360761573048139,
	  .171324492379170
	  }				  -- n==6
     }

gaussian := (f,a,b,n) -> (
     r := (b-a)/2;
     s := (b+a)/2;
     w := ww#n;
     x := xx#n;
     r * sum(n, i -> w#i * f(r * x#i + s)))


gauss := (f,a,b,k,n) -> (
--   count := 0;
--   oldf := f;
--   f = x -> (
--	  count = count + 1;
--	  oldf x);
     h := (b-a)/k;
     int := sum(k, i -> gaussian(f, a + i*h, a + (i+1)*h, n));
--     << count << " function evaluations\n";
     int
     )

integrate = method()
integrate(Function, Number, Number) := (f,a,b) -> gauss(f,a,b,4,6)

-- nodes and weights for 20-point gauss-laguerre formula
-- https://dlmf.nist.gov/3.5.27
gaussLaguerreNodes = {
    0.705398896919887533667e-1,
    0.372126818001611443794,
    0.916582102483273564668,
    0.170730653102834388069e1,
    0.274919925530943212965e1,
    0.404892531385088692237e1,
    0.561517497086161651410e1,
    0.745901745367106330977e1,
    0.959439286958109677247e1,
    0.120388025469643163096e2,
    0.148142934426307399785e2,
    0.179488955205193760174e2,
    0.214787882402850109757e2,
    0.254517027931869055035e2,
    0.299325546317006120067e2,
    0.350134342404790000063e2,
    0.408330570567285710620e2,
    0.476199940473465021399e2,
    0.558107957500638988908e2,
    0.665244165256157538186e2
    }

gaussLaguerreWeights = {
    0.168746801851113862149,
    0.291254362006068281717,
    0.266686102867001288550,
    0.166002453269506840031,
    0.748260646687923705401e-1,
    0.249644173092832210728e-1,
    0.620255084457223684745e-2,
    0.114496238647690824204e-2,
    0.155741773027811974780e-3,
    0.154014408652249156894e-4,
    0.108648636651798235148e-5,
    0.533012090955671475093e-7,
    0.175798117905058200358e-8,
    0.372550240251232087263e-10,
    0.476752925157819052449e-12,
    0.337284424336243841237e-14,
    0.115501433950039883096e-16,
    0.153952214058234355346e-19,
    0.528644272556915782880e-23,
    0.165645661249902329591e-27
    }

integrate(Function, Number, InfiniteNumber) := (f, a, b) -> (
    if b == infinity then (
	g := x -> exp x * f(x + a);
	sum(20, i -> gaussLaguerreWeights_i * g(gaussLaguerreNodes_i)))
    else if b == -infinity then -integrate(f, -infinity, a)
    else error "expected infinity or -infinity")

integrate(Function, InfiniteNumber, Number) := (f, a, b) -> (
    if a == infinity then -integrate(f, b, infinity)
    else if a == -infinity then integrate(x -> f(-x), -b, infinity)
    else error "expected infinity or -infinity")

-- nodes and weights for 20-point gauss-hermite formula
-- https://dlmf.nist.gov/3.5.28
gaussHermiteNodes = {
    0.245340708300901249904,
    0.737473728545394358706,
    0.123407621539532300789e1,
    0.173853771211658620678e1,
    0.225497400208927552308e1,
    0.278880605842813048053e1,
    0.334785456738321632691e1,
    0.394476404011562521038e1,
    0.460368244955074427308e1,
    0.538748089001123286202e1
    }

gaussHermiteWeights = {
    0.462243669600610089650,
    0.286675505362834129720,
    0.109017206020023320014,
    0.248105208874636108822e-1,
    0.324377334223786183218e-2,
    0.228338636016353967257e-3,
    0.780255647853206369415e-5,
    0.108606937076928169400e-6,
    0.439934099227318055363e-9,
    0.222939364553415129252e-12
    }

integrate(Function, InfiniteNumber, InfiniteNumber) := (f, a, b) -> (
    if a == b then 0
    else if a == -infinity and b == infinity then (
	g := x -> exp(x^2) * f(x);
	sum(10, i ->  (
		x := gaussHermiteNodes_i;
		gaussHermiteWeights_i * (g(x) + g(-x)))))
    else if a == infinity and b == -infinity then -integrate(f, b, a)
    else error "expected infinity and/or -infinity")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
