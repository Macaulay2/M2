newPackage(
	"ExampleIdeals",
    	Version => "0.1", 
    	Date => "February 8, 2007",
    	Authors => {{Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"}},
    	Headline => "a package consisting of examples of ideals",
    	DebuggingMode => true
    	)

export {
     readExampleFile,
     ExampleTable,
     box,
     example,

     examplesDGP,
     egPermanents,
     egHaas,
     katsura,
     cyclicRoots,
     cyclicRootsHomogeneous,
     SIN
     }

ExampleTable = new Type of HashTable

box = method()
example = method(Options=>{CoefficientRing => ZZ/32003,
	                   Ring => null})

show(ExampleTable, String) := (H,x) -> print H#x#1()
show(ExampleTable, ZZ) := (H,x) -> print (("-- "|H#x#0) || H#x#1())
show(ExampleTable, List) := (H,x) -> scan(x, x1 -> show(H,x1))
show(ExampleTable) := (H) -> show(H, sort keys H)

box(ExampleTable, ZZ) := (H,x) -> box(H,{x})
box(ExampleTable, String) := (H,x) -> box(H,{x})
box(ExampleTable, List) := (H,x) -> netList apply(x, x1 -> {x1, ("-- "|H#x1#0) || H#x1#1()})
box(ExampleTable) := (H) -> box(H, sort keys H)

example(ExampleTable, ZZ) :=
example(ExampleTable, String) := opts -> (H,x) -> (
     R1 := if opts#?Ring then opts#Ring else null;
     kk := if R1 === null 
             then opts.CoefficientRing 
	     else coefficientRing R1; 
     I := value replace("kk", toString kk, H#x#1());
     if R1 =!= null then (
     	  nvars := numgens ring I;
	  if numgens R1 < nvars then 
	    error ("expected ring with at least "|nvars|" variables");
	  substitute(I, (vars R1)_{0..nvars-1})
	  )
     else I)

readExampleFile = (filename, coeffring) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => () -> replace("kk", toString coeffring, demark("\n",drop(G#i,1)))))
     )

examplesDGP = method()
examplesDGP Ring := (kk) -> readExampleFile("ExampleIdeals/DGP.m2", kk)


-- This is a list of examples from several sources

egPermanents = (kk,m,n,size) -> (
     R = kk[vars(0..m*n-1)];
     I = permanents(size,genericMatrix(R,m,n))
     )

egHaas = (kk) -> (
     -- From Hashemi, Efficient algorithms for computing Noether normalization
     R = kk[t,x,y,z];
     I = ideal"x8+zy4-y,y8+tx4-x,64x7y7-16x3y3zt+4y3z+4x3t-1"
     )

--katsura = method()
--katsura(ZZ,Ring) := (n,R) -> (
--     )
katsura = (n,kk) -> (
     -- This is written to match the Singular version, which seems to differ
     -- from the POSSO version
     n = n-1;
     R = kk[vars(0..n)];
     L := gens R;
     u := (i) -> (
	  if i < 0 then i = -i;
	  if i <= n then L_i else 0_R);
     f1 := -1 + sum for i from -n to n list u i;
     I = ideal prepend(f1,
	  apply(0..n-1, i -> (
	       - u i + sum(-n..n, j -> (u j) * (u (i-j)))
	       )))
     )

cyclicRoots = (n,kk) -> (
     R = kk[vars(0..n-1)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k -> R_((i+k)%n)))) 
       + ideal(product gens R - 1))

cyclicRootsHomogeneous = (n,kk) -> (
     R = kk[vars(0..n)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k -> R_((i+k)%n)))) 
       + ideal(product(n, i -> R_i) - R_n^n))

commuting4by4 = (kk) -> (
  R = kk[vars(0..31),
         MonomialOrder=>{8, 12, 12}, 
	 MonomialSize=>8];
  I = ideal"
          -jo+ip-vA+uB-xC+wD, 
	  -ap+bo+cp-do+kB-lA+mD-nC, 
	  -aB+bA+eB-fA+pq-or-zC+yD, 
	  -aD+bC+gD-hC+ps-ot+BE-AF, 
	  aj-bi-cj+di-qv+ru-sx+tw, 
	  jo-ip-lq+kr-ns+mt, 
	  -cr+dq+er-fq-iB+jA-sz+ty, 
	  -ct+ds+gt-hs-iD+jC-qF+rE, 
	  av-bu-ev+fu-jk+il-xE+wF, 
	  cl-dk-el+fk+mF-nE+ov-pu, 
	  lq-kr+vA-uB-zE+yF, 
	  -eF+fE+gF-hE+ls-kt+vC-uD, 
	  ax-bw-gx+hw-jm+in-vy+uz, 
	  cn-dm-gn+hm+kz-ly+ox-pw, 
	  ez-fy-gz+hy+nq-mr+xA-wB, 
	  ns-mt+xC-wD+zE-yF")


-- The following contain the resolution tests for
-- homogeneous ideals/modules contained in the
-- paper "Standard bases, syzygies, and their 
-- implementation in SINGULAR", by Grassmann, Greuel,
-- Martin, Neumann, Pfister, Pohl, Schoenemann,
-- Siebert.
--
SIN = new MutableHashTable

singF = (a,b,c,t) -> x^a + y^b + z^(3*c) + x^(c+2) * y^(c-1) + 
   x^(c-1) * y^(c-1) * z^3 + x^(c-2) * y^c * (y^2 + t * x)^2
singH = (a) -> x^a + y^a + z^a + x * y * z * (x + y + z)^2 + (x+y+z)^3
singG = (a,b,c,d,e,t) -> x^a + y^b + z^c + x^d * y^(e-5) + 
   x^(d-2) * y^(e-3) + x^(d-3) * y^(e-4) * z^2 + 
   x^(d-4) * y^(e-4) * (y^2 + t * x)^2

PJ = (R,m) -> (
    PJf := (a,b) -> R_(a-1) R_(b-1) - R_(a-2) R_(b+1);
    if m < 4 then 
	matrix(R^1, R^0, {{}})
    else if m == 4 then 
	matrix{{R_3 R_0 - R_1^2, R_3 R_1 - R_2^2}}
    else if m == 5 then 
	(PJ(R,4)) | matrix{{PJf(5,1), PJf(5,2), R_4 R_2 - R_3^2 + R_1 R_0}}
    else if m == 6 then
	(PJ(R,5)) | matrix{{PJf(6,1), PJf(6,2), PJf(6,3), R_5 R_3 - R_4^2 - R_1^2}}
    else if m//2 == 1 then
	(PJ(R,m-1)) | matrix(R, m-3, i -> PJf(m,i+1)) | matrix{{R_(m-1) R_(m-3) - R_(m-2)^2 - R_(m-4) R_(m-6)}}
    else if m//2 == 0 then
	(PJ(R,m-1)) | matrix(R, m-3, i -> PJf(m,i+1)) | matrix{{R_(m-1) R_(m-3) - R_(m-2)^2 - 
                                                                R_(m-2)^2 - R_(m-3) R_(m-5)}} )

PellikaanJaworski = (kk,m) -> (
    R = kk[vars(0..m-1)];
    PJ(R,m))

SIN#1 = (kk) -> (
    R = kk[symbol t, symbol x, symbol y, symbol z, symbol w];
    ideal(5*t^3*x^2*z*w^4 + 2*t^2*y^3*x^5, 
            7*y*w^2 + 4*x^2*y + y^2*x + 2*z*t*w, 
            3*t*z*w^3+ 3*y*z^2*w^2 + 2*y*z^4))

SIN#2 = (kk) -> (
    R = kk[symbol w, symbol t, symbol x, symbol y, symbol z];
    ideal(5*t^3*x^2*z*w^4 + 2*t^2*y^3*x^5, 
            7*y*w^2 + 4*x^2*y + y^2*x + 2*z*t*w, 
            3*t*z*w^3+ 3*y*z^2*w^2 + 2*y*z^4))

SIN#3 = (kk) -> (
    R = kk[symbol w, symbol x, symbol y, symbol z];
    f := singF(11,10,3,1);
    m := flatten diff(transpose matrix{{x,y,z}}, matrix{{f}});
    ideal m)
    
SIN#4 = (kk) -> (
    R = kk[symbol w, symbol x, symbol y, symbol z];
    f := singH 7;
    m := flatten diff(transpose matrix{{x,y,z}}, matrix{{f}});
    ideal m)

SIN#5 = (kk) -> (
    R = kk[symbol t, symbol w, symbol x, symbol y, symbol z];
    ideal(x^2 * t^18 - z^10 * t^10 - z^20,
            x * y^3 * t^26 - z^10 * t^20 - z^30,
            y^6 * t^38 - x * y^3 * w^40))

SIN#6 = (kk) -> (
    -- what is this supposed to be??
    null)

SIN#7 = (kk) -> (
    R = kk[vars(0..5)];
    m := matrix{{b * (a^8 + c^2 * d^2 * e^2 * f^2),
                 c * (a^8 + b^2 * d^2 * e^2 * f^2),
                 d * (a^8 + b^2 * c^2 * e^2 * f^2),
                 e * (a^8 + b^2 * c^2 * d^2 * f^2),
                 f * (a^8 + b^2 * c^2 * d^2 * e^2)}};
    ideal symmetricPower(2,m))

SIN#8 = (kk) -> (
    R = kk[symbol t, symbol x, symbol y, symbol z, symbol w];
    ideal(t^18 * x^2 - t^19 * z - t^18 * z^2, 
            t^26 * x * y - t^27 * z - t^25 * z^3,
            t^38 * y^2 - t^37 * x * y * w))

SIN#9 = (kk) -> (
    R = kk[vars(0..4)];
    ideal random(R^1, R^{-3,-3,-3,-3,-3}))

SIN#10 = (kk) -> (
    R = kk[vars(0..4),symbol h];
    ideal(a+b+c+d+e,
            a*b + b*c + c*d + d*e + e*a,
            a*b*c + b*c*d + c*d*e + d*e*a + e*a*b,
            a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c,
            a*b*c*d*e - h^5))
    
SIN#11 = (kk) -> (
    R = kk[vars(0..4)];
    ideal(a^4 - b^4, 
          b^4 - c^4, 
          c^4 - d^4, 
          d^4 - e^4, 
          a^3*b + b^3*c + c^3*d + d^3*e + e^3*a))

SIN#12 = (kk) -> (
    R = kk[symbol u, symbol v, symbol w, symbol x, symbol y, symbol z];
    ideal(
	 x*y + y^2 + u*z - w*z - x*z + y*z,
	 x^2 - y^2 + u*z - w*z - x*z + z^2,
	 w*y,
	 w*x - u*z + y*z,
	 w^2 - y^2 + u*z - z^2,
	 v*z - y*z - z^2,
	 v*y - y^2 - w*z + x*z + y*z + z^2,
	 v*x - y^2 + u*z + y*z - z^2,
	 v*w - y^2 + u*z + y*z - z^2,
	 v^2 + y^2 + u*z - x*z - y*z - z^2,
	 u*y + y^2 + y*z,
	 u*x - y^2 + w*z - x*z - z^2,
	 u*w - y^2 + u*z + y*z - z^2,
	 u*v - y^2 - x*z + y*z,
	 u^2 + y*z))

SIN#13 = (kk) -> (
    R = kk[vars(0..4)];
    ideal random(R^1, R^{-5,-5,-5}))

SIN#14 = () -> (
    R = ZZ/31991[vars(0..4)];
    ideal(
	 a*b + 14766*a*c - 12713*b*c + 8997* a*d + 1878*a*e,
	 a*c + 9210* b*c + 9399* a*d + 13903*b*d - 9583*a*e,
	 b*c - 13988*a*d - 4712* b*d + 6771* a*e - 7341*b*e,
	 a*d - 2340 *b*d - 7515* c*d + 1575* a*e - 4211*b*e,
	 b*d + 5023* c*d - 874*  a*e + 4773* b*e + 14016*c*e,
	 c*d + 1617* a*e - 14718*b*e - 1384* c*e + 12060*d*e,
	 a^3 - 10731*b^3 + 5818*a*e^2 + 13936*b*e^2 + 11725*c*e^2 + 6401*d*e^2,
	 b^3 - 4862* c^3 - 2334*a*e^2 + 9991*b*e^2 + 14579*c*e^2 + 10173*d*e^2,
	 c^3 - 6087* d^3 + 1135*a*e^2 + 4570*b*e^2 + 5250*c*e^2 + 1393*d*e^2,
	 d^3 + 11392*a*e^2 + 7125*b*e^2 - 15188*c*e^2 - 12706*d*e^2 - 10957*e^3))

SIN#16 = (kk) -> (
    -- is this correct??
    R = kk[symbol w, symbol x, symbol y, symbol z, symbol t];
    ideal(
	 -2*w^2*x - w^2*z + 2*x*y*t + y^2*z,
	 2*w^4 + 4*w^2*x^2 + 4*w^2*x*z - 10*w^2*y^2 - 10*w^2*y*t - x^3*z + 4*x^2*y*t 
    	   + 4*x*y^2*z + 2*y^3*t,
	 - w^2*x - 2*w^2*z + x*t^2 + 2*y*z*t,
	 2*w^4 + 4*w^2*x*z - 10*w^2*y*t + 4*w^2*z^2 - 10*w^2*t^2 
	   - x*z^3 + 4*y*z^2*t + 2*y*t^3))

SIN#17 = (kk) -> PellikaanJaworski(kk,9)

SIN#18 = (kk) -> PellikaanJaworski(kk,10)

SIN#19 = (kk) -> (
    R = kk[vars(0..3)];
    ideal random(R^1, R^{-2,-3,-4,-5,-6,-7,-8,-9,-10}))

SIN#20 = (kk) -> (
    R = kk[vars(0..3)];
    ideal random(R^1, R^{-5,-6,-7,-8,-9,-10}))


end

restart
loadPackage "ExampleIdeals"
time H = examplesDGP QQ
box H
box(H,4)
example(H,1)

H = examplesDGP
example(H,1,CoefficientRing=>QQ)
S = QQ[z_1..z_10]
example(H,1,Ring=>S)
show H
show(H,1)
box(H,1)
box H
box(H,{1,3})
box(H,{3,1})
show(H,{3,1})
kk = QQ; value H#"chemistry"#1()

print netList sort apply(pairs H, x -> {x#0, x#1#1()})
netList sort apply(pairs H, x -> {x#0, x#1#0})
kk = ZZ/32003; value H#"sy-j"#1()
pairs H

I = katsura(5,QQ)
x = symbol x
R = QQ[x_0..x_4]
I = substitute(I,vars R)
SIN#4 QQ
egPermanents(QQ,3,4,3)
egSYJ QQ
egSYSt QQ
egGerdt QQ
egHorrocks QQ

monomialIdeal leadTerm I
radical oo
codim I
dim I
I1 = substitute(I, u => u + 3*x + 5*y + 7*z)
radical monomialIdeal leadTerm I1
