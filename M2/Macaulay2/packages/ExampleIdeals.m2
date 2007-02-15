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

needsPackage "Markov"

export {
     readExampleFile,
     ExampleTable,
     box,
     example,

     examplesDGP,
     examplesSIN,
     examplesBayes,
     egPermanents,
     egHaas,
     katsura,
     cyclicRoots,
     cyclicRootsHomogeneous,
     commuting4by4,
     PellikaanJaworski,
     bayes
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

examplesSIN = method()
examplesSIN Ring := (kk) -> readExampleFile("ExampleIdeals/SIN.m2", kk)

examplesBayes = () -> (
     oldlimit := recursionLimit;
     recursionLimit = 304; 
     H := readExampleFile("ExampleIdeals/bayes5.m2", null);
     recursionLimit = oldlimit;
     H)

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

singF = (a,b,c,t) -> x^a + y^b + z^(3*c) + x^(c+2) * y^(c-1) + 
   x^(c-1) * y^(c-1) * z^3 + x^(c-2) * y^c * (y^2 + t * x)^2
singH = (a) -> x^a + y^a + z^a + x * y * z * (x + y + z)^2 + (x+y+z)^3
singG = (a,b,c,d,e,t) -> x^a + y^b + z^c + x^d * y^(e-5) + 
   x^(d-2) * y^(e-3) + x^(d-3) * y^(e-4) * z^2 + 
   x^(d-4) * y^(e-4) * (y^2 + t * x)^2

PJ = (R,m) -> (
    PJf := (a,b) -> R_(a-1) * R_(b-1) - R_(a-2) * R_(b+1);
    if m < 4 then trim ideal(0_R)
    else if m == 4 then 
	ideal(R_3 * R_0 - R_1^2, R_3 * R_1 - R_2^2)
    else if m == 5 then 
	PJ(R,4) + ideal(PJf(5,1), PJf(5,2), R_4 * R_2 - R_3^2 + R_1 * R_0)
    else if m == 6 then
	PJ(R,5) + ideal(PJf(6,1), PJf(6,2), PJf(6,3), R_5 * R_3 - R_4^2 - R_1^2)
    else if m%2 == 1 then
        PJ(R,m-1) + ideal apply(m-3, i -> PJf(m,i+1)) + ideal(R_(m-1) * R_(m-3) - R_(m-2)^2 - R_(m-4) * R_(m-6))
    else if m%2 == 0 then
        PJ(R,m-1) + ideal apply(m-3, i -> PJf(m,i+1)) + ideal(R_(m-1) * R_(m-3) - R_(m-2)^2 - 
                                                                R_(m-2)^2 - R_(m-3) * R_(m-5)))

PellikaanJaworski = (kk,m) -> (
    R = kk[vars(0..m-1)];
    PJ(R,m))

bayes = (Glist,d) -> (
     R = markovRing d;
     G = makeGraph Glist;
     S = globalMarkovStmts G;
     J := markovIdeal(R,S);
     F := marginMap(1,R);
     J = F J;
     ideal mingens J
     )
end

restart
loadPackage "ExampleIdeals"

time H = examplesDGP QQ
time H = examplesSIN QQ

time H = examplesBayes();
time scan(keys H, x -> example(H,x))

I = example(H,138);
codim I
degree I
betti gens gb I

gbTrace=1
scan(keys H, x -> (
	  time G := gb example(H,x); 
	  M := monomialIdeal leadTerm G;
	  done := if M == radical M then "yes" else "no";
	  << "doing " << x << " radical? " << done << endl;
	  ))

time possibleNonradicals = select(keys H, x -> (
	  time G := gb example(H,x); 
	  M := monomialIdeal leadTerm G;
	  M != radical M))

possibleNonradicals = {10, 20, 23, 24, 25, 27, 37, 55, 58, 
     67, 71, 73, 74, 75, 76, 
     78, 81, 103, 118, 121, 122, 124, 125, 127, 134, 138, 139, 
     140, 145, 148, 150, 152, 154, 155, 157, 158, 159, 169, 
     202, 204, 206, 208, 209, 212, 213, 218, 228, 231, 
     233, 236, 239}

I = example(H,212)
Isat = saturate(I,p_(1,1,1,1,1))
Isat = trim Isat;
Irest = I : Isat;
intersect(Isat,Irest) == I -- true
m = first independentSets(Irest, Limit=>1)

debug PrimaryDecomposition
flatt(Irest,m)
flattener
example(H,71)
H#10
inI = monomialIdeal I;
inI == radical inI

numgens oo
time C = decompose I;
G
box H
box(H,4)
example(H,15)


-- I used the code here to create the file bayes5.m2
scan(#D5s, i -> (
	  print "------------------------------------------------"; 
	  print("--binary bayes "|i|" "|toString D5s#i);
	  print("bayes("|toString D5s#i|",(2,2,2,2,2))")))


apply(keys H, a -> example(H,a))
oo/class
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
