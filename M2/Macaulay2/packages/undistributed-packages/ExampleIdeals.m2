-- -*- coding: utf-8 -*-
newPackage(
	"ExampleIdeals",
	AuxiliaryFiles => true,
    	Version => "0.1", 
    	Date => "February 8, 2007",
    	Authors => {{Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"}},
    	Headline => "examples of ideals",
     	PackageImports => { "Markov" },
    	DebuggingMode => false
    	)

export {
     "readExampleFile",
     "getExampleFile",
     "ExampleTable",
     "box",
     "example",
     "findExamples",

     "toSingular",
     "singularGB",
     "singularGBString",
     "singularIntegralClosure",
     "runSingularGB",     

     "toMagma",
     "magmaGBString",
     "runMagmaGB",
     "runMagmaIntegralClosure",
     
     "examplesStdSingular",
     "examplesDGP",
     "examplesSIN",
     "examplesBayes",
     "egPermanents",
     "egHaas",
     "katsura",
     "cyclicRoots",
     "cyclicRootsHomogeneous",
     "commuting4by4",
     "commuting4by4grevlex",
     "mayr",
     "PellikaanJaworski",
     "bayes"
     }

needs "./ExampleIdeals/mayr-meyer.m2"

ExampleTable = new Type of HashTable

box = method()
example = method(Options=>{CoefficientRing => ZZ/32003,
	                   Ring => null})

findExamples  = method()
findExamples(ExampleTable, String) := (E,regex) -> (
     K := keys E;
     select(K, k -> match(regex, K#k#0))
     )

show(ExampleTable, ZZ) := (H,x) -> print ("-------------------" || ("-- "|H#x#0) || H#x#1)
show(ExampleTable, List) := (H,x) -> scan(x, x1 -> show(H,x1))
show(ExampleTable, String) := (H,re) -> show(H,findExamples(H,re))
show(ExampleTable) := (H) -> show(H, sort keys H)

box(ExampleTable, ZZ) := (H,x) -> box(H,{x})
box(ExampleTable, List) := (H,x) -> netList apply(x, x1 -> {x1, ("-- "|H#x1#0) || H#x1#1})
box(ExampleTable, String) := (H,re) -> box(H,findExamples(H,re))
box(ExampleTable) := (H) -> box(H, sort keys H)

example(ExampleTable, ZZ) :=
example(ExampleTable, String) := opts -> (H,x) -> (
     R1 := if opts#?Ring then opts#Ring else null;
     kk := if R1 === null 
             then opts.CoefficientRing 
	     else coefficientRing R1; 
     I := value replace("kk", toString kk, H#x#1);
     if R1 =!= null then (
     	  nvars := numgens ring I;
	  if numgens R1 < nvars then 
	    error ("expected ring with at least "|nvars|" variables");
	  substitute(I, (vars R1)_{0..nvars-1})
	  )
     else I)

readExampleFile = method()
-* -- remove this code
readExampleFile(String,Ring) := (filename, coeffring) -> (
     G := separateRegexp("---+", get (ExampleIdeals#"source directory"|filename));
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => replace("kk", toString coeffring, demark("\n",drop(G#i,1)))))
     )
readExampleFile(String) := (filename) -> (
     G := separateRegexp("---+", get (ExampleIdeals#"source directory"|filename));
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => demark("\n",drop(G#i,1))))
     )
*-
getExampleFile = method()

-* -- remove this code
getExampleFile(String,String) := (filename,kkstring) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => replace("kk", kkstring, demark("\n",drop(G#i,1)))))
     )
getExampleFile(String) := (filename) -> getExampleFile(filename,"")
*-

-- New code
getExampleFile(String) := (filename) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     -- now for each example, find its name/number and make the string
     G = select(G, s -> #s > 0);
     new ExampleTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => demark("\n",drop(G#i,1))))
     )
readExampleFile(String) := (filename) -> 
     getExampleFile(ExampleIdeals#"source directory"|"ExampleIdeals/"|filename)
     
replaceStrings = (L, str) -> (scan(L, v -> str = replace(v#0,v#1,str)); str)
substitute(ExampleTable, List) := (E, L) -> (
     -- L is a list of options: str => newstr, (or regex => newstr).
     -- return a new ExampleTable where each final string has the given strings in L
     -- replaced (in order).
     new ExampleTable from apply(pairs E, (k,v) -> (
	       k => (v#0 => replaceStrings(L, v#1))
	       ))
     )
substitute(ExampleTable, Option) := (E,subs) -> substitute(E, {subs})


examplesDGP = method()
examplesDGP Ring := (kk) -> substitute(readExampleFile "DGP.m2", {"kk" => toString kk})

examplesSIN = method()
examplesSIN Ring := (kk) -> substitute(readExampleFile "SIN.m2", {"kk" => toString kk})

examplesStdSingular = method()
examplesStdSingular Ring := (kk) -> substitute(readExampleFile "SIN.m2", {"kk" => toString kk})

examplesBayes = () -> readExampleFile "bayes5.m2"

-- This is a list of examples from several sources

egPermanents = (kk,m,n,size) -> (
     R := kk[vars(0..m*n-1)];
     I := permanents(size,genericMatrix(R,m,n))
     )

egHaas = (kk) -> (
     -- From Hashemi, Efficient algorithms for computing Noether normalization
     (t,x,y,z) := ("t","x","y","z")/getSymbol;
     R := kk[t,x,y,z];
     (t,x,y,z) = (t,x,y,z)/value;
     I := ideal"x8+zy4-y,y8+tx4-x,64x7y7-16x3y3zt+4y3z+4x3t-1"
     )

--katsura = method()
--katsura(ZZ,Ring) := (n,R) -> (
--     )
katsura = (n,kk) -> (
     -- This is written to match the Singular version, which seems to differ
     -- from the POSSO version
     n = n-1;
     R := kk[vars(0..n)];
     L := gens R;
     u := (i) -> (
	  if i < 0 then i = -i;
	  if i <= n then L_i else 0_R);
     f1 := -1 + sum for i from -n to n list u i;
     I := ideal prepend(f1,
	  apply(0..n-1, i -> (
	       - u i + sum(-n..n, j -> (u j) * (u (i-j)))
	       )))
     )

cyclicRoots = (n,kk) -> (
     R := kk[vars(0..n-1)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k -> R_((i+k)%n)))) 
       + ideal(product gens R - 1))

cyclicRootsHomogeneous = (n,kk) -> (
     R := kk[vars(0..n)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k -> R_((i+k)%n)))) 
       + ideal(product(n, i -> R_i) - R_n^n))

commuting4by4 = (kk) -> (
  R := kk[vars(0..31),
         MonomialOrder=>{8, 12, 12}, 
	 MonomialSize=>8];
  I := ideal"
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

commuting4by4grevlex = (kk) -> (
  R := kk[vars(0..31),
	 MonomialSize=>8];
  I := ideal"
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

-- these functions aren't used or exported
-- singF = (a,b,c,t) -> x^a + y^b + z^(3*c) + x^(c+2) * y^(c-1) + 
--    x^(c-1) * y^(c-1) * z^3 + x^(c-2) * y^c * (y^2 + t * x)^2
-- singH = (a) -> x^a + y^a + z^a + x * y * z * (x + y + z)^2 + (x+y+z)^3
-- singG = (a,b,c,d,e,t) -> x^a + y^b + z^c + x^d * y^(e-5) + 
--    x^(d-2) * y^(e-3) + x^(d-3) * y^(e-4) * z^2 + 
--    x^(d-4) * y^(e-4) * (y^2 + t * x)^2

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
    R := kk[vars(0..m-1)];
    PJ(R,m))

bayes = (Glist,d) -> (
     R := markovRing d;
     G := makeGraph Glist;
     S := globalMarkovStmts G;
     J := markovIdeal(R,S);
     F := marginMap(1,R);
     J = F J;
     ideal mingens J
     )

Fabrice24 = (kk) -> (
  (x1, x2, x3, y1, y2, y3, z1, z2, z3) := ("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")/getSymbol;
  R := kk[x1, x2, x3, y1, y2, y3, z1, z2, z3];
  (x1, x2, x3, y1, y2, y3, z1, z2, z3) = (x1, x2, x3, y1, y2, y3, z1, z2, z3)/value;
  ideal(62500*x1^2 + 62500*y1^2 + 62500*z1^2 -74529,
       625*x2^2 + 625*y2^2 + 625*z2^2 -1250*x2 -2624,
       12500*x3^2 + 12500*y3^2 + 12500*z3^2 + 2500*x3 -44975*y3 -10982,
       400000*x1*x2 + 400000*y1*y2 + 400000*z1*z2 -400000*x2 + 178837,
       1000000*x1*x3 + 1000000*y1*y3 + 1000000*z1*z3 + 100000*x3 -1799000*y3 -805427,
       2000000*x2*x3 + 2000000*y2*y3 + 2000000*z2*z3 -2000000*x2 + 200000*x3 -3598000*y3 -1403,
       113800000000000*x3*y2*z1 -113800000000000*x2*y3*z1 -113800000000000*x3*y1*z2 + 113800000000000*x1*y3*z2 + 113800000000000*x2*y1*z3 -113800000000000*x1*y2*z3 -206888400000000*x2*y1 + 206888400000000*x3*y1 + 206888400000000*x1*y2 -206888400000000*x3*y2 -206888400000000*x1*y3 + 206888400000000*x2*y3 -2014260000000*x2*z1 + 2014260000000*x3*z1 -61907200000000*y2*z1 + 61907200000000*y3*z1 + 2014260000000*x1*z2 -2014260000000*x3*z2 + 61907200000000*y1*z2 -61907200000000*y3*z2 -2014260000000*x1*z3 + 2014260000000*x2*z3 -61907200000000*y1*z3 + 61907200000000*y2*z3 -362960716800000*x1 + 38025201600000*x2 + 292548849600000*x3 + 11809567440000*y1 + 1475978220000*y2 -825269402280000*y3 -1212982689600000*z1 -151600474800000*z2 + 825859951200000*z3 -19295432410527,
       -777600000000*x3*y2*z1 + 777600000000*x2*y3*z1 + 777600000000*x3*y1*z2 -777600000000*x1*y3*z2 -777600000000*x2*y1*z3 + 777600000000*x1*y2*z3 -1409011200000*x2*y1 + 1409011200000*x3*y1 + 1409011200000*x1*y2 -1409011200000*x3*y2 -1409011200000*x1*y3 + 1409011200000*x2*y3 -1065312000000*x2*z1 + 1065312000000*x3*z1 -805593600000*y2*z1 + 805593600000*y3*z1 + 1065312000000*x1*z2 -1065312000000*x3*z2 + 805593600000*y1*z2 -805593600000*y3*z2 -1065312000000*x1*z3 + 1065312000000*x2*z3 -805593600000*y1*z3 + 805593600000*y2*z3 + 235685027200*x1 + 398417510400*x2 + 158626915200*x3 -311668424000*y1 -268090368000*y2 + 72704002800*y3 + 412221302400*z1 + 354583756800*z2 + 307085438400*z3 + 282499646407,
       3200*x2 + 1271)
  )

Lichtblau = (kk) -> (
     (t,x,y) := (t,x,y)/getSymbol;
     R := kk[t,x,y,MonomialOrder=>Eliminate 1];
     (t,x,y) = (t,x,y)/value;
     ideal"x-110t2+495t3-1320t4+2772t5-5082t6+7590t7-8085t8+5555t9-2189t10+374t11,
           y-22t+110t2-330t3+1848t5-3696t6+3300t7-1650t8+550t9-88t10-22t11"
     )

-- Examples from Grassmann et al, 2002, pg. 6.

examplesSingular1 = new MutableHashTable;
examplesSingular1#1 = (R) -> (
     if R === null then R = QQ{"t","x","y","z"};
     use R;
     ideal"5t3x2z+2t2y3x5,7y+4x2y+y2x+2zt,3tz+3yz2+2yz4"
     )

toClassic = method()
toClassic Ideal := (I) -> (
     g := concatenate between(",\n   ", apply(numgens I, i -> replace(///[\*\^]///,"",toString I_i)));
     "ideal\"" | g | "\""
     )

-----------------------------------------------
-- Translation to Singular --------------------
-----------------------------------------------
-- ring variable name translation
--   ring variable with no subscript, and no 's:  same
--   ring variable with complicated subscripts need to be mapped
--   I think that even doubly indexed variables can't be done?
--   
toSingular = method()
toSingular Ideal := (I) -> (
     g := concatenate between(",\n   ", apply(numgens I, i -> replace(///[\*\^]///,"",toString I_i)));
     "ideal i = " | g | ";\n"
     )
toSingular Ring := (R) -> (
     -- note: R is assumed to be a polynomial ring.  Variables alowed: single letters, 
     -- letters indexed by a single non-negative integer.
     kk := coefficientRing R;
     p := char kk;
     a := "ring R1 = "|p|",(";
     b := concatenate between(",", (gens R)/toString);
     c := "),dp;\n";
     a | b | c
     )
toSingular Ideal := (I) -> (
     a := "ideal I1 = \n";
     g := concatenate between(",\n   ", apply(numgens I, i -> toString I_i));
     a | g | ";\n"
     )
toSingular (Ideal, String) := (I,str) -> (
     a := "ideal " | str | " = \n";
     g := concatenate between(",\n   ", apply(numgens I, i -> toString I_i));
     a | g | ";\n"
     )

timerWrap = (str) -> (
     "rtimer = 1;\nint ti=rtimer;\n"
     |str|
     ///int ti2=rtimer-ti;
print("time used"); print(ti2);
///
     )
-- computing a GB in Singular
-- string with name of ideal @I@
singGBstring = 
  ///ideal J1=groebner(@I@);
print(size(J1));
///

-- computing an integral closure of a ring in Singular
-- this is the integral closure of R/I...!
-- string with name of ideal: @I@
singICstring = "list nor=normal(@I@);\n"

singularGB = method()
singularGB String := (idealName) -> 
     timerWrap replace("@I@",idealName,singGBstring)
          
singularIntegralClosure = method()
singularIntegralClosure String := (idealName) -> 
     timerWrap replace("@I@",idealName,singICstring)

runSingularGB = method()
runSingularGB Ideal := (I) -> (
     "foo"
     << toSingular ring I 
     << toSingular I
     << singularGB "I"
     << "exit(0);\n" << close;
     run "/sw/bin/Singular <foo"
     )
runSingularGB Ideal := (I) -> (
     str := toSingular ring I | toSingular I | singularGB "I1" | "\nexit(0);\n";
     runSingular str
     )

--------------------------
singGBtemplate = 
///proc nummonoms (ideal L1)
{
  int i;
  int result=0;
  for (i=1; i<=size(L1); i++) {
    result = result + size(L1[i]);
  }
  return(result);
}

$ring
$ideal
timer = 1;
int ti=timer;
ideal J1=groebner(I1);
int ti2=timer-ti;
printf("time=%s sec, #gb=%s #monoms=%s", ti2, size(J1), nummonoms(J1));
exit;
///
--------------------------

singularGBString = method()
singularGBString Ideal := (I) -> (
     replace("\\$ring", toSingular ring I,
	  replace("\\$ideal", toSingular I, singGBtemplate))
     )

runSingular = method()
runSingular String := (str) -> (
     "foo" << str << endl << close;
     get "!/sw/bin/Singular <foo"
     )

-----------------------------------------------
-- Translation to Magma -----------------------
-----------------------------------------------

toMagma = method()
toMagma Ring := (R) -> (
     -- note: R is assumed to be a polynomial ring.  Variables alowed: single letters, 
     -- letters indexed by a single non-negative integer.
     -- For now the base ring needs to be ZZ/p or QQ.
     kk := coefficientRing R;
     p := char kk;
     basering := if p === 0 then "RationalField()" else "GF("|p|")";
     "R1<" | concatenate between(",", (gens R)/toString) | "> := PolynomialRing(" 
       | basering | "," | toString numgens R | ",\"grevlex\");"
     )
toMagma Ideal := (I) -> (
     a := "I1 := ideal< R1 | \n   ";
     g := concatenate between(",\n   ", apply(numgens I, i -> toString I_i));
     a | g | ">;\n" 
     )

magmaICstring = "time Js := Normalization(@I@);\n"

magmaIntegralClosure = method()
magmaIntegralClosure String := (idealName) -> 
     replace("@I@",idealName,magmaICstring)

runMagmaGB = method()
runMagmaGB Ideal := (I) -> (
     "foo" 
     << toMagma ring I << endl
     << toMagma I
     << "time J1 := GroebnerBasis(I1);\n"
     << "#J1;\n"
     << "quit;\n" << close;
     run "magma <foo"
     )

runMagmaIntegralClosure = method()
runMagmaIntegralClosure Ideal := (I) -> (
     "foo" 
     << toMagma ring I << endl
     << toMagma I
     << "time Js := Normalization(I1);\n"
     << "#Js;\n"
     << "quit;\n" << close;
     run "magma <foo"
     )

magmaGBtemplate = 
///
$ring
$ideal
time J1 := GroebnerBasis(I1);
#J1;
quit;
///
--------------------------

magmaGBString = method()
magmaGBString Ideal := (I) -> (
     replace("\\$ring", toMagma ring I,
	  replace("\\$ideal", toMagma I, magmaGBtemplate))
     )

beginDocumentation()

doc ///
  Key
    ExampleIdeals
  Headline
    a package containing examples for basic computations
  Description
   Text
      This package allows one to collect examples into a single file, 
      and conveniently run these examples, or computations based on these examples.

      For example, an @TO ExampleTable@ can be read into Macaulay2, and/or computations
      based on them can be executed.
   -- comment out non-working example:
   -- Example
   --    E = getM2ExampleFile("bayes", CoefficientRing => ZZ/32003);
   --    keys E
   --    E
///

doc ///
  Key
    ExampleTable
  Headline
    hash table containing example ideals, computations, or other M2 constructions
  Description
   Text
      The format of an {\tt ExampleTable}, E: the keys should be integers, starting at 1, 
      and the value E#i is a pair (strname => str), where strname is a very short description
      of the example, and {\tt str} is M2 code which generates the example.
   Example
     E = new ExampleTable from {
	  1 => "eg1" => "R = QQ[a..d];\nI = ideal(a^2,b*c,c*d,b^3-c^2*d)"
	  }
   Example
     E#1#1
     example(E,1)   
     box(E)
   Text
      Normally, one does not create such a table directly, one reads one from a file, using
      @TO getExampleFile@ or @TO readExampleFile@.
  SeeAlso
     getExampleFile
     readExampleFile
     example
     box
///

doc ///
  Key
    readExampleFile
  Headline
    read an example file which is packaged with ExampleIdeals
  Usage
    readExampleFile(filename)
  Inputs
    filename:String
  Outputs
    :ExampleTable
  Description
   Text
     read in a predefined example file.  Possible names include:
     DGP, SIN, bayes5, gb-examples, local, pd-run-examples, stdSingular, and symbolicdata.
     (DGP stands for Decker-Greuel-Pfister, SIN for Singular, and symbolicdata 
     for some of the examples at
     symbolicdata.org, which doesn't appear to exist anymore)
   Example
     H = readExampleFile "bayes5.m2";
     keys H
     H#200#0
     H#200#1
     value H#200#1
     apply(10..20, i -> codim value H#i#1)
  SeeAlso
     getExampleFile
///

end

restart

------------------------------------
-- examples for the singular code --
------------------------------------
restart
loadPackage "ExampleIdeals"
R = QQ[a..d,h,r,t]
assert(toSingular R == "ring R = 0,(a,b,c,d,h,r,t),dp;\n")
R = QQ[a..d,h,rt]
assert(toSingular R == "ring R = 0,(a,b,c,d,h,rt),dp;\n")
R = QQ[x_1..x_4]
assert(toSingular R == "ring R = 0,(x(1)-x(4)),dp;\n")

singularGB
loadPackage "ExampleIdeals"
debug ExampleIdeals
R = ZZ/101[a..d]
I = ideal random(R^1, R^{-2,-2})

s = concatenate between("\n",{toSingular ring I,
     toSingular I,
     replace(singularGB, "@I@", "I")})

"!/sw/bin/Singular" << s << close



print toSingular R
print toSingular I
print singularIntegralClosure "I"

runMagmaGB I
runSingularGB I
print toMagma R; print toMagma I

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

--
loadPackage "ExampleIdeals"
I = cyclicRootsHomogeneous(7,ZZ/23)
gbTrace=3
time gens gb I;
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1,1,1,1});

loadPackage "ExampleIdeals"
I = cyclicRootsHomogeneous(8,ZZ/23)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1,1,1,1,1});

loadPackage "ExampleIdeals"
I = cyclicRootsHomogeneous(9,ZZ/23)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1,1,1,1,1,1});

restart
cyclicRootsHomogeneous = (n,kk) -> (
     R = kk[vars(0..n)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k ->
R_((i+k)%n))))
       + ideal(product(n, i -> R_i) - R_n^n))
I = cyclicRootsHomogeneous(7,ZZ/23)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));

I = Fabrice24(ZZ/101)
gbTrace=3
gens gb I
codim I
numgens R
numgens I
eliminate({x1, x2, x3, y1, y2, y3, z1},I)
factor(o12_4)
factor oo
degree I

I = Lichtblau(QQ)
eliminate({t},I)

p = symbol p;
R = ZZ/32003[reverse(p_(1,1,1,1,1)..p_(2,2,2,2,2)), MonomialSize=>8];
J = ideal(
	      -p_(1,1,2,1,1)*p_(2,1,1,1,1)+p_(1,1,1,1,1)*p_(2,1,2,1,1),
	      -p_(1,1,2,1,2)*p_(2,1,1,1,2)+p_(1,1,1,1,2)*p_(2,1,2,1,2),
	      -p_(1,1,2,2,1)*p_(2,1,1,2,1)+p_(1,1,1,2,1)*p_(2,1,2,2,1),
	      -p_(1,1,2,2,2)*p_(2,1,1,2,2)+p_(1,1,1,2,2)*p_(2,1,2,2,2),
	      -p_(1,2,2,1,1)*p_(2,2,1,1,1)+p_(1,2,1,1,1)*p_(2,2,2,1,1),
	      -p_(1,2,2,1,2)*p_(2,2,1,1,2)+p_(1,2,1,1,2)*p_(2,2,2,1,2),
	      -p_(1,2,2,2,1)*p_(2,2,1,2,1)+p_(1,2,1,2,1)*p_(2,2,2,2,1),
	      -p_(1,2,2,2,2)*p_(2,2,1,2,2)+p_(1,2,1,2,2)*p_(2,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
	      -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,1,2),
	      -p_(1,1,1,2,2)*p_(1,2,1,2,1)+p_(1,1,1,2,1)*p_(1,2,1,2,2),
	      -p_(1,1,2,1,2)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,1,2),
	      -p_(1,1,2,2,2)*p_(1,2,2,2,1)+p_(1,1,2,2,1)*p_(1,2,2,2,2),
	      -p_(1,1,1,2,1)*p_(1,2,1,1,1)+p_(1,1,1,1,1)*p_(1,2,1,2,1),
	      -p_(1,1,1,2,2)*p_(1,2,1,1,2)+p_(1,1,1,1,2)*p_(1,2,1,2,2),
	      -p_(1,1,2,2,1)*p_(1,2,2,1,1)+p_(1,1,2,1,1)*p_(1,2,2,2,1),
	      -p_(1,1,2,2,2)*p_(1,2,2,1,2)+p_(1,1,2,1,2)*p_(1,2,2,2,2),
	      -p_(1,1,1,1,2)*p_(1,1,1,2,1)+p_(1,1,1,1,1)*p_(1,1,1,2,2)
		+p_(1,1,1,2,2)*p_(1,1,2,1,1)-p_(1,1,1,2,1)*p_(1,1,2,1,2)
		-p_(1,1,1,1,2)*p_(1,1,2,2,1)-p_(1,1,2,1,2)*p_(1,1,2,2,1)
		+p_(1,1,1,1,1)*p_(1,1,2,2,2)+p_(1,1,2,1,1)*p_(1,1,2,2,2)
		+p_(1,1,1,2,2)*p_(1,2,1,1,1)+p_(1,1,2,2,2)*p_(1,2,1,1,1)
		-p_(1,1,1,2,1)*p_(1,2,1,1,2)-p_(1,1,2,2,1)*p_(1,2,1,1,2)
		-p_(1,1,1,1,2)*p_(1,2,1,2,1)-p_(1,1,2,1,2)*p_(1,2,1,2,1)
		-p_(1,2,1,1,2)*p_(1,2,1,2,1)+p_(1,1,1,1,1)*p_(1,2,1,2,2)
		+p_(1,1,2,1,1)*p_(1,2,1,2,2)+p_(1,2,1,1,1)*p_(1,2,1,2,2)
		+p_(1,1,1,2,2)*p_(1,2,2,1,1)+p_(1,1,2,2,2)*p_(1,2,2,1,1)
		+p_(1,2,1,2,2)*p_(1,2,2,1,1)-p_(1,1,1,2,1)*p_(1,2,2,1,2)
		-p_(1,1,2,2,1)*p_(1,2,2,1,2)-p_(1,2,1,2,1)*p_(1,2,2,1,2)
		-p_(1,1,1,1,2)*p_(1,2,2,2,1)-p_(1,1,2,1,2)*p_(1,2,2,2,1)
		-p_(1,2,1,1,2)*p_(1,2,2,2,1)-p_(1,2,2,1,2)*p_(1,2,2,2,1)
		+p_(1,1,1,1,1)*p_(1,2,2,2,2)+p_(1,1,2,1,1)*p_(1,2,2,2,2)
		+p_(1,2,1,1,1)*p_(1,2,2,2,2)+p_(1,2,2,1,1)*p_(1,2,2,2,2));
R = (coefficientRing R)[x_1..x_(numgens R), MonomialSize=>8]
I = sub(J,vars R)
time gens gb(I, MaxReductionCount=>3000);
print toMagma R; print toMagma I
runMagmaGB I
runSingularGB I
-* -- magma
R<x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11,x_12,x_13,x_14,x_15,x_16,x_17,x_18,x_19,x_20,x_21,x_22,x_23,x_24,x_25,x_26,x_27,x_28,x_29,x_30,x_31,x_32> := PolynomialRing(GF(32003),32,"grevlex");
I := ideal< R | 
   -x_16*x_28+x_12*x_32,
   -x_15*x_27+x_11*x_31,
   -x_14*x_26+x_10*x_30,
   -x_13*x_25+x_9*x_29,
   -x_8*x_20+x_4*x_24,
   -x_7*x_19+x_3*x_23,
   -x_6*x_18+x_2*x_22,
   -x_5*x_17+x_1*x_21,
   -x_24*x_31+x_23*x_32,
   -x_24*x_30+x_22*x_32,
   -x_23*x_30+x_22*x_31,
   -x_24*x_29+x_21*x_32,
   -x_23*x_29+x_21*x_31,
   -x_22*x_29+x_21*x_30,
   -x_20*x_27+x_19*x_28,
   -x_20*x_26+x_18*x_28,
   -x_19*x_26+x_18*x_27,
   -x_20*x_25+x_17*x_28,
   -x_19*x_25+x_17*x_27,
   -x_18*x_25+x_17*x_26,
   -x_24*x_31+x_23*x_32,
   -x_22*x_29+x_21*x_30,
   -x_20*x_27+x_19*x_28,
   -x_18*x_25+x_17*x_26,
   -x_24*x_30+x_22*x_32,
   -x_23*x_29+x_21*x_31,
   -x_20*x_26+x_18*x_28,
   -x_19*x_25+x_17*x_27,
   -x_18*x_19+x_17*x_20+x_20*x_21-x_19*x_22-x_18*x_23-x_22*x_23+x_17*x_24+x_21*x_24+x_20*x_25+x_24*x_25-x_19*x_26-x_23*x_26-x_18*x_27-x_22*x_27-x_26*x_27+x_17*x_28+x_21*x_28+x_25*x_28+x_20*x_29+x_24*x_29+x_28*x_29-x_19*x_30-x_23*x_30-x_27*x_30-x_18*x_31-x_22*x_31-x_26*x_31-x_30*x_31+x_17*x_32+x_21*x_32+x_25*x_32+x_29*x_32>;
time J := GroebnerBasis(I);
*-
