needsPackage "ExampleIdeals"
needsPackage "IntegralClosure"
needsPackage "ReesAlgebra"
readExampleFile1 = (filename) -> (
     G := separateRegexp("---+", get filename);
     G = apply(G, s -> select(lines s, t -> #t > 0));
     G = select(G, s -> #s > 0);
     new HashTable from apply(#G, i -> (
	       s := substring(2,G#i#0); -- remove the first two -- characters
	       i+1 => s => demark("\n",drop(G#i,1))))
     )
runExamples = method(Options=>options integralClosure)
runExamples (HashTable,ZZ) := o -> (H,i) -> (
     I := value H#i#1;
     A = (ring I)/I;
     tim := timing (A' = integralClosure(A, o));
     answer := {i, H#i#0, char ring I, numgens ring I, numgens I, tim#0};
     print answer;
     answer
     )
runExamples (HashTable,List) := o -> (H,L) -> apply(L,a -> runExamples(H,a,o))
runExamples HashTable := o -> (H) -> runExamples(H, sort keys H, o)

debug IntegralClosure
runCanonicalIdealExamples = method(Options=>options integralClosure)
runCanonicalIdealExamples (HashTable,ZZ) := o -> (H,i) -> (
     I := value H#i#1;
     A = (ring I)/I;
     tim := timing (A' = integralClosure(A, o));
     << "answer: " << see trim ideal gens gb ideal A' << endl;
     tim2 := timing (J = canonicalIdeal A');
     << "canonical ideal : " << see J << endl;
     answer := {i, H#i#0, char ring I, numgens ring I, numgens I, tim#0, tim2#0};
     print answer;
     answer
     )
runCanonicalIdealExamples (HashTable,List) := o -> (H,L) -> apply(L,a -> runCanonicalIdealExamples(H,a,o))
runCanonicalIdealExamples HashTable := o -> (H) -> runCanonicalIdealExamples(H, sort keys H, o)

runS2Examples = method(Options=>options integralClosure)
runS2Examples (HashTable,ZZ) := o -> (H,i) -> (
     I := value H#i#1;
     A = (ring I)/I;
     if numgens I > 1 and char A == 0 or char A > 7 then (
       tim := timing ((F,G) = S2ification(A));
       A' := target F;
       if A' =!= A then print "NOT S2";
       answer := {i, H#i#0, char ring I, numgens ring I, numgens I, numgens A', numgens ideal A', tim#0};
       )
     else answer = {i, H#i#0, char ring I};
     print answer;
     answer
     )
runS2Examples (HashTable,List) := o -> (H,L) -> apply(L,a -> runS2Examples(H,a,o))
runS2Examples HashTable := o -> (H) -> runS2Examples(H, sort keys H, o)

viewResults = (L) -> print netList(L, Boxes=>false, HorizontalSpace=>2)


runSingularIC = method()
runSingularIC Ideal := (I) -> (
     "foo.sing"
     << "rtimer=1;\n"
     << "LIB \"normal.lib\";\n"
     << toSingular ring I << toSingular I
     << "int ti=rtimer;\n"
     << "list nor=normal(I);\n"
     << "int ti2=rtimer-ti;\n"
     << "print(\"time used\"); print(ti2);\n"
     << "exit(ti2);\n" << close;
     run "/sw/bin/singular <foo.sing"
     )

runSingIC = method()
runSingIC (HashTable,ZZ) := (H,i) -> (
     I := value H#i#1;
     runSingularIC I;
     answer := {i, H#i#0};
     )
runSingIC (HashTable,List) := (H,L) -> apply(L,a -> runSingIC(H,a))
runSingIC HashTable := (H) -> runSingIC(H, sort keys H)

charPairs = method()
charPairs(ZZ,List,Ring) := (a,bs,kk) -> (
     A := kk[symbol t];
     B := kk[x,y];
     F := map(A,B,{A_0^a, sum(bs,bi -> A_0^bi)});
     coimage F
     )

H = readExampleFile1 "examples.m2"
print netList(apply(keys H, h -> {h, H#h#0}), Boxes=>false, HorizontalSpace=>2)

planecurves = select(keys H, i -> (I := value H#i#1; numgens I == 1 and numgens ring I == 2))
spacesurfaces = select(keys H, i -> (I := value H#i#1; numgens I == 1 and numgens ring I == 3))
others = sort toList(set keys H - set planecurves - set spacesurfaces)

level1 = {1,2,6,7,8,15,16,24,27,
     29,30,31,32,33,34,35,36,37,38,
     41,44,45,48,51} -- these are examples that take roughly < 1 sec 
level2 = {3,4,11,17,28,39,
     43,46,49,50,53} -- these are examples that take roughly 1-10 sec
level3 = {9,25,26,54,55,65,80} -- take 10-60 sec
level4 = {10} -- take 60-600 sec
level5 = {} -- finish, but take > 600 sec
levelbig = {5,12,13,14,18,
     20,21,40,42,61,62} -- these have not successfully completed, or just have not been done yet
levelbuggy = {19,22,23,47,52,79} -- ones that crash (currently -- these will be 
  --moved out to one of the ones above when they start working)
leveltofile = {56, 57, 58, 59, 60, 63, 64, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 81, 82, 83, 84, 85}
end
restart
load "runexamples.m2"
viewResults runExamples(H, {3}, Verbosity=>2, Strategy=>{SimplifyFractions})
viewResults runExamples(H, {10}, Verbosity=>2)
viewResults runExamples(H, {10}, Verbosity=>2, Strategy=>{SimplifyFractions})

viewResults runExamples(H, level1, Verbosity=>2, Strategy=>{SimplifyFractions})
viewResults runExamples(H, level2, Verbosity=>0, Strategy=>{SimplifyFractions, ExtraMinors1})
viewResults runExamples(H, level3)
viewResults runExamples(H, level4)
viewResults runExamples(H, level1, Verbosity=>1)
runExamples(H, leveltofile)

runExamples(H, 10, Verbosity => 3, Strategy =>{SimplifyFractions, ExtraMinors1})
runExamples(H, 10, Verbosity => 6, Strategy =>{ReallySimplifyFractions})
runExamples(H, 10, Verbosity => 3, Strategy =>{SimplifyFractionsInBase})
runExamples(H, 10, Verbosity => 3, Strategy =>{SimplifyFractions, Jacobian1})
runExamples(H, 57, Verbosity => 3, Strategy =>{SimplifyFractions})
runExamples(H, 60, Verbosity => 3, Strategy =>{SimplifyFractions})
runExamples(H, 60, Verbosity => 0, Strategy =>{SimplifyFractionsInBase})
runExamples(H, 60, Verbosity => 0)
=======
runS2Examples H

>>>>>>> .r8646
-- level1 examples, r8637, 4/28/09, MBP 10.5.6
1   leonard1            0      2  1  .697777
2   vanHoeij1           0      2  1  .323094
6   boehm1              0      3  1  .841622
7   boehm2              0      2  1  .238015
8   boehm3              0      3  1  .538263
15  boehm10             0      3  1  .218437
16  boehm11             0      3  1  .978733
24  boehm19             0      3  1  .391508
27  singular-huneke     31991  5  8  .18715 
29  singular-theo1      32003  3  1  .075406
30  singular-theo1a     32003  4  1  .004476
31  singular-theo2      32003  3  1  .194685
32  singular-theo2a     32003  4  4  .105829
33  singular-theo3      32003  3  1  .265903
34  singular-theo5      32003  3  1  .180317
35  singular-theo6      32003  3  1  .146639
36  singular-sakai1     0      2  1  .160144
37  singular-sakai2     0      2  1  .22397 
38  singular-sakai3     0      2  1  .005533
41  singular-sakai6     0      2  1  .372277
44  singular-unnamed1   0      2  1  .189582
45  singular-unnamed2   0      4  2  .007836
48  singular-unnamed5   32003  5  3  .010352
51  2charPairsJacIdeal  32003  4  1  .609814

-- level2 examples, r8637
11  boehm6                0      3  1  7.68076
17  boehm12               32003  3  1  2.35116
28  singular-vasconcelos  32003  5  4  2.14574
39  singular-sakai4       0      2  1  2.2138 
43  singular-koelman2     0      2  1  2.33951
46  singular-unnamed3     0      3  1  1.77848
49  magma-curve           0      2  1  1.71214
50  2charPairs            32003  2  1  1.6427 
53  ReesAlgOf 3 powers    32003  6  3  1.32399

-- level3 examples, r8637
9   boehm4       32003  3  1  46.6114
26  huneke2      32003  6  3  26.0287
54  rees1-32003  32003  6  3  41.2593
55  rees2-32003  32003  6  3  25.7961

-- level4 examples, r8637
10  boehm5  32003  2  1  66.2583

-- leveltofile
56  rees3-32003     32003  6  3  1.74736
57  GLS1-char0      0      2  1  1.85116
58  GLS1-char2      2      2  1  1.17564
59  GLS1-char5      5      2  1  1.46076
60  GLS1-char11     11     2  1  5.02699
63  GLS2-char3      2      2  1  .016203
64  GLS2-char13     11     2  1  .015343
66  GLS3-char0      0      2  1  2.29969
67  GLS3-char2      2      2  1  8.73575
68  GLS3-char5      5      2  1  1.43884
69  GLS3-char11     11     2  1  4.7698 
70  GLS3-char32003  32003  2  1  1.43519
71  GLS4-char0      0      2  1  .22013 
72  GLS4-char5      5      2  1  .201446
73  GLS4-char11     11     2  1  .233176
74  GLS4-char32003  32003  2  1  .298445
75  GLS5-char0      0      2  1  .21652 
76  GLS5-char5      5      2  1  2.26651
77  GLS5-char11     11     2  1  .1877  
78  GLS5-char32003  32003  2  1  .144104
79  GLS6-char0      0      5  5  .057449
80  GLS6-char2      2      5  5  32.2038
81  GLS7-char0      0      5  4  2.40954
82  GLS7-char2      2      5  4  2.07434
83  GLS7-char5      5      5  4  2.14836
84  GLS7-char11     11     5  4  2.02895
85  GLS7-char32003  32003  5  4  2.11497

--------------------------------------------------

-- where we stand at r8620, running on MBP 10.5.6:
print netList runExamples(H, {40,42})

viewResults time runExamples(H, level1)
viewResults time runExamples(H, level1, Strategy=>{RadicalCodim1})

viewResults time runExamples(H, level2, Verbosity=>1)
viewResults time runExamples(H, level2, Verbosity=>0, Strategy=>{RadicalCodim1})

print netList time runExamples(H, level2, Verbosity=>1)
print netList time runExamples(H, level3, Verbosity => 1)
print netList time runExamples(H, level3, Verbosity => 1, Strategy=>{RadicalCodim1})
print netList time runExamples(H, level4, Verbosity => 1)
print netList time runExamples(H, level4, Verbosity => 1, Strategy=>{RadicalCodim1})

print netList time runExamples(H, leveltofile, Verbosity => 1)
print netList time runExamples(H, leveltofile, Verbosity => 1, Strategy=>{RadicalCodim1})

-- MES
print netList time runExamples(H, levelbig, Verbosity => 1, Strategy=>{RadicalCodim1})

print netList time runExamples(H, {5}, Verbosity => 1) -- not hard
print netList time runExamples(H, {14,18,19,20,21,22,61}, Verbosity => 1, Strategy=>{RadicalCodim1}) -- not hard
print netList time runExamples(H, {47,52,79}, Verbosity => 1, Strategy=>{RadicalCodim1}) -- actual error
print netList time runExamples(H, {5,12,13,23,40,42,62}, Verbosity => 1, Strategy=>{RadicalCodim1}) -- hard?
print netList time runExamples(H, {5,12,13,23,40,42,62}, Verbosity => 1) -- hard?

debug IntegralClosure
print netList time runCanonicalIdealExamples(H, level1, Verbosity => 1)
print netList time runCanonicalIdealExamples(H, drop(level2,1), Verbosity => 1)

+--+------------------+-----+-+-+-------+
|1 |leonard1          |0    |2|1|.784942|
+--+------------------+-----+-+-+-------+
|2 |vanHoeij1         |0    |2|1|.369526|
+--+------------------+-----+-+-+-------+
|6 |boehm1            |0    |3|1|.859257|
+--+------------------+-----+-+-+-------+
|7 |boehm2            |0    |2|1|.246213|
+--+------------------+-----+-+-+-------+
|8 |boehm3            |0    |3|1|.590833|
+--+------------------+-----+-+-+-------+
|15| boehm10          |0    |3|1|.273083|
+--+------------------+-----+-+-+-------+
|16| boehm11          |0    |3|1|1.00148|
+--+------------------+-----+-+-+-------+
|24| boehm19          |0    |3|1|.426483|
+--+------------------+-----+-+-+-------+
|29|singular-theo1    |32003|3|1|.145845|
+--+------------------+-----+-+-+-------+
|30|singular-theo1a   |32003|4|1|.007109|
+--+------------------+-----+-+-+-------+
|31|singular-theo2    |32003|3|1|.168751|
+--+------------------+-----+-+-+-------+
|32|singular-theo2a   |32003|4|4|.184579|
+--+------------------+-----+-+-+-------+
|33|singular-theo3    |32003|3|1|.274891|
+--+------------------+-----+-+-+-------+
|34|singular-theo5    |32003|3|1|.188141|
+--+------------------+-----+-+-+-------+
|35|singular-theo6    |32003|3|1|.094427|
+--+------------------+-----+-+-+-------+
|36|singular-sakai1   |0    |2|1|.222489|
+--+------------------+-----+-+-+-------+
|37|singular-sakai2   |0    |2|1|.238344|
+--+------------------+-----+-+-+-------+
|38|singular-sakai3   |0    |2|1|.004263|
+--+------------------+-----+-+-+-------+
|41|singular-sakai6   |0    |2|1|.376096|
+--+------------------+-----+-+-+-------+
|44|singular-unnamed1 |0    |2|1|.206864|
+--+------------------+-----+-+-+-------+
|45|singular-unnamed2 |0    |4|2|.004838|
+--+------------------+-----+-+-+-------+
|48|singular-unnamed5 |32003|5|3|.009701|
+--+------------------+-----+-+-+-------+
|51|2charPairsJacIdeal|32003|4|1|.586808|
+--+------------------+-----+-+-+-------+

print netList runExamples(H, level2)
+--+-----------------+-----+-+-+-------+
|9 |boehm4           |32003|3|1|46.9344|
+--+-----------------+-----+-+-+-------+
|11| boehm6          |0    |3|1|8.08127|
+--+-----------------+-----+-+-+-------+
|17| boehm12         |32003|3|1|2.54958|
+--+-----------------+-----+-+-+-------+
|27|singular-huneke  |31991|5|8|.204536|
+--+-----------------+-----+-+-+-------+
|39|singular-sakai4  |0    |2|1|2.41175|
+--+-----------------+-----+-+-+-------+
|43|singular-koelman2|0    |2|1|2.37119|
+--+-----------------+-----+-+-+-------+
|46|singular-unnamed3|0    |3|1|1.85067|
+--+-----------------+-----+-+-+-------+
|49|magma-curve      |0    |2|1|1.77476|
+--+-----------------+-----+-+-+-------+
|50|2charPairs       |32003|2|1|1.78311|
+--+-----------------+-----+-+-+-------+

print netList runExamples(H, level4)
+--+--------------------+-----+-+-+-------+
|10|boehm5              |32003|2|1|68.1336|
+--+--------------------+-----+-+-+-------+
|28|singular-vasconcelos|32003|5|4|2.11198|
+--+--------------------+-----+-+-+-------+

print netList runExamples(H, {53,54,55,56})

-----------------------------------------------------------
-- The following is an idea of where we stand on 3/13/2009
runExamples(H,1) -- {1, leonard1, .67977}
runExamples(H,2) -- {2, vanHoeij1, .397611}
runExamples(H,3) -- bigger
runExamples(H,4) -- bigger
runExamples(H,5) -- bigger
runExamples(H,6) -- {6, boehm1, .199943}
runExamples(H,7) -- {7, boehm2, .224295}
runExamples(H,8) -- {8, boehm3, .705478}
runExamples(H,9) -- {9, boehm4, 8.99051}
runExamples(H,10) -- {10, boehm5, 68.0163}
runExamples(H,11) -- {11,  boehm6, 7.62081} -- hugefractions? QQ problem
runExamples(H,12) -- bigger
runExamples(H,13) -- long
runExamples(H,14) -- 
runExamples(H,15) -- {15,  boehm10, .202569} -- fractions are not optimal here
runExamples(H,16) -- {16,  boehm11, .761082}
runExamples(H,17) -- {17,  boehm12, 2.38146}
runExamples(H,18) -- bigger?
runExamples(H,19) -- bug: non unit found
runExamples(H,20) -- bigger?
runExamples(H,21) -- bigger?
runExamples(H,22) -- seqfault
runExamples(H,23) -- segfault
runExamples(H,24) -- {24,  boehm19, .42077}
runExamples(H,25) -- bigger
runExamples(H,26) -- bigger.  This was very large.  Changes 3/14 change it to: 42.57 sec

runExamples(H,27) -- {27, singular-huneke, 2.68845}
runExamples(H,28) -- currently: 378.12 sec uugh
runExamples(H,29) -- {29, singular-theo1, .076252}
runExamples(H,30) -- {30, singular-theo1a, .056603}
runExamples(H,31) -- {31, singular-theo2, .179175}
runExamples(H,32) -- {32, singular-theo2a, .152095}
runExamples(H,33) -- {33, singular-theo3, .256082}
runExamples(H,34) -- {34, singular-theo5, .204141}
runExamples(H,35) -- {35, singular-theo6, .099333}
runExamples(H,36) -- {36, singular-sakai1, .193618}
runExamples(H,37) -- {37, singular-sakai2, .206325}
runExamples(H,38) -- {38, singular-sakai3, .014892}
runExamples(H,39) -- {39, singular-sakai4, 4.09353}
runExamples(H,40) -- bigger?
runExamples(H,41) -- {41, singular-sakai6, .405568}
runExamples(H,42) -- bigger?
runExamples(H,43) -- {43, singular-koelman2, 2.19799} -- big nums again
runExamples(H,44) -- {44, singular-unnamed1, .172449}
runExamples(H,45) -- {45, singular-unnamed2, .031908}
runExamples(H,46) -- {46, singular-unnamed3, 1.44704}
runExamples(H,47) -- bus error!!
runExamples(H,48) -- {48, singular-unnamed5, .125091}

runExamples(H,49) -- {49, magma-curve, 2.01796}
runExamples(H,50) -- {50, 2charPairs, 1.626}
runExamples(H,51) -- {51, 2charPairsJacIdeal, .644675}
runExamples(H,52) -- BUG??

loadPackage "ReesAlgebra"
runExamples(H,56) 
runExamples(H,55)
runExamples(H,54) -- {54, rees1-32003, 48.1293} (3-15-09)
-----------------------------------------------------------
-- Status on 10-8-09, getting ready for 1.3 release
viewResults time runExamples(H, level1)
viewResults time runExamples(H, level1, Strategy=>{RadicalCodim1})

viewResults time runExamples(H, level2, Verbosity=>1)
viewResults time runExamples(H, level2, Verbosity=>0, Strategy=>{RadicalCodim1})

viewResults time runExamples(H, level3, Verbosity=>1)
viewResults time runExamples(H, level3, Verbosity=>1, Strategy=>{RadicalCodim1})

-----------------------------------------------------------
runExamples H
runExamples(H,20)
netList runExamples(H,{1,2,3,4,5,6,7})

runSingIC(H,27)
runSingIC(H,28)  -- 3.42
runSingIC(H,29)  -- 0
runSingIC(H,{30,31,32,33,34,35,36,37,38}) -- all easy
runSingIC(H,39)  -- 1
runSingIC(H,40)  -- 0
runSingIC(H,41)  -- 0
runSingIC(H,42)  -- bigger (> few minutes, at least)
runSingIC(H,43)  -- also not immediate...
runSingIC(H,44)  -- 0
runSingIC(H,45)  -- 0
runSingIC(H,46)  -- 1
runSingIC(H,47)  -- bigger
runSingIC(H,48)  -- 0

runSingIC(H,10)  -- these seem to take a while too...
runSingIC(H,25)  -- 
runSingIC(H,26)  -- 

--- example rees3-32003:
kk = ZZ/32003 -- -- time for int closure of ideal on 3/5/09 (DE big machine): 750 sec
S = kk[a,b,c]
I=ideal(a^4+b^4+c^4+a^3+a^2*b+a^2*c)
time R=reesAlgebra ideal jacobian I
J = ideal (flattenRing R)_0
jac = trim(J + minors(codim J, jacobian J))
use ring jac
decompose trim ideal apply(jac_*, f -> f // gcd(f, diff(a,f)))
time decompose trim ideal apply(jac_*, f -> product apply(apply(toList factor f, toList), first))


time sum apply(jac_*, f -> decompose((ideal f) : ideal jacobian ideal f))

jac_*/factor//netList
use ring jac
jac = trim(ideal"4c3+a2,4b3+a2" + jac)
betti jac
decompose jac

----------------------------------------
-- 
kk = ZZ/32003
charPairs(4,{6,7},kk)
time integralClosure oo

kk = ZZ/32003
C = charPairs(4,{6,12,13},QQ)
time C' = integralClosure C
F = (ideal C)_0
use ring F
D = discriminant(F,x)
factor D
singularLocus C
J = ideal oo
eliminate(x,J)

-- This one is really bad at the moment...
kk = ZZ/32003
C = charPairs(6,{9,15,17},kk)
F = (ideal C)_0
use ring oo
factor discriminant(F,y)
factor discriminant(F,x)
time C' = integralClosure C
