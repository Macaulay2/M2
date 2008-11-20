-- -*- coding: utf-8 -*-
newPackage(
	"gfanInterface",
    	Version => "0.1", 
    	Date => "March 11, 2007",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu"}
	     },
    	Headline => "Interface to A. Jensen's gfan package",
	Configuration => { "path" => "",
	     "keep files" => true
	      },
    	DebuggingMode => true
    	)

export {
     gfan, weightVector, inw, groebnerCone, Symmetries, Tracing
     }

gfan'path = gfanInterface#Options#Configuration#"path"

--needs "FourierMotzkin.m2"
needsPackage "FourierMotzkin"

wtvec = (inL,L) -> (
  W := flatten apply(#inL, i -> (
    f := inL#i;
    g := L#i;
    m = first exponents(f);
    apply(exponents(g-f), e -> m-e)));
  W = transpose matrix W;
  F := fourierMotzkin W;
  W' := - F_0;
  w := sum entries transpose W';
  (w, W',F_1,W))

weightVector = method(TypicalValue=>List)
weightVector(List,List) := (inL,L) -> (
     (w,W',H,W) := wtvec(inL,L);
     --(W',H) is the "cone": W' are the positive edges, and H is contained inside.
     -- we need to find a positive element in here
     if min w > 0 then w else
     if numgens source H > 0 then (
	  h := sum entries transpose H;
	  if min h <= 0 then error "need to try harder to find a weight vector";
     	  while min w <= 0 do w = w + h;
     	  w)
     -- other cases should return null
     )

groebnerCone = method()
groebnerCone(List,List) := (inL,L) -> (
     (w,W',H,W) := wtvec(inL,L);
     (W',H)
     )

inw = method(TypicalValue=>Ideal)
inw(List,Ideal) := (w,I) -> (
     R := ring I;
     R1 := (coefficientRing R)[gens R, 
	      Degrees=>(monoid R).Options.Degrees, 
	      MonomialOrder=>Weights=>w];
     I' := substitute(I,R1);
     ideal substitute(leadTerm(1, gens gb I'), R))

writeGfanIdeal = method()
writeGfanIdeal(String,Ideal,List) := (filename,I,symms) -> (
     F := openOut filename;
     R := ring I;
     p := char R;
     F << if p === 0 then "Q" else "Z/"|p|"Z";
     F << toExternalString(new Array from gens R) << endl;
     -- Now make the list of the gens of I
     F << "{";
     n := numgens I - 1;
     for i from 0 to n do (
     	  F << toExternalString(I_i);
	  if i < n then F << "," else F << "}";
	  F << endl;
	  );
     symms = apply(symms, x -> apply(x,index));
     -- If any symmetries, write them here
     if #symms > 0 then (
	  F << toString symms << endl;
	  );
     F << close;
     )

readGfanIdeals = method()
readGfanIdeals String := (f) -> (
     -- Reads using the current ring
     s := get f;
     G := separate("\n,",s);
     G = drop(G,1); -- remove the first line, which contains the ring
     H := apply(G, t -> replace(///[\{\}]*///,"",t));
     apply(H, s -> value("{"|s|"}")))


gfan = method(Options=>{Symmetries=>{}, Tracing => 0})
gfan Ideal := opts -> (I) -> (
     R := ring I;
     p := char R;
     if p === 0 and R =!= QQ then 
     error "expected prime field or QQ";
     -- Create the input file
     f := temporaryFileName();
     << "using temporary file " << f << endl;
     ex := "";
     if opts.Symmetries =!= {}
     then (
	  if not instance(opts.Symmetries, VisibleList)
	  then error "Symmetries value should be a list of permutations (list of lists of integers)";
	  ex = ex|" --symmetry";
	  );
     ex = gfan'path| "gfan " | ex | "  <" | f | " >" | f | ".out";
     writeGfanIdeal(f, I, opts.Symmetries);
     run ex;
     ex2 := gfan'path| "gfan_leadingterms -m <" | f | ".out >" | f | ".lt";
     run ex2;
     L = readGfanIdeals(f | ".out");
     M = readGfanIdeals(f | ".lt");
     (M,L)
     )

beginDocumentation()

document { 
	Key => "gfanInterface",,
	Headline => "a Macaulay2 interface to gfan",
	EM "gfanInterface", " is an interface to Anders Jenssen's gfan package, which is a C++
	program to compute the Groebner fan (i.e. all the initial ideals) of an ideal.",
	PARA{
	"The main function in this package is ", TO gfan, ", which computes all of the Groebner bases and initial ideals
	of a given ideal.  A useful
	feature of this function is that it can handle symmetries in the ideal."},
	PARA{"There are other functions in the gfan package.  If you wish to use
	one whose interface is not included here, you have two options: either write the interface yourself, and then send it
	to the package author, so it can be included for others, or ask the package author to write it."},
	}
document {
	Key => {gfan, (gfan, Ideal)},
	Headline => "all initial monomial ideals of an ideal",
	Usage => "(M,L) = gfan I",
	Inputs => { "I" => Ideal,
	     Symmetries => List => "of permutations of the variables leaving the ideal invariant",
	     Tracing => Boolean => "0 means as quiet as possible, larger numbers give more display"
	     },
	Outputs => {
	     "M" => List => "of lists of monomials",
	     "L" => List => "of all of the initial ideals of I"
	     },
	EXAMPLE lines ///
	  R = ZZ/32003[symbol a..symbol d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  time (M,L) = gfan I
	  M/toString/print;
	  L/toString/print;
     	///,
	PARA{},
	"If the ideal is invariant under some permutation of the variables, then gfan can compute
	all initial ideals up to this equivalence, which can change an intractible problem to a doable one.",
	PARA{},
	"The cyclic group of order 4 a --> b --> c --> d --> a leaves the following ideal fixed.",
	EXAMPLE lines ///
	   S = ZZ/32003[a..e];
	   I = ideal"a+b+c+d,ab+bc+cd+da,abc+bcd+cda+dab,abcd-e4"
	   (inL,L) = gfan I;
	   #inL
	   ///,
	PARA{},
	"There are 96 initial ideals of this ideal.  Let's use the symmetry:",
	EXAMPLE lines ///
	   (inL1, L1) = gfan(I, Symmetries=>{(b,c,d,a,e)});
	   #inL1
	   ///,
	Caveat => {""},
	SeeAlso => {"weightVector", "inw", "groebnerCone"}
	}

document {
	Key => {weightVector, (weightVector, List,List)},
	Headline => "weight vector of a marked set of polynomials",
	Usage => "weightVector(inL,L)",
	Inputs => { "inL" => List => {"of monomials which are to be the lead terms of the elements of ", TT "L"},
	     "L" => List => "of polynomials"
	     },
	Outputs => {
	     {"of positive integers giving a weight vector under which ", TT "inL", " are the lead terms of ", TT "L"}
	     },
	"If there is no weight vector, then ", TT "null", " is returned.",
	EXAMPLE lines ///
	  R = ZZ/32003[symbol a..symbol d]
	  inL = {c^4, b*d^2, b*c, b^2*d, b^3}
	  L = {c^4-a*d^3, -c^3+b*d^2, b*c-a*d, -a*c^2+b^2*d, b^3-a^2*c}
	  weightVector(inL,L)
	  groebnerCone(inL,L)
	  ///,
        PARA{"Now we construct all of the initial ideals of the rational quartic curve in P^3,
	     then compute weight vectors for each, and then verify that the initial ideals that
	     gfan returned are the initial ideals using these weight vectors."},
	EXAMPLE lines ///
	  I = monomialCurveIdeal(R,{1,3,4})
	  time (inLs,Ls) = gfan I
     	  wtvecs = apply(#inLs, i -> weightVector(inLs#i, Ls#i));
	  wtvecs/print;
     	  inL1 = wtvecs/(w -> inw(w,I));
	  inL1/toString/print;
	  assert(inL1 == inLs/ideal)
     	///,
	Caveat => {"In the current implementation, it might be possible that a positive vector exists, but the algorithm fails to find it.  In this 
	     case, use groebnerCone and find one by hand.  You might want to email the package author to complain too!"},
	SeeAlso => {"gfan", "inw", "groebnerCone"}
	}


document {
	Key => {groebnerCone, (groebnerCone, List,List)},
	Headline => "the cone whose interior weight vectors give the given initial ideal",
	Usage => "(C,H) = groebnerCone(inL,L)",
	Inputs => { "inL" => List => {"of monomials which are to be the lead terms of the elements of ", TT "L"},
	     "L" => List => "of polynomials"
	     },
	Outputs => {
     	     "C" => {ofClass Matrix, ", the columns are the extremal rays of the Groebner cone"},
	     "H" => {ofClass Matrix, ", the columns generate the largest linear space contained in the cone"},
	     },
	EXAMPLE lines ///
	  R = ZZ/32003[symbol a..symbol d]
	  inL = {c^4, b*d^2, b*c, b^2*d, b^3}
	  L = {c^4-a*d^3, -c^3+b*d^2, b*c-a*d, -a*c^2+b^2*d, b^3-a^2*c}
	  weightVector(inL,L)
	  groebnerCone(inL,L)
	  I = monomialCurveIdeal(R,{1,3,4})
	  time (inLs,Ls) = gfan I
	  weightVector(inLs#0, Ls#0)
     	  scan(#inLs, i -> print weightVector(inLs#i, Ls#i));
     	  scan(#inLs, i -> print groebnerCone(inLs#i, Ls#i));
     	///,
	Caveat => {"In the current implementation, it might be possible that a positive vector exists, but the algorithm fails to find it.  In this 
	     case, use groebnerCone and find one by hand.  You might want to email the package author to complain too!"},
	SeeAlso => {"gfan", "inw", "groebnerCone"}
	}

document {
	Key => {inw, (inw, List,Ideal)},
	Headline => "initial ideal with respect to a weight vector",
	Usage => "inw(w,I)",
	Inputs => { "w" => List => {"a positive weight vector"},
	     "I" => Ideal => "in a polynomial ring (not a quotient ring)"
	     },
	Outputs => {
	     {"the ideal of lead polynomials under this weight vector"}
	     },
        "The weight vector should be totally positive, even in the homogeneous case.  
	The result may or may not be a monomial ideal.",
	EXAMPLE lines ///
	  R = ZZ/32003[symbol a..symbol d]
	  inL = {c^4, b*d^2, b*c, b^2*d, b^3}
	  L = {c^4-a*d^3, -c^3+b*d^2, b*c-a*d, -a*c^2+b^2*d, b^3-a^2*c}
	  weightVector(inL,L)
	  groebnerCone(inL,L)
	  inw({8,8,3,1},ideal L)
	  inw({5,5,2,1},ideal L)
	  ///,
	SeeAlso => {"gfan", "weightVector", "groebnerCone"}
	}

end
restart
loadPackage "gfanInterface"
installPackage "gfanInterface"
R = ZZ/3[a..d]
I = ideal(a^3-a,b^3-b,a+2*b+c+d-1,a^2*b+b^2*a,c^3-c)
time (M,L) = gfan I;
time apply(#M, i -> (
  weightVector(L_i, M_i)))

wtvecs = apply(oo, x -> x#0)
Mset1 = apply(wtvecs, w -> set flatten entries inw(w,I))
Mset = M/set
Mset === Mset1

R = ZZ/32003[symbol a..symbol d]
I = monomialCurveIdeal(R,{1,3,4})
time (M,L) = gfan I
time apply(#M, i -> (
  wtvec(ideal(L_i), ideal(M_i))))

R = ZZ/32003[symbol a..symbol f]
m = genericSymmetricMatrix(R,a,3)
I = minors(2,m)
time (M,L) = gfan I;
L1 = L/sort;
L2 = L1/(I -> apply(I, leadMonomial))
unique L2
L3 = L2/(I -> flatten entries gens gb ideal I)
unique L3
#oo

-- an example with symmetries
R = ZZ/32003[symbol a..symbol d,symbol e]
I = ideal"a+b+c+d,ab+bc+cd+da,abc+bcd+cda+dab,abcd-e4"
(M1,L1) = gfan I;
(M2,L2) = gfan(I, Symmetries=>{(b,c,d,a,e)});

-- veronese in general coordinates
R = ZZ/32003[symbol a..symbol f]
m = genericSymmetricMatrix(R,a,3)
I = minors(2,m)
F = map(R,R,random(R^1, R^{6:-1}))
I = F I
time (M,L) = gfan I;
M/toString/print

R = ZZ/32003[x,y,z]
I = ideal(x^3-y*z, x*y*z-y^4)
gfan I


gfan  # implement the symmetry
gfan_groebnercone # can replace Fourier-Motzkin
gfan_topolyhedralfan
gfan_render  # for 3 vars
gfan_renderstaircase # for 3 vars

gfan_tropicalbasis		
gfan_tropicalintersection	
gfan_tropicalstartingcone	
  #gfan_tropicaltraverse		



F = get "/tmp/M2-2675-1.out"
G = separate("\n,",F);
H = for i from 0 to #G-1 list if i === 0 then substring(1,G#0) else if i === #G-1 then substring(0,(#G#-1)-1,G#-1) else G#i
apply(H, value)

class output
split
separate("\n,",output)
replace
viewHelp replace
oo/print;


/Users/mike/local/software/sage/sage-1.4.1.2/local/bin/gfan_
