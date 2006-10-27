newPackage(
	"gfanInterface",
    	Version => "1.0", 
    	Date => "October 26, 2006",
    	Authors => {
	     {Name => "Mike Stillman", Email => "", HomePage => ""}
	     },
    	Headline => "Interface to the gfan package",
    	DebuggingMode => true
    	)

export {
     gfan, wtvec, inw
     }

exportMutable {
     options'gfan
     }

options'gfan = new MutableHashTable from {
     "path" => "/Users/mike/local/software/sage/sage-1.4.1.2/local/bin/",
     "keep files" => true
     }

needs "FourierMotzkin.m2"
wtvec = (I,inI) -> (
  W = flatten apply(numgens I, i -> (
    f := inI_i;
    g := I_i;
    m = first exponents(f);
    apply(exponents(g-f), e -> m-e)));
  W = transpose matrix W;
  W' = - (fourierMotzkin W)_0;
  w := sum entries transpose W';
  (w, W',W))

inw = (w,I) -> (
     R := ring I;
     R1 := (coefficientRing R)[gens R, Weights=>w];
     I' := substitute(I,R1);
     substitute(leadTerm gens gb I', R))

gfan = method()
gfan Ideal := (I) -> (
     R := ring I;
     p := char R;
     if p === 0 and R =!= QQ then 
     error "expected prime field or QQ";
     -- Create the input file
     f := temporaryFileName();
     << "using temporary file " << f << endl;
     F := openOut f;
     -- Now make the list of the gens of I
     F << "{";
     n := numgens I - 1;
     for i from 0 to n do (
     	  F << toExternalString(I_i);
	  if i < n then F << "," else F << "}";
	  F << endl;
	  );
     F << close;
     ex := if p > 0 then 
       options'gfan#"path"| "gfan --mod" | p | "  <" | f | " >" | f | ".out"
     else
       options'gfan#"path"| "gfan <" | f | " >" | f | ".out";
     run ex;
     -- Now grab the list of ideals, and lead term ideals
     output = get (f | ".out");
     L = value output;
     M = lines output;
     M = apply(M, s -> if #s == 0 or s#0 == "{" or s#0 == "}" or s#0 == "," 
	  then s
          else (p := regex("^[a-zA-Z0-9\\^\\*]*",s); 
	        substring(s,p#0#0,p#0#1) | s#(#s-1)));
     M = value concatenate M;
     (M,L)
     )

beginDocumentation()

document { 
	Key => "gfanInterface",,
	Headline => "a Macaulay2 interface to gfan",
	EM "gfanInterface", " is an interface to Anders Jenssen's gfan package, which can compute all
	of the initial ideals of an ideal."
	}
document {
	Key => {gfan, (gfan, Ideal)},
	Headline => "all initial monomial ideals of an ideal",
	Usage => "(M,L) = gfan I",
	Inputs => { "I" => Ideal },
	Outputs => {
	     "M" => List => "of lists of monomials",
	     "L" => List => "of all of the initial ideals of I"
	     },
	EXAMPLE lines ///
	  R = ZZ/32003[symbol a..symbol d]
	  I = monomialCurveIdeal(R,{1,3,4})
	  time (M,L) = gfan I
	  M/toString/print;
	  L/toString/print;
     	///,
	Caveat => {""}
	}

end
restart
loadPackage "gfanInterface"
installPackage "gfanInterface"
R = ZZ/3[a..d]
I = ideal(a^3-a,b^3-b,a+2*b+c+d-1,a^2*b+b^2*a,c^3-c)
time (M,L) = gfan I;
time apply(#M, i -> (
  wtvec(ideal(L_i), ideal(M_i))))
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
gfan_facets
gfan_fvector # works for homogeneous ideals, fvector of state polytope
gfan_groebnercone
  #gfan_homogeneityspace # output will probably change
  #gfan_homogenize		
gfan_initialforms # ??
  #gfan_interactive	
  #gfan_ismarkedgroebnerbasis
gfan_leadingterms  # maybe use this to get lead term ideals
  #gfan_markpolynomialset		
  #gfan_polynomialsetunion		

gfan_render  # for 3 vars
gfan_renderstaircase # for 3 vars

  #gfan_stats			
  #gfan_substitute			
  #gfan_tolatex			

gfan_tropicalbasis		
gfan_tropicalintersection	
gfan_tropicalstartingcone	
  #gfan_tropicaltraverse		

gfan_weightvector 
