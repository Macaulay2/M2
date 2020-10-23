-- -*- coding: utf-8 -*-
polymake := findProgram("polymake", "polymake --version", RaiseError => false)

newPackage(
	"StatePolytope",
    	Version => "1.2", 
    	Date => "October 6, 2008",
    	Authors => {
	     {Name => "Dave Swinarski", Email => "swinarsk@math.columbia.edu"}
	     },
    	Headline => "the state polytope of an ideal",
	Keywords => {"Convex Geometry", "Interfaces"},
	-- DebuggingMode should be true while developing a package, 
	--   but false after it is done
	Configuration => {"gfan command" => "gfan"},
    	DebuggingMode => false,
	AuxiliaryFiles => true,
	CacheExampleOutput => true,
	PackageImports => {"gfanInterface"},
	OptionalComponentsPresent => polymake =!= null
    	)

export { 
     "initialIdeals", --documented
     --"hilbertPt", 
     --"statePolytopePoints", 
     --"statePolytopePoints", 
     "polymakeStatePolytope", --documented
     --"maxUGBDegree", 
     "isStable" --documented
     }

if (options StatePolytope)#Configuration#"gfan command" != "gfan" then stderr <<
    "warning: The \"gfan command\" configuration option has been deprecated." <<
    endl <<
    "If gfan is installed in a non-standard location, then specify it by" <<
    endl <<
    "setting programPaths#\"gfan\"." << endl

initialIdeals = method(
     TypicalValue => List
     )
initialIdeals(Ideal) := (I) -> apply(gfan I, first)

hilbertPt = (n,m,L,monomialsList) -> (
     totalDegree := binomial( n-1 + m, n-1) * m / n ;
     totalDegreeVector := apply(n, k-> totalDegree);
     complementHilbPt := flatten exponents product flatten apply(#monomialsList, i -> {if product apply(#L, j -> monomialsList_i % L_j) != 0 then monomialsList_i else 1});
     idealHilbertPt := totalDegreeVector - complementHilbPt;
     return idealHilbertPt
     )

statePolytopePoints = method(
     TypicalValue => List)
statePolytopePoints(ZZ,Ideal,List) := (m,I,initialIdealsList) -> (
     n := numgens ring(I);
     monomialsList := flatten entries(basis(m,ring(I)));
     return apply(#initialIdealsList, k -> hilbertPt(n,m,initialIdealsList_k,monomialsList))
	  )

fullStatePolytopePoints = method(
     TypicalValue => List)
fullStatePolytopePoints(ZZ,Ideal,List) := (m,I,initialIdealsList) -> (
     sum apply(m+1,k-> statePolytopePoints(k,I,initialIdealsList))
     )

runPolymake = args -> (
    if polymake === null then
	polymake = findProgram("polymake", "polymake --version");
    polymakeRun := runProgram(polymake, "--no-config " | args);
    polymakeRun#"output"
)

M2toPolymake = points ->
    "'my $p = new Polytope(POINTS=>[" |
	demark(", ", apply(points, point ->
	    "[" | demark(", ", toString \ prepend(1, point)) | "]")) |
	"]); print $p->VERTICES;'"

polymakeToM2 = (st) ->
    apply(lines st, line -> value \ drop(separate(" ", line), 1))

maxUGBDegree = (L) -> (
     (max apply(#L, i -> max apply(#(L_i), j -> degree L_i_j)))_0
	  )

polymakeStatePolytope = method(
     TypicalValue => List
     )
polymakeStatePolytope(ZZ,Ideal) := (m,I) -> (
    initialIdealsList := initialIdeals(I);
    st := runPolymake M2toPolymake statePolytopePoints(m,I,initialIdealsList);
    polymakeToM2(st)
)

polymakeStatePolytope(Ideal) := (I) -> (
    initialIdealsList := initialIdeals(I);
    st := runPolymake M2toPolymake fullStatePolytopePoints(
	maxUGBDegree(initialIdealsList),I,initialIdealsList);
    polymakeToM2(st)
)

isStable = method(
     TypicalValue => Boolean
     )
isStable(ZZ,Ideal) := (m,I) -> (
     initialIdealsList := initialIdeals(I);
     points := statePolytopePoints(m,I,initialIdealsList);
     st := runPolymake M2toPolymake points;
     n := numgens ring I;
     barycenter := apply(n, i -> sum points_0 / n);
     augmentedpoints := prepend(barycenter, points);
     augmentedstatepolytope :=  runPolymake M2toPolymake augmentedpoints;
     if set polymakeToM2(st) === set polymakeToM2(augmentedstatepolytope)
	then true else false
)

beginDocumentation()
document {    
	Key => StatePolytope,
	Headline => "computes state polytopes of ideals",
	EM "StatePolytope", " computes state polytopes of ideals using the programs ", TT "gfan", ", ", TT "M2", ", ", "and ", TT "polymake", ".  Specifically, it computes ", ITALIC "State", SUB "m", "(I)", " or ", ITALIC "State", "(I)", " as defined in Sturmfels's book ", ITALIC "Groebner bases and convex polytopes", ", page 14.  There is also a function for testing GIT stability of an ideal in a polynomial ring with respect to action of the maximal torus scaling the variables.", PARA{},
	"We assume that the user has ", TT "gfan", " version 0.3 or higher on his or her system.  The default assumption is that the command to run ", TT "gfan", " is ", TT "gfan", ", but this can be changed in the package configuration if necessary.  The user also needs ", TT "polymake", " version 2.3-1 or higher installed on his or her system, and it must be run by this command.   Finally, the user is responsible for making sure the ideal of study is supported by ", TT "gfan", ".  In version 0.3 this means the ideal must be in a polynomial ring over ", TT "QQ", " or ", TT "ZZ/p", " with p < 32749, and no variable's name should be a substring of another's."
	}




   
   document {
	Key => {initialIdeals, (initialIdeals,Ideal)},
	Headline => "calls gfan and returns the list of initial ideals",
	Usage =>  "initialIdeals(I)",
	Inputs => {
		Ideal => "the ideal"
		},
	Outputs => {
		{"the list of initial ideals of I.  Note that each initial ideal is given as a list of monomial generators in ring(I), not as a monomial ideal."
		}},
	"Given an ideal ", TT "I", " in ", TT "M2", ", this function calls ", TT "gfan", " and returns the list of initial ideals of ", TT "I", ". ",
	   
	EXAMPLE lines ///
	        R = QQ[a,b];
		I = ideal(a^2+b^2,a*b);
		initialIdeals(I)  
		///,
		}

TEST ///
R=QQ[a..d];
I=ideal(a*c-b^2,a*d-b*c,b*d-c^2);
L= {{b*d, a*d, a*c}, {a*d, b*d, b^2}, {a*d^2, b*d, b*c, b^2}, {b*d, b^2, b*c,
     c^3}, {a*d, a*c, c^2}, {a^2*d, a*c, c^2, b*c}, {a*c, c^2, b*c, b^3}, {c^2,
     b*c, b^2}};
assert(set initialIdeals(I) === set L)
///
     
document {
	Key => "polymakeStatePolytope",
	Headline => "computes state polytopes of ideals",
	"Computes ",  ITALIC "State", SUB "m", "(I) or ", ITALIC "State", "(I), as defined in Sturmfels's book ", ITALIC "Groebner bases and convex polytopes", ", page 14."
	}
     
     
	


 document {
	Key => (polymakeStatePolytope,Ideal),
	Headline => "computes the state polytope of an ideal",
	Usage => "statePolytope(I)",
	Inputs => {
		Ideal => "the ideal",
		},
Outputs => {
		List => {"the list of vertices of the state polytope"}
		},

"See Sturmfels's book ", ITALIC "Groebner bases and convex polytopes", ", page 14 for the definition of ",  ITALIC "State", "(I).  (The difference between this and ", ITALIC "State", SUB "m", "(I) is that for all sufficiently large m, ", ITALIC "State", SUB "m", "(I) does not distinguish between initial ideals which have the same saturation with regard to the irrelevant ideal, whereas in ", ITALIC "State", "(I), these are separated.) ",

EXAMPLE lines ///
	        R = QQ[a..d];
		I = ideal(a*c-b^2,a*d-b*c,b*d-c^2);  
		polymakeStatePolytope(I)
		///,
		
}  

	

   
       
   document {
	Key => (polymakeStatePolytope,ZZ,Ideal),
	Headline => "computes the mth state polytope of an ideal",
	Usage => "statePolytope(m,I)",
	Inputs => {
	     ZZ => "specifies to compute the mth state polytope",
		Ideal => "the ideal",	
		},
	Outputs => {
		List => "the list of vertices of the state polytope"
		},
	
	"See Sturmfels's book ", ITALIC "Groebner bases and convex polytopes", ", page 14 for the definition of ",  ITALIC "State", SUB "m", "(I).  (The difference between this and ", ITALIC "State", "(I) is that for all sufficiently large m, ", ITALIC "State", SUB "m", "(I) does not distinguish between initial ideals which have the same saturation with regard to the irrelevant ideal, whereas in ", ITALIC "State", "(I), these are separated.) ",
	   
      
	EXAMPLE lines ///
	        R = QQ[a..d];
		I = ideal(a*c-b^2,a*d-b*c,b*d-c^2);
		polymakeStatePolytope(3,I)
		///,
		}


TEST ///
R=QQ[a..d];
I=ideal(a*c-b^2,a*d-b*c,b*d-c^2);
L = {{9, 6, 6, 9}, {9, 3, 12, 6}, {7, 5, 14, 4}, {5, 8, 14, 3}, {3, 12, 12,3}, {6, 12, 3, 9}, {4, 14, 5, 7}, {3, 14, 8, 5}};
assert(set polymakeStatePolytope(3,I) === set L)
M ={{11, 7, 7, 11}, {11, 3, 15, 7}, {8, 6, 18, 4}, {6, 9, 18, 3}, {3, 15, 15, 3}, {7, 15, 3, 11}, {4, 18, 6, 8}, {3, 18, 9, 6}};
assert(set polymakeStatePolytope(I) === set M)
///




	   
	
	document {
	Key => {isStable, (isStable,ZZ,Ideal)},
	Headline => "determines whether the mth Hilbert point of I is GIT stable",
	Usage =>  "isStable(3,I)",
	Inputs => {ZZ => "specifies which Hilbert point to test",
		Ideal => "the ideal"	
		},
	"Bayer and Morrison showed that GIT stability of the mth Hilbert point of I with respect to the maximal torus acting on a polynomial ring by scaling the variables can be tested by whether ", ITALIC "State", SUB "m", "(I) contains a certain point.",
	
	   
	EXAMPLE lines ///
	        R = QQ[a..d];
		I = ideal(a*c-b^2,a*d-b*c,b*d-c^2);
		isStable(3,I)
		I = ideal(a^2,b^2,b*c);
		isStable(3,I) 
		///,
		}	  
	   
	   
TEST ///
R=QQ[a..d];
I=ideal(a*c-b^2,a*d-b*c,b*d-c^2);
assert(isStable(3,I) == true)
R=QQ[a..c];
J=ideal(a*b-c^2,a*b*c);
assert(isStable(2,J) ==true)
assert(isStable(3,J) ==true)
assert(isStable(4,J) ==false)
///

end
   
   
TEST ///
--gfanCommand = concatenate(///!///,((options(StatePolytope))#Configuration)#"gfan command");
f = openInOut gfanCommand;
f << "Q[a,b]" << "{a^2+b^2,a*b}";
f << closeOut;
g = get f;
g = replace(newline,"",g);
assert(g == "Q[a,b]{{b^3,a*b,a^2+b^2},{b^2+a^2,a*b,a^3}}")
///   
   
installPackage "StatePolytope"
installPackage("StatePolytope", RemakeAllDocumentation=>true)
check StatePolytope

--initialIdeals = (I) -> (
--gfanoutputfile := openInOut gfanCommand;
--gfanoutputfile << gfanRingInput(I) << gfanIdealInput(I);
--gfanoutputfile << closeOut;
--gfanoutputstring := get gfanoutputfile;
--if gfanoutputstring == "" then error "gfan output is empty; check the version of gfan and the command you are using";
--markedInitialIdealsString:=replace("[Q,Z].*]","",gfanoutputstring);
--use(ring(I));
--initialIdealsList = value replace("[+-][^,{}]*}","}",replace("[+-][^,{}]*,",",",markedInitialIdealsString));
--return initialIdealsList
--)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=StatePolytope pre-install"
-- End:
