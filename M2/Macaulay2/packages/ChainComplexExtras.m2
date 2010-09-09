-- -*- coding: utf-8 -*-
newPackage(
     "ChainComplexExtras",
     Version => "0.5",
     Date => "December 10, 2006",
     Authors => {
	  {Name => "Frank Moore", Email => "fmoore@math.unl.edu", HomePage => "http://www.math.unl.edu/~s-wmoore3"},
	  {Name => "Greg Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"}
	  },
     Headline => "Some additional ChainComplex Functions.",
     DebuggingMode => true
     )

export(isExact)
export(isChainComplex)
export(isChainComplexMap)
export(isQuism)
export(koszulComplex)
export(taylor)
export(taylorResolution)
export(chainComplexMap)
export(InitialDegree)

substitute(ChainComplex,Ring):=(C,newRing)->(
   --- this function is just a version of substitute for chain complexes
   chainComplex(apply((min C + 1..max C), i -> substitute(C.dd_i, newRing)))
)

Hom(ChainComplex,ChainComplex) := (F,G)->(
   outputCx := new ChainComplex;
   outputCx.ring = ring F;
   topDegree := max G - min F;
   botDegree := min G - max F;
   index1 := topDegree;
   while (index1 > botDegree) do
   {
      sourceList := toList (min F .. max G - index1);
      targetList := toList (min F .. max G - (index1 - 1));
      myFn := i -> (fold((a,b) -> (a || b),
		         apply(targetList,
			       j -> (if (j == i) then 
			               Hom(F_i,G.dd_(i+index1))
				     else if (j == i+1) then
				       (-1)^index1*Hom(F.dd_j,G_(i+index1))
				     else map(Hom(F_j,G_(j+index1-1)),Hom(F_i,G_(i+index1)),0)
				     )
				)
			   )
		     );
      diffl := fold((a,b) -> (a | b), apply(sourceList, myFn));
      outputCx#index1 = source diffl;
      outputCx#(index1 - 1) = target diffl;
      outputCx.dd#index1 = diffl;
      index1 = index1 - 1;
   };
   outputCx.max = topDegree;
   outputCx.min = botDegree;
   outputCx.length = topDegree - botDegree;
   outputCx
)

chainComplexMap=method(
    Options => {InitialDegree => -infinity}
)
chainComplexMap(ChainComplex,ChainComplex,List):= o -> (D,C,maps) -> (
   --- the code commented out should also work, and is in some sense
   --- more desireable as it uses map in the code.  However, something squirly
   --- happens in the map code.
   ---    startDeg := min C;
   ---    if (o.InitialDegree != -infinity) then startDeg = o.InitialDegree;
   ---    definingSet := set (startDeg..startDeg + length maps - 1);
   ---    map(D,C,i -> (if member(i, definingSet) then maps_(i - startDeg) else 0))
   startDeg := min C;
   if (o.InitialDegree != -infinity) then startDeg = o.InitialDegree;
   F := new ChainComplexMap;
   F.degree = 0;
   F.source = C;
   F.target = D;
   index1 := startDeg;
   scan(maps, x -> (F#index1 = x; index1 = index1 + 1;));
   F
)

isExact=method()
isExact(ChainComplex):=(C) -> (
   if (all((min C,max C), i -> (prune HH_i(C) == 0))) then true else false
)

isChainComplex=method()
isChainComplex(ChainComplex):=(inputComplex)->(
   if (inputComplex.dd^2 == 0) then true else false
)

isChainComplexMap=method()
isChainComplexMap(ChainComplexMap):=(inputMap)->(
   isChainComplex(cone inputMap)
)

isQuism=method()
isQuism(ChainComplexMap):=(phi) -> (
   isExact(cone phi)
)

ChainComplexMap | ChainComplexMap := (f,g) -> (
   --- this function overloads the | operator for ChainComplexMap
   retVal := 0;
   matrList := {};
   if (target f == target g) then
      retVal = map(target f, source f ++ source g, (i -> (f_i | g_i)))
   else
   {
      retVal = Nothing;
      error "Targets of ChainComplexMaps must be the same.";
   };
   retVal
)

ChainComplexMap || ChainComplexMap := (f,g) -> (
   --- this function overloads the | operator for ChainComplexMap
   retVal := 0;
   matrList := {};
   if (source f == source g) then
      retVal = map(target f ++ target g, source f, (i -> (f_i || g_i)))
   else
   {  
      retVal = Nothing;
      error "Source of ChainComplexMaps must be the same.";
   };
   retVal
)

koszulComplex=method(
    Options => {LengthLimit => 0}
)

koszulComplex(Ideal):= o -> (I)->(
    --- this function just returns the Koszul complex
    --- where I represents the first differential.
    if not instance(o.LengthLimit, ZZ)
    then error "The optional LengthLimit must be an integer.";
    lengthLimit := 0;
    if (o.LengthLimit == 0) then
       lengthLimit = numgens I
    else
       lengthLimit = o.LengthLimit;
    chainComplex(apply(toList (1 .. lengthLimit), i -> koszul(i, gens I)))
)

myLcm = method()
myLcm(List):=(ringList)->(
   --- just a short method computing the lcm of the list of elements
   myList := apply(ringList, i -> ideal(i));
   (intersect myList)_0
)

taylor = method()
taylor(ZZ,MonomialIdeal):= (n,I)->(
   --- create the nth differential in Taylor's resolution
   retVal := Nothing;
   if (n == 1) then 
      retVal = gens I
   else
   {
      idealList := flatten entries gens I;
      R := ring I;
      sourceSubsets := subsets(toList (0..(numgens I - 1)),n);
      targetSubsets := subsets(toList (0..(numgens I - 1)),n-1);
      sourceList := apply(sourceSubsets, i -> myLcm(idealList_i));
      targetList := apply(targetSubsets, i -> myLcm(idealList_i));
      getCoeff := (i,j) -> if (isSubset(targetSubsets_i,sourceSubsets_j)) then
                             (-1)^(position(sourceSubsets_j, k -> k == (toList(set sourceSubsets_j - set targetSubsets_i))_0))
			   else 0_R;
      myFn := (i,j) -> (tempElt := sourceList_j / targetList_i;
	                if (liftable(tempElt,R)) then getCoeff(i,j)*lift(tempElt,R) else 0_R);
      retVal = map(R^(-apply(targetList, i -> degree i)), R^(-apply(sourceList, i -> degree i)), myFn);
   };
   retVal
)

taylorResolution=method(
    Options => {LengthLimit => 0}
)
taylorResolution(MonomialIdeal):= o -> (I)->(
    --- this function just returns the Koszul complex
    --- where I represents the first differential.
    if not instance(o.LengthLimit, ZZ)
    then error "The optional LengthLimit must be an integer.";
    lengthLimit := 0;
    if (o.LengthLimit == 0) then
       lengthLimit = numgens I
    else
       lengthLimit = o.LengthLimit;
    chainComplex(apply((1..lengthLimit), i -> taylor(i,I)))
)

syzMap = method()
syzMap(Matrix) := (F) -> (
   -- given any R-homomorphism F : M --> N,
   -- compute a matrix P --> M, where P is free
   -- and the image is ker(F).
   z := modulo(matrix F, presentation target F);
   map(source F, source z, z)
)

resolution(ChainComplex) := o -> (C) -> (
   --- C is a ChainComplex
   --- returns a surjective ChainComplexMap from a complex of free modules
   --- to C that is a isomorphism in homology
   --- NOTICE: When using this function, the source complex, as well as the
   ---         map, will always be correct.  However, if you try and take
   ---         the mapping cone of the map and the target complex has some
   ---         zero differentials, the mapping cone complex may not be exact!
   ---         This is a bug in M2, as of 0.9.20, on 10/30/2006.
   mapList := {};
   difflList := {};
   R := ring C;
   lengthLimit := max C + numgens R;
   if (o.LengthLimit != infinity) then lengthLimit = min C + o.LengthLimit;
         
   prevf := syzMap(C.dd_(min C));
   mapList = append(mapList, prevf);
   prevf''' := prevf;
   prevd := map(R^0, source prevf,0);
   index1 := min C;
   while (index1 <= lengthLimit) do
   {
      --- f''' is the part of the previous f map we need for this step
      --- it is a cover of the kernel of the diffl C_i ---> C_(i-1)
      (newd,newf,newf''') := nextReslnStep(prevd,prevf,C.dd_(index1+1),prevf''');
      mapList = append(mapList,newf);
      difflList = append(difflList,newd);
      prevd = newd;
      prevf = newf;
      prevf''' = newf''';
      if (newd == 0) then index1 = lengthLimit;
      index1 = index1 + 1;
   };
   --P := chainComplex(difflList, min C);
   --- this line is here instead of the above one because I cannot get M2 to override
   --- chainComplex(List,ZZ)
   P := chainComplex(difflList)[-min C];
   P#dd = (-1)^(min C)*P.dd;
   chainComplexMap(C,P,mapList)
)

nextReslnStep=method()
nextReslnStep(Matrix,Matrix,Matrix,Matrix) := (prevPDiffl,prevQuism,CDiffl,PDifflCover) -> (
   -- prevPDiffl : P_i --> P_(i-1)
   -- prevQuism : P_i --> C_i
   -- CDiffl == C.dd_(i+1)
   -- PDifflCover = part of prevDQuism, P_i''' --> C_i, cover of kernel of C_i --> C_(i-1)
   -- Returns a triple of maps (d,f,f''')
   -- where
   -- d : P_(i+1) --> P_i
   -- f : P_(i+1) --> C_(i+1)
   -- f''' : P_(i+1)''' ---> C_(i+1)
   -- VARIABLE CONVENTION:
   --              '    <---> fixing prevQuism and prevPDiffl
   --              ''   <---> surjectivity of the quism
   --              '''  <---> fixing CDiffl
   d' := syzMap ( prevQuism || prevPDiffl);
   f' := map(source CDiffl, source d', 0);
   g := map(target CDiffl, source matrix CDiffl, matrix CDiffl);
   f'' := g // CDiffl;
   d'' := g // PDifflCover;
   --- change the target of d'' to be the source of f'''
   F := (source prevQuism)_{0..(numgens source prevQuism-numgens source PDifflCover-1)};
   inc1 := map(source F, source PDifflCover, 0);
   inc2 := id_(source PDifflCover);
   inc := inc1 || inc2;
   d'' = inc * d'';
   f''' := syzMap CDiffl;
   d''' := map(source prevPDiffl, source f''', 0);
   (d' | d'' | d''', f' | f'' | f''', f''')
)

beginDocumentation()

document {
     Key => ChainComplexExtras,
     Headline => "More ChainComplex Functionality."
     }

document {
     Key => (substitute,ChainComplex,Ring),
     Headline => "Change the ring over which the ChainComplex is defined.",
     Usage => "substitute(chCx, S)",
     Inputs => {
	  "chCx" => {},
	  "S" => {},
     },
     Outputs => {
	  {"The ChainComplex chCx over the ring R"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "S = R/ideal{a^2,b^2,c^2}",
	     "kRes = res coker vars R",
	     "kResS = substitute(kRes, S)",
	     }
     }

document {
     Key => (Hom,ChainComplex,ChainComplex),
     Headline => "Create the homomorphism complex of a pair of chain complexes.",
     Usage => "Hom(F,F)",
     Inputs => {
	  "F" => {},
	  "G" => {},
     },
     Outputs => {
	  {"Hom(F,G)"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "Hom(kRes,kRes)",
	     }
     }

document {
     Key => {isExact, (isExact,ChainComplex)},
     Headline => "Test to see if the ChainComplex is exact.",
     Usage => "isExact(F)",
     Inputs => {
	  "F" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "k = coker vars R",
	     "kRes = res k",
	     "isExact kRes",
	     "trivialCx = chainComplex matrix {{1_R}}",
	     "isExact trivialCx",
	     }
     }

document {
     Key => {isChainComplex, (isChainComplex,ChainComplex)},
     Headline => "Test to see if the ChainComplex has square zero differential.",
     Usage => "isChainComplex(F)",
     Inputs => {
	  "F" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     "This function was implemented to verify that ChainComplexMaps returned from
      the extend command were indeed ChainComplexMaps",
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "isChainComplex(kRes)",
	     }
     }

document {
     Key => (resolution,ChainComplex),
     Headline => "Resolves a ChainComplex.",
     Usage => "resolution C",
     Inputs => {
	  "C" => {},
     },
     Outputs => {
	  {"ChainComplexMap"}
     },
     "Returns a surjective ChainComplexMap whose source consists",
     "of free R-modules, where R = C.ring.",
     EXAMPLE {
	        "R = ZZ/32003[a..d]",
		"I = monomialCurveIdeal(R,{1,2,3})",
		"C = koszulComplex(ideal vars R) ** (R^1/I);",
		"CResMap = res C;",
		"CRes = source CResMap",
		"isQuism CResMap",
	     }
     }

document {
     Key => {chainComplexMap, (chainComplexMap,ChainComplex,ChainComplex,List)},
     Headline => "Defines a ChainComplexMap via a list of matrices.",
     Usage => "chainComplexMap(D,C,mapList)",
     Inputs => {
	  "D" => ChainComplex => {"target of ChainComplexMap"},
	  "C" => ChainComplex => {"source of ChainComplexMap"},
	  "mapList" => List => {"list of maps defining the new ChainComplexMap"},
     },
     Outputs => {
	  ChainComplexMap => {"The desired ChainComplexMap."},
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "mapList = apply((min kRes..max kRes), i -> multBya_i)",
	     "multBya2 = chainComplexMap(kRes,kRes,toList mapList)",
	     "multBya2 == multBya",
	     }
     }

document {
     Key => {isChainComplexMap, (isChainComplexMap,ChainComplexMap)},
     Headline => "Test to see if the ChainComplexMap commutes with the differentials.",
     Usage => "isChainComplexMap(phi)",
     Inputs => {
	  "phi" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "isChainComplexMap(multBya)",
	     }
     }

document {
     Key => InitialDegree,
     Headline => "Used to specify an initial degree for chainComplexMap.",
     TT "InitialDegree", " -- an optional argument for chainComplexMap",
     "used to specify the starting degree of the map.",
     PARA{},
     "This symbol is provided by the package ", TO ChainComplexExtras, "."
     }
document {
     Key => [chainComplexMap,InitialDegree],
     Headline => "Specify initial degree.",
     Usage => "chainComplexMap(...,InitialDegree=>n)",
     Inputs => {
        "n" => ZZ => "The initial degree of the ChainComplexMap."
       },
     Consequences => {
        {"The ChainComplexMap starts in degree n instead of the start of source."}
       }
     }

document {
     Key => {isQuism, (isQuism,ChainComplexMap)},
     Headline => "Test to see if the ChainComplexMap is a quasiisomorphism.",
     Usage => "isChainComplexMap(phi)",
     Inputs => {
	  "phi" => {},
     },
     Outputs => {
	  {"Boolean"}
     },
     "A quasiisomorphism is a chain map that is an isomorphism in homology.",
     "Mapping cones currently do not work properly for complexes concentrated",
     "in one degree, so isQuism could return bad information in that case.",
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "kRes = res coker vars R",
	     "multBya = extend(kRes,kRes,matrix{{a}})",
	     "isQuism(multBya)",
	     "F = extend(kRes,kRes,matrix{{1_R}})",
	     "isQuism(F)",
	     }
     }

document {
     Key => {koszulComplex, (koszulComplex,Ideal)},
     Headline => "Gives the Koszul complex on the generators of I.",
     Usage => "koszulComplex(I)",
     Inputs => {
	  "I" => {},
     },
     Outputs => {
	  {"ChainComplex"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b,c]",
	     "K = koszulComplex(ideal vars R)",
	     }
     }

document {
     Key => {taylor, (taylor,ZZ,MonomialIdeal)},
     Headline => "Gives the nth differential in the Taylor resolution of a monomial ideal I.",
     Usage => "T = taylor(n,I)",
     Inputs => {
	  "n" => {},
	  "I" => {},
     },
     Outputs => {
	  {"T, the nth differential in the Taylor Resolution of I."}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b]",
	     "I = monomialIdeal (ideal vars R)^3",
	     "T2 = taylor(2,I)",
    	     "T3 = taylor(3,I)",
	     }
     }

document {
     Key => {taylorResolution, (taylorResolution,MonomialIdeal)},
     Headline => "Gives the Taylor resolution of a monomial ideal I.",
     Usage => "taylorResolution(I)",
     Inputs => {
	  "I" => {},
     },
     Outputs => {
	  {"ChainComplex"}
     },
     EXAMPLE {
	     "R = ZZ/101[a,b]",
	     "I = monomialIdeal (ideal vars R)^3",
	     "T = taylorResolution(I)",
	     "T.dd",
	     }
     }

end

