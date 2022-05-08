 -*
 Copyright 2020, 2020 Ashleigh Adams.

 You may redistribute this file under the terms of the GNU General Public
 License as published by the Free Software Foundation, either version 2 of
 the License, or any later version.
 *-
---------------------------------------

newPackage(
    "ResolutionsOfStanleyReisnerRings",
    Version => "0.1",
    Date => "July 15, 2020",
    	Authors => {{Name => "Ashleigh Adams", Email => "adams869@umn.edu", HomePage => "http://www.ashleigh-adams.com"}},
    Headline => "Comparing resolutions of Stanley-Reisner rings and computing various systems of parameters",
    PackageExports => {"SimplicialComplexes", "Posets", "SimplicialDecomposability"},
    Keywords => { "Combinatorial Commutative Algebra" },
    DebuggingMode => false 
    )

-------------------
-- Exports
------------------- 
export {"colorfulSOP","universalSOP","colorfulPresentation","universalPresentation","sumBetti","equalBettiTally"}

-------------------
-- Exported Code
-------------------

-----------------------------------------
--Barycentric Subdivision-
-----------------------------------------
-- the 'barycentricSubdivison' method is defined in 'SimplicialComplexes'
barycentricSubdivision (SimplicialComplex) := D -> (
   orderComplex(facePoset(D),CoefficientRing=>coefficientRing(ring(D)))
);

barycentricSubdivision (List) := F -> (
   D := simplicialComplex(F);
   orderComplex(facePoset(D),CoefficientRing=>coefficientRing(ring(D)))
);

---------------------------------------------------
--Non-exported code for Colorful Version
---------------------------------------------------

-- Used in balancedColoring function, it returns the color of a vertex
vertexColor = (a,colors) -> (
   return position(toList(colors), c -> member(a,c));
);

-----------------------------------------
--Colorful System of Parameters  
-----------------------------------------

colorfulSOP = method(TypicalValue => List)
colorfulSOP (SimplicialComplex,ZZ) := (B,n) -> (
   R       := ring(B);
   colors  := new MutableList from splice { dim(B)+1:{} };
   H       := gens R;
   colors#0 = H_{1..n};

   ColorsNotAllowed := {0};
   AlreadyColored   := splice{1..#colors#0};
   NotColored       := splice{#colors#0+1..#faces(0,B)};
   
   --Gives a proper coloring to the graph of B
   for i from 1 to #colors#0 do (
      for c in NotColored do (
	 if isSubset({R_i*R_c}, faces(1,B))  then (
            for a in AlreadyColored do (
	       if isSubset({R_c*R_a}, faces(1,B)) then (
                  ColorOfVertA := vertexColor(R_a,colors);
                     if not member(ColorOfVertA,ColorsNotAllowed) then (
                        ColorsNotAllowed = append(ColorsNotAllowed,ColorOfVertA);
                     );
               );
             );
         );
         m  := max(ColorsNotAllowed) + 1;
         colors#m       = append(colors#m,R_c);
         AlreadyColored = append(AlreadyColored,c);
         NotColored     = delete(c,NotColored);
      );
   );
   colors = apply(colors, c->sum(c));
   return toList(colors);
);


colorfulSOP (SimplicialComplex) := (D) -> (
   B0      := barycentricSubdivision(D);
   R       := ring(B0);
   B       := faceDelete(R_0,B0);
   n       := #faces(0,D);
   colors  := new MutableList from splice { dim(B)+1:{} };
   H       := gens R;
   colors#0 = H_{1..n};

   ColorsNotAllowed := {0};
   AlreadyColored   := splice{1..#colors#0};
   NotColored       := splice{#colors#0+1..#faces(0,B)};

   --Gives a proper coloring to the graph of B
   for i from 1 to #colors#0 do (
      for c in NotColored do (
	 if isSubset({R_i*R_c}, faces(1,B)) then (
            for a in AlreadyColored do (
	       if isSubset({R_c*R_a}, faces(1,B)) then (
                  ColorOfVertA := vertexColor(R_a,colors);
                     if not member(ColorOfVertA,ColorsNotAllowed) then (
                        ColorsNotAllowed = append(ColorsNotAllowed,ColorOfVertA);
                     );
               );
             );
         );

         m  := max(ColorsNotAllowed) + 1;
         colors#m       = append(colors#m,R_c);
         AlreadyColored = append(AlreadyColored,c);
         NotColored     = delete(c,NotColored);
      );
   );
   

   colors = apply(colors, c->sum(c));
   return toList(colors);
);
-----------------------------------------
--Main Function for Colorful Version
-----------------------------------------

colorfulPresentation = method(TypicalValue => Module)
colorfulPresentation (SimplicialComplex) := D -> (
   B0        := barycentricSubdivision(D);
   STrivial  := ring(B0);
   B         := faceDelete(STrivial_0,B0);
   r         := dim(D)+1;
   R         := coefficientRing(STrivial)[vars(1..r), Degrees => splice{1..r}];

   -----Constructs graded balanced polynomial ring-----
   seqOfDegrees := ();
   for i from 1 to #gens(STrivial) do (
       seqOfDegrees = join(seqOfDegrees,(#faces(i-2,D):i-1));
   );

   ListOfDegs    := toList(seqOfDegrees);
   S             := newRing(STrivial, Degrees => ListOfDegs);

   -----psi induces the grading on all ring related elements-----
   psi           := map(S,STrivial);
   I             := psi(ideal(B));
   NonGradedKSOP := colorfulSOP(B,#faces(0,D));
   KSOP          := {};
   for k in NonGradedKSOP do (
       KSOP = append(KSOP,psi(k));
   );

   phi       := map(S, R, matrix { KSOP });
   MB        := pushForward(phi, S^1/I);
   return(MB);
);

colorfulPresentation (List) := F -> (
   D         := simplicialComplex(F);
   B0        := barycentricSubdivision(D);
   STrivial  := ring(B0);
   B         := faceDelete(STrivial_0,B0);
   r         := dim(D)+1;
   R         := coefficientRing(STrivial)[vars(1..r), Degrees => splice{1..r}];

   -----Constructs graded balanced polynomial ring-----
   seqOfDegrees := ();
   for i from 1 to #gens(STrivial) do (
       seqOfDegrees = join(seqOfDegrees,(#faces(i-2,D):i-1));
   );

   ListOfDegs    := toList(seqOfDegrees);
   S             := newRing(STrivial, Degrees => ListOfDegs);

   -----psi induces the grading on all ring related elements-----
   psi           := map(S,STrivial);
   I             := psi(ideal(B));
   NonGradedKSOP := colorfulSOP(B,#faces(0,D));
   KSOP          := {};
   for k in NonGradedKSOP do (
       KSOP = append(KSOP,psi(k));
   );

   phi       := map(S, R, matrix { KSOP });
   MB        := pushForward(phi, S^1/I);
   return(MB);
);
---------------------------------------
--Universal System of Parameters (USOP)
---------------------------------------

universalSOP = method(TypicalValue => List)
universalSOP (SimplicialComplex) := D -> (
   Theta    := new MutableList from {};
   for i from 0 to dim(D) do ( 
     Theta#i = faces(i,D)
   );
   Theta     = apply(Theta, t->sum(t));
   return toList(Theta);
);

universalSOP (List) := F -> (
   D        := simplicialComplex(F);
   Theta    := new MutableList from {};
   for i from 0 to dim(D) do ( 
     Theta#i = faces(i,D)
   );
   Theta     = apply(Theta, t->sum(t));
   return toList(Theta);
);


----------------------------------------
--Standard Main Function
----------------------------------------

universalPresentation = method(TypicalValue => Module)
universalPresentation (List) := F -> (
   D        := simplicialComplex(F);
   S        := ring(D);
   r        := dim(D)+1;
   USOPRing := coefficientRing(ring(D))[vars(1..r), Degrees => splice{1..r}];
   phi      := map(S, USOPRing, matrix {universalSOP(D)});
   MD       := pushForward(phi,S^1/ideal(D));
   return(MD);
);

universalPresentation (SimplicialComplex) := D -> (
   S        := ring(D);
   r        := dim(D)+1;
   USOPRing := coefficientRing(ring(D))[vars(1..r), Degrees => splice{1..r}];
   phi      := map(S, USOPRing, matrix {universalSOP(D)});
   MD       := pushForward(phi,S^1/ideal(D));
   return(MD);
);
----------------------------------------
--Sum Betti numbers of resolutions over parameter rings
----------------------------------------

sumBetti = method(TypicalValue => ZZ)
sumBetti (Module) := M -> (
   return(rank(sum(res(M))));
);

----------------------------------------
--Comparison Function
----------------------------------------

equalBettiTally = method(TypicalValue => Boolean)
equalBettiTally (Module,Module) := (M,N) -> (
   return betti(M) == betti(N);
);
--equalBettiTally (SimplicialComplex) := D -> (
--   return betti(universalPresentation(D)) == betti(colorfulPresentation(D));
--);
--equalBettiTally (List) := F -> (
--   return betti(universalPresentation(F)) == betti(colorfulPresentation(simplicialComplex(F)));
--);


----------------------------------------
--Documentation
----------------------------------------
beginDocumentation()

doc /// 
    Key 
        ResolutionsOfStanleyReisnerRings
    Headline 
        Resolutions of Stanley-Reisner rings over certain parameter rings.
    Description
        Text
            {\em ResolutionsOfStanleyReisnerRings} is a package for computing certain systems of parameters and finding the Betti numbers of the resolution of the Stanley-Reisner ring of a simplicial complex over certain parameter rings.

            Given a simplicial complex  {\tt D}, there is a {\em universal system of parameters} (USOP) [AR,GS,HM,S] where each parameter {\tt i}  is a sum of the faces of {\tt D} of dimension {\tt i+1}. The {\em colorful system of parameters} (KSOP) is obtained by giving a proper d-vertex coloring to a balanced simplicial complex {\tt B} of dimension {\tt d-1},  and then summing over the colors so that each parameter {\tt j}  is the sum of vertices of color {\tt j}. This package includes routines for computing both USOP and KSOP for simplicial complexes and for computing the graded Betti numbers of the resolutions of the Stanley-Reisner rings over the graded parameter rings.

            References:

            [AR] A. Adams and V. Reiner, A Colorful Hochster Formula and Universal Parameters for Face Rings. Preprint, 2020; arXiv:2007.13021.

            [GS] A.M. Garsia and D. Stanton, Group actions of Stanley-Reisner rings and invariants of permutation groups. Adv. in Math. 51 (1984), 107–201.

            [HM] J. Herzog and S. Moradi, Systems of parameters and the Cohen--Macaulay property. Preprint, 2020; arXiv:2006.16549.

            [S]  D.E. Smith, On the Cohen-Macaulay property in commutative algebra and simplicial topology. Pac. J. Math. 141 (1990), 165–196.

            The goal of this work was primarily to help compute examples to provide evidence for Conjecture 6.1 [AR]. I have tried to generalize most of the functionality to make it useful in other areas.  This is work in progress and many interesting pieces are still missing. All suggestions and contributions are welcome.
///


doc ///
    Key 
        barycentricSubdivision 
        (barycentricSubdivision,SimplicialComplex)
        (barycentricSubdivision,List)
    Headline
        the barycentric subdivision of a simplicial complex. 
    Usage 
         barycentricSubdivision D
         barycentricSubdivision F
    Inputs
        D:SimplicialComplex
        F:List
            a list of facets defining a simplicial complex
    Outputs
        :SimplicialComplex
            the barycentric subdivision of a simplicial complex of {\tt D}
    Description
        Text
            The following example uses the simplicial complex on four vertices composed of two disconnected edges.
        Example
            S = QQ[a,b,c,d];
            F = {a*b,c*d};
            D = simplicialComplex F
            barycentricSubdivision D            
            barycentricSubdivision F            
    SeeAlso
        SimplicialComplexes
        Posets 
        SimplicialDecomposability
///

doc ///
    Key 
        colorfulSOP
        (colorfulSOP,SimplicialComplex,ZZ)
        (colorfulSOP,SimplicialComplex)
    Headline
        Colorful System of Parameters (KSOP)
    Usage 
        colorfulSOP(B,n)
        colorfulSOP(D)
    Inputs
        B:SimplicialComplex
            the barycentric subdivision of a simplicial complex {\tt D}
        n:ZZ
            the number of vertices of {\tt D}
        D:SimplicialComplex
            any simplicial complex for which you want the KSOP for its barycentric subdivision
    Outputs
        :List
            a list of sums of ring elements of the polynomial ring of {\tt B}
    Description
        Text
            The barycentric subdivision of a simplicial complex is a balanced simplicial complex, and so in this version we require the input of the barycentric subdivision of a simplicial complex.

            The examples use the simplicial complex {\tt D} on five vertices consisting of an isolated vertex and triangle of dimension 2 attached to an edge. The first example uses the barycentric subdivision of {\tt D}. When considering the barycentric subdivision of a simplicial complex it is often useful to remove the minimal element of its face poset. In the next example, we demonstrate how this can be accomplished.
        Example
            S = QQ[a..e];
            D = simplicialComplex {a*b*c,c*d,e};
            n = # vertices D
            ComplexIncludingMinElt = barycentricSubdivision D;
            R = ring(ComplexIncludingMinElt);
            B = faceDelete(R_0,ComplexIncludingMinElt);
            colorfulSOP(B,n)
        Text
            Also included is the option to input any simplicial complex {\tt D} in order to obtain the colorful system of parameters for the face ring of the corresponding barycentric subdivision of {\tt D}.
        Example
            S = QQ[a..e];
            D = simplicialComplex {a*b*c,c*d,e};
            colorfulSOP(D)
    Caveat
         This current version requires the input of the barycentric subdivision of a simplicial complex {\tt D} without the minimal element of the face poset, which corresponds to the empty set of {\tt D}. This must be accomplished by using using the {\tt barycentricSubdivision} method, included in this package. It also has the option of inputting any simplicial complex. The system then takes the barycentric subdivision of {\tt D} and removes its minimal element in order to obtain the colorful system of parameters. 
    SeeAlso
        barycentricSubdivision
        SimplicialComplexes
        Posets 
        SimplicialDecomposability
///

doc ///
    Key
        universalSOP
        (universalSOP,SimplicialComplex)
        (universalSOP,List)
    Headline
        Universal System of Parameters (USOP)
    Usage 
        universalSOP D
        universalSOP F
    Inputs
        D:SimplicialComplex
        F:List
            a list of facets defining a simplicial complex
    Outputs
        :List
         a list of elements from the Stanley-Reisner ring of {\tt D}
    Description
        Text
            The following example uses the simplicial complex consisting of a triangle of dimension 2 attached to an edge and a isolated vertex.
        Example
            S = ZZ/2[a..e];
            F = {a*b*c,c*d,e}
            D = simplicialComplex F
            universalSOP D
            universalSOP F
    SeeAlso
        SimplicialComplexes
/// 

doc ///
    Key
        sumBetti
        (sumBetti,Module)
    Headline
        Computes the sum of Betti numbers.
    Usage
        sumBetti M
    Inputs
        M:Module
    Outputs
         :ZZ
             sum of the Betti numbers of the resolution of {\tt M} 
    Description 
        Example
            S = QQ[a..f];
            m = matrix{{a,b,d,e},{b,c,e,f}} 
            M = coker m
            sumBetti M
    SeeAlso
        Module 
///

doc ///
    Key
        equalBettiTally
        (equalBettiTally,Module,Module)
    Headline
        Computes the Betti table of two modules and compares for equality 
    Usage
        equalBettiTally(M,N)
    Inputs
        M:Module
        N:Module
    Outputs
        :Boolean
    Description 
        Example
            S = QQ[a..f];
            m = matrix{{a,b,d,e},{b,c,e,f}} 
            M = coker m
            n = matrix{{a,b},{c,d},{e,f}}
            N = coker n
            equalBettiTally(M,N)
    SeeAlso
        Module 
///

doc ///
      Key
        universalPresentation
        (universalPresentation,SimplicialComplex)
        (universalPresentation,List)
    Headline
        A presentation of the Stanley-Reisner ring over its universal parameter ring
    Usage
        universalPresentation D 
        universalPresentation F 
    Inputs
        D:SimplicialComplex
        F:List
            A list of facets defining a simplicial complex {\tt D}
    Outputs
        :Module 
            A graded presentation of the Stanley-Reisner ring over its universal parameter ring
    Description 
        Text
            Note that {\tt M} is N-graded.
        Example
		        S = QQ[a..e];
            F = {a*b*c,c*d,e}
            D = simplicialComplex F
            universalPresentation D 
            M = universalPresentation F 
            degrees M
    SeeAlso
        Module 
        universalSOP
///

doc ///
      Key
        colorfulPresentation
        (colorfulPresentation,SimplicialComplex)
        (colorfulPresentation,List)
    Headline
        A presentation of the Stanley-Reisner ring of the barycentric subdivision of a simplicial complex over its colorful parameter ring
    Usage
        colorfulPresentation D 
        colorfulPresentation F 
    Inputs
        D:SimplicialComplex
        F:List
            A list of facets defining a simplicial complex {\tt D}
    Outputs
        :Module 
            A graded presentation of the Stanley-Reisner ring over its universal parameter ring
    Description 
        Text
            Since the barycentric subdivision of a simplicial complex {\tt D} is a balanced simplicial complex {\tt B}, i.e., there exists a {\tt dim(D)+1} proper vertex coloring, this {\tt colorfulPresentation} takes in the barycentric subdivision of a simplicial complex, computes a colorful system of parameters for the Stanley-Reisner ring of {\tt B} and then returns this quotient ring as an N-graded module over the colorful parameter ring. 
        Example
	    S = QQ[a..e];
            F = {a*b*c,c*d,e}
            D = simplicialComplex F
            colorfulPresentation D 
            M = colorfulPresentation F 
            degrees M 
    SeeAlso
        Module 
        colorfulSOP
///

-------------------
-- Tests
-------------------

--colorfulSOP,universalSOP,colorfulPresentation,universalPresentation,barycentricSubdivision,sumBetti,equalBettiTally

-- Tests barycentricSubdivision 
TEST ///
    S = QQ[a,b,c];
    B = barycentricSubdivision simplicialComplex {a*b,c};
    R = ring(B)
    assert(B === simplicialComplex  {v_0*v_2*v_4, v_0*v_1*v_4, v_0*v_3});
    S = QQ[a..e];
    B = barycentricSubdivision simplicialComplex {a*b*c,c*d,e};
    R = ring(B)
    assert(B === simplicialComplex  {v_0*v_3*v_8*v_10,v_0*v_2*v_8*v_10,v_0*v_3*v_7*v_10,v_0*v_1*v_7*v_10,v_0*v_2*v_6*v_10,v_0*v_1*v_6*v_10,v_0*v_4*v_9,v_0*v_3*v_9,v_0*v_5});
///

-- Tests universalSOP
TEST ///
    S = QQ[a,b,c];
    assert(universalSOP simplicialComplex {a*b,c} === {a+b+c,a*b});
    S = QQ[a..e];
    assert(universalSOP simplicialComplex {a*b*c,c*d,e} === {a+b+c+d+e,a*b+a*c+b*c+c*d,a*b*c});
///

-- Tests colorfulSOP
TEST ///
   S         = QQ[a,b,c];
   B0        = barycentricSubdivision(simplicialComplex {a*b,c});
   STrivial  = ring(B0);
   B         = faceDelete(STrivial_0,B0);
   assert(colorfulSOP(B,3) === {v_1+v_2+v_3,v_4});
   S         = QQ[a..e];
   B0        = barycentricSubdivision(simplicialComplex {a*b*c,c*d,e});
   STrivial  = ring(B0);
   B         = faceDelete(STrivial_0,B0);
   assert(colorfulSOP(B,5) === {v_1+v_2+v_3+v_4+v_5,v_6+v_7+v_8+v_9,v_10});
///

-- Tests equalBettiTally 
TEST ///
    S = QQ[a..f];
    m = matrix{{a,b,d,e},{b,c,e,f}}
    n = matrix{{a,b},{c,d},{e,f}}
    assert(equalBettiTally(coker m,coker n) === false);
    n = matrix{{a,b,d,e},{b,c,e,f}}
    assert(equalBettiTally(coker m,coker n));
///
end

-- Tests sumBetti
TEST ///
   S = QQ[a..f];
   assert(sumBetti(coker matrix{{a,b,d,e},{b,c,e,f}}) === 12);
   assert(sumBetti(coker matrix{{a,b},{c,d},{e,f}} === 5);
   S = QQ[a,b,c];
   assert(sumBetti(coker matrix{{a},{b}}) == 3);
///


-- Tests universalPresentation
TEST ///
   S = QQ[a,b,c];
   D = simplicialComplex {a*b,c} 
   M = universalPresentation D;
   f = map(target generators M, source relations M, matrix{{0},{0},{c}})
  c assert(relations M === f);
///


-- Tests colorfulPresentation
TEST ///
   S = QQ[a,b,c];
   D = simplicialComplex {a*b,c} 
   M = colorfulPresentation D;
   f = map(target generators M, source relations M, matrix{{0},{0},{c}})
   assert(relations M === f);
///

