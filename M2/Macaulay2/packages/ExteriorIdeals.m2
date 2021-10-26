-- -*- coding: utf-8 -*-
newPackage(
        "ExteriorIdeals",
        Version => "1.0", 
        Date => "July 13, 2017",
        Authors => {{Name => "Marilena Crupi", Email => "mcrupi@unime.it", HomePage => "http://www.unime.it/it/persona/marilena-crupi"},
                    {Name => "Luca Amata", Email => "lamata@unime.it"}},
        Headline => "monomial ideals over exterior algebras",
	Keywords => {"Commutative Algebra"},
        DebuggingMode => false,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "ExteriorIdeals: a package for computing monomial ideals in an exterior algebra",
	     "acceptance date" => "24 June 2018",
	     "published article URI" => "https://msp.org/jsag/2018/8-1/p07.xhtml",
	     "published article DOI" => "10.2140/jsag.2018.8.71",
	     "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x07-ExteriorIdeals.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/ExteriorIdeals.m2",
	     "release at publication" => "1073789664ba1f00096121613a8b6d932a0e5c4e",	    -- git commit number in hex
	     "version at publication" => "1.0",
	     "volume number" => "8",
	     "volume URI" => "https://msp.org/jsag/2018/8-1/"
	     }
        )

export {
"macaulayExpansion", "solveMacaulayExpansion", "initialDegree", "hilbertSequence", "isLexIdeal", "isHilbertSequence", "lexIdeal", "allHilbertSequences", "isStronglyStableIdeal", "stronglyStableIdeal", "isStableIdeal", "stableIdeal",
--options
"Shift"
}

------------------------------------------------------------------------
-- Compute the i-th Macaulay expansion of a (Shift=>false)
-- Compute the i-th shifted Macaulay expansion of a (Shift=>true)
------------------------------------------------------------------------
macaulayExpansion = method(TypicalValue=>List, Options=>{Shift=>false})
macaulayExpansion(ZZ,ZZ) := opts -> (a,i) -> (
    macexp:={};
    dw:=i;
    up:=dw;
    if opts.Shift and a==0 then macexp={{0,2}};

    while a>0 and i>0 do (
	    while binomial(up,dw)<a do up=up+1;
		
     	if binomial(up,dw)!=a then up=up-1;
		
	    if not opts.Shift then macexp=append(macexp,{up,dw})
        else macexp=append(macexp,{up,dw+1});
	    a=a-binomial(up,dw);
	    dw=dw-1;
	    up=dw;
    );
    macexp
)


------------------------------------------------------------------------
-- Compute the sum of a Macaulay expansion.
------------------------------------------------------------------------
solveMacaulayExpansion = method(TypicalValue=>ZZ)
solveMacaulayExpansion List := l -> sum apply(l,x->binomial(toSequence x))


------------------------------------------------------------------------
-- Compute the initial degree of a graded ideal
------------------------------------------------------------------------
initialDegree = method(TypicalValue=>ZZ)
initialDegree Ideal := I -> (
    if isHomogeneous I then return min flatten degrees ideal mingens I
	else error "expected a graded ideal";
    )

------------------------------------------------------------------------
-- Compute the Hilbert sequence of E/I
------------------------------------------------------------------------
hilbertSequence = method(TypicalValue=>List)
hilbertSequence Ideal := I -> (
    E:=ring I;
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";	
    n:=#flatten entries vars E;

    for k to n list hilbertFunction(k,I)
    )


---------------------------------------------------------------------
-- whether an ideal is lex
-- Taken from Chris Francisco's package: "LexIdeals"
---------------------------------------------------------------------
isLexIdeal = method(TypicalValue=>Boolean)
isLexIdeal Ideal := I -> (
    E:=ring I;
    I=ideal mingens I;
	EL:=newRing(E, MonomialOrder=>Lex);
	RM:=map(EL,E);
    degs:=sort flatten degrees I;

    if isMonomialIdeal I then
        for k from degs#0 to last degs do (
	        m:=ideal vars EL;
	        genI:=flatten entries super basis(k,I);
     	    genLex:=take(unique flatten entries compress gens m^k,#genI);
	        if RM ideal genI!=ideal genLex then return false;
        )
    else error "expected a monomial ideal";
     
	true
    )
	

------------------------------------------------------------------------
-- Compute the shadow of a monomial in an exterior algebra.
------------------------------------------------------------------------
shadowMon = method(TypicalValue=>Set)
shadowMon RingElement := mon -> (
    E:=ring mon;
    n:=(#flatten entries vars E)-1;    

    sh:={};
    if #(flatten entries monomials mon)==1 then 
        sh=select(for k to n list (product support(mon*E_k))_E,x->x!=1);
    sh
)


------------------------------------------------------------------------
-- Compute the shadow of a set of monomials in an exterior algebra.
------------------------------------------------------------------------
shadowSet = method(TypicalValue=>Set)
shadowSet List := l -> set flatten apply(l,x->shadowMon x)

------------------------------------------------------------------------
-- whether the Kruskal-Katona theorem is satisfied
-- That is, the input sequence is an Hilbert sequence
------------------------------------------------------------------------
isHilbertSequence = method(TypicalValue=>Boolean)
isHilbertSequence(List,Ring) := (l,E) -> (
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";
    n:=#flatten entries vars E;	 
	l=join(l,(n+1-#l):0);
	 
    if l#0!=1 or l#1>n or #l>n+1 then return false
	else (     
        for k from 1 to #l-2 do (
            val:=solveMacaulayExpansion macaulayExpansion(l#k,k,Shift=>true);
            if l#(k+1)<0 or l#(k+1)>val then return false
		)
    );

    true
    )

----------------------------------------------------------------------------------
-- Compute the lex ideal with the given Hilbert sequence in an exterior algebra
----------------------------------------------------------------------------------
lexIdeal = method(TypicalValue=>Ideal)

lexIdeal(List, Ring) := (hs,E) -> (
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";
	EL:=newRing(E, MonomialOrder=>Lex);
	IRM:=map(E,EL);
    n:=#flatten entries vars EL;
    ind:=0;
    l:={};
    m:=ideal vars EL;
    ltempold:={};
      
    if isHilbertSequence(hs,EL) then (
        hs=join(hs,(n+1-#hs):0);
     
        for k from 1 to n do (
            completeHomLex:=rsort(take(unique flatten entries compress mingens m^k, binomial(n,k)));
            ind=#completeHomLex-hs#k;
            ltemp:=take(completeHomLex,ind);
            ltemp=rsort toList(set ltemp - shadowSet ltempold);
            l=flatten append(l,ltemp);
            ltempold=take(completeHomLex,ind);
        );
        if #l==0 then l={0_EL};
    ) else error "expected a Hilbert sequence";

    if #l>0 then IRM ideal l
    )

------------------------------------------------------------------------
-- Compute the lex ideal with the same Hilbert sequence of I
------------------------------------------------------------------------
lexIdeal Ideal := I -> lexIdeal(hilbertSequence I,ring I)  


------------------------------------------------------------------------
-- Compute all Hilbert sequences of quotients of an exterior algebra
------------------------------------------------------------------------
allHilbertSequences = method(TypicalValue=>List)
allHilbertSequences Ring := E -> (
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";
    n:=#flatten entries vars E;
    l:={};
    seq:=new MutableList from for k to n list binomial(n,k);
      
    while seq#0>0 do (
        i:=n;
        while i>0 and seq#i<0 do (
            i=i-1;
            seq#i=seq#i-1; 
        );
        while i<n do (
            seq#(i+1)=solveMacaulayExpansion macaulayExpansion(seq#i,i,Shift=>true);          
            i=i+1;
        );      
        if isHilbertSequence(toList seq,E) then l=append(l,toList seq);

        seq#n=seq#n-1;
    );   

    l 
    )


----------------------------------------------------------------------------
-- whether an ideal in an exterior algebra is strongly stable
----------------------------------------------------------------------------
isStronglyStableIdeal = method(TypicalValue=>Boolean)
isStronglyStableIdeal Ideal := I -> (
    E:=ring I;
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";	
    EL:=newRing(E, MonomialOrder=>Lex);
    RM:=map(EL,E);
    var:=flatten entries vars EL;     
    gen:=rsort flatten entries mingens RM I;
    IL:=ideal gen;
    ind:=0;

    if isMonomialIdeal ideal gen then
        while #gen>0 and gen!={1_EL} do (
            mon:=gen#0;
            m:=support mon / index //max;

            supp:=for x in support mon list index x;
            lmon:=apply(supp,k->((product rsort toList(set support mon-{EL_k}))_EL,k));		 
		 
            l:=flatten apply(lmon,y->(y#0)*apply(toList(0..(y#1)-1),k->EL_k));
            l=select(l / support / product, x->x!=1);          
            gen=join(gen,l);
            gen=rsort toList(set gen-{mon});          

            l=select(l / (x -> x%IL),x->x!=0);
            if #l>0 then return false; 
        )
    else error "expected a monomial ideal";

    true
    )


---------------------------------------------------------------------------------------------
-- Compute the smallest strongly stable ideal in an exterior algebra that contains I
----------------------------------------------------------------------------------------------
stronglyStableIdeal = method(TypicalValue=>Ideal)
stronglyStableIdeal Ideal := I -> (
    E:=ring I;
	if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";
    EL:=newRing(E, MonomialOrder=>Lex);
	RM:=map(EL,E);
	IRM:=map(E,EL);
    var:=flatten entries vars EL;     
    gen:=rsort flatten entries mingens RM I;
    ind:=0;
    newgen:=gen;
      
    if isMonomialIdeal ideal gen then (
        while #gen>0 and gen!={1_EL} do (
            mon:=gen#0;
            m:=support mon / index //max;

            supp:=for x in support mon list index x;
		    lmon:=apply(supp,k->((product rsort toList(set support mon-{EL_k}))_EL,k));

            l:=flatten apply(lmon,y->(y#0)*apply(toList(0..(y#1)-1),k->EL_k));
            l=select(l / support / product, x->x!=1);
            gen=join(gen,l);
            gen=rsort toList(set gen-{mon});
         
            newgen=unique join(newgen,l);
        );
		newgen=rsort flatten entries mingens ideal newgen;
    ) else error "expected a monomial ideal";
    
    IRM ideal newgen
    )


----------------------------------------------------------------------------
-- whether an ideal in an exterior algebra is stable
----------------------------------------------------------------------------
isStableIdeal = method(TypicalValue=>Boolean)
isStableIdeal Ideal := I -> (
    E:=ring I;
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";	
    EL:=newRing(E, MonomialOrder=>Lex);
	RM:=map(EL,E);
    var:=flatten entries vars EL;     
    gen:=rsort flatten entries mingens RM I;
    IL:=ideal gen;
    ind:=0;

    if isMonomialIdeal ideal gen then
        while #gen>0 and gen!={1_EL} do (
            mon:=gen#0;
            m:=support mon / index //max;
            mon=(product rsort toList(set support mon-{EL_m}))_EL;            
			
            l:=mon*apply(toList(0..m-1),k->EL_k);
            l=select(l / support / product, x->x!=1); 
            gen=join(gen,l);
            gen=rsort toList(set gen-{mon*EL_m});          

            l=select(l / (x -> x%IL),x->x!=0);
            if #l>0 then return false; 
        )
    else error "expected a monomial ideal";

    true
    )


-------------------------------------------------------------------------------------------
-- Compute the smallest stable ideal in an exterior algebra that contains I
----------------------------------------------------------------------------------------------
stableIdeal = method(TypicalValue=>Ideal)
stableIdeal Ideal := I -> (
    E:=ring I;
    if (options E).SkewCommutative!=flatten entries vars E / index then error "expected an exterior algebra as polynomial ring";	
    EL:=newRing(E, MonomialOrder=>Lex);
	RM:=map(EL,E);
	IRM:=map(E,EL);
    var:=flatten entries vars EL;     
    gen:=rsort flatten entries mingens RM I;
    ind:=0;
    newgen:=gen;

    if isMonomialIdeal ideal gen then (
        while #gen>0 and gen!={1_EL} do (
            mon:=gen#0;
            m:=support mon / index //max;
            mon=(product rsort toList(set support mon-{EL_m}))_EL;

            l:=mon*apply(toList(0..m-1),k->EL_k);
            l=select(l / support / product, x->x!=1);          
            gen=join(gen,l);         
            gen=rsort toList(set gen-{mon*EL_m});
            newgen=unique join(newgen,l);
        );
	    newgen=rsort flatten entries mingens ideal newgen;     	
    ) else error "expected a monomial ideal";
	
    IRM ideal newgen  
    )



beginDocumentation()
-------------------------------------------------------
--DOCUMENTATION ExteriorIdeals
-------------------------------------------------------

document {
     Key => {ExteriorIdeals},
     Headline => "a package for working with ideals over exterior algebra",
     TT "ExteriorIdeals is a package for creating and manipulating ideals over exterior algebra",
     PARA {"Other acknowledgements:"},      
     "The method ", TT "isLexIdeal", " was taken from Chris Francisco's package: LexIdeals, which is available at ",
      HREF{"http://www2.macaulay2.com/Macaulay2/doc/Macaulay2-1.10/share/doc/Macaulay2/LexIdeals/html/","LexIdeals"}
	  
     }

document {
     Key => {macaulayExpansion,(macaulayExpansion,ZZ,ZZ)},
     Headline => "compute the Macaulay expansion of a positive integer",
     Usage => "macaulayExpansion(a,i)",
     Inputs => {"a" => {"a positive integer"},
                "i" => {"a positive integer"}
	  },
     Outputs => {List => {"pairs of positive integers representing the ", TT "i", "-th (shifted) Macaulay expansion of ", TT "a"}},  
     "Given a pair of positive integers ", TT "(a,i)", " there is a unique expression of ", TT "a", " as a sum of binomials ", TT " a=binomial(a_i,i) + binomial(a_{i-1},i-1) + ... + binomial(a_j,j)", " where ", TT " a_i > a_{i-1} > ... > a_j > j >= 1.",
      PARA {"Examples:"},
      EXAMPLE lines ///
           macaulayExpansion(8,4)
           macaulayExpansion(3,1,Shift=>false)
	       macaulayExpansion(8,4,Shift=>true)
           macaulayExpansion(3,1,Shift=>true)
         ///,
     SeeAlso =>{solveMacaulayExpansion}
     }

document {
     Key => {solveMacaulayExpansion,(solveMacaulayExpansion,List)},
     Headline => "compute the sum of a Macaulay expansion",
     Usage => "solveMacaulayExpansion l",
     Inputs => {"l" => {"a list of pairs of natural numbers representing a Macaulay expansion"}
	  },
     Outputs => {ZZ => {"representing the sum of binomials in the list", TT "l"}},
	 "Given a list of pairs ", TT "{{a_1,b_1}, ... ,{a_k,b_k}}", " this method yields the sum of binomials ", TT " binomial(a_1,b_1) + ... + binomial(a_k,b_k).", 
     PARA {"Example:"},
     EXAMPLE lines ///
	   solveMacaulayExpansion({{4,2},{3,1}})
	  ///,
     SeeAlso =>{macaulayExpansion}
     }

document {
     Key => {initialDegree,(initialDegree,Ideal)},
     Headline => "compute the initial degree of a graded ideal",
     Usage => "initialDegree I",
     Inputs => {"I" => {"a graded ideal"}
	  },
     Outputs => {ZZ => {"representing the initial degree of the ideal ", TT "I"}},
	 "The initial degree of a graded ideal ", TT "I", " is the least degree of a homogeneous generator of " , TT "I",
	 PARA {"Example:"},
     EXAMPLE lines ///
	     E=QQ[e_1..e_4,SkewCommutative=>true]
	     initialDegree ideal {e_1*e_2,e_2*e_3*e_4}
		 initialDegree ideal {e_1*e_3*e_4}
	  ///
     }

document {
     Key => {hilbertSequence,(hilbertSequence,Ideal)},
     Headline => "compute the Hilbert sequence of a given ideal in an exterior algebra",
     Usage => "hilbertSequence I",
     Inputs => {"I" => {"an ideal of an exterior algebra ", TT "E"}
	  },
     Outputs => {List => {"nonnegative integers representing the Hilbert sequence of the quotient ", TT "E/I"}},
	 "Given ", TT "sum{h_i t^i}, i=1..n", " the Hilbert series of a graded K-algebra ", TT "E/I", ", the sequence ", TT "(1, h_1, ...,  h_n)", " is called the Hilbert sequence of ", TT " E/I.",
	 PARA {"Example:"},
     EXAMPLE lines ///
	     E=QQ[e_1..e_4,SkewCommutative=>true]
	     hilbertSequence ideal {e_1*e_2,e_2*e_3*e_4}
		 hilbertSequence ideal {e_2*e_3*e_4}
	  ///
     }

document {
     Key => {isLexIdeal,(isLexIdeal,Ideal)},
     Headline => "whether an ideal is lex",
     Usage => "isLexIdeal I",
     Inputs => {"I" => {"a monomial ideal of an exterior algebra"}
	  },
     Outputs => {Boolean => {"true whether ideal ", TT "I", " is lex"}},
      PARA {"Other acknowledgements:"},      
     "This method was taken from Chris Francisco's package: LexIdeals, which is available at ",
      HREF{"http://www2.macaulay2.com/Macaulay2/doc/Macaulay2-1.10/share/doc/Macaulay2/LexIdeals/html/","LexIdeals"},
	  
     PARA {"Examples:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       isLexIdeal ideal {e_1*e_2,e_2*e_3}
           isLexIdeal ideal {e_1*e_2,e_1*e_3,e_1*e_4,e_2*e_3}
     ///,
     SeeAlso =>{lexIdeal},
     }

document {
     Key => {isHilbertSequence,(isHilbertSequence,List,Ring)},
     Headline => "whether the given sequence is a Hilbert sequence",
     Usage => "isHilbertSequence(l,E)",
     Inputs => {"l" => {"a list of integers"},
		        "E" => {"an exterior algebra"}
	  },
     Outputs => {Boolean => {"true whether the sequence ", TT "l", " satisfies the Kruskal-Katona theorem in the exterior algebra ", TT "E"}},
     PARA {"Example:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       isHilbertSequence({1,4,3,1,0},E)
		   isHilbertSequence({1,4,3,1,1},E)
	  ///,
     SeeAlso =>{lexIdeal}
     }

document {
     Key => {lexIdeal,(lexIdeal,List,Ring),(lexIdeal,Ideal)},
     Headline => "compute the lex ideal with a given Hilbert function in an exterior algebra",
     Usage => "lexIdeal(hs,E) or lexIdeal I",
     Inputs => {"hs" => {"a list of integers"},
                "E" => {"an exterior algebra"},
                "I" => {"an ideal of an exterior algebra"}
	  },
     Outputs => {Ideal => {"the lex ideal with Hilbert sequence ", TT "hs", " or the lex ideal with the same Hilbert sequence of ", TT "I"}},
     PARA {"Examples:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       lexIdeal({1,4,3,1,0},E) 
           Ilex=lexIdeal ideal {e_1*e_2,e_2*e_3}
           isLexIdeal Ilex
	  ///,
     SeeAlso =>{isLexIdeal,isHilbertSequence}
     }

document {
     Key => {allHilbertSequences,(allHilbertSequences,Ring)},
     Headline => "compute all Hilbert sequences of quotients in an exterior algebra",
     Usage => "allHilbertSequences E",
     Inputs => {"E" => {"an exterior algebra"}   
	  },
     Outputs => {List => {"all Hilbert sequences of quotients of ", TT "E"}},
	 "A sequence is called a Hilbert sequence whether it satisfies the Kruskal-Katona theorem in the exterior algebra ", TT "E.",
     PARA {"Example:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       allHilbertSequences E
	  ///,
     SeeAlso =>{lexIdeal, isHilbertSequence}
     }

document {
     Key => {isStronglyStableIdeal,(isStronglyStableIdeal,Ideal)},
     Headline => "whether a monomial ideal in an exterior algebra is strongly stable",
     Usage => "isStronglyStableIdeal I",
     Inputs => {"I" => {"a monomial ideal of an exterior algebra"}
	  },
     Outputs => {Boolean => {"true whether ideal ", TT "I", " is strongly stable"}},
     PARA {"Examples:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       isStronglyStableIdeal ideal {e_2*e_3}
           isStronglyStableIdeal ideal {e_1*e_2,e_1*e_3,e_2*e_3}
	  ///,
     SeeAlso =>{stronglyStableIdeal},
     }

document {
     Key => {stronglyStableIdeal,(stronglyStableIdeal,Ideal)},
     Headline => "compute the smallest strongly stable ideal in an exterior algebra containing a given monomial ideal",
     Usage => "stronglyStableIdeal I",
     Inputs => {"I" => {"a monomial ideal of an exterior algebra"}
	  },
     Outputs => {Ideal => {"the smallest strongly stable ideal containing ", TT "I"}},
     PARA {"Example:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       stronglyStableIdeal ideal {e_2*e_3}
	  ///,
     SeeAlso =>{isStronglyStableIdeal}
     }

document {
     Key => {isStableIdeal,(isStableIdeal,Ideal)},
     Headline => "whether a monomial ideal in an exterior algebra is stable",
     Usage => "isStableIdeal I",
     Inputs => {"I" => {"a monomial ideal of an exterior algebra"}
	  },
     Outputs => {Boolean => {"true whether ideal ", TT "I", " is stable"}},
     PARA {"Examples:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       isStableIdeal ideal {e_2*e_3}
           isStableIdeal ideal {e_1*e_2,e_2*e_3}
	  ///,
     SeeAlso =>{stableIdeal},
     }

document {
     Key => {stableIdeal,(stableIdeal,Ideal)},
     Headline => "compute the smallest stable ideal in an exterior algebra containing a given monomial ideal",
     Usage => "stableIdeal I",
     Inputs => {"I" => {"a monomial ideal of an exterior algebra"}
	  },
     Outputs => {Ideal => {"the smallest stable ideal containing ideal ", TT "I"}},
     PARA {"Example:"},
     EXAMPLE lines ///
           E=QQ[e_1..e_4,SkewCommutative=>true]
	       stableIdeal ideal {e_2*e_3}
	  ///,
     SeeAlso =>{isStableIdeal}
     }

------------------------------------------------------------
-- DOCUMENTATION FOR OPTION
------------------------------------------------------------

----------------------------------
-- Shift (for macaulayExpansion)
----------------------------------

document {
     Key => {Shift,
	  [macaulayExpansion,Shift]},
     Headline => "optional argument for macaulayExpansion",
     "Whether it is true the function macaulayExpansion gives the ", TT "i", "-th shifted Macaulay expansion of ", TT "a.", " Given a pair of positive integers ", TT "(a,i)", " the ", TT "i","-th shifted Macaulay expansion is a sum of binomials: ", TT " binomial(a_i,i+1) + binomial(a_{i-1},i) + ... + binomial(a_j,j+1).",
     SeeAlso =>{macaulayExpansion} 
     }


------------------------------------------------------------
-- TESTS
------------------------------------------------------------

----------------------------
-- Test macaulayExpansion
----------------------------
TEST ///
assert(macaulayExpansion(8,2)=={{4,2},{2,1}})
assert(macaulayExpansion(5,6,Shift=>false)=={{6,6},{5,5},{4,4},{3,3},{2,2}})
assert(macaulayExpansion(8,4,Shift=>true)=={{5, 5}, {3, 4}, {2, 3}, {1, 2}})
assert(macaulayExpansion(3,1,Shift=>true)=={{3, 2}})
///

----------------------------
-- Test solveMacaulayExpansion
----------------------------
TEST ///
assert(solveMacaulayExpansion({{5, 5}, {3, 4}, {2, 3}, {1, 2}})==1)
assert(solveMacaulayExpansion({{6,6},{5,5},{4,4},{3,3},{2,2}})==5)
///

----------------------------
-- Test initialDegree
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(initialDegree ideal {e_1*e_2,e_1*e_2*e_3}==2)
assert(initialDegree ideal {e_1*e_2*e_3}==3)
///

----------------------------
-- Test hilbertSequence
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(hilbertSequence ideal {e_2*e_4}=={1,4,5,2,0})
///

----------------------------
-- Test isLexIdeal
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(not isLexIdeal ideal {e_1*e_2,e_2*e_3})
assert(isLexIdeal ideal {e_1*e_2,e_1*e_3,e_1*e_4,e_2*e_3})
///

----------------------------
-- Test isHilbertSequence
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(isHilbertSequence({1,4,3,1,0},E))
assert(not isHilbertSequence({1,4,3,2,0},E))
///

----------------------------
-- Test lexIdeal
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(lexIdeal({1,4,3,1,0},E)==ideal {e_1*e_2,e_1*e_3,e_1*e_4})

assert(lexIdeal ideal {e_1*e_2,e_2*e_3}==ideal {e_1*e_2,e_1*e_3})
assert(lexIdeal ideal {e_1*e_2,e_2*e_3*e_4}==ideal {e_1*e_2,e_1*e_3*e_4})
///

----------------------------
-- Test allHilbertSequences
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert( (allHilbertSequences E)#4=={1,4,6,1,0})
assert( (allHilbertSequences E)#(11)=={1,4,3,1,0})
///

----------------------------
-- Test isStronglyStableIdeal
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(not isStronglyStableIdeal ideal {e_2*e_3})
assert(isStronglyStableIdeal ideal {e_1*e_2,e_1*e_3,e_2*e_3})
///

----------------------------
-- Test stronglyStableIdeal
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(stronglyStableIdeal ideal {e_2*e_3}==ideal {e_1*e_2,e_1*e_3,e_2*e_3})
assert(stronglyStableIdeal ideal {e_1*e_2*e_3*e_4}==ideal {e_1*e_2*e_3*e_4})
///

----------------------------
-- Test isStableIdeal
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(not isStableIdeal ideal {e_2*e_3})
assert(isStableIdeal ideal {e_1*e_2,e_2*e_3})
///

----------------------------
-- Test stableIdeal
----------------------------
TEST ///
E=QQ[e_1..e_4,SkewCommutative=>true]
assert(stableIdeal ideal {e_2*e_3}==ideal {e_1*e_2,e_2*e_3})
assert(stableIdeal ideal {e_1*e_2*e_3*e_4}==ideal {e_1*e_2*e_3*e_4})
///

end

restart
installPackage ("ExteriorIdeals", UserMode=>true)
loadPackage "ExteriorIdeals"
viewHelp
