newPackage(
    "KoszulFiltrations",
    Version => "1.0",  
    Date => "August, 2026",
    Authors => {
	{Name => "Emily Berghofer", Email => "emily.berghofer@mdu.se",
	    HomePage=>"https://emily-berghofer.github.io"},
	{Name => "Lisa Nicklasson", Email => "lisa.nicklasson@mdu.se",
	    HomePage=>"https://www.mdu.se/en/malardalen-university/staff?id=lisa.nicklasson"},
	{Name => "Peder Thompson", Email => "peder.thompson@mdu.se",
	    HomePage=>"https://sites.google.com/view/pederthompson"},
	{Name => "Thomas Westerbäck", Email => "thomas.westerback@mdu.se",
	    HomePage=>"https://www.mdu.se/en/malardalen-university/staff?id=thomas.westerback"}},
    Headline => "Computational tools for Koszul filtrations",
    Keywords => {"Commutative Algebra"},
    DebuggingMode => false
    )

export {"partialLinearFiltration",
      "partialKoszulFiltration",
      "findKoszulFiltration",
       "partialLinearFlags",
       "sortIdealList",
       "isKoszulFiltration"}



--******************************************--
-- Functions    	       	    	    -- 
--******************************************--

partialLinearFiltration = method(TypicalValue=> List)
partialLinearFiltration (Ring,List) := (R,L) -> (
    if all(L,i->instance(i,R)) and all(L, i-> degree i == {1} and isHomogeneous(i))   then(
    	linearIdeals := unique apply(subsets L, a->trim promote(ideal(a),R));
    	F := {{promote(ideal(),R)}};
    	newIdeals := {};
    	colIdeal :={};
    	toInclude :={};
	m := trim ideal gens R;
	e := numgens m;
    	for i from 1 to e do (
            newIdeals := {};
            for I in F#(i-1) do ( 
            	for a in L do (
                    if isSubset(ideal(a), I) then continue;
                    colIdeal = trim (I:a);
                    if member(colIdeal,linearIdeals) then (
                    	toInclude = trim(I+ideal(a));
                    	newIdeals = unique append(newIdeals,toInclude);
                    	);
                    );
            	);
            F = append(F,newIdeals);
            );
    	return F;
    	)
    else(
    	return error " expected second entry to be a list of linear forms");
    );
          




partialKoszulFiltration = method(TypicalValue=> List) 
partialKoszulFiltration(Ring,List):= (R,L) -> (
       LF:=sortIdealList flatten L;
       if all(flatten LF, I ->
    instance(I,Ideal) and
    isHomogeneous(I) and
    (
        I == ideal(0_R) or
        all(flatten entries gens trim I, f -> degree f == {1})
    )
) then (
    
	    F := {}; Fprev := {};
	    for i from 0 to #LF-1 do (
    		F = append(F,unique apply(LF#(i),I->trim I));
    		);
	    while F =!= Fprev do (
    		Ftrim := {{promote(ideal(),R)}};
    		for i from 1 to #F-1 do (
		    FtrimLevel := {};
		    for I1 in F#(i-1) do (
	    		for I2 in F#(i) do (
			    if isSubset(I1,I2) then (
		    		colIdeal := trim (I1 : I2); 
		    		n:=numgens colIdeal;
		    		if colIdeal == ideal(0_R) or member(colIdeal,F#(n)) then (
				    FtrimLevel = append(FtrimLevel, I2);
				    );
		    		);
			    );
	    		);	  
		    FtrimLevel = unique FtrimLevel;
		    Ftrim = append(Ftrim,FtrimLevel);
		    );
    		Fprev = F; F = Ftrim;
    		);
	    return unique F;
	    )else(
	    return error "expected entries in the lists within the second entry to be linear ideals"
	    );
	);

        	
        

findKoszulFiltration = method(TypicalValue=>List)
findKoszulFiltration (Ring, List) := (R, L) -> (

LF:= partialLinearFiltration(R,L);
KF:= partialKoszulFiltration(R,LF);
KF= delete({},KF);
m:= trim ideal gens R;
e:= numgens m;
if #KF== e+1 then (	
    return flatten KF
    )
else (return error "no Koszul filtration exists within your list of generators");
);


partialLinearFlags = method(TypicalValue=>List)
partialLinearFlags (Ring,List,Ideal):= (R,L,I)  -> (
    if(unique degrees trim I=={{1}}) and isHomogeneous(I) then(
    	if all(L, i-> instance(i,R) and degree i =={1} and isHomogeneous(i)) then(
	    linearIdeals := unique apply(subsets L, a->trim promote(ideal(a),R));
	    Flags := {{I}}; i := numgens trim I;
	    while  i > 0 and #Flags =!= 0 do (
	    	Flagsnew := {}; i = i - 1;    
	    	for F in Flags do (
		    for a in linearIdeals do (
		    	if numgens trim a == i and isSubset(a, F#-1) then (
			    colIdeal := a:(F#-1);
			    if isSubset(unique degrees trim colIdeal,{{},{1}}) then (
			    	Fnew := append(F,a);
			    	Flagsnew = append(Flagsnew,Fnew);
			    	);
			    ); 
		     	);
		    );
	     	Flags = Flagsnew;
	     	);
	    return Flags
	    ) else (
	    return error " expected second entry to be a list of linear forms"
	    );
     	)else(
     	return error " ideal must be linear");
    );
     
     
     


sortIdealList= method(TypicalValue=> List)
sortIdealList List:= L->(
    if all( L,l-> instance(l, Ideal)) then (    
    	Lf:= flatten L;
    	Lt:= apply(Lf,i-> trim i);
    	nr:= unique apply(Lt,i-> numgens i);
    	n:= max nr;
    	F:= {};
    	for i from 0 to n do (
	    Li:= select(Lt,j-> numgens(j)==i);
	    F= F|{Li};
	    );        
    	return F
    	
    	)else(
    	return error "entries of the list must be ideals");
    );

isKoszulFiltration = method(TypicalValue=> Boolean)
isKoszulFiltration (Ring,List):= (R, KF) -> (
    L:= flatten KF;
    if not all(L, I -> instance(I,Ideal)) then (
        return error "expected second entry to be a list of ideals" 
    	);

if not all(L, I -> isHomogeneous(I)) then (
    return false;
    );

if not all(L, I -> (
        I == ideal(0_R) or
        all(flatten entries gens trim I, f -> degree f == {1})
    	)) then (
    return false;
    );
F := sortIdealList KF;
F= apply(F,f-> apply(f, i-> promote(i,R)));
F= delete({}, F);
m:= trim ideal gens R;
e:= numgens m;
if #F != (e + 1) then (
    return false;
    ) else (     Ftrim := {{promote(ideal(),R)}};
    for i from 1 to #F-1 do (
	FtrimLevel := {};
	for I1 in F#(i-1) do (
	    for I2 in F#(i) do (   
		if isSubset(I1,I2) then (
		    colIdeal := trim (I1 : I2); 
		    n:=numgens colIdeal;
		    if colIdeal == ideal(0_R) or member(colIdeal,F#(n)) then (
			FtrimLevel = append(FtrimLevel, I2);
			);
		    );
		);
	    );
	FtrimLevel = unique FtrimLevel;
	Ftrim = append(Ftrim,FtrimLevel);
	);
            
    	 return (set flatten Ftrim === set flatten F);	
    	    );
        
    	);


--******************************************--
-- DOCUMENTATION     	       	    	    -- 
--******************************************--


beginDocumentation( )

doc ///
  Key 
    KoszulFiltrations
  Headline 
    Package with computational tools for Koszul filtrations
  Subnodes
   partialLinearFiltration
   partialKoszulFiltration
   findKoszulFiltration
   partialLinearFlags
   sortIdealList
   isKoszulFiltration 
  Description
    Text
      KoszulFiltrations is a package for working with Koszul filtrations. 
      The package aids in finding them, confirming that a set of ideals forms one, and finding linear flags. 
      The package is largely based on algorithms from [BNTW26].


      Recall that the definition of a Koszul filtration as first defined by [CTV01] requires the following conditions for your list KF of ideals (in a standard graded algebra over a field):
    
      * The zero ideal and the maximal ideal are in KF.
      
      * All ideals in KF are generated by linear forms.
      
      * For every nonzero ideal I in KF there exists an ideal J in KF such that I/J is cyclic and J:I is in KF. 
    
    
    Text
      A linear filtration satisfies all the requirements of a Koszul filtration except that J:I is only required to be generated by linear forms.
      
      A partial Koszul filtration (respectively, partial linear filtration) fulfills all requirements of a Koszul filtration (respectively, linear filtration) except that the maximal ideal need not be included.
    
      A (partial) linear flag is a chain of ideals which is a (partial) linear filtration.  

      
    Text
      {\bf References}:
      
      [BNTW26] Emily Berghofer, Lisa Nicklasson, Peder Thompson and Thomas Westerbäck, Constructing Koszul filtrations: existence and non-existence for G-quadratic algebras, arXiv:2602.06490.
      
      [CTV01] Aldo Conca, Ngô Viêt Trung and Giuseppe Valla, Koszul property for points in projective spaces. Mathematica Scandinavica, 89 (2001), no.2, 201–216.
    Text
      Below is an example of how to use the main algorithm.
      
    Example
      R = QQ[x,y,z];
      L = {x,y,z};    
      findKoszulFiltration(R,L)

      ///

-----------------------------------------------
-- Documentation partialLinearFiltration    --
-----------------------------------------------
 
doc ///
  Key
    partialLinearFiltration
    (partialLinearFiltration, Ring, List)
  Headline 
     Constructing a partial linear filtration
  
  Usage
    partialLinearFiltration(R,L)
  Inputs
    R: Ring
    L: List
       A list of linear forms that you suspect could generate the ideals of a Koszul filtration.
  Outputs
    : List
       A list whose entries are lists of ideals sorted according to the number of generators. The ideals form a partial linear filtration.
  Description
    Text 
      The function takes as input a standard graded k-algebra R and a list L of the linear forms you suspect could generate the ideals of a Koszul filtration.
      It returns a partial linear filtration with ideals generated by forms in L. 
      The output is a list of lists where each inner list contains ideals with the same number of generators.
      
      Recall that a linear filtration satisfies all the requirements of a Koszul filtration except that J:I is only required to be generated by linear forms.
      
    Example 
      R = QQ[x,y,z];
      L = {x,y,z};
      partialLinearFiltration(R,L)
      
   
///

-----------------------------------------------
-- Documentation partialKoszulFiltration    --
-----------------------------------------------    


doc ///
    Key 
      partialKoszulFiltration
      (partialKoszulFiltration, Ring, List)
    Headline 
      Constructing a partial Koszul filtration
    Usage
      partialKoszulFiltration(R, LF)
      
    Inputs
      R: Ring
      LF: List
        A list of linear ideals. 
    Outputs
      : List
        A list whose entries are lists of ideals sorted by number of generators. 
	The ideals form a partial Koszul filtration.
    Description
      Text
        Input your standard graded k-algebra R and a list LF of ideals generated by linear forms in which you suspect that a Koszul filtration might be present. 
      Text  
	This list must have the format of the output of partialLinearFiltration, i.e., a list of lists where the ideals are sorted by number of generators. You can use the function sortIdealList to format your list in this way.
      Text
        The function returns a list of ideals. If the maximal ideal is not present in the list, then there does not exist a Koszul filtration with ideals generated by your chosen linear forms.
        If the maximal ideal is present in the list, then the output is a Koszul filtration.
        The output is maximal in the sense that it contains every possible Koszul filtration consisting of ideals generated by your chosen linear forms. 
      Example
  	 R = QQ[x,y,z];
  	 L = {x,y,z};
  	 F = partialLinearFiltration(R,L);
  	 KF = partialKoszulFiltration(R,F)
    SeeAlso
      sortIdealList 
      partialLinearFiltration
///
 -------------------------------------------
-- Documentation findKoszulFiltration    --
--------------------------------------------  

doc ///
  Key 
    findKoszulFiltration
    (findKoszulFiltration, Ring, List)

  Headline
    Find a Koszul filtration
  Usage
    findKoszulFiltration(R, L)
  Inputs
    R: Ring
    L: List
       A list of linear forms.
  Outputs
    : List
      A list whose entries form a Koszul filtration. If no Koszul filtration exists with ideals whose generators lie in L, the output is an error message stating that.
  Description
    Text
      The function calls partialLinearFiltration and partialKoszulFiltration to search for a Koszul filtration. 
      If there exists a Koszul filtration consisting of ideals with generators in your provided list L, then the function returns it. 
      If no such filtration exists, it returns the error "no Koszul filtration exists within your list of generators".
      
      The function returns the maximal Koszul filtration given the set of generators in L. This means that any other Koszul filtration consisting of ideals
      with generators in L will be a subset of the output Koszul filtration.
      
      Note that if you are working over a finite field it is possible to input all linear forms in L and obtain a conclusive answer as to whether a Koszul filtration exists. 
      Be aware, however, that the function is slow for large lists.
      
     
      Recall that the definition of a Koszul filtration requires the following conditions for your list KF of ideals (in a standard graded algebra over a field):
    
      * The zero ideal and the maximal ideal are in KF.
      
      * All ideals in KF are generated by linear forms.
      
      * For every nonzero ideal I in KF there exists an ideal J in KF such that I/J is cyclic and J:I is in KF. 
    
      
      
    Example
      R = QQ[x,y,z];
      I= ideal(x^2-x*y);
      S= R/I;
      L = {x,y,z};
      findKoszulFiltration(S,L)
    
    Text
      The following example comes from
      Alessio D'Alì, The Koszul property for spaces of quadrics of codimension three, J. Algebra 490 (2017), 256-282.
      
    Example
      clearAll;
      R1= QQ[x_1,y_1,t_1,t_2,t_3];
      I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2);
      S1=R1/I1;
      List1= {x_1,y_1,t_1,t_2,t_3};
      findKoszulFiltration(S1,List1)
      
      
  SeeAlso
    partialLinearFiltration
    partialKoszulFiltration  
///

--------------------------------------     
-- Documentation partialLinearFlags --
--------------------------------------
 
doc ///
  Key
    partialLinearFlags
    (partialLinearFlags, Ring, List, Ideal)
  Headline 
    Finding linear flags
  Usage
    partialLinearFlags(R,L,I)
  Inputs
    R: Ring
    L: List
      A list of linear forms in I.
    I: Ideal
      An ideal whose generators are linear.
  Outputs
    : List
     A list whose entries are the linear flags from the ideal I to the zero ideal that consist of ideals which are generated by elements of L.
  Description
    Text
      A (partial) linear flag is a chain of ideals which is a (partial) linear filtration.  
      The function finds all linear flags from I to the zero ideal where the ideals have generators in L and the quotient ideals are linear.
    Example
      R = QQ[x,y,z];
      L = {x,y,z};    
      partialLinearFlags(R,L,ideal(x,y,z))
///

---------------------------------
-- Documentation sortIdealList --
---------------------------------
    
    
doc ///

  Key
   sortIdealList
   (sortIdealList, List)
  
  Headline 
   Sorting a list of ideals according to the number of generators
  Usage
   sortIdealList(L)
  Inputs
   L: List
     A list of ideals.
  
  Outputs
   :List
    A list of lists of ideals sorted according to the number of generators.
    
  Description
   Text
    The function takes a list of ideals and sorts them based on their number of generators. 
    The function formats the list of ideals so that one can input it in the function partialKoszulFiltration.
  
   Example
    R=QQ[a,b,c,d,e,f];
    L={ideal(0),ideal(a),ideal(b),ideal(a,b,c),ideal(a+b,d),ideal(e,f)};
    sortIdealList(L)
  
  SeeAlso
     partialKoszulFiltration 
///

--------------------------------------
-- Documentation isKoszulFiltration --     
--------------------------------------     
doc ///
  Key
    isKoszulFiltration
    (isKoszulFiltration, Ring, List)
  Headline
    Is your family of ideals a Koszul filtration?
  Usage
    isKoszulFiltration(R, L)
  Inputs
    R: Ring
    L: List
      A list whose entries should be ideals that you think/hope form a Koszul filtration.
  Outputs
    : Boolean
      A Boolean indicating whether the given list of ideals forms a Koszul filtration.
 
  Description
    Text
      The function takes a list of ideals and returns true if they form a Koszul filtration and false if they do not.
      Recall that a Koszul filtration requires the following from your list KF of ideals:
 
      * The zero and the maximal ideal are in KF.
      
      * All ideals are generated by linear forms.
      
      * For any ideal I in KF there exists an ideal J in KF such that I/J is cyclic and J:I is in KF. 

    Example 
      R = QQ[x,y,z];
      I = ideal(x^2);
      S = R/I;
      L1 = {ideal(0_S),ideal(x),ideal(y),ideal(z),ideal(x,y),ideal(x,z),ideal(y,z),ideal(x,y,z)};
      isKoszulFiltration(S,L1)

      
    Example 
      R = QQ[x,y,z];
      I = ideal(x^2);
      S = R/I;
      L2 = {ideal(x),ideal(y),ideal(z),ideal(x,y),ideal(x,z),ideal(y,z),ideal(x,y,z)};
      isKoszulFiltration(S,L2)
      
      
    Example 
      R = QQ[x,y,z];
      I = ideal(x^2);
      S = R/I;
      L3 = {ideal(0_S),ideal(x+y),ideal(y-z),ideal(x,y),ideal(x,z),ideal(y,z),ideal(x,y,z)};
      isKoszulFiltration(S,L3)
    
    Text
      The following example comes from
      Alessio D'Alì, The Koszul property for spaces of quadrics of codimension three, J. Algebra 490 (2017), 256-282.


    Example    
      clearAll;
      R1= QQ[x_1,y_1,t_1,t_2,t_3];
      I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2);
      S1=R1/I1;
      KF1={ideal(0_S1),ideal(x_1),ideal(y_1),ideal(x_1,t_2),ideal(y_1,t_1),ideal(y_1,t_1,t_3),ideal(x_1,t_2,t_1),ideal(y_1,t_1,t_2,t_3), ideal(x_1,y_1,t_1,t_2,t_3)};
      isKoszulFiltration(S1,KF1)

///
 --******************************************--
-- TESTS     	       	    	      	    --
--******************************************--

--example Koszul filtrations come from:
--Alessio D'Alì, The Koszul property for spaces of quadrics of codimension three, J. Algebra 490 (2017), 256-282.
   


-----------------------------------------------
-- Test partialLinearFiltration    --
-----------------------------------------------
TEST///
R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
List1= {x_1,y_1,t_1,t_2,t_3}
KF1={ideal(0_S1),ideal(x_1),ideal(y_1),ideal(x_1,t_2),ideal(y_1,t_1),ideal(y_1,t_1,t_3),ideal(x_1,t_2,t_1),ideal(y_1,t_1,t_2,t_3), ideal(x_1,y_1,t_1,t_2,t_3)}

PartialF1= flatten partialLinearFiltration(S1,List1);
assert(all(KF1, I -> any(PartialF1, J -> I == J)))
///

TEST///
R2=QQ[x_2,y_2,t_21,t_22]
I2=ideal(t_22^2, x_2*t_21, t_21*t_22, t_21^2 - x_2*t_22, x_2*y_2, y_2*t_21, y_2*t_22)
S2= R2/I2
List2={x_2,y_2,t_21,t_22}
KF2= {ideal(0_S2),ideal(x_2),ideal(y_2),ideal(y_2,t_21),ideal(x_2,t_22),ideal(x_2,t_21,t_22),ideal(x_2,y_2,t_22),ideal(x_2,y_2,t_21,t_22)}

PartialF2= flatten partialLinearFiltration(S2,List2);
assert(all(KF2, I -> any(PartialF2, J->I==J)))
///


-----------------------------------------------
-- Test partialKoszulFiltration    --
-----------------------------------------------
TEST///
R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
List1= {x_1,y_1,t_1,t_2,t_3}
KF1={ideal(0_S1),ideal(x_1),ideal(y_1),ideal(x_1,t_2),ideal(y_1,t_1),ideal(y_1,t_1,t_3),ideal(x_1,t_2,t_1),ideal(y_1,t_1,t_2,t_3), ideal(x_1,y_1,t_1,t_2,t_3)}

PartialF1= flatten partialLinearFiltration(S1,List1);
PartialFSorted1= sortIdealList(PartialF1);
PKF= flatten partialKoszulFiltration(S1,PartialFSorted1);

assert(all(KF1, I -> any(PKF, J -> I == J)))
///

TEST///

R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
List1= {x_1,y_1,t_1,t_2,t_3}
KF1={ideal(0_S1),ideal(x_1),ideal(y_1),ideal(x_1,t_2),ideal(y_1,t_1),ideal(y_1,t_1,t_3),ideal(x_1,t_2,t_1),ideal(y_1,t_1,t_2,t_3), ideal(x_1,y_1,t_1,t_2,t_3)}

PartialF1= flatten partialLinearFiltration(S1,List1);
PartialFSorted1= sortIdealList(PartialF1);
PKF= flatten partialKoszulFiltration(S1,PartialFSorted1);
assert(isKoszulFiltration(S1,PKF))
///

-----------------------------------------------
-- Test findKoszulFiltration    --
-----------------------------------------------
TEST///
R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
List1= {x_1,y_1,t_1,t_2,t_3}
KF1={ideal(0_S1),ideal(x_1),ideal(y_1),ideal(x_1,t_2),ideal(y_1,t_1),ideal(y_1,t_1,t_3),ideal(x_1,t_2,t_1),ideal(y_1,t_1,t_2,t_3), ideal(x_1,y_1,t_1,t_2,t_3)}

Koszulfiltration:=findKoszulFiltration(S1,List1);

assert (all(KF1, I -> any(Koszulfiltration, J -> I == J)))
///

TEST///
R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
List1= {x_1,y_1,t_1,t_2,t_3}

Koszulfiltration:=findKoszulFiltration(S1,List1);
assert (isKoszulFiltration(S1,Koszulfiltration)== true)
///

TEST///
R2=QQ[x_2,y_2,t_21,t_22]
I2=ideal(t_22^2, x_2*t_21, t_21*t_22, t_21^2 - x_2*t_22, x_2*y_2, y_2*t_21, y_2*t_22)
S2= R2/I2
List2={x_2,y_2,t_21,t_22}
KF2= {ideal(0_S2),ideal(x_2),ideal(y_2),ideal(y_2,t_21),ideal(x_2,t_22),ideal(x_2,t_21,t_22),ideal(x_2,y_2,t_22),ideal(x_2,y_2,t_21,t_22)}

Koszulfiltration:=findKoszulFiltration(S2,List2);

assert (all(KF2, I -> any(Koszulfiltration, J -> I == J)))
///

TEST///
R2=QQ[x_2,y_2,t_21,t_22]
I2=ideal(t_22^2, x_2*t_21, t_21*t_22, t_21^2 - x_2*t_22, x_2*y_2, y_2*t_21, y_2*t_22)
S2= R2/I2
List2={x_2,y_2,t_21,t_22}
KF2= {ideal(0_S2),ideal(x_2),ideal(y_2),ideal(y_2,t_21),ideal(x_2,t_22),ideal(x_2,t_21,t_22),ideal(x_2,y_2,t_22),ideal(x_2,y_2,t_21,t_22)}

Koszulfiltration:=findKoszulFiltration(S2,List2);
assert (isKoszulFiltration(S2,Koszulfiltration)== true)
///

-----------------------------------------------
-- Test partialLinearFlags    --
-----------------------------------------------
TEST///
R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
List1= {x_1,y_1,t_1,t_2,t_3}
pLF=partialLinearFlags(S1,List1,ideal(x_1,y_1,t_1,t_2,t_3));
A= toList(0..4);

testcolonideals= apply(pLF,i-> apply(A,a-> flatten entries gens(i#(a+1):i#a)));-- compute all colon ideals in the linear flags
testcolonideals= flatten flatten testcolonideals;

checkdegrees= unique apply(testcolonideals, i->degree i);-- check that no colon ideal has a generator with degree higher than 1
checkdegrees= flatten checkdegrees ;

assert (all(checkdegrees,i-> i<2)) 

///
-----------------------------------------------
-- Test sortIdealList    --
-----------------------------------------------
TEST///
R=QQ[a,b,c,d,e,f];
unsortedlist= {ideal(0),ideal(a,b,c,d,e,f),ideal(a,b,c),ideal(c,d,e),ideal(a+e,e,a),ideal(f,e+d)};
sortedlist= sortIdealList(unsortedlist);
assert(sortedlist=={{ideal()},{},{ideal(e,a),ideal(f,d+e)},{ideal(c,b,a),ideal(e,d,c)},{},{},{ideal(f,e,d,c,b,a)}})
///


-----------------------------------------------
-- Test isKoszulFiltration    --
-----------------------------------------------
TEST///
R1= QQ[x_1,y_1,t_1,t_2,t_3];
I1= ideal(t_2^2, t_3^2, t_2*t_3, x_1*y_1, x_1*t_1, x_1*t_3, y_1*t_1, y_1*t_2, t_1*t_2, t_1*t_3, x_1*t_2 - t_1^2, y_1*t_3 - t_1^2)
S1=R1/I1
KF1={ideal(0_S1),ideal(x_1),ideal(y_1),ideal(x_1,t_2),ideal(y_1,t_1),ideal(y_1,t_1,t_3),ideal(x_1,t_2,t_1),ideal(y_1,t_1,t_2,t_3), ideal(x_1,y_1,t_1,t_2,t_3)}

assert(isKoszulFiltration(S1,KF1)== true)
///
TEST///
R2=QQ[x_2,y_2,t_21,t_22]
I2=ideal(t_22^2, x_2*t_21, t_21*t_22, t_21^2 - x_2*t_22, x_2*y_2, y_2*t_21, y_2*t_22)
S2= R2/I2
KF2= {ideal(0_S2),ideal(x_2),ideal(y_2),ideal(y_2,t_21),ideal(x_2,t_22),ideal(x_2,t_21,t_22),ideal(x_2,y_2,t_22),ideal(x_2,y_2,t_21,t_22)}

assert(isKoszulFiltration(S2,KF2)== true)
///


