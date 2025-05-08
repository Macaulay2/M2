newPackage(
	"PolyominoIdeals",
	Version => "1.1",
	Date => "October 18, 2023",
	
	Authors => {
		{
			Name => "Carmelo Cisto", 
			Email => "ccisto@unime.it"
		},
		{
			Name => "Francesco Navarra", 
			Email => "fnavarra@unime.it"
		},
		{
			Name => "Rizwan Jahangir", 
			Email => "rizwan@sabanciuniv.edu",
			HomePage => "https://myweb.sabanciuniv.edu/rizwan"
			}
	},
	Headline => "binomial ideals of collections of cells",
	Keywords => {"Combinatorial Commutative Algebra"},
	DebuggingMode => false
)

export {
		"polyoIdeal",
		"polyoMatrix",
		"polyoToric",
		
		--options
		"Field",
		"TermOrder",
		"RingChoice"
}

--------------------------------------------------------------------------------------------------
-- Declaration of some variables
--------------------------------------------------------------------------------------------------

x:=vars(23);
u:=vars(20);
v:=vars(21);
h:=vars(7);

--------------------------------------------------------------------------------------------------
-------------------------------------- polyoVertices ---------------------------------------------
--------------------------------------------------------------------------------------------------
--
-- polyoVertices is a function which computes the set of the vertices of the collection of cells.
--
-- A collection of cells is encoded here by a list Q, whose elements are the lists of the 
-- diagonal corners of the cells.
--
-- For instance:
--       
--		   __
--              __|__|
--           __|__|
--          |__|__|      is encoded as Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}},{{3,3},{4,4}}}
--
--------------------------------------------------------------------------------------------------
 
polyoVertices=(Q)->(
        for i from 0 to #Q-1 do(
            if Q#i#1-Q#i#0 != {1,1} then error "The input list does not represent a collection of cells"
        );
	V:={};
   	for i from 0 to #Q-1 do(
        V=join(V,toList ({Q#i#0#0,Q#i#0#1}..{Q#i#1#0,Q#i#1#1}));
    	);
    	V=set V;
    	V=toList(V);
    	
	return V;
);


--------------------------------------------------------------------------------------------------
--------------------------------------- polyoRingDefault -----------------------------------------
--------------------------------------------------------------------------------------------------
--
-- The function polyoRing defines the ring attached to a collection of cells, where the monomial
-- order is given by the order defined in the option Term order, induced by the following order of
-- the variables: x_a > x_b with a=(i,j) and b=(k,l), if i > k, or i = k and j > l.  
--
--------------------------------------------------------------------------------------------------

polyoRingDefault = method (Options=>{Field => QQ, TermOrder=>Lex})
polyoRingDefault List := opts -> Q ->  (

    V:=reverse(sort(polyoVertices(Q)));
    Gen:={};                                      
    for i from 0 to #V-1 do(
        Gen=join(Gen,{x_(V#i#0,V#i#1)});
    );
    R:=(opts.Field)[Gen, MonomialOrder => opts.TermOrder];
    return R;
);


--------------------------------------------------------------------------------------------------
------------------------------------------ polyoMatrix -------------------------------------------
--------------------------------------------------------------------------------------------------
--
-- The function polyoMatrix define the matrix attached to a collection of cells P, where the
-- smallest interval containing it is [(p,q),(r,s)]. The matrix has r-p rows and s-q columns
-- and the (i,j)-th entry is x_(i,j) if (i,j) is a vertex of P, otherwise it is zero. 
--
--------------------------------------------------------------------------------------------------

-- Define two functions to compute p,q,r and s from the list Q encoding the collection of cells.
-- The function Mv(Q) computes the list {p,r}:

Mv=(Q)->(					  
	V:={};
	for i from 0 to #Q-1 do(
        	V=join(V,toList{Q#i#1#1});
    	);
   	return toList{min(V)-1,max(V)};
	);
	
-- The function Mh(Q) computes the list {q,s}:	
	
Mh=(Q)->(					
	V:={};
	for i from 0 to #Q-1 do(
       		 V=join(V,toList{Q#i#1#0});
    	);
   	return toList{min(V)-1,max(V)};
	);

-- Define the function polyoMatrix, to compute the matrix.
	
polyoMatrix = method(TypicalValue=>Matrix)	
polyoMatrix List := Q ->(				
        R:=polyoRing(Q);
	V:=polyoVertices(Q);				
    	Corners:={};                                      
        for i from 0 to #V-1 do(
        	Corners=join(Corners,{(V#i#0,V#i#1)});	 
    	);
       	H:={};
    	Verti:=Mv(Q);
       	Orizon:=Mh(Q);
    	for j from Verti#0 to Verti#1 do(		
		L:={};
		for i from Orizon#0 to Orizon#1 do(
			if member((i,j),Corners) then 
			L=join(L,toList{x_(i,j)_R})
			else L=join(L,{0});
			);
		H=append(H,L);
		);
	H=reverse(H);
	return matrix(H);
);	


------------------------------------------------------------------------------------------------------
-------------------------------------------- polyoRingConvex -----------------------------------------
------------------------------------------------------------------------------------------------------
--
-- The function polyoRingConvex returns the polynomial ring of a collection of cells P with a new 
-- monomial order. In particular, if P is a weakly connected and convex collections of cells then
-- polyoRingConvex defines a polynomial ring in which the monomial order is defined as in the paper:
-- H. Ohsugi and T. Hibi, "Koszul bipartite graphs ", Adv. Appl. Math. 22,  25-28, 1999. 
-- We know that the generators of the binomial ideal associated with a weakly connected and  
-- convex collections of cells forms the reduced Groebner basis with respect to this order, and 
-- so the initial ideal is squarefree and generated in degree two. 
-----------------------------------------------------------------------------------------------------


-- vectorLessEqThan is a baby function which compares two vectors in N^d, defining A < B if the 
-- rightmost nonzero component of the vector A − B is negative.

vectorLessEqThan=(A,B)->(			
	Ar:=reverse(A);
	Br:=reverse(B);
	if Ar==Br then return true else
		for i from 0 to #A-1 do(
    		if Ar#i<Br#i then break return true; 
    		if Ar#i>Br#i then break return false;
    		);
);

-- Sub1 is a baby function which replaces 1 in the non-null entries of a generic vector.
	
Sub1:=(M)->(					
	N:={};
	for i from 0 to #M-1 do(
		if M#i!=0 then N=join(N,{1})
		else N=join(N,{0}); 
	);
	return N;
);

-- polyoMatrixReduced is a function which returns a new matrix from polyoMatrix(Q) by 
-- switching rows or columns as done in the paper:
-- H. Ohsugi and T. Hibi, "Koszul bipartite graphs ", Adv. Appl. Math. 22,  25-28, 1999. 
	
polyoMatrixReduced=(Q)->(
	PolyominoMat:=polyoMatrix(Q);
	EntrateMat:=entries(PolyominoMat);
	numberrow:=numgens(target(PolyominoMat));
	MutMat:=mutableMatrix(PolyominoMat);
	SubEntrateMat:={};
	for k from 0 to numberrow-1 do(
		SubEntrateMat=join(SubEntrateMat,{Sub1(EntrateMat#k)});
	);
	for i from 0 to numberrow-1 do(
		for j from i to numberrow-1 do(
			if vectorLessEqThan(SubEntrateMat#i,SubEntrateMat#j)==false then(
			MutMat=rowSwap(MutMat,i,j);
			SubEntrateMat=switch(i,j,SubEntrateMat);
			);
		);
	);
	rowMutMat:=matrix(MutMat);
	TMutMat:=transpose(rowMutMat);
	SecondEntrateMat:=entries(TMutMat);
	nc:=numgens(source(rowMutMat));
	MutarowMutMat:=mutableMatrix(rowMutMat);
	Sos:={};
	for c from 0 to nc-1 do(
		Sos=join(Sos,{Sub1(SecondEntrateMat#c)});
	);
	for a from 0 to (nc-1) do(
		for b from a to (nc-1) do(
			if vectorLessEqThan(Sos#a,Sos#b)==false then(
			MutarowMutMat=columnSwap(MutarowMutMat,a,b);
			Sos=switch(a,b,Sos);
			);
		);
	);
	return matrix(MutarowMutMat);
);

-- polyoRingConvex defines a new polynomial ring.

polyoRingConvex = method(Options=>{Field => QQ})	
polyoRingConvex List := opts -> Q ->  (
	PMR:=polyoMatrixReduced(Q); 
	EPMR:=entries(PMR);
	numRow:=numgens(target(PMR));
	numColumn:=numgens(source(PMR));
	variables:={};
	for i from 0 to numRow-1 do(
		for j from 0 to numColumn-1 do(
			if EPMR#i#j==0 then variables=join(variables,toList{})
			else  variables=join(variables,toList{EPMR#i#j});
		);
	);
	Gens:=variables;
	S:=(opts.Field)[Gens, MonomialOrder => RevLex, Global=> false];
	return S;
);


---------------------------------------------------------------------------------------------------
---------------------------------------------polyoRing---------------------------------------------
---------------------------------------------------------------------------------------------------
-- The function polyoRing defines the ring for polyoIdeal. Whether it is 1 or by default it 
-- returns the ideal computed by polyoIdeal in the ambient ring given by polyoRingDefault. 
-- With a value different by 1 it returns the ideal in the ambient ring given by polyoRingConvex. 
------------------------------------------------------------------------------------------------

polyoRing = method (Options=>{Field => QQ, TermOrder=>Lex, RingChoice=>1})
polyoRing List := opts -> Q ->(
if opts.RingChoice==1 then 
    return polyoRingDefault(Q,Field=>opts.Field, TermOrder=>opts.TermOrder)
else return polyoRingConvex(Q,Field=>opts.Field);
);

----------------------------------------------------------------------------------------------------
------------------------------------------ polyoIdeal ----------------------------------------------
----------------------------------------------------------------------------------------------------
-- polyoIdeal is a function which returns the inner 2-minor ideal attached to a collection of cells.
--
-- The option RingChoice with value 1 and by default returns the ideal in the ambient ring given by 
-- polyoRingDefault. With a value different by 1 it returns the ideal in the ambient ring given 
-- by polyoRingConvex
----------------------------------------------------------------------------------------------------

--isInnerInterval is a function such that, if A and B are two cells, it returns true if [A,B] is 
--an inner interval of the collection of cells, otherwise it returns false.

isInnerInterval=(A,B,Q)->(
    C:=B-{1,1};
    if C==A then return true;
    if member({C,B},Q)==false then return false;
    tag:=true;
    for i from A#1+1 to B#1 do (
        for j from A#0 to B#0-1 do (
            if member({{j,i-1},{j+1,i}},Q)==false then return false;
        );
    );
    return tag;
);



polyoIdeal = method (Options=>{Field => QQ, TermOrder=>Lex, RingChoice=>1})
polyoIdeal List := opts -> Q ->(

R:=polyoRing(Q,Field=>opts.Field, TermOrder=>opts.TermOrder, RingChoice=>opts.RingChoice);
InnerBinomials:={};
    for i from 0 to #Q-1 do(
        lLowCorner := Q#i#0;
            for j from 0 to #Q-1 do(
                 rUpCorner := Q#j#1;
                 if lLowCorner#0<rUpCorner#0 and lLowCorner#1<rUpCorner#1 then (
                     if isInnerInterval(lLowCorner,rUpCorner,Q) then (
                         a:=lLowCorner#0;
                         b:=lLowCorner#1;
                         c:=rUpCorner#0;
                         d:=rUpCorner#1;
                         InnerBinomials=join(InnerBinomials,{x_(a,b)_R*x_(c,d)_R-x_(a,d)_R*x_(c,b)_R});
                     );
                 );
            );
    );
    InnerBinomials = set InnerBinomials;
    InnerBinomials = toList InnerBinomials;
    I:=ideal(InnerBinomials);
    return I;
);

	
-------------------------------------------------------------------------------------------------------------------
------------------------------------------- polyoToric ------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------
--Given a polyomino encoded by Q and the list H of the lower left corners of each hole of the 
--polyomino, the function polyoToric returns the toric ideal as defined in the paper: 
--Mascia, Rinaldo, Romeo, "Primality of multiply connected polyominoes", Illinois J. Math. 64(3), 291-304, 2020.
--------------------------------------------------------------------------------------------------------------------

Leq2=(A,B)->(
    if A#0<=B#0 and A#1<=B#1 then return true;
    return false;
);

polyoToric = method(TypicalValue=>Ideal)
polyoToric(List, List) := (Q,H) -> (
    V:=reverse(sort(polyoVertices(Q)));         --IMPORTANT:V contains the vertices Q IN THE RIGHT ORDER with f:=map(S,T,Im); 
    Oriz:={};
    Vert:={};
    for i from 0 to #V-1 do(
        Oriz=join(Oriz,{V#i#0});
        Vert=join(Vert,{V#i#1});
    );
    Oriz=set Oriz;
    Oriz=toList (Oriz);
    Oriz=sort(Oriz);                	 		--Oriz is the sorted list the first coordinates of every cell
    Vert=set Vert;
    Vert=toList (Vert);
    Vert=sort(Vert);                			--Vert is the sorted list the second coordinates of every cell
    VerInt:={};
    for i from min(Oriz) to max(Oriz) do(               --bulding the vertical maxiamal intervals
        j:=min(Vert);
        while j<max(Vert) do(
            L1:={};
            while member({{i,j},{i+1,j+1}},Q) or member({{i-1,j},{i,j+1}},Q)  do(
                L1=join(L1,{{i,j},{i,j+1}});
                j=j+1;
            ); 
            L1=set L1;
            L1=toList (L1);
            VerInt=join(VerInt,{L1});
            j=j+1;            
        );
    );
    VerInt=delete({},VerInt);                   	 --VerInt contains the vertical maximals intervals
    OrInt:={};
    for j from min(Vert) to max(Vert) do(       	 --bulding the orizontal maxiamal intervals
        i:=min(Oriz);
        while i<max(Oriz) do(
            L1:={};
            while member({{i,j},{i+1,j+1}},Q) or member({{i,j-1},{i+1,j}},Q)  do(
                L1=join(L1,{{i,j},{i+1,j}});
                i=i+1;
            ); 
            L1=set L1;
            L1=toList (L1);
            OrInt=join(OrInt,{L1});
            i=i+1;            
        );
    );
    OrInt=delete({},OrInt);                     	-- OrInt contains the orizontal maximals intervals
    Svar:={};                                  		-- variables of the "toric" ring
    for i from 0 to #OrInt-1 do(
        Svar=join(Svar,{u_(i)});
    );
    for i from 0 to #VerInt-1 do(
        Svar=join(Svar,{v_(i)});
    );
    for i from 0 to #H-1 do(
        Svar=join(Svar,{h_(i)});
    );
    S:=QQ[Svar, MonomialOrder => Lex];           	 -- definition of the the "toric" ring 
    Im:={};                                      	 -- images of the variables in the "toric" ring
    for i from 0 to #V-1 do(
        m:=1;
        for k from 0 to #OrInt-1 do(                          
            if  member(V#i,OrInt#k) then m=m*u_(k)_S;
        );
        for k from 0 to #VerInt-1 do(
            if  member(V#i,VerInt#k) then m=m*v_(k)_S;
        );
        for j from 0 to #H-1 do(
            if Leq2(V#i,H#j) then m=m*h_(j)_S;
        );
        Im=join(Im,{m});
    );
    T:=polyoRing(Q);
    f:=map(S,T,Im);         -- It is VERY IMPORTANT TO PAY ATTENTION for the order of variables in T, with V:=reverse                       
    J:= kernel f;
    return J;
);

---------------------------------------------------------------------------------------------------------
					-- End of source code --
---------------------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------------------------
-----------------------------------------    DOCUMENTATION    -------------------------------------------
---------------------------------------------------------------------------------------------------------


beginDocumentation()

document {
        Key => PolyominoIdeals,
        Headline => "a package to work with binomial ideals associated with collections of cells",
        EM "PolyominoIdeals", " is a package for making several computations with the inner 2-minor ideals attached to collections of  cells.\n ",
        BR{},BR{},
        "In [AAQ2012] Ayesha Asloob Qureshi establishes a connection between Combinatorial Commutiative Algebra and collection of cells, assigning to every collection of cells the binomial ideal of its inner $2$-minors.\n",
        BR{},BR{},
        " Consider the natural partial order on $\\NN^2$ and let $a,b \\in \\N^2$ with $a\\leq b$. The set $[a, b] = \\{c \\in \\NN^2 : a \\leq c \\leq b\\}$ is called an interval of $\\NN^2$; moreover, if $b=a+(1,1)$, then $[a,b]$ is called a cell of $\\NN^2$. An interval $C=[a, b]$, where $a = (i, j)$ and $b = (k, l)$, is said to be a proper interval if $i < k$ and $j < l$. The elements $a, b$ are said the diagonal corners of $C$ and  $c = (k, j)$ and $d = (i, l)$ the anti-diagonal ones. If $C$ is a cell, then $V(C)=\\{a,a+(1,1),a+(0,1),a+(1,0)\\}$ is the set of the corners of $C$.\n",
       BR{},
        " To each collection of cells $\\mathcal{P}$, we attach an ideal $I_{\\mathcal{P}}$ as following. Let $K$ be a field and $S=K[x_a: a \\in V (\\mathcal{P})$, where $V (\\mathcal{P})$ is the union of the vertices sets of all cells of $\\mathcal{P}$. A proper interval $[a, b]$ is called an inner interval of $\\mathcal{P}$ if all cells of $[a, b]$ belong to $\\mathcal{P}$. The binomial $f= x_ax_b − x_c x_d$ , where $c$ and $d$ are the anti-diagonal corners of $[a, b]$, is called an inner 2-minor of $\\mathcal{P}$, if $[a, b]$ is an inner interval of $\\mathcal{P}$. We denote by $I_{\\mathcal{P}}$ the ideal generated in $S$ by the inner 2-minors of $\\mathcal{P}$ and by $K [\\mathcal{P}]$ the quotient ring $S/I_{\\mathcal{P}}$, called the coordinate ring of $\\mathcal{P}$.\n",
        BR{},
        " The class of ideals attached to a collection of cells includes, for example, the ideals of 2-minors of two-sided ladders, but it is much more general. Interesting classes of collections of cells are the so-called polyominoes that are well studied in various combinatorial contexts. A collection of cells $\\mathcal{P}$ is called a polyomino if for any two cells $A, B \\in \\mathcal{P}$ there exists a sequence of cells $A=C_1,\\dots, C_m=B$ of $\\mathcal{P}$ such that  $C_i$ and $C_{i+1}$ have an edge in common. In such a case, $I_{\\mathcal{P}}$ is called polyomino ideal of $\\mathcal{P}$. \n",
        BR{},BR{},
        "The aim of this package is to provide several tools to help mathematicians in the study of polyomino ideals. Every contribution is very welcome. \n",
	BR{},BR{},
	BOLD "Literature \n",
	UL {
	  LI {"[AAQ2012] ", EM "Ideals generated by 2-minors, collections of cells and stack polyominoes ", "(A. A. Qureshi, 2012,  J. Algebra).\n"}
	 },
	 
	Subnodes => {
	TO "polyoIdeal",
	TO "polyoToric",
	TO "polyoMatrix",
	TO "Field",
	TO "TermOrder",
	TO "RingChoice",
	},
	 }


----------------------------------------------------------------------------------------------------
-------------------------------- Documentation for exported functions ------------------------------
----------------------------------------------------------------------------------------------------

          	 
document {
     Key => {polyoIdeal,(polyoIdeal, List)},
     Headline => "Ideal of inner 2-minors of a collection of cells",
     Usage => "polyoIdeal Q",
     Inputs => {
          "Q" => { "a list of lists whose elements are the lists of diagonal corners of each cell."} },
     Outputs => { {"The inner 2-minors ideal of the collection of cells encoding by Q"} },
     
    "Let $\\mathcal{P}$ be a collection of cells. This routine returns the ideal $I_{\\mathcal{P}}$ of the inner 2-minors of $\\mathcal{P}$.\n",
    	 BR{},
     "Moreover, if $\\mathcal{P}$ is a polyomino, then it returns the polyomino ideal of $\\mathcal{P}$.\n",
  	 BR{},BR{},
     "If Q does not encod a collection of cells, that is in a list of Q the difference between the first sublist and the second one is different from {1,1}, then we get an error.\n",
    	 BR{},BR{},
    	 EXAMPLE {
          "Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}}}",
          "I = polyoIdeal Q",
        },
   	 BR{},BR{},
   	 EXAMPLE {
   "Q={{{1, 1}, {2, 2}}, {{2, 1}, {3, 2}}, {{3, 1}, {4, 2}}, {{3, 2}, {4, 3}}, {{3, 3}, {4, 4}}, {{2, 3}, {3, 4}}, {{1, 3}, {2, 4}}, {{1, 2}, {2, 3}}};",
   "I = polyoIdeal Q",
    	},
        }
          
document {
	Key => {polyoMatrix, (polyoMatrix, List)},
	Headline => "Matrix attached to a collection of cells",
	Usage => "polyoMatrix Q",
	Inputs => { "Q" => "a list of lists whose elements are the lists of diagonal corners of each cell." },
	Outputs => {"The matrix attached to the collection of cells encoding by Q."},
	"Let $\\mathcal{P}$ be a collection of cells and $[(p,q),(r,s)]$ be the smallest interval of $\\NN^2$ containing $\\mathcal{P}$. The matrix $M(\\mathcal{P})$ is a matrix having $s-q$ rows and $r-q$ columns with $M(\\mathcal{P})_{i,j}=x_{(i,j)}$ if $(i,j)$ is a vertex of $\\mathcal{P}$, otherwise it is zero.\n",
	BR{},
	 "This routine returns the matrix of the collection of cells encoding by Q. In the next example we assume for simplicity that the smallest interval containing $\\mathcal{P}$ is $[(1,1),(r,s)]$.",
	BR{},BR{},
	EXAMPLE {
          "Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}}}",
          "M = polyoMatrix Q",
         },
        BR{},BR{},
        EXAMPLE {
          "Q={{{1, 3}, {2, 4}}, {{2, 2}, {3, 3}}, {{3, 1}, {4, 2}}, {{2, 4}, {3, 5}}, {{3, 5}, {4, 6}}, {{4, 4}, {5, 5}}, {{5, 3}, {6, 4}}, {{4, 2}, {5, 3}}, {{6, 4}, {7, 5}}, {{5, 1}, {6, 2}}, {{7, 5}, {8, 6}}, {{7, 1}, {8, 2}}, {{4, 6}, {5, 7}}, {{5, 7}, {6, 8}}, {{6, 6}, {7, 7}}};",
          "M = polyoMatrix Q",
          },
        BR{},BR{},
        EXAMPLE {
          "Q={{{1, 1}, {2, 2}}, {{2, 2}, {3, 3}}, {{3, 3}, {4, 4}}, {{4, 3}, {5, 4}}};",
          "M = polyoMatrix Q",
	}
	}

document {
	Key => {polyoToric, (polyoToric, List, List)},
	Headline => "Toric ideal of a polyomino",
	Usage => "polyoToric(Q,H)",
	Inputs => { "Q" => {"a list of lists whose elements are the lists of diagonal corners of each cell."},
		    "H" => {"a list of the lower left corners of the holes."}
	},
	Outputs => {"A toric ideal attached to the polyomino encoded by Q."},
	
	"Let $\\mathcal{P}$ be a polyomino. A finite collection $\\mathcal{H}$ of cells not in $\\mathcal{P}$ is called a hole of $\\mathcal{P}$, if any two cells in $\\mathcal{H}$ are connected by a path of cells in $\\mathcal{H}$ and $\\mathcal{H}$ is maximal with respect to the set inclusion. Consider the following total order on $V(\\mathcal{P})$: $a=(i,j)>b=(k, l)$, if $i > k$, or $i = k$ and $j > l$. If $\\mathcal{H}$ is a hole of $\\mathcal{P}$, then we call lower left corner $e$ of $\\mathcal{H}$ the minimum, with respect to <, of the vertices of $\\mathcal{H}$.	Let $\\mathcal{H}_1,\\dots, \\mathcal{H}_r$ be the holes of $\\mathcal{P}$ and $e_k = (i_k, j_k)$ be the lower left corner of $\\mathcal{H}_k$. For $k \\in K =[r]$, we define the following subset $F_k = \\{(i, j) \\in V (\\mathcal{P}) : i \\leq i_k, j \\leq j_k\\}$. Denote by $\\{V_i\\}_{i\\in I}$ the set of all the maximal vertical edge intervals of $\\mathcal{P}$, and by $\\{H_j \\}_{j\\in J}$ the set of all the maximal horizontal edge intervals of $\\mathcal{P}$. Let $\\{v_i\\}_{i\\in I}$, $\\{H_j\\}_{j\\in J}$ , and $\\{w_k\\}_{k∈K}$ be three sets of variables. We consider the map $$\\alpha : V (\\mathcal{P}) \\rightarrow K[{h_i, v_j , w_k } : i \\in I, j \\in J, k \\in K]$$ $$a \\rightarrow \\prod_{a\\in H_i \\cap V_j} h_iv_j \\prod_{a\\in F_k} w_k$$ The toric ring $T_{\\mathcal{P}}$ associated to $\\mathcal{P}$ is defined as $T_{\\mathcal{P}} = K[\\alpha(a):a \\in V (\\mathcal{P})]$. The homomorphism $\\psi : S \\rightarrow T_{\\mathcal{P}}$ with $x_a \\rightarrow \\alpha(a)$ is surjective and the toric ideal $J_{\\mathcal{P}}$ is the kernel of $\\psi$.\n",
	BR{},
	"Note that the homomorphism $\\psi$ defined before is a natural generalization of that given in [QSS2017] for simple polyominoes.\n",
	BR{},BR{},
	"Given the polyomino encoded by Q and the list H of the lower left corners of each hole of the polyomino, the function ", TT "polyoToric", " returns the toric ideal $J_{\\mathcal{P}}$ defined before.\n",


	BR{},BR{},
	BOLD "Literature \n",
	UL {
	LI {"[QSS2017] ", EM "Simple polyominoes are prime ", "(A.A. Qureshi, T. Shibuta, A. Shikama, 2017, J. Commut. Algebra 9(3), 413-422).\n"},
	LI {"[MRR2020] ", EM "Primality of multiply connected polyominoes ", "(C. Mascia, G. Rinaldo, F. Romeo, 2020, Illinois J. Math. 64(3), 291-304).\n"},
	LI {"[CNU2022] ", EM "Primality of weakly connected collections of cells and weakly closed path polyominoes ", "(C. Cisto, F. Navarra, R. Utano, 2022, Illinois J. Math. 66(4), 545-563).\n"},
	   },
	   
     	BR{},
     	BOLD "Examples \n",
     	BR{},BR{},
     
     	EXAMPLE {
 	"Q={{{1, 1}, {2, 2}}, {{2, 1}, {3, 2}}, {{3, 1}, {4, 2}}, {{3, 2}, {4, 3}}, {{3, 3}, {4, 4}}, {{2, 3}, {3, 4}}, {{1, 3}, {2, 4}}, {{1, 2}, {2, 3}}};",
        "J=polyoToric(Q,{{2,2}})",
        },  
      	BR{},BR{},
       
      	EXAMPLE {
       "Q={{{3, 1}, {4, 2}}, {{4, 1}, {5, 2}}, {{5, 1}, {6, 2}}, {{5, 2}, {6, 3}}, {{5, 3}, {6, 4}}, {{5, 4}, {6, 5}}, {{5, 5}, {6, 6}}, {{4, 5}, {5, 6}}, {{3, 2}, {4, 3}}, {{3, 3}, {4, 4}}, {{2, 3}, {3, 4}}, {{1, 3}, {2, 4}}, {{1, 4}, {2, 5}}, {{1, 5}, {2, 6}}, {{2, 5}, {3, 6}}, {{3, 5}, {4, 6}}};",
        "I=polyoIdeal(Q);",
        "J=polyoToric(Q,{{2,4}});",
        "R=ring I",
        "J=substitute(J,R);",
        "J==I",
       } ,
        
      	BR{},BR{},
	"Morevover, if $\\mathcal{P}$ is a simple polyomino, that is it has not any hole, then the function ", TT "polyoToric", " works setting ", TT "H={}", " and it returns the polyomino ideal of $\\mathcal{P}$ in according to [QSS2017].\n",  
	BR{},BR{},
	EXAMPLE {
       "Q={{{2, 1}, {3, 2}}, {{3, 1}, {4, 2}}, {{2, 2}, {3, 3}}, {{1, 2}, {2, 3}}, {{3, 2}, {4, 3}}, {{2, 3}, {3, 4}}};",
       "I=polyoIdeal(Q);",
       "J=polyoToric(Q,{});",
       "R=ring I",
       "J=substitute(J,R);",
       "J==I",
        } ,
	 
	BR{},BR{},
	"In general, the function ", TT "polyoToric", " works also for weakly connected collection of cells. If $\\mathcal{P}$ is the non-simple collections of cells in Figure (A) of the Remark 3.4 in [CNU2022], we know that the inner 2-minor ideal of $\\mathcal{P}$ is not prime.\n",  
	BR{},BR{},
	EXAMPLE {
       "Q={{{2, 1}, {3, 2}}, {{3, 2}, {4, 3}}, {{1, 2}, {2, 3}}, {{2, 3}, {3, 4}}};",
       "I=polyoIdeal(Q);",
       "J=polyoToric(Q,{{2,2}});",
       "R=ring I",
       "J=substitute(J,R);",
       "J==I",
       "select(first entries gens J,f->first degree f>=3)",
        }  
	 
	}
	


-----------------------------------------------------------------------------------------------------
---------------------------------------- Options documentation --------------------------------------
-----------------------------------------------------------------------------------------------------


document {
     Key => {RingChoice,[polyoIdeal,RingChoice]},
     Headline => "optional argument for polyoIdeal",
     "Let $\\mathcal{P}$ be a collection of cells and $[(1,1),(m,n)]$ be the smallest interval of $\\NN^2$ containing $\\mathcal{P}$. Then we attach to $\\mathcal{P}$ the following polynomial ring $S_{\\mathcal{P}}=K[x_a:a\\in V(\\mathcal{P})$, where $K$ is a field.\n",
     BR{},
     "Whether it is 1 or by default it returns the ideal computed by ", TT "polyoIdeal ", "in the ambient ring given by ", TT "polyoRingDefault",". With a value different by 1 it returns the ideal in the ambient ring given by ", TT "polyoRingConvex",".",
     BR{},BR{},
     "Here we describe the using of the two options ", TT "polyoRingDefault"," and ", TT "polyoRingConvex",".\n",   
     BR{},BR{},
     BOLD "PolyoRingDefault \n",
     BR{},BR{},
     "This option gives the ideal $I_{\\mathcal{P}}$ in the polynomial ring $S_{\\mathcal{P}}$ where the monomial order is defined by ", TT "Term order ", "induced by the following order of the variables: $x_a > x_b$ with $a=(i,j)$ and $b=(k, l)$, if $i > k$, or $i = k$ and $j > l$.",
     	BR{},BR{},
     	ITALIC "Examples \n",
     	BR{},BR{},
     	EXAMPLE {
          "Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}}};",
          "I = polyoIdeal(Q);",
          "R=ring I;",
          "describe R",
          },
        BR{},  
     	EXAMPLE {
          "Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}}};",
          "I = polyoIdeal(Q,RingChoice=>1);",
          "R=ring I;",
          "describe R",
          },  
        BR{},  
     	EXAMPLE {
          "Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}}};",
          "I = polyoIdeal(Q,RingChoice=>1,TermOrder=> GRevLex);",
          "R=ring I;",
          "describe R",
          },
          
     BR{},BR{},
     BOLD "PolyoRingConvex \n",
     BR{},BR{},
     "A very interesting class of collections of cells which are studied from a combinatorial point of view is given by the weakly connected and convex ones.\n",
	BR{}, 
	"Let $\\mathcal{P}$ be a collection of cells. We say that $\\mathcal{P}$ is weakly connected if for any two cells $C$ and $D$ of $\\mathcal{P}$, there exist a sequence of cells of $\\mathcal{P}$ as $C = C_1,\\dots, C_m = D$ such that $C_i \\cap C_{i+1} \\neq \\emptyset$, for $i = 1,\\dots, m − 1$. Observe trivially that every polyomino is a weakly connected collection of cells. We say that a weakly connected collection $\\mathcal{P}$ of cells is row convex, if the horizontal cell interval $[A, B]$ is contained in $\\mathcal{P}$ for any two cells $A$ and $B$ of $\\mathcal{P}$ whose lower left corners are in horizontal position. Similarly one defines column convex. Hence $\\mathcal{P}$ is called convex if it is row and column convex.\n",
	BR{},
	"Assume that the smallest interval containing $\\mathcal{P}$ is $[(1,1),(m,n)]$. Consider the edge ring $R = K[s_it_j: (i, j) \\in V (\\mathcal{P})]$ associated to the bipartite graph $G$ with vertex set $\\{s_1,\\dots, s_m\\} \\cup\\{t_1,\\dots, t_n\\}$ to $\\mathcal{P}$ such that each vertex $(i, j) \\in V (\\mathcal{P})$ determines the edge $\\{s_i,t_j \\}$ in $G$. Let $S=K[x_a:a\\in V(\\mathcal{P})$ and $\\phi : S \\rightarrow R$ be the $K$-algebra homomorphism defined by $\\phi(x_{ij} ) = s_it_j$, for all $(i, j) \\in V (\\mathcal{P})$ and set $J_\\mathcal{P} = ker(\\phi)$. From Theorem 2.1 of [AAQ2012], we know that $I_{\\mathcal{P}}=J_{\\mathcal{P}}$, if $\\mathcal{P}$ is a weakly connected and convex collection of cells. In such a case, from [OH1999] we get that the generators of $I_{\\mathcal{P}}$ forms the reduced Groebner basis with respect to a suitable order <, and in particular the initial ideal $\\mathrm{in}_<(I_{\\mathcal{P}})$ is squarefree and generated in degree two. \n",
	BR{}, 
	"Following the proof in [OH1999], this routine implements an algorithm which gives the polynomial ring where the monomial order is <.",
	BR{},BR{},
	"If $\\mathcal{P}$ is a weakly connected and convex collection of cells (or in particular a convex polyomino), then the function", TT " polyoRingConvex ", "returns the polynomial ring attached to $\\mathcal{P}$ whose monomial order $<$ is such that $\\mathrm{in}_<(I_{\\mathcal{P}})$ is squarefree and generated in degree two.",
	BR{},BR{},
	ITALIC "Literature \n",
	UL {
	 LI {"[AAQ2012] ", EM "Ideals generated by 2-minors, collections of cells and stack polyominoes ", "(A. A. Qureshi, 2012,  J. Algebra).\n"},
	 LI {"[OH1999] ", EM "Koszul bipartite graphs ", "(H. Ohsugi and T. Hibi, 1999, Adv. Appl. Math. 22,  25-28)."}
	 },
	BR{},
     	ITALIC "Examples \n",
     	BR{},BR{},
	EXAMPLE {
          "Q={{{1, 2}, {2, 3}}, {{2, 2}, {3, 3}}, {{1, 3}, {2, 4}}, {{2, 3}, {3, 4}}, {{2, 4}, {3, 5}}, {{3, 2}, {4, 3}}, {{3, 1}, {4, 2}}};",
          "I = polyoIdeal(Q,RingChoice=>2);",
          "R=ring I;",
          "describe R",
         },
         
        BR{},BR{},
        EXAMPLE {
 	"Q={{{1, 3}, {2, 4}}, {{2, 2}, {3, 3}}, {{2, 3}, {3, 4}}, {{2, 4}, {3, 5}}, {{3, 4}, {4, 5}}, {{3, 3}, {4, 4}}, {{3, 2}, {4, 3}}, {{3, 1}, {4, 2}}, {{3, 5}, {4, 6}}, {{4, 4}, {5, 5}}, {{4, 3}, {5, 4}}, {{5, 4}, {6, 5}}};",
         "I = polyoIdeal(Q,RingChoice=>2);",
         "In= monomialIdeal(leadTerm(I))",
         } , 
    
     SeeAlso =>{polyoIdeal}
     }
     
     
document {
     Key => {Field, [polyoIdeal,Field]},
     Headline => "optional argument for polyoIdeal",
     "Change the base field for the ambient ring of the ideal returned by ", TT "polyoIdeal", ".",
     BR{},BR{},
   	 EXAMPLE {
          "F = GF(81,Variable=>a)",
          "Q={{{1,1},{2,2}},{{2,1},{3,2}},{{2,2},{3,3}}};",
          "I = polyoIdeal(Q,Field=> F,RingChoice=>1,TermOrder=> GRevLex)",
          },
     SeeAlso =>{polyoIdeal}  
     }
          
document {
     Key => {TermOrder, [polyoIdeal,TermOrder]},
     Headline => "optional argument for polyoIdeal",
     "Change the term order for the ambient ring returned by ", TT "polyoIdeal ", "in the case that the option ", TT "RingChoice ", "is equal to 1.",
     BR{},
     "This option can be used just when ", TT "RingChoice => 1.",
     BR{},BR{},
     EXAMPLE {
          "Q={{{1,1},{2,2}},{{2,2},{3,3}},{{3,3},{4,4}}};",
          "I = polyoIdeal(Q,RingChoice=>1,TermOrder=> GRevLex);",
          "R=ring I;",
          "describe R",
          },
     SeeAlso =>{polyoIdeal, RingChoice}
     }
          
      
--------------------------------------------------------------------------------------------------
----------------------------------    TEST   -----------------------------------------------------
--------------------------------------------------------------------------------------------------
    
          

TEST ///

--polyoIdeal test
Q={{{1, 3}, {2, 4}}, {{2, 2}, {3, 3}}, {{2, 3}, {3, 4}}, {{2, 4}, {3, 5}}, {{3, 4}, {4, 5}}, {{3, 3}, {4, 4}}, {{3, 2}, {4, 3}}, {{3, 1}, {4, 2}}, {{3, 5}, {4, 6}}, {{4, 4}, {5, 5}}, {{4, 3}, {5, 4}}, {{5, 4}, {6, 5}}};
I = polyoIdeal Q;
J = ideal(x_(4,2)*x_(3,1)-x_(4,1)*x_(3,2), x_(5,4)*x_(1,3)-x_(5,3)*x_(1,4), x_(4,5)*x_(2,3)-x_(4,3)*x_(2,5), x_(6,5)*x_(5,4)-x_(6,4)*x_(5,5), x_(3,4)*x_(1,3)-x_(3,3)*x_(1,4), x_(4,3)*x_(3,1)-x_(4,1)*x_(3,3), x_(4,5)*x_(2,4)-x_(4,4)*x_(2,5), x_(6,5)*x_(3,4)-x_(6,4)*x_(3,5), x_(3,3)*x_(2,2)-x_(3,2)*x_(2,3), x_(4,4)*x_(3,1)-x_(4,1)*x_(3,4), x_(4,3)*x_(3,2)-x_(4,2)*x_(3,3), x_(3,4)*x_(2,2)-x_(3,2)*x_(2,4), x_(4,5)*x_(3,1)-x_(4,1)*x_(3,5), x_(4,4)*x_(3,2)-x_(4,2)*x_(3,4), x_(5,4)*x_(2,3)-x_(5,3)*x_(2,4), x_(5,4)*x_(4,3)-x_(5,3)*x_(4,4), x_(3,4)*x_(2,3)-x_(3,3)*x_(2,4), x_(3,5)*x_(2,2)-x_(3,2)*x_(2,5), x_(4,6)*x_(3,1)-x_(4,1)*x_(3,6), x_(4,5)*x_(3,2)-x_(4,2)*x_(3,5), x_(4,4)*x_(3,3)-x_(4,3)*x_(3,4), x_(5,5)*x_(2,3)-x_(5,3)*x_(2,5), x_(5,5)*x_(4,3)-x_(5,3)*x_(4,5), x_(3,5)*x_(2,3)-x_(3,3)*x_(2,5), x_(4,6)*x_(3,2)-x_(4,2)*x_(3,6), x_(4,5)*x_(3,3)-x_(4,3)*x_(3,5), x_(5,5)*x_(2,4)-x_(5,4)*x_(2,5), x_(5,5)*x_(4,4)-x_(5,4)*x_(4,5), x_(3,5)*x_(2,4)-x_(3,4)*x_(2,5), x_(4,6)*x_(3,3)-x_(4,3)*x_(3,6), x_(4,5)*x_(3,4)-x_(4,4)*x_(3,5), x_(2,4)*x_(1,3)-x_(2,3)*x_(1,4), x_(4,4)*x_(1,3)-x_(4,3)*x_(1,4), x_(6,5)*x_(2,4)-x_(6,4)*x_(2,5), x_(4,6)*x_(3,4)-x_(4,4)*x_(3,6), x_(6,5)*x_(4,4)-x_(6,4)*x_(4,5), x_(4,3)*x_(2,2)-x_(4,2)*x_(2,3), x_(4,6)*x_(3,5)-x_(4,5)*x_(3,6), x_(5,4)*x_(3,3)-x_(5,3)*x_(3,4), x_(4,4)*x_(2,2)-x_(4,2)*x_(2,4), x_(5,5)*x_(3,3)-x_(5,3)*x_(3,5), x_(4,5)*x_(2,2)-x_(4,2)*x_(2,5), x_(4,4)*x_(2,3)-x_(4,3)*x_(2,4), x_(5,5)*x_(3,4)-x_(5,4)*x_(3,5));
assert(I==J);
///



TEST ///

--polyoMatrix test
Q={{{3, 1}, {4, 2}}, {{4, 1}, {5, 2}}, {{5, 1}, {6, 2}}, {{5, 2}, {6, 3}}, {{5, 3}, {6, 4}}, {{5, 4}, {6, 5}}, {{5, 5}, {6, 6}}, {{4, 5}, {5, 6}}, {{3, 2}, {4, 3}}, {{3, 3}, {4, 4}}, {{2, 3}, {3, 4}}, {{1, 3}, {2, 4}}, {{1, 4}, {2, 5}}, {{1, 5}, {2, 6}}, {{2, 5}, {3, 6}}, {{3, 5}, {4, 6}}};
M = polyoMatrix Q;
m = matrix({{x_(1,6), x_(2,6), x_(3,6), x_(4,6), x_(5,6), x_(6,6)}, {x_(1,5), x_(2,5), x_(3,5), x_(4,5), x_(5,5), x_(6,5)}, {x_(1,4), x_(2,4), x_(3,4), x_(4,4), x_(5,4), x_(6,4)}, {x_(1,3), x_(2,3), x_(3,3), x_(4,3), x_(5,3), x_(6,3)}, {0, 0, x_(3,2), x_(4,2), x_(5,2), x_(6,2)}, {0,  0, x_(3,1), x_(4,1), x_(5,1), x_(6,1)}});
assert(m==M);
///


TEST ///

--polyoToric test
Q={{{1, 1}, {2, 2}}, {{2, 1}, {3, 2}}, {{3, 1}, {4, 2}}, {{3, 2}, {4, 3}}, {{3, 3}, {4, 4}}, {{2, 3}, {3, 4}}, {{1, 3}, {2, 4}}, {{1, 2}, {2, 3}}};
J = polyoToric(Q,{{2,2}});
K = ideal(-x_(2,2)*x_(1,1)+x_(2,1)*x_(1,2), -x_(3,2)*x_(1,1)+x_(3,1)*x_(1,2), -x_(4,2)*x_(1,1)+x_(4,1)*x_(1,2), -x_(2,3)*x_(1,1)+x_(2,1)*x_(1,3), -x_(2,3)*x_(1,2)+x_(2,2)*x_(1,3), -x_(2,4)*x_(1,1)+x_(2,1)*x_(1,4), -x_(2,4)*x_(1,2)+x_(2,2)*x_(1,4), -x_(2,4)*x_(1,3)+x_(2,3)*x_(1,4), -x_(3,4)*x_(1,3)+x_(3,3)*x_(1,4), -x_(4,4)*x_(1,3)+x_(4,3)*x_(1,4), -x_(3,2)*x_(2,1)+x_(3,1)*x_(2,2), -x_(4,2)*x_(2,1)+x_(4,1)*x_(2,2), -x_(3,4)*x_(2,3)+x_(3,3)*x_(2,4), -x_(4,4)*x_(2,3)+x_(4,3)*x_(2,4), -x_(4,2)*x_(3,1)+x_(4,1)*x_(3,2), -x_(4,3)*x_(3,1)+x_(4,1)*x_(3,3), -x_(4,3)*x_(3,2)+x_(4,2)*x_(3,3), -x_(4,4)*x_(3,1)+x_(4,1)*x_(3,4), -x_(4,4)*x_(3,2)+x_(4,2)*x_(3,4), -x_(4,4)*x_(3,3)+x_(4,3)*x_(3,4));
assert(K == J);
///


TEST ///

--polyoToric with polyoIdeal test
Q={{{1, 2}, {2, 3}}, {{2, 2}, {3, 3}}, {{1, 3}, {2, 4}}, {{2, 3}, {3, 4}}, {{2, 4}, {3, 5}}, {{3, 2}, {4, 3}}, {{3, 1}, {4, 2}}};
I = polyoIdeal Q;
J = polyoToric(Q,{});
R = ring I;
J = substitute(J,R);
assert(I == J);
///

end
