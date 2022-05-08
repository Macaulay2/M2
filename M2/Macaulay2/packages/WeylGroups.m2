-- -*- coding: utf-8 -*-
-- licensed under GPL, any version

newPackage(
	"WeylGroups",
	Version => "0.5.2",
	Date => "November 1, 2021",
	Authors => {
		{Name => "Baptiste Calmès",
		HomePage => "http://bcalmes.perso.math.cnrs.fr/"},
                {Name => "Viktor Petrov"}
		},
	Headline => "root systems and Weyl groups",
	Keywords => {"Lie Groups and Lie Algebras"},
	AuxiliaryFiles => true,
	PackageExports => {"Graphics"},
	DebuggingMode => false)

-- Put here the name of functions that should be visible to users
export{
"RootSystem", 
"cartanMatrix", 
"rootSystem", "rootSystemA", "rootSystemB", "rootSystemC", "rootSystemD", "rootSystemE", "rootSystemF4", "rootSystemG2", 
"Weight", 
"weight", 
"Root", 
"isPositiveRoot", "isRoot", "addRoots",
"halfSumOfRoots", "reflect", "simpleRoot", "rootCoefficients", 
"WeylGroupElement", 
"reduce", "reducedDecomposition", "isReduced", "coxeterLength", "longestWeylGroupElement", "positiveRoots", "reflection", "scalarProduct", "eval", "isReflection", "whoseReflection", 
"Parabolic", "WeylGroupLeftCoset", "WeylGroupRightCoset", "WeylGroupDoubleCoset", 
"parabolic", "minimalRepresentative", "isMinimalRepresentative", 
"DynkinDiagram", "DynkinType", 
"dynkinDiagram", "connectedComponents", "endVertices", "dynkinType", "dynkinExponents",
"poincareSeries",
"HasseDiagram", "HasseGraph", 
"hasseDiagramToGraph", "hasseGraphToPicture", "storeHasseGraph", "loadHasseGraph",
"underBruhat", "aboveBruhat",
"isLtBruhat", "intervalBruhat",
"numberOfPositiveRoots", "listWeylGroupElements", "neutralWeylGroupElement"
}

-- Variables that can be modified by the user
exportMutable{
}

-- Package code 

--Defining the class of root systems
RootSystem = new Type of MutableHashTable

--Cartan matrix of a root system
cartanMatrix = method()

protect CartanMatrixTr
protect RootSystemRank

cartanMatrix(RootSystem) := (R) -> transpose R.CartanMatrixTr

--rank of a root system
rank(RootSystem) := (R) -> R.RootSystemRank

--Defining equality of root systems (maybe we should also test the lengths of the roots?)
RootSystem == RootSystem := (R1, R2) -> R1.CartanMatrixTr == R2.CartanMatrixTr

protect ReducedDecompositions
protect WeylGroupList
protect Reflections
protect PositiveRoots
protect RootNorms
protect CartanMatrixTrInv

--Defining the direct sum of two root systems
RootSystem ++ RootSystem := (R1, R2) ->
	(
	new RootSystem from
	{
	RootSystemRank=>R1.RootSystemRank+R2.RootSystemRank,
	CartanMatrixTr=>R1.CartanMatrixTr++R2.CartanMatrixTr,
	CartanMatrixTrInv=>R1.CartanMatrixTrInv++R2.CartanMatrixTrInv,
	RootNorms=>R1.RootNorms | R2.RootNorms,
	PositiveRoots=>set {}, -- we can do more efficiently if R1.PositiveRoots and R2.PositiveRoots are already computed
	Reflections=>new HashTable from {},
	WeylGroupList=>{},
	ReducedDecompositions=>new MutableHashTable from {}
	}
	)

--Defining the class of Dynkin diagrams as a BasicList of elements {{1,3},{},{},{}} representing a vertex. Inside, the lists are the lists of edges towards another vertex, of resp. simple edges, double towards smaller, double towards greater, triple. Of course, most of the time most will be empty. 

DynkinDiagram = new Type of BasicList 

--Finding rank of Dynkin diagram
rank(DynkinDiagram):= (D) -> #D

--(internal function) Finding the labels of the neighbors of the i-th vertex in a Dynkin diagram
neighbors = method()
neighbors(DynkinDiagram,ZZ) := (D,i) -> set join(D#(i-1)#0,D#(i-1)#1,D#(i-1)#2,D#(i-1)#3,D#(i-1)#4)

--(internal function) Reindexing elements according to a list to obtain a coherent Dynkin diagram
reindex=(D,L)->
	(
	T:=new HashTable from for i from 0 to #L-1 list L#i => i+1; 
	new DynkinDiagram from apply(D,x->applyTable(x,z->T#z))
	)

--Finding the connected components of a Dynkin diagram
connectedComponents=method()
connectedComponents(DynkinDiagram) := (D) ->
	(
	vertcompolist:={}; --will contain the sets of vertices of the connected components
	for i from 1 to #D do
	  (
	  verticompo:=set{i}; --set of vertices of connected component of i
	  vertnoticompolist:={}; --list of other connected components vertices sets 
	  for j from 0 to #vertcompolist-1 do if neighbors(D,i)*vertcompolist#j =!=set{} then 
	    (
	    verticompo=vertcompolist#j+verticompo;
	    )
	    else 
            (
	    vertnoticompolist=append(vertnoticompolist,vertcompolist#j);
            );
	  vertcompolist=append(vertnoticompolist,verticompo);
	  );
	vertcompolist=apply(vertcompolist,toList); --transform the connected components into lists
	vertcompolist=apply(vertcompolist,sort); --sort each of these lists 
	dyncompolist:=applyTable(vertcompolist,i->D#(i-1)); --make a list with the edge data of the corresponding vertices
	apply(dyncompolist,vertcompolist,reindex)
	)

--(internal function) Returning the position (in the list) of the small side of a double edge of an irreducible Dynkin diagram if it has one.
smallSideDoubleEdge= (D)->
	(
	for i from 0 to #D-1 do if (D#i#2 !={}) then return({i});
	return({});
	) 

--(internal function) Returning the position (in the list) of the big side of a double edge of an irreducible Dynkin diagram if it has one.
bigSideDoubleEdge= (D)->
	(
	for i from 0 to #D-1 do if (D#i#1 !={}) then return({i});
	return({});	
	) 

--(internal function) Testing if an irreducible Dynkin diagram has a triple edge 
isG2= (D)->
	(
	for i from 0 to #D-1 do if (D#i#3 !={} or D#i#4 !={}) then return(true);
	return(false);
	) 

--(internal function) Returning the number (ie at least 1) of the branch point of an irreducible Dynkin diagram if it has one 
branchPoint= (D)->
	(
	for i from 0 to #D-1 do if (#(D#i#0)>2) then return({i+1});
	return({});	
	) 

--(internal function) Says whether the vertex at position i (as in a list) in the Dynkin diagram D is an end point. 
isEndPoint=(D,i)->
	(
	(#(D#i#0)+#(D#i#1)+#(D#i#2)<2)
	)

--Finds the indices all end points of a Dynkin diagram D
endVertices=method()
endVertices(DynkinDiagram) := (D) ->
	(
	set for i from 0 to #D-1 list if (#(D#i#0)+#(D#i#1)+#(D#i#2)<2) then i+1 else continue
	)

--Defining the class of Dynkin types.
DynkinType = new Type of BasicList

--Defining the sum (disjoint union) of two Dynkin types
DynkinType ++ DynkinType := (T1,T2) -> new DynkinType from join(T1,T2)

--Defining the equality of two Dynkin types
DynkinType == DynkinType := (T1,T2) -> (sort toList T1) === (sort toList T2)

--(internal function) Finding the type of a connected Dynkin diagram (it should not be empty)
connectedDynkinType=method()
connectedDynkinType(DynkinDiagram):=(D)->
	(
	if isG2(D) then {"G",2}
	else
	  (
	  brp:=branchPoint(D);
	  if brp=={} then
	    (
	    sm:=smallSideDoubleEdge(D);
	    bg:=bigSideDoubleEdge(D);
	    if sm=={} and bg=={} then {"A",rank(D)}
	    else
	      (
	      if isEndPoint(D,sm#0) then {"B",rank(D)}
	      else if isEndPoint(D,bg#0) then {"C", rank(D)} else {"F",4}
	      )
	    )
	  else
	    (
	    if #(endVertices(D)*neighbors(D,brp#0))>1 then {"D",rank(D)} else {"E", rank(D)}
	    )
	  )
	)


--Finding the type of any Dynkin diagram. The result is a list of irreducible types.
dynkinType = method()
dynkinType(DynkinDiagram) := (D) ->
new DynkinType from sort apply(connectedComponents(D),connectedDynkinType)

dynkinType(RootSystem) := (R) -> dynkinType(dynkinDiagram(R))

dynkinType(BasicList) := (L) ->
	(
	for i from 0 to #L-1 do if instance(L#i#1,ZZ)==false or L#i#1<1 then error "Second elements must be positive integers.";
	mylist:={};
	for i from 0 to #L-1 do 
	  (
	  H:=L#i#0;
	  if H == "A" then mylist=append(mylist,{"A", L#i#1})
	  else if H == "B" then (if L#i#1==1 then mylist=append(mylist,{"A", 1}) else mylist=append(mylist,{"B", L#i#1}))
	  else if H == "C" then (if L#i#1==1 then mylist=append(mylist,{"A", 1}) else if L#i#1==2 then mylist=append(mylist,{"B", 2}) else mylist=append(mylist,{"C", L#i#1}))
	  else if H == "D" then (if L#i#1==1 then mylist=append(mylist,{"A",1}) else if L#i#1==2 then mylist=join(mylist,{{"A", 1},{"A",1}}) else if L#i#1==3 then mylist=append(mylist,{"A",3}) else mylist=append(mylist,{"D", L#i#1}))
	  else if H == "E" then (if L#i#1<9 and L#i#1>5 then mylist=append(mylist,{"E",L#i#1}) else error "The type E can only have rank 6,7 or 8.")
	  else if H == "F" then (if L#i#1==4 then mylist=append(mylist,{"F",4}) else error "The type F can only have rank 4.")
	  else if H == "G" then (if L#i#1==2 then mylist=append(mylist,{"G",2}) else error "The type G can only have rank 2.")
	  else error "The first elements must be strings containing one letter among A, B, C, D, E, F or G."
	  );
	new DynkinType from mylist
	)

--exponents of a DynkinType (as listed in Bourbaki for irreducibles)
dynkinExponents=method()
dynkinExponents(DynkinType):= (D) ->
	(
	for i from 0 to #D-1 list 
	  (
	  if D#i#0=="A" then toList (1..D#i#1)
	  else if D#i#0=="B" or D#i#0=="C" then apply(toList (1..D#i#1),x->2*x-1)
	  else if D#i#0=="D" then append(apply(toList(1..D#i#1-1),x->2*x-1),D#i#1-1)
	  else if D#i#0=="G" then {1,5}
	  else if D#i#0=="F" then {1,5,7,11}
	  else if D#i#0=="E" then 
	    (if D#i#1==6 then {1,4,5,7,8,11}
	     else if D#i#1==7 then {1,5,7,9,11,13,17}
	     else if D#i#1==8 then {1,7,11,13,17,19,23,29}
	    ) 
	  )
	)

--Finding the Cartan matrix of a Dynkin diagram.
--cartanMatrix(DynkinDiagram) := (D) ->

--Finding the Dynkin diagram of a root system
dynkinDiagram = method()
dynkinDiagram(RootSystem) := (R) ->
	(
	new DynkinDiagram from 
	for i from 0 to R.RootSystemRank-1 list 
	  (
	  simp:={};
	  sdoub:={};
	  bdoub:={};
	  strip:={};
	  btrip:={};
	  for j from 0 to R.RootSystemRank-1 when #simp+2*(#sdoub+#bdoub)+3*#(strip+btrip)<3 do
	    (
	    if R.CartanMatrixTr_(i,j) != 0 then 
	      (
	      if R.CartanMatrixTr_(j,i) == -1 then
		(
		if R.CartanMatrixTr_(i,j) == -1 then simp=append(simp,j+1)
		else if R.CartanMatrixTr_(i,j) == -2 then bdoub={j+1}
		else if R.CartanMatrixTr_(i,j) == -3 then (btrip={j+1}; break)
		)
	      else if R.CartanMatrixTr_(j,i) == -2 then sdoub={j+1}	
	      else if R.CartanMatrixTr_(j,i) == -3 then (strip={j+1}; break)
	      )
	    );
	  {simp,sdoub,bdoub,strip,btrip}
	  )
	)

--(internal function) Defining the root System of a DynkinType.
rootSystemIrred = (Tirr) ->
	(
	if Tirr#0==="A" then rootSystemA(Tirr#1)
	else if Tirr#0==="B" then rootSystemB(Tirr#1)
	else if Tirr#0==="C" then rootSystemC(Tirr#1)
	else if Tirr#0==="D" then rootSystemD(Tirr#1)
	else if Tirr#0==="E" then rootSystemE(Tirr#1)
	else if Tirr#0==="F" then rootSystemF4
	else if Tirr#0==="G" then rootSystemG2
	)

rootSystem = method()
rootSystem(DynkinType):=(T)->
	(
	if #T<1 then new RootSystem from {RootSystemRank => 0, CartanMatrixTr =>(matrix{{1}})^{}_{} ,CartanMatrixTrInv => (matrix{{1_QQ}})^{}_{} , RootNorms=>{}, PositiveRoots => set{}, Reflections => new HashTable from {}, WeylGroupList => {}, ReducedDecompositions => new MutableHashTable from {}}
	else
	  (
	  R:=rootSystemIrred(T#0);
	  for i from 1 to #T-1 do R=R++rootSystemIrred(T#i);
	  R
	  )
	)

--Constructing the root system of a Dynkin diagram
rootSystem(DynkinDiagram) := (D) -> 
	(
	if #D==0 then
	  (
	  new RootSystem from {
	  RootSystemRank => 0, 
	  CartanMatrixTr => (matrix{{1}})^{}_{},
	  CartanMatrixTrInv => (matrix{{1_QQ}})^{}_{}, 
	  RootNorms=>{}, 
	  PositiveRoots => set{}, 
	  Reflections => new HashTable from {}},
	  WeylGroupList => {},
	  ReducedDecompositions => new MutableHashTable from {}
	  )
	else
	  (
	  M:=matrix for i from 0 to #D-1 list
	    for j from 0 to #D-1 list
	      (
	      if i==j then 2 
	      else if (set(D#i#0))#?(j+1) then -1
	      else if (set(D#i#1))#?(j+1) then -1
	      else if (set(D#i#2))#?(j+1) then -2
	      else if (set(D#i#3))#?(j+1) then -1
	      else if (set(D#i#4))#?(j+1) then -3
	      else 0
	      ); 
	  --we are now trying to find the roots that are not of length one (it means they are on the big side of a multiple edge or connected by simple edges to such a thing)
	  length1set:=set{}; --the ones of squared length 1 
	  length2set:=set{}; --the ones of squared length 2
	  length3set:=set{}; --the ones of squared length 3
	  smallend:=set{}; --ends at the small side of a double edge or a triple edge
	  for i from 0 to #D-1 do  --putting the obvious ones where they belong
	    (
	    if #(D#i#4)>0 then (length1set=length1set+set{i+1}; smallend=smallend+set{i+1})
	    else if #(D#i#3)>0 then length3set=length3set+set{i+1} 
	    else if #(D#i#2)>0 then (length1set=length1set+set{i+1}; smallend=smallend+set{i+1})
	    else if #(D#i#1)>0 then length2set=length2set+set{i+1} 
	    else if #(D#i#0)==0 then length1set=length1set+set{i+1} 
	    else if #(D#i#0)==3 then length1set=length1set+set{i+1} 
	    );
	  theends:=toList(endVertices(D)) - (length2set + length3set + smallend); --ends of the diagram still to be treated
	  todecide:=set{}; --connected set whose length we want to find at the current step
	  i:=0;
	  startend:=0;
	  while #theends>0 do
	    (
	    todecide=set{theends#0};
	    startend=set{theends#0};
	    i=D#((theends#0)-1)#0#0; --next point in the simple edge chain. It has to exist and be unique because we start from an end not isolated or it would already have been removed as an obvious point
	    while true do
	      (
	      if length1set#?i then (length1set= length1set + todecide; theends = theends - startend; break)
	      else if length2set#?i then (
		length2set= length2set + todecide; 
		theends = theends - startend; 
		break
		)
	      else if (set(theends))#?i then (
		length1set= length1set + todecide + set{i}; 
		theends = theends - startend -set{i}; 
		break
		)
	      else (
		todecide=todecide + set{i}; 
		i=((D#(i-1)#0)-todecide)#0 
		)
	      )
	    );
	  new RootSystem from {
	  RootSystemRank => #D,
	  CartanMatrixTr => M,
	  CartanMatrixTrInv => inverse promote(M,QQ),
	  RootNorms => for i from 1 to #D list if length2set#?i then 2 else if length3set#?i then 3 else 1,
	  PositiveRoots => set {},
	  Reflections => new HashTable from {},
	  WeylGroupList => {},
	  ReducedDecompositions => new MutableHashTable from {}
	  }
	  )
	)

--Drawing the Dynkin diagram (what do we really want to see?)
--draw = method()
--draw(DynkinDiagram) := (D)->

--the poincare series of a root system (the characteristic function of the number of Weyl group elements per length)
poincareSeries= method()
poincareSeries(RootSystem,RingElement):=(R,x)->
	(
	product apply(dynkinExponents(dynkinType(R)),y->product apply(y,n->(x^(n+1)-1) // (x-1)))
	)

--number of positive roots
numberOfPositiveRoots = method()

numberOfPositiveRoots(DynkinType) := (T) ->
	(
	sum toList apply(T,x->
		(
		if x#0==="A" then (x#1)*((x#1)+1)//2 else
		if x#0==="B" or x#0==="C" then (x#1)*(x#1) else
		if x#0==="D" then (x#1)*((x#1)-1) else
		if x#0==="E" then (if x#1===6 then 36 else if x#1===7 then 63 else 120) else
		if x#0==="F" then 24 else 6
		))
	)

numberOfPositiveRoots(RootSystem) := (R) -> numberOfPositiveRoots dynkinType R

--Defining the class of weights (expressed as vectors in ZZ^n, the basis consisting in the fundumental weights)
Weight = new Type of Vector

--Constructor for weights
weight=method()
weight(RootSystem,BasicList):= (R,L)->
	(
	if #L != R.RootSystemRank then error "The number of coordinates of the weight vector and the rank of the root system should be equal." 
	else for i in L do if not(instance(i,ZZ)) then error "The weight vector should have integer coefficients.";
	if #L==0 then new Weight from 0_(ZZ^0) else new Weight from vector L
	)

weight(RootSystem,Vector):= (R,V)->
	(
	if rank(class(V)) != R.RootSystemRank then error "The number of coordinates of the weight vector and the rank of the root system should be equal." 
	else for i in entries(V) do if not(instance(i,ZZ)) then error "The weight vector should have integer coefficients.";
	if rank(class(V))==0 then new Weight from 0_(ZZ^0) else new Weight from V
	)

--redefining sum of two weights
Weight + Weight := (p1,p2) -> new Weight from ((new Vector from p1)+(new Vector from p2))

--redefining difference of two weights
Weight - Weight := (p1,p2) -> new Weight from ((new Vector from p1)-(new Vector from p2))

--defining opposite to a weight
- Weight := (p) -> (-1)*p

--check whether a weight is a positive root
isPositiveRoot = method()

isPositiveRoot(RootSystem,Weight) := (R,p) -> (positiveRoots(R))#?(new Root from p)

--check whether a weight is a root
isRoot = method()

isRoot(RootSystem,Weight) := (R,p) -> isPositiveRoot(R,p) or isPositiveRoot(R,-p)

--the half-sum of positive roots
halfSumOfRoots = method()

halfSumOfRoots(RootSystem) := (R) ->
	(
	if R.RootSystemRank>0 then new Weight from vector(toList(R.RootSystemRank:1)) else new Weight from 0_(ZZ^0)
	)

reflect = method()

--reflect a weight with respect to n-th simple root
reflect(RootSystem,ZZ,Weight) := (R,n,p) ->
	(
	p-(p_(n-1))*simpleRoot(R,n)
	)

--do several reflections listed in L
reflect(RootSystem,BasicList,Weight) := (R,L,p) ->
	(
	q := p;
	scan(reverse L,i->q=reflect(R,i,q));
	q
	)

--Defining the class of roots (expressed as appropriate weights)
Root = new Type of Weight

--Sum of roots: gives a root if it is a root, or a weight if not.
addRoots = method()

addRoots(RootSystem,Root,Root) := (R,r,q) -> 
	(
	s:=(new Weight from r) + (new Weight from q);
	if isRoot(R,s) then new Root from s else s
	)

--Preventing scalar multiples on roots
ZZ*Root := (n,r) -> if n==1 or n==-1 then new Root from n*new Weight from r else n*new Weight from r

--reflect a root with respect to n-th simple root
reflect(RootSystem,ZZ,Root) := (R,n,r) -> new Root from reflect(R,n,new Weight from r)

--do several reflections listed in L
reflect(RootSystem,BasicList,Root) := (R,L,r) -> new Root from reflect(R,L,new Weight from r)

--n-th simple root
simpleRoot = method()

simpleRoot(RootSystem,ZZ) := (R,n) ->
	(
	new Root from (R.CartanMatrixTr)_(n-1)
	)

--Vector of the coefficients of something at the simple roots
rootCoefficients = method()

--Vector of the coefficients of a root at the simple roots (integers)
rootCoefficients(RootSystem,Root) := (R,r) -> vector(apply(entries(R.CartanMatrixTrInv*r),floor))

--Vector of the coefficients of a weight at the simple roots (rationals)
rootCoefficients(RootSystem,Weight) := (R,v) -> (R.CartanMatrixTrInv*v)

--the squared norm of a root
norm(RootSystem,Root) := (R,r) ->
	(
	x := rootCoefficients(R,r);
	(sum for i from 0 to rank(R)-1 list x_i*(R.RootNorms)#i*r_i)//2
	)

--the scalar product of two weights
scalarProduct = method()

--the scalar product of the i-th fundamental weight with the j-th
scalarProduct(RootSystem,ZZ,ZZ) := (R,i,j) -> 
	(
	((R.CartanMatrixTrInv)_(j-1,i-1))*(R.RootNorms#(j-1))//2
	)

--the scalar product of two general weights
scalarProduct(RootSystem,Weight,Weight) := (R,u,v) -> 
	(
	sum apply(apply(entries((R.CartanMatrixTrInv)*u),entries(v),(x,y)->x*y),R.RootNorms,(x,y)->x*y)//2
	)

--the scalar product of the i-th fundamental weight with a general weight
scalarProduct(RootSystem,ZZ,Weight) := (R,i,v) -> 
	(
	(((R.CartanMatrixTrInv)*v)_(i-1))*(R.RootNorms#(i-1))//2
	)

--the dual of a root evaluated at something 
eval = method()

--the dual of the simple root j evaluated at the simple root i
eval(RootSystem,ZZ,ZZ) := (R,i,j) -> (R.CartanMatrixTr_(j-1,i-1))

--the dual the j-th simple root evaluated at a weight (no consistency test for the dimension of the Weight, and the root system is not really used).
eval(RootSystem,Weight,ZZ) := (R,v,j) -> v_(j-1)

--the dual of a root evaluated at the i-th fundamental weight 
eval(RootSystem,ZZ,Root) := (R,i,r) ->
	(
	floor(2*scalarProduct(R,i,r)//norm(R,r))
	)

--the dual of a root evaluated at a weight (which might in particular be a root) 
eval(RootSystem,Weight,Root) := (R,v,r) -> 
	(
	floor(2*scalarProduct(R,v,r)//norm(R,r))
	)

--Defining the elements of a Weyl group (expressed as pairs {R,p}, where R is a root system and p=w(rho))
WeylGroupElement = new Type of BasicList

--Defining equality of two Weyl group elements
WeylGroupElement == WeylGroupElement := (w1,w2) -> (w1#0==w2#0) and (w1#1==w2#1)

--Defining action of a Weyl group on weights
WeylGroupElement * Weight := (w,p) ->
	(
	reflect(w#0,reducedDecomposition(w),p)
	)

--Defining action of a Weyl group on roots
WeylGroupElement * Root := (w,r) -> new Root from w*(new Weight from r)

--Defining product of two Weyl group elements
WeylGroupElement * WeylGroupElement := (w1,w2) ->
	(
	if w1#0 =!= w2#0 then error "The arguments should be from the Weyl group of the same root system."
	else new WeylGroupElement from {w1#0, w1*(w2#1)}
	)

--Defining inverse to a Weyl group element
inverse(WeylGroupElement) := (w) -> reduce(w#0,reverse(reducedDecomposition(w)))

--Defining powers of a Weyl group element
WeylGroupElement^ZZ := (w,n) ->
	(
	p := halfSumOfRoots(w#0);
	if n==0 then new WeylGroupElement from {w#0,p}
	else if (n==1) or (w#1==p) then w
	else if n>0 then
	(
		t := w^(n//2);
		if n%2 == 0 then t*t else t*t*w
	)
	else inverse(w)^(-n)
	)

--the Weyl group element defined by a product of simple reflections
reduce = method()

reduce(RootSystem,BasicList) := (R,L) ->
	(
	new WeylGroupElement from {R,reflect(R,L,halfSumOfRoots(R))}
	)

--the (lexicographically least) reduced decomposition of w
reducedDecomposition = method()

reducedDecomposition(WeylGroupElement) := (w) ->
	(
	if (w#0).ReducedDecompositions#?(w#1) then (w#0).ReducedDecompositions#(w#1) else (w#0).ReducedDecompositions#(w#1)=
	(
	  n := rank(w#0);
	  i := 0;
	  while (i<n) and ((w#1)_i>0) do i=i+1;
	  if i==n then {} else prepend(i+1,reducedDecomposition(new WeylGroupElement from {w#0, reflect(w#0,i+1,w#1)}))
	)
	)

--check whether a given decomposition is reduced (relies on l(s_i w)>l(w) equivalent to w(\rho)_i >0 ) 
isReduced = method ()
isReduced(RootSystem,BasicList) := (R,L) ->
	(
	b:=true;
	v:=halfSumOfRoots(R);
	for i from 1 to #L-1 do
	  (
	  v=reflect(R,L#(#L-i),v);
	  if v_(L#(#L-i-1)-1)<0 then (b=false; break);
	  );
	b
	)

--check whether the length of the Weyl group element multiplied on the left by the reflection in the list is l(w)+length of list
isReduced(BasicList,WeylGroupElement) := (L,w) ->
	(
	b:=true;
	v:=w#1;
	for i from 1 to #L do
	  (
	  if v_(L#(#L-i)-1)<0 then (b=false; break);
	  v=reflect(w#0,L#(#L-i),v);
	  );
	b
	)

--the length of a Weyl group element
coxeterLength = method ()
coxeterLength(WeylGroupElement) := (w) -> #(reducedDecomposition(w))

--the longest Weyl group element
longestWeylGroupElement = method()

longestWeylGroupElement(RootSystem) := (R) ->
	(
	new WeylGroupElement from {R,-halfSumOfRoots(R)}
	)

--the neutral Weyl group element
neutralWeylGroupElement = method()

neutralWeylGroupElement(RootSystem) := (R) ->
	(
	new WeylGroupElement from {R,halfSumOfRoots(R)}
	)


--set of all positive roots of R
positiveRoots = method()

positiveRoots(RootSystem) := (R) ->
	(
	if R.RootSystemRank==0 then {}
	else if #(R.PositiveRoots)==0 then R.PositiveRoots=(
	L := reducedDecomposition longestWeylGroupElement R;
	set for i from 0 to #L-1 list reflect(R,take(L,{0,i-1}),simpleRoot(R,L#i))
	)
	else R.PositiveRoots
	)

--reflection with respect to a root
reflection = method()

reflection(RootSystem,Root) := (R,r) ->
	(
	x := rootCoefficients(R,r);
	new WeylGroupElement from {R,halfSumOfRoots(R) - ((sum for i from 0 to rank(R)-1 list (x_i*((R.RootNorms)#i)))//norm(R,r)) *r}
	)

--hash table of all reflections
allReflections = method()

allReflections(RootSystem) := (R) ->
	(
	if #(R.Reflections)==0 then R.Reflections=new HashTable from for r in (toList positiveRoots(R)) list ((reflection(R,r))#1)=>r else R.Reflections
	)

--check whether a Weyl group element is a reflection...
isReflection = method()

isReflection(WeylGroupElement) := (w) -> (allReflections(w#0))#?(w#1)

--...and find whose reflection it is

whoseReflection = method()

whoseReflection(WeylGroupElement) := (w) -> (allReflections(w#0))#(w#1)

isLtBruhat = method()

--Checking whether w1 is less than w2 in the Bruhat order
isLtBruhat(WeylGroupElement, WeylGroupElement) := (w1, w2) ->
	(
	  if w1#0 =!= w2#0 then error "The arguments should be from the Weyl group of the same root system." else
	  (
	    n := rank(w2#0);
	    i := 0;
	    while (i<n) and ((w2#1)_i>0) do i=i+1;
	    if i==n then w1==w2 else
	    if (w1#1)_i<0 then isLtBruhat(new WeylGroupElement from {w1#0, reflect(w1#0,i+1,w1#1)}, new WeylGroupElement from {w2#0, reflect(w2#0,i+1,w2#1)})
	    else isLtBruhat(w1, new WeylGroupElement from {w2#0, reflect(w2#0,i+1,w2#1)})
	  )
	)

--Finding all elements just below w for the Bruhat order
underBruhat = method ()
underBruhat(WeylGroupElement) := (w) ->
	(
	L:=reducedDecomposition(w);
	M:={};
	result:={};
	len:=#L;
	b:=true;
	u:=local u; --the potential element to keep if it has the right length
	s:=local s; --simple reflexion to be removed from the reduced dec. of w
	v:=halfSumOfRoots(w#0); --the end part of the dec. of w, stored as how it acts on the half sum of roots
	for i from 1 to len do
	  (
	  s=L#(#L-1);
	  L=drop(L,-1);
	  b=true;
	  u=v;
	  for j from 1 to #L do --testing if the decomposition with the reflexion s removed is reduced
	    (
	    if u_(L#(#L-j)-1)<0 then (b=false; break);
	    u=reflect(w#0,L#(#L-j),u);
	    );
	  v=reflect(w#0,{s},v);
	  if b then result=append(result,{new WeylGroupElement from {w#0,u},whoseReflection(new WeylGroupElement from {w#0,reflect(w#0,M,v)})});
	  M=append(M,s);
	  );
	result
	)

--Finding elements that are below a list of elements (assumed of the same length)
--Maybe we should check if all elements in L have the same length and are in the same root system, otherwise, it is not very natural.
underBruhat(BasicList):=(L)->
	(
	result:=new MutableHashTable from {};
	U:=local U;
	for i from 0 to #L-1 do
	  (
	  U=underBruhat(L#i); --find the elements below this one.
	  for j from 0 to #U-1 do
		if result#?(U#j#0) then result#(U#j#0)=append(result#(U#j#0),{i,U#j#1})
		else result#(U#j#0)={{i,U#j#1}};
	  );
	apply(pairs result,(x,y)->{x,y})
	)

--the following is not debugged, it produces a weird output.
--underBruhat(BasicList,ZZ):= (L,n) ->
--	(
--	if n<=0 then {} 
--	else
--	  (
--	  L2 := {apply(L,x->{x,{}})};
--	  for i from 1 to n do L2=append(L2,underBruhat(apply(L2#(i-1),x->x#0)));
--	  L2
--	  )
--	)

--Finding all elements just above w for the Bruhat order
aboveBruhat = method ()
aboveBruhat(WeylGroupElement) := (w) ->
	(
	w0:=longestWeylGroupElement w#0;
	apply(underBruhat(w0*w),x->{w0*x#0,x#1})
	)

--like underBruhat (see above), but with elements above.
aboveBruhat(BasicList):=(L)->
	(
	result:=new MutableHashTable from {};
	U:=local U;
	for i from 0 to #L-1 do
	  (
	  U=aboveBruhat(L#i); --find the elements above this one.
	  for j from 0 to #U-1 do
		if result#?(U#j#0) then result#(U#j#0)=append(result#(U#j#0),{i,U#j#1})
		else result#(U#j#0)={{i,U#j#1}};
	  );
	apply(pairs result,(x,y)->{x,y})
	)

--the following is not debugged, it produces a weird output.
--aboveBruhat(BasicList,ZZ):= (L,n) ->
--	(
--	if n<=0 then {} 
--	else
--	  (
--	  L2 := {apply(L,x->{x,{}})};
--	  for i from 1 to n do L2=append(L2,aboveBruhat(apply(L2#(i-1),x->x#0)));
--	  L2
--	  )
--	)

--(internal function) extract elements above given ones in a list of lists produced by apply underBruhat repeatedly 
extractHasse = (A,P) ->
	(
	newA := {};
	mypos := P;
	l:=#A;
	smallerlist := local smallerlist;
	if mypos == {} then
	  (
	  for i from 0 to #A-1 list {}
	  )
	else
	  (
	  for i from 1 to #A do
	    (
	    smallerlist = (A#(l-i))_mypos;
	    mypos =sort toList( sum apply(smallerlist,y->set apply(y#1,x->x#0))); --actually this sum is a set union: we are extracting the indices for the next row.
	    newA=prepend(
	      apply(smallerlist,w->{w#0,apply(w#1,x->{position(mypos,y->(y==x#0)),x#1})}),
	      newA); --we replace the references in the current row to the new positions in the next row and we append this current row to our new big list.
	    );
	  newA
	  )
	)

--(internal function) reverse the links in a list of lists describing a Hasse diagram. In the argument, the links point to the previous row, whereas in the output, the links point towards the next row.
reverseLinks = (A)->
	(
	myrow := local myrow;
	l:=#A;
	results := {apply(A#(l-1),x->{x#0,{}})};
	for i from 2 to l do
	  (
	  myrow = new MutableList from apply(A#(l-i),x->{x#0,{}});
	  for j from 0 to #(A#(l-i+1))-1 do
	    (
	    apply(A#(l-i+1)#j#1, x->(myrow#(x#0) = {myrow#(x#0)#0,append(myrow#(x#0)#1,{j,x#1})}));
	    );
	  results=prepend(toList(myrow),results)
	  );
	results
	)

--list all elements of the Weyl group of a given length
listWeylGroupElements = method()

listWeylGroupElements(RootSystem,ZZ) := (R,k) ->
	(
	if k<0 then error "The length should be nonnegative";
	n := (numberOfPositiveRoots R)-k;
	if n<0 then {} else
	if n<k then (
		w := longestWeylGroupElement R; apply(listWeylGroupElements(R,n),x->w*x)
	) else
	(
		S := local S;
		if #(R.WeylGroupList)==0 then R.WeylGroupList = {{halfSumOfRoots R}};
		for i from #(R.WeylGroupList)-1 to k-1 do 
		  (
		  S = set {};
		  apply((R.WeylGroupList)#i, x->(
			for j from 0 to R.RootSystemRank-1 do if x_j>0 then S = S + set {reflect(R,j+1,x)}
		  ));
		  R.WeylGroupList = append(R.WeylGroupList,toList S);
		  );
		apply((R.WeylGroupList)#k,x->new WeylGroupElement from {R,x})
	)
	)

--Defining the class of parabolic subgroups in a Weyl group, represented by the set of generators
Parabolic = new Type of Set

--Constructing a parabolic subgroup out of a root system and a set of roots. This just checks that the roots selected have numbers that are less than the rank of the root system.
parabolic = method()
parabolic(RootSystem,Set) := (R,S) ->
	(
	for i in toList S do if not (instance(i,ZZ) and i>0 and i<=R.RootSystemRank) then error "The indices designating the simple roots in the parabolic need to be integers at least 1 and at most the rank of the root system.";
	new Parabolic from S
	)

--equality of parabolics
Parabolic == Parabolic := (P1,P2) -> P1===P2

--Finding the Dynkin Diagram of the Levy sugroup of a parabolic sugroup from the Dynkin diagram of the group.
dynkinDiagram(DynkinDiagram,Parabolic) := (D,P) -> 
	(
	L:=sort(toList P);
	newdynkin:=for i from 0 to #L-1 list D#((L#i)-1);
	newdynkin=apply(newdynkin,v->apply(v,x->select(x,i->P#?i))); --suppresses edges towards vertices not in P 
	reindex(newdynkin,L)
	)

--the poincare series of a parabolic of a root system (the characteristic function of the number of elements per length in the set of minimal representents of the Weyl group modulo the root systemp of the parabolic.
poincareSeries(RootSystem,Parabolic,RingElement):=(R,P,x)->
	(
	poincareSeries(R,x) // poincareSeries(rootSystem(R,P),x)
	)

--Defining the class of Hasse diagrams as BasicList of {{w,{{1,v1},{3,v3}...}},...} representing a row of a Hasse diagram, with vertices w in the Weyl group and the links to the next row, together with the reflections of the link. 

HasseDiagram = new Type of BasicList 

--(not used anymore, another algorithm below) compute the interval between two Weyl group elements for the Bruhat order, i.e. the set of elements w such that u<=w<=v together with the reflections involved. This is done by finding successively the "cone" of all elements i steps below u and above v until these two cones can intersect (have a row in the same length). Whether we go down from u or up from v is decided at each iteration (variable fromtop) according to what seems to have the least number of elements in the last computed row. Then, when the two computed cones arrive at a length where they can intersect, one computes if they actually do, and one goes back up (and down) using their intersection finding all elements above this intersection or below it that are in the cones.
--intervalBruhat = method()
--intervalBruhat(WeylGroupElement,WeylGroupElement):= (u,v) ->
--	(
--	if u#0 =!= v#0 then error "The two elements must live in the same Weyl group."
--	else
--	(
--	lendiff := coxeterLength(v)-coxeterLength(u);
--	if lendiff<0 then new HasseDiagram from {}
--	else
--	  (
--	  fromtop := true;
--	  toppart :=new MutableList from {{{v,{}}}};
--	  topindex := 0;
--	  bottompart :=new MutableList from {{{u,{}}}};
--	  bottomindex := 0;
--	  pos:=local pos;
--	  while lendiff - topindex - bottomindex >0 do
--	    (
--	    if fromtop then
--	      (
--	      toppart#(topindex+1)=underBruhat(apply(toppart#topindex,x->x#0));
--	      topindex=topindex+1;
--	      )
--	    else
--	      (
--	      bottompart#(bottomindex+1)=aboveBruhat(apply(bottompart#bottomindex,x->x#0));
--	      bottomindex=bottomindex+1;
--	      );
--	    fromtop = (#(toppart#topindex)<=#(bottompart#bottomindex));
--	    );
--	  toprow := apply(toppart#topindex,x->x#0);
--	  bottomrow := apply(bottompart#bottomindex,x->x#0);
--	  poslist :={};
--	  newbottomrow := {};
--	  for i from 0 to #bottomrow-1 do
--	    (
--	    pos = position(toprow,x->(x==bottomrow#i));
--	    if pos =!= null then
--	      (
--	      poslist = append(poslist,pos);
--	      newbottomrow = append(newbottomrow,bottompart#bottomindex#i);
--	      )
--	    );
--	  bottompart#bottomindex = newbottomrow; --replacing the top row of the bottom cone by the elements that are also in the bottom row of the top cone.
--	  newbottompart := extractHasse(bottompart,toList(0..#(bottompart#bottomindex)-1));
--	  newtoppart := extractHasse(toppart,poslist);
--	  newtoppart = drop(reverseLinks(newtoppart),-1);
--	  new HasseDiagram from newtoppart|reverse(newbottompart)
--	  )
--	)
--	)

--compute the interval between two Weyl group elements for the Bruhat order, i.e. the set of elements w such that u<=w<=v together with the reflections involved.  It starts from the top element v, and recursively generates elements under, but checks for each element that it is indeed above u by using isLtBruhat. If it is not, it is discarded right away. The advantage over intervalBruhat is that lots of elements are discarded, but on the other hand it takes some time to apply isLtBruhat on every element.

intervalBruhat = method()
intervalBruhat(WeylGroupElement,WeylGroupElement):= (u,v) ->
	(
	if u#0 =!= v#0 then error "The two elements must live in the same Weyl group."
	else
	(
	lendiff := coxeterLength(v)-coxeterLength(u);
	if lendiff<0 or isLtBruhat(u,v)==false then new HasseDiagram from {}
	else	  
	  (
	  myinterval:=new MutableList from {{{v,{}}}};
	  rowindex := 0;
	  while lendiff>rowindex do
	    (
	    myinterval#(rowindex+1)=select(underBruhat(apply(myinterval#rowindex,x->x#0)),y->isLtBruhat(u,y#0));
	    rowindex=rowindex+1;
	    );
	  new HasseDiagram from reverseLinks(myinterval)
	  )
	)
	)

--computing the poincare series of a hasse diagram.
poincareSeries(HasseDiagram,RingElement):=(H,x)->
	(
	sum for i from 0 to #H-1 list #(H#i)*x^i
	)

--Defining the class of Hasse graphs. It is a bit the same thing as Hasse diagrams, except that it is intended for graphic display, so vertices and links are just strings (that will be used in the graphic). So a typical row looks like {{"1",{2,"121"},{},...},...}. The labels can be anything.

HasseGraph = new Type of BasicList

--Turning a Hasse diagram into a Hasse graph minimal implementation with no labels.
hasseDiagramToGraph = method(Options => true)
hasseDiagramToGraph(HasseDiagram) := {"labels"=>""} >> opts -> (D) ->
	(
	if (opts#"labels")==="reduced decomposition" then
	  new HasseGraph from 
	  for i from 0 to #D-1 list
	    apply(D#i,x->{concatenate(apply(reducedDecomposition(x#0),toString)),apply(x#1,y->{concatenate(apply(reducedDecomposition(reflection(x#0#0,y#1)),toString)),y#0})})
	else
	  (
	  new HasseGraph from 
	  for i from 0 to #D-1 list
	    apply(D#i,x->{"",apply(x#1,y->{"",y#0})})
	  )
	)

--making graphic elements for the package "Graphics" out of a HasseGraph (and return them in a Picture object). No labels for the moment.
hasseGraphToPicture = method(Options=>true)
hasseGraphToPicture(HasseGraph) := {"top margin"=>100,"left margin"=>100,"horizontal space"=>100,"row space"=>100,"tag distance"=>20,"point radius"=>2,"point options"=>new HashTable from {"fill"=>"black"}, "edge options" => new HashTable from {"stroke-width"=>"2"}, "tag options"=>new HashTable from {}} >> opts -> (G) ->
	(
	topmargin:=opts#"top margin";
	leftmargin:=opts#"left margin";
	interpoint:= opts#"horizontal space";
	interrow:= opts#"row space";
	pointtagdist:=opts#"tag distance";
	--edgetagdist:=20;
	pointrad:= opts#"point radius";
	--linewidth:= opts#"edge width";
	rowsize:=toList(apply(G,x->#x));
	maxpoints:=max(rowsize);
	optionspoints:= opts#"point options"; 
	optionstags:= opts#"tag options"; 
	optionslinks:= opts#"edge options";
	mypoints := flatten for i from 0 to #G-1 list
	  (
	  flatten for j from 0 to #(G#i)-1 list
	    {formatGraphicPrimitives({circle(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),pointrad)}, optionspoints),
	    formatGraphicPrimitives({textTag(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),G#i#j#0)}, optionstags)}
	  ); 
	mylinks := flatten for i from 0 to #G-1 list
	  flatten for j from 0 to #(G#i)-1 list
	    flatten for k from 0 to #(G#i#j#1)-1 list
	      formatGraphicPrimitives({segment(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),point(round(0.0+leftmargin+((maxpoints-rowsize#(i+1))/2 +(G#i#j#1#k#1))*interpoint),topmargin+(i+1)*interrow))},optionslinks);
	picture(mylinks|mypoints)
	)

--	for i from 0 to #G-1 do
--	  for j from 0 to #(G#i)-1 do 
--	    (
--	    myPicture = append(myPicture,formatGraphicPrimitive(
--		point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow), 
--		new HashTable from {"fill"=>"black","r"=>toString(pointrad)}
--		));
--	    myPicture = append(myPicture,formatGraphicPrimitive(
--		textTag(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),G#i#j#0), 
--		new HashTable from {}
--		));
--
--	    for k from 0 to #(G#i#j#1)-1 do
--	      (
--	      myPicture = append(myPicture,formatGraphicPrimitive(
--		segment(point(round(0.0+leftmargin+((maxpoints-rowsize#i)/2 +j)*interpoint),topmargin+i*interrow),point(round(0.0+leftmargin+((maxpoints-rowsize#(i+1))/2 +(G#i#j#1#k#1))*interpoint),topmargin+(i+1)*interrow)),
--		new HashTable from {"stroke-width"=>"toString(linewidth)"}
--		));
--	      );
--	    );
--	myPicture

--Finding the root system of the Levi part of a parabolic subgroup.
rootSystem(RootSystem,Parabolic) := (R,P) ->
	(
	L:=sort(toList P) - (toList ((#P):1));
	M:= if #P>0 then submatrix(R.CartanMatrixTr,L,L) else (matrix{{1}})^{}_{};
	new RootSystem from {
	RootSystemRank => #P,
	CartanMatrixTr => M,
	CartanMatrixTrInv => inverse(promote(M,QQ)), 
	RootNorms => apply(L,x->(R.RootNorms)#x), 
	PositiveRoots => set{}, --This should be made more efficient. Later...
	Reflections => new HashTable from {}, --This I don't know how to make efficient for the moment.
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}
	)

--Checking whether a root is in the sub root system of the parabolic.
isRoot(RootSystem,Parabolic,Root) := (R,P,r) ->
	(
	C:=toList(set (1..R.RootSystemRank) - P);
	C=C-toList(#C:1);
	zeroList:=(#C:0);
	toSequence((entries(rootCoefficients(R,r)))_C)===zeroList
	)

--Finding the positive roots of the root system that are in the subroot system of P 
positiveRoots(RootSystem,Parabolic) := (R,P) ->
	(
	C:=toList(set (1..R.RootSystemRank) - P);
	C=C-toList(#C:1);
	zeroList:=(#C:0);
	set select(toList positiveRoots(R),r->toSequence((entries(rootCoefficients(R,r)))_C)==zeroList)
	)



--Defining the class of left cosets of the form wW_P, represented by the list {P,w}, w being of minimal length
WeylGroupLeftCoset = new Type of BasicList

--Defining equality of left cosets
WeylGroupLeftCoset == WeylGroupLeftCoset := (c1, c2) -> isSubset(c1#0,c2#0) and isSubset(c2#0,c1#0) and (c1#1==c2#1)

--the minimal representative of a left coset
minimalRepresentative = method()

minimalRepresentative(WeylGroupLeftCoset) := (c) -> c#1

--Defining the class of right cosets of the form W_Pw, represented by the list {P,w}, w being of minimal length
WeylGroupRightCoset = new Type of BasicList

--Defining equality of right cosets
WeylGroupRightCoset == WeylGroupRightCoset := (c1, c2) -> isSubset(c1#0,c2#0) and isSubset(c2#0,c1#0) and (c1#1==c2#1)

--the minimal representative of a right coset
minimalRepresentative(WeylGroupRightCoset) := (c) -> c#1

--Defining the class of double cosets of the form W_PwW_Q, represented by the list {P,Q,w}, w being of minimal length
WeylGroupDoubleCoset = new Type of BasicList

--Defining equality of double cosets
WeylGroupDoubleCoset == WeylGroupDoubleCoset := (c1, c2) -> isSubset(c1#0,c2#0) and isSubset(c2#0,c1#0) and isSubset(c1#1,c2#1) and isSubset(c2#1,c1#1) and (c1#2==c2#2)

--the minimal representative of a double coset
minimalRepresentative(WeylGroupDoubleCoset) := (c) -> c#2

--compute the left coset wW_P
WeylGroupElement % Parabolic := (w, P) -> new WeylGroupLeftCoset from {P,inverse ((P % inverse(w))#1)}

--compute the right coset W_Pw
Parabolic % WeylGroupElement := (P, w) ->
	(
	n := rank(w#0);
	t := w#1;
	i := local i;
	while true do (
	i = 0;
	while (i<n) and (not member(i+1,P) or (t_i>0)) do i=i+1;
	if i==n then break;
	t = reflect(w#0,i+1,t);
	);
	new WeylGroupRightCoset from {P,new WeylGroupElement from {w#0,t}}
	)

--check whether an element of the Weyl group is the minimal representative of a left coset
isMinimalRepresentative = method()

isMinimalRepresentative(WeylGroupElement,Parabolic) := (w, P) -> isMinimalRepresentative(P, inverse w)

--check whether an element of the Weyl group is the minimal representative of a right coset
isMinimalRepresentative(Parabolic,WeylGroupElement) := (P, w) -> all(toList P,i -> (w#1)_(i-1) > 0)

--check whether an element of the Weyl group is the minimal representative of a double coset
isMinimalRepresentative(Parabolic,WeylGroupElement,Parabolic) := (P, w, Q) -> isMinimalRepresentative(P,w) and isMinimalRepresentative(w,Q)

--compute the double coset
Parabolic % WeylGroupLeftCoset := (P, c) -> new WeylGroupDoubleCoset from {P, c#0, (P % (c#1))#1}

WeylGroupRightCoset % Parabolic := (c, Q) -> new WeylGroupDoubleCoset from {c#0, Q, ((c#1) % Q)#1}

--action of the Weyl group on left cosets
WeylGroupElement * WeylGroupLeftCoset := (w, c) -> new WeylGroupLeftCoset from {c#0, ((w*(c#1))%(c#0))#1}

--action of the Weyl group on right cosets
WeylGroupRightCoset * WeylGroupElement := (c, w) -> new WeylGroupRightCoset from {c#0, ((c#0)%((c#1)*w))#1}

--longest element of a parabolic sugroup of the Weyl group (as an element of the Weyl group)
longestWeylGroupElement(RootSystem,Parabolic) := (R,P) ->
	(
	longestWeylGroupElement(R)*inverse(minimalRepresentative(P % longestWeylGroupElement(R)))
	)

--parabolic associated to a double coset (as in Finite Groups of Lie Type, p. 65, thm 2.7.4)
parabolic(WeylGroupDoubleCoset) := (w) ->
	(
	R:=w#2#0;
	P:=w#0;
	Q:=w#1;
	v:=inverse(w#2);
	r:= local r;
	new Parabolic from set(select(toList P,i->(r=v*simpleRoot(R,i); isPositiveRoot(R,r) and isRoot(R,P,r))))
	)

--compute a Bruhat interval for the Bruhat order on W/W_P.
intervalBruhat(WeylGroupLeftCoset,WeylGroupLeftCoset):= (u,v) ->
	(
	if u#0 =!= v#0 or u#1#0 =!= v#1#0 then error "The cosets must live in the same Weyl group (same root system) and be modulo the same parabolic."	
	else
	(
	lendiff := coxeterLength(v#1)-coxeterLength(u#1);
	if lendiff<0 or isLtBruhat(u#1,v#1)==false then new HasseDiagram from {}
	else	  
	  (
	  myinterval:=new MutableList from {{{v#1,{}}}};
	  rowindex := 0;
	  while lendiff>rowindex do
	    (
	    myinterval#(rowindex+1)=select(underBruhat(apply(myinterval#rowindex,x->x#0)),y->(isLtBruhat(u#1,y#0) and isMinimalRepresentative(y#0,u#0)));
	    rowindex=rowindex+1;
	    );
	  new HasseDiagram from reverseLinks(myinterval)
	  )
	)
	)

--not used anymore, new algorithm above 
----compute a Bruhat interval for the Bruhat order on W/W_P.
--intervalBruhat(WeylGroupLeftCoset,WeylGroupLeftCoset):= (u,v) ->
--	(
--	if u#0 =!= v#0 or u#1#0 =!= v#1#0 then error "The cosets must live in the same Weyl group (same root system) and be modulo the same parabolic."
--	else
--	(
--	P:= v#0;
--	lendiff := coxeterLength(v#1)-coxeterLength(u#1);
--	if lendiff<0 then new HasseDiagram from {}
--	else
--	  (
--	  fromtop := true;
--	  toppart :=new MutableList from {{{v#1,{}}}};
--	  topindex := 0;
--	  bottompart :=new MutableList from {{{u#1,{}}}};
--	  bottomindex := 0;
--	  pos:=local pos;
--	  while lendiff - topindex - bottomindex >0 do
--	    (
--	    if fromtop then
--	      (
--	      toppart#(topindex+1)=select(underBruhat(apply(toppart#topindex,x->x#0)),y->isMinimalRepresentative(y#0,P));
--	      topindex=topindex+1;
--	      )
--	    else
--	      (
--	      bottompart#(bottomindex+1)=select(aboveBruhat(apply(bottompart#bottomindex,x->x#0)),y->isMinimalRepresentative(y#0,P));
--	      bottomindex=bottomindex+1;
--	      );
--	    fromtop = (#(toppart#topindex)<=#(bottompart#bottomindex));
--	    );
--	  toprow := apply(toppart#topindex,x->x#0);
--	  bottomrow := apply(bottompart#bottomindex,x->x#0);
--	  poslist :={};
--	  newbottomrow := {};
--	  for i from 0 to #bottomrow-1 do
--	    (
--	    pos = position(toprow,x->(x==bottomrow#i));
--	    if pos =!= null then
--	      (
--	      poslist = append(poslist,pos);
--	      newbottomrow = append(newbottomrow,bottompart#bottomindex#i);
--	      )
--	    );
--	  bottompart#bottomindex = newbottomrow; --replacing the top row of the bottom cone by the elements that are also in the bottom row of the top cone.
--	  newbottompart := extractHasse(bottompart,toList(0..#(bottompart#bottomindex)-1));
--	  newtoppart := extractHasse(toppart,poslist);
--	  newtoppart = drop(reverseLinks(newtoppart),-1);
--	  new HasseDiagram from newtoppart|reverse(newbottompart)
--	  )
--	)	
--	)

--compute a Bruhat interval for the Bruhat order on W_P\W.
intervalBruhat(WeylGroupRightCoset,WeylGroupRightCoset):= (u,v) ->
	(
	if u#0 =!= v#0 or u#1#0 =!= v#1#0 then error "The cosets must live in the same Weyl group (same root system) and be modulo the same parabolic."	
	else
	(
	lendiff := coxeterLength(v#1)-coxeterLength(u#1);
	if lendiff<0 or isLtBruhat(u#1,v#1)==false then new HasseDiagram from {}
	else	  
	  (
	  myinterval:=new MutableList from {{{v#1,{}}}};
	  rowindex := 0;
	  while lendiff>rowindex do
	    (
	    myinterval#(rowindex+1)=select(underBruhat(apply(myinterval#rowindex,x->x#0)),y->(isLtBruhat(u#1,y#0) and isMinimalRepresentative(u#0,y#0)));
	    rowindex=rowindex+1;
	    );
	  new HasseDiagram from reverseLinks(myinterval)
	  )
	)
	)

--no longer used, new algo above.
----compute a Bruhat interval for the Bruhat order on W_P\W.
--intervalBruhat(WeylGroupRightCoset,WeylGroupRightCoset):= (u,v) ->
--	(
--	if u#0 =!= v#0 or u#1#0 =!= v#1#0 then error "The cosets must live in the same Weyl group (same root system) and be modulo the same parabolic."
--	else
--	(
--	P:= v#0;
--	lendiff := coxeterLength(v#1)-coxeterLength(u#1);
--	if lendiff<0 then new HasseDiagram from {}
--	else
--	  (
--	  fromtop := true;
--	  toppart :=new MutableList from {{{v#1,{}}}};
--	  topindex := 0;
--	  bottompart :=new MutableList from {{{u#1,{}}}};
--	  bottomindex := 0;
--	  pos:=local pos;
--	  while lendiff - topindex - bottomindex >0 do
--	    (
--	    if fromtop then
--	      (
--	      toppart#(topindex+1)=select(underBruhat(apply(toppart#topindex,x->x#0)),y->isMinimalRepresentative(P,y#0));
--	      topindex=topindex+1;
--	      )
--	    else
--	      (
--	      bottompart#(bottomindex+1)=select(aboveBruhat(apply(bottompart#bottomindex,x->x#0)),y->isMinimalRepresentative(P,y#0));
--	      bottomindex=bottomindex+1;
--	      );
--	    fromtop = (#(toppart#topindex)<=#(bottompart#bottomindex));
--	    );
--	  toprow := apply(toppart#topindex,x->x#0);
--	  bottomrow := apply(bottompart#bottomindex,x->x#0);
--	  poslist :={};
--	  newbottomrow := {};
--	  for i from 0 to #bottomrow-1 do
--	    (
--	    pos = position(toprow,x->(x==bottomrow#i));
--	    if pos =!= null then
--	      (
--	      poslist = append(poslist,pos);
--	      newbottomrow = append(newbottomrow,bottompart#bottomindex#i);
--	      )
--	    );
--	  bottompart#bottomindex = newbottomrow; --replacing the top row of the bottom cone by the elements that are also in the bottom row of the top cone.
--	  newbottompart := extractHasse(bottompart,toList(0..#(bottompart#bottomindex)-1));
--	  newtoppart := extractHasse(toppart,poslist);
--	  newtoppart = drop(reverseLinks(newtoppart),-1);
--	  new HasseDiagram from newtoppart|reverse(newbottompart)
--	  )
--	)	
--	)

--Root system of type A
rootSystemA = (n) ->
	(
	if not instance(n,ZZ) then error "The argument should be an integer." else
	if n<=0 then error "The argument should be positive." else
	(
	M := matrix(
	for i from 1 to n list
	for j from 1 to n list if (j<i-1) or (j>i+1) then 0 else if j==i then 2 else -1
	);
	new RootSystem from {
	RootSystemRank => n,
	CartanMatrixTr => M,
	CartanMatrixTrInv => inverse promote(M,QQ),
	RootNorms => toList (n:1),
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}
	)
	)

--Root system of type B
rootSystemB = (n) ->
	(
	if not instance(n,ZZ) then error "The argument should be an integer." else
	if n<=0 then error "The argument should be positive." else
	(
	M := matrix(
	for i from 1 to n list
	for j from 1 to n list if (i==n) and (j==n-1) then -2 else if (j<i-1) or (j>i+1) then 0 else if j==i then 2 else -1
	);
	new RootSystem from {
	RootSystemRank => n,
	CartanMatrixTr => M,
	CartanMatrixTrInv => inverse promote(M,QQ),
	RootNorms => append(toList((n-1):2),1),
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}
	)
	)


--Root system of type C
rootSystemC = (n) ->
	(
	if not instance(n,ZZ) then error "The argument should be an integer" else
	if n<=0 then error "The argument should be positive" else
	(
	M := matrix(
	for i from 1 to n list
	for j from 1 to n list if (i==n-1) and (j==n) then -2 else if (j<i-1) or (j>i+1) then 0 else if j==i then 2 else -1
	);
	new RootSystem from {
	RootSystemRank => n,
	CartanMatrixTr => M,
	CartanMatrixTrInv => inverse promote(M,QQ),
	RootNorms => append(toList((n-1):1),2),
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}
	)
	)

--Root system of type D
rootSystemD = (n) ->
	(
	if not instance(n,ZZ) then error "The argument should be an integer" else
	if n<=0 then error "The argument should be positive" else
	(
	M := matrix(
	for i from 1 to n list
	for j from 1 to n list if ((i==n-2) and (j==n)) or ((i==n) and (j==n-2)) then -1 else
	if ((i==n-1) and (j==n)) or ((i==n) and (j==n-1)) or (j<i-1) or (j>i+1) then 0 else if j==i then 2 else -1
	);
	new RootSystem from {
	RootSystemRank => n,
	CartanMatrixTr => M,
	CartanMatrixTrInv => inverse promote(M,QQ),
	RootNorms => toList(n:1),
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}
	)
	)

--Root system of type E
rootSystemE = (n) ->
	(
	if not instance(n,ZZ) then error "The argument should be an integer" else
	if (n<6) or (n>8) then error "The argument should be equal to 6, 7, or 8" else
	(
	M := matrix(
	for i from 1 to n list
	for j from 1 to n list if ((i==1) and (j==3)) or ((i==3) and (j==1)) or ((i==2) and (j==4)) or ((i==4) and (j==2)) then -1 else
	if ((i==1) and (j==2)) or ((i==2) and (j==1)) or ((i==2) and (j==3)) or ((i==3) and (j==2)) or (j<i-1) or (j>i+1) then 0 else if j==i then 2 else -1
	);
	new RootSystem from {
	RootSystemRank => n,
	CartanMatrixTr => M,
	CartanMatrixTrInv => inverse promote(M,QQ),
	RootNorms => toList(n:1),
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}
	)
	)

--Root system of type F4
rootSystemF4 = new RootSystem from
	{
	RootSystemRank => 4,
	CartanMatrixTr => matrix{{2,-1,0,0},{-1,2,-1,0},{0,-2,2,-1},{0,0,-1,2}},
	CartanMatrixTrInv => inverse promote(matrix{{2,-1,0,0},{-1,2,-1,0},{0,-2,2,-1},{0,0,-1,2}},QQ),
	RootNorms => {2,2,1,1},
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}

--Root system of type G2
rootSystemG2 = new RootSystem from
	{
	RootSystemRank => 2,
	CartanMatrixTr => matrix{{2,-3},{-1,2}},
	CartanMatrixTrInv => inverse promote(matrix{{2,-3},{-1,2}},QQ),
	RootNorms => {1,3},
	PositiveRoots => set {},
	Reflections => new HashTable from {},
	WeylGroupList => {},
	ReducedDecompositions => new MutableHashTable from {}
	}

--Will store a Hasse graph in a file.
storeHasseGraph=method()
storeHasseGraph(HasseGraph,String) := (H,filename) ->
	(
	filename << (new List from H) << endl << close
	)

--Will extract a hasse graph stored as a list in a file.
loadHasseGraph=method()
loadHasseGraph(String):=(filename)->
	(
	mylist:=value get filename;
	new HasseGraph from
	for i from 0 to #mylist-1 list
	apply(mylist#i,x->{if x#0===null then "" else toString(x#0),apply(x#1,y->{toString(y#0),y#1})})
	)


--The rest of the file is documentation.

beginDocumentation()

doc ///
	Key
		WeylGroups
	Headline
		Weyl groups
	Description
		Text
			This package provides functions to compute in Weyl groups of root systems. In particular, it can compute intervals for the Bruhat order.
		Text	
			Here is a quick @HREF(currentLayout#"packages" | "WeylGroups/tutorial.html","tutorial")@ on how to use it.
///

doc ///
	Key
		RootSystem
	Headline
		the class of all root systems
///

doc ///
	Key
		cartanMatrix
	Headline
		the Cartan matrix of a root system
///

doc ///
	Key
		(cartanMatrix, RootSystem)
	Headline
		the Cartan matrix of a root system
	Usage
		cartanMatrix(R)
	Inputs
		R: RootSystem
	Outputs
		: Matrix
			the Cartan matrix of {\tt R}
	Description
		Example
			R=rootSystemA(3)
			cartanMatrix R
///

TEST ///
	R=rootSystemA(3);
	assert(cartanMatrix(R)==matrix{{2,-1,0},{-1,2,-1},{0,-1,2}})
///

doc ///
	Key
		(rank, RootSystem)
	Headline
		the rank of a root system
	Usage
		r=rank(R)
	Inputs
		R: RootSystem
	Outputs
		: ZZ
			the rank of {\tt R}
	Description
		Example
			R = rootSystemA(5)
			rank R
///
TEST ///
	R=rootSystemA(3);
	assert(rank(R)==3)
///


doc ///
	Key
		(symbol ++,RootSystem,RootSystem)
	Headline
		the direct sum of root systems
	Usage
		R1 ++ R2
	Inputs
		R1: RootSystem
		R2: RootSystem
	Outputs
		: RootSystem
			the direct sum of {\tt R1} and {\tt R2}
	Description
		Example
			R1=rootSystemA(2)
			R2=rootSystemA(3)
			R1++R2
///

doc ///
	Key
		(symbol ==,RootSystem,RootSystem)
	Headline
		equality of root systems
	Usage
		R1 == R2
	Inputs
		R1: RootSystem
		R2: RootSystem
	Outputs
		: Boolean
			{\tt true} if the Cartan matrices of {\tt R1} and {\tt R2} coincide and {\tt false} otherwise
	Description
		Example
			R1=rootSystemA(2)
			R2=rootSystemA(2)
			R1==R2
///


doc ///
	Key
		rootSystemA
	Headline
		a root system of type A
	Usage
		rootSystemA(n)
	Inputs
		n: ZZ
	Outputs
		: RootSystem
			a root system of type A and of rank {\tt n}
	Description
		Example
			rootSystemA(5)
///

doc ///
	Key
		rootSystemB
	Headline
		a root system of type B
	Usage
		rootSystemB(n)
	Inputs
		n: ZZ
	Outputs
		: RootSystem
			a root system of type B and of rank {\tt n}
	Description
		Example
			rootSystemB(5)
///

doc ///
	Key
		rootSystemC
	Headline
		a root system of type C
	Usage
		rootSystemC(n)
	Inputs
		n: ZZ
	Outputs
		: RootSystem
			a root system of type C and of rank {\tt n}
	Description
		Example
			rootSystemC(5)
///

doc ///
	Key
		rootSystemD
	Headline
		a root system of type D
	Usage
		rootSystemD(n)
	Inputs
		n: ZZ
	Outputs
		: RootSystem
			a root system of type D and of rank {\tt n}
	Description
		Example
			rootSystemD(5)
///

doc ///
	Key
		rootSystemE
	Headline
		a root system of type E
	Usage
		rootSystemE(n)
	Inputs
		n: ZZ
			from 6 to 8
	Outputs
		: RootSystem
			a root system of type E and of rank {\tt n}
	Description
		Example
			rootSystemE(6)
///

doc ///
	Key
		"rootSystemF4"
	Headline
		the root system of type F4
///

doc ///
	Key
		"rootSystemG2"
	Headline
		the root system of type G2
///

doc ///
	Key
		rootSystem	
	Headline
		obtain a root system
///

doc ///
	Key
		(rootSystem,RootSystem,Parabolic)
	Headline
		the root system of the Levy subgroup of a parabolic 
	Usage
		rootSystem(R,P)
	Inputs
		R: RootSystem 
		P: Parabolic
	Outputs
		: RootSystem 
			the root system of the Levy subgroup of {\tt P} 
	Description
		Example
			R1=rootSystemD(4)
			P=parabolic(R1,set{1,3,4})
			R2=rootSystem(R1,P)
			dynkinType(R2)
///

TEST ///
	R1=rootSystemD(4);
	P=parabolic(R1,set{1,3,4});
	R2=rootSystem(R1,P);
	assert(dynkinType(R2)==dynkinType({{"A",1},{"A",1},{"A",1}}))
///

doc ///
	Key
		(rootSystem,DynkinType)
	Headline
		the root system of a given type
	Usage
		rootSystem(T)
	Inputs
		T: DynkinType
	Outputs
		: RootSystem 
			the root system of the corresponding type
	Description
		Example
			rootSystem(dynkinType({{"A",2},{"D",4}}))
///

TEST ///
	assert(rootSystem(dynkinType({{"A",2},{"D",4}}))==rootSystemA(2)++rootSystemD(4))
///

doc ///
	Key
		(rootSystem,DynkinDiagram)
	Headline
		the root system corresponding to a Dynkin diagram 
	Usage
		rootSystem(D)
	Inputs
		D: DynkinDiagram
	Outputs
		: RootSystem 
			the root system corresponding to the Dynkin diagram 
	Description
		Example
			R1=rootSystemB(3)++rootSystemD(4)
			D=dynkinDiagram(R1)
			R2=rootSystem(D)
			R1==R2	
///

TEST ///
		R1=rootSystemB(3)++rootSystemD(4)
		D=dynkinDiagram(R1)
		R2=rootSystem(D)
		assert(R1==R2)
///

doc ///
	Key
		Weight
	Headline
		the class of weights
	Description
		Text
			a weight is represented by an element of ZZ^n, the basis consisting in the fundamental weights
///

doc ///
	Key
		(symbol +,Weight,Weight)
	Headline
		the sum of two weights
	Usage
		p1 + p2
	Inputs
		p1: Weight
		p2: Weight
	Outputs
		: Weight
			the sum of {\tt p1} and {\tt p2}
	Description
		Example
			R=rootSystemA(4)
			M=cartanMatrix R
			p1=weight(R,M_0)
			p2=weight(R,M_1)
			p1+p2
///

TEST ///
	R=rootSystemA(4);
	p1=weight(R,{1,1,2,3});
	p2=weight(R,{1,1,-2,3});
	assert(p1+p2==weight(R,{2,2,0,6}))
///

doc ///
	Key
		(symbol -,Weight,Weight)
	Headline
		the difference of two weights
	Usage
		p1 - p2
	Inputs
		p1: Weight
		p2: Weight
	Outputs
		: Weight
			the difference of {\tt p1} and {\tt p2}
	Description
		Example
			R=rootSystemA(4)
			M=cartanMatrix R
			p1=weight(R,M_0)
			p2=weight(R,M_1)
			p1-p2
///

TEST ///
	R=rootSystemA(4);
	p1=weight(R,{1,1,2,3});
	p2=weight(R,{1,1,-2,3});
	assert(p1-p2==weight(R,{0,0,4,0}))
///

doc ///
	Key
		(symbol -,Weight)
	Headline
		the negative of a weight
	Usage
		- p
	Inputs
		p: Weight
	Outputs
		: Weight
			the negative of {\tt p}
	Description
		Example
			R=rootSystemA(4)
			M=cartanMatrix R
			p=weight(R,M_2)
			-p
///

TEST ///
	R=rootSystemA(3);
	p=weight(R,{1,2,1});
	assert(-p==weight(R,{-1,-2,-1}))
///

doc ///
	Key
		weight
	Headline
		construct a weight in the weight lattice of a root system 
///

doc ///
	Key
		(weight,RootSystem,BasicList)
	Headline
		construct a weight from a list 
	Usage
		weight(R,L)
	Inputs
		R: RootSystem
		L: BasicList 
			the coordinates of the weight	
	Outputs
		: Weight 
			a weight vector in a the weight lattice of {\tt R}
	Description
		Example
			R=rootSystemB(3)
			weight(R,{1,2,1})
///

TEST ///
	R=rootSystemA(4);
	assert(weight(R,{1,1,2,3})==new Weight from vector{1,1,2,3})
///

doc ///
	Key
		(weight,RootSystem,Vector)
	Headline
		construct a weight from a vector 
	Usage
		weight(R,v)
	Inputs
		R: RootSystem
		v: Vector 
			the coordinates of the weight	
	Outputs
		: Weight 
			a weight vector in a the weight lattice of {\tt R}
	Description
		Example
			R=rootSystemB(3)
			weight(R,vector {1,2,1})
///

TEST ///
	R=rootSystemA(4);
	assert(weight(R,vector{1,1,2,3})==new Weight from vector{1,1,2,3})
///

doc ///
	Key
		Root
	Headline
		the class of roots (or, more generally, elements of the root lattice)
	Description
		Text
			a root is represented by the respective weight
///

doc ///
	Key
		addRoots
	Headline
		adding roots
///

doc ///
	Key
		(addRoots, RootSystem, Root, Root)
	Headline
		the sum of two roots 
	Usage
		r1 + r2
	Inputs
		R: RootSystem
		r1: Root 
		r2: Root 
	Outputs
		: Weight
			the sum of {\tt r1} and {\tt r2}. Actually, it is returned as an object of class @TO Root@ if the resulting weight is a root.
	Description
		Example
			R=rootSystemA(2)
			r1=simpleRoot(R,1)
			r2=simpleRoot(R,2)
			r=addRoots(R,r1,r2)
			addRoots(R,r1,r)
///

TEST ///
	R=rootSystemA(2);
        r1=simpleRoot(R,1);
        r2=simpleRoot(R,2);
	assert(addRoots(R,r1,r1)==weight(R,{4,-2}));
	assert(isRoot(R,addRoots(R,r1,r2))==true)
///

doc ///
	Key
		(symbol *, ZZ, Root)
	Headline
		multiplication of a root by an integer
	Usage
		n*r
	Inputs
		n: ZZ 
		r: Root 
	Outputs
		: Weight
	Description
		Example
			R=rootSystemA(3)
			r=simpleRoot(R,1)
			2*r
		Text
			Note that if the integer is +1 or -1, we obtain a weight that is a root.
		Example
			-1*r
///

TEST ///
	R=rootSystemA(2);
        r=simpleRoot(R,1);
	assert(-2*r==weight(R,{-4,2}));
///

doc ///
	Key
		halfSumOfRoots
	Headline
		the half-sum of positive roots
///

doc ///
	Key
		(halfSumOfRoots, RootSystem)
	Headline
		the half-sum of positive roots
	Usage
		halfSumOfRoots(R)
	Inputs
		R: RootSystem
	Outputs
		: Weight
			the half-sum of positive roots in {\tt R}
	Description
		Example
			halfSumOfRoots(rootSystemE(6))
///

TEST ///
	R=rootSystemE(6);
	assert(halfSumOfRoots(R)==weight(R,{1,1,1,1,1,1}))	
///

doc ///
	Key
		isPositiveRoot
	Headline
		check whether a weight is a positive root
///

doc ///
	Key
		(isPositiveRoot, RootSystem, Weight)
	Headline
		check whether a weight is a positive root
	Usage
		isPositiveRoot(R,p)
	Inputs
		R: RootSystem
		p: Weight
	Outputs
		: Boolean
			{\tt true} if {\tt p} is a positive root in {\tt R} and {\tt false} otherwise
	Description
		Example
			R=rootSystemE(6)
			p=halfSumOfRoots(R)
			isPositiveRoot(R,p)
///

TEST ///
	R=rootSystemA(3);
	w1=weight(R,{1,2,1});
	w2=weight(R,{2,-1,0});
	assert(isPositiveRoot(R,w1)==false);
	assert(isPositiveRoot(R,w2)==true)
///

doc ///
	Key
		isRoot
	Headline
		check whether a weight is a root or whether a root is in a parabolic sub root system
///

doc ///
	Key
		(isRoot, RootSystem, Weight)
	Headline
		check whether a weight is a positive root
	Usage
		isRoot(R,p)
	Inputs
		R: RootSystem
		p: Weight
	Outputs
		: Boolean
			{\tt true} if {\tt p} is a root in {\tt R} and {\tt false} otherwise
	Description
		Example
			R=rootSystemE(6)
			p=simpleRoot(R,1)
			isPositiveRoot(R,-p)
			isRoot(R,-p)
///

TEST ///
	R=rootSystemE(6);
	p=simpleRoot(R,1);
	isPositiveRoot(R,-p);
	assert(isRoot(R,-p)==true)
///

doc ///
	Key
		(isRoot, RootSystem, Parabolic, Root)
	Headline
		check whether a root is in the sub root system of the parabolic 
	Usage
		isRoot(R,P,r)
	Inputs
		R: RootSystem
		P: Parabolic
		r: Root 
	Outputs
		: Boolean
			{\tt true} if {\tt p} is a root in the sub root system of {\tt R} defined by {\tt P} and {\tt false} otherwise
	Description
		Example
			R=rootSystemE(6)
			P=parabolic(R,set{1,3})
			r1=simpleRoot(R,1)
			r2=simpleRoot(R,2)
			isRoot(R,P,r1)
			isRoot(R,P,r2)
///

TEST ///
	R=rootSystemE(6);
	P=parabolic(R,set{1,3});
	r1=simpleRoot(R,1);
	r2=simpleRoot(R,2);
	assert(isRoot(R,P,r1)==true);
	assert(isRoot(R,P,r2)==false)
///

doc ///
	Key
		simpleRoot
	Headline
		a simple root
///

doc ///
	Key
		(simpleRoot, RootSystem, ZZ)
	Headline
		the n-th simple root
	Usage
		simpleRoot(R,n)
	Inputs
		R: RootSystem
		n: ZZ
	Outputs
		: Root
			the {\tt n}-th simple root of {\tt R}
	Description
		Example
			simpleRoot(rootSystemE(6),2)
///

doc ///
	Key
		(norm, RootSystem, Root)
	Headline
		the squared norm of a root
	Usage
		norm(R,r)
	Inputs
		R: RootSystem
		r: Root
	Outputs
		: ZZ
			the squared norm of {\tt r}, short roots being of norm 1
	Description
		Example
			norm(rootSystemF4,simpleRoot(rootSystemF4,2))
///

TEST ///
	assert(norm(rootSystemF4,simpleRoot(rootSystemF4,2))==2)
///

doc ///
	Key
		scalarProduct
	Headline
		compute a scalar product (invariant by the Weyl group)
///

doc ///
	Key
		(scalarProduct, RootSystem, ZZ, ZZ)
	Headline
		the scalar product of two fundamental weights
	Usage
		scalarProduct(R,i,j)
	Inputs
		R: RootSystem
		i: ZZ 
			the number of the first fundamental weight
		j: ZZ 
			the number of the second fundamental weight
	Outputs
		: QQ
			the scalar product of the {\tt i}-th fundamental weight with the {\tt j}-th
	Description
		Example
			R=rootSystemA(2)
			scalarProduct(R,1,1)
			scalarProduct(R,1,2)
			scalarProduct(R,2,2)
///

TEST ///
	assert(scalarProduct(rootSystemA(2),1,2)==1/6)
///

doc ///
	Key
		(scalarProduct, RootSystem, ZZ, Weight)
	Headline
		the scalar product of a fundamental weight and a weight
	Usage
		scalarProduct(R,i,v)
	Inputs
		R: RootSystem
		i: ZZ 
			the number of the fundamental weight
		v: Weight 
			the other weight
	Outputs
		: QQ
			the scalar product of the {\tt i}-th fundamental weight with the weight {\tt v} 
	Description
		Example
			R=rootSystemA(2)
			v=weight(R,{1,2})
			scalarProduct(R,1,v)
///

TEST ///
	R=rootSystemA(2);
	v=weight(R,{1,2});
	assert(scalarProduct(R,1,v)==2/3)
///

doc ///
	Key
		(scalarProduct, RootSystem, Weight, Weight)
	Headline
		the scalar product of two weights
	Usage
		scalarProduct(R,u,v)
	Inputs
		R: RootSystem
		u: Weight 
			the first weight
		v: Weight 
			the second weight
	Outputs
		: QQ
			the scalar product of {\tt u} and {\tt v} 
	Description
		Example
			R=rootSystemA(2)
			u=weight(R,{1,2})
			v=weight(R,{0,3})
			scalarProduct(R,u,v)
///

TEST ///
	R=rootSystemA(2)
	u=weight(R,{1,2})
	v=weight(R,{0,3})
	assert(scalarProduct(R,u,v)==5/2)
///

doc ///
	Key
		eval
	Headline
		evaluate the dual of a root at something
///

doc ///
	Key
		(eval, RootSystem, ZZ, ZZ)
	Headline
		evaluate the dual of a simple root at another one
	Usage
		eval(R,i,j)
	Inputs
		R: RootSystem
		i: ZZ 
			the number of the first simple root 
		j: ZZ 
			the number of the second simple root 
	Outputs
		: ZZ 
			the dual of the {\tt j}-th simple root evaluated at the {\tt i}-th simple root 
	Description
		Example
			R=rootSystemA(2)
			eval(R,1,1)
			eval(R,1,2)
///

TEST ///
			R=rootSystemA(2);
			assert(eval(R,1,1)==2);
			assert(eval(R,1,2)==-1);
///

doc ///
	Key
		(eval, RootSystem, Weight, ZZ)
	Headline
		evaluate the dual of a simple root at a Weight
	Usage
		eval(R,v,i)
	Inputs
		R: RootSystem
		v: Weight 
		i: ZZ 
			the number of the simple root 
	Outputs
		: ZZ 
			the dual of the {\tt i}-th simple root evaluated at the weight {\tt v} 
	Description
		Example
			R=rootSystemA(2)
			v=weight(R,{1,2})
			eval(R,v,1)
///

TEST ///
	R=rootSystemA(2);
	v=weight(R,{1,2});
	assert(eval(R,v,1)==1)
///

doc ///
	Key
		(eval, RootSystem, Weight, Root)
	Headline
		evaluate the dual of a root at a Weight
	Usage
		eval(R,v,r)
	Inputs
		R: RootSystem
		v: Weight 
		r: Root 
	Outputs
		: ZZ 
			the dual of the root {\tt r} evaluated at the weight {\tt v} 
	Description
		Example
			R=rootSystemA(2)
			L=toList(positiveRoots(R))
			v=weight(R,{1,2})
			eval(R,v,L#0)
			eval(R,v,L#1)
			eval(R,v,L#2)
///

doc ///
	Key
		(eval, RootSystem, ZZ, Root)
	Headline
		evaluate the dual of a root at a fundamental weight
	Usage
		eval(R,i,r)
	Inputs
		R: RootSystem
		i: ZZ
			the number of a weight
		r: Root 
	Outputs
		: ZZ 
			the dual of the root {\tt r} evaluated at the {\tt i}-th fundamental weight
	Description
		Example
			R=rootSystemA(2)
			L=toList(positiveRoots(R))
			eval(R,1,L#0)
			eval(R,1,L#1)
			eval(R,1,L#2)
///

TEST ///
	R=rootSystemA(2);
	L=toList(positiveRoots(R));
	v=weight(R,{1,2});
    assert(L/(ell -> (eval(R,v,ell), eval(R,1,ell)))//sort == sort {(2,0),(3,1),(1,1)})
	--assert(eval(R,v,L#0)==2);
	--assert(eval(R,v,L#1)==3);
	--assert(eval(R,v,L#2)==1);
	--assert(eval(R,1,L#0)==0);
	--assert(eval(R,1,L#1)==1);
	--assert(eval(R,1,L#2)==1)
///

doc ///
	Key
		rootCoefficients
	Headline
		coefficients at the simple roots
///

doc ///
	Key
		(rootCoefficients,RootSystem,Root)
	Headline
		the coefficients at the simple roots
	Usage
		rootCoefficients(R,r)
	Inputs
		R: RootSystem
		r: Root
	Outputs
		: Vector
			whose components are the coefficients of {\tt r} at the simple roots of {\tt R}
	Description
		Example
			R=rootSystemE(8)
			r1=simpleRoot(R,1)
			r2=simpleRoot(R,2)
			rootCoefficients(R,r1+r2)
///

doc ///
	Key
		(rootCoefficients,RootSystem,Weight)
	Headline
		the coefficients at the simple roots
	Usage
		rootCoefficients(R,p)
	Inputs
		R: RootSystem
		p: Weight
	Outputs
		: Vector
			whose components are the coefficients of {\tt p} at the simple roots of {\tt R}
	Description
		Example
			R=rootSystemE(6)
			rootCoefficients(R,halfSumOfRoots(R))
///

TEST ///
	R=rootSystemE(6);
	r1=simpleRoot(R,1);
	r2=simpleRoot(R,2);
	assert(rootCoefficients(R,halfSumOfRoots(R))==vector{8_QQ,11,15,21,15,8});
	assert(rootCoefficients(R,r1+r2)==vector{1_QQ,1,0,0,0,0})
///

doc ///
	Key
		positiveRoots
	Headline
		the set of all positive roots
///

doc ///
	Key
		(positiveRoots,RootSystem)
	Headline
		the set of all positive roots
	Usage
		positiveRoots(R)
	Inputs
		R: RootSystem
	Outputs
		: Set
			of all positive roots of {\tt R}
	Description
		Example
			positiveRoots(rootSystemA(3))
///

doc ///
	Key
		(positiveRoots,RootSystem, Parabolic)
	Headline
		the set of all positive roots in a parabolic sugroups
	Usage
		positiveRoots(R,P)
	Inputs
		R: RootSystem
		P: Parabolic
	Outputs
		: Set
			of all positive roots of {\tt R} that are written as sums of roots corresponding to reflexions in {\tt P}
	Description
		Example
			R=rootSystemA(4)
			P=parabolic(R,set{1,2})
			positiveRoots(R,P)
///

doc ///
	Key
		numberOfPositiveRoots
	Headline
		the number of positive roots
///

doc ///
	Key
		(numberOfPositiveRoots,RootSystem)
	Headline
		the number of positive roots
	Usage
		numberOfPositiveRoots(R)
	Inputs
		R: RootSystem
	Outputs
		: ZZ
			the number of positive roots of {\tt R}
	Description
		Example
			numberOfPositiveRoots(rootSystemD(4))
///

doc ///
	Key
		(numberOfPositiveRoots,DynkinType)
	Headline
		the number of positive roots
	Usage
		numberOfPositiveRoots(D)
	Inputs
		D: DynkinType
	Outputs
		: ZZ
			the number of positive roots of {\tt D}
	Description
		Example
			D=dynkinType{{"A",5},{"E",6}}
			numberOfPositiveRoots(D)
///

TEST ///
	R=rootSystemA(4);
	P=parabolic(R,set{1,2});
	assert(#positiveRoots(R,P)==numberOfPositiveRoots(rootSystemA(2)));
	assert(numberOfPositiveRoots(dynkinType{{"A",2},{"E",6}})==39)
///

doc ///
	Key
		listWeylGroupElements
	Headline
		list all elements of a given length in a Weyl group
///

doc ///
	Key
		(listWeylGroupElements,RootSystem,ZZ)
	Headline
		list all elements of a given length in a Weyl group		
	Usage
		listWeylGroupElements(R,k)
	Inputs
		R: RootSystem
		k: ZZ
	Outputs
		: List
			of all elements of length {\tt k} in the Weyl group of {\tt R}
	Description
		Example
			listWeylGroupElements(rootSystemG2,4)
///

TEST ///
	assert(set apply(listWeylGroupElements(rootSystemG2,4),reducedDecomposition)===set {{1,2,1,2},{2,1,2,1}})
///

doc ///
	Key
		reflect
	Headline
		apply the reflection with respect to a root
///

doc ///
	Key
		(reflect,RootSystem,ZZ,Weight)
	Headline
		apply to a weight the reflection with respect to a root
	Usage
		reflect(R,i,p)
	Inputs
		R: RootSystem
		i: ZZ
		p: Weight
	Outputs
		: Weight
			s_i(p)
	Description
		Example
			R = rootSystemB(4)
			p = halfSumOfRoots(R)
			reflect(R,4,p)
///

doc ///
	Key
		(reflect,RootSystem,BasicList,Weight)
	Headline
		apply to a weight several reflections with respect to roots
	Usage
		reflect(R,L,p)
	Inputs
		R: RootSystem
		L: BasicList
		p: Weight
	Outputs
		: Weight
			s_{L_0}...s_{L_{\#L-1}}(p)
	Description
		Example
			R = rootSystemB(4)
			p = halfSumOfRoots(R)
			reflect(R,{1,2,3,4},p)
///

doc ///
	Key
		(reflect,RootSystem,ZZ,Root)
	Headline
		apply to a root the reflection with respect to a simple root
	Usage
		reflect(R,i,r)
	Inputs
		R: RootSystem
		i: ZZ
		r: Root
	Outputs
		: Weight
			s_i(r)
	Description
		Example
			R = rootSystemB(4)
			r = simpleRoot(R,3)
			reflect(R,4,r)
///

doc ///
	Key
		(reflect,RootSystem,BasicList,Root)
	Headline
		apply to a root several reflections with respect to simple roots 
	Usage
		reflect(R,L,r)
	Inputs
		R: RootSystem
		L: BasicList
		r: Root
	Outputs
		: Weight
			s_{L_0}...s_{L_{\#L-1}}(r)
	Description
		Example
			R = rootSystemB(4)
			r = simpleRoot(R,3)
			reflect(R,{1,2,3,4},r)
///

TEST ///
	R=rootSystemB(4);
	assert(reflect(R,{1,2,3,4},halfSumOfRoots(R))==weight(R,{-4,1,1,3}))
///

doc ///
	Key
		WeylGroupElement
	Headline
		the class of elements of Weyl groups
	Description
		Text
			{\tt w} is represented by the list consisting of a root system and {\tt w} applied to the half-sum of positive roots
///

doc ///
	Key
		(symbol ==,WeylGroupElement,WeylGroupElement)
	Headline
		equality of elements of Weyl groups
	Usage
		w1 == w2
	Inputs
		w1: WeylGroupElement
		w2: WeylGroupElement
	Outputs
		: Boolean
			{\tt true} if {\tt w1} and {\tt w2} belong to the same Weyl group and coincide, and {\tt false} otherwise
	Description
		Example
			w1=reduce(rootSystemD(5),{3})
			w1==inverse(w1)
///

TEST ///
	w1=reduce(rootSystemD(5),{3});
	assert(w1==inverse(w1))
///

doc ///
	Key
		(symbol *,WeylGroupElement,WeylGroupElement)
	Headline
		the product of two elements of a Weyl group
	Usage
		w1 * w2
	Inputs
		w1: WeylGroupElement
		w2: WeylGroupElement
	Outputs
		: WeylGroupElement
			the product of {\tt w1} and {\tt w2}
	Description
		Example
			R=rootSystemC(4)
			w1=reduce(R,{1})
			w2=reduce(R,{2})
			w1*w2
///

TEST ///
	R=rootSystemC(4);
	w1=reduce(R,{1});
	w2=reduce(R,{2});
	assert(reducedDecomposition(w1*w2*w1)=={1,2,1})
///

doc ///
	Key
		(inverse,WeylGroupElement)
	Headline
		the inverse to an element of a Weyl group
	Usage
		inverse(w)
	Inputs
		w: WeylGroupElement
	Outputs
		: WeylGroupElement
			the inverse to {\tt w}
	Description
		Example
			w=reduce(rootSystemE(6),{1,2,3})
			inverse w
///

TEST ///
	assert(reducedDecomposition(inverse(reduce(rootSystemE(6),{1,2,3})))=={2,3,1})
///

doc ///
	Key
		(symbol ^,WeylGroupElement,ZZ)
	Headline
		the power of an element of a Weyl group
	Usage
		w^n
	Inputs
		w: WeylGroupElement
		n: ZZ
	Outputs
		: WeylGroupElement
			the {\tt n}-th power of {\tt w}
	Description
		Example
			w=reduce(rootSystemE(6),{1,2,3})
			w^10
///

TEST ///
	R=rootSystemE(6);
	assert(reducedDecomposition((reduce(R,{1,2,3}))^3)=={2})
///

doc ///
	Key
		(symbol *,WeylGroupElement,Weight)
	Headline
		apply an element of a Weyl group to a weight
	Usage
		w * p
	Inputs
		w: WeylGroupElement
		p: Weight
	Outputs
		: Weight
			{\tt w} applied to {\tt p}
	Description
		Example
			R=rootSystemC(4)
			w=reduce(R,{1})
			p=halfSumOfRoots R
			w*p
///

doc ///
	Key
		(symbol *,WeylGroupElement,Root)
	Headline
		apply an element of a Weyl group to a root
	Usage
		w * r
	Inputs
		w: WeylGroupElement
		r: Root
	Outputs
		: Root
			{\tt w} applied to {\tt r}
	Description
		Example
			R=rootSystemC(4)
			w=reduce(R,{1})
			r=simpleRoot(R,2)
			w*r
///

TEST ///
	R=rootSystemC(4);
	w=reduce(R,{1});
	p=halfSumOfRoots R
	assert(w*p==weight(R,{-1,2,1,1}))
///

doc ///
	Key
		longestWeylGroupElement
	Headline
		the longest element of a Weyl group
///

doc ///
	Key
		(longestWeylGroupElement, RootSystem)
	Headline
		the longest element of the Weyl group of a root system
	Usage
		longestWeylGroupElement(R)
	Inputs
		R: RootSystem
	Outputs
		: WeylGroupElement
			the longest element of the Weyl group of {\tt R}
	Description
		Example
			longestWeylGroupElement rootSystemF4
///

TEST ///
	assert(reducedDecomposition(longestWeylGroupElement rootSystemG2)=={1,2,1,2,1,2})
///

doc ///
	Key
		(longestWeylGroupElement, RootSystem, Parabolic)
	Headline
		the longest element of a parabolic subgroup of the Weyl group of a root system
	Usage
		longestWeylGroupElement(R,P)
	Inputs
		R: RootSystem
		P: Parabolic
	Outputs
		: WeylGroupElement
			the longest element of the parabolic sugroup of the Weyl group of {\tt R} generated by the reflections in P
	Description
		Example
			R=rootSystemF4;
			longestWeylGroupElement(R,parabolic(R,set{1,2}))
///

TEST ///
	R=rootSystemF4;
	assert(reducedDecomposition(longestWeylGroupElement(R,parabolic(R,set{3,4})))=={3,4,3})
///

doc ///
	Key
		neutralWeylGroupElement
	Headline
		the neutral element of a Weyl group
///

doc ///
	Key
		(neutralWeylGroupElement, RootSystem)
	Headline
		the neutral element of the Weyl group of a root system
	Usage
		neutralWeylGroupElement(R)
	Inputs
		R: RootSystem
	Outputs
		: WeylGroupElement
			the neutral element of the Weyl group of {\tt R}
	Description
		Example
			neutralWeylGroupElement rootSystemF4
///

TEST ///
	assert(reducedDecomposition(neutralWeylGroupElement(rootSystemG2))=={})
///

doc ///
	Key
		reduce
	Headline
		the product of several reflections
///

doc ///
	Key
		(reduce,RootSystem,BasicList)
	Headline
		the product of several reflections with respect to simple roots
	Usage
		reduce(R,L)
	Inputs
		R: RootSystem
		L: BasicList
	Outputs
		: WeylGroupElement
			s_{L_0}...s_{L_{\#L-1}}
	Description
		Example
			reduce(rootSystemF4,{1,2,3,4})
///

TEST ///
	R=rootSystemA(3);
	assert(reduce(R,{1,3})==reduce(R,{3,1}))
///

doc ///
	Key
		reducedDecomposition
	Headline
		the reduced decomposition of an element of a Weyl group
///

doc ///
	Key
		(reducedDecomposition, WeylGroupElement)
	Headline
		the reduced decomposition of an element of a Weyl group
	Usage
		reducedDecomposition(w)
	Inputs
		w: WeylGroupElement
	Outputs
		: List
			the lexicographically least reduced decomposition of {\tt w}
	Description
		Example
			reducedDecomposition longestWeylGroupElement rootSystemF4
///

TEST ///
	assert(reducedDecomposition(longestWeylGroupElement(rootSystemB(2)))=={1,2,1,2})
///

doc ///
	Key
		isReduced
	Headline
		check whether a decomposition is of minimal length 
///

doc ///
	Key
		(isReduced, RootSystem, BasicList)
	Headline
		whether a decomposition in simple reflections is reduced
	Usage
		isReduced(R,L)
	Inputs
		R: RootSystem 
		L: BasicList
			of indices of simple reflections
	Outputs
		: Boolean 
			true if the length of the product of these simple reflections is indeed their number, and false if not
	Description
		Example
			R=rootSystemF4
			L=reducedDecomposition longestWeylGroupElement R
			isReduced(R,L)
			L={1,1} 
			isReduced(R,L)
///

TEST ///
	R=rootSystemF4;
	L=reducedDecomposition longestWeylGroupElement R;
	assert(isReduced(R,L)==true);
	L={1,1}; 
	assert(isReduced(R,L)==false)
///

doc ///
	Key
		(isReduced, BasicList, WeylGroupElement)
	Headline
		whether an Weyl group element can be multiplied by some simple reflections with length increasing at each step 
	Usage
		isReduced(L,w)
	Inputs
		L: BasicList
			of indices of simple reflections
		w: WeylGroupElement
	Outputs
		: Boolean 
			true if the length of the product (on the left) of these simple reflections by w is the length of w plus their number, and false if not
	Description
		Example
			R=rootSystemA(3)
			w=reduce(R,{1,2,1})
			isReduced({2,3},w)
			isReduced({1},w)
///

TEST ///
	R=rootSystemA(3);
	w=reduce(R,{1,2,1});
	assert(isReduced({2,3},w));
	assert(isReduced({1},w)==false)
///

doc ///
	Key
		coxeterLength
	Headline
		the length of a reduced decomposition of an element of a Weyl group
///

doc ///
	Key
		(coxeterLength, WeylGroupElement)
	Headline
		the length of a reduced decomposition of an element of a Weyl group
	Usage
		coxeterLength(w)
	Inputs
		w: WeylGroupElement
	Outputs
		: ZZ 
			the number of simple reflections in a reduced decomposition of {\tt w}
	Description
		Example
			coxeterLength longestWeylGroupElement rootSystemF4
///

TEST ///
	assert(coxeterLength longestWeylGroupElement rootSystemF4==24)
///

doc ///
	Key
		reflection
	Headline
		the reflection with respect to a root
///

doc ///
	Key
		(reflection, RootSystem, Root)
	Headline
		the reflection with respect to a root
	Usage
		reflection(R,r)
	Inputs
		R: RootSystem
		r: Root
	Outputs
		: WeylGroupElement
			the reflection with respect to {\tt r}
	Description
		Example
			R=rootSystemE(8)
			r=addRoots(R,simpleRoot(R,1),simpleRoot(R,3))
			reflection(R,r)
	
///

TEST ///
	R=rootSystemE(8);
	r=addRoots(R,simpleRoot(R,1),simpleRoot(R,3));
	assert(reducedDecomposition(reflection(R,r))=={1,3,1})
///

doc ///
	Key
		isReflection
	Headline
		checks whether an element of a Weyl group is a reflection
///

doc ///
	Key
		(isReflection, WeylGroupElement)
	Headline
		checks whether an element of a Weyl group is a reflection
	Usage
		isReflection(w)
	Inputs
		w: WeylGroupElement
	Outputs
		: Boolean
			{\tt true} if {\tt w} is a reflection and {\tt false} otherwise
	Description
		Example
			R=rootSystemD(5)
			w=reduce(R,{3})
			isReflection(w)
///

TEST ///
	assert(isReflection(reduce(rootSystemD(3),{3})))
///

doc ///
	Key
		whoseReflection
	Headline
		the positive root whose reflection is a given element of a Weyl group
///

doc ///
	Key
		(whoseReflection, WeylGroupElement)
	Headline
		the positive root whose reflection is a given element of a Weyl group
	Usage
		whoseReflection(w)
	Inputs
		w: WeylGroupElement
	Outputs
		: Root
			the root whose corresponding reflection is {\tt w}
	Description
		Example
			R=rootSystemD(5)
			w=reduce(R,{3})
			if isReflection(w) then whoseReflection(w)
	Caveat
		If {\tt w} is not a reflection, an error occurs.
///

TEST ///
	R=rootSystemD(5);
	w=reduce(R,{3});
	assert(whoseReflection(w)==simpleRoot(R,3))
///

doc ///
	Key
		Parabolic
	Headline
		the class of parabolic subgroups of Weyl groups
	Description
		Text
			A parabolic subgroup is represented by the set of indices of generating reflections.
///

doc ///
	Key
		parabolic
	Headline
		construct a parabolic
///

doc ///
	Key
		(symbol ==,Parabolic,Parabolic)
	Headline
		equality of parabolics 
	Usage
		P1 == P2
	Inputs
		P1: Parabolic 
		P2: Parabolic 
	Outputs
		: Boolean
			{\tt true} if the underlying sets are equal 
	Description
		Example
			R=rootSystemA(3)
			P1=parabolic(R,set{1,2})
			P2=parabolic(R,set{1,2})
			P1==P2
///

doc ///
	Key
		(parabolic, RootSystem,Set)
	Headline
		construct a parabolic from a set of simple roots
	Usage
		parabolic(R,S)
	Inputs
		R: RootSystem
		S: Set
			the subsets of simple roots defining the standard parabolic
	Outputs
		: Parabolic
			a standard parabolic subgroup represented by a set of simple roots
	Description
		Example
			parabolic(rootSystemA(5),set{1,2})
///

TEST ///
	assert(parabolic(rootSystemA(5),set{1,2})==new Parabolic from set{1,2})
///

doc ///
	Key
		(parabolic, WeylGroupDoubleCoset)
	Headline
		the parabolic associated to a double coset 
	Usage
		parabolic(d)
	Inputs
		d: WeylGroupDoubleCoset 
	Outputs
		: Parabolic
			a standard parabolic subgroup corresponding to the coset {\tt d} as in Carter, {\em Finite Groups of Lie Type}, p. 65, thm 2.7.4.
	Description
		Example 
			R=rootSystemA(4)
			P=parabolic(R,set{1,2,4})
			w=reduce(R,{3,2,4,3})
			d= P % w % P
			parabolic(d)
///

TEST ///
	R=rootSystemA(4);
	P=parabolic(R,set{1,2,4});
	w=reduce(R,{3,2,4,3});
	d= P % w % P;
	assert(parabolic(d)==parabolic(R,set{4,2}))
///

doc ///
	Key
		WeylGroupLeftCoset
	Headline
		the class of left cosets of Weyl groups by parabolic subgroup
	Description
		Text
			a coset {wW_P} is represented by the list consisting of a parabolic subgroup {\tt P} and the representative of minimal length {\tt w}
///

doc ///
	Key
		(symbol ==,WeylGroupLeftCoset,WeylGroupLeftCoset)
	Headline
		equality of left cosets
	Usage
		w1 == w2
	Inputs
		w1: WeylGroupLeftCoset
		w2: WeylGroupLeftCoset
	Outputs
		: Boolean
			{\tt true} if {\tt w1} and {\tt w2} belong to the same quotient and coincide, and {\tt false} otherwise
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			w1=w % P
			w2=(w * reduce(R,{2,3})) % P
			w1==w2
///

TEST ///
	R=rootSystemD(5);
	P=parabolic(R,set{2,3,4,5});
	w=longestWeylGroupElement R;
	w1=w % P;
	w2=(w * reduce(R,{2,3})) % P;
	assert(w1==w2)
///

doc ///
	Key
		WeylGroupRightCoset
	Headline
		the class of right cosets of Weyl groups by parabolic subgroups
	Description
		Text
			a coset {W_Pw} is represented by the list consisting of a parabolic subgroup {\tt P} and the representative of minimal length {\tt w}
///

doc ///
	Key
		(symbol ==,WeylGroupRightCoset,WeylGroupRightCoset)
	Headline
		equality of right cosets
	Usage
		w1 == w2
	Inputs
		w1: WeylGroupRightCoset
		w2: WeylGroupRightCoset
	Outputs
		: Boolean
			{\tt true} if {\tt w1} and {\tt w2} belong to the same quotient and coincide, and {\tt false} otherwise
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			w1=P % w
			w2=P % (reduce(R,{2,3})*w)
			w1==w2
///

TEST ///
	R=rootSystemD(5);
	P=parabolic(R,set{2,3,4,5});
	w=longestWeylGroupElement R;
	w1=P % w;
	w2=P % (reduce(R,{2,3})*w);
	assert(w1==w2)
	
///

doc ///
	Key
		WeylGroupDoubleCoset
	Headline
		the class of double cosets of Weyl groups by pairs of parabolic subgroups
	Description
		Text
			a coset {W_PwW_Q} is represented by the list consisting of parabolic subgroups {\tt P} and {\tt Q}, and the representative of minimal length {\tt w}
///

doc ///
	Key
		(symbol ==,WeylGroupDoubleCoset,WeylGroupDoubleCoset)
	Headline
		equality of double cosets
	Usage
		w1 == w2
	Inputs
		w1: WeylGroupDoubleCoset
		w2: WeylGroupDoubleCoset
	Outputs
		: Boolean
			{\tt true} if {\tt w1} and {\tt w2} belong to the same quotient and coincide, and {\tt false} otherwise
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			w1=P % (w % P)
			w2=P % ((reduce(R,{4}) * w * reduce(R,{2,3})) % P)
			w1==w2
///

TEST ///
	R=rootSystemD(5);
	P=parabolic(R,set{2,3,4,5});
	w=longestWeylGroupElement R;
	w1=P % (w % P);
	w2=P % ((reduce(R,{4}) * w * reduce(R,{2,3})) % P);
	assert(w1==w2)
///

doc ///
	Key
		(symbol %,WeylGroupElement,Parabolic)
	Headline
		the left coset defined by an element of Weyl group
	Usage
		w % P
	Inputs
		w: WeylGroupElement
		P: Parabolic
	Outputs
		: WeylGroupLeftCoset
			wW_P
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			w % P
///

doc ///
	Key
		(symbol %,Parabolic,WeylGroupElement)
	Headline
		the right coset defined by an element of Weyl group
	Usage
		P % w
	Inputs
		P: Parabolic
		w: WeylGroupElement
	Outputs
		: WeylGroupLeftCoset
			W_Pw
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			P % w
///

doc ///
	Key
		(symbol %,Parabolic,WeylGroupLeftCoset)
	Headline
		the double coset defined by a left coset
	Usage
		P % c
	Inputs
		P: Parabolic
		c: WeylGroupLeftCoset
	Outputs
		: WeylGroupDoubleCoset
			W_Pc
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			Q=parabolic(R,set{1,2,3,4})
			w=longestWeylGroupElement R
			c = w % Q
			P % c
///

doc ///
	Key
		(symbol %,WeylGroupRightCoset,Parabolic)
	Headline
		the double coset defined by a right coset
	Usage
		c % Q
	Inputs
		c: WeylGroupRightCoset
		Q: Parabolic
	Outputs
		: WeylGroupDoubleCoset
			cW_Q
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			Q=parabolic(R,set{1,2,3,4})
			w=longestWeylGroupElement R
			c = P % w
			c % Q
///


doc ///
	Key
		(symbol *,WeylGroupElement,WeylGroupLeftCoset)
	Headline
		apply an element of a Weyl group to a left coset
	Usage
		w * c
	Inputs
		w: WeylGroupElement
		c: WeylGroupLeftCoset
	Outputs
		: WeylGroupLeftCoset
			{\tt w} applied to {\tt c}
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			c=w % P
			reduce(R,{1})*c
///

doc ///
	Key
		(symbol *,WeylGroupRightCoset,WeylGroupElement)
	Headline
		apply an element of a Weyl group to a left coset
	Usage
		c * w
	Inputs
		c: WeylGroupRightCoset
		w: WeylGroupElement
	Outputs
		: WeylGroupRightCoset
			{\tt w} applied to {\tt c}
	Description
		Example
			R=rootSystemD(5)
			P=parabolic(R,set{2,3,4,5})
			w=longestWeylGroupElement R
			c=P % w
			c*reduce(R,{1})
///

doc ///
	Key
		minimalRepresentative
	Headline
		the minimal representative of a coset
///

doc ///
	Key
		(minimalRepresentative, WeylGroupLeftCoset)
	Headline
		the minimal representative of a coset
	Usage
		minimalRepresentative(c)
	Inputs
		c: WeylGroupLeftCoset
	Outputs
		: WeylGroupElement
			the representative of minimal length
	Description
		Example
			R=rootSystemF4
			P=parabolic(R,set{1,2,3})
			w=longestWeylGroupElement R
			c=w % P
			minimalRepresentative c			
///

doc ///
	Key
		(minimalRepresentative, WeylGroupRightCoset)
	Headline
		the minimal representative of a coset
	Usage
		minimalRepresentative(c)
	Inputs
		c: WeylGroupRightCoset
	Outputs
		: WeylGroupElement
			the representative of minimal length
	Description
		Example
			R=rootSystemF4
			P=parabolic(R,set{1,2,3})
			w=longestWeylGroupElement R 
			c=P % w
			minimalRepresentative c			
///

doc ///
	Key
		(minimalRepresentative, WeylGroupDoubleCoset)
	Headline
		the minimal representative of a coset
	Usage
		minimalRepresentative(c)
	Inputs
		c: WeylGroupDoubleCoset
	Outputs
		: WeylGroupElement
			the representative of minimal length
	Description
		Example
			R=rootSystemF4
			P=parabolic(R,set{1,2,3})
			w=longestWeylGroupElement R 
			c=P % (w % P)
			minimalRepresentative c			
///

doc ///
	Key
		isMinimalRepresentative
	Headline
		check whether an element of a Weyl group is the minimal representative of a coset
///

doc ///
	Key
		(isMinimalRepresentative, WeylGroupElement, Parabolic)
	Headline
		check whether an element of a Weyl group is the minimal representative of a left coset
	Usage
		minimalRepresentative(w,P)
	Inputs
		w: WeylGroupElement
		P: Parabolic
	Outputs
		: Boolean
			{\tt true} if {\tt w} is the representative of minimal length of a left coset, and {\tt false} otherwise
	Description
		Example
			R=rootSystemE(6)
			P=parabolic(R,set{1,2,3,4,5})
			w=minimalRepresentative ((longestWeylGroupElement R) % P)
			isMinimalRepresentative(w,P)
	Caveat
		This function is less efficient than the corresponding function for right cosets.
///

doc ///
	Key
		(isMinimalRepresentative, Parabolic, WeylGroupElement)
	Headline
		check whether an element of a Weyl group is the minimal representative of a right coset
	Usage
		minimalRepresentative(P,w)
	Inputs
		P: Parabolic
		w: WeylGroupElement
	Outputs
		: Boolean
			{\tt true} if {\tt w} is the representative of minimal length of a right coset, and {\tt false} otherwise
	Description
		Example
			R=rootSystemE(6)
			P=parabolic(R,set{1,2,3,4,5})
			w=minimalRepresentative (P % (longestWeylGroupElement R))
			isMinimalRepresentative(P,w)
///

doc ///
	Key
		(isMinimalRepresentative, Parabolic, WeylGroupElement, Parabolic)
	Headline
		check whether an element of a Weyl group is the minimal representative of a double coset
	Usage
		minimalRepresentative(P,w,Q)
	Inputs
		P: Parabolic
		w: WeylGroupElement
		Q: Parabolic
	Outputs
		: Boolean
			{\tt true} if {\tt w} is the representative of minimal length of a double coset, and {\tt false} otherwise
	Description
		Example
			R=rootSystemE(6)
			P=parabolic(R,set{1,2,3,4,5})
			Q=parabolic(R,set{2,3,4,5,6})
			w=minimalRepresentative ((P % (longestWeylGroupElement R)) % Q)
			isMinimalRepresentative(P,w,Q)
///

TEST ///
	R=rootSystemD(5);
	P=parabolic(R,set{2,3,4,5});
	w=longestWeylGroupElement R;
	c1=P % w;
	c2=w % P;
	d=P % c2;
	assert(reducedDecomposition(minimalRepresentative(c1*reduce(R,{1})))=={1,2,3,4,5,3,2});
	assert(reducedDecomposition(minimalRepresentative(reduce(R,{1})*c2))=={2,3,4,5,3,2,1});
	assert(reducedDecomposition(minimalRepresentative(d))=={1,2,3,4,5,3,2,1})
	assert(isMinimalRepresentative(w,P)==false);
	assert(isMinimalRepresentative(P,w)==false);
	assert(isMinimalRepresentative(P,w,P)==false);
///

doc ///
	Key
		DynkinDiagram
	Headline
		the class of Dynkin diagrams
	Description
		Text
			A Dynkin diagram contains lists representing vertices of the Dynkin diagram (in other words a simple root). Each of these lists contains sublists of end points of an edge from the vertex. There are in this order, the simple edges, double edge to a smaller root, double edge to a larger root, triple edge to a smaller root, triple edge to a larger root. If there is no such edge, we have an empty sublist.
		Example
			dynkinDiagram(rootSystemB(4))
			dynkinDiagram(rootSystemD(4))
///

doc ///
	Key
		dynkinDiagram
	Headline
		produce a Dynkin Diagram
///

doc ///
	Key
		(dynkinDiagram, RootSystem)
	Headline
		the Dynkin diagram of a root system
	Usage
		dynkinDiagram(R)
	Inputs
		R: RootSystem 
	Outputs
		: DynkinDiagram
			the Dynkin diagram of the root system R
	Description
		Example
			R1=rootSystemA(5)
			dynkinDiagram(R1)
			R2=rootSystemF4
			dynkinDiagram(R2)
///

doc ///
	Key
		(dynkinDiagram, DynkinDiagram, Parabolic)
	Headline
		the Dynkin diagram of the Levy subgroup of a parabolic 
	Usage
		dynkinDiagram(D,P)
	Inputs
		D: DynkinDiagram 
		P: Parabolic
	Outputs
		: DynkinDiagram
			the Dynkin diagram of the root system of the Levy subgroup of the Parabolic P 
	Description
		Example
			R=rootSystemD(5)
			D1=dynkinDiagram(R)
			P=parabolic(R,set{1,3,4,5})
			D2=dynkinDiagram(D1,P)
			dynkinType(D2)
///

TEST ///
	R=rootSystemD(5);
	D1=dynkinDiagram(R);
	P=parabolic(R,set{1,3,4,5});
	D2=dynkinDiagram(D1,P);
	assert(dynkinType(D1)==dynkinType{{"D",5}});
	assert(dynkinType(D2)==dynkinType{{"A",1},{"A",3}})
///

doc ///
	Key
		connectedComponents
	Headline
		get the connected components 
///

doc ///
	Key
		(connectedComponents,DynkinDiagram)
	Headline
		the connected components of a Dynkin diagram
	Usage
		connectedComponents(D)
	Inputs
		D: DynkinDiagram
	Outputs
		: List 
			containing Dynkin diagrams that are connected and whose disjoint union is D 
	Description
		Example
			R=rootSystemE(6)++rootSystemF4
			D=dynkinDiagram(R)
			connectedComponents(D)
///

TEST ///
	R=rootSystemE(6)++rootSystemF4
	D=dynkinDiagram(R)
	assert(apply(connectedComponents(D),dynkinType)=={dynkinType{{"E",6}},dynkinType{{"F",4}}})
///

doc ///
	Key
		endVertices	
	Headline
		the vertices with at most one edge 
///

doc ///
	Key
		(endVertices,DynkinDiagram)
	Headline
		the vertices of a Dynkin diagram with at most one neighbor 
	Usage
		connectedComponents(D)
	Inputs
		D: DynkinDiagram
	Outputs
		: List 
			containing the vertices of D whose removal does not disconnect any part of D. They can have at most one neighbor. 
	Description
		Example
			R=rootSystemE(6)
			D=dynkinDiagram(R)
			endVertices(D)
///

TEST ///
	R=rootSystemD(4);
        D=dynkinDiagram(R);
        assert(endVertices(D)===set{1,3,4})
///

doc ///
	Key
		DynkinType
	Headline
		the class of Dynkin Types 
	Description
		Text
			A Dynkin Type is a list of items representing Dynkin types of irreducible root systems. These types are a string (the letter representing the family A, B, C, D, E, F or G) and an integer, the rank.
		Example
			R=rootSystemB(4)++rootSystemA(6)
			dynkinType(R)
///

doc ///
	Key
		(symbol ++,DynkinType,DynkinType)
	Headline
		the disjoint union of Dynkin Types 
	Usage
		T1 ++ T2
	Inputs
		T1: DynkinType 
		T2: DynkinType 
	Outputs
		: DynkinType 
			the disjoint union (concatenation) of {\tt T1} and {\tt T2}
	Description
		Example
			T1=dynkinType(rootSystemB(2))
			T2=dynkinType(rootSystemA(3))
			T1++T2
		Text
			Note that it is not reordered.
///

doc ///
	Key
		(symbol ==,DynkinType,DynkinType)
	Headline
		the equality of Dynkin Types 
	Usage
		T1 == T2
	Inputs
		T1: DynkinType 
		T2: DynkinType 
	Outputs
		: Boolean 
			true if {\tt T1} is isomorphic to {\tt T2} and false if not
	Description
		Example
			T1=dynkinType(rootSystemB(2))
			T2=dynkinType(rootSystemA(3))
			(T1++T2) == (T2++T1)
		Text
		 	whereas
		Example
			(T1++T2) === (T2++T1)
///

doc ///
	Key
		dynkinType
	Headline
		obtaining a Dynkin type	
///

doc ///
	Key
		(dynkinType,DynkinDiagram)
	Headline
		the Dynkin type of a Dynkin diagram
	Usage
		dynkinType(D)
	Inputs
		D: DynkinDiagram
	Outputs
		: List 
			of types of the connected components of the Dynkin diagram {\tt D} 
	Description
		Example
			R=rootSystemE(6)++rootSystemA(4)++rootSystemB(3)++rootSystemA(4);
			D=dynkinDiagram(R);
			dynkinType(D)
///

doc ///
	Key
		(dynkinType,BasicList)
	Headline
		constructing a Dynkin type
	Usage
		dynkinType(L)
	Inputs
		L: BasicList
			of irreducible types that are lists containing a string of one letter from A to F and a positive integer, the rank 
	Outputs
		: DynkinType 
	Description
		Example
			dynkinType({{"A",2},{"B",3}})
		Text
			Note that low rank isomorphisms between classical groups are automatically used so that the letter is alphabetically minimal and the elementary types are irreducible (D2=A1xA1).
		Example
			dynkinType({{"A",2},{"B",1}})
			dynkinType({{"A",2},{"C",2}})
			dynkinType({{"A",2},{"D",2}})
///

doc ///
	Key
		(dynkinType,RootSystem)
	Headline
		the Dynkin type of a root system 
	Usage
		dynkinType(R)
	Inputs
		R: RootSystem 
	Outputs
		: List 
			of types of the irreducible components of the root system {\tt R} 
	Description
		Example
			R=rootSystemE(6)++rootSystemA(4)++rootSystemB(3)++rootSystemA(4);
			dynkinType(R)
///

TEST ///
	T1=dynkinType(rootSystemB(2));
	T2=dynkinType(rootSystemA(3));
	assert((T1++T2) == (T2++T1))

///

doc ///
	Key
		dynkinExponents
	Headline
		the exponents associated to a type
///

doc ///
	Key
		(dynkinExponents,DynkinType)
	Headline
		the exponents of the Dynkin type 
	Usage
		dynkinExponents(D)
	Inputs
		D: DynkinType 
	Outputs
		: List 
			of lists of integers, the exponents listed in the tables in Bourbaki, Lie groups and Lie algebras, following chapter 6. 
	Description
		Example
			R=rootSystemE(6)++rootSystemA(4)++rootSystemB(3)++rootSystemA(4);
			dynkinExponents(dynkinType(R))
///

TEST ///
	R=rootSystemE(6)++rootSystemA(4)++rootSystemB(3)++rootSystemA(4);
	assert(dynkinExponents(dynkinType(R))=={{1, 2, 3, 4}, {1, 2, 3, 4}, {1, 3, 5}, {1, 4, 5, 7, 8, 11}});
///

doc ///
	Key
		poincareSeries
	Headline
		a generating series for number of elements in a Weyl group
///

doc ///
	Key
		(poincareSeries,RootSystem,RingElement)
	Headline
		the generating series of the Weyl group by length 
	Usage
		poincareSeries(R,x)
	Inputs
		R: RootSystem 
		x: RingElement
	Outputs
		: RingElement 
			a polynomial in the variable {\tt x} where the coefficient in front of {\tt x^i} is the number of elements in the Weyl group of R of length i.
	Description
		Example
			R=rootSystemA(2)
			ZZ[x]
			poincareSeries(R,x)
///

doc ///
	Key
		(poincareSeries,RootSystem,Parabolic,RingElement)
	Headline
		the generating series of a quotient of the Weyl group by length 
	Usage
		poincareSeries(R,P,x)
	Inputs
		R: RootSystem 
		P: Parabolic
		x: RingElement
	Outputs
		: RingElement 
			a polynomial in the variable {\tt x} where the coefficient in front of {\tt x^i} is the number of cosets in the Weyl group of R modulo the Weyl group of P where the minimal element is of length i.
	Description
		Example
			R=rootSystemA(3)
			P=parabolic(R,set{1,2})
			ZZ[x]
			poincareSeries(R,P,x)
///

TEST /// 
	R=rootSystemA(3);
	P=parabolic(R,set{1,2});
	ZZ[x];
	assert(poincareSeries(R,x)==x^6+3*x^5+5*x^4+6*x^3+5*x^2+3*x+1);
	assert(poincareSeries(R,P,x)==x^3+x^2+x+1)
///

doc ///
	Key
		(poincareSeries,HasseDiagram,RingElement)
	Headline
		the generating series of a Hasse diagram 
	Usage
		poincareSeries(H,x)
	Inputs
		H: HasseDiagram 
		x: RingElement
	Outputs
		: RingElement 
			a polynomial in the variable {\tt x} where the coefficient in front of {\tt x^i} is the number of elements in the i-th row of {\tt H} (counting from 0). 
	Description
		Example
			R=rootSystemA(2)
			H=intervalBruhat(neutralWeylGroupElement R, longestWeylGroupElement R)
			ZZ[x]
			poincareSeries(H,x)
///

TEST ///
	R=rootSystemA(2);
	H=intervalBruhat(neutralWeylGroupElement R, longestWeylGroupElement R);
	ZZ[x];
	assert(poincareSeries(H,x)==x^3+2*x^2+2*x+1)
///

doc ///
	Key
		(rank,DynkinDiagram)
	Headline
		the rank of a Dynkin diagram
	Usage
		rank(D)
	Inputs
		D: DynkinDiagram
	Outputs
		: ZZ 
			the number of vertices of the Dynkin diagram {\tt D}
	Description
		Example
			R=rootSystemE(6)
			D=dynkinDiagram(R);
			rank(D)
///

TEST ///
	R=rootSystemE(6);
	D=dynkinDiagram(R);
	assert(rank(D)==6)
///

doc ///
	Key
		isLtBruhat
	Headline
		compare two Weyl group elements in the Bruhat order	
///

doc ///
	Key
		(isLtBruhat,WeylGroupElement,WeylGroupElement)
	Headline
		compare two Weyl group elements in the Bruhat order	
	Usage
		isLtBruhat(w1,w2)
	Inputs
		w1: WeylGroupElement 
		w2: WeylGroupElement
	Outputs
		: Boolean 
			{\tt true} if {\tt w1} is less then {\tt w2} in the Bruhat order
	Description
		Example
			R=rootSystemA(3)
			w1=reduce(R,{3,1,2});
			w2=reduce(R,{1,2,3,2,1});
			isLtBruhat(w1,w2)
///

TEST ///
	R=rootSystemA(3);
	w1=reduce(R,{3,1,2});
	w2=reduce(R,{1,2,3,2,1});
	w3=reduce(R,{1,2,3});
	assert(isLtBruhat(w1,w2));
	assert(isLtBruhat(w1,w3)==false)
///

doc ///
	Key
		underBruhat
	Headline
		obtain Weyl group elements less than an element for the Bruhat order	
///

doc ///
	Key
		(underBruhat,WeylGroupElement)
	Headline
		Weyl group elements just under a given one for the Bruhat order
	Usage
		underBruhat(w)
	Inputs
		w: WeylGroupElement 
	Outputs
		: List 
			of elements smaller than {\tt w} and of length one less together with the root whose reflection is composed with w (on the right) to obtain the element
	Description
		Example
			R=rootSystemA(3)
			L=underBruhat(longestWeylGroupElement(R))
			apply(L,x->reducedDecomposition (x#0))
///

doc ///
	Key
		(underBruhat,BasicList)
	Headline
		Weyl group elements just under the ones in the list for the Bruhat order
	Usage
		underBruhat(B)
	Inputs
		B: BasicList
			of WeylGroupElement 
	Outputs
		: List 
			of elements smaller than the ones in {\tt B} and of length one less together with the root of the reflection composed with (on the right) to obtain the element
	Description
		Example
			R=rootSystemA(3)
			L=underBruhat(longestWeylGroupElement(R))
			L1=apply(L,x->x#0)
			underBruhat(L1)
///

doc ///
	Key
		aboveBruhat
	Headline
		obtain Weyl group elements just greater than an element for the Bruhat order	
///

doc ///
	Key
		(aboveBruhat,WeylGroupElement)
	Headline
		Weyl group elements just above a given one for the Bruhat order
	Usage
		aboveBruhat(w)
	Inputs
		w: WeylGroupElement 
	Outputs
		: List 
			of elements greater than {\tt w} and of length one more together with the root whose reflection is composed with w (on the right) to obtain the element
	Description
		Example
			R=rootSystemA(3)
			L=aboveBruhat(neutralWeylGroupElement(R))
			apply(L,x->reducedDecomposition (x#0))
///

doc ///
	Key
		(aboveBruhat,BasicList)
	Headline
		The Weyl group elements just under the ones in the list for the Bruhat order
	Usage
		aboveBruhat(B)
	Inputs
		B: BasicList
			of WeylGroupElement 
	Outputs
		: List 
			of elements greater than the ones in {\tt B} and of length one more together with the root of the reflection composed with (on the right) to obtain the element
	Description
		Example
			R=rootSystemA(3)
			L=aboveBruhat(neutralWeylGroupElement(R))
			L1=apply(L,x->x#0)
			aboveBruhat(L1)
///

doc ///
	Key
		HasseDiagram
	Headline
		the class of Hasse diagrams
	Description
		Text
			A Hasse diagram is a list describing the connexions in a Hasse diagram. It contains sublists representing rows. Each row contains several vertices together with edges to some element in the next row given by its position in the row. This data format is intended for displaying.
///

doc ///
	Key
		HasseGraph
	Headline
		the class of Hasse graphs 
	Description
		Text
			A Hasse graph is a Hasse diagram ready for display. It contains labels instead of Weyl group elements and reflections.
///

doc ///
	Key
		hasseGraphToPicture
	Headline
		construct the picture of a Hasse Graph
///

doc ///
	Key
		(hasseGraphToPicture,HasseGraph)
	Headline
		Obtain a picture from a Hasse graph
	Usage
		hasseGraphToPicture(H)
	Inputs
		H: HasseGraph
	Outputs
		: Picture 
			describing the Hasse graph in the picture language of the package Graphics 
///

doc ///
	Key
		intervalBruhat
	Headline
		obtaining an interval for the Bruhat order	
///

doc ///
	Key
		(intervalBruhat,WeylGroupElement,WeylGroupElement)
	Headline
		elements between two given ones for the Bruhat order on a Weyl group
	Usage
		intervalBruhat(u,v)
	Inputs
		u: WeylGroupElement 
			the lower bound of the interval
		v: WeylGroupElement
			the upper bound of the interval
	Outputs
		: HasseDiagram 
			of elements smaller than {\tt u} and bigger than {\tt v} together with the reflections to go from an element to the next one (on the right) attached to the corresponding edge.
	Description
		Example
			R=rootSystemA(3)
			w1 = reduce(R,{2,1,2})
			w2 = reduce(R,{1,2,1,3,2})
			myInterval=intervalBruhat(w1,w2)
		Text
			Each row of the Hasse diagram contains the elements of a certain length together with their links to the next row.
		Example
			myInterval#1
///

doc ///
	Key
		(intervalBruhat,WeylGroupLeftCoset,WeylGroupLeftCoset)
	Headline
		elements between two given ones for the Bruhat order on a quotient of a Weyl group
	Usage
		intervalBruhat(u,v)
	Inputs
		u: WeylGroupLeftCoset
		v: WeylGroupLeftCoset
	Outputs
		: HasseDiagram 
			of elements of minimal length in their cosets, bigger than {\tt u} and smaller than {\tt v}, together with the reflections to go from an element to the next one (on the right) attached to the corresponding edge. 
	Description
		Example
			R=rootSystemA(3)
			P=parabolic(R,set {3})
			w1 = reduce(R,{2})
			w2 = reduce(R,{1,2,1,3,2})
			myInterval=intervalBruhat(w1 % P,w2 % P)
		Text
			Each row of the Hasse diagram contains the elements of a certain length together with their links to the next row.
		Example
			myInterval#1
///

doc ///
	Key
		(intervalBruhat,WeylGroupRightCoset,WeylGroupRightCoset)
	Headline
		elements between two given ones for the Bruhat order on a quotient of a Weyl group
	Usage
		intervalBruhat(u,v)
	Inputs
		u: WeylGroupRightCoset
		v: WeylGroupRightCoset
	Outputs
		: HasseDiagram 
			of elements of minimal length in their coset, bigger than {\tt u} and smaller than {\tt v}, together with the reflections to go from an element to the next one (on the right) attached to the corresponding edge. 
	Description
		Example
			R=rootSystemA(3)
			P=parabolic(R,set {3})
			w1 = reduce(R,{2})
			w2 = reduce(R,{1,2,1,3,2})
			myInterval=intervalBruhat(P % w1,P % w2)
		Text
			Each row of the Hasse diagram contains the elements of a certain length together with their links to the next row.
		Example
			myInterval#1
///

TEST ///
	R=rootSystemA(3);
	P=parabolic(R,set {3});
	w1 = reduce(R,{2});
	w2 = reduce(R,{1,2,1,3,2});
	myInterval=intervalBruhat(P % w1,P % w2);
    G = hasseDiagramToGraph(myInterval, "labels" => "reduced decomposition")
    -- the following test is dependent on the order chosen
    -- this test should be changed to check correctness, not the specific order of nodes
    assert(#G == 5)
    G = toList G;
    assert(G/length == {1,2,3,3,1})
    -- check that the labels are all as expected
    assert(G/(x -> x/first//set) === {
        set{"12132"}, 
        set{"2132", "1213"}, 
        set{"121", "213", "123"}, 
        set{"21", "23", "12"}, 
        set{"2"}})
    -- we should check that the links are correct too.
    -- the following is one possible answer, but it can change.  Why?!
    -*
      assert(G#0 == {{"12132", {{"3", 0}, {"2", 1}}}});
      assert(G#1 == {
              {"2132", {{"232", 1}, {"2", 2}}}, 
              {"1213", {{"1", 0}, {"3", 1}, {"232", 2}}}
              });
      assert(G#2 == {
              {"123", {{"3", 0}, {"12321", 2}}}, 
              {"121", {{"1", 0}, {"2", 1}}}, 
              {"213", {{"3", 1}, {"1", 2}}}
              });
      assert(G#3 == {{"12", {{"121", 0}}}, {"21", {{"1", 0}}}, {"23", {{"3", 0}}}})
      assert(G#4 == {{"2", {}}})
      *-
///

doc ///
	Key
		hasseDiagramToGraph
	Headline
		turning a hasse diagram into a graph (intended for graphic representation)
///

doc ///
	Key
		(hasseDiagramToGraph,HasseDiagram)
	Headline
		turning a hasse diagram into a graph (intended for graphic representation)
	Usage
		hasseDiagramToGraph(h)
	Inputs
		h: HasseDiagram 
	Outputs
		: HasseGraph
	Description
		Text
			By default, the HasseGraph will have vertices with empty labels.
		Example
			R=rootSystemA(3)
			w1 = reduce(R,{2})
			w2 = reduce(R,{1,2,1,3,2})
			myInterval=intervalBruhat(w1,w2)
			hasseDiagramToGraph(myInterval)
		Text
			It is also possible to ask for reduced decompositions as labels by changing the option "labels" as below.
		Example
			hasseDiagramToGraph(myInterval,"labels"=>"reduced decomposition")
///

TEST ///
	R=rootSystemA(3);
	w1 = reduce(R,{2,1,2});
	w2 = reduce(R,{1,2,1,3,2});
	myInterval=intervalBruhat(w1,w2);
	assert(hasseDiagramToGraph(myInterval)===new HasseGraph from {{{"", {{"", 0}, {"", 1}}}}, {{"", {{"", 0}}}, {"", {{"", 0}}}}, {{"", {}}}})
///

doc ///
	Key
		storeHasseGraph
	Headline
		store a Hasse graph in a file 
///

doc ///
	Key
		(storeHasseGraph,HasseGraph,String)
	Headline
		store a Hasse graph in a file
	Usage
		storeHasseGraph(H,f)
	Inputs
		H: HasseGraph
		f: String
			a file name
	Description
		Text
			Will store the Hasse graph {\tt H} in the file {\tt f} so that it can be retrieved later with "loadHasseGraph".
	SeeAlso	
		"loadHasseGraph(String)"
///

doc ///
	Key
		loadHasseGraph
	Headline
		load a Hasse graph from a file 
///

doc ///
	Key
		(loadHasseGraph,String)
	Headline
		load a Hasse graph from a file
	Usage
		loadHasseGraph(f)
	Inputs
		f: String
			a file name
	Description
		Text
			Will load a Hasse graph from the file {\tt f}.
	SeeAlso	
		"storeHasseGraph(HasseGraph,String)"
///



