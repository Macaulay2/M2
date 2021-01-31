--*- coding: utf-8 -*-
---------------------------------------------------------------------------
--
-- PURPOSE: Computations with convex polyhedra 
-- PROGRAMMER : Nathan Ilten, Josephine Yu, Qingchun Ren 
-- UPDATE HISTORY : August 2012 
---------------------------------------------------------------------------
newPackage("Polyhedra2",
    Headline => "convex polyhedra",
    Version => ".1",
    Date => "August 5, 2011",
    Authors => {
         {Name => "Nathan Ilten",
	  HomePage => "http://math.berkeley.edu/~nilten",
	  Email => "nilten@math.berkeley.edu"},
     	  {Name => "Qingchun Ren"},
	  {Name => "Josephine Yu"}
     },
    PackageImports => { "FourierMotzkin", "PolyhedralObjects","PolymakeInterface" },
    Configuration => {"DefaultUsePolymake"=>false}
    )

---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2012 Nathan Ilten, Josephine Yu, and Qingchun Ren 
-- Some parts copyright 2010 Rene Birkner
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------
DefaultUsePolymake := (options Polyhedra2).Configuration#"DefaultUsePolymake"


pmopt:={UsePolymake=>DefaultUsePolymake}
export { "isFace",
     "faces",   
         "mixedVolume",
	 "toSublattice",
	 "sublatticeBasis",
	 "volume",
         "triangulate",
         "ehrhart",
         "newtonPolytope",
         "crossPolytope",
	 "cyclicPolytope",
	 "hypercube",
	 "posOrthant",
	 "bipyramid",
	 "pyramid",
	  "stdSimplex",
	  "tailCone",
	  "latticePoints",
     	  "isCompact",
	  "affinePreimage",
     	  "affineImage",
        "hilbertBasis",
     	"coneToPolyhedron",
	"directProduct",
	"UsePolymake",
	"PolymakePath",
     	"emptyPolyhedron",
	"isEmpty",
	"minkowskiSum",
	"dualCone",
	"affineHull",
	"intersection",
	"linSpace",
	"vertices",
	"rays",
	"ambDim",
	"hyperplanes",
	"halfspaces",
	"contains",
	"convexHull", 
	"posHull"}

Cone == Cone := (C1,C2)->(
     contains(C1,C2) and contains(C2,C1)
     )

Polyhedron == Polyhedron := (C1,C2)->(
     contains(C1,C2) and contains(C2,C1)
     )

convexHull = method()
convexHull (Matrix,Matrix,Matrix):=(M,N,L)->(
     new Polyhedron from hashTable {
	  "Points"=>promote(homCoordinates(transpose M,transpose N),QQ),
     	  "InputLineality"=>promote(homRays(transpose L),QQ)}
     )

convexHull (Matrix,Matrix):=(M,N)->(convexHull(M,N,map(QQ^(numRows M),QQ^0,0)))
     
convexHull Matrix :=M->(convexHull(M,map(QQ^(numRows M),QQ^0,0)))

convexHull (Polyhedron,Polyhedron):=(P1,P2)->convexHull {P1,P2}
convexHull (Cone,Cone):=(C1,C2)->posHull {C1,C2}

convexHull List := L->(
   datalist:=apply(L,P->(
       if instance(P,Polyhedron) then (
	    if not P#?"Points" and not P#?"Vertices" then (rays P; linSpace P);
	    if P#?"Vertices" and P#?"LinealitySpace" then return(P#"Vertices",P#"LinealitySpace");
	    if not P#?"InputLineality" then P#"InputLineality"=map(QQ^0,numColumns P#"Points",0);
	    return (P#"Points",P#"InputLineality")
	    )
       else if instance(P,Cone) then (
	    if not P#?"InputRays" and not P#?"Rays" then (rays P; linSpace P);
	    if P#?"Rays" then return(homRays P#"Rays",homRays P#"LinealitySpace");
    	    if not P#?"InputLineality" then P#"InputLineality"=map(QQ^0,numColumns P#"InputRays",0);
	    return (homRays P#"InputRays",homRays P#"InputLineality")	   
    	   )
	else if instance(P,Matrix) then (
	     return (homPoints transpose promote(P,QQ),transpose map(QQ^(1+numRows P),QQ^0,0))
		    )
	      else return (promote(homCoordinates(transpose P#0,transpose P#1),QQ),
		   transpose map(QQ^(1+numRows P),QQ^0,0))));
    vlist:=matrix apply(datalist,i->{i#0});
    llist:=matrix apply(datalist,i->{i#1});
    new Polyhedron from hashTable {
	  "Points"=>vlist,
     	  "InputLineality"=>llist})


posHull = method()
posHull (Matrix, Matrix):= (M,N)-> (
     new Cone from hashTable {
	  "InputLineality"=>promote(transpose N,QQ),
  	  "InputRays"=>promote(transpose M,QQ)}
     )

posHull Matrix:=M ->(posHull(M,map(QQ^(numRows M),QQ^0,0)))
posHull (Cone,Cone):=(C1,C2)->(posHull {C1,C2})
posHull Polyhedron:=C1->(posHull {C1})
     
posHull List:=L->(
     datalist:=apply(L,P->(
	       if instance(P,Polyhedron) then (
		    if not P#?"Vertices" and not P#?"Points" then (rays P,linSpace P);
		    if P#?"Vertices" then return(dehom P#"Vertices",dehom P#"LinealitySpace");
	    	    if not P#?"InputLineality" then P#"InputLineality"=map(QQ^0,numColumns P#"Points",0);
		    return(dehom P#"Points",dehom P#"InputLineality")		    
		    )
	       else if instance(P,Cone) then (
		    if not P#?"Rays" and not P#?"InputRays" then (rays P; linSpace P);
		    if P#?"Rays" then return(P#"Rays",P#"LinealitySpace");
	    	    if not P#?"InputLineality" then P#"InputLineality"=map(QQ^0,numColumns P#"InputRays",0);
		    return(P#"InputRays",P#"InputLineality")		    		    
		    )
	       else if instance(P,Matrix) then (
		    return(promote(transpose P,QQ),map(QQ^0,numRows P,0))
		    )
	       else (
		    return(transpose P#0,transpose P#1)		    
		    )
	       ));
     rlist:= matrix apply(datalist,i->{i#0});
     llist:= matrix apply(datalist,i->{i#1});
     new Cone from hashTable {
	  "InputLineality"=>llist,
  	  "InputRays"=>rlist}     
     )

intersection = method()
intersection (Matrix,Matrix):=(M,N)->(
     if not numColumns N ==1 then return new Cone from hashTable {
	  "Equations"=>promote(- N,QQ),
  	  "Inequalities"=>promote(- M,QQ)};
     intersection(M,N,map(QQ^0,QQ^(1+numColumns M),0),map(QQ^0,QQ^0,0))
     )	  

intersection Matrix:=M->(intersection(M,map(QQ^0,QQ^(numColumns M),0)))

intersection (Matrix,Matrix,Matrix,Matrix):=(M,v,N,w)->(
     new Polyhedron from hashTable {
	  "Inequalities"=>promote(v|(-M),QQ),
 	  "Equations"=>promote(w|(-N),QQ)
	  }
     )     

intersection (Cone,Cone):=(P1,P2)->intersection {P1,P2}
intersection (Polyhedron,Polyhedron):=(P1,P2)->intersection {P1,P2}

intersection List := L -> (
     datalist:=apply(L,P->(
	       if instance(P,Polyhedron) then (
		    if not P#?"Facets" and not P#?"Inequalities" then (hyperplanes P;halfspaces P);
		    if P#?"Facets" then return(P#"Facets",P#"AffineHull");
		    if not P#?"Equations" then P#"Equations"=map(QQ^0,numColumns P#"Inequalities",0);
		    return(P#"Inequalities",P#"Equations")		    
		    )
	       else if instance(P,Cone) then (
		    if not P#?"Facets" and not P#?"Inequalities" then (hyperplanes P;halfspaces P);
		    if P#?"Facets" then return(homRays P#"Facets",homRays P#"LinearSpan");
		    if not P#?"Equations" then P#"Equations"=map(QQ^0,numColumns P#"Inequalities",0);		    
		    return(homRays P#"Inequalities",homRays P#"Equations")		    		    
		    )
	       else if instance(P,Sequence) then (
		    return(promote(P#1|(-P#0),QQ),map(QQ^(numRows P#0),QQ^(1+numColumns P#0),0))
		    )
	       else (
		    return(map(QQ^(numRows P#0),QQ^(1+numColumns P#0),0),promote(P#1|(-P#0),QQ))		    
		    )
	       ));
     ilist:=matrix apply(datalist,i->{i#0});
     elist:=matrix apply(datalist,i->{i#1});
     new Polyhedron from hashTable{
	  "Inequalities"=>ilist,
 	  "Equations"=>elist}
     )

hyperplanes = method(Options=>pmopt)
hyperplanes Cone := opts->P -> (
	if P#?"LinearSpan" then return P#"LinearSpan";
	if opts#UsePolymake then runPolymake(P,"LinearSpan")
	else computeFacets P;
	P#"LinearSpan")

hyperplanes Polyhedron := opts->P -> (
	if not P#?"AffineHull" then 
	if opts#UsePolymake then runPolymake(P,"AffineHull")
	else computeFacets P;
	M:=P#"AffineHull";
	(-M_(toList(1..numColumns M-1)),M_{0})
	)


halfspaces = method(Options=>pmopt)
halfspaces Cone := opts->P -> (
	if P#?"Facets" then return -P#"Facets";
	if opts#UsePolymake then runPolymake(P,"Facets")
	else computeFacets P;	
	-P#"Facets")
   
halfspaces Polyhedron := opts->P -> (
	if not P#?"Facets" then 
	if opts#UsePolymake then runPolymake(P,"Facets")
	else computeFacets P;
	computeFacets P;
	M:=P#"Facets";
	(-M_(toList(1..numColumns M-1)),M_{0})
	)



rays = method(Options=>pmopt)
rays Cone := opts->P -> (
	if P#?"Rays" then return transpose P#"Rays";
	if opts#UsePolymake then runPolymake(P,"Rays")
	else computeRays P;
	transpose P#"Rays")
   
rays Polyhedron := opts->P -> (
	if not P#?"Vertices" then 
	if opts#UsePolymake then runPolymake(P,"Vertices")
	else computeRays P;
	transpose (dehomCoordinates P#"Vertices")_1)   

vertices = method(Options=>pmopt)
vertices Polyhedron := opts->P -> (
	if not P#?"Vertices" then 
	if opts#UsePolymake then runPolymake(P,"Vertices")
        else computeRays P;
	transpose (dehomCoordinates P#"Vertices")_0)   

linSpace = method(Options=>pmopt)
linSpace Cone := opts->P -> (
	if P#?"LinealitySpace" then return transpose P#"LinealitySpace";
	if opts#UsePolymake then runPolymake(P,"LinealitySpace")
        else computeRays P;
	transpose P#"LinealitySpace")
   

linSpace Polyhedron := opts->P -> (
	if P#?"LinealitySpace" then return transpose dehom P#"LinealitySpace";
	if opts#UsePolymake then runPolymake(P,"LinealitySpace")
	else computeRays P;
	transpose dehom P#"LinealitySpace")
   


ambDim = method (Options=>pmopt)
ambDim Cone:=opts->C->(
     if not C#?"ConeAmbientDim" then (
     if opts#UsePolymake then runPolymake(C,"ConeAmbientDim")
     else
     (
     if C#?"Rays" then C#"ConeAmbientDim"=numColumns C#"Rays" 	  
     else if C#?"InputRays" then C#"ConeAmbientDim"=numColumns C#"InputRays" 	  
     else if C#?"Inequalities" then C#"ConeAmbientDim"=numColumns C#"Inequalities" 	  
     else if C#?"Facets" then C#"ConeAmbientDim"=numColumns C#"Facets"
     else error ("Your cone is ill-defined"))); 	  
      C#"ConeAmbientDim")
 
ambDim Polyhedron:=opts->C->(
     if not C#?"ConeAmbientDim" then (
     if opts#UsePolymake then runPolymake(C,"ConeAmbientDim")
     else
     (	  
     if C#?"Vertices" then C#"ConeAmbientDim"=numColumns C#"Vertices"  	  
     else if C#?"Points" then C#"ConeAmbientDim"=numColumns C#"Points" 	  
     else if C#?"Inequalities" then C#"ConeAmbientDim"=numColumns C#"Inequalities" 	  
     else if C#"Facets" then C#"ConeAmbientDim"=numColumns C#"Facets"
     else error ("Your cone is ill-defined"))); 	  
      C#"ConeAmbientDim"-1)


dim Cone:=C->(if not C#?"ConeDim" then C#"ConeDim"=ambDim C-numRows ((hyperplanes C));C#"ConeDim")
dim Polyhedron:=C->(if not C#?"ConeDim" then C#"ConeDim"=ambDim C-numRows ((hyperplanes C)_0);C#"ConeDim")

affineHull = method ()
affineHull Polyhedron := P->(hp:=hyperplanes P;
     intersection(map(QQ^0,QQ^(1+ambDim P),0),map(QQ^0,QQ^0,0),hp#0,hp#1))

dualCone = method ()
dualCone Cone:=C->(
     C2:=new Cone from hashTable {};
     if C#?"InputRays" then C2#"Inequalities"=C#"InputRays";
     if C#?"InputLineality" then C2#"Equations"=C#"InputLineality";
     if C#?"Rays" then C2#"Facets"=C#"Rays";     
     if C#?"LinealitySpace" then C2#"LinearSpan"=C#"LinealitySpace";
     if C#?"Facets" then C2#"Vertices"=C#"Facets";
     if C#?"LinearSpan" then C2#"LinealitySpace"=C#"LinearSpan";
     if C#?"Inequalities" then C2#"InputRays"=C#"Inequalities";
     if C#?"Equations" then C2#"InputLineality"=C#"Equations";
     C2)


affineImage = method ()
affineImage (Matrix,Polyhedron,Matrix) := (A,P,v)->(
     if not P#?"Points" and not P#?"Vertices" then (vertices P,linSpace P);
     Q:=new Polyhedron from hashTable {};
     M:=(transpose (map(QQ^1,1+numColumns A,(i,j)->if j==0 then 1 else 0)||(v|A)));
     if P#?"Vertices" then (Q#"Points"=P#"Vertices"*M;     
     	  Q#"InputLineality"=P#"LinealitySpace"*M)
     else  (Q#"Points"=P#"Points"*M;
	    if not P#?"InputLineality" then P#"InputLineality"=map(QQ^0,numColumns P#"Points",0);
	    Q#"InputLineality"=P#"InputLineality"*M);
     Q)

affineImage (Matrix,Polyhedron) := (A,P)->(affineImage(A,P,map(QQ^(numRows A),QQ^1,0)))
     
affineImage (Polyhedron,Matrix) := (P,v)->(affineImage(id_(QQ^(ambDim P)),P,v))     



affineImage (Matrix,Cone):=(A,P)->(
     if not P#?"InputRays" and not P#?"Rays" then (rays P,linSpace P);
     Q:=new Cone from hashTable {};
     if P#?"Rays" then (Q#"InputRays"=P#"Rays"*(transpose (A));     
     	  Q#"InputLineality"=P#"LinealitySpace"*(transpose (A)))
     else  (Q#"InputRays"=P#"InputRays"*(transpose (A));
	    if not P#?"InputLineality" then P#"InputLineality"=map(QQ^0,numColumns P#"InputRays",0);	  
      Q#"InputLineality"=P#"InputLineality"*(transpose (A)));
     Q)

affineImage (Matrix,Cone,Matrix) := (A,P,v)->(
     if v==0 then return affineImage(A,P);
     affineImage(A,coneToPolyhedron P,v))

affineImage (Cone,Matrix) :=(P,v)->(
     if v==0 then return P;
     affineImage(coneToPolyhedron,v))

affinePreimage = method ()
affinePreimage(Matrix,Polyhedron,Matrix) := (A,P,b) -> (
     --note: could set up to use eq/ineq if facets don't exist
     -- Checking for input errors
     if ambDim P =!= numRows A then error("Matrix source must be ambient space");
     if numRows A =!= numRows b then error("Vector must lie in target space of matrix");
     if numColumns b =!= 1 then error("Second argument must be a vector");
     -- Constructing the new half-spaces and hyperplanes
     (M,v) := halfspaces P;
     (N,w) := hyperplanes P;
     v = v - (M * b);
     w = w - (N * b);
     M = M * A;
     N = N * A;
     intersection(M,v,N,w))


affinePreimage(Matrix,Polyhedron) := (A,P) -> (
     affinePreimage(A,P,map(target A,QQ^1,0)))

affinePreimage(Polyhedron,Matrix) := (P,b) -> affinePreimage(map(QQ^(ambDim P),QQ^(ambDim P),1),P,b)

affinePreimage(Matrix,Cone,Matrix) := (A,C,b) -> if b == 0 then affinePreimage(A,C) else affinePreimage(A,coneToPolyhedron C,b)

affinePreimage(Matrix,Cone) := (A,C) -> posHull affinePreimage(A,coneToPolyhedron C)

affinePreimage(Cone,Matrix) := (C,b) -> affinePreimage(coneToPolyhedron C,b)

latticePoints = method(TypicalValue => List,Options=>pmopt)
latticePoints Polyhedron := opts -> P -> (
     if not P#?"LatticePoints" then (
	  if opts#UsePolymake then runPolymake(P,"LatticePoints")
	  else (
	  -- Checking for input errors
	  if  not isCompact P then error("The polyhedron must be compact");
	  -- Recursive function that intersects the polyhedron with parallel hyperplanes in the axis direction
	  -- in which P has its minimum extension
	  latticePointsRec := P -> (
	       -- Finding the direction with minimum extension of P
	       V := entries vertices P;
	       n := ambDim P;
	       minv := apply(V,min);
	       maxv := apply(V,max);
	       minmaxv := maxv-minv;
	       pos := min minmaxv;
	       pos = position(minmaxv,v -> v == pos);
	       -- Determining the lattice heights in this direction
	       L := toList({{ceiling minv_pos}}..{{floor maxv_pos}});
	       -- If the dimension is one, then it is just a line and we take the lattice points
	       if n == 1 then apply(L,matrix)
	       -- Otherwise intersect with the hyperplanes and project into the hyperplane
	       else flatten apply(L,p -> (
			 NP := intersection {P,{map(QQ^1,QQ^n,(i,j) -> if j == pos then 1 else 0),matrix p}};
			 if numColumns vertices P == 1 then (
			      v := vertices NP;
			      if promote(substitute(v,ZZ),QQ) == v then substitute(v,ZZ) else {})
			 else (
			      A := matrix drop((entries id_(QQ^n)),{pos,pos});
			      apply(latticePointsRec affineImage(A,NP),v -> v^{0..(pos-1)} || matrix p || v^{pos..(n-2)})))));
	  -- Checking if the polytope is just a point
	  if dim P == 0 then P#"LatticePoints" = if liftable(vertices P,ZZ) then homPoints transpose lift(vertices P,ZZ) else map(ZZ^0,ZZ^(ambDim P,0))
	  -- Checking if the polytope is full dimensional
	  else if (dim P == ambDim P) then P#"LatticePoints" = homPoints transpose matrix {latticePointsRec P}
	  -- If not checking first if the affine hull of P contains lattice points at all and if so projecting the polytope down
	  -- so that it becomes full dimensional with a map that keeps the lattice
	  else (
	       (M,v) := hyperplanes P;
	       -- Finding a lattice point in the affine hull of P
	       b := if all(entries M, e -> gcd e == 1) then (
		    -- Computing the Smith Normal Form to solve the equation over ZZ
		    (M1,Lmatrix,Rmatrix) := smithNormalForm substitute(M,ZZ);
		    v1 := flatten entries (Lmatrix*v);
		    w := apply(numRows M1, i -> M1_(i,i));
		    -- Checking if the system is at least solvable over QQ
		    if all(#w, i -> w#i != 0 or v1#i == 0) then (
			 -- If it is, then solve over QQ
			 w = apply(#w, i -> (v1#i/w#i,v1#i%w#i));
			 if all(w, e -> e#1 == 0) then (
			      -- If the solution is in fact in ZZ then return it
			      w = transpose matrix{apply(w,first) | toList(numColumns M1 - numRows M1:0)};
			      Rmatrix * w)));
	       -- If there is no lattice point in the affine hull then P has none
	       if b === null then P#"LatticePoints" = homPoints map(ZZ^0,ZZ^(ambDim P),0)
	       else (
		    A := gens ker substitute(M,ZZ);
		    -- Project the translated polytope, compute the lattice points and map them back
		    P#"LatticePoints" = homPoints transpose matrix {apply(latticePoints affinePreimage(A,P,b),e -> substitute(A*e + b,ZZ))}))));
     apply(numRows dehom P#"LatticePoints",i->(transpose dehom P#"LatticePoints")_{i})
     )



emptyPolyhedron=method ()
emptyPolyhedron ZZ:=n->(
     if n < 1 then error("The ambient dimension must be positive");
     C:=convexHull map(QQ^n,QQ^0,0);
     C#"ConeAmbientDim"=n;
     C#"ConeDim"=-1;
     C)
     

isEmpty=method()
isEmpty Polyhedron:=P->(dim P==-1)

isCompact = method(TypicalValue => Boolean)
isCompact Polyhedron := P -> (linSpace P == 0 and rays P == 0)


-- PURPOSE : Tests if the first Polyhedron/Cone is a face of the second Polyhedron/Cone
isFace = method(TypicalValue => Boolean)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
isFace(Polyhedron,Polyhedron) := (P,Q) -> (
     -- Checking if the two polyhedra lie in the same space and computing the dimension difference
     c := dim Q - dim P;
     if ambDim P == ambDim Q and c >= 0 then (
	  -- Checking if P is the empty polyhedron
	  if c > dim Q then true
	  -- Checking if one of the codim 'c' faces of Q is P
	  else any(faces(c,Q), f -> f === P))
     else false)

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
isFace(Cone,Cone) := (C1,C2) -> (
     c := dim C2 - dim C1;
     -- Checking if the two cones lie in the same space and the dimension difference is positive
     if ambDim C1 == ambDim C2 and c >= 0 then (
	  -- Checking if one of the codim 'c' faces of C2 is C1
	  any(faces(c,C2), f -> f === C1))
     else false)





minkowskiSum = method()

minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
     if (ambDim P1) =!= (ambDim P2) then error("Polyhedral objects must lie in the same space");
     if isEmpty P1 or isEmpty P2 then emptyPolyhedron ambDim P1 else if P1 == P2 then 2 * P1;
     V1 := vertices P1;
     V2 := vertices P2;
     R := promote(rays P1 | rays P2,QQ) | map(target V1,QQ^1,0);
     Vnew := map(target V1,QQ^0,0);
     -- Collecting all sums of vertices of P1 with vertices of P2
     Vnew = matrix {unique flatten apply(numColumns V1, i -> apply(numColumns V2, j -> V1_{i}+V2_{j}))};
     convexHull(Vnew,R))



minkowskiSum(Cone,Cone) := (C1,C2) -> (
     -- Checking for input errors
     if (ambDim C1) =!= (ambDim C2) then error("Cones must lie in the same space");
     posHull((rays C1)|(rays C2),(linSpace C1)|linSpace C2))


minkowskiSum(Cone,Polyhedron) := (C,P) -> minkowskiSum(coneToPolyhedron C,P)

minkowskiSum(Polyhedron,Cone) := (P,C) -> minkowskiSum(P,coneToPolyhedron C)

QQ * Polyhedron := (k,P) -> (
     -- Checking for input errors
     if k <= 0 then error("The factor must be strictly positiv");
     Q:=new Polyhedron from hashTable {};
     if P#?"Points" then Q#"Points"=homCoordinates(k*(dehomCoordinates P#"Points")_0,(dehomCoordinates P#"Points")_1);
     if P#?"InputLineality" then Q#"InputLineality"=P#"InputLineality";
     if P#?"Vertices" then Q#"Vertices"=homCoordinates(k*(dehomCoordinates P#"Vertices")_0,(dehomCoordinates P#"Vertices")_1);
     if P#?"LinealitySpace" then Q#"LinealitySpace"=P#"LinealitySpace";
     if P#?"Inequalities" then Q#"Inequalities"=((k*(P#"Inequalities")_{0})|(P#"Inequalities")_(toList(1..numColumns P#"Inequalities")));
     if P#?"Equations" then Q#"Equations"=P#"Equations";
     if P#?"Facets" then Q#"Facets"=((k*(P#"Facets")_{0})|(P#"Facets")_(toList(1..numColumns P#"Facets"-1)));
     if P#?"AffineHull" then Q#"AffineHull"=P#"AffineHull";
     if P#?"AmbDim" then Q#"AmbDim"=P#"AmbDim";
     if P#?"ConeDim" then Q#"ConeDim"=P#"ConeDim";
     Q)
      
coneToPolyhedron=method()
coneToPolyhedron Cone:=P->(
     Q:=new Polyhedron from hashTable {};
     if P#?"InputRays" then Q#"Points"=homRays(P#"InputRays");
     if P#?"InputLineality" then Q#"InputLineality"=homRays P#"InputLineality";
     if P#?"Rays" then Q#"Vertices"=homRays P#"Rays";
     if P#?"LinealitySpace" then Q#"LinealitySpace"=homRays P#"LinealitySpace";
     if P#?"Inequalities" then Q#"Inequalities"=homRays P#"Inequalities";
     if P#?"Equations" then Q#"Equations"=homRays P#"Equations";
     if P#?"Facets" then Q#"Facets"=homRays P#"Facets";
     if P#?"LinSpan" then Q#"LinSpan"=homRays P#"LinSpan";
     if P#?"AmbDim" then Q#"AmbDim"=P#"AmbDim";
     if P#?"ConeDim" then Q#"ConeDim"=P#"ConeDim";      
      Q)


ZZ * Polyhedron := (k,P) -> promote(k,QQ) * P


Cone + Polyhedron := minkowskiSum
Cone + Cone := minkowskiSum

directProduct = method ()
directProduct(Cone,Polyhedron) :=(C,P)->directProduct((coneToPolyhedron C),P)
directProduct (Polyhedron ,Cone):=(P,C)->directProduct(P,(coneToPolyhedron C))

directProduct (Polyhedron, Polyhedron):=(P1,P2)->(
     --very lazy implementation; we should really check what keys exist
     C:=convexHull((vertices P1)++(vertices P2),(rays P1)++(rays P2),linSpace P1++linSpace P2);
     C#"LinealitySpace"=C#"InputLineality";
     C#"Vertices"=C#"Points";
     C     )

directProduct (Cone, Cone):=(P1,P2)->(
     --very lazy implementation; we should really check what keys exist
     C:=posHull(rays P1++rays P2,linSpace P1++linSpace P2);
     C#"LinealitySpace"=C#"InputLineality";
     C#"Rays"=C#"InputRays";
     C
     )

PolyhedralObject * PolyhedralObject := directProduct
Polyhedron + Polyhedron := minkowskiSum
Polyhedron + Cone := minkowskiSum



-- PURPOSE : Computing the Hilbert basis of a Cone 
--   INPUT : 'C',  a Cone
--  OUTPUT : 'L',  a list containing the Hilbert basis as one column matrices 
hilbertBasis = method(TypicalValue => List,Options=>pmopt)
hilbertBasis Cone := opts->C -> (
     if C#?"HilbertBasis" then return (apply(numRows C#"HilbertBasis",i->(transpose C#"HilbertBasis")_{i}));
     if opts#UsePolymake then runPolymake(C,"HilbertBasis")
     else (
     -- Computing the row echolon form of the matrix M
     ref := M -> (
	  n := numColumns M;
	  s := numRows M;
	  BC := map(ZZ^n,ZZ^n,1);
	  m := min(n,s);
	  -- Scan through the first square part of 'M'
	  i := 0;
	  stopper := 0;
	  while i < m and stopper < n do (
		    -- Selecting the first non-zero entry after the i-th row in the i-th column
		    j := select(1,toList(i..s-1),k -> M_i_k != 0);
		    -- if there is a non-zero entry, scan the remaining entries and compute the reduced form for this column
		    if j != {} then (
			 j = j#0;
			 scan((j+1)..(s-1), k -> (
				   if M_i_k != 0 then (
					a := M_i_j;
					b := M_i_k;
					L := gcdCoefficients(a,b);
					a = substitute(a/(L#0),ZZ);
					b = substitute(b/(L#0),ZZ);
					M = M^{0..j-1} || (L#1)*M^{j} + (L#2)*M^{k} || M^{j+1..k-1} || (-b)*M^{j} + a*M^{k} || M^{k+1..s-1})));
			 if i != j then (
			      M = M^{0..i-1} || M^{j} || M^{i+1..j-1} || M^{i} || M^{j+1..s-1});
			 if M_i_i < 0 then M = M^{0..i-1} || -M^{i} || M^{i+1..s-1})
		    else (
			 M = M_{0..i-1} | M_{i+1..n-1} | M_{i};
			 BC = BC_{0..i-1} | BC_{i+1..n-1} | BC_{i};
			 i = i-1);
		    i = i+1;
		    stopper = stopper + 1);
	  (M,BC));
     -- Function to compute the/one preimage of h under A
     preim := (h,A) -> (
	  -- Take the generators of the kernel of '-h|A' and find an element with 1 as first entry -> the other entrys are a preimage
	  -- vector
	  N := gens ker(-h|A);
	  N = transpose (ref transpose N)#0;
	  N_{0}^{1..(numRows N)-1});
     A := -halfspaces C;
     if hyperplanes C =!= 0 then A = A || hyperplanes C || -hyperplanes C;
     A = substitute(A,ZZ);
     -- Use the project and lift algorithm to compute a basis of the space of vectors positive on 'A' whose preimages are the HilbertBasis
     (B,BC) := ref transpose A; 
     H := constructHilbertBasis B;
     BC = inverse transpose BC;
     C#"HilbertBasis"=transpose matrix {apply(H,h -> preim(BC*h,A))});
     (apply(numRows C#"HilbertBasis",i->(transpose C#"HilbertBasis")_{i})))







-- PURPOSE : Check if 'P' contains 'Q'
contains = method(TypicalValue => Boolean)
contains(Polyhedron,Polyhedron) := (P,Q) -> (
      vertices Q;
      halfspaces P;
      hyperplanes P;
      linSpace Q;
      A:=Q#"Vertices";
      B:=Q#"LinealitySpace";
      C:=P#"Facets";
      D:=P#"AffineHull";
       ((A||B) * transpose D)==0 and (B*(transpose C)==0) and
            all(flatten entries ( A* transpose C),i->i>=0))


contains(Cone,Cone) := (P,Q) -> (
      vertices Q;
      halfspaces P;
      hyperplanes P;
      linSpace Q;
      A:=Q#"Rays";
      B:=Q#"LinealitySpace";
      C:=P#"Facets";
      D:=P#"LinearSpan";
       ((A||B) * transpose D)==0 and (B*(transpose C)==0) and
            all(flatten entries ( A* transpose C),i->i>=0))

contains(Polyhedron,Matrix) := (P,p) -> (
      -- checking for input errors
      if ambDim P =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(P,convexHull p))

contains(Cone,Matrix) := (C,p) -> (
      -- checking for input errors
      if ambDim C =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(C,convexHull p))




contains(Cone,Polyhedron):=(P,Q)->(coneToPolyhedron P,Q)
contains(Polyhedron,Cone):=(P,Q)->(P,coneToPolyhedron Q)

contains(List,Cone) := (L,C) -> any(L, C1 -> C1 == C)
contains(List,Polyhedron) := (L,P) -> any(L, Q -> Q == P)


tailCone = method(TypicalValue => Cone)
tailCone Polyhedron := P -> posHull(rays P,linSpace P) --computes more than necessary

stdSimplex = method(TypicalValue => Polyhedron)
stdSimplex ZZ := d -> (
     -- Checking for input errors
     if d < 0 then error("dimension must not be negative");
     -- Generating the standard basis
     convexHull map(QQ^(d+1),QQ^(d+1),1))


posOrthant = method(TypicalValue => Cone)
posOrthant ZZ := n -> posHull map(QQ^n,QQ^n,1)

pyramid = method(TypicalValue => Polyhedron)
pyramid Polyhedron := P -> (
     vertices P;
     A:=P#"Vertices";
     B:=P#"LinealitySpace";
     n:=ambDim P;
     new Polyhedron from hashTable{
	  "Vertices"=>(A|(map(QQ^(numRows A),QQ^1,0))||map(QQ^1,QQ^(n+2),(j,i)->(if i==0 or i==n+1 then 1 else 0))),
	  "LinealitySpace"=>B|map(QQ^(numRows B),QQ^1,0)
     })
     
-- PURPOSE : Computing the bipyramid over the polyhedron 'P'
--   INPUT : 'P',  a polyhedron 
--  OUTPUT : A polyhedron, the convex hull of 'P', embedded into ambientdim+1 space and the 
--     	         points (barycenter of 'P',+-1)
bipyramid = method(TypicalValue => Polyhedron)
bipyramid Polyhedron := P -> (
     -- Saving the vertices
     V := promote(vertices P,QQ);
     linSpace P;
     A:=P#"Vertices";
     B:=P#"LinealitySpace";
     n := numColumns V;
     if n == 0 then error("P must not be empty");
     -- Computing the barycenter of P
     v := matrix toList(n:{1_QQ});
     v = (1/n)*V*v;
     vplus:=matrix {{1}} | (transpose v) | matrix {{1}};
     vminus:=matrix {{1}} | (transpose v) | matrix {{-1}};
     new Polyhedron from hashTable{
	  "Vertices"=>(A|(map(QQ^(numRows A),QQ^1,0))||vplus||vminus),
	  "LinealitySpace"=>B|map(QQ^(numRows B),QQ^1,0)
     })


-- PURPOSE : Generating the 'd'-dimensional crosspolytope with edge length 2*'s'
crossPolytope = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and 's' is
--     	    	       a strictly positive rational number, the distance of the vertices to the origin
--  OUTPUT : The 'd'-dimensional crosspolytope with vertex-origin distance 's'
crossPolytope(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if d < 1 then error("dimension must at least be 1");
     if s <= 0 then error("size of the crosspolytope must be positive");
     constructMatrix := (d,v) -> (
	  if d != 0 then flatten {constructMatrix(d-1,v|{-1}),constructMatrix(d-1,v|{1})}
	  else {v});
     homVert := (matrix {toList(2*d:1_QQ)} || (map(QQ^d,QQ^d,s) | map(QQ^d,QQ^d,-s)));
     new Polyhedron from hashTable {
	  "Vertices"=>transpose homVert,
	  "LinealitySpace"=>map(QQ^0,QQ^(d+1),0)})


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and 's' is a
--     	    	        strictly positive integer, the distance of the vertices to the origin
crossPolytope(ZZ,ZZ) := (d,s) -> crossPolytope(d,promote(s,QQ))


--   INPUT :  'd',  where 'd' is a strictly positive integer, the dimension of the polytope
crossPolytope ZZ := d -> crossPolytope(d,1_QQ)

-- PURPOSE : Computing the cyclic polytope of n points in QQ^d
--   INPUT : '(d,n)',  two positive integers
--  OUTPUT : A polyhedron, the convex hull of 'n' points on the moment curve in 'd' space 
-- COMMENT : The moment curve is defined by t -> (t,t^2,...,t^d) in QQ^d, if we say we take 'n' points 
--            on the moment curve, we mean the images of 0,...,n-1
cyclicPolytope = method(TypicalValue => Polyhedron)
cyclicPolytope(ZZ,ZZ) := (d,n) -> (
     -- Checking for input errors
     if d < 1 then error("The dimension must be positive");
     if n < 1 then error("There must be a positive number of points");
     convexHull map(ZZ^d, ZZ^n, (i,j) -> j^(i+1)))

-- PURPOSE : Generating the 'd'-dimensional hypercube with edge length 2*'s'
hypercube = method(TypicalValue => Polyhedron)

--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and
--     	    	       's' is a positive rational number, half of the edge length
--  OUTPUT : The 'd'-dimensional hypercube with edge length 2*'s' as a polyhedron
hypercube(ZZ,QQ) := (d,s) -> (
     -- Checking for input errors
     if d < 1 then error("dimension must at least be 1");
     if s <= 0 then error("size of the hypercube must be positive");
     -- Generating half-spaces matrix and vector
     intersection(map(QQ^d,QQ^d,1) || -map(QQ^d,QQ^d,1),matrix toList(2*d:{s})))


--   INPUT : '(d,s)',  where 'd' is a strictly positive integer, the dimension of the polytope, and
--     	    	       's' is a positive integer, half of the edge length
hypercube(ZZ,ZZ) := (d,s) -> hypercube(d,promote(s,QQ))

     
--   INPUT : 'd',  is a strictly positive integer, the dimension of the polytope 
hypercube ZZ := d -> hypercube(d,1_QQ)

-- PURPOSE : Computing the Newton polytope for a given (Laurent) polynomial
--   INPUT : 'p',  a RingElement
--  OUTPUT : The polyhedron that has the exponent vectors of the monomials of 'p' as vertices
newtonPolytope = method(TypicalValue => Polyhedron)
newtonPolytope RingElement := p -> (
     if class class p===PolynomialRing then convexHull transpose matrix exponents p
     else if class class p===FractionField then (
	  f:=numerator p;
	  l:=(exponents denominator p);
	  if #l =!=1 then error("Not a (Laurent) polynomial");
	  convexHull transpose matrix apply(exponents f,i->i-l_0))
     else error ("Not a (Laurent) polynomial"))
	  
-- PURPOSE : Computing the Ehrhart polynomial of a polytope
--   INPUT : 'P',  a polyhedron which must be compact, i.e. a polytope
--  OUTPUT : A polynomial in QQ[x], the Ehrhart polynomial
-- COMMENT : Compactness is checked within latticePoints
ehrhart = method(TypicalValue => RingElement,Options =>pmopt)
ehrhart Polyhedron := opts->P -> (
	n := dim P;
	if not	P#?"EhrhartPolynomialCoeff" then (
	     if opts#UsePolymake then runPolymake(P,"EhrhartPolynomialCoeff")
	     else (
		v := matrix apply(n,k -> {-1+#latticePoints( (k+1)*P)});
		     M := promote(matrix apply(n,i -> reverse apply(n, j -> (i+1)^(j+1))),QQ);
		     P#"EhrhartPolynomialCoeff"={1}|reverse flatten entries ((inverse M)*v)));
        R := QQ[getSymbol "x"];
	x := R_"x";
	sum apply(n+1,i -> P#"EhrhartPolynomialCoeff"#i * x^(i)))

-- PURPOSE : Triangulating a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A list of the simplices of the triangulation. Each simplex is given by a list 
--    	     of its vertices.
--COMMENTS : The triangulation is build recursively, for each face that is not a simplex it takes 
--     	     the weighted centre of the face. for each codim 1 face of this face it either takes the 
--     	     convex hull with the centre if it is a simplex or triangulates this in the same way.
triangulate = method()
triangulate Polyhedron := P -> (
     -- Defining the recursive face triangulation
     -- This takes a polytope and computes all facets. For each facet that is not a simplex, it calls itself
     -- again to replace this facet by a triangulation of it. then it has a list of simplices triangulating 
     -- the facets. Then it computes for each of these simplices the convex hull with the weighted centre of 
     -- the input polytope. The weighted centre is the sum of the vertices divided by the number of vertices.
     -- It returns the resulting list of simplices in a list, where each simplex is given by a list of its 
     -- vertices.
     -- The function also needs the dimension of the Polyhedron 'd', the list of facets of the original 
     -- polytope, the list 'L' of triangulations computed so far and the dimension of the original Polytope.
     -- 'L' contains a hash table for each dimension of faces of the original Polytope (i.e. from 0 to 'n').
     -- If a face has been triangulated than the list of simplices is saved in the hash table of the 
     -- corresponding dimension with the weighted centre of the original face as key.
     recursiveFaceTriangulation := (P,d,originalFacets,L,n) -> (
	  -- Computing the facets of P, given as lists of their vertices
	  F := intersectionWithFacets({(set P,set{})},originalFacets);
	  F = apply(F, f -> toList(f#0));
	  d = d-1;
	  -- if the facets are at least 2 dimensional, then check if they are simplices, if not call this 
	  -- function again
	  if d > 1 then (
	       F = flatten apply(F, f -> (
			 -- Check if the face is a simplex
			 if #f != d+1 then (
			      -- Computing the weighted centre
			      p := (sum f)*(1/#f);
			      -- Taking the hash table of the corresponding dimension
			      -- Checking if the triangulation has been computed already
			      if L#d#?p then L#d#p
			      else (
				   -- if not, call this function again for 'f' and then save this in 'L'
				   (f,L) = recursiveFaceTriangulation(f,d,originalFacets,L,n);
				   L = merge(L,hashTable {d => hashTable{p => f}},(x,y) -> merge(x,y,));
				   f))
			 else {f})));
	  -- Adding the weighted centre to each face simplex
	  q := (sum P)*(1/#P);
	  P = apply(F, f -> f | {q});
	  (P,L));
     -- Checking for input errors
     if not isCompact P then error("The polytope must be compact!");
     n := dim P;
     -- Computing the facets of P as lists of their vertices
     (HS,v) := halfspaces P;
     (HP,w) := hyperplanes P;
     originalFacets := apply(numRows HS, i -> intersection(HS,v, HP || HS^{i}, w || v^{i}));
     originalFacets = apply(originalFacets, f -> (
	       V := vertices f;
	       (set apply(numColumns V, i -> V_{i}),set {})));
     -- Making a list of the vertices of P
     P = vertices P;
     P = apply(numColumns P, i -> P_{i});
     if #P == n+1 then {P} else (
	  d := n;
	  -- Initiating the list of already computed triangulations
	  L := hashTable apply(n+1, i -> i => hashTable {});
	  (P,L) = recursiveFaceTriangulation(P,d,originalFacets,L,n);
	  P))

-- PURPOSE : Computing the volume of a full dimensional polytope
--   INPUT : 'P',  a compact polyhedron
--  OUTPUT : QQ, giving the volume of the polytope
volume = method(TypicalValue => QQ,Options=>pmopt)
volume Polyhedron := opts->Q -> (
     d := dim(Q);
     if not Q#?"LatticeVolume" then (
       	    if  not isCompact Q then error("The polyhedron must be compact, i.e. a polytope.");
	    if opts#UsePolymake then runPolymake(Q,"LatticeVolume")
	    else  (
     -- If P is not full dimensional then project it down
     P:=Q;
     if d != ambDim Q then (
	  A := substitute((hyperplanes Q)#0,ZZ);
	  A = inverse (smithNormalForm A)#2;
	  n := ambDim Q;
	  A = A^{n-d..n-1};
	  P = affineImage(A,Q));
     -- Computing the triangulation of P
     P = triangulate P;
     -- Computing the volume of each simplex without the dimension factor, by 
     -- taking the absolute of the determinant of |v_1-v_0..v_d-v_0|
     P = apply(P, p -> abs det matrix transpose apply(toList(1..d), i -> flatten entries(p#i - p#0)));
     -- Summing up the volumes and dividing out the dimension factor
     Q#"LatticeVolume"=(sum P)));
     (Q#"LatticeVolume")/d!)
	       

sublatticeBasis = method(TypicalValue => Matrix,Options=>pmopt)

--   INPUT : 'M',  a Matrix
--  OUTPUT : A matrix, a basis of the sublattice spanned by the lattice points in 'M'
sublatticeBasis Matrix := pmopt-> M -> (
     -- Checking for input errors
     M = promote(M,QQ);
     M = if promote(substitute(M,ZZ),QQ) == M then substitute(M,ZZ) else error("The matrix must contain only lattice points.");
     -- The sublattice is isomorphic to source mod kernel, i.e. A/K
     A := source M; 
     K := ker M;
     -- Taking minimal generators and applying M gives a basis in target M
     M*(mingens (A/K)))


--   INPUT : 'P',  a polyhedron,
--  OUTPUT : A matrix, a basis of the sublattice spanned by the lattice points of 'P'
sublatticeBasis Polyhedron := opts->P -> (
     L := latticePoints(P,opts);
     -- Checking for input errors
     if L == {} then error("The polytope must contain lattice points.");
     -- Translating 'P' so that it contains the origin if it did not already
     if all(L,l -> l != 0) then L = apply(L, l -> l - L#0);
     sublatticeBasis(matrix {L}))
   
   
-- PURPOSE : Calculating the preimage of a polytope in the sublattice generated by its lattice points
--   INPUT : 'P',  a polyhedron
--  OUTPUT : A polyhedron, the projected polyhedron, which is now normal
toSublattice = method(Options=>pmopt)
toSublattice Polyhedron := opts->P -> (
     L := latticePoints P;
     -- Checking for input errors
     if L == {} then error("The polytope must contain lattice points.");
     b := L#0;
     -- Translating 'P' so that it contains the origin if it did not already
     if all(L,l -> l != 0) then L = apply(L, l -> l - L#0);     
     affinePreimage(sublatticeBasis matrix {L},P,b))

-- PURPOSE : Computes the mixed volume of n polytopes in n-space
--   INPUT : 'L'  a list of n polytopes in n-space
--  OUTPUT : the mixed volume
-- COMMENT : Note that at the moment the input is NOT checked!
mixedVolume = method()
mixedVolume List := L -> (
     n := #L;
     Elist := apply(L, P -> apply(faces(dim P -1,P),vertices));
     liftings := apply(n, i -> map(ZZ^n,ZZ^n,1)||matrix{apply(n, j -> random 25)});
     Qlist := apply(n, i -> affineImage(liftings#i,L#i));
     local Qsum;
     Qsums := apply(n, i -> if i == 0 then Qsum = Qlist#0 else Qsum = Qsum + Qlist#i);
     mV := 0;
     Elist = apply(n, i -> apply(Elist#i, e -> (e,(liftings#i)*e)));
     E1 := Elist#0;
     Elist = drop(Elist,1);
     center := matrix{{1/2},{1/2}};
     edgeTuple := {};
     k := 0;
     selectRecursion := (E1,edgeTuple,Elist,mV,Qsums,Qlist,k) -> (
	  for e1 in E1 do (
	       Elocal := Elist;
	       if Elocal == {} then mV = mV + (volume sum apply(edgeTuple|{e1}, et -> convexHull first et))
	       else (
		    Elocal = for i from 0 to #Elocal-1 list (
			 HP := halfspaces(Qsums#k + Qlist#(k+i+1));
			 HP = for j from 0 to numRows(HP#0)-1 list if (HP#0)_(j,n) < 0 then ((HP#0)^{j},(HP#1)^{j}) else continue;
			 returnE := select(Elocal#i, e -> (
				   p := (sum apply(edgeTuple|{e1}, et -> et#1 * center)) + (e#1 * center);
				   any(HP, pair -> (pair#0)*p - pair#1 == 0)));
			 --if returnE == {} then break{};
			 returnE);
		    mV = selectRecursion(Elocal#0,edgeTuple|{e1},drop(Elocal,1),mV,Qsums,Qlist,k+1)));
	  mV);
     selectRecursion(E1,edgeTuple,Elist,mV,Qsums,Qlist,k))

-- PURPOSE : Computing the faces of codimension 'k' of 'P'
--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'P'  plus one a polyhedron
--  OUTPUT : a List, containing the faces as polyhedra
faces = method(TypicalValue => List)
faces(ZZ,Polyhedron) := (k,P) -> (
     if k== 0 then return {P};
     if k == dim P +1 then (
	  return {emptyPolyhedron ambDim P};
	  );
     L:=linSpace P;
     d := dim P - k;
     dl := numColumns L;
     if d < dl then return {};
     if k== dim P then return apply(numColumns vertices P,i->convexHull((vertices P)_{i}));
     if not P.?cache then P.cache = new MutableHashTable from hashTable {};
     if not P.cache#?"Faces" then P.cache#"Faces" = new MutableHashTable from hashTable {};
     if not P.cache#"Faces"#?k then (faceBuilder(k,P));
     apply(P.cache#"Faces"#k,i->(local j;
	       if i#1===set {} then j=map(QQ^(ambDim P),QQ^0,0) else j=matrix {toList i#1};
	  convexHull(matrix {toList i#0},j,L))))



--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a cone
--  OUTPUT : a List, containing the faces as cones
faces(ZZ,Cone) := (k,P) -> (
     if k== 0 then return {P};
     L:=linSpace P;
     d := dim P - k;
     dl := numColumns L;
     if d < dl then return {};
     if k== dim P then return {posHull map(QQ^(ambDim P),QQ^1,0)};
     if not P.?cache then P.cache = new MutableHashTable from hashTable {};
     if not P.cache#?"Faces" then P.cache#"Faces" = new MutableHashTable from hashTable {};
     if not P.cache#"Faces"#?k then (faceBuilderCone(k,P));
     apply(P.cache#"Faces"#k,i->(
	  posHull(matrix {toList i},L))))








--Non-exported stuff

--rows are coordinates as in Polymake
homCoordinates=(M,N)->((map(QQ^(numRows M),QQ^1,(i,j)->1)|M)||(map(QQ^(numRows N),QQ^1,0)|N))
homRays=(N)->(map(QQ^(numRows N),QQ^1,0)|N)
homPoints=(N)->(map(QQ^(numRows N),QQ^1,(i,j)->1)|N)
--makes first coordinate 1 or 0
normalizeCoordinates=M->transpose matrix {apply(numRows M,i->(v:=(transpose M)_{i};
	  if v_(0,0)==0 then return  v;
	  ((1/(v_(0,0)))*v)))}
--assume that coordinates are normalized
dehom=M->transpose (transpose (M_(toList(1..numColumns M-1))))
     
dehomCoordinates=M->(
     MT:=transpose M;
     DM:=transpose (M_(toList(1..numColumns M-1)));
     verticesp:=select(numRows M,i->(MT_{i})_(0,0)==1);
     raysp:=select(numRows M,i->(MT_{i})_(0,0)==0);
     (transpose DM_verticesp,transpose DM_raysp))


computeFacets = method ()
computeFacets Cone := C -> (
     local fm;
     if not C#?"Rays" and not C#?"InputRays" then computeRays C;
     if C#?"Rays" then fm=fourierMotzkin(transpose  C#"Rays",transpose C#"LinealitySpace")
     else fm=fourierMotzkin(transpose  C#"InputRays",transpose C#"InputLineality");
     C#"Facets"=-transpose fm_0;
     C#"LinearSpan"=-transpose fm_1;
     )
     
computeFacets Polyhedron :=C->(
     local fm;
     if not C#?"Vertices" and not C#?"Points" then computeRays C;     
     if C#?"Vertices" then fm=fourierMotzkin(transpose  C#"Vertices",transpose C#"LinealitySpace")
     else fm=fourierMotzkin(transpose  C#"Points",transpose C#"InputLineality");
     C#"Facets"=-transpose fm_0;
     C#"AffineHull"=-transpose fm_1;     
     )

computeRays = method ()
computeRays Cone := C -> (
     local fm;
     if not C#?"Facets" and not C#?"Inequalities" then computeFacets C;
     if C#?"Facets" then fm=fourierMotzkin(transpose C#"Facets",transpose C#"LinearSpan")
     else fm=fourierMotzkin(transpose C#"Inequalities",transpose C#"Equations");
     C#"Rays"=-transpose fm_0;
     C#"LinealitySpace"=-transpose fm_1;
     )

computeRays Polyhedron := C -> (
     local fm;
     if not C#?"Facets" and not C#?"Inequalities" then computeFacets C;
     if C#?"Facets" then fm=fourierMotzkin(
	  transpose (C#"Facets"||map(QQ^1,QQ^(numColumns C#"Facets"),(i,j)->(if j==0 then 1 else 0))),transpose C#"AffineHull")
     else fm=fourierMotzkin(
	  transpose (C#"Inequalities"||map(QQ^1,QQ^(numColumns C#"Inequalities"),(i,j)->(if j==0 then 1 else 0))),transpose C#"Equations");
     C#"Vertices"=normalizeCoordinates (-transpose fm_0);
     C#"LinealitySpace"=-transpose fm_1;
     )

     

constructHilbertBasis = A -> (
    -- Defining the function to determine if u is lower v
    lowvec := (u,v) -> (
	 n := (numRows u)-1;
	 diffvec := flatten entries(u-v);
	 if all(diffvec, i -> i <= 0) then abs(u_(n,0)) <= abs(v_(n,0)) and (u_(n,0))*(v_(n,0)) >= 0
	 else false);
    -- Collecting data
    A = substitute(A,ZZ);
    H := {A^{0}_{0}};
    s := numRows A;
    n := numColumns A;
    --doing the project and lift algorithm step by step with increasing dimensions
    scan(n-1, i -> (
	      -- the set 'F' will contain the lifted basis vectors, 'B' are the first i+2 columns of 'A' as a rowmatrix,
	      -- the set 'H' contains the vectors from the last loop that are one dimension smaller
	      F := {};
	      B := transpose A_{0..(i+1)};
	      -- Decide between lifting the existing vectors (i > s-1) or also adding the next column of 'B'
	      if i < s-1 then (
		   -- Lifting the existing vectors from 'H'
		   F = apply(H, h -> (
			     j := 0;
			     while numRows h == i+1 do (
				  if isSubset(image(h || matrix{{j}}), image B) then h = (h || matrix{{j}});
				  j = j+1);
			     h));
		   -- Adding +- times the next column of 'B'
		   F = join(F,{B_{i+1}^{0..(i+1)},-B_{i+1}^{0..(i+1)}}))
	      else (
		   -- Lifting the existing vectors from 'H'
		   nullmap := map(ZZ^1,ZZ^s,0);
		   nullvec := map(ZZ^1,ZZ^1,0);
		   F = apply(H, h -> B*substitute(vertices intersection(nullmap,nullvec,B^{0..i},h),ZZ)));
	      -- Computing the S-pairs from the elements of 'F' and saving them in 'C'
	      C := select(subsets(#F,2), j -> (
			f := F#(j#0);
			g := F#(j#1);
			(f_(i+1,0))*(g_(i+1,0)) < 0 and f+g != 0*(f+g)));
	      C = apply(C, j -> F#(j#0)+F#(j#1));
	      -- The elements of 'F' are saved in 'G'
	      G := F;
	      j := 0;
	      -- Adding those elements of 'C' to 'G' that satisfy the "normalform" condition by increasing last entry
	      while C != {} do (
		   Cnow := partition(e -> sum drop(flatten entries e,-1) == j,C);
		   C = if Cnow#?false then Cnow#false else {};
		   Cnow = if Cnow#?true then select(Cnow#true, f -> all(G, g -> not lowvec(g,f))) else {};
		   Cnew := flatten apply(Cnow, f -> apply(select(G, g -> f_(i+1,0)*g_(i+1,0) < 0 and f+g != 0*(f+g)), g -> f+g));
		   if all(Cnew, e -> sum drop(flatten entries e,-1) != j) then j = j+1;
		   C = unique (C | Cnew);
		   G = unique (G | Cnow));
	      -- saving those elements of 'G' with positive last entry into 'H'
	      H = select(G, g -> g_(i+1,0) >= 0)));
    H)

-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections that
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of Sequences each containing a set of vertices and a set of rays giving the faces of a 
--     	    	   certain dimension of a polyhedron
--     	     'F', a list of Sequences each containing a set of vertices and a set of rays giving the facets 
--     	    	   of the same polyhedron
--  OUTPUT : a list of Sequences each containing a set of vertices and a set of rays giving the faces 
--     	    	   of the same polyhedron one dimension lower then the ones in 'L'
intersectionWithFacets = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> if e#0 =!= set{} then e =!= l else false;
	  newL := {};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := ((l#0)*(f#0),(l#1)*(f#1));
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			      if isValid(e,l) then (
				   if not any(newL, g -> isSubset(e#0,g#0) and isSubset(e#1,g#1)) then (
					newL = select(newL, g -> not (isSubset(g#0,e#0) and isSubset(g#1,e#1)))|{e}))))));
	  newL);


-- PURPOSE : intersect every face in L with every facet in F and return the inclusion maximal intersections that
--     	     are not equal to one element in L
--   INPUT : 'L',  a list of sets each containing the rays of the faces of a certain dimension of a polyhedron
--     	     'F', a list of sets each containing the rays of the facets of the same polyhedron
--  OUTPUT : a list of sets each containing the rays of the faces of the same polyhedron one dimension lower 
--     	     then the ones in 'L'
intersectionWithFacetsCone = (L,F) -> (
	  -- Function to check if 'e' has at least one vertex and is not equal to 'l'
	  isValid := (e,l) -> if e =!= set{} then e =!= l else false;
	  newL := {};
	  -- Intersecting each element of 'L' with each element of 'F'
	  scan(L, l -> (
		    scan(F, f -> (
			      e := l*f;
			      -- if the intersection is valid add it to newL if it is not contained in one of the elements 
			      -- already in newL and remove those contained in 'e'
			     if isValid(e,l) then (
				  if not any(newL, g -> isSubset(e,g)) then (
					newL = select(newL, g -> not isSubset(g,e))|{e}))))));
	  newL);



faceBuilder = (k,P) -> (
     --Checking for input errors
     if k < 1 or k > dim P then error("the codimension must be between 1 and the dimension of the polyhedron");
     i := (max (keys (P.cache#"Faces")|{1}));
      if k < i then return;
     --otherwise:

	       if i == 1 then (
		    -- Saving the half-spaces and hyperplanes
		    (HS,v) := halfspaces P;
		    (HP,w) := hyperplanes P;
		    -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
		    Fl := apply(numRows HS, i -> (intersection(HS,v,HP || HS^{i},w || v^{i})));
		    Fl = apply(Fl, f -> (
			      V := vertices f;
			      R := rays f;
			      (set apply(numColumns V, i -> V_{i}),set apply(numColumns R, i -> R_{i}))));
		    i = 2;
		    P.cache#"Faces"#1 = Fl);
	       F := P.cache#"Faces"#1;
	       i = i - 1;
	       L := P.cache#"Faces"#i;
	       -- Intersecting L k-1 times with F and returning the maximal inclusion sets which are the faces of codim plus 1
	       while i < k do (
		    L = intersectionWithFacets(L,F);
		    i = i+1;
		    P.cache#"Faces"#i = L);)

faceBuilderCone = (k,P) -> (
     --Checking for input errors
     if k < 1 or k > dim P then error("the codimension must be between 1 and the dimension of the cone");
     i := (max (keys (P.cache#"Faces")|{1}));
      if k < i then return;
     --otherwise:

	       if i == 1 then (
		    -- Saving the half-spaces and hyperplanes
		    HS := halfspaces P;
		    HP := hyperplanes P;
		    -- Generating the list of facets where each facet is given by a list of its vertices and a list of its rays
	       Fl := apply(numRows HS, i -> intersection(HS,HP || HS^{i}));
	       Fl = apply(Fl, f -> (
			 R := rays f;
			 (set apply(numColumns R, i -> R_{i}))));
		    i = 2;
		    P.cache#"Faces"#1 = Fl);
	       F := P.cache#"Faces"#1;
	       i = i - 1;
	       L := P.cache#"Faces"#i;
	       -- Intersecting L k-1 times with F and returning the maximal inclusion sets which are the faces of codim plus 1
	       while i < k do (
		    L = intersectionWithFacetsCone(L,F);
		    i = i+1;
		    P.cache#"Faces"#i = L);)	  
	  








beginDocumentation()




