--this file is in the public domain

newPackage( "SectionRing",
     Version => "0.2", Date => "September 21 2016", Authors => {
     	  {Name=> "Andrew Bydlon",
     	       Email=> "thelongdivider@gmail.com",
     	       HomePage => "http://www.math.utah.edu/~bydlon/"
     	       }
	  },
     PackageImports => {"Divisor", "Varieties"},
     Keywords => {"Commutative Algebra"},
     Headline => "the section ring of a Weil Divisor"
     )

export{
	"globallyGenerated",
	"isMRegular",
	"mRegular",
	"sectionRing",
	"isVectScalar",
	"convertScalarVect"
}

-----------------------------------------------------------------------

dualToIdeal = method();

dualToIdeal(Ideal) := (I) -> (
--Produces an ideal module isomorphic to the dual of the given ideal I.
	R := ring(I);
	M := module(I);
	embedAsIdeal(Hom(M,R),IsGraded=>true,ReturnMap=>true)
);

-----------------------------------------------------------------------

globallyGenerated = method();

globallyGenerated(WeilDivisor) := (D) -> (				
--Finds the smallest positive number (using a binary search) such that O_X(a*D) is globally generated, D ample.
	a:=1;

	while ((1%(baseLocus(a*D)) == 0) != true) do (
		a =2*a;
	);

	upperbound := a;
	lowerbound := ceiling(a/2);

	while (lowerbound < upperbound-1) do (
		a = ceiling((lowerbound + upperbound)/2);
		if ((1%(baseLocus(a*D)) == 0) != true) then (
			lowerbound = a;
		)
		else if ((1%(baseLocus(a*D)) == 0) == true) then (
			upperbound = a;
		);

	);
	upperbound
);


globallyGenerated(Ideal) := (I) -> (
--Finds when I^* is globally generated.
	globallyGenerated(divisor(I))
);

globallyGenerated(Module) := (M) -> (
	globallyGenerated(divisor(M))
);

-----------------------------------------------------------------------

isMRegular = method();

isMRegular(CoherentSheaf,CoherentSheaf,ZZ) := (F,G,m) ->(
--Outputs whether a sheaf F is m-regular in the sense of Castelnuovo relative to G
	V := variety(F);
	dV := dim(V);
	j:=1;
	bool := true;
	while(j<(dV+1)) do (
		if (bool == true) then(
			if(m!=j) then(
				bool = (HH^j((F**(G^**(m-j)))) == 0);
			)
			else if (m==j) then (
				bool = (HH^j(F) == 0);
			);
		);
		j = j+1;
	);
	bool
);

isMRegular(CoherentSheaf,ZZ) := (F,m) ->(
--Outputs whether F is m-regular (rel O_X(1))
	local V;
	local G;
	local mRegularParticular;	
	V = variety(F);
	G = OO_V(1);
	mRegularParticular(F,G,m)
);

-----------------------------------------------------------------------

mRegular = method();

mRegular(CoherentSheaf,CoherentSheaf) := (F,G) -> (
--Computes the regularity of the sheaf F relative to G, in the sense of Castelnuovo-Mumford, using a binary search
	bool0 := isMRegular(F,G,0);
	m:=0;
	lowerbound:=0;
	upperbound:=0;
	a:=0;

	if (bool0 == true) then (
--Tests for a negative-regularity in the case that F is 0-regular relative to G
		m=-1;
		while (isMRegular(F,G,m)) do (
			m=2*m;
		);

		lowerbound = m;
		upperbound = ceiling(m/2);

		while (lowerbound < upperbound-1) do (
			a = ceiling((lowerbound + upperbound)/2);
			if (isMRegular(F,G,a) != true) then (
				lowerbound = a;
			)
			else if (isMRegular(F,G,a) == true) then (
				upperbound = a;
			);
		);
	)

	else if (bool0==false) then (
--Tests for positive-regularity in the case that F is NOT 0-regular relative to G
		m=1;
		while (isMRegular(F,G,m) != true) do (
			m=2*m;
		);

		upperbound = m;
		lowerbound = ceiling(m/2);

		while (lowerbound < upperbound-1) do (
			a = ceiling((lowerbound + upperbound)/2);
			if (isMRegular(F,G,a) != true) then (
				lowerbound = a;
			)
			else if (isMRegular(F,G,a) == true) then (
				upperbound = a;
			);
		);
	);
	upperbound
);

mRegular(CoherentSheaf) := (F) -> ( 
--Computes the regularity of a sheaf F (relative OO_V(1))
	V := variety(F);					
	mRegular(F,(OO_V(1)))
);

mRegular(Ideal) := (I) -> (
--Returns the number m for which O_X(D) is m-regular, where  I is an ideal, and D is the corresponding divisor to I.
	R := ring(I);						
	F := sheaf(Hom(module(I),R));
	mRegular(F)
);

-----------------------------------------------------------------------

sectionRing = method();

sectionRing(Ideal) := (I) -> (
--Computes the section ring of a semi-ample divisor associated to I

	local L;
	local Rel;
	local KerT;
	local Part;
	local LengP;
	local LengPa;
	local numDegs;
	local AdmPart;
	local NumCols;
	local b;
	local e;

	R := ring(I);
					
--To Apply the Regularity Theorem of Mumford, the sheaf needs to be Globally Generated Sheaf. Thus in the case O_X(D) is not globally generated, we consider F=O_X(D),O_X(2D), ... , O_X((l-1)D) (which correspond to J#1, J#2,...) and F being relatively G-m-regular, where G = O_X(lD) is globally generated.  This produces bound, where all generators are found in lower degrees than bound.

	l := globallyGenerated(I);
	bound := l;
	G := first entries gens I;
	J :={0};

	j:=1;
	while(j<l+1) do (						
		J = J|{Hom(ideal(apply(G, z->z^j)),R)};
		j=j+1;
	);
	
	j=1;
	while(j<(l+1)) do (	
		bound = max(bound,l*(mRegular(sheaf(J#j),sheaf(J#l)))+j);
		j = j+1;
	);
	bound = bound + 1;

--The next block of code produces a polynomial ring S with generators in degrees 1,2,3,...,bound which will then be quotiented to produce the section ring.  Here Map_i Represents the map H^0(O_X(iD)) -> J^(i) and n_i is the rank of H^0(O_X(iD)).

	KK:= coefficientRing(R);
	Z := dualToIdeal(I);
	Shift := (Z#1)#0;
	J = {0,reflexify((Z#0))};
	FF := {0,basis(Shift,J#1)};
	n := {0,numColumns(FF#1)};
	F := {0,map(R^(numRows(FF#1)),R^(n#1),FF#1)};
	Map := {0,(gens J#1)*(F#1)};
	Y := local Y;
	myVars := {toList(Y_{1,1}..Y_{1,n#1})};						
	DegreeList :={};
	l=0;
	while(l<n#1) do(
		DegreeList = DegreeList | toList({1});
		l = l+1;
	);					
	i:=2;
	while (i < bound) do(
		J = J | {reflexivePower(i,J#1)};
		FF = FF | {basis((Shift*i),J#i)};
		n = n | {numColumns((FF#i))};			
		F = F | {map(R^(numRows((FF#i))),R^(n#i),FF#i)};
		Map = Map | {(gens J#i)*(F#i)};
		myVars = myVars | {toList(Y_{i,1}..Y_{i,n#i})};  
		l=0;
		while(l<n#i) do(
			DegreeList = DegreeList | toList({i});
			l = l+1;
		);
		i=i+1;
	);

	Vars := flatten myVars;
	numVars:= #Vars;

	S := KK [Vars,Degrees=>DegreeList];
	myVars = apply(myVars, z->apply(z,x->value(x)));
	numDegs = #myVars;

--The following block of code is used to compute the relations on S which define the section ring SR.  It does so by going degree by degree (starting at degree 2), and considering the morphisms \oplus_{i=0,\ldots,[j/2]} H^0((j-i)D) \otimes H^0(iD) --> H^0(jD), computing its kernel, and multiplying the matrix representing the kernel with the corresponding vector of variables of S, Vect_{j-i} \otimes Vect_i.  This gives relations, then inserted into RelIdeal.

	RelIdeal := ideal(0);
	Spar := S;
	Vect := {0};

	c:=1;
	while((c<bound) and (n#c>0)) do (
		Vect = Vect | {transpose matrix{myVars#(c-1)}};
		c=c+1;
	);

	j=2;
	while ( (dim(Spar) >  dim(R)) or (isDomain(Spar) != true)) do (	

--Relations are achieved by finding the kernels of the direct sums of tensor products of global sections.  However, some efficiency improvements can be achieve by considering a minimal number of such sums/tensors.  To do this, I consider partitions of the given degree of interest and throw out any partitions which are either above the bound in which our generators are considered, or can be factored through another partition.  For example, O_V(D)^{\otimes 3} -> O_V(2D)\otimes O_V(D) -> O_V(3D), so if bound>1, then the partition (1,1,1) of 3 is excluded.  Throughout, a is an index for which partition is chosen, and b is an index for an element of a given partition.  Additionally, MapTot is the total map of lower degree tensors into the degree in which relations are being considered, VectTot the corresponding vector of variables.

		Part = partitions(j);
		LengP = #(Part);
		a:=0;
		AdmPart = {};
		while (a<LengP) do(
			if((Part#a#0 < bound) and ((Part#a)#(#(Part#a)-1) + (Part#a)#(#(Part#a)-2) > min(bound,j)-1)) then (
				AdmPart = AdmPart | {(Part)#a}; 
			);
			a=a+1;
		);
	
		LengP = #(AdmPart);

		a=0;

		TotMapTemp := Map#(AdmPart#a#0);
		TotVectTemp := Vect#(AdmPart#a#0);
		b=1;
		LengPa = #((AdmPart)#a);
		while (b<LengPa) do (
			TotMapTemp = TotMapTemp ** Map#(AdmPart#a#b);
			TotVectTemp = TotVectTemp ** Vect#(AdmPart#a#b);
			b = b+1;
		);

		MapTot := TotMapTemp;
		VectTot := TotVectTemp;

		a=1;
		while(a<LengP) do ( 
			TotMapTemp = Map#(AdmPart#a#0);
			TotVectTemp = Vect#(AdmPart#a#0);

			b=1;
			LengPa = #(AdmPart#a);
			while (b<LengPa) do (
				TotMapTemp = TotMapTemp ** Map#(AdmPart#a#b);
				TotVectTemp = TotVectTemp ** Vect#(AdmPart#a#b);
				b = b+1;
			);

			MapTot = MapTot | TotMapTemp;
			VectTot = VectTot || TotVectTemp;
			a = a+1;
		);

		KerT = generators ker(MapTot);		

		NumCols = numColumns(KerT);
		e = 0;
			
		while (e < NumCols) do (
			L = flatten entries KerT_{e};
			if ((isVectScalar L) == true) then (
				L = convertScalarVect(S,L);
				Rel = sub((entries (matrix{L}*VectTot))#0#0,S);
				RelIdeal = trim(RelIdeal + ideal(Rel));
				Spar = S/RelIdeal;
			);
			e=e+1;
		); 
		j=j+1;
	);

--Some code to improve the presentation of the ring, both in terms of having a more standard list of generators A_1...A_N, and eliminating redundant generators

	A := local A;
	BetterS := KK[A_1..A_numVars,Degrees=>DegreeList];
	BetterMap := map(BetterS,S,toList(A_1..A_numVars));	
	BetterRelIdeal := BetterMap(sub(RelIdeal,S));
	minimalPresentation(BetterS/BetterRelIdeal)
);

-----------------------------------------------------------------------

sectionRing(WeilDivisor) := D -> (
	sectionRing(ideal(D))
);

-----------------------------------------------------------------------

isVectScalar = L -> (
	Ramb := ring (L#0); 
	all(L, z -> (degree(z) <= degree (sub(1, Ramb))) ) 
);

convertScalarVect = (newS, L) -> (apply(L, z->sub(z, newS)));

-----------------------------------------------------------------------

beginDocumentation();

doc ///
  Key
    SectionRing
  Headline
    computing the section ring of a Weil Divisor
  Description
    Text
      This package provides a method for computing the section ring of a Weil
      divisor.
///    

doc ///
   	Key
   	 dualToIdeal
   	Headline
   	  dual ideal
   	Usage
   	 dualToIdeal(I)
   	Inputs
	 I:Ideal
   	Outputs
   	 :Ideal
	   the dual of I
        Description
	 Text
	  Takes an ideal I as input, dualizes the ideal, and maps it back into the ring, producing Hom_R(I,R) ~ J < R.  Used to produce the global sections H^0(mD), where D is an integral divisor defined by I.
	  
///

doc ///
   	Key
   	 globallyGenerated
   	Headline
   	 globallyGenerated(D) produces a smallest integer a such that O_X(aD) is globally generated.
   	Usage
	  globallyGenerated(D)
   	  globallyGenerated(I)
	  globallyGenerated(M)
   	Inputs
	 D:WeilDivisor
	 I:Ideal
	 M:Module
   	Outputs
   	 :Number
        Description
	 Text
	  Takes a divisor as input.  It then uses a binary search to check for the smallest integer a with the property that |aD| is a basepoint-free linear series.  In this case, the corresponding line bundle is globally generated.
	  
///

doc ///
   	Key
   	 isMRegular
   	Headline
   	 isMRegular(F,G,m) tests where F is m-regular with respect to G (globally generated) in the sense of Castelnuovo-Mumford.  Omitting G assumes G=O_X(1).
   	Usage
	 isMRegular(F,G,m)
	 isMRegular(F,m)
   	Inputs
	 F:CoherentSheaf
	 G:CoherentSheaf
	 m:ZZ
   	Outputs
   	 :Boolean
        Description
	 Text
	  isMRegular(F,G,m) tests definition 1.8.4 of Lazarsfeld's Positivity in Algebraic Geometry I, which is to say whether H^i(F \otimes G^(m-i)) = 0 for every i>0.  It tests (in this order) H^1, H^2, \ldots, H^dim(X), and stops as soon as a non-zero cohomology is found.  If none is found, F is m-G-regular, and it outputs true.
	  
///

doc ///
   	Key
   	 mRegular
   	Headline
   	 mRegular(F,G) computes the regularity of F with respect to G (globally generated), in the sense of Castelnuovo-Mumford.  Omitting G assumes G=O_X(1).
   	Usage
	 mRegular(F,G)
	 mRegular(F)
   	Inputs
	 F:CoherentSheaf
	 G:CoherentSheaf
   	Outputs
   	 :ZZ
        Description
	 Text
	  mRegular(F,G) utilizes a binary search to compute the smallest m such that F is m-regular with respect to G, utilizing the function isMRegular.  mRegular(I) computes the regularity of O_X(D), where D is the associated divisor to I.
	  
///

doc ///
   	Key
   	 sectionRing
   	Headline
   	 sectionRing(I) produces the section ring of an ample divisor.  If I is an ideal, one can input I to get the section ring of the corresponding divisor.
   	Usage
	 sectionRing(I)
	 sectionRing(D)
   	Inputs
	 I:Ideal
	 D:WeilDivisor
   	Outputs
   	 :Ring
        Description
	 Text
	  sectionRing(I) begins by computing the regularity m of O_X, O_X(D), O_X(2D), ..., O_X((l-1)D) with respect to O_X(lD), where l is the output of globallyGenerated(D).  By Mumford's Thm (1.8.5 in Positivity) yields that each of the maps O_X(iD)\otimes O_X(lD)^\otimes m -> O_X((i+ml)D) is surjective.  Thus, all generators for the section ring can be assumed in lower degree than bound.  Thus it forms a polynomial ring S over the base field with h^0(iD)-many generators in degree i, for i=1,2,...,bound-1.  Next, relations in degree d are computing by considering the total maps \oplus_{partitions P of d} \otimes_{i\in P} O_X(i D) -> O_X(dD).  Each of these relations is then quotiented, until the point that a domain of the correct dimension is produced.  Some steps are then performed to make the output more readable and standard.
	  
///

TEST ///
R = QQ[x,y,z]/ideal(x^3+y^3-z^3);
I = ideal(x,y-z);
assert( globallyGenerated(I) == 2)
///


TEST ///
R = QQ[x,y,z]/ideal(x^4+y^4-z^4);
I = ideal(x,y-z);
assert( globallyGenerated(I) == 3)
///

TEST ///
R = QQ[x,y,z]/ideal(x^5+y^5-z^5);
I = ideal(x,y-z);
assert( globallyGenerated(I) == 4)
///

TEST ///
X = Proj(QQ[x,y,z,w,f]);
F = OO_X(4);
G = OO_X(-2);
assert( (mRegular(F) == -4) and (mRegular(G) == 2))
///

TEST ///
R = QQ[x,y,z,w,f];
I = ideal(x-y+w);
S = sectionRing I;
assert( (#(vars S)) == dim S)
///

TEST ///
R = QQ[x,y,z]/(x^3+y^3-z^3)
I = ideal(x,y-z);
S = sectionRing I;
J = ideal(S);
L = first entries gens J;
assert((#L==1) and ((degree(L#0))#0 == 6))
///

end
