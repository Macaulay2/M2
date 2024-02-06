-- -*- coding: utf-8 -*-
newPackage(
	"LexIdeals", 
	Version => "1.2",
	Date => "13 October 2008",
	Authors => {
		{Name => "Chris Francisco", 
		     Email => "chris@math.okstate.edu", 
		     HomePage => "http://www.math.okstate.edu/~chris"}
	},
	Headline => "lexicographic-type monomial ideals",
	Keywords => {"Commutative Algebra"},
	DebuggingMode => false
	)

export {"macaulayRep", "macaulayBound", "macaulayLowerOperator", "isHF", "hilbertFunct", 
     "isCM", "lexIdeal", "isLexIdeal", "isPurePower", "LPP", "generateLPPs", "isLPP", "cancelAll", 
     "multUpperHF", "multLowerBound", "multUpperBound", "multBounds", "PrintIdeals", "MaxDegree"}

--gives the d-th Macaulay representation of a.
--use for finding upper bound for Hilbert function in degree d+1
macaulayRep = method(TypicalValue=>List)
macaulayRep(ZZ,ZZ) := (a,d) -> (
     macaulayrep:={};
     topnum:=d-1;
     bottomnum:=d;
     aprime:=a;
     --find the largest topnum so that binomial(topnum,bottomnum) <= a
     while aprime > 0 do (
	  while binomial(topnum,bottomnum) < aprime do (
	       topnum=topnum+1;
	  );
     	  if binomial(topnum,bottomnum) != aprime then topnum=topnum-1;
	  --append the new binomial coefficient to the output list
	  macaulayrep=append(macaulayrep,{topnum,bottomnum});
	  aprime=aprime-binomial(topnum,bottomnum);
	  bottomnum=bottomnum-1;
	  topnum=bottomnum-2;
     );
     macaulayrep
);

--gives upper bound of Hilbert function in degree d+1 from
--Macaulay's Theorem (given that the HF is a in degree d)
macaulayBound = method(TypicalValue=>ZZ)
macaulayBound(ZZ,ZZ) := (a,d) -> (
     macaulayrep:=macaulayRep(a,d); --get macaulay representation
     aUpperd:=apply(macaulayrep, li->{(li#0)+1,(li#1)+1});
     sum(apply(aUpperd,li->binomial(li#0,li#1)))
);

--gives a_<d> operator: given a = (b_d choose d) + ... + (b_i choose i)
--returns (b_d - 1 choose d) + ... + (b_i - 1 choose i)
--see Bruns-Herzog, page 161
macaulayLowerOperator = method(TypicalValue=>ZZ)
macaulayLowerOperator(ZZ,ZZ) := (a,d) -> (
     macaulayrep:=macaulayRep(a,d); --get macaulay representation
     aUpperd:=apply(macaulayrep, li->{(li#0)-1,(li#1)});
     sum(apply(aUpperd,li->binomial(li#0,li#1)))
);

--takes a finite length list of nonnegative integers and
--tells whether the sequence is an O-sequence (must begin with 1)
isHF = method(TypicalValue=>Boolean)
isHF List := (hilb) -> (
     result:=true;
     if not all(hilb,i->instance(i,ZZ)) then result=false else(
	  if hilb#0 != 1 then result=false else (
	       leng:=#hilb;
	       degr:=1;
	       while result==true and degr < leng-1 do (
	       	    if hilb#(degr+1) > macaulayBound(hilb#degr,degr) then 
	       	    result=false else degr=degr+1;
	  	    );
     	       );
	  );
     result
)

--return the Hilbert function of R/I as a list, where I is a homogeneous 
--ideal in a ring R, and R is a standardly graded polynomial ring or a 
--standardly graded quotient of a polynomial ring.
--if no degree bound is specified for a non-Artinian ideal, the default 
--bound is degree 20.
hilbertFunct = method(TypicalValue=>List,Options => {MaxDegree => null})
hilbertFunct(Ideal) := opts -> I -> (
     dimR:=dim ambient ring I;
     dimen:=dim I;
     h := poincare I;
     t := (ring h)_0;
     h = h // (1-t)^(dimR-dimen); --difference instead of codim to
     R := ZZ[local u];     	          --make it work in quotient rings
     h = substitute(h, {t=>u});
     optDeg:=opts.MaxDegree;
     if dimen==0 then (
	  if optDeg===null then optDeg=first degree h else h = h % u^(optDeg+1);
	  )
     else if dimen!=0 and optDeg===null then optDeg=20;
     uDeg := (1-u^(optDeg+1)) // (1-u);
     for i from 1 to dimen do (
     	  h = (h * uDeg);
	  h = h % u^(optDeg+1));
     reverse apply(listForm h, x -> x#1))

--determines whether (ring I/I) is Cohen-Macaulay, where ring I is a polynomial ring
isCM = method(TypicalValue=>Boolean)
isCM(Ideal) := (I) -> (
     R:=ring I;
     if not instance(R,PolynomialRing) then error "expected ideal in a polynomial ring";
     if I == ideal(1_R) then true else
     (pdim coker gens I == codim I)
)

--add trailing zeros so that two lists have same length
makeSameLength = method(TypicalValue=>List)
makeSameLength(List,List) := (list1,list2) -> (
     length1:=#list1;
     length2:=#list2;
     lengthdiff:=abs(length1-length2);
     if length1 > length2 then list2=join(list2,toList(lengthdiff:0))
          else list1=join(list1,toList(lengthdiff:0));
     {list1,list2}
)

--given a polynomial ring R and Artinian Hilbert function hilb,
--returns the lex ideal in R corresponding to that HF
lexIdeal = method(TypicalValue=>Ideal)
lexIdeal(PolynomialRing,List) := (R,hilb) -> (
     leng:=#hilb;
     numvars:=dim R;
     hilb=append(hilb,0); --takes care of powers of maximal ideal
     --check that hilb is a valid HF
     if (hilb#1 > numvars) or not (isHF hilb) then return null;
     m:=ideal vars R;
     --took out a trim below since not clear why needed
     iterIdeal:=ideal(0_R);
     --get linear generators (because no Macaulay bound from deg 0 to 1)
    if numvars-hilb#1 > 0 then iterIdeal=ideal take(first entries gens m,{0,(numvars-hilb#1)-1});
     deg:=2;
     while deg <= leng do (
	  --compute number of monomials to add in degree deg
	  mB:=macaulayBound(hilb#(deg-1),deg-1);
	  numberToAdd:=mB-hilb#(deg);
	  if numberToAdd > 0 then (
	       --compute a basis of monomials for R/iterIdeal in degree
	       --deg. they're ordered in descending lex order, so add
	       --the first numberToAdd.
	       mons:=flatten entries basis(deg,coker gens iterIdeal);
	       iterIdeal=iterIdeal+ideal(take(mons,numberToAdd));
	       );
     	  deg=deg+1;
     );
     trim iterIdeal
     )

--given a quotient ring R, a polynomial ring modulo a homogeneous ideal, and 
--an Artinian Hilbert function hilb, returns the lex ideal R corresponding
--to hilb if one exists
lexIdeal(QuotientRing,List) := (R,hilb) -> (
	hilbertLength:=#hilb;
     	ringHF:=toList apply(0..hilbertLength-1,i->hilbertFunction(i,R));
	numMonsNeeded:=ringHF-hilb;
	d:=1;
	I:=ideal(0_R);
	m:=ideal vars R;
	while d < hilbertLength do (
	        toAdd:=(matrix{unique flatten entries compress gens m^d})_{0..(numMonsNeeded#d)-1};
		if #toAdd != 0 then I=I+ideal(toAdd);
		d=d+1;
	);
	I=trim(I+m^d);
	if (hilbertFunct(I,MaxDegree=>hilbertLength-1)) == hilb then I else null
)

--create the lex ideal with the same HF as I
--works for Artinian and non-Artinian
--assumes Gotzmann in quotient rings
lexIdeal(Ideal) := (I) -> (
    R:=ring I;
    if I == ideal(0_R) then return I;
    if dim I == 0 then return lexIdeal(R,hilbertFunct I) else (
         highGen:=max flatten degrees module ideal leadTerm I;
         deg:=highGen;
         done:=false;
         while done == false do (
            hilb:=hilbertFunct(I,MaxDegree=>deg+2);
            artinian:=lexIdeal(R,drop(hilb,{deg+2,deg+2}));
	    if not isIdeal artinian then return null;
            hfArtinian:=hilbertFunct(artinian,MaxDegree=>deg+1);
            lex:=ideal select(flatten entries gens artinian,i->first degree i <= deg);
            if (hilbertFunct(lex,MaxDegree=>deg+1))#(deg+1) == hfArtinian#(deg+1) then done=true;
            deg=deg+10;
            );
         return lex;
         );
    )

--checks whether an ideal in a polynomial ring or quotient of a
--polynomial ring by a graded ideal is a lex ideal IN THAT RING.
--that is, the ideal may be a lex ideal in the quotient but not 
--in the polynomial ring itself
isLexIdeal = method(TypicalValue=>Boolean)
isLexIdeal(Ideal) := (I) -> (
     R:=ring I;
     genDegs:=flatten degrees I;
     minDeg:=min genDegs;
     maxDeg:=max genDegs;
     deg:=minDeg;
     result:=true;
     while deg <= maxDeg and result==true do (
	  m:=ideal vars R;
	  basisI:=flatten entries super basis(deg,I);
	  hf:=#basisI;
     	  basisLex:=take(unique flatten entries compress gens m^deg,hf);
	  result=(ideal basisI==ideal basisLex);
	  deg=deg+1;
	  );
     result
     )

--determines whether a monomial in a polynomial ring is 
--a pure power of a variable
isPurePower = method(TypicalValue=>Boolean)
isPurePower(RingElement) := (mon) -> (
     number(flatten exponents mon,i->i!=0)==1
--     number(flatten exponents lift(mon,ambient ring mon),i->i!=0)==1
     )

--determine whether a list of numbers is nondecreasing
isNonDec = method(TypicalValue=>Boolean)
isNonDec(List) := (li) -> (
     len:=#li;
     pos:=0;
     result:=true;
     while result==true and pos < len-1 do (
  	  result=li#(pos+1)>=li#pos;
  	  pos=pos+1;
	  );
     result
)

--compute the LPP ideal in R with HF hilb and power sequence powers
--returns null if no such LPP ideal exists
LPP = method(TypicalValue=>Ideal)
LPP(PolynomialRing,List,List) := (R,hilb,powers) -> (
     numvars:=dim R;
     m:=ideal vars R;
     if #powers != numvars then return null;
     if not isNonDec(powers) then return null;
     if not isHF(hilb) then return null;     
     gen:=toList apply(0..(numvars-1),i->(R_i)^(powers#i));
     lppIdeal:=ideal gen;
     hilbGens:=hilbertFunct lppIdeal;
     hilbSameLength:=makeSameLength(hilbGens,hilb);
     toAdd:=hilbSameLength#0-hilbSameLength#1; --number of mons we need to add
     deg:=1;                                   --in each degree
     --continue in while loop while the toList is not all 0's and has all
     --nonnegative entries
     while toAdd != toList (#toAdd:0) and toAdd >= toList (#toAdd:0) do (
	  if toAdd#deg == 0 then deg=deg+1 else (
	       mons:=flatten entries basis(deg,coker gens lppIdeal);
	       --select the monomials not already in lppIdeal
	       addMons:=take(mons,toAdd#deg);
	       if not all(addMons,i->(not isPurePower i)) then return null;
	       if #addMons != toAdd#deg then return null;
	       gen=join(gen,addMons);
	       lppIdeal=ideal gen;
	       hilbGens=hilbertFunct(ideal gen);
     	       hilbSameLength=makeSameLength(hilbGens,hilb);
     	       toAdd=hilbSameLength#0-hilbSameLength#1;
	       ); 
     );
     if (toAdd != toList (#toAdd:0)) then return null
     else lppIdeal
)

--generates all LPP ideals in a polynomial ring with a given HF
--outputs {{powers,ideal}}
--if option PrintIdeals=>true, prints {list of powers} LPP ideal
--default is not to print
generateLPPs = method(TypicalValue=>List, Options => {PrintIdeals => false})
generateLPPs(PolynomialRing,List) := opts -> (ri,hilb) -> (
     numvars:=dim ri;
     toppower:=#hilb;
     --extend the HF by one place to deal with powers of (x_1,...,x_n) 
     hilbext:=append(hilb,0);
     hilbbound:=apply((#hilb)+1,i->binomial(numvars+i-1,i));
     firstdeg:=position(hilbext-hilbbound,i->(i!=0)); --lowest deg of a gen.
     --get all the power sequences one has to check
     powerlist:=prepend(firstdeg,toList(numvars-1:firstdeg));
     upperbound:=prepend(firstdeg,toList(numvars-1:#hilbext));
     powerstocheck:=select(powerlist..upperbound,i->isNonDec(i));
     --now loop to check all the power sequences, using LPPTest
     num:=#powerstocheck;
     j:=0;
     output:={};
     while j < num-1 do (
  	  getOutput:=LPP(ri,hilb,powerstocheck#j);
  	  if not (getOutput===null) then (
	       if opts.PrintIdeals == true then (
    	       	    << powerstocheck#j << " " << getOutput << endl;
		    );
	       output=append(output,{powerstocheck#j,getOutput});
  	       );
  	  j=j+1;
	  );
     output
)

--tests whether an ideal I in a polynomial ring is an LPP ideal
isLPP = method(TypicalValue=>Boolean)
isLPP(Ideal) := (I) -> (
     ri:=ring I;
     numvars:=dim ri;
     m:=ideal vars ri;
     Igens:=first entries gens trim I;
     --check that the power sequence has right length and is nondecreasing
     powers:=select(Igens,i->isPurePower i);
     if #powers != numvars then return false;
     expon:=apply(sort apply(powers,i->purePowerIndex i),i->i#1);
     if not (isNonDec expon) then return false;
     --nonPowers are not a power of a variable
     nonPowers:=select(Igens,i->not isPurePower i);
     if nonPowers=={} then return true; --if a CI with nondec. powers
     maxDeg:=first flatten max(apply(nonPowers,i->flatten degree i));
     deg:=1;
     while deg <= maxDeg do (
	  --get the minimal generators in degree deg and find the smallest
	  --in descending lex order that isn't a pure power
	  nonPowersDeg:=select(nonPowers,i->(first flatten degree i)==deg);
	  ringBasis:=flatten entries basis(deg,ri);
	  lastPos:=position(reverse ringBasis,i->member(i,nonPowersDeg));
     	  if lastPos===null then deg=deg+1 else (
	       normalPos:=binomial(numvars-1+deg,deg)-1-lastPos;
	       lastGen:=ringBasis#normalPos;
	       --check that all mons > than lastGen in desc. lex order
	       --are in I
	       if not all(0..normalPos,i->((ringBasis#i) % I)==0) then return false else deg=deg+1;
     	       );
	  );
     true
)

--local function for use in isLPP
--if mon is a pure power, returns {index of variable, exponent}
purePowerIndex = method(TypicalValue=>Boolean)
purePowerIndex(RingElement) := mon -> (
     expVector:=flatten exponents mon;
     if number(expVector,i->i!=0) > 1 then false else (
	  pos:=position(expVector,i->i!=0);
	  {position(expVector,i->i!=0),expVector#pos}
     )
)

-------------------------multiplicity stuff----------------------------

--takes the graded free resolution of an ideal I
--makes all potentially possible cancellations in the generator
--and first syzygy columns; then all potentially possible cancellations
--in the first and second syzygy columns, etc., moving left to right
--in the Betti diagram.
--for use in multUpperHF
cancelAll = method(TypicalValue=>List)
cancelAll(Ideal) := I -> (
     C:=res I;
     pd:=pdim coker gens I;
     deglist:={};
     i:=1;
     while i < pd do (
     	  --get the shifts in two consecutive spots in the resolution
	  if i==1 then F:=(flatten degrees C_i);
	  G:=flatten degrees C_(i+1);
	  j:=0;
	  --cancel everything possible, left to right
	  while j < #F do (
	       if member(F#j,G) then (
		    pos:=position(G,l->(l==(F#j)));
		    F=drop(F,{j,j});
		    G=drop(G,{pos,pos});
		   )
     	       else j=j+1;
	  );
	  deglist=append(deglist,F);
     	  F=G;
	  i=i+1;  
     );
     deglist=append(deglist,F);
     << endl; << cancelAllDisplay deglist << endl;
     deglist
     )

--local function for displaying the result of cancelAll in Betti
--diagram form
cancelAllDisplay = method()
cancelAllDisplay(List) := (li) -> (
     numberLists:=#li;
     reg:=(max last li)-numberLists;
     pairsList:=apply(apply(prepend({0},li),i->tally i),w->pairs w);
     listNum:=0;
     newPairsList:={};
     while listNum <= numberLists do (
	  newPairsList=append(newPairsList,apply(pairsList#listNum,i->append(i,listNum)));
	  listNum=listNum+1;
	  );
     bettiDisplay hashTable apply(flatten newPairsList,(j,k,l)->(j,l)=>k)
     )

--local function, taken from chaincomplexes.m2
bettiDisplay = method()
bettiDisplay(HashTable) := v -> (
     -- convert the hash table created by rawbetti to the standard display
     v = applyKeys( v, (d,i) -> (d-i,i) );		    -- skew the degrees
     k := keys v;
     fi := first \ k;
     la := last  \ k;
     mincol := min la;
     maxcol := max la;
     minrow := min fi;
     maxrow := max fi;
     v = table(toList (minrow .. maxrow), toList (mincol .. maxcol),
	  (i,j) -> if v#?(i,j) then v#(i,j) else 0);
     leftside := apply(
	  splice {"total:", apply(minrow .. maxrow, i -> toString i | ":")},
	  s -> (6-# s,s));
     totals := apply(transpose v, sum);
     v = prepend(totals,v);
     v = transpose v;
     v = applyTable(v, bt -> if bt === 0 then "." else toString bt);
     v = apply(v, col -> (
	       wid := 1 + max apply(col, i -> #i);
	       apply(col, s -> (wid-#s, s))));
     v = prepend(leftside,v);
     v = transpose v;
     stack apply(v, concatenate))


--tests sufficient condition for all Cohen-Macaulay modules R/I with HF hilb 
--to satisfy the upper bound of the multiplicity conjecture of 
--Huneke-Srinivasan, Herzog-Srinivasan.
--false doesn't mean a counterexample but just that the sufficient condition
--doesn't hold.
--method from C. Francisco's thesis
multUpperHF = method(TypicalValue=>Boolean)
multUpperHF(PolynomialRing,List) := (R,hilb) -> (
     L:=lexIdeal(R,hilb);
     mult:=degree L;
     c:=codim L;
     shifts:=cancelAll L;
     maxShifts:=apply(shifts,i->max i);
     upperBound:=(product maxShifts)/(c!);
     << endl; << "degree = " << mult << "  upper bound = " << upperBound << endl;
     mult <= upperBound
     )

--does I satisfy the upper bound of the multiplicity conjecture
--of Huneke-Srinivasan, Herzog-Srinivasan?
--uses stronger upper bound of using codimension rather than pdim
--in non-Cohen-Macaulay case
multUpperBound = method(TypicalValue=>Boolean)
multUpperBound(Ideal) := I -> (
     mult:=degree I;
     c:=codim I;
     resol:=res I;
     maxShifts:=apply(1..c,i->max flatten degrees resol_i);
     upperBound:=(product toList maxShifts)/(c!);
     << endl; << "degree = " << mult << "  upper bound = " << upperBound << endl;
     mult <= upperBound
     )


--let R/I be C-M. does I satisfy the lower bound of the multiplicity conjecture
--of Huneke-Srinivasan, Herzog-Srinivasan?
multLowerBound = method(TypicalValue=>Boolean)
multLowerBound(Ideal) := I -> (
     mult:=degree I;
     c:=codim I;
     resol:=res I;
     minShifts:=apply(1..c,i->min flatten degrees resol_i);
     lowerBound:=(product toList minShifts)/(c!);
     << endl; << "lower bound = " << lowerBound << " degree = " << mult << endl;
     lowerBound <= mult
     )


--let R/I be C-M. does I satisfy the bounds of the multiplicity conjecture
--of Huneke-Srinivasan, Herzog-Srinivasan?
multBounds = method(TypicalValue=>Boolean)
multBounds(Ideal) := I -> (
     mult:=degree I;
     c:=codim I;
     resol:=res I;
     minShifts:=apply(1..c,i->min flatten degrees resol_i);     
     maxShifts:=apply(1..c,i->max flatten degrees resol_i);
     lowerBound:=(product toList minShifts)/(c!);
     upperBound:=(product toList maxShifts)/(c!);
     << endl; << "lower bound = " << lowerBound << " degree = " << mult << " upper bound = " << upperBound << endl;
     lowerBound <= mult and mult <= upperBound
     )

beginDocumentation()

------------------------------------------------------------------------
--
--DOCUMENTATION
--
------------------------------------------------------------------------

-------------------------------------------------------
--DOCUMENTATION LexIdeals
-------------------------------------------------------
doc///
	Key
		LexIdeals
	Headline
		a package for working with lex ideals
	Description
		Text
	     		{\em LexIdeals} is a package for creating lexicographic ideals and 
	     		lex-plus-powers (LPP) ideals.  There are also several functions for
	     		use with the multiplicity conjectures of Herzog, Huneke, and Srinivasan.
///

-------------------------------------------------------
--
--DOCUMENTATION FOR FUNCTIONS
--
-------------------------------------------------------

-------------------------------------------------------
--DOCUMENTATION macaulayRep
-------------------------------------------------------

doc///
     Key
     	  macaulayRep
	  (macaulayRep,ZZ,ZZ)
     Headline
     	  the Macaulay representation of an integer
     Usage
     	  h=macaulayRep(a,d)
     Inputs
     	  a:ZZ
		a nonnegative integer
	  d:ZZ
		a positive integer
     Outputs
     	  L:List
		a list of pairs {i,j} of integers such that j <= d for all terms, and a is the sum of the {\tt binomial(i,j)}.
     Description
     	  Text
		Yields the {\tt d}-th Macaulay representation of the integer {\tt a}. Given a positive integer {\tt d}, 
		each positive integer a can be uniquely represented as a sum of binomials 
		{\tt binomial(b_d,d) + binomial(b_{d-1},d-1) + ... + binomial(b_1,1)}, where {\tt b_d > b_{d-1} > ... > b_1 >= 0}.
	  Example
	       macaulayRep(100,4)
	       macaulayRep(10,5)
     SeeAlso
     	  macaulayBound
	  isHF
///

-------------------------------------------------------
--DOCUMENTATION macaulayBound
-------------------------------------------------------

doc///
     Key
          macaulayBound
	  (macaulayBound,ZZ,ZZ)
     Headline
     	  the bound on the growth of a Hilbert function from Macaulay's Theorem
     Usage
     	  h=macaulayBound(a,d)
     Inputs
	  a:ZZ
     	       a positive integer
	  d:ZZ
	       a positive integer
     Outputs
     	  h:ZZ
	       the Macaulay upper bound for the Hilbert function in degree {\tt d+1} given that it is {\tt a} in degree {\tt d}.
     Description
     	  Text
	       Given a Hilbert function of {\tt a} in degree {\tt d}, {\tt macaulayBound} yields the upper bound from Macaulay's Theorem 
	       for the Hilbert function in degree {\tt d+1}. 
	  Example
	       macaulayBound(3,1)
	       macaulayBound(15,5)
     SeeAlso
     	  macaulayRep
	  macaulayLowerOperator
	  isHF
///

doc///
     Key
     	  macaulayLowerOperator
	  (macaulayLowerOperator,ZZ,ZZ)
     Headline
     	  the a_<d> operator used in Green's proof of Macaulay's Theorem
     Usage
     	  h=macaulayLowerOperator(a,d)
     Inputs
     	  a:ZZ
	       a positive integer
	  d:ZZ
	       a positive integer
     Outputs
     	  h:ZZ
	       a nonnegative integer representing {\tt a_<d>}
     Description
     	  Text
	       Given positive integers {\tt a} and {\tt d}, {\tt macaulayLowerOperator} yields {\tt a_<d>}, the operation from 
	       Green's proof of Macaulay's Theorem. See Bruns and Herzog, {\em Cohen-Macaulay Rings}, page 161.
	  Example
	       macaulayLowerOperator(3,1)
	       macaulayLowerOperator(15,5)
     SeeAlso
     	  macaulayRep
	  macaulayBound
///

doc///
     Key
     	  isHF
	  (isHF,List)
     Headline
     	  is a finite list a Hilbert function of a polynomial ring mod a homogeneous ideal
     Usage
     	  b=isHF L
     Inputs
     	  L:List
	       a finite list of integers
     Outputs
     	  b:Boolean
	       {\tt true} if {\tt L} is a Hilbert function of a polynomial ring modulo a homogeneous ideal and {\tt false} otherwise
     Description
     	  Text
	       Macaulay's Theorem characterizes the sequences of integers that occur as the Hilbert function of a polynomial 
	       ring modulo a homogeneous ideal. {\tt isHF} checks that the input is a list of integers and that the 
	       first entry of the list is 1, and then it checks Macaulay's bound in each degree, using @TO macaulayBound@. The 
	       function returns {\tt true} if the sequence of numbers in the list satisfies the conditions of Macaulay's Theorem 
	       and {\tt false} otherwise.
     	  Example
	       isHF({1,3,6,7,5,3})
	       isHF({2,3,4,3,2}) --doesn't start with a 1 in degree 0
	       isHF({1,3,6,8,14,3}) --growth from 8 to 14 is too high
     SeeAlso
     	  macaulayRep
	  macaulayBound
///

doc///
     Key
     	  hilbertFunct
	  (hilbertFunct,Ideal)
     Headline
     	  return the Hilbert function of a polynomial ring mod a homogeneous ideal as a list
     Usage
     	  L=hilbertFunct I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring or a quotient of a polynomial ring (where the ring has the standard grading)
     Outputs
     	  L:List
	       returns the Hilbert function of {\tt (ring I)/I} as a list
     Description
     	  Text
	       Let {\tt I} be a homogeneous ideal in a ring {\tt R} that is either a polynomial ring or a quotient 
	       of a polynomial ring, and suppose that {\tt R} has the standard grading. {\tt hilbertFunct} returns 
	       the Hilbert function of {\tt R/I} as a list.
	       
	       If {\tt R/I} is Artinian, then the default is for {\tt hilbertFunct} to return the entire Hilbert 
	       function (i.e., until the Hilbert function is zero) of {\tt R/I} as a list. The user can override 
	       this by using the {\tt MaxDegree} option to bound the highest degree considered.
	       
	       If {\tt R/I} is not Artinian, then {\tt hilbertFunct} returns the Hilbert function of {\tt R/I} 
	       through degree 20. Again, the user can select a different upper bound for the degree by using the 
	       {\tt MaxDegree} option.
	       
	       We require the standard grading on {\tt R} in order to compute with the Hilbert series, which is presently 
	       much faster than repeatedly computing the Hilbert function.
     	  Example
	       R=ZZ/32003[a..c];
	       hilbertFunct ideal(a^3,b^3,c^3)
	       hilbertFunct ideal(a^3,a*b^2)
	       hilbertFunct(ideal(a^3,a*b^2),MaxDegree=>4)
	       M=ideal(a^3,b^4,a*c);
	       Q=R/M;
	       hilbertFunct ideal(c^4)
	       hilbertFunct ideal(b*c,a*b)
     SeeAlso
     	  isHF
	  lexIdeal
///

doc///
     Key
     	  isCM
	  (isCM,Ideal)
     Headline
     	  test whether a polynomial ring modulo a homogeneous ideal is Cohen-Macaulay
     Usage
     	  B=isCM I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring
     Outputs
     	  B:Boolean
	       {\tt true} if {\tt (ring I)/I} is Cohen-Macaulay and {\tt false} otherwise
     Description
     	  Text
	       {\tt isCM} takes a homogeneous ideal {\tt I} in a polynomial ring {\tt R} and, by computing the 
	       projective dimension and codimension of {\tt I}, determines whether {\tt R/I} is Cohen-Macaulay. 
	       Of course, {\tt isCM} works only if Macaulay 2 can compute the projective dimension of {\tt I}.
	  Example
	       R=ZZ/32003[a..c];
	       isCM(ideal(a^2,b^4)) --complete intersection
	       isCM(ideal(a^3,b^5,c^4,a*c^3)) --Artinian
	       isCM(ideal(a^3,a*b^2))
///

doc/// 
     Key
     	  lexIdeal
	  (lexIdeal,PolynomialRing,List)
	  (lexIdeal,QuotientRing,List)
	  (lexIdeal,Ideal)
     Headline
     	  produce a lexicographic ideal
     Usage
     	  L=lexIdeal(R,hilb) or L=lexIdeal(Q,hilb) or L=lexIdeal(I)
     Inputs
	  R:PolynomialRing
	  Q:QuotientRing
	       a polynomial ring modulo a homogeneous ideal
	  hilb:List
	       a finite length list of positive integers
	  I:Ideal
	       a homogeneous ideal
     Outputs
     	  L:Ideal
	       the Artinian lexicographic ideal {\tt L} in {\tt R} (resp. {\tt Q}) such that {\tt R/L} (resp. {\tt Q/L}) has 
	       Hilbert function {\tt hilb} OR the lexicographic ideal (possibly not Artinian) {\tt L} in {\tt R} or {\tt Q} 
	       with the same Hilbert function as {\tt I}
     Description
     	  Text
	       When {\tt R} is a polynomial ring, if {\tt hilb} is an O-sequence (that is, it satisfies Macaulay's Theorem), 
	       such an {\tt L} always exists. When {\tt Q} is a quotient of a polynomial ring, there may be no lexicographic 
	       ideal with a particular Hilbert function even if it is an O-sequence. {\tt lexIdeal} returns {\tt null} if no 
	       lexicographic ideal {\tt L} corresponding to the Hilbert function {\tt hilb} exists in {\tt R} or {\tt Q}.
	       
	       When {\tt lexIdeal} has an ideal as its input, it returns the lexicographic ideal with the same Hilbert function 
	       as its output. If no such ideal exists, which may happen since Macaulay's Theorem fails in some quotient rings, 
	       then {\tt lexIdeal} returns {\tt null}.

	       The function now works for quotients by arbitrary homogeneous ideals, not just monomial ideals.
	       We thank David Eisenbud and Jeff Mermin for contributing their ideas.
     	  Example
	       R=ZZ/32003[a..c];
	       lexIdeal(R,{1,3,4,3,1})
	       lexIdeal ideal(a*b,b*c)
	       lexIdeal(R,{1,3,7}) --not an O-sequence, so no lex ideal exists
	       Q=R/ideal(a^3,b^3,a*c^2);
	       lexIdeal(Q,{1,3,6,4,2})
	       lexIdeal(Q,{1,3,6,4,4}) --value of 4 in degree 4 is too high in this ring
     Caveat
     	  Note that we use the Gotzmann Persistence Theorem as a stopping criterion, so one should make sure this holds in the 
	  ring in which one is computing.
     SeeAlso 
     	  macaulayRep
	  macaulayBound
	  isHF
	  isLexIdeal
///

doc/// 
     Key
     	  isLexIdeal
	  (isLexIdeal,Ideal)
     Headline
     	  determine whether an ideal is a lexicographic ideal
     Usage
     	  B=isLexIdeal I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring or quotient of a polynomial ring by a homogeneous ideal
     Outputs
     	  B:Boolean
	       {\tt true} if {\tt I} is a lexicographic ideal in {\tt ring I} and {\tt false} otherwise
     Description
     	  Text
	       Given an ideal {\tt I} in a ring {\tt R} that is either a polynomial ring or a quotient of a polynomial 
	       ring by a monomial ideal, {\tt isLexIdeal} computes bases of {\tt I} in each degree up through the 
	       maximum degree of a minimal generator of {\tt I} to determine whether {\tt I} is a lexicographic ideal in {\tt R}.
     	  Example
	       R=ZZ/32003[a..c];
	       isLexIdeal lexIdeal(R,{1,3,4,3,1})
	       isLexIdeal ideal(a^3-a^2*b)
	       isLexIdeal ideal(a^3,a^2*b)
	       isLexIdeal ideal(a^3,a^2*b,a^3-a^2*b) --not given as a monomial ideal but still a lex ideal
	       Q=R/ideal(a^3,b^3,a*c^2);
	       isLexIdeal ideal(a^2*b,a^2*c)
	       isLexIdeal ideal(a^2*b,a*b^2)
     SeeAlso 
     	  lexIdeal
///

doc///
     Key
     	  isPurePower
	  (isPurePower,RingElement)
     Headline
     	  determine whether a ring element is a pure power of a variable
     Usage
     	  B=isPurePower f
     Inputs
     	  f:RingElement
	       an element of a polynomial ring
     Outputs
     	  B:Boolean
	       {\tt true} if {\tt f} is a nonzero power of a variable and {\tt false} otherwise
     Description
     	  Text
	       {\tt isPurePower} tests a ring element in a polynomial ring to determine whether or not 
	       it is nonzero and a power of a variable. {\tt isPurePower} is used in the lex-plus-powers @TO LPP@ code.
	  Example
	       R=ZZ/32003[a..c];
	       isPurePower a^4
	       isPurePower (a*b^5)
	       isPurePower (a^3-b^3)
     SeeAlso
     	  LPP
	  isLPP
///

doc///
     Key
     	  LPP
	  (LPP,PolynomialRing,List,List)
     Headline 
     	  return the lex-plus-powers (LPP) ideal corresponding to a given Hilbert function and power sequence
     Usage
     	  L=LPP(R,hilb,power)
     Inputs
     	  R:PolynomialRing
	  hilb:List
	       a Hilbert function as a list
	  power:List
	       a list of positive integers in weakly increasing order
     Outputs
     	  L:Ideal
	       an LPP ideal with the desired Hilbert function and power sequence if one exists and {\tt null} otherwise
     Description
     	  Text
	       Let {\tt a_1 <= ... <= a_n} be positive integers. A monomial ideal {\tt L} in a polynomial ring {\tt R=k[x_1,...,x_n]} 
	       is called an {\tt (a_1,...a_n)}-lex-plus-powers (LPP) ideal if it satisfies two conditions:
     	       
	       (1) L is minimally generated by {\tt x_1^{a_1}, ..., x_n^{a_n}} and monomials {\tt m_1, ..., m_t}.
	       (2) Suppose that r is a monomial such that {\tt deg r = deg m_i}, and {\tt r > m_i} in the lex order. Then 
	       {\tt r} is in {\tt L}.
	       
	       LPP ideals are generalizations of Artinian lexicographic ideals. Condition (2) represents the lex portion of 
	       the LPP ideal; monomials that are not powers of a variable must satisfy a lexicographic condition similar to 
	       what generators of lex ideals satisfy. An LPP ideal also contains powers of all the variables in weakly 
	       increasing order.

     	       LPP ideals arise in conjectures of Eisenbud-Green-Harris and Charalambous-Evans in algebraic geometry, 
	       Hilbert functions, and graded free resolutions. They are conjectured to play a role analogous to that of 
	       lexicographic ideals in theorems of Macaulay on Hilbert functions and Bigatti, Hulett, and Pardue on resolutions.
	  Example
	       R=ZZ/32003[a..c];
	       LPP(R,{1,3,6,5,3},{3,3,4})
	       LPP(R,{1,3,4,2,1},{2,3,5}) --an Artinian lex ideal
	       LPP(R,{1,3,4,2,1},{2,4,3}) --exponents not in weakly increasing order
	       LPP(R,{1,3,4,2,1},{2,2,3}) --no LPP ideal with this Hilbert function and power sequence
     SeeAlso
     	  generateLPPs
	  isLPP
///

doc///
     Key
     	  generateLPPs
	  (generateLPPs,PolynomialRing,List)
     Headline
     	  return all LPP ideals corresponding to a given Hilbert function
     Usage
     	  li=generateLPPs(R,hilb)
     Inputs
     	  R:PolynomialRing
	  hilb:List
	       a Hilbert function as a list
     Outputs
     	  li:List
	       a list of the form {{powers, LPP ideal}, {powers, LPP ideal}, ...}
     Description
     	  Text
	       Given a polynomial ring {\tt R} and a Hilbert function {\tt hilb} for {\tt R} modulo a homogeneous 
	       ideal, {\tt generateLPPs} generates all the LPP ideals corresponding to {\tt hilb}. The power sequences 
	       and ideals are returned in a list. If the user sets the {\tt PrintIdeals} option to {\tt true}, the power sequences 
	       and ideals are printed on the screen in a nice format.
	  Example
	       R=ZZ/32003[a..c];
     	       generateLPPs(R,{1,3,4,3,2})
     	  Text
	       Same example with the {\tt PrintIdeals} option set to {\tt true}:
     	  Example
	       generateLPPs(R,{1,3,4,3,2},PrintIdeals=>true)
     SeeAlso
     	  LPP
	  isLPP
///

doc///
     Key 
     	  isLPP
	  (isLPP,Ideal)
     Headline 
     	  determine whether an ideal is an LPP ideal
     Usage 
     	  B=isLPP I
     Inputs
     	  I:Ideal
	       an ideal in a polynomial ring
     Outputs
     	  B:Boolean
	       {\tt true} if {\tt I} is an LPP ideal in {\tt ring I} and {\tt false} otherwise
     Description
     	  Text
	       Given an ideal {\tt I} in a polynomial ring {\tt R}, {\tt isLPP} checks that {\tt I} is Artinian and that the power 
	       sequence is weakly increasing. Then {\tt isLPP} computes bases of {\tt R/I} in each degree up through the maximum 
	       degree of a minimal generator of {\tt I} to determine whether {\tt I} is an LPP ideal in {\tt R}.
     	  Example
	       R=ZZ/32003[a..c];
	       isLPP LPP(R,{1,3,4,3,2},{2,2,4})
	       isLPP ideal(a^3,b^3,c^3,a^2*b,a^2*c,a*b^2*c^2)
	       isLPP ideal(a^3,b^4) --not Artinian since no power of c
	       isLPP ideal(a^3,b^4,c^3) --powers not weakly increasing
	       isLPP ideal(a^3,b^3,c^3,a^2*b,a*b^2)
     SeeAlso 
     	  isLexIdeal
	  LPP
	  generateLPPs
///

doc/// 
     Key
     	  cancelAll
	  (cancelAll,Ideal)
     Headline
     	  make all potentially possible cancellations in the graded free resolution of an ideal
     Usage
     	  l=cancelAll I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring
     Outputs
     	  l:List
	       lists of the new shifts in the Betti diagram
     Description
     	  Text
	       This function was useful in testing the upper bound of the conjecture of Herzog-Huneke-Srinivasan on 
	       the multiplicity of an ideal; it is designed to make all potentially possible cancellations in a Betti 
	       diagram in a certain order described below.

     	       Let {\tt R} be a polynomial ring, let {\tt I} and {\tt J} be homogeneous ideals in {\tt R} of codimension 
	       {\tt c} with the same Hilbert function, and assume that {\tt R/I} and {\tt R/J} are Cohen-Macaulay. Suppose 
	       that the graded Betti numbers of {\tt R/I} are at most those of {\tt R/J}; that is, 
	       {\tt beta_{ij}(R/I) <= beta_{ij}(R/J)} for all {\tt i} and {\tt j}. (We can form a partial order on resolutions 
	       of modules with the same Hilbert function by saying that {\tt beta(R/I) <= beta(R/J)} if and only if 
	       {\tt beta_{ij}(R/I) <= beta_{ij}(R/J)} for all {\tt i} and {\tt j}.)
	  
	       Let {\tt m_i(R/I)} and {\tt M_i(R/I)} be the minimum and maximum shifts at step {\tt i} in the minimal 
	       graded free resolution of {\tt R/I}. Let {\tt e(R/I)} be the multiplicity of {\tt R/I}. Herzog, Huneke, and 
	       Srinivasan conjectured that
	       
	       {\tt m_1(R/I) ... m_c(R/I) / c! <= e(R/I) <= M_1(R/I) ... M_c(R/I) / c!},
	  
	       where the ellipses signify that we are taking the product of the shifts. This conjecture was proven in 2008 work 
	       of Eisenbud-Schreyer and Boij-Soderberg using decompositions of Betti diagrams as positive linear combinations of 
	       Betti diagrams of modules with pure resolutions.

     	       Suppose that {\tt R/I} satisfies the conjectured inequalities. Then it is easy to show that {\tt R/J} must as 
	       well: Since the resolution of {\tt R/J} contains all the shifts in the resolution of {\tt R/I} plus possibly more, 
	       the minimum shifts for {\tt R/J} can only be the same or lower, and the maximum shifts can only be the same or higher. 
	       Therefore one can attack this conjecture by looking at resolutions minimal in the partial order discussed above.
	       
	       It is difficult to find which potential resolutions at the bottom of the partial order for a given Hilbert function 
	       do occur for a module, however. The function {\tt cancelAll} implements a fast way of proving that the upper bound of 
	       the Herzog-Huneke-Srinivasan conjecture is satisfied for particular Hilbert functions; it is used in 
	       @TO multUpperHF@. The idea is to create a Betti diagram that is minimal in the partial order that may or may not 
	       actually occur for a module; we can then test the upper bound on that potential resolution. As we explain below, 
	       if the inequality holds, it doesn't matter whether the Betti diagram occurs for a module or not.

     	       {\tt cancelAll} starts with the far left side of the Betti diagram of the ideal I the user inputs, and it makes 
	       all potentially possible cancellations in the first two columns. Then it moves to the first and second syzygies 
	       columns and makes all potentially possible cancellations, continuing through the rest of the Betti diagram. This 
	       technique is designed to investigate the upper bound of the conjecture. One may have choices of which syzygies to 
	       cancel from the resolution of the lexicographic ideal. This algorithm selects the cancellations that, if we start out 
	       with a lexicographic ideal {\tt L}, and {\tt R/L} is Cohen-Macaulay, minimize each {\tt M_i} that could occur for a 
	       Cohen-Macaulay module with the same Hilbert function as {\tt R/L}. Therefore if the upper bound of the conjecture holds 
	       for the potential Betti diagram that {\tt cancelAll} produces, it holds for all Cohen-Macaulay modules with the same Hilbert 
	       function and codimension. Even if the potential Betti diagram {\tt cancelAll} produces doesn't occur for a module, the maximum 
	       shifts could only increase, which won't cause the inequality to fail if it holds for the result of {\tt cancelAll}.
	       
	       {\tt cancelAll} makes all the cancellations with the process described above, prints the resulting Betti diagram, and 
	       then returns the shifts in a list of lists.
	       
	       See C. Francisco, New approaches to bounding the multiplicity of an ideal, {\em J. Algebra} {\bf 299} (2006), no. 1, 309-328.
	  Example
	       R=ZZ/32003[a..c];
	       L=lexIdeal(R,{1,3,6,9,9,6,2});
	       betti res L
	       cancelAll L
	       M=lexIdeal(R,{1,3,4,3,2});
	       betti res M
	       cancelAll M
	       A=ZZ/32003[a..e];
	       J=lexIdeal(A,{1,5,12,10,6,3})
	       betti res J
	       cancelAll J
     SeeAlso
     	  multUpperHF
	  multUpperBound
	  multLowerBound
	  multBounds
///

doc/// 
     Key
     	  multUpperHF
	  (multUpperHF,PolynomialRing,List)
     Headline 
     	  test a sufficient condition for the upper bound of the multiplicity conjecture
     Usage
     	  B=multUpperHF(R,hilb)
     Inputs
     	  R:PolynomialRing
	       the polynomial ring in which one wants to work
	  hilb:List
	       a Hilbert function for {\tt R} modulo a homogeneous ideal
     Outputs
     	  B:Boolean
	       {\tt true} if the sufficient condition holds and {\tt false} otherwise
     Description
     	  Text
	       This function uses @TO cancelAll@ to test a sufficient condition for the upper bound of the 
	       Herzog-Huneke-Srinivasan conjecture to hold for all quotients of polynomial rings with Hilbert function 
	       {\tt hilb}. The conjecture, proven in 2008 work of Eisenbud-Schreyer and Boij-Soderberg, asserts that if 
	       {\tt R} is a polynomial ring, and {\tt I} is a homogeneous ideal in {\tt R}, then 
	       
	       {\tt e(R/I) <= M_1 ... M_c / c!},
	       
	       where {\tt M_i} is the largest shift at step {\tt i} of the minimal graded free resolution of {\tt R/I}, 
	       {\tt c} is the codimension of {\tt I}, and {\tt e(R/I)} is the multiplicity of {\tt R/I}.
	       
	       {\tt multUpperHF} forms the lex ideal corresponding to the Hilbert function the user inputs. Then it calls 
	       @TO cancelAll@ to make all potentially possible cancellations in the resolution of the lex ideal (as described 
	       in the @TO cancelAll@ documentation). @TO cancelAll@ prints the diagram with the cancellations. In addition, 
	       {\tt multUpperHF} compares the multiplicity associated to the given Hilbert function to the conjectured upper 
	       bound from the Betti diagram with the cancellations. If the conjectured inequality holds, the function returns 
	       {\tt true}, and the conjectured upper bound is true for all modules {\tt R/I} with the given Hilbert function. 
	       If the inequality fails, then the function returns {\tt false}, and the sufficient condition does not hold. 
	       (However, this of course does not mean that the conjecture is false for the given Hilbert function; it may be 
	       that the Betti diagram with the cancellations cannot exist.) Note that since {\tt hilb} is a finite list, all 
	       modules with that Hilbert function are Artinian and hence Cohen-Macaulay.

     	       See C. Francisco, New approaches to bounding the multiplicity of an ideal, {\em J. Algebra} {\bf 299} (2006), no. 
	       1, 309-328.
	  Example
	       S=ZZ/32003[a..c];
	       betti res lexIdeal(S,{1,3,4,2,1}) --just to see the resolution of the lex ideal
	       multUpperHF(S,{1,3,4,2,1})
     SeeAlso
     	  cancelAll
	  multUpperBound
	  multLowerBound
	  multBounds
///

doc/// 
     Key
     	  multUpperBound
	  (multUpperBound,Ideal)
     Headline
     	  determine whether an ideal satisfies the upper bound of the multiplicity conjecture
     Usage
     	  B=multUpperBound I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring
     Outputs
     	  B:Boolean
	       {\tt true} if {\tt I} satisfies the upper bound and {\tt false} otherwise
     Description
     	  Text
	       Let {\tt I} be a homogeneous ideal of codimension {\tt c} in a polynomial ring {\tt R}. 
	       Huneke and Srinivasan (and later Herzog and Srinivasan in the non-Cohen-Macaulay case) 
	       conjectured that
	       
	       {\tt e(R/I) <= M_1 ... M_c / c!},
	       
	       where {\tt M_i} is the maximum shift in the minimal graded free resolution of {\tt R/I} 
	       at step {\tt i}, and {\tt e(R/I)} is the multiplicity of {\tt R/I}. {\tt multUpperBound} 
	       tests this inequality for the given ideal, returning {\tt true} if the inequality holds and 
	       {\tt false} otherwise, and it prints the upper bound and the multiplicity (listed as the degree).
	       
	       This conjecture was proven in 2008 work of Eisenbud-Schreyer and Boij-Soderberg.
	  Example
	       R=ZZ/32003[a..c];
	       multUpperBound ideal(a^4,b^4,c^4)
	       multUpperBound ideal(a^3,b^5,c^6,a^2*b,a*b*c)
     SeeAlso
     	  cancelAll
	  multUpperHF
	  multLowerBound
	  multBounds
///

doc/// 
     Key
     	  multLowerBound
	  (multLowerBound,Ideal)
     Headline
     	  determine whether an ideal satisfies the lower bound of the multiplicity conjecture
     Usage
     	  B=multLowerBound I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring {\tt R}
     Outputs 
     	  B:Boolean
	       {\tt true} if {\tt I} satisfies the lower bound and {\tt false} otherwise
     Description
     	  Text
	       Let {\tt I} be a homogeneous ideal of codimension {\tt c} in a polynomial ring {\tt R} such 
	       that {\tt R/I} is Cohen-Macaulay. Huneke and Srinivasan conjectured that 
	       
	       {\tt m_1 ... m_c / c! <= e(R/I)},

     	       where {\tt m_i} is the minimum shift in the minimal graded free resolution of {\tt R/I} at 
	       step {\tt i}, and {\tt e(R/I)} is the multiplicity of {\tt R/I}. {\tt multLowerBound} tests 
	       this inequality for the given ideal, returning {\tt true} if the inequality holds and {\tt false} 
	       otherwise, and it prints the lower bound and the multiplicity (listed as the degree).

     	       This conjecture was proven in 2008 work of Eisenbud-Schreyer and Boij-Soderberg.
     	  Example
	       R=ZZ/32003[a..c];
	       multLowerBound ideal(a^4,b^4,c^4)
	       multLowerBound ideal(a^3,b^5,c^6,a^2*b,a*b*c)
     Caveat
	  Note that {\tt multLowerBound} makes no attempt to check to see whether {\tt R/I} is Cohen-Macaulay.
     SeeAlso
     	  cancelAll
	  multUpperHF
	  multUpperBound
	  multBounds
///

doc/// 
     Key 
     	  multBounds
	  (multBounds,Ideal)
     Headline
     	  determine whether an ideal satisfies the upper and lower bounds of the multiplicity conjecture
     Usage
     	  B=multBounds I
     Inputs
     	  I:Ideal
	       a homogeneous ideal in a polynomial ring 
     Outputs
     	  B:Boolean
	       {\tt true} if both the upper and lower bounds hold and {\tt false} otherwise
     Description
     	  Text
	       Let {\tt I} be a homogeneous ideal of codimension {\tt c} in a polynomial ring {\tt R} such that {\tt R/I} is Cohen-Macaulay. 
	       Herzog, Huneke, and Srinivasan conjectured that if {\tt R/I} is Cohen-Macaulay, then 
	       
	       {\tt m_1 ... m_c / c! <= e(R/I) <= M_1 ... M_c / c!},
      
      	       where {\tt m_i} is the minimum shift in the minimal graded free resolution of {\tt R/I} at step {\tt i}, {\tt M_i} is the 
	       maximum shift in the minimal graded free resolution of {\tt R/I} at step {\tt i}, and {\tt e(R/I)} is the multiplicity of 
	       {\tt R/I}. If {\tt R/I} is not Cohen-Macaulay, the upper bound is still conjectured to hold. {\tt multBounds} tests the 
	       inequalities for the given ideal, returning {\tt true} if both inequalities hold and {\tt false} otherwise. 
	       {\tt multBounds} prints the bounds and the multiplicity (called the degree), and it calls @TO multUpperBound@ and @TO multLowerBound@.
	       
	       This conjecture was proven in 2008 work of Eisenbud-Schreyer and Boij-Soderberg.
     	  Example
	       S=ZZ/32003[a..c];
	       multBounds ideal(a^4,b^4,c^4)
	       multBounds ideal(a^3,b^4,c^5,a*b^3,b*c^2,a^2*c^3)
     Caveat	       
     	  Note that {\tt multBounds} makes no attempt to check to see whether {\tt R/I} is Cohen-Macaulay.         
     SeeAlso
     	  cancelAll
	  multUpperHF
	  multUpperBound
	  multLowerBound
///

-------------------------------------------------------
--
--DOCUMENTATION FOR OPTIONS
--
-------------------------------------------------------

------------------------------------------------------------
-- DOCUMENTATION PrintIdeals (for generateLPPs)
------------------------------------------------------------

doc///
        Key
	        PrintIdeals
	Headline
	        optional argument for generateLPPs
	Description
	     	Text
		     Tells @TO generateLPPs@ to display the LPP ideals nicely on the screen.
	SeeAlso
		generateLPPs
///

doc///
     	  Key
	       [generateLPPs,PrintIdeals]
	  Headline
	       print LPP ideals nicely on the screen
	  Usage
	       li = generateLPPs(R,hilb,PrintIdeals=>true)
	  Inputs
	       R:PolynomialRing
	       	    the ring in which to work
	       hilb:List
	       	    the values of the Hilbert function as a list
     	  Outputs
	       li:List
	       	    the LPP ideals with Hilbert function {\tt hilb}
	  Description
	       Text
	       	    The default value is {\tt false}. If {\tt PrintIdeals} is set to {\tt true}, the power sequences and
		    ideals are printed nicely on the screen to make them more readable than they are in list form.
	  SeeAlso
	       generateLPPs
///	       	       

------------------------------------------------------------
-- DOCUMENTATION MaxDegree (for hilbertFunct)
------------------------------------------------------------

doc///
     Key
     	  MaxDegree
     Headline
     	  optional argument for hilbertFunct
     Description
     	  Text
	       Used to set the highest degree through which hilbertFunct should compute the
	       Hilbert function.
     SeeAlso
     	  hilbertFunct
///

doc///
     	  Key
	       [hilbertFunct,MaxDegree]
	  Headline
	       bound degree through which Hilbert function is computed
	  Usage
	       li = hilbertFunct(I,MaxDegree=>d)
	  Inputs
	       I:Ideal
	       	    a homogeneous ideal
	       d:ZZ
	       	    the highest degree in which the Hilbert function should be computed
     	  Outputs
	       li:List
	       	    the values of the Hilbert function through degree {\tt d}
	  Description
	       Text
	       	    If {\tt I} is Artinian, the default is to return all values of the Hilbert 
		    function until it becomes zero. If not, the default maximum degree is 20. This 
		    option allows one to specify any maximum degree.
	  SeeAlso
	       hilbertFunct
///

------------------------------------------------------------
-- TESTS
------------------------------------------------------------

----------------------------
-- Test macaulayRep
----------------------------

TEST///
assert(macaulayRep(8,2)=={{4,2}, {2,1}})
assert(macaulayRep(5,6)=={{6,6},{5,5},{4,4},{3,3},{2,2}})
///

----------------------------
-- Test macaulayBound
----------------------------

TEST///
assert(macaulayBound(3,1)==6)
assert(macaulayBound(8,3)==10)
///

----------------------------
-- Test macaulayLowerOperator
----------------------------

TEST///
assert(macaulayLowerOperator(9,3)==3)
assert(macaulayLowerOperator(50,7)==10)
///

----------------------------
-- Test isHF
----------------------------

TEST///
assert(isHF {1,3,6,5,3,2} == true)
assert(isHF {0,1,3,4,2} == false)
assert(isHF {1,3,6,9,15} == false)
///

----------------------------
-- Test hilbertFunct
----------------------------

TEST///
R=ZZ/101[a..c];
assert(hilbertFunct ideal(a^3,b^3,c^3) == {1,3,6,7,6,3,1})
assert(hilbertFunct ideal(a^3,a*b^2) ==  {1, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25})
assert(hilbertFunct(ideal(a^3,a*b^2),MaxDegree=>4)=={1,3,6,8,9})
M=ideal(a^3,b^3,a*c);
Q=R/M;
assert(hilbertFunct ideal(c^4)=={1,3,5,5,3,1})
///


----------------------------
-- Test isCM
----------------------------

TEST///
R=ZZ/101[a..c];
assert(isCM ideal(a^2,a*b)==false)
assert(isCM ideal(a^2,b^2)==true)
assert(isCM ideal(a^2,b^3,c^4,b^2*c^2) == true)
///

----------------------------
-- Test lexIdeal
----------------------------

TEST///
R=ZZ/32003[a..c];
assert(lexIdeal(R,{1,3,4,3,1})==ideal(a*b,a^2,a*c^2,b^3,b*c^3,b^2*c^2,c^5))
assert(lexIdeal ideal(a*b,b*c)==ideal(a^2,a*b))
assert(lexIdeal(R,{1,3,7})==null)
Q=R/ideal(a^3,b^3,a*c^2);
assert(lexIdeal(Q,{1,3,6,4,2})==ideal(a^2*c,a*b^2,a^2*b,b^2*c^2,c^5,b*c^4))
assert(lexIdeal(Q,{1,3,6,4,4})==null)
T=ZZ/101[a..c]/ideal(a^2-b*c);
assert(lexIdeal ideal(a*b,b^2,a*c,b*c,c^2)==ideal(a*b,b^2,a*c,b*c,c^2))
assert(lexIdeal ideal(a*b)==ideal(a^2))
///

----------------------------
-- Test isLexIdeal
----------------------------

TEST///
R=ZZ/32003[a..c];
assert(isLexIdeal lexIdeal(R,{1,3,4,3,1})==true) 
assert(isLexIdeal ideal(a^3-a^2*b) == false)
assert(isLexIdeal ideal(a^3,a^2*b)==true)
assert(isLexIdeal ideal(a^3,a^2*b,a^3-a^2*b)==true)
Q=R/ideal(a^3,b^3,a*c^2);
assert(isLexIdeal ideal(a^2*b,a^2*c)==true)
assert(isLexIdeal ideal(a^2*b,a*b^2)==false)
T=ZZ/101[a..c]/ideal(a^2-b*c);
assert(isLexIdeal ideal(b*c)==true)
///

----------------------------
-- Test isPurePower
----------------------------

TEST///
R=ZZ/101[a..c]
assert(isPurePower(b^5)==true)
assert(isPurePower(b*c^4)==false)
///

----------------------------
-- Test LPP
----------------------------

TEST///
R=ZZ/32003[a..c];
assert(LPP(R,{1,3,6,5,3},{3,3,4})==ideal(a^3,b^3,c^4,a^2*b,a^2*c,a*b^2,a*b*c^2,b^2*c^3))
assert(LPP(R,{1,3,4,2,1},{2,3,5})==lexIdeal(R,{1,3,4,2,1}))
assert(LPP(R,{1,3,4,2,1},{2,4,3})==null)
assert(LPP(R,{1,3,4,2,1},{2,2,3})==null)
///

----------------------------
-- Test generateLPPs
----------------------------

TEST///
R=ZZ/101[a..c];
assert(flatten generateLPPs(R,{1,3,6,10})=={{4,4,4},(ideal vars R)^4})
assert(#generateLPPs(R,{1,3,4,2})==4)
assert(generateLPPs(R,{1,3,6,11})=={})
///

----------------------------
-- Test isLPP
----------------------------

TEST///
R=ZZ/101[a..c];
assert(isLPP ideal(a^3,b^4,c^4,a^2*b,a^2*c^2)==true)
assert(isLPP ideal(a^4,b^3,c^4,a^2*b,a^2*c^2)==false)
assert(isLPP ideal(a^3,b^4,c^4)==true)
assert(isLPP ideal(a^4,b^5,c^4)==false)
///

----------------------------
-- Test cancelAll
----------------------------

TEST///
R=ZZ/101[a..c];
assert(cancelAll lexIdeal(R,{1,3,4,2})=={{2, 2, 3, 3}, {4, 4, 4, 5, 5}, {6, 6}})
assert(cancelAll ideal(a^2,b^2,c^2)=={{2,2,2},{4,4,4},{6}})
assert(cancelAll lexIdeal(R,{1,3,3,3})=={{2, 2, 2, 4, 4}, {3, 3, 5, 5, 5, 5, 5, 5}, {4, 6, 6, 6}})
///

----------------------------
-- Test multUpperHF
----------------------------

TEST///
R=ZZ/101[a..c];
assert(multUpperHF(R,{1,3,4,2,1})==true)
assert(multUpperHF(R,{1,3,4,4,3})==false)
///

--------------------------------------------------
-- Test multLowerBound, multUpperBound, multBounds
--------------------------------------------------

TEST///
R=ZZ/101[a..c];
assert(multBounds(ideal(a^3,b^3,c^5))==true)
assert(multUpperBound(ideal(a^2,a*b))==true)
assert(multLowerBound(ideal(a^2,a*b,b^2))==true)
assert(multLowerBound(ideal(a^2,a*b))==false)
///

end

restart
installPackage ("LexIdeals", UserMode=>true)
loadPackage "LexIdeals"
viewHelp
