newPackage(
	"LexIdeals", 
	Version => "1.0",
	Date => "10 September 2006",
	Authors => {
		{Name => "Chris Francisco", Email => "chrisf@math.missouri.edu", 
		     HomePage => "http://www.math.missouri.edu/~chrisf"}
	},
	Headline => "A Macaulay 2 package for manipulating lexicographic-type monomial ideals",
	DebuggingMode => true
	)

export {macaulayRep, macaulayBound, macaulayLowerOperator, isHF, hilbertFunct, 
     isCM, lexIdeal, isLexIdeal, purePower, LPP, generateLPPs, isLPP, cancelAll, 
     multUpperHF, multLowerBound, multUpperBound, multBounds}

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
macaulayLowerOperator = method(TypicalValue=>List)
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
hilbertFunct = method(TypicalValue=>List,Options => {Degree => Null})
hilbertFunct(Ideal) := opts -> I -> (
     dimR:=dim ambient ring I;
     dimen:=dim I;
     h := poincare I;
     t := (ring h)_0;
     h = h // (1-t)^(dimR-dimen); --difference instead of codim to
     R := ZZ[u];     	          --make it work in quotient rings
     h = substitute(h, {t=>u});
     optDeg:=opts.Degree;
     if dimen==0 then (
	  if optDeg===Null then optDeg=first degree h else h = h % u^(optDeg+1);
	  )
     else if dimen!=0 and optDeg===Null then optDeg=20;
     uDeg := (1-u^(optDeg+1)) // (1-u);
     for i from 1 to dimen do (
     	  h = (h * uDeg);
	  h = h % u^(optDeg+1));
     reverse apply(listForm h, x -> x#1))

--determines whether (ring I/I) is Cohen-Macaulay, where ring I is a polynomial ring
isCM = method(TypicalValue=>Boolean)
isCM(Ideal) := (I) -> (
     if I == ideal(1_(ring I)) then true else
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
     iterIdeal:=trim ideal(0_R);
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
     iterIdeal
     )

--given a quotient ring R, a polynomial ring modulo a monomial ideal, and 
--an Artinian Hilbert function hilb, returns the lex ideal R corresponding
--to hilb if one exists
lexIdeal(QuotientRing,List) := (R,hilb) -> (
     m:=ideal vars R;
     hilbertLength:=#hilb;
     deg:=1;
     iterIdeal:=ideal(0_R);
     --loop to get monomials in each degree
     while deg < hilbertLength do (
	  --make list of monomials of proper degree in descending lex order
   	  monomialList:=flatten entries basis(deg,coker gens iterIdeal);
	  --add proper number of generators in each degree to idealgens
	  numberMons:=(#monomialList)-(hilb#deg);
	  if numberMons < 0 then return null;
	  if numberMons > 0 then (
	       degMons:=take(monomialList,numberMons);
	       iterIdeal=iterIdeal+ideal(degMons);
	       );
	  deg=deg+1;
	  );
     --add in all generators of next degree
     lastDegGens:=flatten entries basis(hilbertLength,coker gens iterIdeal);
     if lastDegGens != {} then iterIdeal=iterIdeal+ideal(lastDegGens);
     --clean up generators to remove redundancy
     I:=trim iterIdeal;
     --make sure the HF was allowable
     if (hilbertFunct I) == hilb then I else null
     )

--checks whether an ideal in a polynomial ring or quotient of a
--polynomial ring by a monomial ideal is a lex ideal
--fixed bug 7/21/2004: needed to check that I is a monomial ideal,
--and in quotient rings, need to lift to the ambient polynomial ring
--before checking that
isLexIdeal = method(TypicalValue=>Boolean)
isLexIdeal(Ideal) := (I) -> (
     R:=ring I;
     maxDeg:=max flatten apply(flatten entries gens I,i->degree i);
     deg:=0;
     result:=true;
     if not isMonomialIdeal lift(I,ambient ring I) then result=false;
     while deg <= maxDeg and result==true do (
	  basisI:=flatten entries basis(deg, coker gens I);
	  basisLex:=take(flatten entries basis(deg,R),-#basisI);
	  result=(basisI==basisLex);
	  deg=deg+1;
	  );
     result
     )

--determines whether a monomial in a polynomial ring is 
--a pure power of a variable
--the lift code is neeed as of 6/30/04 since exponents isn't working
--in a quotient ring
purePower = method(TypicalValue=>Boolean)
purePower(RingElement) := (mon) -> (
     number(flatten exponents lift(mon,ambient ring mon),i->i!=0)==1
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
	       if not all(addMons,i->(not purePower i)) then return null;
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
--if option Print=>true, prints {list of powers} LPP ideal
--default is not to print
generateLPPs = method(TypicalValue=>List, Options => {Print => false})
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
	       if opts.Print == true then (
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
     if not isMonomialIdeal I then return false; --added 7/21/2004
     ri:=ring I;
     numvars:=dim ri;
     m:=ideal vars ri;
     Igens:=first entries gens trim I;
     --check that the power sequence has right length and is nondecreasing
     powers:=select(Igens,i->purePower i);
     if #powers != numvars then return false;
     expon:=apply(sort apply(powers,i->purePowerIndex i),i->i#1);
     if not (isNonDec expon) then return false;
     --nonPowers are not a power of a variable
     nonPowers:=select(Igens,i->not purePower i);
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
document { Key => "LexIdeals",
     Headline => "Lex Ideals",
     EM "LexIdeals", " is a package for creating lexicographic ideals and 
     lex-plus-powers (LPP) ideals.  There are also several functions for
     use with the multiplicity conjectures of Herzog, Huneke, and Srinivasan.",
     }

document { Key => {macaulayRep,(macaulayRep,ZZ,ZZ)},
     Headline => "the Macaulay representation of an integer",
     Usage => "macaulayRep(a,d)",
     Inputs => {
	  "a" => ZZ => {"a positive integer"},
	  "d" => ZZ => {"a positive integer"}
	  },
     Outputs => { List => {"a list of pairs ", TT "{i,j}", " of integers such
	  that ", TT "j <= d", " for all terms, and ", TT "a", " is the sum of the ", TT "binomial(i,j)", "."}
	  },
     "Yields the ", TT"d", "-th Macaulay representation of the integer ", TT "a", ". Given a positive integer ", TT "d", ", each positive integer ", TT "a", " can be uniquely 
     represented as a sum of binomials ", TT "binomial(b_d,d) + binomial(b_(d-1),d-1) 
     + ... + binomial(b_1,1)", ", where ", TT "b_d > b_(d-1) > ... > b_1 >= 0", ".",
     EXAMPLE {
	  "macaulayRep(100,4)",
	  "macaulayRep(10,5)"
	  },
     SeeAlso => {"macaulayBound", "isHF"}
     }

document { Key => {macaulayBound,(macaulayBound,ZZ,ZZ)},
     Headline => "the bound on the growth of a Hilbert function from Macaulay's Theorem",
     Usage => "macaulayBound(a,d)",
     Inputs => {
	  "a" => ZZ => {"a positive integer"},
	  "d" => ZZ => {"a positive integer"}
	  },
     Outputs => {ZZ => {"an integer ", TT "j", " such that if the Hilbert function of a polynomial ring modulo a homogeneous ideal is ", TT "a", " in degree ", TT "d", ", the Hilbert function is at most ", TT "j", " in degree ", TT "d+1", "."}
	  },
     "Given a Hilbert function of ", TT "a", " in degree ", TT "d", ", yields the upper bound from Macaulay's Theorem for the Hilbert function in degree ", TT "d+1", ".", 
     EXAMPLE {
	  "macaulayBound(3,1)",
	  "macaulayBound(15,5)"
	  },
     SeeAlso => {"macaulayRep", "macaulayLowerOperator", "isHF"}
     }

document { Key => {macaulayLowerOperator,(macaulayLowerOperator,ZZ,ZZ)},
     Headline => "the a_<d> operator used in Green's proof of Macaulay's Theorem",
     Usage => "macaulayLowerOperator(a,d)",
     Inputs => {
	  "a" => ZZ => {"a positive integer"},
	  "d" => ZZ => {"a positive integer"}
	  },
     Outputs => {List => {"given ", TT "d", "-th Macaulay representation of ", TT "a = binomial(b_d,d) + ... + binomial(b_i,i)", ", returns ", TT "binomial(b_d - 1,d) + ... + binomial(b_i - 1,i)"}
	  },
     "Given positive integers ", TT "a", " and ", TT "d", ", yields ", TT "a_<d>", ", the operation from Green's proof of Macaulay's Theorem. See Bruns and Herzog, Cohen-Macaulay Rings, page 161.", 
     EXAMPLE {
	  "macaulayLowerOperator(3,1)",
	  "macaulayLowerOperator(15,5)"
	  },
     SeeAlso => {"macaulayRep", "macaulayBound"}
     }

document { Key => {isHF,(isHF,List)},
     Headline => "test whether a finite list is a Hilbert function of a polynomial ring modulo a homogeneous ideal",
     Usage => "isHF(hilb)",
     Inputs => {
	  List => "hilb" => {"a finite list of positive integers"}
	  },
     Outputs => { Boolean => {"returns ", TT "true", " if ", TT "hilb", " is a Hilbert function of a polynomial ring modulo a homogeneous ideal and ", TT "false", " otherwise."}
	  },
     "Macaulay's Theorem characterizes the sequences of integers that occur as the Hilbert function of a polynomial ring modulo a homogeneous ideal. ", TT "isHF", " checks that the input is a list of integers and that the first entry of the list is 1, and then it checks Macaulay's bound in each degree, using ", TO2((macaulayBound,ZZ,ZZ), "macaulayBound"), ". The function returns ", TT "true", " if the sequence of numbers in the list satisfies the conditions of Macaulay's Theorem.", 
     EXAMPLE {
	  "isHF({1,3,6,7,5,3})",
	  "isHF({2,3,4,3,2}) --doesn't start with a 1 in degree 0",
	  "isHF({1,3,6,8,14,3}) --growth from 8 to 14 is too high"
	  },
     SeeAlso => {"macaulayRep", "macaulayBound"}
     }

document { Key => {hilbertFunct,(hilbertFunct,Ideal)},
     Headline => "return the Hilbert function of a quotient by a homogeneous ideal as a list",
     Usage => "hilbertFunct I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring or a quotient of a polynomial ring (where the ring has the standard grading)"}
	  },
     Outputs => {List => {"returns the Hilbert function of ", TT "(ring I)/I", " as a list"}
	  },
     "Let ", TT "I", " be a homogeneous ideal in a ring ", TT "R", " that is either a polynomial ring or a quotient of a polynomial ring, and suppose that ", TT "R", " has the standard grading. ", TT "hilbertFunct", " returns the Hilbert function of ", TT "R/I", " as a list.",
     PARA {},
     "If ", TT "R/I", " is Artinian, then the default is for ", TT "hilbertFunct", " to return the entire Hilbert function (i.e., until the Hilbert function is zero) of ", TT "R/I", " as a list. The user can override this by using the ", TT "Degree", " option to bound the highest degree considered.",
     PARA {},
     "If ", TT "R/I", " is not Artinian, then ", TT "hilbertFunct", " returns the Hilbert function of ", TT "R/I", " through degree 20. Again, the user can select a different upper bound for the degree by using the ", TT "Degree", " option.",
     PARA {},
     "We require the standard grading on ", TT "R", " in order to compute with the Hilbert series, which is much faster than repeatedly computing the Hilbert function.",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "hilbertFunct ideal(a^3,b^3,c^3)",
	  "hilbertFunct ideal(a^3,a*b^2)",
	  "hilbertFunct(ideal(a^3,a*b^2),Degree=>4)",
	  "M=ideal(a^3,b^4,a*c);",
	  "Q=R/M;",
	  "hilbertFunct ideal(c^4)",
	  "hilbertFunct ideal(b*c,a*b)",
	  },
     SeeAlso => {"isHF", "lexIdeal"}
     }

document { Key => {isCM,(isCM,Ideal)},
     Headline => "test whether a polynomial ring modulo a homogeneous ideal is Cohen-Macaulay",
     Usage => "isCM I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if ", TT "(ring I)/I", " is Cohen-Macaulay and ", TT "false", " otherwise"}
	  },
     "", TT "isCM", " takes a homogeneous ideal ", TT "I", " in a polynomial ring ", TT "R", " and, by computing the projective dimension and codimension of ", TT "I", ", determines whether ", TT "R/I", " is Cohen-Macaulay. Of course, ", TT "isCM", " works only if Macaulay 2 can compute the projective dimension of ", TT "I", ".",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "isCM(ideal(a^2,b^4)) --complete intersection",
	  "isCM(ideal(a^3,b^5,c^4,a*c^3)) --Artinian",
	  "isCM(ideal(a^3,a*b^2))"
	  }
     }

document { Key => {lexIdeal,(lexIdeal,PolynomialRing,List),(lexIdeal,QuotientRing,List)},
     Headline => "produce an Artinian lexicographic ideal",
     Usage => "lexIdeal(R,hilb)\nlexIdeal(Q,hilb)",
     Inputs => {
	  "R" => PolynomialRing,
	  "Q" => QuotientRing => {"a polynomial ring modulo a monomial ideal"},
	  "hilb" => List => {"a finite length list of positive integers"}
	  },
     Outputs => {Ideal => {"the Artinian lexicographic ideal ", TT "L", " in ", TT "R", " (resp. ", TT "Q", ") such that ", TT "R/L", " (resp. ", TT "Q/L", ") has Hilbert function ", TT "hilb", "."}
	  },
     "When ", TT "R", " is a polynomial ring, if ", TT "hilb", " is an O-sequence (that is, it satisfies Macaulay's Theorem), such an ", TT "L", " always exists. When ", TT "Q", " is a quotient of a polynomial ring, there may be no lexicographic ideal with a particular Hilbert function even if it is an O-sequence. ", TT "lexIdeal", " returns ", TT "null", " if no lexicographic ideal ", TT "L", " corresponding to the Hilbert function ", TT "hilb", " exists in ", TT "R", " or ", TT "Q", ".",
     PARA {},
     "We hope eventually to implement a version of ", TT "lexIdeal", " for nonArtinian ideals, taking a Hilbert series as the input.",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "lexIdeal(R,{1,3,4,3,1})",
	  "lexIdeal(R,{1,3,7}) --not an O-sequence, so no lex ideal exists",
	  "Q=R/ideal(a^3,b^3,a*c^2);",
	  "lexIdeal(Q,{1,3,6,4,2})",
	  "lexIdeal(Q,{1,3,6,4,4}) --value of 4 in degree 4 is too high in this ring"
	  },
     SeeAlso => {"macaulayRep","macaulayBound","isHF","isLexIdeal"}
     }

document { Key => {isLexIdeal,(isLexIdeal,Ideal)},
     Headline => "determine whether an ideal is a lexicographic ideal",
     Usage => "isLexIdeal I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring or quotient of a polynomial ring by a monomial ideal"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if ", TT "I", " is a lexicographic ideal in ", TT "ring I", " and ", TT "false", " otherwise."}
	  },
     "Given an ideal ", TT "I", " in a ring ", TT "R", " that is either a polynomial ring or a quotient of a polynomial ring by a monomial ideal, ", TT "isLexIdeal", " first checks to see that ", TT "I", " is a monomial ideal. If not, it returns ", TT "false", ". If so, ", TT "isLexIdeal", " computes bases of ", TT "R/I", " in each degree up through the maximum degree of a minimal generator of ", TT "I", " to determine whether ", TT "I", " is a lexicographic ideal in ", TT "R", ".",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "isLexIdeal lexIdeal(R,{1,3,4,3,1})",
	  "isLexIdeal ideal(a^3-a^2*b)",
	  "isLexIdeal ideal(a^3,a^2*b)",
	  "Q=R/ideal(a^3,b^3,a*c^2);",
	  "isLexIdeal ideal(a^2*b,a^2*c)",
	  "isLexIdeal ideal(a^2*b,a*b^2)"
	  },
     SeeAlso => {"lexIdeal"}
     }

document { Key => {purePower,(purePower,RingElement)},
     Headline => "determine whether a ring element is a pure power of a variable",
     Usage => "purePower f",
     Inputs => {
	  "f" => RingElement => {"an element of a polynomial ring or quotient of a polynomial ring"}
	  },
     Outputs => {Boolean => {"returns true if ", TT "f", " is a nonzero power of a variable and false otherwise."}
	  },
     "", TT "purePower", " tests a ring element in a polynomial ring or quotient of a polynomial ring to determine whether or not it is nonzero and a power of a variable. ", TT "purePower", " is used in the lex-plus-powers (", TO2((LPP,PolynomialRing,List,List),"LPP"), ") code.",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
     	  "purePower a^4",
	  "purePower (a*b^5)",
	  "purePower (a^3-b^3)",
	  "Q=R/ideal(a^3,a*b^3);",
	  "purePower b^4",
	  "purePower a^3 --false since a^3 is zero in the quotient"
	  },
     SeeAlso => {"LPP", "isLPP"}
     }

document { Key => {LPP,(LPP,PolynomialRing,List,List)},
     Headline => "return the lex-plus-powers (LPP) ideal corresponding to a given Hilbert function and power sequence",
     Usage => "LPP(R,hilb,power)",
     Inputs => {
	  "R" => PolynomialRing,
	  "hilb" => List => {"a Hilbert function as a list"},
	  "power" => List => {"a list of positive integers in weakly increasing order"}
	  },
     Outputs => {Ideal => {"returns an LPP ideal with the desired Hilbert function and power sequence if one exists and ", TT "null", " otherwise."}
	  },
     "Let ", TT "a_1 <= ... <= a_n", " be positive integers. A monomial ideal ", TT "L", " in a polynomial ring ", TT "R=k[x_1,...,x_n]", " is called an ", TT "(a_1,...a_n)", "-lex-plus-powers (LPP) ideal if it satisfies two conditions:",
     PARA{},
     "(1) L is minimally generated by ", TT "x_1^a_1, ..., x_n^a_n", " and monomials ", TT "m_1, ..., m_t", ".",
     PARA{},
     "(2) Suppose that r is a monomial such that ", TT "deg r = deg m_i", ", and ", TT "r > m_i", " in the lex order. Then ", TT "r", " is in ", TT "L", ".",
     PARA{},
     "LPP ideals are generalizations of Artinian lexicographic ideals. Condition (2) represents the lex portion of the LPP ideal; monomials that are not powers of a variable must satisfy a lexicographic condition similar to what generators of lex ideals satisfy. An LPP ideal also contains powers of all the variables in weakly increasing order.",
     PARA {},
     "LPP ideals arise in conjectures of Eisenbud-Green-Harris and Charalambous-Evans in algebraic geometry, Hilbert functions, and graded free resolutions. They are conjectured to play a role analogous to that of lexicographic ideals in theorems of Macaulay on Hilbert functions and Bigatti, Hulett, and Pardue on resolutions.",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
     	  "LPP(R,{1,3,6,5,3},{3,3,4})",
	  "LPP(R,{1,3,4,2,1},{2,3,5}) --an Artinian lex ideal",
	  "LPP(R,{1,3,4,2,1},{2,4,3}) --exponents not in weakly increasing order",
	  "LPP(R,{1,3,4,2,1},{2,2,3}) --no LPP ideal with this Hilbert function and power sequence"
	  },
     SeeAlso => {"generateLPPs", "isLPP"}
     }

document { Key => {generateLPPs,(generateLPPs,PolynomialRing,List)},
     Headline => "return all LPP ideals corresponding to a given Hilbert function",
     Usage => "generateLPPs(R,hilb)",
     Inputs => {
	  "R" => PolynomialRing,
	  "hilb" => List => {"a Hilbert function as a list"}
	  },
     Outputs => {List => {"returns a list of the form {{powers, LPP ideal}, {powers, LPP ideal}, ...}"}
	  },
     "Given a polynomial ring ", TT "R", " and a Hilbert function ", TT "hilb", " for ", TT "R", " modulo a homogeneous ideal, ", TT "generateLPPs", " generates all the LPP ideals corresponding to ", TT "hilb", ". The power sequences and ideals are returned in a list. If the user sets the ", TT "Print", " option to ", TT "true", ", the power sequences and ideals are printed on the screen in a nice format.",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "generateLPPs(R,{1,3,4,3,2})"
	  },
     "Same example with the ", TT "Print", " option set to ", TT "true", ":",
     EXAMPLE {
	  "generateLPPs(R,{1,3,4,3,2},Print=>true)"
	  },
     SeeAlso => {"LPP", "isLPP"}
     }

document { Key => {isLPP,(isLPP,Ideal)},
     Headline => "determine whether an ideal is an LPP ideal",
     Usage => "isLPP I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if ", TT "I", " is an LPP ideal in ", TT "ring I", " and ", TT "false", " otherwise."}
	  },
     "Given an ideal ", TT "I", " in a polynomial ring ", TT "R", ", ", TT "isLPP", " first checks to see that ", TT "I", " is a monomial ideal. If not, it returns ", TT "false", ". If so, ", TT "isLPP", " checks that ", TT "I", " is Artinian and that the power sequence is weakly increasing. Then ", TT "isLPP", " computes bases of ", TT "R/I", " in each degree up through the maximum degree of a minimal generator of ", TT "I", " to determine whether ", TT "I", " is an LPP ideal in ", TT "R", ".",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "isLPP LPP(R,{1,3,4,3,2},{2,2,4})",
	  "isLPP ideal(a^3,b^3,c^3,a^2*b,a^2*c,a*b^2*c^2)",
	  "isLPP ideal(a^3,b^4) --not Artinian since no power of c",
	  "isLPP ideal(a^3,b^4,c^3) --powers not weakly increasing",
	  "isLPP ideal(a^3,b^3,c^3,a^2*b,a*b^2)"
	  },
     SeeAlso => {"isLexIdeal", "LPP", "generateLPPs"}
     }

document { Key => {cancelAll,(cancelAll,Ideal)},
     Headline => "make all potentially possible cancellations in the graded free resolution of an ideal in a particular way",
     Usage => "cancelAll I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring"}
	  },
     Outputs => {List => {"prints the new Betti diagram and returns lists of the new shifts."}
	  },
	"This function is useful in testing the upper bound of the conjecture of Herzog-Huneke-Srinivasan on the multiplicity of an ideal; it is designed to make all potentially possible cancellations in a Betti diagram in a certain order described below.",
	PARA {},
	"Let ", TT "R", " be a polynomial ring, let ", TT "I", " and ", TT "J", " be homogeneous ideals in ", TT "R", " of codimension ", TT "c", " with the same Hilbert function, and assume that ", TT "R/I", " and ", TT "R/J", " are Cohen-Macaulay. Suppose that the graded Betti numbers of ", TT "R/I", " are at most those of ", TT "R/J", "; that is, ", TT "beta_{ij}(R/I) <= beta_{ij}(R/J)", " for all ", TT "i", " and ", TT "j", ". (We can form a partial order on resolutions of modules with the same Hilbert function by saying that ", TT "beta(R/I) <= beta(R/J)", " if and only if ", TT "beta_{ij}(R/I) <= beta_{ij}(R/J)", " for all ", TT "i", " and ", TT "j", ".)",
	PARA {},
	"Let ", TT "m_i(R/I)", " and ", TT "M_i(R/I)", " be the minimum and maximum shifts at step ", TT "i", " in the minimal graded free resolution of ", TT "R/I", ". Let ", TT "e(R/I)", " be the multiplicity of ", TT "R/I", ". Herzog, Huneke, and Srinivasan have conjectured that",
	PARA {},
	TT "m_1(R/I) ... m_c(R/I) / c! <= e(R/I) <= M_1(R/I) ... M_c(R/I) / c!", ",",
	PARA {},
	"where the ellipses signfiy that we are taking the product of the shifts.",
	PARA {},
	"Suppose that ", TT "R/I", " satisfies the conjectured inequalities. Then it is easy to show that ", TT "R/J", " must as well: Since the resolution of ", TT "R/J", " contains all the shifts in the resolution of ", TT "R/I", " plus possibly more, the minimum shifts for ", TT "R/J", " can only be the same or lower, and the maximum shifts can only be the same or higher. Therefore one can attack this conjecture by looking at resolutions minimal in the partial order discussed above.",
	PARA {},
	"It is difficult to find which potential resolutions at the bottom of the partial order for a given Hilbert function do occur for a module, however. The function ", TT "cancelAll", " implements a fast way of proving that the upper bound of the Herzog-Huneke-Srinivasan conjecture is satisfied for particular Hilbert functions; it is used in ", TO2 ((multUpperHF,PolynomialRing,List),"multUpperHF"), ". The idea is to create a Betti diagram that is minimal in the partial order that may or may not actually occur for a module; we can then test the upper bound on that potential resolution. As we explain below, if the inequality holds, it doesn't matter whether the Betti diagram occurs for a module or not.",
	PARA {},
	"", TT "cancelAll", " starts with the far left side of the Betti diagram of the ideal I the user inputs, and it makes all potentially possible cancellations in the first two columns. Then it moves to the first and second syzygies columns and makes all potentially possible cancellations, continuing through the rest of the Betti diagram. This technique is designed to investigate the upper bound of the conjecture. One may have choices of which syzygies to cancel from the resolution of the lexicographic ideal. This algorithm selects the cancellations that, if we start out with a lexicographic ideal ", TT "L", ", and ", TT "R/L", " is Cohen-Macaulay, minimize each ", TT "M_i", " that could occur for a Cohen-Macaulay module with the same Hilbert function as ", TT "R/L", ". Therefore if the upper bound of the conjecture holds for the potential Betti diagram that ", TT "cancelAll", " produces, it holds for all Cohen-Macaulay modules with the same Hilbert function and codimension. Even if the potential Betti diagram ", TT "cancelAll", " produces doesn't occur for a module, the maximum shifts could only increase, which won't cause the inequality to fail if it holds for the result of ", TT "cancelAll", ".",
	PARA {},
	"", TT "cancelAll", " makes all the cancellations with the process described above, prints the resulting Betti diagram, and then returns the shifts in a list of lists.",
   	PARA {},
	"See C. Francisco, New approaches to bounding the multiplicity of an ideal, ", EM " J. Algebra ", "299 (2006), no. 1, 309-328.",
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "L=lexIdeal(R,{1,3,6,9,9,6,2});",
     	  "betti res L",
	  "cancelAll L",
	  "M=lexIdeal(R,{1,3,4,3,2});",
	  "betti res M",
	  "cancelAll M",
	  "A=ZZ/32003[a..e];",
	  "J=lexIdeal(A,{1,5,12,10,6,3})",
	  "betti res J",
	  "cancelAll J"
	  },
     SeeAlso => {"multUpperHF", "multUpperBound", "multLowerBound", "multBounds"}
     }

document { Key => {multUpperHF,(multUpperHF,PolynomialRing,List)},
     Headline => "determine whether a particular sufficient condition is satisfied that implies the upper bound of the Herzog-Huneke-Srinivasan conjecture for all modules with a given Hilbert function",
     Usage => "multUpperHF(R,hilb)",
     Inputs => {
	  "R" => PolynomialRing,
	  "hilb" => List => {"a Hilbert function for ", TT "R", " modulo a homogeneous ideal"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if the sufficient condition holds and ", TT "false", " otherwise."}
	  },
     "This function uses ", TO2((cancelAll,Ideal),"cancelAll"), " to test a sufficient condition for the upper bound of the Herzog-Huneke-Srinivasan conjecture to hold for all quotients of polynomial rings with Hilbert function ",  TT "hilb", ". The conjecture asserts that if ", TT "R", " is a polynomial ring, and ", TT "I", " is a homogeneous ideal in ", TT "R", " then ",
     PARA{},
     TT "e(R/I) <= M_1 ... M_c / c!", ",",
     PARA{},
     "where ", TT "M_i", " is the largest shift at step ", TT "i", " of the minimal graded free resolution of ", TT "R/I", ", ", TT "c", " is the codimension of ", TT "I", ", and ", TT "e(R/I)", " is the multiplicity of ", TT "R/I", ".",
     PARA{},
     TT "multUpperHF", " forms the lex ideal corresponding to the Hilbert function the user inputs. Then it calls ", TO2((cancelAll,Ideal),"cancelAll"), " to make all potentially possible cancellations in the resolution of the lex ideal (as described in the ", TO2((cancelAll,Ideal),"cancelAll"), " documentation). ", TO2((cancelAll,Ideal),"cancelAll"), " prints the diagram with the cancellations. In addition, ", TT "multUpperHF", " compares the multiplicity associated to the given Hilbert function to the conjectured upper bound from the Betti diagram with the cancellations. If the conjectured inequality holds, the function returns ", TT "true", ", and the conjectured upper bound is true for all modules ", TT "R/I", " with the given Hilbert function. If the inequality fails, then the function returns ", TT "false", ", and the sufficient condition does not hold. (However, this of course does not mean that the conjecture is false for the given Hilbert function; it may be that the Betti diagram with the cancellations cannot exist.) Note that since ", TT "hilb", " is a finite list, all modules with that Hilbert function are Artinian and hence Cohen-Macaulay.",
   	PARA {},
	"See C. Francisco, New approaches to bounding the multiplicity of an ideal, ", EM " J. Algebra ", "299 (2006), no. 1, 309-328.",
      EXAMPLE {
	  "S=ZZ/32003[a..c];",
	  "betti res lexIdeal(S,{1,3,4,2,1}) --just to see the resolution of the lex ideal",
	  "multUpperHF(S,{1,3,4,2,1})"
	  },
     SeeAlso => {"cancelAll", "multUpperBound", "multLowerBound", "multBounds"}
     }

document { Key => {multUpperBound,(multUpperBound,Ideal)},
     Headline => "determine whether the multiplicity of an ideal satisfies the upper bound conjectured by Herzog-Huneke-Srinivasan",
     Usage => "multUpperBound I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if ", TT "I", " satisfies the upper bound and ", TT "false", " otherwise."}
	  },
     "Let ", TT "I", " be a homogeneous ideal of codimension ", TT "c", " in a polynomial ring ", TT "R", ". Huneke and Srinivasan (and later Herzog and Srinivasan in the non-Cohen-Macaulay case) conjectured that ",
     PARA{},
     TT "e(R/I) <= M_1 ... M_c / c!", ",",
     PARA{},
     "where ", TT "M_i", " is the maximum shift in the minimal graded free resolution of ", TT "R/I", " at step ", TT "i", ", and ", TT "e(R/I)", " is the multiplicity of ", TT "R/I", ". ", TT "multUpperBound", " tests this inequality for the given ideal, returning ", TT "true", " if the inequality holds and ", TT "false", " otherwise, and it prints the upper bound and the multiplicity (listed as the degree).",     
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "multUpperBound ideal(a^4,b^4,c^4)",
	  "multUpperBound ideal(a^3,b^5,c^6,a^2*b,a*b*c)"
	  },
     SeeAlso => {"cancelAll", "multUpperHF", "multLowerBound", "multBounds"}
     }

document { Key => {multLowerBound,(multLowerBound,Ideal)},
     Headline => "determine whether the multiplicity of an ideal satisfies the lower bound of the Herzog-Huneke-Srinivasan conjecture",
     Usage => "multLowerBound I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring ", TT "R", " such that ", TT "R/I", " is Cohen-Macaulay"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if ", TT "I", " satisfies the lower bound and ", TT "false", " otherwise."}
	  },
     "Let ", TT "I", " be a homogeneous ideal of codimension ", TT "c", " in a polynomial ring ", TT "R", " such that ", TT "R/I", " is Cohen-Macaulay. Huneke and Srinivasan conjectured that ",
     PARA {},
     TT "m_1 ... m_c / c! <= e(R/I)", ",",
     PARA {},
     "where ", TT "m_i", " is the minimum shift in the minimal graded free resolution of ", TT "R/I", " at step ", TT "i", ", and ", TT "e(R/I)", " is the multiplicity of ", TT "R/I", ". ", TT "multLowerBound", " tests this inequality for the given ideal, returning ", TT "true", " if the inequality holds and ", TT "false", " otherwise, and it prints the lower bound and the multiplicity (listed as the degree).",     
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "multLowerBound ideal(a^4,b^4,c^4)",
	  "multLowerBound ideal(a^3,b^5,c^6,a^2*b,a*b*c)"
	  },
     SeeAlso => {"cancelAll", "multUpperHF", "multUpperBound", "multBounds"}
     }

document { Key => {multBounds,(multBounds,Ideal)},
     Headline => "determine whether the multiplicity of an ideal satisfies the upper and lower bounds from the conjectures of Herzog-Huneke-Srinivasan",
     Usage => "multBounds I",
     Inputs => {
	  "I" => Ideal => {"a homogeneous ideal in a polynomial ring such that ", TT"(ring I)/I", " is Cohen-Macaulay"}
	  },
     Outputs => {Boolean => {"returns ", TT "true", " if both the upper and lower bounds hold and ", TT "false", " otherwise."}
	  },
     "Let ", TT "I", " be a homogeneous ideal of codimension ", TT "c", " in a polynomial ring ", TT "R", " such that ", TT "R/I", " is Cohen-Macaulay. Herzog, Huneke, and Srinivasan conjectured that if ", TT "R/I", " is Cohen-Macaulay, then ",
     PARA{},
     TT "m_1 ... m_c / c! <= e(R/I) <= M_1 ... M_c / c!", ",",
     PARA{}, 
     "where ", TT "m_i", " is the minimum shift in the minimal graded free resolution of ", TT "R/I", " at step ", TT "i", ", ", TT "M_i", " is the maximum shift in the minimal graded free resolution of ", TT "R/I", " at step ", TT "i", ", and ", TT "e(R/I)", " is the multiplicity of ", TT "R/I", ". If ", TT "R/I", " is not Cohen-Macaulay, the upper bound is still conjectured to hold. ", TT "multBounds", " tests the inequalities for the given ideal, returning ", TT "true", " if both inequalities hold and ", TT "false", " otherwise. ", TT "multBounds", " prints the bounds and the multiplicity (called the degree), and it calls ", TO2((multUpperBound,Ideal), "multUpperBound"), " and ", TO2((multLowerBound,Ideal), "multLowerBound"), ".",         
     EXAMPLE {
	  "S=ZZ/32003[a..c];",
     	  "multBounds ideal(a^4,b^4,c^4)",
	  "multBounds ideal(a^3,b^4,c^5,a*b^3,b*c^2,a^2*c^3)"
	  },
     SeeAlso => {"cancelAll", "multUpperHF", "multUpperBound", "multLowerBound"}
	}

end


///
restart
errorDepth = 0
debugLevel = 5
loadPackage( "LexIdeals" )
installPackage LexIdeals

installPackage(LexIdeals, 
     Prefix => "./tmp/",
     AbsoluteLinks => true)
run ("open " | "tmp/LexIdeals-1.0/share/doc/Macaulay2/LexIdeals/html/index.html")

pname = LexIdeals#"title" | "-" | LexIdeals.Options.Version
prefix1 = "./tmp/"
prefix1p = "./tmp/" | pname
prefix2p = "/capybara/encap/" | pname
prefix3 = "/capybara"
installPackage(LexIdeals, Prefix => prefix1)
copyDirectory (prefix1p, prefix2p, Verbose => debugLevel > 0)
stderr << "--installed package LexIdeals in " << prefix2p << endl
run "epkg LexIdeals"
view = () -> (
     fn := prefix3 | "/" | LAYOUT#"packagehtml" LexIdeals#"title" | "index.html";
     run ( "open " | fn ))
view()
///
