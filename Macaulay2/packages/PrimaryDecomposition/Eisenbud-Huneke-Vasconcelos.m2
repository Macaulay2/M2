-- This code is based on/taken from the paper
-- "Direct methods for primary decomposition" by 
-- Eisenbud, Huneke, and Vasconcelos that appeared in
-- Inventiones Math. 110, 207-235 (1992)

--
--
-- This first section of the file defines the assasinator.
-- There are two strategies defined first
--
--

ass1 := (I,printlevel) -> (
     if I.cache.?Assassinator then I.cache.Assassinator else I.cache.Assassinator = (
     assasinator := {};
     RI := ring I;
     polyRing := ring presentation RI;
     I1 := lift(I, polyRing);
     i := codim I1;
     d := dim polyRing;
     local currentext;
     C := resolution(cokernel generators I, LengthLimit => d+1);
     --here we look at the associated primes of the i-th
     --ext and pick out the height i components ala EHV
     while i <= d do ( 
	  if printlevel >= 2 then (<< "  ass loop " << i 
	       << endl);
	  currentext = Ext^i(polyRing^1/I1,polyRing);
	  if codim currentext == i then (
	       firstlist := decompose ann currentext;
	       scan(firstlist, P -> (if codim P == i then (
			      if printlevel >= 1 then 
			      << "    " << P  << endl << endl ;
			      assasinator = 
			      append(assasinator,P)))
		    ) );
	  i=i+1;);
     assasinator = apply(
	  assasinator, P -> (trim substitute(P, RI))
	  );
     assasinator
     ))


ass2 := (I,printlevel) -> (
     if I.cache.?Assassinator then I.cache.Assassinator else I.cache.Assassinator = (
     assasinator := {};
     local newcomponents;
     RI := ring I;
     polyRing := ring presentation RI;
     currentI := lift(I,polyRing);
     topcurrentI := topComponents currentI;
     while not isSubset(topcurrentI, currentI) do(
	if printlevel >= 2 then ( 
	     print "  beginning new ass loop");
        newcomponents = flatten decompose topcurrentI;
	if printlevel >=1 then (scan(newcomponents, P -> (
		       << endl << "    " << P << endl)));
	assasinator = append(assasinator, newcomponents);
	  currentI = currentI:topcurrentI;
	  topcurrentI = topComponents currentI;
	  );
     newcomponents = flatten decompose topcurrentI;
     if printlevel >=1 then (scan(newcomponents, P -> (
		    << endl << "    " << P << endl)));
     assasinator = append(assasinator, newcomponents);
     assasinator = flatten assasinator;
     assasinator = apply(
	  assasinator, P -> (trim substitute(P,RI))
	  );
     assasinator 
     ))


ass Ideal := List => o -> (I) -> (
     if I.cache.?Assassinator then I.cache.Assassinator else I.cache.Assassinator = (
     	  if o.Strategy === 1 then (
	       if o.PrintLevel >= 1 then
	       print "ass:  Using Strategy 1";
	       ass1 (I,o.PrintLevel)) 
     	  else (
	       if o.PrintLevel >= 1 then
	       print "ass:  Using Strategy 2";
	       ass2 (I,o.PrintLevel)))
	  )


TEST ///
-- This last little code is to check if two lists are the
-- same up to permutation.

isSameList = (L1,L2) ->(
     ret := null;
     ret1 := true;
     counter1 := 0;
     counter2 := 0;
     if #L1 =!= #L2 then ret = false else(
     	  while counter1 < #L1 and ret1 == true do (
	     ret2 := false;
	     counter2 = 0;
	     while counter2 < #L2 and ret2 == false do(
		    if L1#counter1 == L2#counter2 
		    then ret2 = true;
		    counter2 = counter2 + 1;
		    );
	       if ret2 == false then ret1 = false;
	       counter1 = counter1 + 1;);
	  ret = ret1;
	  );
     ret	       
     )     	    

R=ZZ/(101)[x,y,z];
I=ideal (x^2,x*y);
assoutcome = ass(I,Strategy=>1,PrintLevel=>2)
trueass = {ideal(x),ideal(x,y)};
assert(isSameList(assoutcome,trueass))

I=ideal (x^2,x*y);
assoutcome = ass(I,Strategy=>2,PrintLevel=>2)
isSameList(assoutcome, trueass)
S=R/I;
J=ideal(0_S);	
assoutcome = ass J
trueass = {ideal(x),ideal(x,y)};
assert(isSameList(assoutcome, trueass))

R=ZZ/31991[x,y,z]
I1 = ideal(x^3,x*y,z^2);
I2 = ideal(x,y+z);
I3 = ideal(x^5*z,y^3*z^2);
K=intersect(I1,I2,I3)
assoutcome = ass K
trueass = {ideal(z), ideal(y,x), ideal(z,x), ideal(y+z,x)}     
assert(isSameList(assoutcome, trueass))
///


--In localize, P is an associated prime.
--This routine computes the localization of I at P.
--For this version, we have to know ass I

SYlocalize := (assroutine) -> (I,P,printlevel) ->(
     local ret;
     RI := ring I;
     polyRing := ring presentation RI;
     I1 := lift(I,polyRing);
     P1 := lift(P,polyRing);
     IntersectionOfPrimes := ideal (1_polyRing);
     if printlevel >= 1 then (<< endl << 
	  "Finding the assasinator of " << I1 << endl);
     Assasin := assroutine(I1,printlevel);
     if printlevel >= 2 then (<< endl << "It equals "
	  << Assasin << endl);
     RP := polyRing/P1;
     scan(Assasin, Q -> if not isSubset(Q,P) 
	  then (IntersectionOfPrimes = 
	       intersect(IntersectionOfPrimes,Q))
	  );
     IOPrp := substitute(IntersectionOfPrimes,RP);
     if IntersectionOfPrimes == ideal(1_RI) 
     then ret = I
     else (
	  if printlevel >= 1 then (<< endl << 
	       "Finding a separator polynomial" << endl);
	  f := lift((flatten entries generators gb IOPrp)#0,
	       polyRing);
	  if printlevel >= 2 then (<< endl << "It equals "
	       << f << endl);
	  f = substitute (f,RI);
	  if printlevel >= 1 then (<< endl << 
	       "Saturating with respect to the separator polynomial"
	       << endl);
	  ret = saturate(I,f);
	  );
     ret
     ) 

-- alternate localize: you only need to know I and the prime
-- this only works if it's homogeneous and equidimensional
-- and radical. 

EHVlocalize := (I,P,printlevel) ->(
     polyRing := ring presentation ring I;
     I0 := lift(I,polyRing);
     P0 := lift(P,polyRing);
     d := degree I0;
     if printlevel >= 1 then (<< endl << "Finding the "
     	  << d*(d+1) << " power of the prime ideal" << endl);
     P0Power := P0^(d*(d+1));
     if printlevel >= 2 then << endl << "It equals " 
          << P0Power << endl;
     if printlevel >= 1 then (<< endl << "Finding the top of "
	  << I0 + P0Power << endl);
     I1 := topComponents (I0 + P0Power);
     if printlevel >= 2 then(<< endl << "It equals " << endl 
	  << I1 << endl);
     gensI1 := flatten entries mingens I1;
     gensI1 = apply(gensI1, g -> 
	  if degree g <= d then g else 0);
     I2 := ideal gensI1;
     if printlevel >= 1 then (<< endl << 
	  "Finding the final top" << endl);
     I3 := topComponents I2;
     if printlevel >= 2 then (<< endl << "It equals " <<endl
	  << I3 << endl);
     if printlevel >= 1 then (<< endl << 
	  "Finding the radical" << endl);
     radical I3;
     trim substitute(I3,ring I)
     )

localize = method(Options =>{
	  PrintLevel => 0,
	  Strategy => 1})

localize (Ideal,Ideal) := Ideal => o -> (I,P) -> (
     if o.Strategy === 0 then (
	  if o.PrintLevel >=1 then
	  print "localize:  Using Strategy 0";
	  EHVlocalize(I,P,o.PrintLevel))
     else if o.Strategy === 1 then (
	  if o.PrintLevel >=1 then
	  print "localize:  Using Strategy 1";
	  (SYlocalize ass1)(I,P,o.PrintLevel))
     else (
	  if o.PrintLevel >=1 then
	  print "localize:  Using Strategy 2";
	  (SYlocalize ass2)(I,P,o.PrintLevel))
     )

TEST ///
R = ZZ/(101)[x,y];
I = ideal (x^2,x*y);
P1 = ideal (x);
outcome = localize(I,P1)
outcome == P1
P2 = ideal (x,y);
outcome = localize(I,P2)
outcome == I

R = ZZ/(31991)[x,y,z];
I = ideal(x^2,x*z,y*z);
P1 = ideal(x,y);
outcome = localize(I,P1)
outcome == P1
P2 = ideal(x,z);
outcome = localize(I,P2)
trueanswer = ideal(x^2,z);
outcome == trueanswer
///

--This part should compute the/a primary component of I
--corresponding to P.
--A BIG issue here is how to increment m.  this could make
--a big speed difference.

-- Of course in this next thing, if we know ass, we can use
-- localize, but if we just know I and the prime, we might 
-- rather use EHVlocalize.  It's hard to know.  
primarycomponent := (localizeroutine) -> 
     (I,P,printlevel,inc) -> (
     polyRing := ring presentation ring I;
     I0 := lift(I,polyRing);
     P0 := lift(P,polyRing);
     ret := null;
     m := 1;
     I0P0 := localizeroutine (I0,P0,printlevel);
     ImportantColonIdeal := saturate (I0P0,P0);
     while ret === null do (
     	  if printlevel >= 1 then (<< endl << 
	  "primaryComponent checking power " << m << endl);
     	  if printlevel >= 2 then (<< endl << "Computing
	       topComponents of " << I0 << "+ the power of " << P0 << 
	       endl);
	  Q := topComponents (I0 + P0^m);
	  if printlevel >= 2 then (<< endl << "It equals " <<
	       Q << endl); 
     	  IIntersection := intersect(Q,ImportantColonIdeal);
     	  if isSubset(IIntersection,I0P0) 
     	  then ret = Q 
     	  else m = m+inc;
	  );
     trim substitute(ideal mingens ret,ring I)
     )

primaryComponent = method(
     Options => {
	  Strategy => 2,
	  PrintLevel => 0,
	  Increment =>1})

primaryComponent(Ideal,Ideal) := Ideal => o -> (I,P) -> (
     localizefcn := if o.Strategy === 1 then
	  SYlocalize ass1
     else if o.Strategy === 2 then
	  SYlocalize ass2
     else EHVlocalize;
     (primarycomponent localizefcn)(I,P,o.PrintLevel,
	  o.Increment))

TEST ///
R = ZZ/(101)[x,y];
I = ideal (x^2,x*y);
P1 = ideal (x);
primaryComponent(I,P1)
P2 = ideal (x,y);
primaryComponent(I,P2)

R = ZZ/(31991)[x,y,z];
I = ideal(x^2,x*z,y*z);
P1 = ideal(x,y);
primaryComponent(I,P1)
P2 = ideal(x,z);
primaryComponent(I,P2)
///

--This computes a primary decomposition of an ideal.
EHVprimaryDecomposition = (I,printlevel) -> (
    Assasin := ass(I,PrintLevel=>printlevel);
    nprimes := #Assasin;
    counter := 0;
    ListofPrimaries := {};
    while counter < nprimes do (
	 newcomponent := primaryComponent(I,
	      Assasin#counter,
	      PrintLevel=>printlevel);
	 ListofPrimaries = append(ListofPrimaries,
	      newcomponent);
    	 counter = counter + 1;
	 );
     ListofPrimaries
     )

--This computes a primary decomposition of an ideal.
HprimaryDecomposition = (I,assstrategy,localizestrategy,printlevel) -> (
     assroutine := (
	  if assstrategy === 1 then ass1
	  else ass2);
     localizeroutine := (
	  if localizestrategy === 1 then EHVlocalize
	  else SYlocalize assroutine);
     primarycomponentroutine := primarycomponent(localizeroutine);
     Assasin := assroutine(I,printlevel);
     nprimes := #Assasin;
     counter := 0;
     ListofPrimaries := {};
     while counter < nprimes do (
	  newcomponent := primarycomponentroutine(I,
	       Assasin#counter,
	       printlevel,1);
	  ListofPrimaries = append(ListofPrimaries,
	       newcomponent);
    	  counter = counter + 1;
	  );
     ListofPrimaries
     )


TEST ///
R=ZZ/(101)[x,y,z];
I=ideal (x^2,x*y);
ass(I,Strategy=>1,PrintLevel=>2)
ass(I,Strategy=>2,PrintLevel=>2)
S=R/I;
J=ideal(0_S);
primaryDecomposition(J,Strategy=>EHV)
ass J
P=ideal(x);

R = ZZ/31991[x,y,z]
J=ideal (x*y^2,x*z^2);
P2=ideal(y,z);
ass(J)
localize (J,P2)
primaryComponent(J,P2)
primaryDecomposition(J,Strategy=>EHV)

R = ZZ/101[a..d]
S = R/(a*b-c^2)
T = S/(a^3,b^3)
J = ideal(c)
ring presentation T
J1 = lift(J,ring presentation T)
J1 = J1 + ideal(R_3^5)
trim substitute(J1,T)
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
