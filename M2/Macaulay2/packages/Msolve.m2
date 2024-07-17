newPackage(
	"Msolve",
	Version => "1.23", 
    	Date => "March 2024",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "mhelmer@ncsu.edu", 
		  HomePage => "http://martin-helmer.com/"}, {Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "https://math.cornell.edu/michael-e-stillman"},{Name => "Anton Leykin", 
		  Email => "leykin@math.gatech.edu", 
		  HomePage => "https://antonleykin.math.gatech.edu/"}},
    	Headline => "An interface to the msolve package (https://msolve.lip6.fr/) which computes Groebner Basis and does real root isolation",
    	AuxiliaryFiles => true,
	DebuggingMode => false,       
	Configuration => {"msolveExecutable"=>"msolve"}
    	);
--currently setup to use the  configation option to point to the folder where the msolve binary file lives
msolveEXE = (options Msolve).Configuration#"msolveExecutable";
msolveEXEwithOpts:=(opt)->msolveEXE|" -v "|opt#"level of verbosity"|" -t "|toString(opt#"number of threads");
if not instance(msolveEXE,String) then error "expected configuration option msolveMainFolder to be a string."
export{
    "msolveGB",
    "msolveSaturate",
    "msolveEliminate",
    "msolveRUR",
    "msolveLeadMonomials",
    "msolveRealSolutions",
    -- option names
    "Output"
    }
importFrom_Core { "raw", "rawMatrixReadMsolveFile" }

use'readMsolveOutputFile := true;
readMsolveOutputFile = method()
readMsolveOutputFile(Ring,String) := Matrix => (R,mOut) -> if use'readMsolveOutputFile 
    then map(R, rawMatrixReadMsolveFile(raw R, mOut)) else (
	msolveOut := get mOut;
    	moutStrL:=separate(",",first separate("[]]",last separate("[[]",msolveOut)));
    	--the line below should be replaced by a call to the C-function to parse the string
    	M2Out:=for s in moutStrL list value(s);
    	matrix {M2Out};
    	)

inputOkay=method(TypicalValue=>Boolean);
inputOkay(Ideal):=I->(
    R:=ring I;
    if not instance(R,PolynomialRing) then (print "input must be in a polynomial ring over a field";return false;);
    kk:=coefficientRing(R);
    if instance(kk,InexactFieldFamily) then(
        print "input must be over the rationals or a (prime) finite field of chacteristic between 2^16 and 2^32";
	return false;
	);
    if char(kk)>0 then (
	if (char(kk)<2^16) or (char(kk)>2^32) then(
	    print "input must be over the rationals or a (prime) finite field of chacteristic between 2^16 and 2^32";
	    return false;
	    );
	);
    return true;
    );
msolveGB=method(TypicalValue=>Matrix, Options=>{"level of verbosity"=>0, "number of threads"=>maxAllowableThreads});
msolveGB(Ideal):=opt->I->(
    if not inputOkay(I) then return 0;
    mIn:=temporaryFileName()|".ms";
    R:=ring I;
    kk:=coefficientRing(R);
    gR:=toString(gens R);
    l1:=substring(1,#gR-2,gR);
    l2:=char R;
    gI:=toString flatten entries gens I;
    if (isField(kk) and (char(kk)==0)) then gI=replace("[)(]","",gI);
    Igens:=replace(" ",""|newline,substring(1,#gI-2,gI));
    inStr:=l1|newline|l2|newline|Igens;
    mIn<<inStr<<close;
    mOut:=temporaryFileName()|".ms";
    << "msolve input file is called: " << mIn << endl;
    << "msolve output file is called: " << mOut << endl;
    callStr:=msolveEXEwithOpts(opt)|" -g 2 -f "|mIn|" -o "|mOut;
    run(callStr);
    <<"done msolve"<<endl;
    msolGB:=readMsolveOutputFile(R, mOut);
    return gens forceGB msolGB;
    );
msolveLeadMonomials=method(TypicalValue=>Matrix, Options=>{"level of verbosity"=>0, "number of threads"=>maxAllowableThreads});
msolveLeadMonomials(Ideal):=opt->(I)->(
    if not inputOkay(I) then return 0;
    mIn:=temporaryFileName()|".ms";
    R:=ring I;
    kk:=coefficientRing(R);
    gR:=toString(gens R);
    l1:=substring(1,#gR-2,gR);
    l2:=char R;
    gI:=toString flatten entries gens I;
    if (isField(kk) and (char(kk)==0)) then gI=replace("[)(]","",gI);
    Igens:=replace(" ",""|newline,substring(1,#gI-2,gI));
    inStr:=l1|newline|l2|newline|Igens;
    mIn<<inStr<<close;
    mOut:=temporaryFileName()|".ms";
    callStr:=msolveEXEwithOpts(opt)|" -g 1 -f "|mIn|" -o "|mOut;
    run(callStr);
    msolGB:=readMsolveOutputFile(R, mOut);
    return gens forceGB msolGB;
    );
msolveEliminate=method(Options=>{"level of verbosity"=>0, "number of threads"=>maxAllowableThreads});
msolveEliminate(Ideal,RingElement):=Ideal =>opt-> (I,elimvar)->(
    return msolveEliminate(I,{elimvar});
    );
msolveEliminate(RingElement,Ideal):=Ideal => opt->(elimvar,I) ->
    msolveEliminate(I,{elimvar})
msolveEliminate(List,Ideal):=Ideal => opt->(elimvars,I) -> 
    msolveEliminate(I,elimvars)
msolveEliminate(Ideal,List):=Ideal => opt->(J,elimvars)->(
    if not inputOkay(J) then return 0;
    mIn:=temporaryFileName()|".ms";
    S:=ring J;
    kk:=coefficientRing(S);
    keepvars:=select(gens S, s->not member(s,elimvars)); 
    elimNum:=length(elimvars);
    --R:=kk(monoid [join(elimvars,keepVars),MonomialOrder=>{numgens(S):1}]);
    R:=kk(monoid [join(elimvars,keepvars),MonomialOrder=>{#elimvars, #keepvars}]);
    elimR:=kk(monoid [keepvars]);
    I:=sub(J,R);
    gR:=toString(gens R);
    l1:=substring(1,#gR-2,gR);  -- removing parentheses
    l2:=char R;
    gI:=toString flatten entries gens I;
    if (isField(kk) and (char(kk)==0)) then gI=replace("[)(]","",gI);
    Igens:=replace(" ",""|newline,substring(1,#gI-2,gI));
    inStr:=l1|newline|l2|newline|Igens;
    mIn<<inStr<<close;
    mOut:=temporaryFileName()|".ms";
    << "mOut = " << mOut << endl;
    callStr:=msolveEXEwithOpts(opt)|" -e "|toString(elimNum)|" -g 2 -f "|mIn|" -o "|mOut;
    run(callStr);
    if char R === 0 then 
      ideal readMsolveOutputFile(elimR, mOut)
    else
      ideal readMsolveOutputFile(R, mOut)
    )

msolveSaturate=method(TypicalValue=>Matrix, Options=>{"level of verbosity"=>0, "number of threads"=>maxAllowableThreads});
msolveSaturate(Ideal,RingElement):=opt->(I,f)->(
    if not inputOkay(I) then return 0;
    mIn:=temporaryFileName()|".ms";
    R:=ring I;
    kk:=coefficientRing(R);
    gR:=toString(gens R);
    l1:=substring(1,#gR-2,gR);
    l2:=char R;
    gI:=toString flatten entries gens I;
    if (isField(kk) and (char(kk)==0)) then gI=replace("[)(]","",gI);
    Igens:=replace(" ",""|newline,substring(1,#gI-2,gI));
    inStr:=l1|newline|l2|newline|Igens|","|newline|toString(f);
    mIn<<inStr<<close;
    mOut:=temporaryFileName()|".ms";
    callStr:=msolveEXEwithOpts(opt)|" -f "|mIn|" -S -g 2 -o "|mOut;
    run(callStr);
    msolGB:=readMsolveOutputFile(R, mOut);
    << "msolve output file is called: " << mOut << endl;
    return gens forceGB msolGB;
    );

msolveRealSolutions=method(TypicalValue=>List,Options => {Output=>"rationalInterval","level of verbosity"=>0, "number of threads"=>maxAllowableThreads});
msolveRealSolutions(Ideal):=opts->(I)->(
    if not inputOkay(I) then return 0;
    mIn:=temporaryFileName()|".ms";
    R:=ring I;
    kk:=coefficientRing(R);
    gR:=toString(gens R);
    l1:=substring(1,#gR-2,gR);
    l2:=char R;
    gI:=toString flatten entries gens I;
    if (isField(kk) and (char(kk)==0)) then gI=replace("[)(]","",gI);
    Igens:=replace(" ",""|newline,substring(1,#gI-2,gI));
    inStr:=l1|newline|l2|newline|Igens;
    mIn<<inStr<<close;
    mOut:=temporaryFileName()|".ms";
    callStr:=msolveEXEwithOpts(opts)|" -f "|mIn|" -o "|mOut;
    run(callStr);
    outStrFull:=get(mOut);
    mOutStr:=replace("[]]","}",replace("[[]","{",(separate("[:]",outStrFull))_0));
    solsp:=value(mOutStr);
    if solsp_0>0 then (print "Input ideal not zero dimensional, no solutions found."; return 0;);
    if (solsp_1)_0>1 then (print "unexpected msolve output, returning full output"; return solsp;);
    sols:=(solsp_1)_1;
    if opts.Output=="rationalInterval" then return sols;
    if opts.Output=="floatInterval" then return (1.0*sols);
    if opts.Output=="float" then return (for s in sols list(for s1 in s list sum(s1)/2.0));
    );
msolveRUR=method(TypicalValue=>List,Options=>{"level of verbosity"=>0, "number of threads"=>maxAllowableThreads});
msolveRUR(Ideal):=opt->(I)->(
    if not inputOkay(I) then return 0;
    mIn:=temporaryFileName()|".ms";
    R:=ring I;
    kk:=coefficientRing(R);
    gRm2:=gens R;
    gR:=toString(gRm2);
    l1:=substring(1,#gR-2,gR);
    l2:=char R;
    gI:=toString flatten entries gens I;
    if (isField(kk) and (char(kk)==0)) then gI=replace("[)(]","",gI);
    Igens:=replace(" ",""|newline,substring(1,#gI-2,gI));
    inStr:=l1|newline|l2|newline|Igens;
    mIn<<inStr<<close;
    mOut:=temporaryFileName()|".ms";
    run(msolveEXEwithOpts(opt)|" -P 2 -f "|mIn|" -o "|mOut);
    mOutStr:=replace("[]]","}",replace("[[]","{",(separate("[:]",get(mOut)))_0));
    solsp:=value replace("[']","\"",mOutStr);
    if first solsp!=0  then (print "Input ideal not zero dimensional, no solutions found."; return 0;);
    lc:=(solsp_1)_4;
    l:=sum(numgens R,i->lc_i*gRm2_i);
    RUR:= new MutableHashTable;
    T:= getSymbol("T");
    S2:=(coefficientRing(R))[T];
    T=first gens S2;
    RUR#"T"=l;
    RUR#"var"=T;
    RUR#"degree"=(solsp_1)_2;
    para:= ((solsp_1)_5)_1;
    W:=sum((para_0)_0+1,i->(T)^i*(((para_0)_1)_i));
    RUR#"findRootsUniPoly"=W;
    RUR#"denominator"=diff(T,W);
    vs:=last para;
    RUR#"numerator"=append(for f in vs list sum((f_0)_0+1,i->T^i*((f_0)_1)_i),-T*diff(T,W));
    return new HashTable from RUR;
    );

beginDocumentation()
multidoc ///

Node 
     Key
     	  Msolve
     Headline
     	  Macaulay2 interface for msolve; computes real solutions, Groebner basis (in GRevLex), saturations, and elimination ideals.
     Description
     	  Text

              This package provides a Macaulay2 interface for the
              msolve package (https://msolve.lip6.fr/) developed by
              Jérémy Berthomieu, Christian Eder, and Mohab Safey El
              Din.
	      
	      The package has functions to compute Groebner basis, in
	      GRevLex order only, for ideals with rational or finite
	      field coefficients. Finite field characteristics must be
	      between 2^16 and 2^32. There are also functions to
	      compute elimination ideals, for ideals with rational or
	      finite field coefficients. Finite field characteristics
	      must be between 2^16 and 2^32.
	      
	      The saturation of an ideal by a single polynomial may be
	      computed for ideals with finite field coefficients, again
	      with characteristic between 2^16 and 2^32.
	      
	      For zero dimensional polynomial ideals, with integer or
	      rational coefficients, there are functions to compute all
	      real solutions, and to compute a rational univariate
	      representation of all (complex) solutions.
	      
	      The M2 interface assumes that the binary executable is
	      named "msolve", to tell the M2 package where to find the
	      executable you should use either needsPackage("Msolve",
	      Configuration=>{"msolveBinaryFolder"=>"path_to_folder_with_msolve_binary"}),
	      or installPackage("Msolve",
	      Configuration=>{"msolveBinaryFolder"=>"path_to_folder_with_msolve_binary"})
	      once and needsPackage("Msolve") on subsequent usages;
	      here "path_to_folder_with_msolve_binary" denotes the
	      folder where your msolve executable is saved. E.g. if
	      your msolve binary (named msolve) is in a folder called
	      msolve-v0.5.0 in your home directory then you would use
	      either needsPackage("Msolve",
	      Configuration=>{"msolveBinaryFolder"=>"~/msolve-v0.5.0"})
	      or installPackage("Msolve",
	      Configuration=>{"msolveBinaryFolder"=>"~/msolve-v0.5.0"})
	      and needsPackage("Msolve") subsequently
	      
	      
	      References:
	      [1] msolve: https://msolve.lip6.fr/
	      
Node 
    Key
    	msolveGB
	(msolveGB, Ideal)
    Headline
    	Computes generators of a Groebner basis in GrevLex order.
    Usage
    	msolveGB(I)
    Inputs
    	I:Ideal
	    an ideal in a polynomial ring with GrevLex order and either rational coefficients, integer coefficients, or finite field coefficients. For a finite field the characteristic must be between 2^16 and 2^32. 
    Outputs
        GB:Matrix
	    a matrix whose columns form a Groebner basis for the input ideal I, in the GrevLex order.    
    Description 
        Text
    	    This functions uses the F4 implementation in the msolve package to compute a Groebner basis, in GrevLex order, of a polynomial ideal with either rational coefficients or finite field coefficients with characteristic between 2^16 and 2^32. If the input ideal is a polynomial ring with monomial order other than GrevLex a GrevLex basis is returned (and no warning is given). The input ideal may also be given in a ring with integer coefficients, in this case a Groebner basis for the given ideal over the rationals  will be computed, denominators will be cleared, and the output will be a Groebner basis over the rationals in GrevLex order with integer coefficients.
    	Text
	    First an example over a finite field
	Example
	    R=ZZ/1073741827[z_1..z_3]
	    I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
	    gB=msolveGB I
	    lT=monomialIdeal leadTerm gB
	    degree lT
	    dim lT	    
	Text
	    Now the same example over the rationals. 
	Example 
	    R=QQ[z_1..z_3]
	    I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
	    gB=msolveGB I
	    (ideal gB)== ideal(groebnerBasis I)
	    lT=monomialIdeal leadTerm gB
	    degree lT
	    dim lT  
Node 
    Key
    	msolveLeadMonomials
	(msolveLeadMonomials, Ideal)
    Headline
    	Computes the leading monomials of a Groebner basis in GrevLex order.
    Usage
    	msolveLeadMonomials(I)
    Inputs
    	I:Ideal
	    an ideal in a polynomial ring with GrevLex order and either rational coefficients, integer coefficients, or finite field coefficients. For a finite field the characteristic must be between 2^16 and 2^32. 
    Outputs
        GB:Matrix
	    a matrix whose columns are the leading monomials (of a Groebner basis for) the input ideal I, in the GrevLex order.    
    Description 
        Text
    	    This functions uses the F4 implementation in the msolve package to compute leading monomials via a Groebner basis, in GrevLex order, of a polynomial ideal with either rational coefficients or finite field coefficients with characteristic between 2^16 and 2^32. If the input ideal is a polynomial ring with monomial order other than GrevLex a GrevLex basis is returned (and no warning is given). The input ideal may also be given in a ring with integer coefficients, in this case a Groebner basis for the given ideal over the rationals  will be computed, denominators will be cleared, and the output will be a Groebner basis over the rationals in GrevLex order with integer coefficients.
    	Text
	    First an example over a finite field
	Example
	    R=ZZ/1073741827[z_1..z_3]
	    I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
	    lm=monomialIdeal msolveLeadMonomials I
	    degree lm
	    dim lm	    
	Text
	    Now the same example over the rationals; note over the rationals msolve first computes over a finite field and when only the leading monomials are asked for the correct leading monomials will be returned but the full Groebner basis over Q will not be computed. Hence if only degree and dimension are desired this command will often be faster that the Groeber basis command.  
	Example 
	    R=QQ[z_1..z_3]
	    I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
	    lm=monomialIdeal msolveLeadMonomials I
	    lt=monomialIdeal leadTerm groebnerBasis I
	    lm==lt
	    degree lm
	    dim lm

Node 
    Key
    	msolveRealSolutions
	(msolveRealSolutions, Ideal)
    Headline
    	Uses symbolic methods to compute a certified solution interval for each real solution to a zero dimensional polynomial system.
    Usage
    	msolveRealSolutions(I)
    Inputs
    	I:Ideal
	    a zero dimensional ideal in a polynomial ring with either rational or integer coefficients. 
    Outputs
        Sols:List
	    a list of lists, each entry in the list Sol consists of a list representing the coordinates of a solution. By default each solution coordinate value is also represented by a two element list of rational numbers, {a, b}, this means that that coordinate of the solution has a value greater than or equal to a and less than or equal to b. This interval is computed symbolically and its correctness is guaranteed by exact methods.      
    Description 
        Text
    	    This functions uses the msolve package to compute the real solutions to a zero dimensional polynomial ideal with either integer or rational coefficients. The output is a list of lists, each entry in the list Sol consists of a list representing the coordinates of a solution. By default each solution coordinate value is also represented by a two element list of rational numbers, {a, b}, this means that that coordinate of the solution has a value greater than or equal to a and less than or equal to b. This interval is computed symbolically and its correctness is guaranteed by exact methods. Note that using the option Output one may specify the output in terms of either a float interval with "floatInterval" or an average of the interval endpoints as a single float with "float".      

    	Text
	    First an example over a finite field
	Example
	    n=3
	    R=QQ[x_1..x_n]
	    f = (x_1-1)*x_1
	    g = (x_2-5/2)*(x_2-1/2)
	    h=(x_3-2)*(x_3-1)
	    I = ideal (f,g,h)
	    sols=msolveRealSolutions(I)
	    floatSolsInterval=msolveRealSolutions(I,Output=>"floatInterval")
	    floatAproxSols=msolveRealSolutions(I,Output=>"float")
	    	    
	Text
	    Note in cases where solutions have multiplicity this is not returned, but the presence of multiplicity also does not reduce accuracy or reliability in any way.   
	Example 
	    n=3
	    R=QQ[x_1..x_n]
	    f = (x_1-1)*x_1^3
	    g = (x_2-5/2)*(x_2-1/2)
	    h=(x_3-2)*(x_3-1)
	    I = ideal (f,g,h)
	    sols=msolveRealSolutions(I)
	    floatSolsInterval=msolveRealSolutions(I,Output=>"floatInterval")
	    floatAproxSols=msolveRealSolutions(I,Output=>"float")
Node 
    Key
    	msolveRUR
	(msolveRUR, Ideal)
    Headline
    	Uses symbolic methods to compute the rational univariate representation of a zero dimensional polynomial system.
    Usage
    	msolveRUR(I)
    Inputs
    	I:Ideal
	    a zero dimensional ideal in a polynomial ring with either rational or integer coefficients. 
    Outputs
        RUR:HashTable
	    a HashTable with 6 keys giving the rational univariate representation of I.     
    Description 
        Text
    	    This functions uses the msolve package to compute the rational univariate representation (RUR) of a zero dimensional polynomial ideal with either integer or rational coefficients.
	    
	    The RUR gives a parametrization for all complex solutions to the input system. For a complete definition of the RUR see the paper: Rouillier, Fabrice (1999). "Solving Zero-Dimensional Systems Through the Rational Univariate Representation". Appl. Algebra Eng. Commun. Comput. 9 (9): 433–461.
	    
	    If I is a zero dimensional ideal in QQ[x_1..x_n] then the RUR is given by:
	    
	    (x_1,..,x_n)={ (-v_1(T)/w'(T), .. , -v_n(T)/w'(T)) | w(T)=0}
	    
	    The output is a hash table with 6 keys. 
	    
	    The key "degree" is the number of solutions to I, counted with multiplicity. 
	    
	    The key "findRootsUniPoly" gives the polynomial w(T) above.
	    
	    The key "denominator"  gives the polynomial w'(T), which is the derivative of w(T) and is the denominator of each coordinate above.
	    
	    The key "numerator" gives a list {v_1(T), .. , v_n(T)} of length n above, with n the number of variables, where the polynomial v_i(T) gives the numerator of the ith coordinate.
	    
	    The key "var" gives the variable name in the univariate polynomial ring; by default this is: T.
	    
	    The key "T" gives the linear relation between the variables of the ring of I and the single variable, which is denoted T above.
	    
    	Text
	    A simple example, where the input ideal is zero dimensional and radical.
	Example
	    n=3
	    R=QQ[x_1..x_n]
	    f = (x_1-1)
	    g = (x_2-2)
	    h=(x_3^2-9)
	    I = ideal (f,g,h)
	    decompose I
	    rur=msolveRUR(I)
	    factor rur#"findRootsUniPoly"
	    sols=-1*(rur#"numerator")	    
	    denom= rur#"denominator"
	    (for s in sols list sub(s,T=>3))/sub(denom,T=>3)
	    (for s in sols list sub(s,T=>-3))/sub(denom,T=>-3)	    
	Text
	    In cases where the input ideal has dimension greater than zero an error will be returned.     

Node 
    Key
    	msolveSaturate
	(msolveSaturate, Ideal,RingElement)
    Headline
    	Computes a Groebner basis for the saturation of an ideal by a single polynomial in GrevLex order; only works over a finite field.
    Usage
    	msolveSaturate(I)
    Inputs
    	I:Ideal
	    an ideal in a polynomial ring with GrevLex order and finite field coefficients. The finite field must have characteristic between 2^16 and 2^32. 
	f:RingElement
	    a polynomial in the same ring as I.    
    Outputs
        GB:Matrix
	    a matrix whose columns form a Groebner basis for the ideal I:f^\infty, in the GrevLex order.    
    Description 
        Text
    	    This functions uses the saturation implementation in the msolve package to compute a Groebner basis, in GrevLex order, of I:f^\infty, that is of the saturation of the ideal I by the principal ideal generated by the polynomial f.
    	Text
	    First an example; note the ring must be a polynomial ring over a finite field
	Example
	    R=ZZ/1073741827[z_1..z_3]
	    I=ideal(z_1*(z_2^2-17*z_1-z_3^3),z_1*z_2)
	    satMsolve=ideal msolveSaturate(I,z_1)
	    satM2=saturate(I,z_1)	    	    
Node 
    Key
    	msolveEliminate
	(msolveEliminate, Ideal,List)
	(msolveEliminate, List,Ideal)
	(msolveEliminate, Ideal,RingElement)
    Headline
    	Computes the elimination ideal of a given ideal.
    Usage
    	msolveEliminate(I,elimVars)
    Inputs
    	I:Ideal
	    an ideal in a polynomial ring with rational or finite field coefficients. If working over a finite field, it must have characteristic between 2^16 and 2^32. 
	elimVars:List
	    a list of variables in the same ring as I, these variables will be eliminated.    
    Outputs
        GB:Matrix
	    a matrix whose columns are generators for the elimination ideal.    
    Description 
        Text
            This function takes as input a polynomial ideal and
            computes the elimination ideal given by eliminating the
            variables specified in the inputted list.

            The behavior is very different over finite (prime) fields, and the rationals.
            Over QQ, the subideal over a smaller set of variables eliminating the given ones is returned.
            Over a finite field, the Groebner basis in the product order eliminating the given block of variables
            is returned (warning: this is a copy of the ring with (potentially) permuted variables.
        Text
	    First an example over the rationals. 
	Example
	    R = QQ[x,a,b,c,d]
	    f = 7*x^2+a*x+b
	    g = 2*x^2+c*x+d
	    M2elim=eliminate(x,ideal(f,g))
	    Msolveelim=msolveEliminate(x,ideal(f,g))
	    sub(M2elim,ring Msolveelim)==Msolveelim    
	Text
            We can also work over a finite field. Here we get the full
            Groebner basis in the permuted variables with a block
            order.
        Example 
	    R = ZZ/1073741827[x,y,a,b,c,d]
	    f = c*x^2+a*x+b*y^2+d*x*y+y
	    g =diff(x,f)
	    h=diff(y,f)
	    M2elim=eliminate({x,y},ideal(f,g,h))
	    Msolveelim=msolveEliminate({x,y},ideal(f,g,h))
	    M2elim_0 == sub(Msolveelim_0, R)
///	      

-*
    uninstallPackage "Msolve"
    restart
    needsPackage("Msolve")
    installPackage "Msolve"
    restart
    check "Msolve"
*-

TEST ///
R=ZZ/1073741827[z_1..z_3]
I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
gB=msolveGB I
lT=monomialIdeal leadTerm gB
degree lT
dim lT
R=QQ[z_1..z_3]
I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
gB=msolveGB I
(ideal gB)== ideal(groebnerBasis I)
lT=monomialIdeal leadTerm gB
degree lT
dim lT
///
	      
TEST ///
R=QQ[x,y];
n=2;
I = ideal ((x-3)*(x^2+1),y-1);
sols=msolveRealSolutions(I,Output=>"float");
assert(sols=={{3.0,1.0}});
///

TEST ///
S = ZZ/1073741827[t12,t13,t14,t23,t24,t34];
I = ideal((t13*t12-t23)*(1-t14)+(t14*t12-t24)*(1-t13) - (t12+1)*(1-t13)*(1-t14), (t23*t12-t13)*(1-t24)+(t24*t12-t14)*(1-t23) - (t12+1)*(1-t23)*(1-t24), (t14*t34-t13)*(1-t24)+(t24*t34-t23)*(1-t14) - (t34+1)*(1-t14)*(1-t24));
sat=(1-t24);
J1= saturate(I,sat);
J2=ideal msolveSaturate(I,sat);
assert(J1==J2);
///

TEST ///
R = QQ[x,a,b,c,d];
f = x^2+a*x+b;
g = x^2+c*x+d;
eM2=eliminate(x,ideal(f,g));
eMsolve=msolveEliminate(x,ideal(f,g));
assert(eM2==sub(eMsolve,ring eM2))
///
