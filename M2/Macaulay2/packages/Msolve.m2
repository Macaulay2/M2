msolvePresent := run ("type msolve >/dev/null 2>&1") === 0
-- msolveVersion := if msolvePresent then ... -- no way to get version info!!!
msolveVersionNeeded := "0.6.4"
msolvePresentAndModern := msolvePresent -- and match("^[0-9.]+$",msolveVersion) and msolveVersion >= msolveVersionNeeded

newPackage(
	"Msolve",
	Version => "1.24.05", 
    	Date => "July 2024",
    	Authors => {{Name => "Martin Helmer", 
		  Email => "mhelmer@ncsu.edu", 
		  HomePage => "http://martin-helmer.com/"}, {Name => "Mike Stillman", 
		  Email => "mike@math.cornell.edu", 
		  HomePage => "https://math.cornell.edu/michael-e-stillman"},{Name => "Anton Leykin", 
		  Email => "leykin@math.gatech.edu", 
		  HomePage => "https://antonleykin.math.gatech.edu/"}},
	  Keywords => {"Groebner Basis Algorithms" , "Interfaces"},
    	Headline => "An interface to the msolve package (https://msolve.lip6.fr/) which computes Groebner Basis and does real root isolation",
    	AuxiliaryFiles => true,
	DebuggingMode => false,       
	OptionalComponentsPresent => msolvePresent
	);

---------------------------------------------------------------------------

export{
    "msolveGB",
    "msolveSaturate",
    "msolveEliminate",
    "msolveRUR",
    "msolveLeadMonomials",
    "msolveRealSolutions"
    }

importFrom_Core { "raw", "rawMatrixReadMsolveFile" }

---------------------------------------------------------------------------

msolve = (mIn, mOut, args, opts) -> (
    mCmd := demark_" " { "msolve", args,
	"-t", toString opts#"number of threads",
	"-v", toString opts.Verbosity,
	"-f", mIn, "-o", mOut };
    if opts.Verbosity > 0 then (
	printerr "msolve command:";
	-- not commented for ease of copying
	-- and running manually in terminal:
	stderr << "\t" << mCmd << endl);
    mRet := run mCmd;
    if mRet =!= 0 then error("msolve exited with non-zero exit code ", mRet);
    mRet)

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
    if not msolvePresentAndModern then (error "msolve (version > 0.6.4) is not installed in system"; return false;);
    R:=ring I;
    if not instance(R,PolynomialRing) then (print "input must be in a polynomial ring over a field";return false;);
    kk:=coefficientRing(R);
    if (instance(kk,InexactFieldFamily) or instance(kk,InexactField)) then(
        print "input must be over the rationals or a (prime) finite field of chacteristic less than 2^32";
	return false;
	);
    if char(kk)>0 then (
	if (char(kk)>2^32) then(
	    print "input must be over the rationals or a (prime) finite field of chacteristic less than 2^32";
	    return false;
	    );
	);
    return true;
    );
msolveGB=method(TypicalValue=>Matrix, Options=>{Verbosity=>0, "number of threads"=>allowableThreads});
msolveGB(Ideal):=opt->I->(
    if not inputOkay(I) then error "Problem with input, please refer to documentation.";
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
    msolve(mIn, mOut, "-g 2", opt);
    msolGB:=readMsolveOutputFile(R, mOut);
    return gens forceGB msolGB;
    );
msolveLeadMonomials=method(TypicalValue=>Matrix, Options=>{Verbosity=>0, "number of threads"=>allowableThreads});
msolveLeadMonomials(Ideal):=opt->(I)->(
    if not inputOkay(I) then error "Problem with input, please refer to documentation.";
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
    msolve(mIn, mOut, "-g 1", opt);
    msolGB:=readMsolveOutputFile(R, mOut);
    return gens forceGB msolGB;
    );
msolveEliminate=method(Options=>{Verbosity=>0, "number of threads"=>allowableThreads});
msolveEliminate(Ideal,RingElement):=Ideal =>opt-> (I,elimvar)->(
    return msolveEliminate(I,{elimvar});
    );
msolveEliminate(RingElement,Ideal):=Ideal => opt->(elimvar,I) ->
    msolveEliminate(I,{elimvar})
msolveEliminate(List,Ideal):=Ideal => opt->(elimvars,I) -> 
    msolveEliminate(I,elimvars)
msolveEliminate(Ideal,List):=Ideal => opt->(J,elimvars)->(
    if not inputOkay(J) then error "Problem with input, please refer to documentation.";
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
    msolve(mIn, mOut, "-e " | toString elimNum | " -g 2", opt);
    if char R === 0 then 
      ideal readMsolveOutputFile(elimR, mOut)
    else
      ideal readMsolveOutputFile(R, mOut)
    )

msolveSaturate=method(TypicalValue=>Matrix, Options=>{Verbosity=>0, "number of threads"=>allowableThreads});
msolveSaturate(Ideal,RingElement):=opt->(I,f)->(
    if not inputOkay(I) then error "Problem with input, please refer to documentation.";
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
    msolve(mIn, mOut, "-S -g 2", opt);
    msolGB:=readMsolveOutputFile(R, mOut);
    return gens forceGB msolGB;
    );

msolveRealSolutions=method(TypicalValue=>List,Options => {"output type"=>"rationalInterval",Verbosity=>0, "number of threads"=>allowableThreads});
msolveRealSolutions(Ideal):=opt->(I)->(
    if not inputOkay(I) then error "Problem with input, please refer to documentation.";
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
    msolve(mIn, mOut, "", opt);
    outStrFull:=get(mOut);
    mOutStr:=replace("[]]","}",replace("[[]","{",(separate("[:]",outStrFull))_0));
    solsp:=value(mOutStr);
    if solsp_0>0 then (error "Input ideal not zero dimensional, no solutions found.";);
    if (solsp_1)_0>1 then (print "unexpected msolve output, returning full output"; return solsp;);
    sols:=(solsp_1)_1;
    if opt#"output type"=="rationalInterval" then return sols;
    if opt#"output type"=="floatInterval" then return (1.0*sols);
    if opt#"output type"=="float" then return (for s in sols list(for s1 in s list sum(s1)/2.0));
    );
msolveRUR=method(TypicalValue=>List,Options=>{Verbosity=>0, "number of threads"=>allowableThreads});
msolveRUR(Ideal):=opt->(I)->(
    if not inputOkay(I) then error "Problem with input, please refer to documentation.";
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
    msolve(mIn, mOut, "-P 2", opt);
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
     	  Macaulay2 interface for msolve; computes real solutions and Groebner basis, etc. 
     Description
     	  Text

              This package provides a Macaulay2 interface for the
              msolve package (https://msolve.lip6.fr/) developed by
              Jérémy Berthomieu, Christian Eder, and Mohab Safey El
              Din.
	      
	      The package has functions to compute Groebner basis, in
	      GRevLex order only, for ideals with rational or finite
	      field coefficients. Finite field characteristics must be
	      less than 2^32. There are also functions to
	      compute elimination ideals, for ideals with rational or
	      finite field coefficients.
	      
	      The saturation of an ideal by a single polynomial may be
	      computed for ideals with finite field coefficients, again
	      with characteristic less than 2^32.
	      
	      For zero dimensional polynomial ideals, with integer or
	      rational coefficients, there are functions to compute all
	      real solutions, and to compute a rational univariate
	      representation of all (complex) solutions.
	      
	      The M2 interface assumes that the binary executable is
	      named "msolve" is on the executable path.

	      
	      Rings with double subscript variables are NOT supported,
	      i.e. QQ[x_(1,1)..x_(1,3)] will NOT work. 
	      You should use rings with single subscript, e.g., QQ[x_1..x_5],
	      or rings with some characters as variables, e.g. QQ[a..d] or QQ[aa,bcd,x1] etc.

	      For all functions the option Verbosity can be used. It has levels 0, 1, 2. The default is 0. 

	      Msolve supports parallel computations. The option "number of threads" is used to set this. 
	      The default value is allowableThreads, but this can be set manually by the user when 
	      calling a function. E.g. for an ideal I:
	      msolveGB(I, Verbosity=>2, "number of threads"=>6)
	      
	      
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
	    an ideal in a polynomial ring with GrevLex order and either rational coefficients, integer coefficients, or finite field coefficients. For a finite field the characteristic must be less than 2^32. 
    Outputs
        GB:Matrix
	    a matrix whose columns form a Groebner basis for the input ideal I, in the GrevLex order.    
    Description 
        Text
    	    This functions uses the F4 implementation in the msolve package to compute a Groebner basis, in GrevLex order, of a polynomial ideal with either rational coefficients or finite field coefficients with characteristic less than 2^32. If the input ideal is a polynomial ring with monomial order other than GrevLex a GrevLex basis is returned (and no warning is given). The input ideal may also be given in a ring with integer coefficients, in this case a Groebner basis for the given ideal over the rationals  will be computed, denominators will be cleared, and the output will be a Groebner basis over the rationals in GrevLex order with integer coefficients.
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
	    an ideal in a polynomial ring with GrevLex order and either rational coefficients, integer coefficients, or finite field coefficients. For a finite field the characteristic must be less than 2^32. 
    Outputs
        GB:Matrix
	    a matrix whose columns are the leading monomials (of a Groebner basis for) the input ideal I, in the GrevLex order.    
    Description 
        Text
    	    This functions uses the F4 implementation in the msolve package to compute leading monomials via a Groebner basis, in GrevLex order, of a polynomial ideal with either rational coefficients or finite field coefficients with characteristic less than 2^32. If the input ideal is a polynomial ring with monomial order other than GrevLex a GrevLex basis is returned (and no warning is given). The input ideal may also be given in a ring with integer coefficients, in this case a Groebner basis for the given ideal over the rationals  will be computed, denominators will be cleared, and the output will be a Groebner basis over the rationals in GrevLex order with integer coefficients.
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
    	Uses symbolic methods to compute all real solutions to a zero dim systems.
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
	    floatSolsInterval=msolveRealSolutions(I,"output type"=>"floatInterval")
	    floatAproxSols=msolveRealSolutions(I,"output type"=>"float")
	    	    
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
	    floatSolsInterval=msolveRealSolutions(I,"output type"=>"floatInterval")
	    floatAproxSols=msolveRealSolutions(I,"output type"=>"float")
Node 
    Key
    	msolveRUR
	(msolveRUR, Ideal)
    Headline
    	Uses symbolic methods to compute the rational univariate representation.
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
	    an ideal in a polynomial ring with GrevLex order and finite field coefficients. The finite field must have characteristic less than 2^32. 
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
	(msolveEliminate, RingElement,Ideal)
    Headline
    	Computes the elimination ideal of a given ideal.
    Usage
    	msolveEliminate(I,elimVars)
    Inputs
    	I:Ideal
	    an ideal in a polynomial ring with rational or finite field coefficients. If working over a finite field, it must have characteristic less than 2^32. 
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

load "./Msolve/EXA/Msolve-testing.m2"
