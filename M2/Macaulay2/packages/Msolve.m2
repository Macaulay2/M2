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
	Headline => "interface to the msolve library for solving multivariate polynomial systems using Groebner Bases",
	PackageImports => { "Elimination", "Saturation" },
    	AuxiliaryFiles => true,
	DebuggingMode => false
	)

---------------------------------------------------------------------------

export{
    "msolveGB",
    "msolveSaturate",
    "msolveEliminate",
    "msolveRUR",
    "msolveLeadMonomials",
    "msolveRealSolutions",
    "QQi",
    }

importFrom_Core { "raw", "rawMatrixReadMsolveFile" }

-- used in msolveEliminate
importFrom_Core { "monoidIndices" }
importFrom_Elimination { "eliminationRing" }

---------------------------------------------------------------------------

msolveMinimumVersion = "0.7.0"
msolveProgram = findProgram("msolve", "msolve --help",
    MinimumVersion => (msolveMinimumVersion, "msolve -V"),
    RaiseError => false,
    Verbose => debugLevel > 0)

if msolveProgram === null then (
    printerr("warning: could not find msolve with version at least v" | msolveMinimumVersion);
    -- note: msolve -h returns status code 1 :/
    msolveProgram = findProgram("msolve", "true", Verbose => debugLevel > 0))

msolveDefaultOptions = new OptionTable from {
    Threads => allowableThreads,
    Verbosity => 0,
    }

runMsolve = (mIn, mOut, args, opts) -> runProgram(msolveProgram,
    demark_" " { args,
	"-t", toString opts.Threads,
	"-v", toString opts.Verbosity,
	"-f", toString mIn,
	"-o", toString mOut },
    KeepFiles => true,
    RaiseError => true,
    Verbose => opts.Verbosity > 0)

-- e.g. turns x_(0,0)... to p_0...
toMsolveRing = I -> (
    S0 := ring I;
    S1 := first flattenRing S0;
    kk := ultimate(coefficientRing, S0);
    if not instance(S1, PolynomialRing) or instance(kk, GaloisField)
    or not isField kk or char kk > 2^31 or precision kk < infinity
    then error "msolve: expected an ideal in a polynomial ring over QQ or ZZ/p with chacteristic less than 2^31";
    -- resets the variables to p_0...
    S := newRing(S1, Variables => numgens S1);
    S, kk, substitute(I, vars S))

toMsolveString = X -> (
    elts := if instance(X, List)     then X
    else if instance(X, Ring)        then X_*
    else if instance(X, Ideal)       then X_*
    else if instance(X, RingElement) then {X};
    str := toExternalString elts;
    -- (2/5)*x -> 2/5*x
    str = replace("[)(]", "", str);
    -- {x,y,z} -> x,y,z
    str_(1, #str-2))

toMsolveInput = (S, K, I) -> demark_newline {
    toMsolveString S,
    toString char K,
    replace(",", ",\n",
	toMsolveString I)}

msolve = (S, K, I, args, opts) -> (
    tmp := temporaryFileName();
    mIn := (tmp | "-in.ms") << toMsolveInput(S, K, I) << endl << close;
    mOut := tmp | "-out.ms";
    runMsolve(mIn, mOut, args, opts);
    mOut)

use'readMsolveOutputFile := true;
readMsolveOutputFile = method()
readMsolveOutputFile(Ring,String) := Matrix => (R,mOut) -> if use'readMsolveOutputFile 
    -- TODO: this substitution should be unnecessary,
    -- but without it the result for tower rings is in the wrong order!
    then substitute(map(R, rawMatrixReadMsolveFile(raw R, mOut)), R) else (
	--the line below should be replaced by a call to the C-function to parse the string
	-- this is a hack that has global consequences (e.g. breaks rings with p_i vars)
	use newRing(R, Variables => numgens R);
	substitute(matrix {value readMsolveList get mOut}, vars R))

readMsolveList = mOutStr -> (
    mOutStr = toString stack select(lines mOutStr,
	line -> not match("#", line));
    mOutStr = replace("\\[", "{", mOutStr);
    mOutStr = replace("\\]", "}", mOutStr);
    -- e.g. 'p_0' to "p_0"
    mOutStr = replace("'", "\"",  mOutStr);
    mOutStr = first separate(":", mOutStr);
    mOutStr)

msolveGB = method(TypicalValue => Matrix, Options => msolveDefaultOptions)
msolveGB Ideal := opts -> I0 -> (
    (S, K, I) := toMsolveRing I0;
    mOut := msolve(S, K, I_*, "-g 2", opts);
    gens forceGB readMsolveOutputFile(ring I0, mOut))
-- TODO: add as a hook for gb

importFrom_Core "numallvars"
msolveLeadMonomials = method(TypicalValue => Matrix, Options => msolveDefaultOptions)
msolveLeadMonomials Ideal := opts -> I0 -> (
    -- TODO: premute the coefficient variables to make this work for tower rings
    S0 := ring I0;
    if numgens S0 =!= S0.numallvars then error "msolveLeadMonomials: unsupported tower ring";
    (S, K, I) := toMsolveRing I0;
    mOut := msolve(S, K, I_*, "-g 1", opts);
    gens forceGB readMsolveOutputFile(ring I0, mOut))
-- TODO: add leadMonomials Ideal, then add this as a hook

msolveEliminate = method(Options => msolveDefaultOptions)
msolveEliminate(RingElement, Ideal) := Ideal => opts -> (elimvar,  I) -> msolveEliminate(I, {elimvar}, opts)
msolveEliminate(List,        Ideal) := Ideal => opts -> (elimvars, I) -> msolveEliminate(I,  elimvars, opts)
msolveEliminate(Ideal, RingElement) := Ideal => opts -> (I,  elimvar) -> msolveEliminate(I, {elimvar}, opts)
msolveEliminate(Ideal,        List) := Ideal => opts -> (I0, elimvars) -> (
    -- turns generators into indices, but also allows indices
    elimIndices := monoidIndices(S0 := ring I0, elimvars);
    keepvars := S0_* - set S0_*_elimIndices;
    -- gives ring maps to and from a ring with elimvars first, keepvars last
    (toS0', toS0) := eliminationRing(elimIndices, S0);
    (S, K, I) := toMsolveRing(I' := toS0' I0);
    mOut := msolve(S, K, I_*, "-g 2 -e " | length elimIndices, opts);
    S' := if char K === 0
    then K(monoid [keepvars]) -- msolve does not return the remaining generators over QQ
    else newRing(ring I', MonomialOrder => {#elimvars, #keepvars}); -- but over ZZ/p it does
    ideal readMsolveOutputFile(S', mOut))
-- TODO: add as a hook for eliminate

msolveSaturate = method(TypicalValue => Matrix, Options => msolveDefaultOptions)
msolveSaturate(Ideal, RingElement) := opts -> (I0, f0) -> (
    (S, K, I) := toMsolveRing I0;
    f := substitute(f0, vars S);
    -- see https://github.com/algebraic-solving/msolve/issues/165
    if char K < 2^16 or 2^31 < char K then error "msolveSaturate: expected characteristic between 2^16 and 2^31 for F4SAT";
    -- msolve expects a list of the generators of the ideal followed by f
    mOut := msolve(S, K, I_* | {f}, "-S -g 2", opts);
    gens forceGB readMsolveOutputFile(ring I0, mOut))
addHook((saturate, Ideal, RingElement), Strategy => Msolve,
    -- msolveSaturate doesn't use any options of saturate, like DegreeLimit, etc.
    (opts, I, f) -> try ideal msolveSaturate(I, f))

--------------------------------------------------------------------------------
-- Rational interval type, constructors, and basic methods
--------------------------------------------------------------------------------

QQi = new Ring of List -- TODO: array looks better, but List implements arithmetic by default!
QQi.synonym = "rational interval"

ring      QQi := x -> QQi
precision QQi := x -> infinity

QQinterval = method(TypicalValue => QQi)
QQinterval VisibleList := bounds -> (
    if #bounds == 2 then QQinterval(bounds#0, bounds#1)
    else error "expected a lower bound and upper bound")
QQinterval Number                        := midpt  -> QQinterval(midpt/1, midpt/1)
QQinterval InexactNumber                 := midpt  -> QQinterval lift(midpt, QQ)
QQinterval(InexactNumber, InexactNumber) := (L, R) -> QQinterval(lift(L, QQ), lift(R, QQ))
QQinterval(Number,        Number)        := (L, R) -> new QQi from [L/1, R/1]

-- TODO: these are compiled functions, make them methods and define for QQi
left'     = first
right'    = last
midpoint' = int -> sum int / 2
diameter QQi := x -> x#1 - x#0

interval QQi := opts -> x -> interval(x#0, x#1, opts)

QQi == Number :=
Number == QQi := (x, y) -> QQinterval x == QQinterval y

-- ZZ, QQ, RR, RRi
promote(Number, QQi) := (n, QQi) -> QQinterval n

-- ZZ, QQ
lift(QQi, Number) := o -> (x, R) -> (
    if diameter x == 0 then lift(midpoint' x, R)
    else if o.Verify then error "lift: interval has positive diameter")

--------------------------------------------------------------------------------

msolveDefaultPrecision = 32 -- alternative: defaultPrecision

msolveRealSolutions = method(TypicalValue => List, Options => msolveDefaultOptions)
msolveRealSolutions Ideal              := opt ->  I0     -> msolveRealSolutions(I0, QQi, opt)
msolveRealSolutions(Ideal, RingFamily) := opt -> (I0, F) -> msolveRealSolutions(I0, F_msolveDefaultPrecision, opt)
msolveRealSolutions(Ideal, Ring)       := opt -> (I0, F) -> (
    if not any({QQ, QQi, RR_*, RRi_*}, F' -> ancestor(F', F))
    then error "msolveRealSolutions: expected target field to be rationals, reals, or a rational or real interval field";
    (S, K, I) := toMsolveRing I0;
    -- if precision is not specified, we want to use msolve's default precision
    prec := if precision F === infinity then msolveDefaultPrecision else precision F;
    mOut := msolve(S, K, I_*, "-p " | prec, opt);
    -- format: [dim, [numlists, [ solution boxes ]]] when zero-dimensional, otherwise [1?, numgens, -1, []]
    mSeq := toSequence value readMsolveList get mOut;
    d := mSeq#0;
    if d =!= 0 then error "msolveRealSolutions: expected zero dimensional system of equations";
    solsp := mSeq#1;
    if solsp_0 > 1 then (
	printerr "msolveRealSolutions: unexpected msolve output, returning full output"; return {d, solsp});
    prec  = max(defaultPrecision, prec); -- we want the output precision to be at least defaultPrecision
    sols := apply(last solsp, sol -> apply(sol, QQinterval));
    if ancestor(QQi,   F) then sols else
    if ancestor(QQ,    F) then apply(sols, sol -> apply(sol, midpoint'))                    else
    if ancestor(RR_*,  F) then apply(sols, sol -> apply(sol, midpoint') * numeric(prec, 1)) else
    if ancestor(RRi_*, F) then apply(sols, sol -> apply(sol, range -> interval(range, Precision => prec))))

msolveRUR = method(TypicalValue => List, Options => msolveDefaultOptions)
msolveRUR Ideal := opt -> I0 ->(
    S0 := ring I0;
    (S, K, I) := toMsolveRing I0;
    mOut := msolve(S, K, I_*, "-P 2", opt);
    -- format: [dim, [char, nvars, deg, vars, form, [1, [lw, lwp, param]]]]:
    solsp := value readMsolveList get mOut;
    if first solsp != 0 then error "msolveRUR: expected zero dimensional input ideal";
    lc:=(solsp_1)_4;
    l:=sum(numgens S0,i->lc_i*S0_i);
    RUR:= new MutableHashTable;
    T:= getSymbol("T");
    S2 := K(monoid[T]);
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

load "./Msolve/tests.m2"

beginDocumentation()

doc ///
Node 
     Key
     	  Msolve
     Headline
	  Macaulay2 interface for msolve; computes real solutions and Groebner basis, etc.
     Description
     	  Text
              This package provides a Macaulay2 interface for the
	      msolve library [1] developed by
              Jérémy Berthomieu, Christian Eder, and Mohab Safey El
              Din.
	      
	      The package has functions to compute Groebner basis, in
	      @TO GRevLex@ order only, for ideals with rational or finite
	      field coefficients. Finite field characteristics must be
	      less than $2^{31}$. There are also functions to
	      compute elimination ideals, for ideals with rational or
	      finite field coefficients.
	      
	      The @TO2 {"Saturation::saturate", "saturation"}@ of an ideal by a single polynomial may be
	      computed for ideals with finite field coefficients, again
	      with characteristic less than $2^{31}$.
	      
	      For zero dimensional polynomial ideals, with integer or
	      rational coefficients, there are functions to compute all
	      real solutions, and to compute a rational univariate
	      representation of all (complex) solutions.
	      
	      The M2 interface assumes that the binary executable is
	      named "msolve" is on the executable path.

	      For all functions the option @TT "Verbosity"@ can be used.
	      It has levels 0, 1, 2. The default is 0.

	      Msolve supports parallel computations. The option @TT "Threads"@ is used to set this.
	      The default value is allowableThreads, but this can be set manually by the user when 
	      calling a function. E.g. for an ideal I:
	  Example
	      R = QQ[x,y,z]
	      I = ideal(x, y, z)
	      msolveGB(I, Verbosity => 2, Threads => 6)
    References
      [1] The msolve library: @HREF {"https://msolve.lip6.fr"}@;

Node 
    Key
    	msolveGB
       (msolveGB, Ideal)
       [msolveGB, Threads]
       [msolveGB, Verbosity]
    Headline
	compute generators of a Groebner basis in GRevLex order
    Usage
    	msolveGB(I)
    Inputs
    	I:Ideal
	    in a polynomial ring with @TO GRevLex@ order and coefficients over @TO QQ@ or
	    @TO2 {"finite fields", TT "ZZ/p"}@ in characteristic less than $2^{31}$
	Threads => ZZ -- number of processor threads to use
	Verbosity => ZZ -- level of verbosity between 0, 1, and 2
    Outputs
        GB:Matrix
	    whose columns form a Groebner basis for the input ideal I, in the GRevLex order
    Description 
        Text
	    This functions uses the F4 implementation in the msolve package to compute a Groebner basis,
	    in GRevLex order, of a polynomial ideal with either rational coefficients or finite field
	    coefficients with characteristic less than $2^{31}$. If the input ideal is a polynomial ring
	    with monomial order other than GRevLex a GRevLex basis is returned (and no warning is given).
	    The input ideal may also be given in a ring with integer coefficients, in this case a Groebner
	    basis for the given ideal over the rationals  will be computed, denominators will be cleared,
	    and the output will be a Groebner basis over the rationals in GRevLex order with integer coefficients.
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
       [msolveLeadMonomials, Threads]
       [msolveLeadMonomials, Verbosity]
    Headline
	compute the leading monomials of a Groebner basis in GRevLex order
    Usage
    	msolveLeadMonomials(I)
    Inputs
    	I:Ideal
	    in a polynomial ring with @TO GRevLex@ order and coefficients over @TO QQ@ or
	    @TO2 {"finite fields", TT "ZZ/p"}@ in characteristic less than $2^{31}$
	Threads => ZZ -- number of processor threads to use
	Verbosity => ZZ -- level of verbosity between 0, 1, and 2
    Outputs
        GB:Matrix
	    whose columns are the leading monomials (of a Groebner basis for) the input ideal I, in the GRevLex order
    Description 
        Text
	    This functions uses the F4 implementation in the msolve package to compute leading
	    monomials via a Groebner basis, in GRevLex order, of a polynomial ideal with either
	    rational coefficients or finite field coefficients with characteristic less than $2^{31}$.
	    If the input ideal is a polynomial ring with monomial order other than GRevLex a GRevLex
	    basis is returned (and no warning is given). The input ideal may also be given in a ring
	    with integer coefficients, in this case a Groebner basis for the given ideal over the
	    rationals  will be computed, denominators will be cleared, and the output will be a
	    Groebner basis over the rationals in GRevLex order with integer coefficients.
    	Text
	    First an example over a finite field
	Example
	    R=ZZ/1073741827[z_1..z_3]
	    I=ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
	    lm=monomialIdeal msolveLeadMonomials I
	    degree lm
	    dim lm	    
	Text
	    Now the same example over the rationals; note over the rationals msolve first
	    computes over a finite field and when only the leading monomials are asked for
	    the correct leading monomials will be returned but the full Groebner basis over
	    @TO QQ@ will not be computed. Hence if only degree and dimension are desired
	    this command will often be faster that the Groebner basis command.
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
        QQi
    Headline
        the class of all rational intervals
    Description
        Text
            This class is similar to the class of @TO2(RRi, "real intervals")@,
	    except that the boundaries are arbitrary precision rational numbers.
    Caveat
        Currently this class is not implemented in the interpreter,
	which means rings or matrices over rational intervals are not supported,
	and many functionalities of @TO RRi@ are not yet available.

Node 
    Key
    	msolveRealSolutions
       (msolveRealSolutions, Ideal)
       (msolveRealSolutions, Ideal, Ring)
       (msolveRealSolutions, Ideal, RingFamily)
       [msolveRealSolutions, Threads]
       [msolveRealSolutions, Verbosity]
    Headline
	compute all real solutions to a zero dimensional system using symbolic methods
    Usage
    	msolveRealSolutions(I)
	msolveRealSolutions(I, K)
    Inputs
    	I:Ideal
	    which is zero dimensional, in a polynomial ring with coefficients over @TO QQ@
	K:{Ring,RingFamily}
	    the field to find answers in, which must be one of
	    @TO QQi@ (default), @TO QQ@, @TO RR@, or @TO RRi@ (possibly with specified precision)
	Threads => ZZ -- number of processor threads to use
	Verbosity => ZZ -- level of verbosity between 0, 1, and 2
    Outputs
        :List
	    of lists; each entry in the list consists of a list representing
	    the coordinates of a solution. By default each solution coordinate value is
	    also represented by a @TO2(QQi, "rational interval")@ consisting of a two element
	    list of rational numbers, @TT "{a, b}"@, this means that that coordinate of the
	    solution has a value greater than or equal to @TT "a"@ and less than or equal to @TT "b"@.
	    This interval is computed symbolically and its correctness is guaranteed by exact methods.
    Description 
        Text
	    This functions uses the msolve package to compute the real solutions to a zero
	    dimensional polynomial ideal with either integer or rational coefficients.
	Text
	    The second input is optional, and indicates the alternative ways to provide output
	    either using an exact rational interval @TO QQi@, a real interval @TO RRi@,
	    or by taking a rational or real approximation of the midpoint of the intervals.
	Example
	    R = QQ[x,y]
	    I = ideal {(x-1)*x, y^2-5}
	    rationalIntervalSols = msolveRealSolutions I
	    rationalApproxSols = msolveRealSolutions(I, QQ)
	    floatIntervalSols = msolveRealSolutions(I, RRi)
	    floatIntervalSols = msolveRealSolutions(I, RRi_10)
	    floatApproxSols = msolveRealSolutions(I, RR)
	    floatApproxSols = msolveRealSolutions(I, RR_10)
	Text
	    Note in cases where solutions have multiplicity this is not reflected in the output.
	    While the solver does not return multiplicities,
	    it reliably outputs the verified isolating intervals for multiple solutions.  
	Example 
	    I = ideal {(x-1)*x^3, (y^2-5)^2}
	    floatApproxSols = msolveRealSolutions(I, RRi)
Node 
    Key
    	msolveRUR
       (msolveRUR, Ideal)
       [msolveRUR, Threads]
       [msolveRUR, Verbosity]
    Headline
	compute the rational univariate representation using symbolic methods
    Usage
    	msolveRUR(I)
    Inputs
    	I:Ideal
	    which is zero dimensional, in a polynomial ring with coefficients over @TO QQ@
	Threads => ZZ -- number of processor threads to use
	Verbosity => ZZ -- level of verbosity between 0, 1, and 2
    Outputs
        RUR:HashTable
	    with 6 keys giving the rational univariate representation of I
    Description 
        Text
	    This functions uses the msolve package to compute the rational univariate representation
	    (RUR) of a zero dimensional polynomial ideal with either integer or rational coefficients.
	    
	    The RUR gives a parametrization for all complex solutions to the input system.
	    For a complete definition of the RUR see the paper: Rouillier, Fabrice (1999).
	    "Solving Zero-Dimensional Systems Through the Rational Univariate Representation".
	    Appl. Algebra Eng. Commun. Comput. 9 (9): 433–461.
	    
	    If I is a zero dimensional ideal in QQ[x_1..x_n] then the RUR is given by:
	    
	    (x_1,..,x_n)={ (-v_1(T)/w'(T), .. , -v_n(T)/w'(T)) | w(T)=0}
	    
	    The output is a hash table with 6 keys. 
	    
	    The key "degree" is the number of solutions to I, counted with multiplicity. 
	    
	    The key "findRootsUniPoly" gives the polynomial w(T) above.
	    
	    The key "denominator"  gives the polynomial w'(T), which is the derivative of w(T) and
	    is the denominator of each coordinate above.
	    
	    The key "numerator" gives a list {v_1(T), .. , v_n(T)} of length n above, with n the
	    number of variables, where the polynomial v_i(T) gives the numerator of the ith coordinate.
	    
	    The key "var" gives the variable name in the univariate polynomial ring; by default this is: "T".
	    
	    The key "T" gives the linear relation between the variables of the ring of I and
	    the single variable, which is denoted T above.
	    
    	Text
	    A simple example, where the input ideal is zero dimensional and radical.
	Example
	    R = QQ[x_1..x_3]
	    f = (x_1-1)
	    g = (x_2-2)
	    h = (x_3^2-9)
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
       [msolveSaturate, Threads]
       [msolveSaturate, Verbosity]
    Headline
	compute a Groebner basis for the saturation of an ideal by a single polynomial in GRevLex order
    Usage
    	msolveSaturate(I)
    Inputs
    	I:Ideal
	    in a polynomial ring with @TO GRevLex@ order and coefficients over
	    @TO2 {"finite fields", TT "ZZ/p"}@ in characteristic more than $2^16$ and less than $2^{31}$
	f:RingElement
	    a polynomial in the same ring as I.    
	Threads => ZZ -- number of processor threads to use
	Verbosity => ZZ -- level of verbosity between 0, 1, and 2
    Outputs
        GB:Matrix
	    whose columns form a Groebner basis for the ideal $I:f^\infty$, in the GRevLex order
    Description 
        Text
	    This functions uses the F4SAT algorithm implemented in the msolve library to compute a
	    Groebner basis, in GRevLex order, of $I:f^\infty$, that is of the saturation of the
	    ideal $I$ by the principal ideal generated by the polynomial $f$.
	Example
	    R=ZZ/1073741827[z_1..z_3]
	    I=ideal(z_1*(z_2^2-17*z_1-z_3^3),z_1*z_2)
	    satMsolve=ideal msolveSaturate(I,z_1)
	    satM2=saturate(I,z_1)
	Text
	    Note that the ring must be a polynomial ring over a finite field.
    Caveat
	Currently the F4SAT algorithm is only implemented over prime fields in characteristic between $2^{16}$ and $2^{31}$.

Node 
    Key
    	msolveEliminate
	(msolveEliminate, Ideal,List)
	(msolveEliminate, List,Ideal)
	(msolveEliminate, Ideal,RingElement)
	(msolveEliminate, RingElement,Ideal)
       [msolveEliminate, Threads]
       [msolveEliminate, Verbosity]
    Headline
	compute the elimination ideal of a given ideal
    Usage
    	msolveEliminate(I,elimVars)
    Inputs
    	I:Ideal
	    in a polynomial ring with @TO GRevLex@ order and coefficients over
	    @TO2 {"finite fields", TT "ZZ/p"}@ in characteristic less than $2^{31}$
	elimVars:List
	    of variables in the same ring as @TT "I"@, these variables will be eliminated
	Threads => ZZ -- number of processor threads to use
	Verbosity => ZZ -- level of verbosity between 0, 1, and 2
    Outputs
        GB:Matrix
	    whose columns are generators for the elimination ideal
    Description 
        Text
            This function takes as input a polynomial ideal and
            computes the elimination ideal given by eliminating the
            variables specified in the inputted list.

            The behavior is very different over finite (prime) fields, and the rationals.
	    Over @TO QQ@, the subideal over a smaller set of variables eliminating the given ones is returned.
            Over a finite field, the Groebner basis in the product order eliminating the given block of variables
	    is returned (warning: this is a copy of the ring with potentially permuted variables).
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
            Groebner basis in the permuted variables with a block order.
        Example 
	    R = ZZ/1073741827[x,y,a,b,c,d]
	    f = c*x^2+a*x+b*y^2+d*x*y+y
	    g = diff(x,f)
	    h = diff(y,f)
	    M2elim = eliminate({x,y}, ideal(f,g,h))
	    Msolveelim = msolveEliminate({x,y}, ideal(f,g,h))
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
