newPackage("Padic",
    Headline => "p-adic numbers",
    Version => "0.1",
    Date => "April 28, 2026",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Algebraic Number Theory"},
    PackageExports => {"Valuations"},
    PackageImports => {"ForeignFunctions"})

endpkg = msg -> (
    document {Key => Padic,
	Headline => (options currentPackage).Headline,
	"Warning: Padic was loaded without key components."};
    printerr("warning: ", msg, "; ending");
    end)

if not ForeignFunctions#"private dictionary"#?"foreignFunction"
then endpkg "foreign function interface is not available"

flint = try openSharedLibrary "flint" else endpkg "flint is not available"

export {
    -- methods
    "prime",
    "pVal",
    "teichmullerLift",
    "unit",

    -- classes
    "PadicNumber",
    "PadicFieldFamily",
    }

-- unexported symbols
protect context

---------------------
-- FLINT interface --
---------------------

-- fmpz (flint integer type)
fmpzInit = foreignFunction(flint, "fmpz_init", void, voidstar)
fmpzClear = foreignFunction(flint, "fmpz_clear", void, voidstar)
fmpzSetMpz = foreignFunction(flint, "fmpz_set_mpz", void, {voidstar, mpzT})
fmpzGetMpz = foreignFunction(flint, "fmpz_get_mpz", void, {mpzT, voidstar})

toFmpz = x -> (
    -- typedef slong fmpz;
    -- typedef fmpz fmpz_t[1];
    y := getMemory size long;
    fmpzInit y;
    registerFinalizer(y, fmpzClear);
    fmpzSetMpz(y, x);
    y)

fromFmpz = x -> (
    y := mpzT 0;
    fmpzGetMpz(y, x);
    value y)

-- fmpq (flint rational type)
fmpqInit = foreignFunction(flint, "fmpq_init", void, voidstar)
fmpqClear = foreignFunction(flint, "fmpq_clear", void, voidstar)
fmpqSetFmpzFrac = foreignFunction(flint, "fmpq_set_fmpz_frac", void,
    {voidstar, voidstar, voidstar})
fmpqGetMpzFrac = foreignFunction(flint, "fmpq_get_mpz_frac", void,
    {mpzT, mpzT, voidstar})

toFmpq = x -> (
    -- typedef struct {
    --     fmpz num;
    --     fmpz den;
    -- } fmpq;
    y := getMemory(2 * size long);
    fmpqInit y;
    registerFinalizer(y, fmpqClear);
    fmpqSetFmpzFrac(y, toFmpz numerator x, toFmpz denominator x);
    y)

fromFmpq = x -> (
    (a, b) := (mpzT 0, mpzT 1);
    fmpqGetMpzFrac(a, b, x);
    value a / value b)

-- padic_ctx_t
padicCtxInit = foreignFunction(flint, "padic_ctx_init", void,
    {voidstar, voidstar, long, long, int})
padicCtxClear = foreignFunction(flint, "padic_ctx_clear", void, voidstar)

newPadicContext = memoize((p, N) -> (
	-- typedef struct {
	--     fmpz_t p;
	--     double pinv;
	--     fmpz *pow;
	--     slong min;
	--     slong max;
	--     enum padic_print_mode mode;
	-- } padic_ctx_struct;
	ctx := getMemory(4 * size long + size double + size int);
	m := max(0, N - 10);
	M := max(0, N + 10);
	padicCtxInit(ctx, toFmpz p, m, M, 1 -* PADIC_SERIES *-);
	registerFinalizer(ctx, padicCtxClear);
	ctx))

-- padic_t
padicInit2 = foreignFunction(flint, "padic_init2", void, {voidstar, long})
padicClear = foreignFunction(flint, "padic_clear", void, voidstar)
padicUnit = foreignFunction(flint, "padic_unit", voidstar, voidstar)
padicGetVal = foreignFunction(flint, "padic_get_val", long, voidstar)
padicGetPrec = foreignFunction(flint, "padic_get_prec", long, voidstar)
padicSetFmpq = foreignFunction(flint, "padic_set_fmpq", void,
    {voidstar, voidstar, voidstar})
padicGetFmpz = foreignFunction(flint, "padic_get_fmpz", void,
    {voidstar, voidstar, voidstar})
padicGetFmpq = foreignFunction(flint, "padic_get_fmpq", void,
    {voidstar, voidstar, voidstar})

newPadic = N -> (
    -- typedef struct {
    --  fmpz u;
    --  slong v;
    --  slong N;
    -- } padic_struct;
    y := getMemory(3 * size long);
    padicInit2(y, N);
    registerFinalizer(y, padicClear);
    y)

padicGetStr = foreignFunction(flint, "padic_get_str", charstar,
    {charstar, voidstar, voidstar})

padicAdd = foreignFunction(flint, "padic_add", void,
    {voidstar, voidstar, voidstar, voidstar})

padicSub = foreignFunction(flint, "padic_sub", void,
    {voidstar, voidstar, voidstar, voidstar})

padicMul = foreignFunction(flint, "padic_mul", void,
    {voidstar, voidstar, voidstar, voidstar})

padicDiv = foreignFunction(flint, "padic_div", void,
    {voidstar, voidstar, voidstar, voidstar})

padicShift = foreignFunction(flint, "padic_shift", void,
    {voidstar, voidstar, long, voidstar})

padicNeg = foreignFunction(flint, "padic_neg", void,
    {voidstar, voidstar, voidstar})

padicInv = foreignFunction(flint, "padic_inv", void,
    {voidstar, voidstar, voidstar})

padicSqrt = foreignFunction(flint, "padic_sqrt", int,
    {voidstar, voidstar, voidstar})

padicPowSi = foreignFunction(flint, "padic_pow_si", void,
    {voidstar, voidstar, long, voidstar})

padicExp = foreignFunction(flint, "padic_exp", int,
    {voidstar, voidstar, voidstar})

padicLog = foreignFunction(flint, "padic_log", int,
    {voidstar, voidstar, voidstar})

padicTeichmuller = foreignFunction(flint, "padic_teichmuller", int,
    {voidstar, voidstar, voidstar})

padicEqual = foreignFunction(flint, "padic_equal", int, {voidstar, voidstar})

padicIsZero = foreignFunction(flint, "padic_is_zero", int, voidstar)

--------------------
-- p-adic numbers --
--------------------

PadicFieldFamily = new Type of RingFamily
PadicFieldFamily.synonym = "p-adic field family"

expression PadicFieldFamily := kk -> Subscript(QQ, prime kk)
net PadicFieldFamily := net @@ expression
toString PadicFieldFamily := toString @@ expression

PadicNumber = new Type of Number
PadicNumber.synonym = "p-adic number"

precision PadicNumber := x -> value padicGetPrec x.number

unit = method()
unit PadicNumber := x -> fromFmpz padicUnit x.number

padicValuation PadicFieldFamily :=
valuation PadicFieldFamily      := kk -> kk.valuation

pVal = method()
pVal PadicNumber := x -> (valuation class x) x

prime = method()
prime PadicFieldFamily := kk -> kk.prime
prime PadicNumber := x -> prime class x

numdigits := x -> floor log(10, x) + 1
toString PadicNumber := x -> (
    (N, v, p) := (precision x, pVal x, prime x);
    if v == infinity then v = 0;
    -- from src/padic/get_str.c
    n := (N - v) * (2 * numdigits p + numdigits max(abs v, abs N) + 5) + 1;
    value padicGetStr(concatenate(n:"\0"), x.number, x.context))

PadicNumber.AfterPrint = lookup(AfterPrint, InexactNumber)

peek'(ZZ, PadicNumber) := lookup(peek', ZZ, HashTable)

describe PadicNumber := x -> describe(unit x * Power(prime x, pVal x))

knownPadicFields = new MutableHashTable

-- want to use QQ_p, but Ring_ZZ already exists, so overwrite it
oldRingSubZZ = lookup(symbol _, Ring, ZZ)
Ring _ ZZ := (R, p) -> (
    if R =!= QQ then oldRingSubZZ(R, p)
    else (
	if not isPrime p then error "expected a prime number";
	QQp := knownPadicFields#p ??= (
	    new PadicFieldFamily of PadicNumber
	    from hashTable {symbol prime => p});
	QQp.valuation = valuation(
	    x -> (
		if x == 0 then infinity
		else value padicGetVal (QQp x).number),
	    QQp, ZZ);
	QQp))

new PadicNumber from (voidstar, voidstar) := (T, ctx, num) -> (
    new T from hashTable {
	symbol context => ctx,
	symbol number => num})
new PadicNumber from (ZZ, Number) := (T, N, x) -> (
    try x _= QQ else x ^= QQ; -- promote/lift to QQ if needed
    ctx := newPadicContext(prime T, N);
    y := newPadic N;
    padicSetFmpq(y, toFmpq x, ctx);
    new T from (ctx, y))
new PadicNumber from Number := (T, x) -> new T from (20, x)
new PadicNumber from Constant := (T, x) -> new T from numeric x
new PadicNumber from (ZZ, Constant) := (T, N, x) -> new T from (N, numeric x)
new PadicNumber from PadicNumber := (T, x) -> (
    if prime T == prime x then x
    else T(precision x, x^QQ))

PadicFieldFamily Thing := (T, x) -> new T from x

----------------
-- operations --
----------------

combinePadics = (x, y) -> (
    p := prime x;
    if p != prime y then error "expected elements of the same field";
    N := min(precision x, precision y);
    ctx := newPadicContext(p, N);
    val := newPadic N;
    QQ_p(ctx, val))

PadicNumber + PadicNumber := (x, y) -> (
    z := combinePadics(x, y);
    padicAdd(z.number, x.number, y.number, z.context);
    z)
PadicNumber + Number := (x, y) -> x + QQ_(prime x) y
Number + PadicNumber := (x, y) -> QQ_(prime y) x + y

PadicNumber - PadicNumber := (x, y) -> (
    z := combinePadics(x, y);
    padicSub(z.number, x.number, y.number, z.context);
    z)
PadicNumber - Number := (x, y) -> x - QQ_(prime x) y
Number - PadicNumber := (x, y) -> QQ_(prime y) x - y

PadicNumber * PadicNumber := (x, y) -> (
    z := combinePadics(x, y);
    padicMul(z.number, x.number, y.number, z.context);
    z)
PadicNumber * Number := (x, y) -> x * QQ_(prime x) y
Number * PadicNumber := (x, y) -> QQ_(prime y) x * y

PadicNumber / PadicNumber := (x, y) -> (
    if y == 0 then error "division by zero";
    z := combinePadics(x, y);
    padicDiv(z.number, x.number, y.number, z.context);
    z)
PadicNumber / Number := (x, y) -> x / QQ_(prime x) y
Number / PadicNumber := (x, y) -> QQ_(prime y) x / y

PadicNumber << ZZ := (x, y) -> (
    z := newPadic precision x;
    padicShift(z, x.number, y, x.context);
    QQ_(prime x)(x.context, z))

(PadicNumber >> ZZ) := (x, y) -> x << -y

-PadicNumber := x -> (
    y := newPadic precision x;
    padicNeg(y, x.number, x.context);
    QQ_(prime x)(x.context, y))

+PadicNumber := identity

abs PadicNumber := x -> (prime x)^(-pVal x)

inverse PadicNumber := x -> (
    if x == 0 then error "division by zero";
    y := newPadic precision x;
    padicInv(y, x.number, x.context);
    QQ_(prime x)(x.context, y))

sqrt PadicNumber := x -> (
    y := newPadic precision x;
    r := value padicSqrt(y, x.number, x.context);
    if r == 1 then QQ_(prime x)(x.context, y)
    else error("not a ", prime x, "-adic square"))

PadicNumber^ZZ := (x, y) -> (
    z := newPadic precision x;
    padicPowSi(z, x.number, y, x.context);
    QQ_(prime x)(x.context, z))

exp PadicNumber := x -> (
    y := newPadic precision x;
    r := value padicExp(y, x.number, x.context);
    if r == 1 then QQ_(prime x)(x.context, y)
    else error(prime x, "-adic exponential function does not converge"))

log PadicNumber := x -> (
    y := newPadic precision x;
    r := value padicLog(y, x.number, x.context);
    if r == 1 then QQ_(prime x)(x.context, y)
    else error(prime x, "-adic logarithm function does not converge"))

teichmullerLift = method()
teichmullerLift PadicNumber := x -> (
    if pVal x < 0 then error("expected a ", prime x, "-adic integer");
    y := newPadic precision x;
    padicTeichmuller(y, x.number, x.context);
    QQ_(prime x)(x.context, y))

lift(PadicNumber, ZZ) := o -> (x, kk) -> (
    if pVal x < 0 then error("expected a ", prime x, "-adic integer");
    y := toFmpz 0;
    padicGetFmpz(y, x.number, x.context);
    fromFmpz y)

lift(PadicNumber, QQ) := o -> (x, kk) -> (
    y := toFmpq(0/1);
    padicGetFmpq(y, x.number, x.context);
    fromFmpq y)

lift(PadicNumber, PadicNumber) := o -> (x, kk) -> (
    if prime x == prime kk then x
    else error("can't lift from ", toString class x, " to ", toString kk))

Number^PadicFieldFamily := (x, kk) -> lift(x, kk)

promote(ZZ, PadicNumber)          :=
promote(QQ, PadicNumber)          := (x, kk) -> kk x
promote(PadicNumber, PadicNumber) := (x, kk) -> (
    if prime x == prime kk then x
    else error("can't promote from ", toString class x, " to ", toString kk))

Number_PadicFieldFamily := (x, kk) -> promote(x, kk)

numeric PadicNumber := x -> numeric x^QQ
numeric(ZZ, PadicNumber) := (prec, x) -> numeric(prec, x^QQ)
interval PadicNumber := o -> x -> interval(x^QQ, o)
interval(PadicNumber, PadicNumber) := o -> (x, y) -> interval(x^QQ, y^QQ, o)
interval(PadicNumber, Number) := o -> (x, y) -> interval(x^QQ, y, o)
interval(Number, PadicNumber) := o -> (x, y) -> interval(x, y^QQ, o)

PadicNumber == PadicNumber := (x, y) -> (
    prime x == prime y and value padicEqual(x.number, y.number) == 1
    or
    -- if primes don't agree, then just compare in QQ
    x^QQ == y^QQ)

PadicNumber == Number := (x, y) -> (
    if y == 0 then value padicIsZero x.number == 1
    else x^QQ == y)
Number == PadicNumber := (x, y) -> y == x

beginDocumentation()

doc ///
  Key
    Padic
  Headline
    p-adic numbers
  Description
    Text
      The field of $p$-adic numbers $\QQ_p$ consists of all formal
      Laurent series
      $$\sum_{n=\nu}^\infty a_n p^n = a_\nu p^\nu + a_{\nu+1} p^{\nu+1} + \cdots,$$
      where $\nu \in \ZZ$ and $a_n \in \{0, \ldots, p - 1\}$, together with the
      usual operations of addition and multiplication in base $p$, with carrying
      that may continue indefinitely.  Equivalently, $\QQ_p$ is the completion
      of $\QQ$ with respect to the $p$-adic absolute value $|x|_p =
      p^{-\nu_p(x)}$, just as $\RR$ is the completion of $\QQ$ under the usual
      absolute value.

      Elements of $\QQ_p$ are created by applying @ofClass PadicFieldFamily@ to a
      rational number.  They are stored and displayed as truncated series, with a
      default @TO2((precision, PadicNumber), "precision")@ of 20 base-$p$ digits.
    Example
      x = QQ_7(12/7)
    Text
      This package is implemented using the
      @TO "ForeignFunctions::ForeignFunctions"@ package to call $p$-adic
      arithmetic routines from the @HREF("https://flintlib.org/", "FLINT")@
      C library.

      See the paper @arXiv("2604.16799",
	  "Implementing p-adic numbers in Macaulay2 using its foreign function interface and FLINT")@
      for more information.
  Citation
    @misc{torrance2026implementingpadicnumbersmacaulay2,
      title={Implementing p-adic numbers in Macaulay2 using its foreign function interface and FLINT},
      author={Douglas A. Torrance},
      year={2026},
      eprint={2604.16799},
      archivePrefix={arXiv},
      primaryClass={math.AG},
      url={https://arxiv.org/abs/2604.16799},
    }
  Subnodes
    PadicFieldFamily
    PadicNumber
///

doc ///
  Key
    PadicFieldFamily
  Headline
    class for p-adic fields
  Description
    Text
      For any prime $p$, the $p$-adic field $\QQ_p$ is represented in Macaulay2
      by the object @CODE "QQ_p"@.
    Example
      QQ_7
      class QQ_7
    Text
      Elements of $\QQ_p$ are created by applying the field to a number.
      The default @TO2((precision, PadicNumber), "precision")@ is 20 base-$p$
      digits; an explicit precision $N$ may be supplied as a first argument.
    Example
      QQ_7 3
      QQ_7(30, 3)
  Subnodes
    prime
///

undocumented {
    (expression, PadicFieldFamily),
    (net, PadicFieldFamily),
    (toString, PadicFieldFamily),
    (toString, PadicNumber),
    (peek', ZZ, PadicNumber),
    (describe, PadicNumber)}

doc ///
  Key
    PadicNumber
  Headline
    base class for p-adic numbers
  Description
    Text
      Every $p$-adic number is an instance of the class @CODE "QQ_p"@ for
      the appropriate value of @VAR "p"@.  However, each of of these classes
      is a subclass of @CODE "PadicNumber"@.
    Example
      x = QQ_7 5
      ancestors class x
    Text
      To install methods that work for all $p$-adic numbers, regardless of the
      specific field, install methods for this class.
    Example
      foo = method()
      foo PadicNumber := x -> x + 2
      foo x
  Subnodes
    (precision, PadicNumber)
    unit
    pVal
    (symbol ==, PadicNumber, PadicNumber)
    (symbol +, PadicNumber, PadicNumber)
    (symbol *, PadicNumber, PadicNumber)
    (abs, PadicNumber)
    (symbol <<, PadicNumber, ZZ)
    (symbol ^, PadicNumber, ZZ)
    (exp, PadicNumber)
    teichmullerLift
    (lift, PadicNumber, ZZ)
    (promote, ZZ, PadicNumber)
    (numeric, PadicNumber)
    (interval, PadicNumber)
///

doc ///
  Key
    (precision, PadicNumber)
  Headline
    precision of a p-adic number
  Usage
    precision x
  Inputs
    x:PadicNumber
  Outputs
    :ZZ
  Description
    Text
      Every $p$-adic number is stored with a maximum number of base $p$ digits.
      This is its precision.
    Example
      x = QQ_7 (-1)
      precision x
      y = QQ_7(30, -2)
      precision y
    Text
      When performing binary operations on $p$-adic numbers, the result has the
      smallest of the two precisions.
    Example
      x + y
      precision oo
///

doc ///
  Key
    unit
    (unit, PadicNumber)
  Headline
    unit part of a p-adic number
  Usage
    unit x
  Inputs
    x:PadicNumber
  Outputs
    :ZZ
  Description
    Text
      Every $x\in\QQ_p$ can be decomposed into a product $x=up^\nu$, where
      $u\in\ZZ_p^\times$, i.e., it is a unit in the ring of $p$-adic integers.
      This function returns $u$ as an integer.
    Example
      x = QQ_7 (1/49)
      unit x
    Text
      Note that in general, $u$ has infinitely many $p$-adic digits.  Therefore,
      it is truncated based on the @TO2((precision, PadicNumber), "precision")@
      of $x$.
    Example
      x = QQ_7(-1/49)
      unit x
///

doc ///
  Key
    (symbol +, PadicNumber, PadicNumber)
    (symbol +, PadicNumber, Number)
    (symbol +, Number, PadicNumber)
    (symbol +, PadicNumber)
  Headline
    add p-adic numbers
  Usage
    x + y
  Inputs
    x:PadicNumber
    y:PadicNumber
  Outputs
    :PadicNumber -- the sum of x and y
  Description
    Text
      Add two $p$-adic numbers.
    Example
      QQ_7 3 + QQ_7 11
    Text
      If one of the arguments is an ordinary number, it is first promoted to
      the appropriate $p$-adic field.
    Example
      QQ_7 3 + 4
      5 + QQ_7 6
    Text
      When adding two $p$-adic numbers, the result has the smaller of the two
      @TO2((precision, PadicNumber), "precision")@ values.
    Example
      QQ_7(10, 3) + QQ_7(20, 4)
    Text
      The unary @CODE "+"@ operator is the identity.
    Example
      +QQ_7 3
  Subnodes
    (symbol -, PadicNumber, PadicNumber)
///

doc ///
  Key
    (symbol -, PadicNumber, PadicNumber)
    (symbol -, PadicNumber, Number)
    (symbol -, Number, PadicNumber)
    (symbol -, PadicNumber)
  Headline
    subtract or negate p-adic numbers
  Usage
    x - y
    -x
  Inputs
    x:PadicNumber
    y:PadicNumber
  Outputs
    :PadicNumber -- the difference of x and y, or the negation of x
  Description
    Text
      Subtract two $p$-adic numbers.
    Example
      QQ_7 11 - QQ_7 3
    Text
      If one of the arguments is an ordinary number, it is first promoted to
      the appropriate $p$-adic field.
    Example
      QQ_7 11 - 3
      11 - QQ_7 3
    Text
      The unary @CODE "-"@ operator negates a $p$-adic number.
    Example
      -QQ_7 3
  SeeAlso
    (symbol +, PadicNumber, PadicNumber)
///

doc ///
  Key
    (symbol *, PadicNumber, PadicNumber)
    (symbol *, PadicNumber, Number)
    (symbol *, Number, PadicNumber)
  Headline
    multiply p-adic numbers
  Usage
    x * y
  Inputs
    x:PadicNumber
    y:PadicNumber
  Outputs
    :PadicNumber -- the product of x and y
  Description
    Text
      Multiply two $p$-adic numbers.
    Example
      QQ_7 3 * QQ_7 5
    Text
      If one of the arguments is an ordinary number, it is first promoted to
      the appropriate $p$-adic field.
    Example
      QQ_7 3 * 5
      4 * QQ_7 3
  SeeAlso
    (symbol +, PadicNumber, PadicNumber)
  Subnodes
    (symbol /, PadicNumber, PadicNumber)
    (inverse, PadicNumber)
///

doc ///
  Key
    (symbol /, PadicNumber, PadicNumber)
    (symbol /, PadicNumber, Number)
    (symbol /, Number, PadicNumber)
  Headline
    divide p-adic numbers
  Usage
    x / y
  Inputs
    x:PadicNumber
    y:PadicNumber
  Outputs
    :PadicNumber -- the quotient of x and y
  Description
    Text
      Divide two $p$-adic numbers.
    Example
      QQ_2 3 / QQ_2 2
    Text
      If one of the arguments is an ordinary number, it is first promoted to
      the appropriate $p$-adic field.
    Example
      QQ_7 6 / 2
      6 / QQ_7 2
  Caveat
    An error is raised if @VAR "y"@ is zero.
  SeeAlso
    (inverse, PadicNumber)
///

doc ///
  Key
    (inverse, PadicNumber)
  Headline
    multiplicative inverse of a p-adic number
  Usage
    inverse x
  Inputs
    x:PadicNumber
  Outputs
    :PadicNumber -- the multiplicative inverse of x
  Description
    Text
      Returns the multiplicative inverse of a $p$-adic number.
    Example
      inverse QQ_7 3
      QQ_7 3 * inverse QQ_7 3
  Caveat
    An error is raised if @VAR "x"@ is zero.
  SeeAlso
    (symbol /, PadicNumber, PadicNumber)
///

doc ///
  Key
    (abs, PadicNumber)
  Headline
    p-adic absolute value
  Usage
    abs x
  Inputs
    x:PadicNumber
  Outputs
    :QQ
  Description
    Text
      Returns the $p$-adic absolute value $|x|_p = p^{-\nu_p(x)}$.
    Example
      abs QQ_7 49
      abs QQ_7(1/7)
      abs QQ_7 0
  SeeAlso
    pVal
///

doc ///
  Key
    (symbol <<, PadicNumber, ZZ)
  Headline
    multiply a p-adic number by a power of p
  Usage
    x << n
  Inputs
    x:PadicNumber
    n:ZZ
  Outputs
    :PadicNumber -- x times p^n
  Description
    Text
      Returns $x \cdot p^n$, shifting the $p$-adic expansion left by $n$ places.
      A negative shift divides by $p^{|n|}$.
    Example
      QQ_7 3 << 2
      QQ_7 3 << -1
  Subnodes
    (symbol >>, PadicNumber, ZZ)
///

doc ///
  Key
    (symbol >>, PadicNumber, ZZ)
  Headline
    divide a p-adic number by a power of p
  Usage
    x >> n
  Inputs
    x:PadicNumber
    n:ZZ
  Outputs
    :PadicNumber -- x divided by p^n
  Description
    Text
      Returns $x / p^n = x \cdot p^{-n}$, shifting the $p$-adic expansion right
      by $n$ places.  A negative shift multiplies by $p^{|n|}$.
    Example
      QQ_7 3 >> 2
      QQ_7 3 >> -1
  SeeAlso
    (symbol <<, PadicNumber, ZZ)
///

doc ///
  Key
    (symbol ^, PadicNumber, ZZ)
  Headline
    raise a p-adic number to an integer power
  Usage
    x^n
  Inputs
    x:PadicNumber
    n:ZZ
  Outputs
    :PadicNumber -- x raised to the n-th power
  Description
    Text
      Raise a $p$-adic number to an integer power.
    Example
      (QQ_7 3)^2
      (QQ_7 3)^(-1)
  Subnodes
    (sqrt, PadicNumber)
///

doc ///
  Key
    (sqrt, PadicNumber)
  Headline
    square root of a p-adic number
  Usage
    sqrt x
  Inputs
    x:PadicNumber
  Outputs
    :PadicNumber -- a square root of x
  Description
    Text
      Returns a square root of a $p$-adic number.
    Example
      sqrt QQ_7 2
      oo^2
  Caveat
    An error is raised if @VAR "x"@ is not a $p$-adic square.
  SeeAlso
    (symbol ^, PadicNumber, ZZ)
///

doc ///
  Key
    (exp, PadicNumber)
  Headline
    p-adic exponential function
  Usage
    exp x
  Inputs
    x:PadicNumber
  Outputs
    :PadicNumber -- the p-adic exponential of x
  Description
    Text
      Returns the $p$-adic exponential of $x$, defined by the power series
      $\exp_p x = \sum_{n=0}^\infty x^n / n!$.  The series converges when
      $|x|_p < p^{-1/(p-1)}$.
    Example
      exp QQ_7 7
      log oo
  Caveat
    An error is raised if the series does not converge.
  Subnodes
    (log, PadicNumber)
///

doc ///
  Key
    (log, PadicNumber)
  Headline
    p-adic logarithm function
  Usage
    log x
  Inputs
    x:PadicNumber
  Outputs
    :PadicNumber -- the p-adic logarithm of x
  Description
    Text
      Returns the $p$-adic logarithm of $x$, defined by the power series
      $\log_p x = \sum_{n=1}^\infty (-1)^{n-1}(x-1)^n/n$.  The series converges
      when $|x - 1|_p < 1$.
    Example
      log exp QQ_7 7
  Caveat
    An error is raised if the series does not converge.
  SeeAlso
    (exp, PadicNumber)
///

doc ///
  Key
    pVal
    (pVal, PadicNumber)
  Headline
    p-adic valuation of a p-adic number
  Usage
    pVal x
  Inputs
    x:PadicNumber
  Outputs
    :ZZ
      or @TO InfiniteNumber@ if @VAR "x"@ is zero
  Description
    Text
      Returns the $p$-adic valuation $\nu_p(x)$, i.e., the exponent $\nu$ in
      the factorization $x = u p^\nu$ where $u$ is a $p$-adic unit.
    Example
      pVal QQ_7 49
      pVal QQ_7(1/7)
      pVal QQ_7 0
  SeeAlso
    unit
    (abs, PadicNumber)
///

doc ///
  Key
    prime
    (prime, PadicFieldFamily)
    (prime, PadicNumber)
  Headline
    prime of a p-adic field or number
  Usage
    prime kk
    prime x
  Inputs
    kk:PadicFieldFamily
    x:PadicNumber
  Outputs
    :ZZ
  Description
    Text
      Returns the prime $p$ of a $p$-adic field or number.
    Example
      prime QQ_7
      prime QQ_7 3
///

doc ///
  Key
    (symbol ==, PadicNumber, PadicNumber)
    (symbol ==, PadicNumber, Number)
    (symbol ==, Number, PadicNumber)
  Headline
    equality of p-adic numbers
  Usage
    x == y
  Inputs
    x:PadicNumber
    y:PadicNumber
  Outputs
    :Boolean
  Description
    Text
      Test equality of two $p$-adic numbers.  Numbers in the same field are
      compared directly.
    Example
      QQ_7 3 == QQ_7 3
      QQ_7 3 == QQ_7 4
    Text
      If the two numbers lie in different $p$-adic fields, or if one argument is
      an ordinary number, equality is tested by comparing their lifts to @TO QQ@.
    Example
      QQ_7 3 == QQ_5 3
      QQ_7 3 == 3
      3 == QQ_7 3
///

doc ///
  Key
    teichmullerLift
    (teichmullerLift, PadicNumber)
  Headline
    Teichmüller lift of a p-adic integer
  Usage
    teichmullerLift x
  Inputs
    x:PadicNumber
  Outputs
    :PadicNumber
  Description
    Text
      Returns the Teichmüller lift of $x$, which is the unique root of unity
      $t \in \ZZ_p^\times$ satisfying $t \equiv x \pmod{p}$ and $t^p = t$.
    Example
      t = teichmullerLift QQ_7 3
      t^7 == t
  Caveat
    An error is raised if $\nu_p(x) < 0$, i.e., if $x$ is not a $p$-adic integer.
///

doc ///
  Key
    (lift, PadicNumber, ZZ)
    (lift, PadicNumber, QQ)
    (lift, PadicNumber, PadicNumber)
    (symbol ^, Number, PadicFieldFamily)
    [lift, Verify]
  Headline
    lift a p-adic number to another ring
  Usage
    lift(x, R)
    x^R
  Inputs
    x:PadicNumber
    R:{ZZ, QQ, PadicNumber}
  Outputs
    :Number -- an element of @VAR "R"@
  Description
    Text
      Lift a $p$-adic number to an other ring.  This is only well-defined when
      @VAR "R"@ to @TO ZZ@, @TO QQ@, or the $p$-adic field containing @VAR "x"@.
    Example
      x = QQ_7 49
      lift(x, ZZ)
      lift(x, QQ)
      lift(x, QQ_7)
    Text
      As usual, the @TO symbol ^@ operator is a shorthand for this operation.
    Example
      x^ZZ
      x^QQ
      x^(QQ_7)
  Caveat
    Lifting to @TO ZZ@ raises an error if the $p$-adic valuation of @VAR "x"@
    is negative.
  SeeAlso
    (promote, ZZ, PadicNumber)
///

doc ///
  Key
    (promote, ZZ, PadicNumber)
    (promote, QQ, PadicNumber)
    (promote, PadicNumber, PadicNumber)
    (symbol _, Number, PadicFieldFamily)
  Headline
    promote a number to a p-adic field
  Usage
    promote(x, kk)
    x_kk
  Inputs
    x:{ZZ, QQ, PadicNumber}
    kk:PadicNumber
  Outputs
    :PadicNumber
  Description
    Text
      Promote an integer, rational, or $p$-adic number to the given $p$-adic
      field.  The subscript operator @TO symbol _@ may also be used.
    Example
      promote(3, QQ_7)
      promote(3/2, QQ_7)
      3_(QQ_7)
      (3/2)_(QQ_7)
  SeeAlso
    (lift, PadicNumber, ZZ)
///

doc ///
  Key
    (numeric, ZZ, PadicNumber)
    (numeric, PadicNumber)
  Headline
    convert a p-adic number to a real number
  Usage
    numeric(prec, x)
    numeric x
  Inputs
    prec:ZZ -- number of bits of precision
    x:PadicNumber
  Outputs
    :RR
  Description
    Text
      Converts a $p$-adic number to a floating-point real number by first
      lifting to @TO QQ@.
    Example
      numeric QQ_7 5
      numeric(100, QQ_7 5)
  SeeAlso
    (interval, PadicNumber)
    (lift, PadicNumber, ZZ)
///

doc ///
  Key
    (interval, PadicNumber, PadicNumber)
    (interval, PadicNumber, Number)
    (interval, Number, PadicNumber)
    (interval, PadicNumber)
    [interval, Precision]
  Headline
    convert p-adic numbers to a real interval
  Usage
    interval(x, y)
    interval x
  Inputs
    x:{PadicNumber, Number}
    y:{PadicNumber, Number}
  Outputs
    :RRi
  Description
    Text
      Converts a $p$-adic number to a real interval by first lifting to @TO QQ@.
      When two arguments are given, returns the interval with those endpoints.
    Example
      interval QQ_7 5
      interval(QQ_7 5, QQ_7 6)
      interval(QQ_7 5, 6)
      interval(5, QQ_7 6)
  SeeAlso
    (numeric, PadicNumber)
///

TEST ///
assert Equation(net QQ_7, "QQ\n  7"^0)
assert Equation(toString QQ_7, "QQ_7")
assert Equation(toString QQ_7(12/7), "5*7^-1 + 1")
///

TEST ///
assert Equation(pVal QQ_7 49, 2)
assert Equation(pVal QQ_7(1/7), -1)
assert Equation(pVal QQ_7 3, 0)
assert Equation(pVal QQ_7 0, infinity)
assert Equation(unit QQ_7 49, 1)
assert Equation(unit QQ_7(12/7), 12)
assert Equation(precision QQ_7 49, 20)
assert Equation(precision QQ_7(30, 49), 30)
assert Equation(prime QQ_7, 7)
assert Equation(prime QQ_7 3, 7)
assert Equation(abs QQ_7 49, 1/49)
assert Equation(abs QQ_7(1/7), 7)
assert Equation(abs QQ_7 3, 1)
///

TEST ///
assert Equation(QQ_7 3 + QQ_7 2, QQ_7 5)
assert Equation(QQ_7 3 + 4, QQ_7 7)
assert Equation(4 + QQ_7 3, QQ_7 7)
assert Equation(QQ_7 3 - QQ_7 2, QQ_7 1)
assert Equation(QQ_7 5 - 3, QQ_7 2)
assert Equation(3 - QQ_7 5, QQ_7(-2))
assert Equation(QQ_7 3 * QQ_7 2, QQ_7 6)
assert Equation(QQ_7 3 * 4, QQ_7 12)
assert Equation(4 * QQ_7 3, QQ_7 12)
assert Equation(QQ_2 3 / QQ_2 2, QQ_2(3/2))
assert Equation(QQ_7 6 / 3, QQ_7 2)
assert Equation(6 / QQ_7 3, QQ_7 2)
assert Equation(QQ_7 3 << 2, QQ_7(3 * 49))
assert Equation(QQ_7 3 >> 2, QQ_7(3/49))
assert Equation(-QQ_7 3 + QQ_7 3, 0)
assert Equation(+QQ_7 3, QQ_7 3)
assert Equation(QQ_7 3 * inverse QQ_7 3, QQ_7 1)
assert Equation((QQ_7 3)^(-1), inverse QQ_7 3)
assert Equation(sqrt QQ_7 4, QQ_7 2)
assert Equation((sqrt QQ_7 2)^2, QQ_7 2)
assert Equation((QQ_7 3)^2, QQ_7 9)
assert Equation(precision(QQ_7(10, 3) + QQ_7(20, 4)), 10)
assert Equation(log exp QQ_7 7, QQ_7 7)
t = teichmullerLift QQ_7 3
assert Equation(t^7 - t, 0)
///

TEST ///
assert Equation(QQ_2 5, QQ_2 5)
assert not (QQ_2 5 == QQ_2 6)
assert Equation(QQ_2 5, QQ_3 5)
assert not (QQ_2 5 == QQ_3 6)
assert Equation(QQ_2 5, 5)
assert Equation(5, QQ_2 5)
-- since === doesn't work in QQ_p :(
is = (x, y) -> assert(class x === class y and x == y)
is(promote(5, QQ_2), QQ_2 5)
is(promote(5/2, QQ_2), QQ_2(5/2))
is(5_(QQ_2), QQ_2 5)
assert BinaryOperation(symbol ===, (QQ_2 5)^ZZ, 5)
assert BinaryOperation(symbol ===, (QQ_2 5)^QQ, 5/1)
is(lift(QQ_2 5, QQ_2), QQ_2 5)
assert BinaryOperation(symbol ===, numeric QQ_2 5, 5.0)
assert BinaryOperation(symbol ===, numeric(100, QQ_2 5), 5p100)
assert BinaryOperation(symbol ===, interval QQ_2 5, interval 5)
assert BinaryOperation(symbol ===, interval(QQ_2 5, Precision => 100), interval 5p100)
assert BinaryOperation(symbol ===, interval(QQ_2 5, QQ_2 6), interval(5, 6))
assert BinaryOperation(symbol ===, interval(QQ_2 5, 6), interval(5, 6))
assert BinaryOperation(symbol ===, interval(5, QQ_2 6), interval(5, 6))
///

TEST ///
-- error cases
checkError = (f, msg) -> (
    (ret, err) := trap f();
    assert Equation(msg, toString err))
checkError(() -> QQ_4, "expected a prime number")
checkError(() -> QQ_7 1 / QQ_7 0, "division by zero")
checkError(() -> inverse QQ_7 0, "division by zero")
checkError(() -> sqrt QQ_7 3, "not a 7-adic square")
checkError(() -> exp QQ_7 3, "7-adic exponential function does not converge")
checkError(() -> log QQ_7 3, "7-adic logarithm function does not converge")
checkError(() -> teichmullerLift QQ_7(1/7), "expected a 7-adic integer")
checkError(() -> lift(QQ_7(1/7), ZZ), "expected a 7-adic integer")
///

TEST ///
-- hensel's lemma example (cube root of 2 in QQ_5)
newton = x -> x - (x^3 - 2)/(3*x^2)
x = QQ_5 3
while x != (x = newton x) do null
assert Equation(x^3, 2)
///

end

loadPackage("Padic", FileName => "~/src/macaulay2/macaulay2-padic/Padic.m2", Reload => true)
