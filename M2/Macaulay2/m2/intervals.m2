needs "methods.m2"
needs "shared.m2"
needs "reals.m2"

interval = method(Options => {Precision => -1})

interval Number := opts -> N -> (
    p := if opts.Precision < 0 then defaultPrecision else opts.Precision;
    interval(numeric(p, N), opts))
interval RR := opts -> N -> (
    if opts.Precision < 0 or opts.Precision == precision N then toRRi N
    else toRRi(opts.Precision, N, N))
interval RRi := opts -> N -> (
    if opts.Precision < 0 or opts.Precision == precision N then N
    else toRRi(opts.Precision, left N, right N))
interval CC := opts -> N -> (
    if opts.Precision < 0 or opts.Precision == precision N then toCCi N
    else toCCi(opts.Precision, realPart N, imaginaryPart N))
interval CCi := opts -> N -> (
    if opts.Precision < 0 or opts.Precision == precision N then N
    else toCCi(opts.Precision, realPart N, imaginaryPart N))

interval(Number, Number) := opts -> (N, M) -> (
    p := opts.Precision;
    if p < 0 then p = min(precision N, precision M);
    if isInfinite p then p = defaultPrecision;
    interval(numeric(p, N), numeric(p, M), opts))
interval(RR, RR) := opts -> (N,M) -> (
    if opts.Precision < 0 then toRRi(N,M)
    else toRRi(opts.Precision,N,M))
interval(RR, RRi) := opts -> (N,M) -> interval(toRRi N, M, opts)
interval(RRi, RR) := opts -> (N,M) -> interval(N, toRRi M, opts)
interval(RRi,RRi) := opts -> (N,M) -> (
    if opts.Precision < 0 then toCCi(N,M)
    else toCCi(opts.Precision,N,M))

interval(RR,CC) := opts -> (N, M) -> interval(toCC N, M, opts)
interval(CC,RR) := opts -> (N, M) -> interval(N, toCC M, opts)
interval(CC,CC) := opts -> (N,M) -> (
    if opts.Precision < 0
    then toCCi(
	interval(realPart N, realPart M),
	interval(imaginaryPart N, imaginaryPart M))
    else toCCi(opts.Precision,
	interval(realPart N, realPart M, opts),
	interval(imaginaryPart N, imaginaryPart M, opts)))

-- the following don't make sense
interval(RRi, CC)     :=
interval(CC, RRi)     :=
interval(CCi, Number) :=
interval(Number, CCi) := opts -> (N, M) -> error "interval not well-defined"

interval(Array) := opts -> A -> (
    if (length(A) == 0) or (length(A)>2) then error("expected length 2")
    else if length(A) == 1 then interval(opts,A_0)
    else interval(opts,A_0,A_1))    

span = method(Dispatch => Thing, Options => {Precision => -1}, Binary => true)
span Number := opts -> N -> interval(N, opts)
span(Number, Number) := opts -> (N,M) -> span(
    interval(N, opts), interval(M, opts), opts)
span(RRi, RRi) := opts -> (N,M) -> (
    if isEmpty(N) then interval(M, opts)
    else if isEmpty(M) then interval(N, opts)
    else interval(min(left N, left M), max(right N, right M), opts))
span(RRi, CCi) := opts -> (N, M) -> span(toCCi N, M, opts)
span(CCi, RRi) := opts -> (N, M) -> span(N, toCCi M, opts)
span(CCi, CCi) := opts -> (N, M) -> (
    if isEmpty N then interval(M, opts)
    else if isEmpty M then interval(N, opts)
    else interval(
	span(realPart M, realPart N, opts),
	span(imaginaryPart M, imaginaryPart N, opts),
	opts))

for A in {ZZ,QQ,RR} do
isMember(A,RRi) := (N,M) -> subsetRRi(N,M);
isMember(CC,CCi) := (N,M) -> subsetRRi(realPart N,realPart M) and subsetRRi(imaginaryPart N, imaginaryPart M);
isMember(CC,RRi) := (N,M) -> subsetRRi(realPart N,realPart M) and subsetRRi(imaginaryPart N, imaginaryPart M);

isSubset(RRi,RRi) := (N,M) -> subsetRRi(N,M);
isSubset(CCi,RRi) := (N,M) -> subsetRRi(realPart N,realPart M) and subsetRRi(imaginaryPart N,imaginaryPart M);

for A in {ZZ,QQ,RR} do
isMember(A,CCi) := (N,M) -> subsetRRi(realPart N,realPart M) and subsetRRi(imaginaryPart N,imaginaryPart M);

for A in {RRi,CCi} do
isSubset(A,CCi) := (N,M) -> subsetRRi(realPart N,realPart M) and subsetRRi(imaginaryPart N,imaginaryPart M);

-- intersect is an associative binary method, so it works on arbitrary lists and sequences
intersect RRi       := RRi => { Precision => -1 } >> opts -> identity
intersect(RRi, RRi) := RRi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then intersectRRi(N,M)
    else intersectRRi(opts.Precision,N,M))

intersect CCi       := CCi => { Precision => -1 } >> opts -> identity
intersect(CCi, RRi) := CCi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then interval(intersectRRi(realPart N,M), imaginaryPart N)
    else interval(intersectRRi(opts.Precision,realPart N,M), imaginaryPart N))
intersect(RRi, CCi) := CCi => { Precision => -1 } >> opts -> (N, M) -> intersect(M, N)
intersect(CCi, CCi) := CCi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then interval(intersectRRi(realPart N,realPart M), intersectRRi(imaginaryPart N, imaginaryPart M))
    else interval(intersectRRi(opts.Precision,realPart N, realPart M), intersectRRi(opts.Precision, imaginaryPart N, imaginaryPart M)))

CCi ? CCi := (x, y) -> (
    if (r := realPart x ? realPart y) =!= symbol == then r
    else imaginaryPart x ? imaginaryPart y)
CCi ? Number := (x, y) -> x ? toCCi numeric y
Number ? CCi := (x, y) -> toCCi numeric x ? y

-- need to define these or we get "comparison not implemented" from the
-- interpeter
ZZ  ? CCi :=
QQ  ? CCi :=
RR  ? CCi :=
RRi ? CCi :=
CC  ? CCi := (x, y) -> toCCi x ? y

isEmpty RRi := Boolean => isEmptyRRi
isEmpty CCi := x -> isEmptyRRi realPart x or isEmptyRRi imaginaryPart x

midpoint = method()
midpoint Number := identity
midpoint RRi := midpoint CCi := midpoint0

midpoint Ring := R -> (
    if instance(R, RealIntervalField) then RR_(precision R)
    else if instance(R, ComplexIntervalField) then CC_(precision R)
    else R)

intervalPolyHelper := (func, f) -> (
    R := midpoint ring f;
    if R === ring f then f
    else sum(listForm f, (m, c) -> func c * product(#m, i -> R_i^(m#i))))

midpoint   RingElement := f -> intervalPolyHelper(midpoint,   f)
left       RingElement := f -> intervalPolyHelper(left,       f)
right      RingElement := f -> intervalPolyHelper(right,      f)
lowerLeft  RingElement := f -> intervalPolyHelper(lowerLeft,  f)
lowerRight RingElement := f -> intervalPolyHelper(lowerRight, f)
upperLeft  RingElement := f -> intervalPolyHelper(upperLeft,  f)
upperRight RingElement := f -> intervalPolyHelper(upperRight, f)

toExternalString RRi := x -> "interval" | toExternalString (left x, right x)
toExternalString CCi := x -> "interval" | toExternalString (left realPart x+(left imaginaryPart x)*ii,right realPart x+(right imaginaryPart x)*ii)
