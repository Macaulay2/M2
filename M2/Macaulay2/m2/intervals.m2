needs "methods.m2"
needs "shared.m2"
needs "reals.m2"

interval = method(Options => {Precision => -1})

for A in {ZZ,QQ,RR} do
interval A := opts -> N -> (
    if opts.Precision < 0 then toRRi(N)
    else toRRi(opts.Precision,N,N))
interval CC := opts -> N -> (
    if opts.Precision < 0 then toCCi(N)
    else toCCi(opts.Precision,N,N))

for A in {ZZ,QQ,RR} do
for B in {ZZ,QQ,RR} do
interval(A,B) := opts -> (N,M) -> (
    if opts.Precision < 0 then toRRi(N,M)
    else toRRi(opts.Precision,N,M))

for A in {ZZ,QQ,RR} do
interval(A,CC) := opts -> (N,M) -> (
    if opts.Precision < 0 then toCCi(toRRi(min(N,realPart M),max(N,realPart M)),toRRi(imaginaryPart M))
    else toCCi(toRRi(opts.Precision,min(N,realPart M),max(N,realPart M)),toRRi(opts.Precision,imaginaryPart M,imaginaryPart M)))

for A in {ZZ,QQ,RR} do
interval(CC,A) := opts -> (M,N) -> (
    if opts.Precision < 0 then toCCi(toRRi(min(N,realPart M),max(N,realPart M)),toRRi(imaginaryPart M))
    else toCCi(toRRi(opts.Precision,min(N,realPart M),max(N,realPart M)),toRRi(opts.Precision,imaginaryPart M,imaginaryPart M)))

interval(CC,CC) := opts -> (N,M) -> (
    if opts.Precision < 0 then toCCi(toRRi(min(realPart N,realPart M),max(realPart N,realPart M)),
	toRRi(min(imaginaryPart N,imaginaryPart M),max(imaginaryPart N,imaginaryPart M)))
    else toCCi(toRRi(opts.Precision,min(realPart N,realPart M),max(realPart N,realPart M)),
	toRRi(opts.Precision,min(imaginaryPart N,imaginaryPart M),max(imaginaryPart N,imaginaryPart M))))

interval(Array) := opts -> A -> (
    if (length(A) == 0) or (length(A)>2) then error("expected length 2")
    else if length(A) == 1 then interval(opts,A_0)
    else interval(opts,A_0,A_1))

spanRRi = method(Options => {Precision => -1})

for A in {ZZ,QQ,RR} do
for B in {ZZ,QQ,RR} do
spanRRi(A,B) := opts -> (N,M) -> (
    if opts.Precision < 0 then toRRi(min(M,N),max(M,N))
    else toRRi(opts.Precision,min(M,N),max(M,N)))

for A in {ZZ,QQ,RR} do (
spanRRi(A,CC) := opts -> (N,M) -> (
    if opts.Precision < 0 then toCCi(toRRi(min(realPart M,N),max(realPart M,N)),toRRi(imaginaryPart M))
    else toCCi(toRRi(opts.Precision,min(realPart M,N),max(realPart M,N)),toRRi(opts.Precision,imaginaryPart M)));
spanRRi(CC,A) := opts -> (M,N) -> span(opts,N,M))

for A in {ZZ,QQ,RR} do (
spanRRi(RRi,A) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,M)
    else if opts.Precision < 0 then toRRi(min(left N,M),max(right N,M))
    else toRRi(opts.Precision,min(left N,M),max(right N,M)));
spanRRi(A,RRi) := opts -> (N,M) -> span(opts,M,N))

spanRRi(RRi,CC) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,M)
    else if opts.Precision < 0 then toCCi(toRRi(min(left N,realPart M),max(right N,realPart M)),toRRi(imaginaryPart M))
    else toCCi(toRRi(opts.Precision,min(left N,realPart M),max(right N,realPart M)),toRRi(opts.Precision,imaginaryPart M)));
spanRRi(CC,RRi) := opts -> (N,M) -> spanRRi(opts,M,N)

spanRRi(RRi,RRi) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,left M, right M)
    else if isEmpty(M) then interval(opts, left N, right N)
    else if opts.Precision < 0 then toRRi(min(left N,left M),max(right N,right M))
    else toRRi(opts.Precision,min(left N,left M),max(right N,right M)))

spanRRi(RRi,CCi) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,left M, right M)
    else if isEmpty(M) then interval(opts, left N, right N)
    else if opts.Precision < 0 then toCCi(toRRi(min(left N,left realPart M),max(right N,right realPart M)),imaginaryPart M)
    else toCCi(toRRi(opts.Precision,min(left N,left realPart M),max(right N,right realPart M)),
	toRRi(opts.Precision, imaginaryPart M)))
spanRRi(CCi,RRi) := opts -> (N,M) -> spanRRi(opts,M,N)

spanRRi(CCi,CCi) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,left M, right M)
    else if isEmpty(M) then interval(opts, left N, right N)
    else if opts.Precision < 0 then toCCi(toRRi(min(left realPart N,left realPart M),max(right realPart N,right realPart M)),
	toRRi(min(left imaginaryPart N, left imaginaryPart M),max(right imaginaryPart N, right imaginaryPart M)))
    else toCCi(toRRi(opts.Precision, min(left realPart N,left realPart M),max(right realPart N,right realPart M)),
	toRRi(opts.Precision, min(left imaginaryPart N, left imaginaryPart M),max(right imaginaryPart N, right imaginaryPart M))))

span = method(Dispatch => Thing, Options => true)

span ZZ := span QQ := span RR := {Precision => -1} >> opts -> N -> interval(N,opts)

span RRi := {Precision => -1} >> opts -> N -> interval(left N,right N,opts)

span List := span Sequence := {Precision => -1} >> opts -> L -> fold(L, (N, M) -> spanRRi(N, M, opts))

for A in {ZZ,QQ,RR} do
isMember(A,RRi) := (N,M) -> subsetRRi(N,M);
isMember(CC,RRi) := (N,M) -> subsetCCi(N,M);

for A in {ZZ,QQ,RR,CC} do
isMember(A,CCi) := (N,M) -> subsetCCi(N,M);

isSubset(RRi,RRi) := (N,M) -> subsetRRi(N,M);
isSubset(CCi,RRi) := (N,M) -> subsetCCi(N,M);
isSubset(CCi,CCi) := (N,M) -> subsetCCi(N,M);

-- TODO: now that intersect is a binary method with arbitrary options, we can remove this
intersection(RRi,RRi) := {Precision => -1} >> opts -> (N,M) -> (
    intersect(N, M, opts))
intersection(RRi,CCi) := {Precision => -1} >> opts -> (N,M) -> (
    intersect(N, M, opts))
intersection(CCi,RRi) := {Precision => -1} >> opts -> (N,M) -> (
    intersect(N, M, opts))
intersection(CCi,CCi) := {Precision => -1} >> opts -> (N,M) -> (
    intersect(N, M, opts))

-- intersect is an associative binary method, so it works on arbitrary lists and sequences
intersect RRi       := RRi => { Precision => -1 } >> opts -> identity
intersect CCi       := CCi => { Precision => -1 } >> opts -> identity
intersect(RRi, RRi) := RRi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then intersectRRi(N,M)
    else intersectRRi(opts.Precision,N,M))
intersect(RRi, CCi) := CCi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then intersectCCi(N,M)
    else intersectCCi(opts.Precision,N,M))
intersect(CCi, RRi) := CCi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then intersectCCi(N,M)
    else intersectCCi(opts.Precision,N,M))
intersect(CCi, CCi) := CCi => { Precision => -1 } >> opts -> (N, M) -> (
    if opts.Precision < 0 then intersectCCi(N,M)
    else intersectCCi(opts.Precision,N,M))

isEmpty RRi := Boolean => isEmptyRRi
isEmpty CCi := Boolean => isEmptyCCi

toExternalString CCi :=
toExternalString RRi:= x -> "interval" | toExternalString (left x, right x)
