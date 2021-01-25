interval = method(Options => {Precision => -1})

for A in {ZZ,QQ,RR} do
interval A := opts -> N -> (
    if opts.Precision < 0 then toRRi(N)
    else toRRi(opts.Precision,N,N))

for A in {ZZ,QQ,RR} do
for B in {ZZ,QQ,RR} do
interval(A,B) := opts -> (N,M) -> (
    if opts.Precision < 0 then toRRi(N,M)
    else toRRi(opts.Precision,N,M))

 interval(Array) := opts -> A -> (
    if (length(A) == 0) or (length(A)>2) then error("expected length 2")
    else if length(A) == 1 then interval(opts,A_0)
    else interval(opts,A_0,A_1))

-- Make this a methodfunctionsingle?
span = method(Options => {Precision => -1})

for A in {ZZ,QQ,RR} do
for B in {ZZ,QQ,RR} do
span(A,B) := opts -> (N,M) -> (
    if opts.Precision < 0 then toRRi(min(M,N),max(M,N))
    else toRRi(opts.Precision,min(M,N),max(M,N)))

for A in {ZZ,QQ,RR} do (
span(RRi,A) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,M)
    else if opts.Precision < 0 then toRRi(min(left N,M),max(right N,M))
    else toRRi(opts.Precision,min(left N,M),max(right N,M)));
span(A,RRi) := opts -> (N,M) -> span(opts,M,N))

span(RRi,RRi) := opts -> (N,M) -> (
    if isEmpty(N) then interval(opts,left M, right M)
    else if isEmpty(M) then interval(opts, left N, right N)
    else if opts.Precision < 0 then toRRi(min(left N,left M),max(right N,right M))
    else toRRi(opts.Precision,min(left N,left M),max(right N,right M)))

for A in {ZZ,QQ,RR,RRi} do
for B in {ZZ,QQ,RR,RRi} do
isSubset(A,B) := (N,M) -> subsetRRi(N,M);

-*
intersection = method(Options => true)
for A in {ZZ,QQ,RR} do (
intersection(RRi, A) := {Precision => -1} >> opts -> (N,M) -> (
    if isSubset(M,N) then interval(opts,M)
    else interval(1,0,Precision=>precision N));
intersection(A, RRi) := {Precision => -1} >> opts -> (N,M) -> intersection(M,N))

intersection(RRi,RRi) := {Precision => -1} >> opts -> (N,M) -> (
    if opts.Precision < 0 then intersectRRi(N,M)
    else intersectRRi(opts.Precision,N,M))
*-

intersectRRiNumber := (opts, N, M) -> (
    if any({ZZ, QQ, RR}, T -> instance(M, T)) then (
	if isSubset(M, N)
	then interval(opts, M)
	else interval(1, 0, Precision => precision N))
    else error("intersect: cannot intersect " | indefinite synonym RRi | " and " | indefinite synonym class M))

-- see (intersect, Sequence) in intersect.m2
RRi.intersect = { Precision => -1 } >> opts -> L -> (
    -- TODO: can this also be implemented for a longer list or sequence?
    -- in that case also implement intersect for a single RRi (and other Numbers?)
    if #L != 2 then error "intersect: expected two numbers";
    (N, M) := L;
    if instance(N, RRi) and instance(M, RRi) then (
	if opts.Precision < 0
	then intersectRRi(N, M)
	else intersectRRi(opts.Precision, N, M))
    else if instance(N, RRi) then intersectRRiNumber(opts, N, M)
    else if instance(M, RRi) then intersectRRiNumber(opts, M, N)
    else error("intersect: expected at least one " | synonym RRi))

-- Note: the method intersect is defined with Dispatch => Thing, which means that
-- RRi.intersect is mainly called via (intersect, List) or (intersect, Sequence).
-- These two lines only serve to install methods for the purpose of documentation.
-- They can be safely removed otherwise.
intersect(RRi, RRi) := RRi.intersect
scan({ZZ, QQ, RR}, T -> intersect(T, RRi) := intersect(RRi, T) := RRi.intersect)
