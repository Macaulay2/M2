offset = 3 -- for recursion tests
timer = (f, args) -> (t0 := cpuTime(); f args; cpuTime() - t0)
timer = (f, args) -> (f args; 0)

-- single methods
S = method(Dispatch => Thing)
S(ZZ) := a -> if a <= offset then true else S(a-1)
assert(timer(S, 149) < 0.1) -- recursion test
-- WISHLIST: S(150)
assert(timer((M, N) -> apply(M, i -> S(N)), (10000,148)) < 1) -- timing test

S = method(Dispatch => Thing, Options => {b => 1})
S(ZZ) := o -> a -> if a <= offset then true else S(a-o.b)
assert(timer(S, 97) < 0.1) -- recursion test
-- WISHLIST: S(103,b=>1)
assert(timer((M, N) -> apply(M, i -> S(N,b=>1)), (10000,95)) < 1.8) -- timing test

-- associative binary methods
B = method(Binary => true)
B(String, String) := concatenate
assert(B toSequence(ascii\ascii "hi") == "hi")
assert(B toSequence(ascii\ascii "hello") == "hello")
B(String, ZZ) := (s, n) -> concatenate(s, ascii n)
assert(B("h", 105) == "hi")
assert(B splice("h", toSequence ascii "ello") == "hello")
B(ZZ, ZZ) := concatenate @@ ascii
assert(B toSequence ascii "hi" == "hi")
assert(B toSequence ascii "hello" == "hello")

B = method(Binary => true)
B(ZZ, ZZ) := (a,b) -> a+b
assert(timer(B, 400000:1) < 0.5)
B = method(Binary => true)
B(ZZ, ZZ) := (a,b) -> if a == b then true else B(a,b-1)
assert(timer(B, (0,41)) < 0.1) -- recursion test
-- WISHLIST: B(0,43)
assert(timer((M, N) -> apply(M, i -> B(offset,N)), (10000,41)) < 1) -- timing test

-- multiple methods
M = method()
M(ZZ, ZZ, ZZ) := (a,b,c) -> if a == b then true else M(a,b-c,c)
assert(timer(M, (offset,296,1)) < 0.1) -- recursion test
-- WISHLIST: M(0,301,1)
assert(timer((m, n) -> apply(m, i -> M(offset,n,1)), (10000,294)) < 1.5) -- timing test

M = method(Options => {d => 0})
M(ZZ, ZZ, ZZ) := o -> (a,b,c) -> if a == b then true else M(a,b-c,c+o.d)
assert(timer(M, (offset,100,1,d=>0)) < 0.1) -- recursion test
-- WISHLIST: M(0,100,1,d=>0)
assert(timer((m, n) -> apply(m, i -> M(offset,n,1)), (10000,98)) < 1.5) -- timing test

-----------------------------------------------------------------------------

-- this is documented behavior:
s = method(Options => true)
s ZZ := { Slope => 17 } >> o -> x -> o.Slope * x;
s RR := { Intercept => 11 } >> o -> x -> x + o.Intercept;
assert( s 100 === 1700 )
assert( s 1000. === 1011. )
assert( (options s) === null )
assert( (options(s,ZZ)) === new OptionTable from {Slope => 17} )
assert( (options(s,RR)) === new OptionTable from {Intercept => 11} )

-- it should work the same way for single arg dispatch
t = method(Options => true, Dispatch => Thing)
t ZZ := { Slope => 17 } >> o -> x -> o.Slope * x;
t RR := { Intercept => 11 } >> o -> x -> x + o.Intercept;
assert( t 100 === 1700 )
assert( t 1000. === 1011. )
assert( (options t) === null )
assert( (options(t,ZZ)) === new OptionTable from {Slope => 17} )
assert( (options(t,RR)) === new OptionTable from {Intercept => 11} )

-- individual methods should not have to do options processing at all, if no options are given:

s QQ := x -> x + 1/2
-- assert( s (1/1) === 3/2 ) -- requires a modification to newmethod1234c

t QQ := x -> x + 1/2
assert( t (1/1) === 3/2 )
assert( options(t,QQ) === null )

-- it should work for single arg type dispatch
u = method(Options => true, Dispatch => Type)
u Number := identity
assert( u ZZ === ZZ )
assert( u(ZZ,FOO=>BAR) === (new OptionTable from {FOO => BAR},ZZ) )

-- chainComplex is now an example, because it is defined by chainComplex = method(Options => true, Dispatch => Thing, TypicalValue => ChainComplex)
X = new Type of BasicList
chainComplex X := { FOO => BAR } >> o -> x -> (o,x);
assert ( chainComplex (new X, FOO => 123 ) === (new OptionTable from {FOO => 123},new X from {}) )
assert ( chainComplex (new X) === (new OptionTable from {FOO => BAR},new X from {}) )
chainComplex X := identity
assert( chainComplex (new X) === new X )
