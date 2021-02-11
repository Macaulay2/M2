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

timer := (f, args) -> (t0 := cpuTime(); f args; cpuTime() - t0)
B = method(Binary => true)
B(ZZ, ZZ) := (a, B) -> (a+B)
assert(timer(B, 4000000:1) < 4)

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
