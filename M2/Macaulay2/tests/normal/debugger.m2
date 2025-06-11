f = (x -> "blah");
assert( (match(":.:5-.:16", toString locate f)) === true );
disassemble' = f -> replace("(frameID:|global-fetch) [0-9]+", "\\1 [...]", disassemble functionBody f);
assert( (disassemble' even) === "(2-OP === 0 (2-OP % (local-fetch 0 0) 2))" );
assert( (disassemble' odd) === "(2-OP === 1 (2-OP % (local-fetch 0 0) 2))" );
assert( (disassemble'(() -> if true then 1)) === "(if (global-fetch [...]) then: 1 else: (null))" );
assert( (disassemble'(() -> if true then 1 else 2)) === "(if (global-fetch [...]) then: 1 else: 2)" );
assert( (disassemble'(() -> try true)) === "(try (global-fetch [...]) (null) (null))" );
assert( (disassemble'(() -> try true then 1)) === "(try (global-fetch [...]) 1 (null))" );
assert( (disassemble'(() -> try true else 2)) === "(try (global-fetch [...]) (null) 2)" );
assert( (disassemble'(() -> try true then 1 else 2)) === "(try (global-fetch [...]) 1 2)" );
assert( (disassemble'(n -> for i in 1..n when odd i list 10*i do print i)) === "(for in: (2-OP .. 1 (local-fetch 0 0)) from: (null) to: (null) when: (adjacent (global-fetch [...]) (local-fetch 0 0)) list: (2-OP * 10 (local-fetch 0 0)) do: (adjacent (global-fetch [...]) (local-fetch 0 0)))" );
assert( (disassemble'(n -> for i from 1 to n when odd i list 10*i do print i)) === "(for in: (null) from: 1 to: (local-fetch 0 0) when: (adjacent (global-fetch [...]) (local-fetch 0 0)) list: (2-OP * 10 (local-fetch 0 0)) do: (adjacent (global-fetch [...]) (local-fetch 0 0)))" );
assert( (disassemble'(n -> for i from 1 to n when odd i list 10*i)) === "(for in: (null) from: 1 to: (local-fetch 0 0) when: (adjacent (global-fetch [...]) (local-fetch 0 0)) list: (2-OP * 10 (local-fetch 0 0)) do: (null))" );
assert( (disassemble'(n -> for i to n when odd i list 10*i)) === "(for in: (null) from: (null) to: (local-fetch 0 0) when: (adjacent (global-fetch [...]) (local-fetch 0 0)) list: (2-OP * 10 (local-fetch 0 0)) do: (null))" );
assert( (disassemble'(n -> for i to n list 10*i)) === "(for in: (null) from: (null) to: (local-fetch 0 0) when: (null) list: (2-OP * 10 (local-fetch 0 0)) do: (null))" );
assert( (disassemble'(n -> for i when i^2 < 5 list 10*i)) === "(for in: (null) from: (null) to: (null) when: (2-OP < (2-OP ^ (local-fetch 0 0) 2) 5) list: (2-OP * 10 (local-fetch 0 0)) do: (null))" );
assert( (disassemble'(n -> for i when i^2 < 5 do print i)) === "(for in: (null) from: (null) to: (null) when: (2-OP < (2-OP ^ (local-fetch 0 0) 2) 5) list: (null) do: (adjacent (global-fetch [...]) (local-fetch 0 0)))" );
assert( (disassemble'(n -> for i from 1 to n when odd i list 10*i do print i)) === "(for in: (null) from: 1 to: (local-fetch 0 0) when: (adjacent (global-fetch [...]) (local-fetch 0 0)) list: (2-OP * 10 (local-fetch 0 0)) do: (adjacent (global-fetch [...]) (local-fetch 0 0)))" );
assert( (disassemble'(n -> for i from 1 to n when odd i list 10*i do print i)) === "(for in: (null) from: 1 to: (local-fetch 0 0) when: (adjacent (global-fetch [...]) (local-fetch 0 0)) list: (2-OP * 10 (local-fetch 0 0)) do: (adjacent (global-fetch [...]) (local-fetch 0 0)))" );
assert( (disassemble'(() -> catch scan(0..10, i -> if i == 5 then throw 18 else print i))) === "(catch (adjacent (global-fetch [...]) (sequence (2-OP .. 0 10) (function restargs: true numparms: 1 framesize: 1 frameID: [...] (if (2-OP == (local-fetch 0 0) 5) then: (1-OP throw 18) else: (adjacent (global-fetch [...]) (local-fetch 0 0)))))))" );
assert( (disassemble'(x -> x)) === "(local-fetch 0 0)" );
assert( (disassemble'((x) -> x)) === "(local-fetch 0 0)" );
assert( (disassemble'(sin @@ cos)) === "(adjacent (local-fetch 0 1) (adjacent (local-fetch 1 1) (local-fetch 0 0)))" );
assert( (disassemble'(pack_3)) === "(adjacent (local-fetch 0 1) (adjacent (global-fetch [...]) (sequence (local-fetch 1 1) (local-fetch 0 0))))" );
assert( (disassemble'({"A" => 123} >> o -> x -> (x, o))) === "(adjacent (global-fetch [...]) (sequence (local-fetch 1 1) (adjacent (global-fetch [...]) (sequence (local-fetch 0 1) (local-fetch 0 0)))))" );
assert( (disassemble'(true >> o -> x -> (x, o))) === "(adjacent (global-fetch [...]) (sequence (local-fetch 1 1) (adjacent (global-fetch [...]) (sequence (null) (local-fetch 0 0)))))" );
f = x -> y -> x = x+y;
assert( (disassemble' f) === "(function restargs: true numparms: 1 framesize: 1 frameID: [...] (local-assign 0 1 (2-OP + (local-fetch 0 1) (local-fetch 0 0))))" );
h = f(0);
g = f(0);
assert( (h =!= g) === true );
assert( (disassemble' g) === "(local-assign 0 1 (2-OP + (local-fetch 0 1) (local-fetch 0 0)))" );
assert( (g(1)) === 1 );
assert( (disassemble' g) === "(local-assign 0 1 (2-OP + (local-fetch 0 1) (local-fetch 0 0)))" );
assert( (g(2)) === 3 );
assert( (functionBody h === functionBody g) === true );
X = new Type of FunctionClosure;
assert( (disassemble' new X from (a -> 2*a)) === "(2-OP * 2 (local-fetch 0 0))" );
assert( (disassemble'((a,b,c) -> a*b+c)) === "(2-OP + (2-OP * (local-fetch 0 0) (local-fetch 1 0)) (local-fetch 2 0))" );
assert( (disassemble'((a,b,c) -> a+b*c)) === "(2-OP + (local-fetch 0 0) (2-OP * (local-fetch 1 0) (local-fetch 2 0)))" );
assert( (disassemble'((a,b,c) -> a*b*c)) === "(2-OP * (2-OP * (local-fetch 0 0) (local-fetch 1 0)) (local-fetch 2 0))" );
assert( (disassemble'((a,b,c) -> (a*b)*c)) === "(2-OP * (2-OP * (local-fetch 0 0) (local-fetch 1 0)) (local-fetch 2 0))" );
assert( (disassemble'((a,b,c) -> a*(b*c))) === "(2-OP * (local-fetch 0 0) (2-OP * (local-fetch 1 0) (local-fetch 2 0)))" );
needsPackage "FirstPackage"
assert( (toString last code locate symbol firstFunction) === "PRE{CODE{firstFunction = method(TypicalValue => String), class => language-macaulay2}}" );

end--
-*
-- to update this file simply run these lines:
src = last separate("end-{2,}", get "debugger.m2");
"debugger.m2" << generateAssertions src << endl;
"debugger.m2" << "end" << "--" << src << close;
*-

f = (x -> "blah");
match(":.:5-.:16", toString locate f)

disassemble' = f -> replace("(frameID:|global-fetch) [0-9]+", "\\1 [...]", disassemble functionBody f);
--
disassemble' even
disassemble' odd
-- if
disassemble'(() -> if true then 1)
disassemble'(() -> if true then 1 else 2)
-- try
disassemble'(() -> try true)
disassemble'(() -> try true then 1)
disassemble'(() -> try true else 2)
disassemble'(() -> try true then 1 else 2)
-- for
disassemble'(n -> for i in 1..n when odd i list 10*i do print i)
disassemble'(n -> for i from 1 to n when odd i list 10*i do print i)
disassemble'(n -> for i from 1 to n when odd i list 10*i)
disassemble'(n -> for i to n when odd i list 10*i)
disassemble'(n -> for i to n list 10*i)
disassemble'(n -> for i when i^2 < 5 list 10*i)
disassemble'(n -> for i when i^2 < 5 do print i)
disassemble'(n -> for i from 1 to n when odd i list 10*i do print i)
disassemble'(n -> for i from 1 to n when odd i list 10*i do print i)
-- TODO: also test continue and break
-- catch
disassemble'(() -> catch scan(0..10, i -> if i == 5 then throw 18 else print i))
-- while

-- restargs
disassemble'(x -> x)
disassemble'((x) -> x)

-- @@ and _
disassemble'(sin @@ cos)
disassemble'(pack_3)
disassemble'({"A" => 123} >> o -> x -> (x, o))
disassemble'(true >> o -> x -> (x, o))

-- closures
f = x -> y -> x = x+y;
disassemble' f
h = f(0);
g = f(0);
h =!= g
disassemble' g
g(1)
disassemble' g
g(2)
functionBody h === functionBody g

-- testing SpecialExpr
X = new Type of FunctionClosure;
disassemble' new X from (a -> 2*a)
-- precedence
disassemble'((a,b,c) -> a*b+c)
disassemble'((a,b,c) -> a+b*c)
-- associativity
disassemble'((a,b,c) -> a*b*c)
disassemble'((a,b,c) -> (a*b)*c)
disassemble'((a,b,c) -> a*(b*c))

-- testing locate
needsPackage "FirstPackage"
toString last code locate symbol firstFunction
