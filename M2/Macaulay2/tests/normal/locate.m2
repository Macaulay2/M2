f = () -> if ( true ) then ( "true" ) else ( "false" )
assert( ((F = functionBody f;)) === null );
assert( ((c = pseudocode   f;)) === null );
assert( ((C = pseudocode   F;)) === null );
getcols = P -> (toList P)_{2,4,6}
assert( (getcols locate f) === {4,54,4} );
assert( (getcols locate c) === {4,54,4} );
assert( (getcols locate F) === {10,54,10} );
assert( (getcols locate C) === {10,54,10} );
assert( (getcols locate C_0) === {15,19,15} );
assert( (getcols locate C_1) === {29,35,29} );
assert( (getcols locate C_2) === {45,52,45} );
f = () -> (    1+1    )
assert( ((F = functionBody f;)) === null );
assert( ((c = pseudocode   f;)) === null );
assert( ((C = pseudocode   F;)) === null );
assert( (getcols locate f) === {4,23,4} );
assert( (getcols locate c) === {4,23,4} );
assert( (getcols locate F) === {15,18,16} );
assert( (getcols locate C) === {15,18,16} );
f = (a,b) -> a
end--
-*
-- to update this file simply run these lines:
restart
src = last separate("end-{2,}", get "locate.m2");
"locate.m2" << generateAssertions src << endl;
"locate.m2" << "end" << "--" << src << close;
*-
--  0  0  1  1 1  1 2 2    2 2    3 3 3    4 4     5 5
--  4  7  0  3 5  8 0 2    7 9    4 6 8    3 5     1 3
f = () -> if ( true ) then ( "true" ) else ( "false" )
(F = functionBody f;)
(c = pseudocode   f;)
(C = pseudocode   F;)
getcols = P -> (toList P)_{2,4,6}
getcols locate f
getcols locate c
getcols locate F
getcols locate C
getcols locate C_0
getcols locate C_1
getcols locate C_2

--        1    1 1    2
--  4  7  0    5 7    2
f = () -> (    1+1    )
(F = functionBody f;)
(c = pseudocode   f;)
(C = pseudocode   F;)
getcols locate f
getcols locate c
getcols locate F
getcols locate C

--        1  1
--  4     0  3
f = (a,b) -> a
-- FIXME: match(".*:.*:4:.*:.*: error: expected 2 arguments but got 1", last capture "f(1)")

-- TODO: add tests for other kinds of code as well
