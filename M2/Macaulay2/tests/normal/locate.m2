f = () -> if ( true ) then ( "true" ) else ( "false" )
assert( ((F = functionBody f;)) === null );
assert( ((c = pseudocode   f;)) === null );
assert( ((C = pseudocode   F;)) === null );
getcols = P -> (toList P)_{2,4,6}
assert( (getcols locate f) === {4,54,7} );
assert( (getcols locate f) === {4,54,7} );
assert( (getcols locate F) === {4,54,7} );
assert( (getcols locate c) === {10,54,43} );
assert( (getcols locate C) === {10,54,43} );
assert( (getcols locate C_0) === {15,19,15} );
assert( (getcols locate C_1) === {29,35,29} );
assert( (getcols locate C_2) === {45,52,45} );
end--
-*
-- to update this file simply run these lines:
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
getcols locate f-* why is focus on '->'? *-
getcols locate F
getcols locate c-* why is focus on '( "false" )'? *-
getcols locate C
getcols locate C_0
getcols locate C_1
getcols locate C_2

-- TODO: add tests for other kinds of code as well
