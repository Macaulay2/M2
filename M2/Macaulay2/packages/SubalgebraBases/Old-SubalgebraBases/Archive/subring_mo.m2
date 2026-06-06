debug Core; 


esubi = method()
esubi(ZZ, ZZ) := List => (i, n) -> (
    -- i = an integer
    -- n = an integer
    -- returns a list of length n with n-1 zeroes and 1 one in the ith position

    toList(i:0) | {1} | toList(n-i-1:0)
    );

weightVectors = method()
weightVectors(Ring) := List => (R) -> (
    -- R = a polynomial ring
    -- returns a list of weight vectors giving the monomial order of R

    M := monomialOrderMatrix R;
    n := #(options R).Variables; -- size for weight vectors

    head := entries M_0;
    tail := if M_1 === Lex then
                for i from 0 to n-1 list esubi(i, n)
            else if M_1 === RevLex then
                for i from 0 to n-1 list -esubi(n-i-1, n)
	    else
	    	error "invalid monomial order";

    head | tail
    );

R = QQ[vars(0..3), MonomialOrder=>{1, Weights=>{1,2,3,4}, Position=>Down}];


R = QQ[vars(0..3), MonomialOrder=>{1, Weights=>{1,2,3,4}, Position=>Down}];
mo = monomialOrderMatrix R; -- this one is NOT CORRECT YET...
ans = (
    matrix {{1, 0, 0, 0}, {0, 1, 2, 3}, {0, 1, 1, 1}},
    RevLex,
    Position => Down,
    "ComponentBefore" => 2
    );










error "stop"
