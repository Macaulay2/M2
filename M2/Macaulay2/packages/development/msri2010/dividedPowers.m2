DividedPowersElement = new Type of MutableHashTable

dividedPowersElement = method()
dividedPowersElement(RingElement) := f -> (
     expon := exponents f;
     coeff := flatten entries (coefficients f)_1;
     flist := pack ( 2, mingle { expon, coeff } );
     new DividedPowersElement from (hashTable append(flist, {symbol ring, ring f} ) )
)

-- Multiplication
dividedPowersTimesKeys = (A,B) -> (
     	  C := apply ( #A, i -> binomial ( A_i + B_i, B_i ) );
     	  D := join ( plus ( A, B ), {fold ( C, times )} )
     )

DividedPowersElement*DividedPowersElement := (X,Y) -> (
     R := X.ring;
     X = hashTable delete ( (symbol ring, X.ring), pairs X);
     Y = hashTable delete ( (symbol ring, Y.ring), pairs Y);
     F := combine(X,Y, dividedPowersTimesKeys,times,plus);
     H := toSequence (apply ( #(pairs F), i -> flatten (pairs F)_i ) );
     J := pack ( 2, join H );
     K := apply ( #J, i -> (if odd i then fold ( J_i, times ) else J_i ) );
     new DividedPowersElement from (append ( pack ( 2, K ), {symbol ring, R} ) )
)

-- Addition
DividedPowersElement+DividedPowersElement := (X,Y) -> (
     R := X.ring;
     X = hashTable delete ( (symbol ring, X.ring), pairs X);
     Y = hashTable delete ( (symbol ring, Y.ring), pairs Y);
     new DividedPowersElement from append ( pairs merge ( X, Y, plus ), {symbol ring, R} )
)

-- Additive Inverse
-DividedPowersElement := (X) -> (
     R := X.ring;
     X = hashTable delete ( (symbol ring, X.ring), pairs X);
     new DividedPowersElement from append ( pairs applyValues ( X, v -> -v ), {symbol ring, R})
)

-- Subtraction
DividedPowersElement-DividedPowersElement := (X,Y) -> (
     Y = -Y;
     new DividedPowersElement from pairs ( X+Y )
)
