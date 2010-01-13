
DividedPowersElement = new Type of MutableHashTable

dividedPowersElement = method()
dividedPowersElement(RingElement) := f -> (
     expon := exponents f;
     coeff := flatten entries (coefficients f)_1;
     deg := apply (degrees ring f,first );
     flist := pack ( 2, mingle { expon, coeff } );
     H := new MutableHashTable from {{symbol ring, ring f}, {symbol degrees, deg} };
     H#(symbol terms) = new HashTable from flist;     
     new DividedPowersElement from H
)
-- Currently the hashtables are stored as {expon => coeff}.  Might be faster and easier
-- to code of make them {expon => {expon, coeff}}.

-- Multiplication
dividedPowersTimesKeys = (A,B) -> (
     	  C := apply ( #A, i -> binomial ( A_i + B_i, B_i ) );
     	  D := join ( plus ( A, B ), {fold ( C, times )} )
)

DividedPowersElement*DividedPowersElement := (X,Y) -> (
     F := combine ( X.terms, Y.terms, dividedPowersTimesKeys, times, plus );
     seq := toSequence (apply ( #(pairs F), i -> flatten (pairs F)_i ) );
     J := flatten apply ( seq , s -> (pack ( #X.degrees, s ) ) );
     newTerms := pack ( 2, apply ( #J , i -> (if odd i then fold ( J_i, times ) else J_i ) ) ); --pack ( 2, apply ( #J, i -> (if odd i then fold ( J_i, times ) else J_i ) ) );
     H := new MutableHashTable from {{symbol ring, X.ring}, {symbol degrees, X.degrees} };
     H#(symbol terms) = new HashTable from newTerms;
     new DividedPowersElement from H
)

-- Addition
DividedPowersElement+DividedPowersElement := (X,Y) -> (
     sumXY := new MutableHashTable from X;
     sumXY#(symbol terms) = merge(X.terms,Y.terms,plus);
     new DividedPowersElement from sumXY
)

-- Additive Inverse
-DividedPowersElement := (X) -> (
     invX := new MutableHashTable from X;
     invX#(symbol terms) = new HashTable from applyValues ( X.terms, v -> -v );
     new DividedPowersElement from invX
)

-- Subtraction
DividedPowersElement-DividedPowersElement := (X,Y) -> (
     new DividedPowersElement from  X+(-Y)
)

-- Ingteger Power
DividedPowersElement^ZZ := (X,z) -> (
     product apply ( z, i -> X )
)

-- Degree of DividedPowerElement
degree(DividedPowersElement) := (X) -> (
     fold ( first rsort apply ( #(keys X.terms), e -> ( apply ( #(keys X.terms)_e , i -> ( (keys X.terms)_e_i * X.degrees_i ) ) ) ), plus )
)


