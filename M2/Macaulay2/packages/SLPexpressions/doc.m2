
doc ///
    Key
        SLPexpressions
    Headline
        Straight Line Programs and expressions for evaluation circuits
    Description
        Text 
	    Many polynomials can be stored and evaluated efficiently when represented as a straight line program (SLP), also known as an @HREF("https://en.wikipedia.org/wiki/Circuit_(computer_science)", "algebraic circuit")@. By contrast, elements of a @TO PolynomialRing@s are necessarily represented in "expanded" form. 
	    
	    This package provides basic types and methods for constructing and evaluating SLPs.
	    
	    Here is a simple example illustrating an advantage of SLP representations.
        Example
            declareVariable x
            f = x + 1
            n = 12;
            for i from 1 to n do f = f*f -- f = (x+1)^(2^n)
            slp = makeSLProgram({x},{f})
	    time A = evaluate(slp,matrix{{1}});
            ZZ[y];
            time B = sub((y+1)^(2^n),{y=>1})    
            A == B
	Text
    SeeAlso
        NAGtypes
///

doc ///
    Key
        Gate
        InputGate
        SumGate
        ProductGate
    Headline
        an expression that is a part of an evaluation circuit (abstract type)
    Description
        Text
            Some basic gates:
        
            @TO InputGate@ is constructed with @TO inputGate@ {\tt name}, if {\tt name} is a number then this gate is assumed to be constant.
        
            @TO SumGate@ is constructed with @TO sumGate@ {\tt list of gates}, or @TO (symbol +,Gate,Gate)@.
        
            @TO ProductGate@ is constructed with @TO productGate@ {\tt list of gates}, or @TO (symbol *,Gate,Gate)@.
///

doc ///
    Key
    	"creating gates"
        inputGate
        sumGate
        productGate
    Headline
        create an input gate
    Usage
        inputGate L
        sumGate L
        productGate L
    Inputs
        L:List
    Outputs
        :InputGate
    Description
        Text
            This method returns an @TO InputGate@ from the given data.
            
--            Some specific @TO InputGate@s: 
        
            @TO SumGate@ is constructed with @TO sumGate@ {\tt list of gates}.
        
            @TO ProductGate@ is constructed with @TO productGate@ {\tt list of gates}.
    SeeAlso
        InputGate
///

doc ///
    Key
        GateMatrix
    Headline
        a matrix of Gates
    Description
        Text
            An object of this type is a matrix with Gates as entries. Some algebraic operations (matrix multiplication, determinant, etc.) are defined for this type. It is provided, in part, for convenience of setting up involved evaluation circuits.
    SeeAlso
        Gate
        gateMatrix
///

doc ///
    Key
        gateMatrix
        (gateMatrix, GateMatrix)
        (gateMatrix, List)
        (gateMatrix, Matrix)
    Headline
        create a GateMatrix
    Usage
        gateMatrix M
    Inputs
        M:
            a @TO matrix@ or a table (a nested @TO List@)
    Outputs
        :GateMatrix
    Description
        Text
            This methods creates a @TO GateMatrix@ from the given input data, which should be a matrix or a doubly-nested list.
        
            The package @TO SLPexpressions@ overrides @TO matrix@ to allow a table (a nested list) of @TO Gate@s as an argument.
                 
        Example
            X = inputGate x; Y = inputGate y;
            A = matrix { apply(5,i->i*X) }
            A#0
            A#0#0
            B = matrix { apply(4,i->Y^i) }
            C = transpose A * B    
            numrows C, numcols C
    SeeAlso
        GateMatrix
///

doc ///
    Key
        gatePolynomial
        (gatePolynomial, RingElement)
        (gatePolynomial, Matrix)
    Headline
        creates an input gate for a given polynomial
    Usage
        gatePolynomial f
    Inputs
        f:RingElement
    Outputs
        :Gate
            representing the polynomial $f$
    Description
        Text
            This methods creates a @TO Gate@ from the given input polynomial $f$. The resulting @TO Gate@ is a @TO SumGate@ whose terms are @TO2{ProductGate, "product gates"}@ corresponding to monomials of $f$.                 
        Example
            R = QQ[x,y]
            f = random(3, R)
            gatePolynomial f
    SeeAlso
        Gate
///

doc ///
    Key
        getVarGates
        (getVarGates, PolynomialRing)
    Headline
        returns the input gates for variables in a polynomial ring
    Usage
        getVarGates R
    Inputs
        R:PolynomialRing
    Outputs
        :List
            of input gates for the variables in $R$
    Description
        Text
            This methods returns a @TO List@ of @TO2{InputGate, "input gates"}@ corresponding to variables in the given polynomial $R$, and caches the result in the ring for future use.                 
        Example
            R = QQ[x,y]
            getVarGates R
    SeeAlso
        Gate
///

doc ///
    Key
    	"arithmetic with circuits"
        (symbol *,Gate,Gate)
        (symbol +,Gate,Gate)
     	(symbol /,Gate,Gate)
     	(symbol ^,Gate,ZZ)
        (symbol *,GateMatrix,GateMatrix)
        (symbol *,GateMatrix,Matrix)
        (symbol *,GateMatrix,RingElement)
        (symbol +,GateMatrix,GateMatrix)
        (symbol +,GateMatrix,Matrix)
        (symbol -,GateMatrix,GateMatrix)
        (symbol -,GateMatrix,Matrix)
        (symbol ^,GateMatrix,List)
        (symbol *,Matrix,Gate)
        (symbol *,Matrix,GateMatrix)
        (symbol +,Matrix,GateMatrix)
        (symbol -,Matrix,GateMatrix)
        (symbol +,Gate,Number)
        (symbol -,Gate,Number)
        (symbol *,Gate,Number)
        (symbol +,Number,Gate)
        (symbol -,Number,Gate)
        (symbol *,Number,Gate)
	(symbol *,RingElement,Gate)
	(symbol *,RingElement,GateMatrix)
	(symbol +,RingElement,Gate)
	(symbol -,RingElement,Gate)
	(symbol *,Gate,Matrix)
	(symbol *,Gate,RingElement)
	(symbol +,Gate,RingElement)
	(symbol -,Gate,Gate)
	(symbol -,Gate,RingElement)
    Description
        Text
    	    There are many arithmetic operations that can be performed on @TO2{Gate, "gates"}@. This makes it easy to create combine existing gates into new gates.
        Example
            X = inputGate x; Y = inputGate y;
	    F = Y^2-X^3-X        
        Text
            If one of the inputs is a @TO Number@, it is first converted to an @TO InputGate@:
        Example
            X + 2        
        Text
            By extension, arithmetic operations also work with @TO2{GateMatrix, "gate matrices"}@:
        Example
            M = matrix {{X, Y}}
            3*M
            transpose M * M
    SeeAlso
        InputGate
        GateMatrix
///

doc ///
    Key
    	"differentiating circuits"
     	(diff, InputGate, Gate)    
     	(diff, InputGate, GateMatrix)    
    Description
        Text
    	    The output of these commands is generally a circuit for evaluating the derivative of the second argument with respect to the first.
        Example
            X = inputGate x; Y = inputGate y;
	    F = Y^2-X^3-X
	    diff(X,F)
	    J = diff(gateMatrix{{X,Y}},gateMatrix{{F}})
	    (numrows J, numcols J)
    SeeAlso
        InputGate
        GateMatrix
///

doc ///
    Key
        "working with gate matrices"
        (symbol _,GateMatrix,List)
        (symbol _,GateMatrix,Sequence)
        (symbol |,GateMatrix,GateMatrix)
        (symbol |,GateMatrix,Matrix)
        (symbol ||,GateMatrix,GateMatrix)
        (symbol ||,GateMatrix,Matrix)
        (symbol |,Matrix,GateMatrix)
        (symbol ||,Matrix,GateMatrix)
        (numColumns,GateMatrix)
        (numRows,GateMatrix)
    Description
        Text
            Many typical matrix operations can also be performed on @TO2{GateMatrix, "gate matrices"}@, such as obtaining entries, number of rows and columns, and vertical or horizontal concatenation.
        Example
            R = RR[x,y]
            M = gateMatrix basis(3, R)
            numcols M, numrows M
        Text
            Rows or entries can be accessed with &#95; or #:
        Example
            M_0
            M#0
            M#0#0
        Text
            Horizontal (resp. vertical) concatenation is done with @TO2{(symbol |, GateMatrix, Matrix), "|"}@ (resp. @TO2{(symbol ||, GateMatrix, Matrix), "||"}@):
        Example
            N = gateMatrix {delete(x^2*y^2, flatten entries basis(4, R))}
            M | N
            M || N
    SeeAlso
        GateMatrix
///

doc ///
    Key
        "compressing circuits"
	(compress,Gate)
	(compress,GateMatrix)
    Usage
        g' = compress g
	G' = compress G
    Inputs
    	g:Gate
        G:GateMatrix
    Outputs
        g':Gate    	   
        G':GateMatrix
    Description
        Text
	    These commands attempt to remove superfluous operations involving constants from the building blocks of an @TO SLProgram@. The example below is contrived, but illustrates what may happen in general.
	Example
	    declareVariable \ {a,b,c}
	    x = inputGate 1
	    y = inputGate 2
    	    G = gateMatrix{{(x+y)+3+4+b+4+c+4*(a+b+2)}}
	    cG = compress G
	    depth G
	    depth cG
	    countGates G
	    countGates cG
    SeeAlso
	"measuring the size of circuits"
///

doc ///
    Key
        "measuring the size of circuits"
	(depth,DetGate)
	(depth,DivideGate)
	(depth,GateMatrix)
	(depth,InputGate)
	(depth,ProductGate)
	(depth,SumGate)
	countGates
	(countGates, GateMatrix)
    Usage
        d = depth g
	d = depth G
	H = countGates g
	H = countGates G
    Inputs
    	g:Gate
        G:GateMatrix
    Outputs
    	d:ZZ
	    circuit depth
	H:HashTable
	    total number of gates of each type
    Description
        Text
	    The depth of an algebraic circuit is the length of the longest path of evaluations from any input gate to any output gate.
        Example
            declareVariable x
	    f = x + 1
	    n = 12;
	    for i from 1 to n do f = f*f -- f = (x+1)^(2^n)
    	    depth f
    	Text
            depth is not the sole indicator of circuit complexity. For instance, the total number of gates in a circuit (sometimes referred to as its "size") also plays a role. "countGates" returns the number of constituent @TO Gate@s according to their type.
        Example
	    x = symbol x
	    n = 8
	    varGates = declareVariable \ for i from 1 to n list x_i
    	    G1 = gateMatrix{{x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8}}
	    G2 = gateMatrix{{((x_1+x_2)+(x_3+x_4))+((x_5+x_6)+(x_7+x_8))}}
	    depth G1
	    depth G2
    	    countGates G1
	    countGates G2
    SeeAlso
        "compressing circuits"
///
