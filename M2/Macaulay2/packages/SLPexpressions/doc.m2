
doc ///
    Key
        SLPexpressions
    Headline
        Straight Line Programs and expressions for evaluation circuits
    Description
        Text 
	    ???What is this useful for??? 
        Example
            declareVariable x
            f = x + 1
            n = 12;
            for i from 1 to n do f = f*f -- f = (x+1)^(2^n)
            slp = makeSLProgram({x},{f})
	    time A = evaluate(slp,matrix{{1}})
            ZZ[y];
            time B = sub((y+1)^(2^n),{y=>1})    
            A == B
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
