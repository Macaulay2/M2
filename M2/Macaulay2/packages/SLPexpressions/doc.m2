
doc ///
    Key
        SLPexpressions
    Headline
        Straight Line Programs and expressions for evaluation circuits
    Description
        Example
            X = inputGate x
            f = X + 1
            n = 12;
            for i from 1 to n do f = f*f -- f = (x+1)^(2^n)
            time A = value(f,valueHashTable({X},{1}))  
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
        (symbol *,Gate,Gate)
        (symbol +,Gate,Gate)
    Headline
        create an input gate
    Usage
        inputGate L
        sumGate L
        productGate L
        Gate + Gate
        Gate * Gate
    Inputs
        L:List
    Outputs
        :InputGate
    Description
        Text
            This method returns an @TO InputGate@ from the given data.
            
--            Some specific @TO InputGate@s: 
        
            @TO SumGate@ is constructed with @TO sumGate@ {\tt list of gates}, or @TO (symbol +,Gate,Gate)@.
        
            @TO ProductGate@ is constructed with @TO productGate@ {\tt list of gates}, or @TO (symbol *,Gate,Gate)@.
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
        
            The package @TO SLPexpressions@ overrides @TO matrix@ to allow a table (a nested list) of @TO Gate@s as an argument.
                 
        Example
            X = inputGate x; Y = inputGate y;
            A = matrix { apply(5,i->i*X) }
            B = matrix { apply(4,i->Y^i) }
            C = transpose A * B    
            numrows C, numcols C
    SeeAlso
        Gate
        Matrix
///

doc ///
    Key
    	"differentiating circuits"
    	(diff, GateMatrix, GateMatrix)
     	(diff, InputGate, DetGate)    
     	(diff, InputGate, DivideGate) 
     	(diff, InputGate, GateMatrix) 
     	(diff, InputGate, InputGate)  
     	(diff, InputGate, ProductGate)
     	(diff, InputGate, SumGate)    
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

