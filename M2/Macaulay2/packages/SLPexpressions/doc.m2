
doc ///
    Key
        SLPexpressions
    Headline
        Straight Line Programs and expressions for evaluation circuits
    Description
        Text 
	    Many polynomials can be stored and evaluated efficiently when represented as a straight line program (SLP), also known as an @HREF("https://en.wikipedia.org/wiki/Circuit_(computer_science)", "algebraic circuit")@. By contrast, elements of a @TO PolynomialRing@ in Macaulay2 are necessarily represented in "expanded" form, e.g. via a monomial basis. 
	    
	    This package provides basic types and methods for constructing and evaluating SLPs.
	    
	    Here is a simple example illustrating an advantage of SLP representations.
        Example
            declareVariable x
            f = x + 1
            n = 12;
            for i from 1 to n do f = f*f -- f = (x+1)^(2^n)
            slp = makeInterpretedSLProgram({x},{f})
	    time A = evaluate(slp,matrix{{1}});
            ZZ[y];
            time B = sub((y+1)^(2^n),{y=>1})    
            A == B
	Text
    SeeAlso
        NAGtypes
	makeCompiledSLProgram
///

doc ///
    Key
        Gate
        InputGate
        SumGate
        ProductGate
        DivideGate
        DetGate
    Headline
        the class of all gates
    Description
        Text
            A gate is a building block of an evaluation circuit. For instance, an @TO InputGate@ represents an abstract unit of input, and a @TO SumGate@ takes a list of inputs, and has an output which represents the sum of the inputs. For more information on the types of gates available in this package, as well as how to construct gates, see @TO2{inputGate, "creating input gates"}@.
            
///

doc ///
    Key
    	"creating gates"
        inputGate
        (inputGate, Thing)
        sumGate
        (sumGate, List)
        productGate
        (productGate, List)
        divideGate
        detGate
        (detGate, List)
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
            This method returns a type of @TO Gate@ from the given data. Some specific @TO Gate@s are constructed as follows:
            
            @TO InputGate@ is constructed with @TO inputGate@ {\tt name}, if {\tt name} is a number then this gate is assumed to be constant.
        Example
            declareVariable X
            declareVariable Y
            inputGate 3
        Text
            @TO SumGate@ is constructed with @TO sumGate@ L, where L is a list of gates, or @TO (symbol +,Gate,Gate)@.
        Example
            X + 1
            sumGate{X,Y}
        Text
            @TO ProductGate@ is constructed with @TO productGate@ L, where L is a list of gates, or @TO (symbol *,Gate,Gate)@.
        Example
            2*Y
            productGate{X,-X,Y}
        Text
            @TO DivideGate@ is constructed with @TO (symbol /,Gate,Gate)@.
        Example
            X / Y
        Text
            @TO DetGate@ is constructed with @TO detGate@ L, where L is a doubly-nested list of gates, or det A, where A is a @TO GateMatrix@.
        Example
            detGate {{X, Y}, {-Y, X}}
            det matrix{{Y, 1}, {-1, X}}
    SeeAlso
        Gate
///

doc ///
    Key
        declareVariable
        (declareVariable,InputGate)
        (declareVariable,IndexedVariable)
        (declareVariable,Symbol)
        (declareVariable,Thing)
    Headline
        assigns an input gate to a given variable name
    Usage
        declareVariable x
    Inputs
        x:Symbol
            or an @TO IndexedVariable@
    Outputs
        :InputGate
    Description
        Text
            This method assigns an @TO InputGate@ which has the same @TO Name@ as the given symbol or the @TO baseName@ of the given @TO IndexedVariable@, so that later references to the symbol will be treated (and can be used) as an @TO InputGate@. This can be reverted with @TO undeclareVariable@.
        Example
            declareVariable x
            x + 1
    SeeAlso
        undeclareVariable
///

doc ///
    Key
        undeclareVariable
        (undeclareVariable,InputGate)
    Headline
        clears assignment of an input gate to a variable name
    Usage
        undeclareVariable x
    Inputs
        x:Symbol
            or an @TO IndexedVariable@
    Outputs
        :InputGate
    Description
        Text
            This method undoes the action of @TO declareVariable@, by reverting the assignment of an @TO InputGate@ to a symbol.
        Example
            declareVariable x
            x + 1
            undeclareVariable x
            x
    SeeAlso
        declareVariable
///

doc ///
    Key
        GateMatrix
    Headline
        a matrix of Gates
    Description
        Text
            An object of this type is a matrix with Gates as entries. Some algebraic operations (matrix multiplication, determinant, etc.) are defined for this type. It is provided, in part, for convenience of setting up involved evaluation circuits. To see how to create a @TO GateMatrix@, see @TO gateMatrix@. For information about operations that can be performed on gate matrices, see @TO2{(entries, GateMatrix), "working with gate matrices"}@.
    SeeAlso
        Gate
        gateMatrix
        "working with gate matrices"
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
            declareVariable x; declareVariable y;
            A = matrix { apply(5,i->i*x) }
            A#0
            A#0#0
            B = matrix { apply(4,i->y^i) }
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
        valueHashTable
        (valueHashTable, List, List)
        ValueHashTable
    Headline
        creates a hash table of values for evaluation
    Usage
        valueHashTable(L1, L2)
    Inputs
        L1:List
            of @TO InputGate@s
        L2:List
            of values
    Outputs
        :ValueHashTable
    Description
        Text
            This methods creates an object of type @TO ValueHashTable@ from a @TO List@ of @TO InputGate@s and a @TO List@ of corresponding values. The evaluations of the input gates at the values can be performed using the method @TO value@.
        Example
            declareVariable X
            declareVariable Y
            h = valueHashTable({X,Y},{1,ii})
            peek h
    SeeAlso
        "evaluating gates"
///

doc ///
    Key
    	"evaluating gates"
	(value, Gate, ValueHashTable)    
	(value, InputGate, ValueHashTable)    
     	(value, SumGate, ValueHashTable)
        (value, ProductGate, ValueHashTable)
        (value, DivideGate, ValueHashTable)
        (value, DetGate, ValueHashTable)
        (value, GateMatrix, ValueHashTable)
    Description
        Text
    	    The standard method @TO value@ provides a way to evaluate @TO Gate@s at particular inputs, provided in the form of a @TO ValueHashTable@. The results are cached, and thus are not recomputed when called again with the same @TO Gate@ and @TO ValueHashTable@ as inputs.
        Example
            declareVariable X
            declareVariable Y
            C = sumGate {X+Y,Y,X}
            D = productGate {X*Y,Y,C}
            h = valueHashTable({X,Y},{1,ii})
            v = value(D,h)
            v == product{value(X*Y,h),value(Y,h),value(C,h)}
    SeeAlso
        valueHashTable
///

doc ///
    Key
        makeSLProgram
        (makeSLProgram,GateMatrix,GateMatrix)
        (makeSLProgram,List,List)
	setTryJustInTimeCompilation
	(setTryJustInTimeCompilation,Boolean)
    Headline
        create a straight-line program (either interpreted or compiled)
    Description
        Text
    	    Passes the arguments to 
	    @TO makeInterpretedSLProgram@ or @TO makeCompiledSLProgram@ 
	    depending on the flag set by @TO setTryJustInTimeCompilation@. (Giving `true` 
	    as a value triggers a search for gnu C++ compiler and, 
	    if successful, makes a @TO makeCompiledSLProgram@ the default choice.
            Currently, just-in-time compilation works over RR_53 and CC_53)   
///

doc ///
    Key
        makeInterpretedSLProgram
        (makeInterpretedSLProgram,GateMatrix,GateMatrix)
        (makeInterpretedSLProgram,List,List)
        InterpretedSLProgram
    Headline
        create a straight-line program
    Usage
        makeInterpretedSLProgram(inL, outL)
    Inputs
        inL:List
            of inputs
        outL:List
            of outputs
    Outputs
        :InterpretedSLProgram
    Description
        Text
            This method returns an object of type @TO InterpretedSLProgram@, which encodes a method for evaluating an algebraic circuit.
        Example
            declareVariable X; declareVariable C;
            XpC = X+C
            XXC = productGate{X,X,C}
            detXCCX = detGate{{X,C},{C,X}}
            XoC = X/C
            slp = makeInterpretedSLProgram(matrix{{C,X}},matrix{{XXC,detXCCX,XoC,XpC+2}})
    SeeAlso
        (evaluate, SLProgram, MutableMatrix, MutableMatrix)
///

doc ///
    Key
        (evaluate,SLProgram,MutableMatrix,MutableMatrix)
        (evaluate,SLProgram,Matrix)
    Headline
        evaluate a straight-line program
    Usage
        evaluate(slp, inp, out)
    Inputs
        slp:SLProgram
        inp:MutableMatrix
            of inputs
        out:MutableMatrix
            of outputs
    Consequences
        Item
            The second argument (i.e. the mutable matrix {\tt out}) is changed to be the value of the straight-line program on the input {\tt inp}.
    Description
        Text
            This method evaluates an object of type @TO SLProgram@ on a given input. The two matrices {\tt inp} and {\tt out} should both be @TO2{MutableMatrix, "mutable"}@, of the same sizes, and be defined over the same ring.
        Example
            declareVariable X; declareVariable C;
            XpC = X+C
            XXC = productGate{X,X,C}
            detXCCX = detGate{{X,C},{C,X}}
            XoC = X/C
            slp = makeInterpretedSLProgram(matrix{{C,X}},matrix{{XXC,detXCCX,XoC,XpC+2}})
            inp = mutableMatrix{{1.2,-1}}
            out = mutableMatrix(ring inp,1,4)
            evaluate(slp,inp,out)
            clean_0.001(out - mutableMatrix {{1.2, -.44, -.833333, 2.2}}) == 0
        Text
            The straight-line program can also be evaluated at different inputs in different rings:
        Example
            inp = mutableMatrix{{-5_QQ,3}}
            ring inp
            out = mutableMatrix(ring inp,1,4)
            evaluate(slp, inp, out)
            out
    SeeAlso
        SLProgram
	InterpretedSLProgram
	CompiledSLProgram 
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
        (isConstant, InputGate)
    Headline
        whether an input gate is constant
    Usage
        isConstant g
    Inputs
        g:InputGate
    Outputs
        :Boolean
            whether g is a constant
    Description
        Text
            An input gate is constant if its @TO Name@ is either a @TO Number@ or a @TO RingElement@.
        Example
            declareVariable X
            isConstant X
            isConstant 3
    SeeAlso
        InputGate
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
        (symbol -,Gate)
    Description
        Text
    	    There are many arithmetic operations that can be performed on @TO2{Gate, "gates"}@. This makes it easy to create combine existing gates into new gates.
        Example
            declareVariable X; declareVariable Y;
            m1 = Y*Y
            m2 = X^3
	    F = m1-m2-X        
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
	(diff, GateMatrix, GateMatrix)    
    Description
        Text
    	    The output of these commands is generally a circuit for evaluating the derivative of the second argument with respect to the first.
        Example
            declareVariable X; declareVariable Y;
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
        (entries, GateMatrix)
        (transpose, GateMatrix)
        (determinant, GateMatrix)
        (submatrix,GateMatrix,List,List)
        (substitute,GateMatrix,GateMatrix,GateMatrix)
        (substitute,GateMatrix,List)
        (substitute,GateMatrix,HashTable)
        (substitute,GateMatrix,Option)
    Description
        Text
            Many typical matrix operations can also be performed on @TO2{GateMatrix, "gate matrices"}@, such as obtaining entries, number of rows and columns, transpose, and vertical or horizontal concatenation.
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
            entries M
        Text
            Horizontal (resp. vertical) concatenation is done with @TO2{(symbol |, GateMatrix, Matrix), "|"}@ (resp. @TO2{(symbol ||, GateMatrix, Matrix), "||"}@):
        Example
            N = gateMatrix {delete(x^2*y^2, flatten entries basis(4, R))}
            M | N
            M || N
        Text
            The determinant of a gate matrix is a @TO DetGate@:
        Example
            P = transpose M*M
            det P
        Text
            The native method @TO sub@ has also been overloaded to work with gate matrices: the input should be a list of @TO2{Option, "options"}@ of the form "A => B" where A is an @TO InputGate@ and B is a @TO Gate@; and the output is another @TO GateMatrix@.
        -- Example
            -- A = gateMatrix vars R
            -- B = gateMatrix random(R^2, R^1)
            -- L = apply(flatten entries A, flatten entries B, (a,b)->a=>b)
            -- sub(P, L)
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
	    These commands attempt to remove superfluous operations involving constants 
	    from the building blocks of a compound @TO Gate@ or @TO GateMatrix@. 
	    The example below is contrived, but illustrates what may happen in general.
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
	(countGates, Gate)
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

doc /// 
  Key 
    (symbol .., InputGate, InputGate)
  Headline 
    Passing .. to InputGate names (see code). TO DO: expand this.
///

doc ///
  Key 
    "creating variables as objects of type InputGate"
    (vars, IndexedVariable)
    (vars, Symbol)
    (vars, InputGate) 
///
