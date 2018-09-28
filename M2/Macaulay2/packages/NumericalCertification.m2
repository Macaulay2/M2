newPackage(
	"NumericalCertification",
    	Version => "0.5", 
    	Date => "July, 2016",
    	Authors => {
	     {Name => "Kisun Lee", Email => "klee669@gatech.edu"}
	     },
    	HomePage => "http://people.math.gatech.edu/~klee669",
    	Headline => "Numercial Certification",
	PackageExports => {"NumericalAlgebraicGeometry"},
    	--DebuggingMode => true		 -- set to true only during development
    	DebuggingMode => false,
	AuxiliaryFiles => true
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists

export {"absValue", "pointNorm", "polyNorm", "polySysNorm", "complexToRational", "rationalToComplex", "computeConstants", "certifySolution", "certifyDistinctSoln", "frobeniusNormSq", "Interval", "interval", "invmat",
    "StringOption", "stringOption", "mInterval", "intervalNorm", "intervalVectorNorm", "intervalMatrix", "IntervalMatrix", "wInterval", "intervalMatrixNorm"
    ,"absInterval", "intervalIntersection", "intervalUnion", "krawczykOper", "identityIntMat", "intervalOptionList", "IntervalOptionList"}
exportMutable {}

-- in: a rational number a, precision parameter epsilon
-- out: a rational q(a) with q(a) >= sqrt(a) and |q(a) - sqrt(a) | <= epsilon
-- potentially of use in computing gamma parameter
sqrtUpper = method()
sqrtUpper (QQ, QQ) := (a, epsilon) -> (
    p := (1+a)/2;
    while abs(p^2 - a) >= epsilon * (p + min(a,1)) do (
	p = p - (p^2-a)/(p+a)
	);
    p
    )
sqrtUpper (ZZ, QQ) := (a, epsilon) -> (
    p := (1+a)/2;
    while abs(p^2 - a) >= epsilon * (p + min(a,1))  do (
	p = p - (p^2-a)/(p+a)
	);
    p
    )

absValue = method()
absValue ZZ := abs
absValue QQ := abs
absValue RR := abs
absValue CC := abs
absValue(RingElement) := r -> (
    R := ring(r);
    LT := leadTerm(sub(r,R));
    VV := (leadCoefficient(LT))^2 + (sub(r,R)-LT)^2;
    sqrt(sub(VV,RR))
    )




pointNorm = method()
pointNorm(Point) := x -> (
    N := sqrt(1+((norm(2,x))^2))
    )

polyNorm = method()
polyNorm(RingElement) := r -> (
    L := listForm r;
    sqrt(sum(L,a->(
	(e,c) := a;
	((absValue c))^2*(product(e,b->b!)*(((degree r)#0-(sum e))!)/((degree r)#0)!)
 	)))
    )

polySysNorm = method()
polySysNorm(PolySystem) := f -> (
    listOfEq := equations f;
    listOfpolyNorms := apply( listOfEq, i -> (polyNorm(i))^2);
    N := sqrt(sum listOfpolyNorms)
    )

frobeniusNormSq = method()
frobeniusNormSq(Matrix) := r -> (
    a := flatten entries r;
    sum apply(a, s -> (absValue(s))^2)
    )







computeConstants = method()
computeConstants(PolySystem, Point) := (ff, xx) -> (
    R := ring ff;
    numOfPoly := # equations ff;
    jacobianOfSys := jacobian ff;
    J := evaluate(jacobianOfSys, xx);
    if det J == 0 then error "The Jacobian is not invertible";
    eval := evaluate(ff,xx);
    y := point(inverse J * eval);
    degs := flatten for i from 1 to numOfPoly list degree ((equations ff)#(numOfPoly-i));
    diagonals := flatten for i from 1 to numOfPoly list sqrt((degs)#(numOfPoly-i))*(pointNorm(xx))^((degs)#(numOfPoly-i)-1);
    deltaD := diagonalMatrix diagonals;
    mu := max {1, (polySysNorm(ff)) * norm(inverse J * deltaD)};    
    maxdeg := max degs;
    beta := norm(2,y);
    gamma := mu*sqrt(maxdeg^3)/(2* pointNorm(xx));
    alpha := beta * gamma;
    (alpha, beta, gamma)
    )








certifySolution = method()
certifySolution(PolySystem, Point) := (f, x) -> (
    Consts := computeConstants(f,x);
    if Consts #0<(13-3*sqrt(17))/4 then (
	 print "The point is an approximate solution to the system";
	 << "The value of alpha is " << (Consts)#0 << endl;
	 << "The value of beta is " << (Consts)#1 << endl;
	 << "The value of gamma is " << (Consts)#2 << endl;
	 true
	 )
    else (
	print "The point is not an approximate solution to the system";
	false
	)
    )

certifyDistinctSoln = method()
certifyDistinctSoln(PolySystem, Point, Point) := (f, x1, x2) -> (
    Consts1 := computeConstants(f,x1);
    Consts2 := computeConstants(f,x2);
    if Consts1 #0 >= (13-3*sqrt(17))/4 then (
	print "The first point is not an approximate solution to the system";
	false
	)
    else if Consts2 #0 >= ((13-3*sqrt(17))/4) then (
	print "The second point is not an approximate solution to the system";
	false
	)
    else if norm(2,point{(coordinates x1)-(coordinates x2)}) > 2*(Consts1)#1 + (Consts2)#1 then (
	print "Associated solutions are distinct";
	true
	)
    else if (Consts1)#0 < 0.03 and norm(2,point{(coordinates x1)-(coordinates x2)}) < 1/(20*(Consts1)#2) or (Consts2)#0 < 0.03 and norm(2,point{(coordinates x1)-(coordinates x2)}) < 1/(20*(Consts2)#2) then (
	print "Associated solutions are not distinct";
	false
	)
    )



Interval = new Type of List
StringOption = new Type of Option
IntervalMatrix = new Type of List
IntervalOptionList = new Type of List


-- In order to use the options of interval, we need to change options into new Type of Option 'StringOption'
stringOption = method(TypicalValue => StringOption)
stringOption(Option) := o -> new StringOption from (
    o
    )

-- List the StringOptions and change it into new Type IntervalOptionList
intervalOptionList = method(TypicalValue => IntervalOptionList)
intervalOptionList(List) := l -> new IntervalOptionList from (
    ll := apply(l, i -> stringOption(i));
    ll
    )


-- the following net function changes design of intervals in M2 output
net Interval := i -> net "[" | net first i | ", " | net last i | "]" 

-- Function to define intervals
interval = method(TypicalValue => Interval)
interval (Number, Number) := (a, b) -> new Interval from (
    if a < b then (int := (a, b);
    int':= toList int
    )
    else (int = (b, a);
    int' = toList int
    )
    )


-- If interval function takes only one input, the it makes an interval with width 0
interval (Number) := a -> new Interval from (
    interval (a,a)
    )


-- interval function for polynomial entries
interval (Number, RingElement) := (a, b) -> new Interval from (
    a' := sub(a, ring b);
    int := toList (a',b)
    )
interval (RingElement, Number) := (b, a) -> new Interval from (
    a' := sub(a, ring b);
    int := toList (a',b)
    )
interval (RingElement, RingElement) := (f,g) -> new Interval from (
    int := toList (f,g)
    )
interval (RingElement, Interval) := (f, i) -> new Interval from (
    f toString (*) i
    )

-- if interval function takes Interval, then it just shows its input again. It will be needed when we do the computation between number and interval such as interval(a+I).
interval(Interval) := i -> new Interval from (
    i
    )


-- binary operators for intervals
Interval + Interval := (i1,i2) -> (
    if (class i1#0 === RingElement or class i1#1 === RingElement or i2#0 === RingElement or i2#1 === RingElement) 
    then (i1 + i2)
    else
    a := i1#0;
    b := i2#0;
    c := i1#1;
    d := i2#1;
    a' := a+b;
    b' := d+c;
    interval(a',b')
    )
Interval - Interval := (i1,i2) -> (
        if (class i1#0 === RingElement or class i1#1 === RingElement or i2#0 === RingElement or i2#1 === RingElement) 
    then (i1 - i2)
    else
        a := i1#0;
    b := i2#0;
    c := i1#1;
    d := i2#1;
    a' := a-d;
    b' := c-b;
    interval(a',b')
    )

Interval * Interval := (i1,i2) -> (
        if (class i1#0 === RingElement or class i1#1 === RingElement or i2#0 === RingElement or i2#1 === RingElement) 
    then (i1 * i2)
    else
        a := i1#0;
    c := i2#0;
    b := i1#1;
    d := i2#1;
    A := {a*c,a*d,b*c,b*d};
    interval(min A, max A)
    )
Number * Interval := (a,i1) -> (
    b:=i1#0;
    c:=i1#1;
    if a<0 then interval(a*c,a*b)
    else
    interval(a*b,a*c)
    )
RingElement * Interval := (a,i1) -> (
    interval(a,a) * i1
    )
Interval * RingElement := (i1, a) -> (
    interval(a,a) * i1
    )

Interval ^ Number := (i,a) -> (
    if a == 0 then interval(1,1) 
    else if a == 1 then i  
    else j := i ; k := 1 ; l := while k < a list j*i do (k=k+1 ; j=j*i); 
    (flatten join{{interval(1,1),i},l})#a
    )

Number ^ Interval := (a, i) -> (
    b := i#0;
    c := i#1;
    interval(a^b,a^c)
    )

Interval/Interval := (i1,i2) -> (
    if (i2#0 < 0 and 0 < i2#1) then print "division is impossible"
    else (
    a := 1/(i2#1);
    b := 1/(i2#0); 
    i2' := interval(a,b);
       i1*i2'
       )
       )

-- substitution function. it applies one option to given polynomial.
-- inputs are polynomial and one option such as x => I
-- then it shows interval of polynomials obtained by switching x variable into the inteval I
sub(RingElement, StringOption) := (f, o) -> (
    xx := toExternalString f;  -- make polynomial into String
    o1 := o #0; -- take the variable 
    o2 := o #1; -- take the interval
    rep := replace(o1,o2,xx);  -- replace the variable 'o1' into the interval 'o2' in the String 'xx'
    repc := concatenate("(",rep, ")*interval(1,1)");  -- in order to deal with the constant term, multiply the interval '[1,1]' on the whole string
    value repc -- read string
    )


-- substitution function for interval and one option
-- inputs are interval of polynomials and one option such as x => I
-- then it switch the variable x in the given interval of polynomials into the interval I 
sub(Interval, StringOption) := (f, o) -> (
    interval((sub((f)#0,o))#0,(sub((f)#1,o))#1)
    )


-- substitution function for interval and interval option list
-- using above function, it changes all variables in the option list into intervals in the option list
sub(Interval, IntervalOptionList) := (f, o) -> (
    interval((sub((f)#0,o))#0,(sub((f)#1,o))#1)
    )


-- when we have a just constant 'r' not an interval, then it just shows us an interval '[r,r]'
sub(Number, StringOption) := (r, o) -> (
    interval(r,r)
    )
    

-- this substitute function substitutes intervals into all variables in the given polynomial
-- inputs are polynomial and the list of interval options
sub(RingElement, IntervalOptionList) := (f, l) -> (
    i := 0;
    D := (flattenRing ring f)#0;
    fr := sub(f,D);
    tl := terms fr;  -- list the all monomial terms of polynomial
    al := apply(tl, i -> i*interval(1,1));  -- multiply the interval '[1,1]' on all monomial terms of polynomial
    sl := (i = 0; while i < (length l) list al = apply(al, j -> sub(j, l#i)) do i = i+1);  -- apply options for all monomial terms
    -- a := sum sl;
    lsl := last sl;
    sum lsl 
    )



-- absolute value for an interval means the maximum absolute value of elements in interval
absInterval = method()
absInterval(Interval) := i -> (
    max{abs(i#0),abs(i#1)}
    )


-- width of an interval
wInterval = method()
wInterval(Interval) := i -> (
    i#1-i#0
    )


-- midpoint of an interval
mInterval = method()
mInterval(Interval) := i -> (
    (i#0+i#1)/2
    )


-- midpoint function can be applied to a list of intervals
mInterval(List) := l -> (
    kk := apply(l, k -> mInterval(k));
    kk
    )


-- interval intersection when inputs are disjoint, it gives the zero interval
intervalIntersection = method()
intervalIntersection(Interval, Interval) := (i,j) -> (
    if (j#1 < i#0 or i#1 < j#0) then interval(0,0) 
    else interval(max{i#0, j#0}, min{i#1, j#1})
    )


-- interval union
intervalUnion = method()
intervalUnion(Interval, Interval) := (i,j) -> (
    interval(min{i#0, j#0}, max{i#1, j#1})
    )


-- interval norm shows us the maximum element in an interval
intervalNorm = method()
intervalNorm(Interval) := i -> (
    l := max i;
    l
    )


-- The norm for interval vector implies the maximum interval norm for each interval entry in the vector 
intervalVectorNorm = method()
intervalVectorNorm(List) := l -> (
    iv := max(apply(l, k -> intervalNorm(k)));
    iv
    )


-- the function to construct an interval matrix
-- In order to compute interval matrices, input should be the nested list of intervals such as {{[1,2],[2,3]},{[1,3],[2,4]}}
intervalMatrix = method(TypicalValue => IntervalMatrix)
intervalMatrix(List) := l -> new IntervalMatrix from (
    l
    )

-- interval matrix multiplication
IntervalMatrix * IntervalMatrix := (l, n) -> (
    numrow := length l;
    numc := length(l#0);
    numcol := length(n#0);
    mat :=  apply(apply(l, i -> (k := 0; while k < numc list ( j := 0; while j < numcol list ((i#k)*((n#k)#j)) do j = j+1)  do k=k+1)), b -> sum b);
    intervalMatrix mat
    )


-- intervalMatrixNorm computes the norm of interval matrix, an interval extension of the maximum row sum norm
intervalMatrixNorm = method()
intervalMatrixNorm(IntervalMatrix) := i -> (
    numrow := length(i);
    numcol := length(i#0);
    listOfAbs := (l := 0; while l < numcol list sum((apply(i, j -> (k := 0; while k < numrow list absInterval(j#k) do k = k+1)))#l) do l = l+1);
    max listOfAbs
    )



-- the function constructs an nxn interval identity matrix
-- will be used in order to compute Krawczyk operator
identityIntMat = method()
identityIntMat(ZZ) := n -> (
    lt := toList(1..n);
    intervalMatrix apply(lt, i -> for k from 1 to n list (if i == k then interval(1,1) else interval(0,0)))
    )


invmat = method()
invmat(List, IntervalOptionList) := (p, o) -> (
    mm := polySystem transpose matrix{p};
    m := mm#PolyMap;
    nv := numgens ring p#0;
    ng := numgens coefficientRing(ring p#0);
    j := if ng == 0 then transpose jacobian transpose m else (transpose jacobian transpose m) +(transpose (matrix apply((entries vars coefficientRing(ring p#0))#0, i ->  flatten entries ((value(toString(i)|"'"))*diff(i,m))))^{0..(nv-1)});
    e := entries j;
    n := length e; 
    -- plug in intervals into the jacobian entries   
    ijm := intervalMatrix applyTable(e, a -> interval(sub(a,o))); 
    mf := matrix applyTable(ijm, i -> mInterval i);
    inverse mf
    )

-- function to construct the Krawczyk operator
-- inputs are list of polynomials in system and n-box of intervals
krawczykOper = method()
krawczykOper(List, IntervalOptionList) := (p, o) -> (
    mm := polySystem transpose matrix{p};
    m := mm#PolyMap;
    nv := numgens ring p#0;
    ng := numgens coefficientRing(ring p#0);
    j := if ng == 0 then transpose jacobian transpose m else (transpose jacobian transpose m) +(transpose (matrix apply((entries vars coefficientRing(ring p#0))#0, i ->  flatten entries ((value(toString(i)|"'"))*diff(i,m))))^{0..(nv-1)});
    e := entries j;
    n := length e; 
    -- plug in intervals into the jacobian entries   
    ijm := intervalMatrix applyTable(e, a -> interval(sub(a,o)));
    mf := matrix applyTable(ijm, i -> mInterval i);
    -- midpoints of o
    y := toList apply(o, i ->  mInterval(value((i)#1)) );
    oll := toList apply(0.. length(o)-1, k -> value((o#k)#0) => y#k);
    -- START constructing the (box containing the) inverse 
    my := inverse mf;
    yintmatrix := intervalMatrix applyTable(entries my, a -> interval(a,a));
    -- centering o at the origin
    z := intervalMatrix apply(o, i -> {interval(-wInterval(value((i)#1)/2),wInterval(value((i)#1))/2)} );
    lengthofmat := length(yintmatrix*ijm);
    identitysubstractMatrix := identityIntMat(lengthofmat)-yintmatrix*ijm;
    -- substitute y values into system
    eval := matrix apply(p, k -> {sub(k,oll)});
    -- multiplying Y matrix and f(y)
    entofmat := entries( ( (transpose matrix {take(y,lengthofmat)})-(my*eval)));
    -- computing Krawczyk operator
    (intervalMatrix apply(entofmat, i ->  {interval((i#0),(i#0))}))+(identitysubstractMatrix*z)
    )


-- in order to use Krawczyk method, that is the method for isolating solution of system of equations, we use approximate inverse matrix 'Y' as input
-- it shows us the Krawczyk operator to determine whether the solution is contained in the n-box
krawczykOper(List, IntervalOptionList, Matrix) := (p, o, Y) -> (
    mm := polySystem transpose matrix{p};
    m := mm#PolyMap;
    nv := numgens ring p#0;
    ng := numgens coefficientRing(ring p#0);
    j := if ng == 0 then transpose jacobian transpose m else (transpose jacobian transpose m) +(transpose (matrix apply((entries vars coefficientRing(ring p#0))#0, i ->  flatten entries ((value(toString(i)|"'"))*diff(i,m))))^{0..(nv-1)});
    e := entries j;
    n := length e; 
    -- plug in intervals into the jacobian entries   
    ijm := intervalMatrix applyTable(e, a -> interval(sub(a,o))); 
    mf := matrix applyTable(ijm, i -> mInterval i);
    -- midpoints of o
    y := toList apply(o, i ->  mInterval(value((i)#1)) );
    oll := toList apply(0.. length(o)-1, k -> value((o#k)#0) => y#k);
    -- START constructing the (box containing the) inverse 
    my := Y;
    yintmatrix := intervalMatrix applyTable(entries my, a -> interval(a,a));
    -- centering o at the origin
    z := intervalMatrix apply(o, i -> {interval(-wInterval(value((i)#1)/2),wInterval(value((i)#1))/2)} );
    lengthofmat := length(yintmatrix*ijm);
    identitysubstractMatrix := identityIntMat(lengthofmat)-yintmatrix*ijm;
    -- substitute y values into system
    eval := matrix apply(p, k -> {sub(k,oll)});
    -- multiplying Y matrix and f(y)
    entofmat := entries( ( (transpose matrix {take(y,lengthofmat)})-(my*eval)));
    -- computing Krawczyk operator
    (intervalMatrix apply(entofmat, i ->  {interval((i#0),(i#0))}))+(identitysubstractMatrix*z)
    )



TEST ///
R = CC[x,y]
f = polySystem {x + y, x^2 - 4}
sols = solveSystem f
assert all(sols, p -> certifySolution(f,p))
p = point{{2.0, -2.0}}
q = point{{-2, 2.000001}}
computeConstants(f,p)
certifySolution(f,p)
certifyDistinctSoln(f,p,q)
FFF = QQ[j]/ideal(j^2+1)
R'=FFF[x,y]
ff = complexToRational(f,FFF)
certifySolution(ff,p)
certifyDistinctSoln(ff,p,q)
///

end



restart
needsPackage "AlphaTest"
FF = QQ[i]/ideal(i^2 + 1)
R = FF[x,y]
p1 = point matrix{{2,4+i}}
norm(2,p1)
absValue(2)
absValue(4+i)


restart
check "AlphaTest"
uninstallAllPackages()
installPackage "AlphaTest"
viewHelp AlphaTest
