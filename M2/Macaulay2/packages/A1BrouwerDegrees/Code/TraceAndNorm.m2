  

--We define a method to represent multiplication by an element in a finite dimensional K-algebra as a matrix indexed by basis elements of its base field

isRingElement=method()

isRingElement(Ring,Thing):=(R,a) -> (
    try promote(a,R) else (
	error "the given element is not a member of the input ring"
    )
)

getMultiplicationMatrix=method()

--This applies the method by accepting a K-algebra C and an element a as inputs

getMultiplicationMatrix(Ring,Thing):= (C,a) -> (
	isRingElement(C,a);
	B:=basis(C);
	r:=degree C;
	Q:=(a)*(transpose B)*B;
	toVector := q -> last coefficients(q,Monomials=>B);
	Matrep := q -> (M:=toVector(q*B_(0,0));i:=1;while i<r do
	    (M=M|(toVector (q*B_(0,i))) ; i=i+1); M);
	lift(Matrep a, coefficientRing C))

--This applies the method by accepting a polynomial ring C, an ideal I and an element a as input to find matrix representation of multiplication by the element over the corresponding quotient ring 

getMultiplicationMatrix(Ring,Ideal,Thing):= (S,I,b) -> (
        isRingElement(S,b);
	B:=basis(S/I);
	r:=degree I;
	Q:=(b)*(transpose B)*B;
	toVector := q -> last coefficients(q,Monomials=>B);
	Matrep := q -> (M:=toVector(q*B_(0,0));i:=1;while i<r do
	    (M=M|(toVector (q*B_(0,i))) ; i=i+1); M);
	lift(Matrep b, coefficientRing S))

--We use the implemented matrix representation to calculate the algebraic trace
    
getTrace=method()

getTrace(Ring,Thing) := (C,a) -> (
	M:=getMultiplicationMatrix(C,a);
	trace M)
    
getTrace(Ring,Ideal,Thing) := (S,I,b) -> (
	M:=getMultiplicationMatrix(S,I,b);
	trace M)

--We use the implemented matrix representation to calculate the algebraic norm 
    
getNorm=method()

getNorm(Ring,Thing) := (C,a) -> (
	M:=getMultiplicationMatrix(C,a);
	det M)

getNorm(Ring,Ideal,Thing) := (S,I,b) -> (
	M:=getMultiplicationMatrix(S,I,b);
	det M)
