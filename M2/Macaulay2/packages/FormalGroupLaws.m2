-- -*- coding: utf-8 -*-
-- licensed under GPL, any version
newPackage(
	"FormalGroupLaws",
	Version => "0.2",
	Date => "February 26, 2010",
	Authors => {
		{Name => "Baptiste Calmès",
		HomePage => "http://bcalmes.perso.math.cnrs.fr/"},
                {Name => "Viktor Petrov"}
		},
	Headline => "commutative formal group laws",
	Keywords => {"Group Theory"},
	PackageImports => {"Truncations"},
	DebuggingMode => false)

-- Put here the name of functions that should be visible to users
export{"series", "FormalGroupLaw", "FormalSeries", "valuation", "compositionInverse", "FGL", "FormalGroupPoint", "universalFGL", "universalFGLQ", "formalGroupPoint"
}

-- Variables that can be modified by the user
exportMutable{
}

-- Package code 

--Defining the class of all formal series (with a precision).
FormalSeries = new Type of BasicList

--Defining a method to create a FormalSeries from a polynomial and an integer.
series = method()
series(RingElement,ZZ) := (s,n) -> 
	(
	if n<0 then error "The second argument should be a nonnegative integer."
	else if instance(class(s),PolynomialRing)==false then error "The first element of the list should be an element of a polynomial ring."
	else new FormalSeries from {part(0,n,numgens(class(s)) : 1,s),n} 
	);

--Defining equality of FormalSeries
FormalSeries == FormalSeries := (s,t) ->
	(
	s#0==t#0 and s#1==t#1
	);

--Defining truncation of formal series (note: truncate is already defined for modules, but I don't think it is important.)
truncate(FormalSeries,ZZ) := {} >> o -> (s,n) -> new FormalSeries from {part(0,min(n,s#1),numgens(class(s#0)):1,s#0),min(n,s#1)}

--Defining the sum of FormalSeries
FormalSeries + FormalSeries := (s,t) ->
	(
	u:=s#0+t#0;
	new FormalSeries from {part(0,min(s#1,t#1),numgens(class(u)):1,u),min(s#1,t#1)}
	)

--Defining the opposite of a FormalSeries
-FormalSeries := (s) -> 
	(
	new FormalSeries from {-s#0,s#1}
	)

--Defining the difference of FormalSeries
FormalSeries - FormalSeries := (s,t) ->
	(
	u:=s#0-t#0;
	new FormalSeries from {part(0,min(s#1,t#1),numgens(class(u)) :1 ,u),min(s#1,t#1)}
	)

--Defining multiples of a FormalSeries
ZZ * FormalSeries := (n,s) -> new FormalSeries from {n*s#0,s#1}

RingElement * FormalSeries := (n,s) ->
	(
	if ring n =!= coefficientRing ring s#0 then error "The constant must be from the coefficient ring."
	else new FormalSeries from {n*s#0,s#1}
	)

--Defining valuation of an element in a Polynomial ring. Maybe there is a built in function or a better way, but I cannot find it. It is a bit annoying, because we need this to be very quick, because it is used in every multiplication of FormalSeries.
valuation = (x) ->
	(
	if instance(class(x),PolynomialRing) then min(apply(exponents(x),sum))
	else error "The argument must be an element of a polynomial ring."
	)

--Defining product of FormalSeries
FormalSeries * FormalSeries := (s,t) ->
	(
	k := min(s#1,t#1);
	n:=numgens(class(s#0)):1 ;
	m:=numgens(class(t#0)):1 ;
	new FormalSeries from {sum for i from 0 to k list (part(i,i,n,s#0))*(part(0,(k-i),m,t#0)),k}
	)

--Defining inverse of FormalSeries. It has the same precision. Still slow because the internal multiplication of macaulay2 is not used. Instead, we use our multiplciation, that is slow.

inverse(FormalSeries) := (s) ->
	(
	const:=(part(0,0,numgens(class(s#0)) :1 ,s#0))#0;
	R:=class(s#0);
	if const==1_R then
		(
		use R;
		u:= new FormalSeries from {1,s#1};
		v:=u-s;
		t:=u+v;
		for i from 2 to s#1 do t=u+(v*t); 		
		t	
		)
	else if const==-1_R then -inverse(-s)
	else error "Only formal series with constant term +1 or -1 can be inverted for the moment." 
	)

--Defining powers of FormalSeries
FormalSeries^ZZ := (s,n) ->
	(
	if n==0 then new FormalSeries from {1_(class(s#0)),s#1}
	else if (n==1) or (s#0==0_(class(s#0))) then s
	else if n>0 then
	(
		t := s^(n//2);
		if n%2 == 0 then t*t else t*t*s
	)
	else inverse(s)^(-n)
	)

--Defining composition of FormalSeries
monomialSubstitute = method()
monomialSubstitute(BasicList,BasicList) := (s,v) -> --the first BasicList is a list of objects to multiply (ex: variables or FormalSeries), the second is the exponents of the monomial. A test that the two lengths match could be added.
	(
	product(for i from 0 to #v-1 list (s#i)^(v#i))
	)

substitute(FormalSeries,BasicList) := (s,v) ->
	(
	if s#0==0_(class(s#0)) then s else (
	l:=listForm(s#0);
	u:=apply(v,x->truncate(x,s#1));
	sum(for i from 0 to #l-1 list (l#i#1)*monomialSubstitute(u,l#i#0))
	)
	)

--Defining reverse formal series (inverse for composition)
compositionInverse = method()
compositionInverse(FormalSeries):= (s) ->
	(
	R:= class(s#0);
	firstCoeffs:=part(0,1,{1} ,s#0);
	if numgens(R) =!= 1 then error "Only formal series in one variable may have a composition inverse."
	else if firstCoeffs==R_0 then 
		(
		local g;
		g=new FormalSeries from {R_0,1};
		for k from 2 to s#1 do
		(
		g= new FormalSeries from {g#0,k};
		g= g + (new FormalSeries from {R_0,k})-substitute(s,{g});
		);
		g	
		)
	else if firstCoeffs==-R_0 then 
		(
		local g;
		g=new FormalSeries from {-R_0,1};
		for k from 2 to s#1 do
		(
		g= new FormalSeries from {g#0,k};
		g= g + (new FormalSeries from {-R_0,k})+substitute(s,{g});
		);
		g	
		) 
	else error "Only formal series with constant term 0 can be inverted for the composition. And for the moment, the next one also has to be 1 or -1 (and not just any invertible element)." 
	)


--Defining the class of formal group laws
FormalGroupLaw = new Type of FormalSeries 

--Method to construct a new formal group law from a FormalSeries in two variables. It tests if the formal series defines a commutative formal group law with neutral element. This is quite conservative. We might also want to make things quicker by forgeting about formal series in the computations, and then truncating at the end. 
FGL = method()
FGL(FormalSeries) := (s) -> 
	(
	x:= local x;
	y:= local y;
	z:= local z;
	S:= class(s#0);
	R := (coefficientRing(S))[x,y,z];
	t:= new FormalSeries from {sub(s#0,{S_0=>x,S_1=>y}),s#1}; -- s seen in polynomials with an extra variable in order to check associativity (even if z is already the name of a variable of S, M2 distinguishes between the two, and z now means this third z, whereas the other variables should be called R_0 and R_1).
	u:= new FormalSeries from {x,s#1}; --first variable
	v:= new FormalSeries from {y,s#1}; --second variable
	w:= new FormalSeries from {z,s#1}; --third variable
	q:= new FormalSeries from {0_R,s#1}; --zero series
	if numgens(S)=!=2 then error "The formal series must be in two variables."
	else if (((substitute(t,{u, q, q})) =!= u) or ((substitute(t,{q, v, q})) =!= v)) then error "The formal series does not satisfy the neutral axiom of formal group laws."
	else if (substitute(t,{v, u, q})) =!=t then error "The formal series does not satisfy the commutativity axiom of commutative formal group laws." 
	else if (substitute(t,{t,w,q})) =!= (substitute(t,{u,substitute(t,{v,w,q}),q})) then error "The formal series does not satisfy the axiom of associative formal group laws."
	else new FormalGroupLaw from s
	)

--Defining the class of ``points'' of formal groups (that is, pairs consisting of a FGL and a formal series without free term)
FormalGroupPoint = new Type of BasicList

--Constructor for FormalGroupPoint objects
formalGroupPoint= method()
formalGroupPoint(FormalGroupLaw,FormalSeries) := (f,s) -> (
	if f#1<s#1 then error "The precision of the formal group law should be at least that of the formal series."
	else if coefficientRing(class(s#0)) =!= coefficientRing(class(f#0)) then error "The formal group law and the series should be over the same coefficient ring."
	else new FormalGroupPoint from {f,s}
)

--formal sum
FormalGroupPoint + FormalGroupPoint := (s,t) -> (
	if s#0 != t#0 then error "Points must belong to the same formal group"
	else new FormalGroupPoint from {s#0,substitute(s#0,{s#1,t#1})}
	)

--Invert a formal group law
- FormalGroupPoint := s ->
	(
	g := - truncate(s#1,1);
	for k from 2 to (s#1)#1 do
	(
	g=new FormalSeries from {g#0,k};
	g=g - substitute(s#0,{s#1,g});
	);
	new FormalGroupPoint from {s#0,g}
	)

--formal difference
FormalGroupPoint - FormalGroupPoint := (s,t) -> s + (-t)

--formal multiple
ZZ * FormalGroupPoint := (n,s) ->
	(
	if n==0 then new FormalGroupPoint from {s#0,new FormalSeries from {0_(class((s#1)#0)),(s#1)#1}}
	else if (n==1) or ((s#1)#0==0_(class((s#1)#0))) then s
	else if n>0 then
	(
		t := (n//2) * s;
		if n%2 == 0 then t+t else t+t+s
	)
	else (-n)*(-s)
	)


--Defining the universal formal group law on the Lazard ring tensor Q up to a precision.
universalFGLQ=method()
universalFGLQ(ZZ,String,String,String) := (n,b,x,y) ->
	(
	R := ZZ[(value(b))_1..(value(b))_(n-1)];
	t := local t;
	S := R[t];
	lg := new FormalSeries from {t+sum for k from 1 to n-1 list t^(k+1)*(value(b))_k,n};
	T := R[value(x),value(y)];
	new FormalGroupLaw from substitute(compositionInverse lg,{(new FormalSeries from {sub(lg#0,{S_0=>T_0}),lg#1})+(new FormalSeries from {sub(lg#0,{S_0=>T_1}),lg#1})})
	)

--computation of the nu(i,d) of Strickland's notes. They are Bezout coefficients satisfying an extra property that makes them unique.
gcdCoeffs=method()
gcdCoeffs(BasicList):= (L) ->
	(
	if #L==0 then error "The list should have at least one element."
	else if #L==1 then {1}
	else
	  (
	  dwz:={L#0,0,1}; --gcdCoefficients(L#0,L#1);
	  wzlist:= for i from 1 to #L-1 list dwz do dwz=gcdCoefficients(dwz#0,L#i);
	  wzlist=append(wzlist,dwz);
	  v:=1;
	  i:=#wzlist-1;
	  nu:= 0;
	  result:={};
	  while i>0 do 
	    (
	    nu=wzlist#(i-1)#0//wzlist#i#0;
	    result=prepend((v*(wzlist#i#2))%nu,result);
	    v=v*wzlist#i#1+((v*wzlist#i#2)//nu)*(L#(i)//wzlist#i#0);
	    i=i-1;
	    	    );
	  result=prepend(v,result)
	  )	
	)

--Universal FGL with integer coefficients. Try to remove the dependence on the name of the b's in all this by asking for a string "b" maybe. The idea is to express the coefficients of the universal formal group law in terms of some b_i's that are the coefficients of a universal logarithm. Then, we form the a_i's thatare linear combinations of the a_(i,j) using some special coefficients. It turns out that these a_i's are multiples of the b_i's modulo some smaller b_j's. So rationnally, we can express the b_i's in terms of the a_i's, and so the a_(i,j)'s in terms of a_i's. The coefficients appearing in this last expression are integers, and this gives the coeffs of the universal formal group law on Z[a_i], which is the Lazard ring (really seen as a polynomial ring).
universalFGL=method()
universalFGL(ZZ,String,String,String) := (n,s,u,v) ->
	(
--A test should be added here to return an error if the symbol value(s) has been affected a value
	x:= local x;
	y:= local y;
	b:= local b;
	a:= local a;
	R:=QQ[b_1..b_(n-1),a_1..a_(n-1)]; --Defining a big ring with all our variables to be able to substitute and sum freely in there.
	S:=R[x];
	f:=series(x+sum for i from 1 to n-1 list b_i*x^(i+1),n); 
	T:=R[x,y];
	g:=series(x+sum for i from 1 to n-1 list b_i*x^(i+1),n); 
	F:=substitute(compositionInverse(f),{g+substitute(g,{series(y,n),series(y,n)})}); --F lives in T
	alist:= for d from 1 to n-1 list 
		(
		coeffs:=gcdCoeffs(for i from 1 to d list binomial(d+1,i)); 
		sum for i from 1 to d list (coeffs#(i-1))*((F#0)_(x^i*y^(d+1-i)))
		); --this alist is a list supposed to contain the a_i's expressions in term of the b_i's.
	blist:={}; --this list (of elements of the form b_i => ...) will contain the b_i's expressed in terms of the a_i's.
	for i from 1 to n-1 do 
		(
		blist=append(blist,
			b_i=>1/((alist#(i-1))_(b_i))*(a_i+substitute((alist#(i-1))_(b_i)*b_i - alist#(i-1),blist)))
		); 
--this part is dangerous. It assumes that the map sends coefficients in QQ that are integers to the correct element in ZZ. It seems to work, but I doubt it is firmly supported by M2, and I don't know if it will be supported in the future.
	U:=ZZ[for i from 1 to n-1 list (value(s))_i];	
	V:=U[value(u),value(v)];
	mymap:=map(V,T,join(for i from 1 to n-1 list a_i => U_(i-1), for i from 1 to n-1 list b_i => 0, {x=>V_0, y=>V_1}));
	new FormalGroupLaw from {mymap(substitute(F#0,blist)),n}
	)

--Todo       

--compute logarithm of a formal group law if the coefficient ring is a Q-algebra.

--The rest of the file is documentation.

beginDocumentation()

doc ///
	Key 
		FormalGroupLaws
	Headline
		commutative formal group laws
	Description
		Text
			This package provides elementary functions to deal with commutative formal group laws of dimension one.
///

doc ///
	Key
		FormalSeries
	Headline
		 the class of all formal series
	Description
		Text
			An object of the class {\tt FormalSeries} is a list {\tt \{P,n\}} where {\tt P} is an element of a polynomial ring and {\tt n} is an integer representing the precision (the terms in degree strictly greater than {\tt n} are considered zero).
///

doc ///
	Key
		series
	Headline
		constructing a formal series
///

doc ///
	Key
		(series,RingElement, ZZ)
	Usage
		series(s,n)
	Inputs
		s: RingElement
			an element of a polynomial ring
		n: ZZ
	Outputs
		: FormalSeries
	Description
		Text
			This constructs an object of the class {\tt FormalSeries} out of an element of a {\tt PolynomialRing} and an integer representing the precision.
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
		Text
			Note that the polynomial is automatically truncated at the precision.
		Example
			t = series(x^3+x^2+x+y,2)
///

doc ///
	Key 
		(symbol ==, FormalSeries, FormalSeries)
	Headline
		equality of formal series
	Usage 
		s == t
	Inputs
		s: FormalSeries
		t: FormalSeries
	Outputs
		: ZZ	
	Description
		Text
			True is returned if the formal series are equal (they should have the same precision) and false is returned if not.
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			t = series(x+y+1,2)
			s == s
			s == t
			r = truncate(t,1)
			t == r
///

TEST ///
	R=ZZ[x,y];
	s = series(x^2+x+y,2);
	t = series(x+y,2);
	assert((s == t)==false);
	r = truncate(s,1);
	u = truncate(t,1);
	assert(u == r)
///

doc ///
	Key
		valuation
	Headline
		smallest degree of monomials
	Usage
		valuation(p)
	Inputs
		p: 
			an element of a {\tt PolynomialRing}
	Outputs
		: 
			an integer, or infinity
	Description
		Text
			This function computes the minimum of the degrees of monomials appearing in {\tt p}. It gives infinity if {\tt p} is zero.
		Example
			R= ZZ[x,y]
			valuation(x^2+y)
			valuation(0_R)
///

TEST ///
	R= ZZ[x,y];
	assert(valuation(x^2+y)==1);
	assert(valuation(0_R)==infinity)
///

doc ///
	Key
		(symbol +, FormalSeries, FormalSeries)
	Headline
		addition of formal series
	Usage
		s + t
	Inputs
		s : FormalSeries
		t : FormalSeries
	Outputs
		: FormalSeries	
	Description
		Text
			The sum of the formal series is returned.
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			t = series(x+y+1,2)
			s + t
///

doc ///
	Key
		(symbol -, FormalSeries)
	Headline
		minus a formal series
	Usage
		-s
	Inputs
		s : FormalSeries
	Description
		Text
			This returns the additive opposite of the formal series {\tt s}.
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			-s
///

doc ///
	Key
		(symbol -, FormalSeries, FormalSeries)
	Headline
		difference of formal series
	Usage 
		s - t
	Inputs
		s : FormalSeries
		t : FormalSeries
	Outputs 
		: FormalSeries
	Description
		Text
			The difference of the formal series is returned.
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			t = series(x+y+1,2)
			s - t
///

doc ///
	Key
		(symbol *, FormalSeries, FormalSeries)
	Headline
		multiplication of formal series
	Usage
		s * t
	Inputs
		s : FormalSeries
		t : FormalSeries
	Outputs
		: FormalSeries
			the product of the formal series {\tt s} and {\tt t} 
	Description
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			t = series(x+y+1,2)
			s * t
///

doc ///
	Key
		(symbol *, ZZ, FormalSeries)
	Headline
		multiplication of a formal series by an integer
	Usage
		n * t
	Inputs
		n : ZZ
		t : FormalSeries
	Outputs
		: FormalSeries
			The multiplication of the formal series {\tt s} by the integer {\tt n}
	Description
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			4 * s
///

doc ///
	Key
		(symbol *, RingElement, FormalSeries)
	Headline 
		multiplication of a formal series by a constant
	Usage
		r * t
	Inputs
		r : RingElement
		t : FormalSeries
	Outputs
		: FormalSeries
			the multiplication of the formal series {\tt s} by the constant {\tt r}
	Description
		Example
			R=ZZ[a]
                	S=R[x,y]
			s = series(x^2+x+y,2)
			(a^2+a) * s
///

TEST ///
	R=ZZ[a];
	S=R[x,y];
	s = series(x^2+x+y,2);
	assert((a^2+a) * s ==series((a^2+a)*x^2+(a^2+a)*x+(a^2+a)*y,2))
///

doc ///
	Key
		(symbol ^, FormalSeries, ZZ)
	Headline
		powers of formal series
	Usage
		s^n
	Inputs
		s : FormalSeries
		n : ZZ
	Outputs
		: FormalSeries
			the {\tt n}-th power of the formal series {\tt s}
	Description
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			s^4
	Caveat
		If {\tt n} is 0, the formal series returned is 1 with the same precision as {\tt s}. If {\tt n} is negative, then {\tt s} needs to have its constant part equal to either +1 or -1, otherwise an error occurs.
///

TEST ///
	R=ZZ[x,y];
	s = series(x^2+x+y,2);
	t = series(x+y+1,2);
	assert(s + t==series(x^2+2*x+2*y+1,2));
	assert(-s==series(-x^2-x-y,2));
	assert(t-s == series(-x^2+1,2));
	assert(-2*t == series(-2*x-2*y-2,2));
	assert(s^2 == series(x^2+y^2+2*x*y,2))
///

doc ///
	Key
		(truncate, FormalSeries, ZZ)
	Headline
		truncate formal series
	Usage
		truncate(s,n)
	Inputs
		s : FormalSeries
		n : ZZ
	Outputs
		: FormalSeries
	Description
		Text
			This function truncates the formal series {\tt s} at the precision {\tt n}.
		Example
			R=ZZ[x,y]
			s = series(x^7+x^2+x+y,7)
			truncate(s,4)
///

TEST ///
	R=ZZ[x,y];
	s = series(x^7+x^2+x+y,7);
	assert(truncate(s,4)==series(x^2+x+y,4))
///

doc ///
	Key
		(inverse, FormalSeries)
	Headline 
		multiplicative inverse of formal series
	Usage
		inverse(s)
	Inputs
		s : FormalSeries
	Outputs
		: FormalSeries
			the multiplicative inverse of the formal series {\tt s}
	Description
		Example
			R=ZZ[x,y]
			s = series(2*x^2*y+x*y+x^2+x+y+1,3)
			inverse(s)
	Caveat
		If the constant coefficient of {\tt s} is not +1 or -1, {\tt inverse} returns an error.
///

TEST ///
	R=ZZ[x,y];
	s = series(2*x^2*y+x*y+x^2+x+y+1,3);
	assert(inverse(s) == series(x^3-x^2*y-x*y^2-y^3+x*y+y^2-x-y+1,3))
///

doc ///
	Key
		(substitute, FormalSeries, BasicList)
	Headline 
		compose formal series
	Usage
		substitute(s,l)
	Inputs
		s : FormalSeries
		l : BasicList
	Outputs
		: FormalSeries
	Description
		Text
			All the {\tt FormalSeries} involved should have the same coefficient ring. The function substitutes the variables in {\tt s} with the series in the {\tt BasicList} {\tt l}. Also note that this also works when {\tt s} is a {\tt FormalGroupLaw} which is a subclass of {\tt FormalSeries}.
		Example
			R=ZZ[x,y]
			s = series(x^2+x+y,2)
			substitute(s,{s,s})
///

TEST ///
	R=ZZ[x,y];
	s = series(x^2+x+y,2);
	assert(substitute(s,{s,s})==series(3*x^2+2*x*y+y^2+2*x+2*y,2))
///

doc ///
	Key
		(compositionInverse)
	Headline
		inverse for composition
///

doc ///
	Key
		(compositionInverse, FormalSeries)
	Headline
		inverse for composition of formal series
	Usage
		compositionInverse(s)
	Inputs
		s : FormalSeries
	Outputs 
		: FormalSeries
	Description
		Text
			The {\tt FormalSeries} {\tt s} must be in one variable and have a zero constant coefficient and a coefficient +1 or -1 in degree 1. Then, {\tt compositionInverse} computes the inverse of {\tt s} for the composition of formal series, up to the precision of {\tt s}.
		Example
			ZZ[x]
			s = series(x+x^2+2*x^3-5*x^4,4)
			t = compositionInverse(s)
			substitute(s,{t})
			substitute(t,{s})
///

TEST ///
	ZZ[x];
	s = series(x+x^2+2*x^3-5*x^4,4);
	t = compositionInverse(s);
	assert(substitute(s,{t})==series(x,4));
	assert(substitute(t,{s})==series(x,4))
///

doc ///
	Key
		FormalGroupLaw
	Headline
		the class of all formal group laws
	Description
		Text
			An object of the class {\tt FormalGroupLaw} is a {\tt FormalSeries} in two variables.
///

doc ///
	Key
		FGL
	Headline
		constructing a formal group law
///

doc ///
	Key
		(FGL, FormalSeries)
	Usage
		FGL(s)
	Inputs
		s : FormalSeries
			in two variables
	Outputs
		: FormalGroupLaw
	Description	
		Text
			This constructs an object of the class {\tt FormalGroupLaw} out of a {\tt FormalSeries} living in a {\tt PolynomialRing} with two generators. The axioms of the neutral element, commutativity and associativity are checked up to the precision of {\tt s}.
		Example
			R=ZZ[x,y]
			s = series(x+y+x*y,2)
			f= FGL(s)
///

doc ///
	Key
		FormalGroupPoint
	Headline
		the class of all points of a formal group
	Description
		Text
			An object of the class {\tt FormalGroupPoint} is a list {\tt \{f,s\}} where {\tt f} is a formal group law, {\tt s} is a formal series without free term
///

doc ///
	Key
		formalGroupPoint
	Headline
		constructing a formal group point
///

doc ///
	Key
		(formalGroupPoint, FormalGroupLaw, FormalSeries)
	Usage
		formalGroupPoint(f,s)
	Inputs
		f : FormalGroupLaw 
		s : FormalSeries
			with the same coefficient ring as {\tt f}
	Outputs
		p : FormalGroupPoint
	Description
		Text
			This constructs an object of the class {\tt FormalGroupPoint} out of a {\tt FormalGroupLaw} and a {\tt FormalSeries} with the same coefficient ring and such that {\tt s} has precision at most that of {\tt f}.
		Example
			ZZ[x,y]
			f=FGL(series(x+y+x*y,2))
			ZZ[u,v]
			s = series(u+v+u^2,2)
			p= formalGroupPoint(f,s)
///

doc ///
	Key
		(symbol +, FormalGroupPoint, FormalGroupPoint)
	Headline
		sum of points of a formal group
	Usage
		s+t
	Inputs
		s : FormalGroupPoint
		t : FormalGroupPoint
	Outputs
		: FormalGroupPoint
	Description
		Example
			ZZ[x,y]
			f = FGL(series(x+y+x*y,10))
			ZZ[u]
			s = formalGroupPoint(f,series(u^2+u,5))
			t = formalGroupPoint(f,series(u^3,5))
			s+t
///

doc ///
	Key
		(symbol -, FormalGroupPoint, FormalGroupPoint)
	Headline
		difference of points of a formal group
	Usage
		s-t
	Inputs
		s : FormalGroupPoint
		t : FormalGroupPoint
	Outputs
		: FormalGroupPoint
	Description
		Example
			ZZ[x,y]
			f = FGL(series(x+y+x*y,10))
			ZZ[u]
			s = formalGroupPoint(f,series(u^2+u,5))
			t = formalGroupPoint(f,series(u^3,5))
			s-t
///

doc ///
	Key
		(symbol -, FormalGroupPoint)
	Headline
		inverse to a point of a formal group
	Usage
		-s
	Inputs
		s : FormalGroupPoint
	Outputs
		: FormalGroupPoint
	Description
		Example
			ZZ[x,y]
			f = FGL(series(x+y+x*y,10))
			ZZ[u]
			s = formalGroupPoint(f,series(u^2+u,5))
			t = -s
			s+t
///

doc ///
	Key
		(symbol *, ZZ, FormalGroupPoint)
	Usage
		n*s
	Inputs
		n : ZZ
		s : FormalGroupPoint
	Outputs 
		: FormalGroupPoint
	Description
		Example
			ZZ[x,y]
			f = FGL(series(x+y+x*y,10))
			ZZ[u]
			s = formalGroupPoint(f,series(u^2+u,5))
			3*s
///

TEST ///
	ZZ[x,y];
	f = FGL(series(x+y+x*y,10));
	ZZ[u];
	s = formalGroupPoint(f,series(u^2+u,5));
	t = formalGroupPoint(f,series(u^3,5));
	assert(s+t===formalGroupPoint(f,series(u^5+u^4+u^3+u^2+u,5)));
	assert(s-t===formalGroupPoint(f,series(-u^5-u^4-u^3+u^2+u,5)));
	assert(-s===formalGroupPoint(f,series(-u^4+u^3-u,5)));
	assert((-3)*s===formalGroupPoint(f,series(9*u^5-9*u^4+2*u^3+3*u^2-3*u,5)))
///

doc ///
	Key
		universalFGL
	Headline 
		universal formal group law
///

doc ///
	Key
		(universalFGL,ZZ,String,String,String)
	Headline 
		universal formal group law in the Lazard ring
	Usage
		universalFGL(n,s,t,u)
	Inputs
		n: ZZ
			the degree of precision
		s : String
			the name (such as "a") to be used for the variables of the Lazard ring
		t : String
			the name (such as "x") of the first variable of the formal group law
		u : String
			the name (such as "y") of the second variable of the formal group law
	Outputs
		: FormalGroupLaw 
	Description
		Text	
			The following returns the formal group law over the Lazard ring (seen as a polynomial ring in the {a_i}'s up to degree {\tt n}.
		Example
			universalFGL(3,"a","x","y")
			universalFGL(4,"a","x","y")
	Caveat
		The decomposition of the Lazard as a polynomial ring in an infinite number of variables is not canonical, we have made a choice, here, which amounts to choosing, for every d at most n, of Bezout coefficients for the set of binomial coefficients (d,i), 1<i<d.
		Variables with names equal to the strings (like x, y or a, here) should not have been assigned values (like 3) beforehand otherwise an error will occur.
///

TEST ///
	assert(universalFGL(3,"a","x","y")==FGL(series(a_2 *x^2*y+a_2 *x*y^2+ a_1 *x*y +x+y,3)))
///

doc ///
	Key
		universalFGLQ
	Headline
		universal formal group law over rationals
///

doc ///
	Key
		(universalFGLQ,ZZ,String,String,String)
	Headline
		universal formal group law in the Lazard ring tensor Q
	Usage
		universalFGLQ(n,s,t,u)
	Inputs
		n : ZZ
			the degree of precision
		s : String
			the name (such as "b") to be used for the variables of the Lazard ring
		t : String
			the name (such as "x") of the first variable of the formal group law
		u : String
			the name (such as "y") of the second variable of the formal group law
	Outputs
		: FormalGroupLaw
	Description
		Text
			The following returns a formal group law over the Lazard ring tensor Q (seen as a polynomial ring in the classes of the projective spaces {b_i}'s, up to degree {\tt n}.
		Example
			universalFGLQ(3,"b","x","y")
			universalFGLQ(4,"b","x","y")
	Caveat
		Variables with names equal to the strings (like x, y or b, here) should not have been assigned values (like 3) beforehand otherwise an error will occur.
///

TEST ///
	assert(universalFGLQ(3,"b","x","y")==FGL(series((4*b_1^2  - 3*b_2 )*x^2*y + (4*b_1^2  - 3*b_2 )*x*y^2  - 2*b_1 *x*y + x + y, 3)))
///

--document {
--	Key => (functionName, argumentClass1, argumentClass2, ...),
--	Headline => "one line description", -- only if different functionName Headline
--	Usage => "usage",
--	Inputs => {
--		-- each input is a hypertext list
--		},
--	Outputs => {
--		-- each output is a hypertext list
--		},
--	Consequences => {
--		-- each effect is a hypertext list
--		},
--	"There can be explanatory prose here in the form of a hypertext list.",
--	EXAMPLE {
--		"m2code",
--		"m2code",
--		"m2code"
--		},
--	"There can be explanatory prose here in the form of a hypertext list.",
--	Caveat => {"warning"}
--	}

-- Tests associated to the package
-- TEST "..."
