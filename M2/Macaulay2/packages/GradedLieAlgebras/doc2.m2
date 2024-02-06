doc ///
Key     
       (annihilator,FGLieSubAlgebra)    
Headline
  make the annihilator Lie subalgebra  
Usage
  A=annihilator(S)  
Inputs
  S: FGLieSubAlgebra   
Outputs
  A: LieSubAlgebra
     the Lie subalgebra of {\tt L=S#lieAlgebra} consisting of 
     the set $A$ of elements $x$ in $L$  
     such that $x y=0$ for all $y$ in $S$.
SeeAlso
    "quotient(LieIdeal,FGLieSubAlgebra)"
Description
  Text
    The optional input given above is not relevant for Lie algebras.
  Example
    L = lieAlgebra{a,b,c}/{a a c+b a c,c c a}
    S=lieSubAlgebra({a c})
    A=annihilator(S)
    dims(1,4,A)
    basis(3,A)
    
   
/// 
doc ///
Key     
       (basis,ZZ,LieAlgebra)    
Headline
  compute a basis  
Usage
  b=basis(m,L)  
Inputs
  m: ZZ
  L: LieAlgebra   
Outputs
  b: List
     a basis for the part of $L$ of first degree $m$.
SeeAlso
    dims
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
    
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    basis(7,Q)
    
   
/// 
doc ///
Key     
       (basis,ZZ,ExtAlgebra)    
Headline
  compute a basis  
Usage
  b=basis(m,E)  
Inputs
  m: ZZ
  E: ExtAlgebra   
Outputs
  b: List
     a basis for the part of $E$ of first degree $m$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    E=extAlgebra(5,Q)
    basis(4,E)
    
   
/// 
doc ///
Key     
       (basis,ZZ,VectorSpace)    
Headline
  compute a basis  
Usage
  b=basis(m,V)  
Inputs
  m: ZZ
  V: VectorSpace
     an instance of type {\tt VectorSpace}  
Outputs
  b: List
     a basis for the part of $V$ of first degree $m$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    H=lieHomology Q
    basis(5,H)
    Z=cycles Q
    basis(3,Z)
    
   
/// 
doc ///
Key     
       (basis,ZZ,ZZ,LieAlgebra)    
Headline
  compute a basis  
Usage
  b=basis(m,n,L)  
Inputs
  m: ZZ
  n: ZZ
  L: LieAlgebra   
Outputs
  b: List
     a basis for the part of $L$ of first degree $m$ and homological degree $n$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    basis(7,4,Q)
    
   
/// 
doc ///
Key     
       (basis,ZZ,ZZ,ExtAlgebra)    
Headline
  compute a basis  
Usage
  b=basis(m,n,E)  
Inputs
  m: ZZ
  n: ZZ
  E: ExtAlgebra   
Outputs
  b: List
     a basis for the part of $E$ of first degree $m$ and homological degree $n$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    E=extAlgebra(5,Q)
    basis(4,4,E)
    
   
/// 
doc ///
Key     
       (basis,ZZ,ZZ,VectorSpace)    
Headline
  compute a basis  
Usage
  b=basis(m,n,V)  
Inputs
  m: ZZ
  n: ZZ
  V: VectorSpace
     an instance of type {\tt VectorSpace}   
Outputs
  b: List
     a basis for the part of $V$ of first degree $m$ and homological degree $n$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    H=lieHomology Q
    basis(5,3,H)
    Z=cycles Q
    basis(5,3,Z)
    
   
/// 
doc ///
Key     
       (basis,List,LieAlgebra)    
Headline
  compute a basis  
Usage
  b=basis(s,L)  
Inputs
  s: List
     the weight
  L: LieAlgebra   
Outputs
  b: List
     a basis for the part of $L$ of multi-degree $s$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    basis({7,4},Q)
    
   
/// 
doc ///
Key     
       (basis,List,ExtAlgebra)    
Headline
  compute a basis  
Usage
  b=basis(s,E)  
Inputs
  s: List
     the weight
  E: ExtAlgebra   
Outputs
  b: List
     a basis for the part of $E$ of multi-degree $s$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    "basis(List,VectorSpace)"
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    E=extAlgebra(5,Q)
    basis({4,4},E)
    
   
/// 
doc ///
Key     
       (basis,List,VectorSpace)    
Headline
  compute a basis  
Usage
  b=basis(s,V)  
Inputs
  s: List
     the weight
  V: VectorSpace
     an instance of type {\tt VectorSpace}      
Outputs
  b: List
     a basis for the part of $V$ of multi-degree $s$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "basis(ZZ,ZZ,LieAlgebra)"
    "basis(List,LieAlgebra)"
    "basis(ZZ,ExtAlgebra)"
    "basis(ZZ,ZZ,ExtAlgebra)"
    "basis(List,ExtAlgebra)"
    "basis(ZZ,VectorSpace)"
    "basis(ZZ,ZZ,VectorSpace)"
    
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    H=lieHomology Q
    basis({5,3},H)
    Z=cycles Q
    basis({5,3},Z)
    
   
///
document {
Key => {
         weight,
	   (weight,LieElement),
	   (weight,ExtElement),
	   (weight,LieDerivation)
       },
     
Headline => 
  "get the weight of a homogeneous element",
  TEX "The weight of a homogeneous Lie (or Ext) element $x$ 
     is obtained as ", TT "weight(x).",TEX " The 
     zero element of a Lie algebra 
     has weight equal to a list of zeroes of length
     equal to the degree length of the Lie algebra; however, 
     its weight should
     be thought of as arbitrary.
     The weight of a derivation $d$ is the weight of $d$ as a graded map
     and may also be obtained as ", TT "d#weight.", 
SeeAlso => {"firstDegree(LieElement)","sign","degreeLength"},
SYNOPSIS {  
    Usage =>
      "w=weight(x)",   
    Inputs => {
      "x" => LieElement
       }, 
    Outputs => {
      "w" => List
       }
   },
EXAMPLE {
     "L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
	 LastWeightHomological=>true, Signs => 1)",
     "D=differentialLieAlgebra{0_L,a a,a b}/{a a b, a a c, b a b}",
     "x=a b c+2 c b a",
     "weight x",
     "weight 0_D"    
          },
SYNOPSIS {  
    Usage =>
      "w=weight(x)",   
    Inputs => {
      "x" => ExtElement
       }, 
    Outputs => {
      "w" => List
       }
   },
EXAMPLE {
     "E=extAlgebra(5,D)",
     "b=basis(5,E)",
     "apply(b,weight)"
    },
SYNOPSIS {  
    Usage =>
      "w=weight(d)",   
    Inputs => {
      "d" => LieDerivation
       }, 
    Outputs => {
      "w" => List
       }
   },
EXAMPLE {
      "weight differential D"
    },    
}
document {
     Key => {
	   computedDegree,
	     (computedDegree,LieAlgebra),
	     (computedDegree,ExtAlgebra)
	  },
     Headline => "get the degree to which the computations 
                  have been performed",
   TEX " The computed degree of a Lie algebra (or Ext-algebra) $L$ is $d$ when the 
    computation 
    of $L$ has been performed up to degree $d$.",   
     
     SYNOPSIS {
	  Usage => "n=computedDegree(L)",	            	            ,
     	  Inputs => {
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       "n" => ZZ => "the degree of computation of L"
	      }
	  },
      SYNOPSIS {
	  Usage => "n=computedDegree(E)",	            	            ,
     	  Inputs => {
	       "E" => ExtAlgebra
	       },
	  Outputs => {
	       "n" => ZZ => "the degree of computation of E"
	      }
	  },
     
     EXAMPLE{
	 "L = lieAlgebra({a,b,c},Weights=>{1,2,3})",
	 "dims(1,3,L)",
	 "computedDegree L",
	 "firstDegree(c a b+a b c)",
	 "computedDegree L"},       
     }
document {
     Key => {
	   firstDegree,
	     (firstDegree,LieElement),
	     (firstDegree,ExtElement),
	     (firstDegree,LieDerivation)
	  },
     Headline => "get the degree of an element",
    TEX " The degree of an element $x$ in a Lie algebra or
    an Ext-algebra is 
    the first degree
    of the homogeneous element $x$. 
    The zero element has degree 0; however, its degree should
     be thought of as arbitrary. 
    The degree of a Lie derivation $d$ is the first degree of $d$ 
    considered as 
    a graded map.", 
      
     SeeAlso => {"weight"},
     
     SYNOPSIS {
	  Usage => "n=firstDegree(x)",	  
     	  Inputs => {
	       "x" => LieElement
	       },
	  Outputs => {
	       "n" => ZZ => TEX " the first degree of $x$"	       
	       }
	   },
     SYNOPSIS {
	  Usage => "n=firstDegree(x)",	  
     	  Inputs => {
	       "x" => ExtElement
	       },
	  Outputs => {
	       "n" => ZZ => TEX " the first degree of $x$"	       
	       }
	   },
     SYNOPSIS {
	  Usage => "n=firstDegree(d)",	  
     	  Inputs => {
	       "d" => LieDerivation
	       },
	  Outputs => {
	       "n" => ZZ => TEX " the first degree of $d$"	       
	       }
	   },
     EXAMPLE{
	 "L = lieAlgebra({a,b,c},Weights=>{1,2,3})",
	 "firstDegree(c a b+a b c)",
	 "E = extAlgebra(5,L)",
	 "apply(gens E,firstDegree)",
	 "d=lieDerivation({a c b,b b c,c c b})",
	 "firstDegree d"
	 },       
     }
doc ///
Key     
       (dim,ZZ,LieAlgebra)    
Headline
  compute the dimension  
Usage
  d=dim(m,L)  
Inputs
  m: ZZ
  L: LieAlgebra   
Outputs
  d: ZZ
     the dimension of $L$ in first degree $m$.
SeeAlso
    dims
    "basis(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
  
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    dim(7,Q)
    
   
///
doc ///
Key     
       (dim,ZZ,ExtAlgebra)    
Headline
  compute the dimension  
Usage
  d=dim(m,E)  
Inputs
  m: ZZ
  E: ExtAlgebra   
Outputs
  d: ZZ
     the dimension of $E$ in first degree $m$.
SeeAlso
    dims
    "basis(ZZ,ExtAlgebra)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
  
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    E=extAlgebra(5,Q)
    dim(4,E)
    
   
///
doc ///
Key     
       (dim,ZZ,VectorSpace)    
Headline
  compute the dimension  
Usage
  d=dim(m,V)  
Inputs
  m: ZZ
  V: VectorSpace 
     an instance of type {\tt VectorSpace} 
Outputs
  d: ZZ
     the dimension of $V$ in first degree $m$.
SeeAlso
    dims
    "basis(ZZ,VectorSpace)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
 
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    H=lieHomology Q
    dim(5,H)
    Z=cycles Q
    dim(5,Z)
    
    
   
///
doc ///
Key     
       (dim,ZZ,ZZ,LieAlgebra)    
Headline
  compute the dimension  
Usage
  d=dim(m,n,L)  
Inputs
  m: ZZ
  n: ZZ
  L: LieAlgebra   
Outputs
  d: ZZ
     the dimension of $L$ in first degree $m$ and homological degree $n$.
SeeAlso
    dims
    "basis(ZZ,ZZ,LieAlgebra)"
    "dim(ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
  
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    dim(7,4,Q)
    
    
   
///
doc ///
Key     
       (dim,ZZ,ZZ,ExtAlgebra)    
Headline
  compute the dimension  
Usage
  d=dim(m,n,E)  
Inputs
  m: ZZ
  n: ZZ
  E: ExtAlgebra   
Outputs
  d: ZZ
     the dimension of $E$ in first degree $m$ and homological degree $n$.
SeeAlso
    dims
    "basis(ZZ,ZZ,ExtAlgebra)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
  
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    E=extAlgebra(5,Q)
    dim(4,4,E)
    
    
   
///
doc ///
Key     
       (dim,ZZ,ZZ,VectorSpace)    
Headline
  compute the dimension  
Usage
  d=dim(m,n,V)  
Inputs
  m: ZZ
  n: ZZ
  V: VectorSpace
     an instance of type {\tt VectorSpace}   
Outputs
  d: ZZ
     the dimension of $V$ in first degree $m$ and homological degree $n$.
SeeAlso
    dims
    "basis(ZZ,ZZ,VectorSpace)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
 
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    H=lieHomology Q
    dim(5,3,H)
    Z=cycles Q
    dim(5,3,Z)
      
///
doc ///
Key     
       (dim,List,LieAlgebra)    
Headline
  compute the dimension  
Usage
  d=dim(s,L)  
Inputs
  s: List
     the weight
  L: LieAlgebra   
Outputs
  d: ZZ
     the dimension of $L$ in multi-degree $s$.
SeeAlso
    dims
    "basis(List,LieAlgebra)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
 
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    dim({7,4},Q)
      
///
doc ///
Key     
       (dim,List,ExtAlgebra)    
Headline
  compute the dimension  
Usage
  d=dim(s,E)  
Inputs
  s: List
     the weight
  E: ExtAlgebra   
Outputs
  d: ZZ
     the dimension of $E$ in multi-degree $s$.
SeeAlso
    dims
    "basis(List,ExtAlgebra)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"
    "dim(List,VectorSpace)"
Description
 
  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    E=extAlgebra(5,Q)
    dim({4,4},E)
      
///
doc ///
Key     
       (dim,List,VectorSpace)    
Headline
  compute the dimension  
Usage
  d=dim(s,V)  
Inputs
  s: List
     the weight
  E: VectorSpace 
     an instance of type {\tt VectorSpace}     
Outputs
  d: ZZ
     the dimension of $V$ in multi-degree $s$.
SeeAlso
    dims
    "basis(List,VectorSpace)"
    "dim(ZZ,LieAlgebra)"
    "dim(ZZ,ZZ,LieAlgebra)"
    "dim(List,LieAlgebra)"
    "dim(ZZ,ExtAlgebra)"
    "dim(ZZ,ZZ,ExtAlgebra)"
    "dim(List,ExtAlgebra)"
    "dim(ZZ,VectorSpace)"
    "dim(ZZ,ZZ,VectorSpace)"

Description

  Example
    L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>{1,1,1},LastWeightHomological=>true)
    D= differentialLieAlgebra({0_L,a a,a b})     
    J=lieIdeal({b b + 4 a c})
    Q=D/J
    H=lieHomology Q
    dim({5,3},H)
    Z=cycles Q
    dim({5,3},Z)
      
///
document {
     Key => {
	   dims,
       (dims,ZZ,LieAlgebra),
       (dims,ZZ,ExtAlgebra),
       (dims,ZZ,VectorSpace),
       (dims,ZZ,ZZ,LieAlgebra),
       (dims,ZZ,ZZ,ExtAlgebra),
       (dims,ZZ,ZZ,VectorSpace)
	  },
     Headline => "compute the dimensions of a Lie algebra, Ext-algebra or 
     vector space",
      
     SeeAlso => {"dim(ZZ,LieAlgebra)","basis(ZZ,LieAlgebra)"},
     
     SYNOPSIS {
	  Usage => "dm=dims(n,L)",	  
     	  Inputs => {
	       "n" => ZZ => "the maximal degree",
	       "L" => LieAlgebra 
	       },
	  Outputs => {
	       "dm" => Matrix => TEX " the matrix of dimensions of $L$ 
	                         in first degree $i$ and homological degree
	                         $j$, where $i$ ranges from $1$ to $n$ and $j$ from 0 to $n-1$ "	       
	       }
	   },
     SYNOPSIS {
	  Usage => "dm=dims(n,E)",	  
     	  Inputs => {
	       "n" => ZZ => "the maximal degree",
	       "E" => ExtAlgebra 
	       },
	  Outputs => {
	       "dm" => Matrix => TEX " the matrix of dimensions of $E$ 
	                         in first degree $i$ and homological degree
	                         $j$, where $i$ ranges from $1$ to $n$ and $j$ from 0 to $n-1$ "	       
	       }
	   },
     SYNOPSIS {
	  Usage => "dm=dims(n,V)",	  
     	  Inputs => {
	       "n" => ZZ => "the maximal degree",
	       "V" => VectorSpace => {"an instance of type ",TT " VectorSpace"}
	       },
	  Outputs => {
	       "dm" => Matrix => TEX " the matrix of dimensions of $V$ 
	                         in first degree $i$ and homological degree
	                         $j$, where $i$ ranges from $1$ to $n$ and $j$ from 0 to $n-1$ "	       
	       }
	   },
     SYNOPSIS {
	  Usage => "dl=dims(n,m,L)",	  
     	  Inputs => {
	       "n" => ZZ => "the starting degree",
	       "m" => ZZ => "the ending degree",
	       "L" => LieAlgebra 
	       },
	  Outputs => {
	       "dl" => List => TEX " the dimensions of $L$ in first 
	                         degree $i$, where $i$ ranges from $n$ to $m$ "	       
	       }
	   },
     SYNOPSIS {
	  Usage => "dl=dims(n,m,E)",	  
     	  Inputs => {
	       "n" => ZZ => "the starting degree",
	       "m" => ZZ => "the ending degree",
	       "E" => ExtAlgebra 
	       },
	  Outputs => {
	       "dl" => List => TEX " the dimensions of $E$ in first 
	                         degree $i$, where $i$ ranges from $n$ to $m$ "	       
	       }
	   },
     SYNOPSIS {
	  Usage => "dl=dims(n,m,V)",	  
     	  Inputs => {
	       "n" => ZZ => "the starting degree",
	       "m" => ZZ => "the ending degree",
	       "V" => VectorSpace => {"an instance of type ",TT " VectorSpace"}
	       },
	  Outputs => {
	       "dl" => List => TEX " the dimensions of $V$ in first 
	                         degree $i$, where $i$ ranges from $n$ to $m$ "	       
	       }
	   },
     EXAMPLE{
	"L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
             Signs=>{1,1,1},LastWeightHomological=>true)",
        "D= differentialLieAlgebra({0_L,a a,a b})",      
        "J=lieIdeal({b b + 4 a c})",
	"Q=D/J",
	"dims(7,Q)",
	"Z=cycles Q",
	"dims(5,Z)",
	"H=lieHomology Q",
	"dims(1,5,H)",
	"E=extAlgebra(5,Q)",
	"dims(4,E)"
	 },       
     }
document {
Key => {
         sign,
       (sign,LieElement),
       (sign,ExtElement),
       (sign,LieDerivation)
       },
     
Headline => 
  "get the sign of a homogeneous element  ",
  TEX "The sign of a homogeneous Lie (or Ext) element $x$ 
     is obtained as ", TT "sign(x).", TEX " The 
     zero element has sign equal to 0; however, its sign should 
     be thought of as arbitrary. 
     The sign of a derivation $d$ is the sign of $d$ as a graded map
     and may also be obtained as ", TT "d#sign.", 
SeeAlso => {"firstDegree(LieElement)", "weight"},
SYNOPSIS {  
    Usage =>
      "s=sign(x)",   
    Inputs => {
      "x" => LieElement => {TEX "an element of type $L$ where $L$ is of type ",
	  TT "LieAlgebra"}
       }, 
    Outputs => {
      "s" => ZZ => "the sign; 0 or 1"
       }
   },
EXAMPLE {
     "L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
	 LastWeightHomological=>true, Signs => 1)",
     "D=differentialLieAlgebra{0_L,a a,a b}/{a a b, a a c, b a b}",
     "x=a b c+2 c b a",
     "sign x"    
          },
SYNOPSIS {  
    Usage =>
      "s=sign(x)",   
    Inputs => {
      "x" => ExtElement => {TEX "an element of type $E$ where $E$ is of type ",
	  TT "ExtAlgebra"}
       }, 
    Outputs => {
      "s" => ZZ => "the sign; 0 or 1"
       }
   },
EXAMPLE {
     "E=extAlgebra(5,D)",
     "b=basis(5,E)",
     "apply(b,sign)"
    },
SYNOPSIS {  
    Usage =>
      "s=sign(d)",   
    Inputs => {
      "d" => LieDerivation
       }, 
    Outputs => {
      "s" => ZZ => "the sign; 0 or 1"
       }
   },
EXAMPLE {
      "sign differential D"
    },    
}

document {
     Key => {
	     (describe,LieAlgebra),
	     (describe,ExtAlgebra),
	     (describe,LieAlgebraMap),
	     (describe,LieDerivation),
	     (describe,VectorSpace)
	  },
     Headline => "real description",
     "The function displays a list of relevant information about the object in 
     question.",  
     
     SYNOPSIS {
	  Usage => "describe(L)",
	  
     	  Inputs => {
	       "L" => LieAlgebra
	       }
	   },
     SYNOPSIS {
	  Usage => "describe(E)",
	  
     	  Inputs => {
	       "E" => ExtAlgebra
	       }
	   },
     SYNOPSIS {
	  Usage => "describe(f)",
	  
     	  Inputs => {
	       "f" => LieAlgebraMap
	       }
	   },
     SYNOPSIS {
	  Usage => "describe(d)",
	  
     	  Inputs => {
	       "d" => LieDerivation
	       }
	   },
     SYNOPSIS {
	  Usage => "describe(V)",
	  
     	  Inputs => {
	       "V" => VectorSpace => {"an instance of type ",TT " VectorSpace"}
	       }
	   },
	 EXAMPLE{
	  "L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
	         Signs=>{1,1,1},LastWeightHomological=>true)",
          "D= differentialLieAlgebra({0_L,a a,a b})", 
	  "I=lieIdeal{b b+4 a c}",
	  "Q=D/I",               
	  "describe Q",
	  "describe I",
	  "describe map(Q,D)",
	  "describe differential D",
	  "describe extAlgebra(5,Q)"
         }       
     }

doc ///
Key
  boundaries
     (boundaries,LieAlgebra)
    
     
Headline
  make the subalgebra of boundaries
Usage
  B=boundaries(L)
     
Inputs  
  L: LieAlgebra
Outputs
  B: LieSubAlgebra 
        the image of the differential
SeeAlso
  "cycles"
  "lieHomology" 
Description
 Example
    L=lieAlgebra({a,b},Signs=>{1,0},Weights=>{{2,0},{2,1}},
	LastWeightHomological=>true,Field=>ZZ/3)
    L=differentialLieAlgebra{0_L,a}
    B=boundaries L
    dims(10,B)
    
  
///
document {
Key => {
         lieHomology,
	   (lieHomology,LieAlgebra)
       },
     
Headline => 
  "make the homology as a vector space",
  "The basis elements for the homology of
     a Lie algebra (which is of type ", TT "VectorSpace", ")
     are represented as cycles
     in the Lie algebra.", 
SeeAlso => {"boundaries","cycles"},
SYNOPSIS {  
    Usage =>
      "H=lieHomology(L)",   
    Inputs => {
      "L" => LieAlgebra
       }, 
    Outputs => {
      "H" => VectorSpace => "the homology of the Lie algebra"
       },
EXAMPLE {
     "L=lieAlgebra({a,b},Signs=>{1,0},Weights=>{{2,0},{2,1}},
        Field=>ZZ/3,LastWeightHomological=>true)",
     "D=differentialLieAlgebra({0_L,a})",
     "H=lieHomology D",
     "dims(10,H)",
     "basis(6,H)"
          },
    }  
 }

doc ///

Key
  cycles
     (cycles,LieAlgebra)
    
     
Headline
  make the subalgebra of cycles
Usage
  C=cycles(L)
     
Inputs
  L: LieAlgebra
Outputs
  C: LieSubAlgebra
       the kernel of the differential
SeeAlso
  "boundaries"
  "lieHomology" 
Description
 Example
     L=lieAlgebra({a,b},Signs=>{1,0},Weights=>{{2,0},{2,1}},
        Field=>ZZ/3,LastWeightHomological=>true)
     D=differentialLieAlgebra({0_L,a})
     C=cycles D
     dims(10,C)
     
 
  
///


doc ///

Key
  center
     (center,LieAlgebra)
    
     
Headline
  make the ideal of central elements
Usage
  c=center(L)
     
Inputs  
  L: LieAlgebra
Outputs
  c: LieIdeal
       the ideal of central elements
SeeAlso
  "annihilator(FGLieSubAlgebra)"
  "quotient(LieIdeal,FGLieSubAlgebra)"   
Description
 Example
  L = lieAlgebra{a,b,c}/{a b,a c,c b c,b b c b}
  C = center L
  dims(1,5,C)
  basis(3,C)
  
///


doc ///
Key
     (trace, ZZ, LieSubSpace, LieAlgebraMap)  
Headline
  compute the trace of a Lie algebra map acting on a Lie subspace
Usage
  r = trace(n,S,f)  
Inputs
  n: ZZ
     the degree
  S: LieSubSpace
       
  f: LieAlgebraMap
       
SeeAlso
     "isIsomorphism(LieAlgebraMap)"
     "isWellDefined(ZZ,LieAlgebraMap)"
     
Outputs
  r: RingElement
     the trace of the map $f$ acting on $S$ in degree $n$
      
Description
  Text  
    The subspace $S$ in degree $n$ 
    should be invariant 
    under $f$ (which is tested by the program), and the output
    gives the trace of $f$ acting on $S$ in degree $n$, 
    which is an element in {\tt L#Field}.
    
    
  Example
    L = lieAlgebra({a,b,c}, Field=>ZZ/31) 
    S=lieSubAlgebra{a,b,c} 
    f=map(L,L,{b,c,a})
    trace(3,S,f)
    f c b a
    f b c a
    
///
doc ///
Key
  (coefficients,LieElement)
     
Headline
  get the coefficients and monomials of a Lie element
Usage
  c = coefficients(x)  
Inputs
  x: LieElement
      $x$ is of type $L$, where $L$ is of type {\tt LieAlgebra}
SeeAlso
     "monomials(LieElement)"
     indexForm
     (symbol @,LieElement,LieElement)
     
Outputs
  c: List
     the lists of coefficients and monomials in $x$  
Description
  Text
    The optional inputs given above are not relevant for Lie algebras.
    A Lie element $x$ has a normal form in a Lie algebra $L$, which is a 
    linear combination of basis elements in 
    a certain order; {\tt coefficients(x)} gives the 
    lists of coefficients and monomials in this
    representation. If the Lie element has been 
    obtained using the "formal" operators, 
    then {\tt coefficients(x)}
    gives the coefficients and monomials for all the iterated 
    Lie products used in the expression. 
    
  Example
    L = lieAlgebra{a,b,c}
    x = a b c - 3 c b a +(1/3) b a c
    coefficients x
    y = a@b@c/3@c@b@a++(1/3)@b@a@c
    coefficients y
    
    
///
doc ///
Key
  (monomials,LieElement)
     
Headline
  get the monomials of a Lie element
Usage
  c = monomials(x)  
Inputs
  x: LieElement
      $x$ is of type $L$, where $L$ is of type LieAlgebra
SeeAlso
     "coefficients(LieElement)"
     indexForm
     (symbol @,LieElement,LieElement)
     
Outputs
  c: List
     the list of monomials in $x$  
Description
  Text
    The optional input given above is not relevant for Lie algebras.
    This is equal to the second part of @TO "coefficients(LieElement)"@.
    
    
  Example
    L = lieAlgebra{a,b,c}
    x = a b c - 3 c b a +(1/3) b a c
    monomials x
    y = a@b@c/3@c@b@a++(1/3)@b@a@c
    monomials y
    
    
///
doc ///
Key
  differential
     
Headline
   make the derivation defined by the differential
Usage
  d=differential(L)
Inputs
  L: LieAlgebra
Outputs
  d: LieDerivation
       the derivation defined by the differential 
SeeAlso
  lieAlgebra
  lieDerivation
  
   
Description
  Text
    If the current Lie algebra has no differential, then 0 is returned.

     
     
  Example    
     L=lieAlgebra({a,b},Signs=>{1,0},Weights=>{{2,0},{2,1}},
	 LastWeightHomological=>true)
     L=differentialLieAlgebra{0_L,a}
     d=differential(L)
     d b b a 
          
/// 
doc ///
Key
     (eulers, ZZ, LieAlgebra)
Headline
  compute the list of Euler characteristics
Usage
  l = eulers(n,L)
SeeAlso
  lieHomology
Inputs
  n: ZZ
     the  maximal degree
  L: LieAlgebra
Outputs
  l: List
     the list of Euler characteristics from degree $1$ to $n$
Description
  Text
    For each first degree $d$, where $d$ goes from $1$ to $n$, 
    the alternating sum of the dimensions 
    of the Lie algebra in homological degree 0 to $d-1$ is computed. 
    As we know, the same numbers
    are obtained using the homology of the Lie algebra instead.
   
  Example
    F=lieAlgebra({a,b,c,r3,r4,r42},
         Weights => {{1,0},{1,0},{2,0},{3,1},{4,1},{4,2}},
         Signs=>{0,0,0,1,1,0},LastWeightHomological=>true)
    L=differentialLieAlgebra{0_F,0_F,0_F,a c,a a c,r4 - a r3}
    Q=L/{b c - a c,a b,b r4 - a r4}
    dims(5,Q)
    eulers(5,Q)
    H=lieHomology Q
    dims(5,H)
    
/// 
doc ///
Key
     (euler, LieAlgebra)
Headline
  compute the Euler derivation
Usage
  d = euler(L)
SeeAlso
  lieDerivation
  innerDerivation
Inputs
  L: LieAlgebra
Outputs
  d: LieDerivation     
Description
  Text
    The Euler derivation is defined as {\tt x -> firstDegree(x) x}
   
  Example
     L = lieAlgebra({a,b,c},Weights=>{{1,0},{2,1},{3,2}},
          Signs=>1,LastWeightHomological=>true)
     D= differentialLieAlgebra({0_L,a a,a b})
     d=euler D
     d a b c
     describe d
     ic=innerDerivation c 
     e=d ic
     describe e
     e===(firstDegree ic) ic
    
/// 
document {
     Key => {
	   holonomy,
             (holonomy, List),
             (holonomy, List,List) 
	  },
     Headline => "compute the holonomy Lie algebra associated 
     to an arrangement or matroid",
   TEX " Two lists in the union of $x$ and $y$ have at most one element in common 
    and the sets in $x$ are disjoint. All sets in $x$ have length at least 2
    and all sets in $y$ have length at least 3. In the case of a single argument $y$,
    there is a unique simple matroid of rank at most 3 such that $y$ is 
    the set of all 2-flats of size at least 3. In some cases this matroid 
    may be realized as the matroid of a central arrangement of hyperplanes. 
    
    The output ", TT "holonomy(y)",
    TEX " is the holonomy Lie algebra of this matroid (or arrangement).
    
       
    In the geometric language, the case with two arguments $x$ and $y$
    corresponds to the deconing process of a central hyperplane 
    arrangement, see ", TO "Holonomy Lie algebras and symmetries",  
    TEX ", yielding an affine hyperplane arrangement. The set $x$ consists of the 
    maximal sets of parallel hyperplanes of size at least 2, and
    $y$ is the set of all maximal sets of hyperplanes of size at least 3 that 
    intersect in an affine space of codimension 2. 
    
    The output ", TT "holonomy(x,y)" , 
    TEX "is the holonomy Lie algebra of the affine arrangement, which is
    the same in degrees at least 2 as ", TT "holonomy(z)" , TEX " where $z$ is 
    obtained by choosing a new variable and adding it to all sets in $x$ and 
    then taking the union with $y$.",  
     SeeAlso => {"holonomyLocal","decompose(LieAlgebra)","Holonomy Lie algebras and symmetries"},
     SYNOPSIS {
	  Usage => "L=holonomy(y)",	            	            ,
     	  Inputs => {
	       "y" => List => TEX "a list of subsets of the finite set $X$"
	       },
	  Outputs => {
	       "L" => LieAlgebra => TEX "the holonomy Lie algebra with generators $X$"
	      }
	  },
      SYNOPSIS {
	  Usage => "L=holonomy(x,y)",	            	            ,
     	  Inputs => {
	      "x" => List => TEX "a list of subsets of the finite set $X$",
	      "y" => List => TEX "a list of subsets of the finite set $X$"
	       },
	  Outputs => {
	      "L" => LieAlgebra => TEX "the holonomy Lie algebra with generators $X$"
	      }
	  },
     
     EXAMPLE{
    "L=holonomy({{a0,a1,a2,a3},{a0,a4,a5},{a1,a4,a6}})",
    "describe L",
    "dims(1,4,L)",
    "M=holonomy({{a1,a2,a3},{a4,a5}},{{a1,a4,a6}})",
    "describe M",
    "dims(1,4,M)" },       
     }                         

doc ///
Key
  [holonomy,Field]
Headline
  optional argument for holonomy
Usage
  L = holonomy(A,Field => F)
  L = holonomy(A,B,Field => F)   
Inputs     
    F:   Ring
        the field of coefficients
    A: List
    B: List
Outputs
  L: LieAlgebra
SeeAlso
  lieAlgebra
  holonomy
  "Second Lie algebra tutorial"
  "Holonomy Lie algebras and symmetries"
Description
  Text
    This is an option for @TO holonomy@ to define the coefficient field, 
    which is {\tt QQ} by default. You may use any "exact" field (not the real numbers or the
	  complex numbers), such as a prime field or an algebraic extension, e.g.,
      {\tt toField(ZZ/7[x]/ideal\{x^2+1\})} or a fraction field, e.g., {\tt frac(QQ[x])}. 
      Observe that it 
      is necessary to use the function {\tt toField} when $F$ is defined 
      as an algebraic extension of a prime field.
  Example
      F = toField(ZZ/7[x]/ideal{x^2+1})
      L = holonomy({{a,d}},{{a,b,c}},Field=>F)
      (3*x+2) a b + (2*x+3) b a
///
doc ///
Key
  holonomyLocal
    (holonomyLocal, ZZ, LieAlgebra)

Headline
 compute the Lie algebra for a local subalgebra of the holonomy Lie algebra 
SeeAlso
  holonomy
  "decompose(LieAlgebra)"
  "Holonomy Lie algebras and symmetries"
Usage
  L = holonomyLocal(i,H)
Inputs
  i: ZZ
    referring to the $i$th set, where the sets in the first input of holonomy 
    are counted before the sets in the second input
  H: LieAlgebra
    the holonomy Lie algebra   
Outputs
 L : LieAlgebra
      the Lie subalgebra
Description
  Text
    The generators in the $i$th set (beginning with $i=0$) in the inputs of 
    @TO holonomy@ generate a subalgebra
    of the holonomy Lie algebra $H$, and the output of {\tt holonomyLocal(i,H)} 
    is this Lie subalgebra. If the set is of size $k$, then the local Lie
    algebra is free on $k$ generators if the set belongs to the first input
    set, and it is free on $k-1$ generators in degrees $\ge 2$ if it belongs
    to the second input set. 
		  
    
  Example
    H=holonomy({{a1,a2},{a3,a4}},{{a1,a3,a5},{a2,a4,a5}})
    describe holonomyLocal(1,H) 
    describe holonomyLocal(2,H) 
   
///
doc ///
Key
     (decompose, LieAlgebra)  
Headline
    compute the ideal associated to an arrangement or matroid
Usage
  I = decompose(L) 
Inputs
  L: LieAlgebra
     the holonomy Lie algebra
Outputs
  I: LieIdeal     
SeeAlso
  holonomy
  holonomyLocal
  "Holonomy Lie algebras and symmetries"     
Description
  Text
  
   This gives the kernel of the Lie homomorphism from 
   [$L,L$] to the direct sum of [$L_i,L_i$], where 
   $L_i$ is the Lie subalgebra generated by the $i$th subset in
   the input for the holonomy Lie algebra $L$; 
   see @TO holonomyLocal@. The ideal is generated by the basis elements in degree 3
   of the form {\tt (x y z)}, where not all {\tt x,y,z}
   belong to the same $L_i$. The ideal is zero if and only if $L$ decomposes into the direct
   sum of the local Lie subalgebras $L_i$ in degrees $\ge \ 2$.
  
  Example
    L=holonomy({{a0,a1,a2},{a0,a3,a4},{a1,a3,a5},{a2,a4,a5}})
    I=decompose L
    dims(1,4,I)
    basis(3,I)
    
///
doc ///
Key
  indexForm
     (indexForm, LieElement)
     
Headline
  get a Lie element in the polynomial ring representation
Usage
  r=indexForm(a)
Inputs
  a: LieElement     
Outputs
  r: RingElement
     the element in {\tt L#cache.mbRing} corresponding to the input.  
SeeAlso
   mbRing
   "coefficients(LieElement)" 
Description
 Text
     The ring {\tt L#cache.mbRing}, see @TO mbRing@, is used to get an output of 
     Lie elements with indexed basis elements, 
     which sometimes is better to use than the 
     iterated Lie products of generators, especially in 
     high degrees. Use @TO indexForm@ to get the 
     output in  {\tt L#cache.mbRing} and @TO "standardForm(RingElement,LieAlgebra)"@ 
     to get back the standard output. 
     The ring @TO mbRing@ is very
     large: it has as many generators as the total 
     dimension of the computed Lie algebra. 
     For this reason, you should give the ring a name
     to avoid
     a large output. When $x$ is a linear polynomial in 
     {\tt L#cache.mbRing}, the composition
     {\tt indexForm(standardForm(x)} gives back $x$ . 
     When $x$ is a Lie element in $L$, the composition {\tt standardForm(indexForm x)} 
     is equal to $x$ modulo the relations
     in $L$. 
     
 Example
    L = lieAlgebra{a,b,c}
    x = (basis(2,L))_0 (basis(3,L))_4
    R = L#cache.mbRing
    numgens R
    indexForm x 
    standardForm(oo,L) 
    indexForm a a b c
    standardForm(oo,L)
    a a b c===oo


///
doc ///
Key
  mbRing
   
Headline
  a polynomial ring representation of the Lie algebra used for output
Usage
 R=L#cache.mbRing
  
Outputs
  R: PolynomialRing
SeeAlso
  indexForm
  "standardForm(RingElement,LieAlgebra)"
  lieRing
  
  
Description
  Text
     The ring is used as a representation of the Lie algebra $L$ and 
     may be obtained as 
     {\tt L#cache.mbRing}. The ring @TO mbRing@ is very
     large: it has as many generators as the total 
     dimension of the computed Lie algebra. 
     For this reason, you should give the ring a name
     to avoid
     a large output.
     In order to transform
     a Lie element to a linear polynomial
     in {\tt L#cache.mbRing}, use @TO indexForm@. For the other direction, 
     use @TO "standardForm(RingElement,LieAlgebra)"@.
     
     
  Example
     L=lieAlgebra{a,b,c}/{a b-a c}
     dims(1,5,L)
     R=L#cache.mbRing
     numgens R
     indexForm(a a a a b+a a a b c)
     standardForm(oo,L)
     a a a a b+a a a b c
///
doc ///
Key
  lieRing
   
Headline
  get the internal ring for representation of Lie elements
SeeAlso
   "Second Lie algebra tutorial"
    mbRing
Usage
   R = L#cache.lieRing 
Inputs
  L: LieAlgebra
Outputs
  R: PolynomialRing
Description
  Text
    The ring $R$ is the internal polynomial ring representation of 
    Lie elements, which can be obtained
    by writing {\tt L#cache.lieRing}. The Lie monomials are represented as
    commutative monomials in this ring. The number of generators in {\tt lieRing}
    is the number of generators in the Lie algebra times the internal counter
    {\tt L#cache.max}, which initially is set to $5$, and is changed to $n+5$ if a computation
    is performed up to degree $n$ with $n\ > $ {\tt L#cache.max}.  
      
  Example
    L=lieAlgebra{a,b}/{a a a b,b b b a}
    dims(1,4,L)
    L#cache.max	
    L#cache.lieRing	      
    dims(1,6,L)
    L#cache.max
    numgens L#cache.lieRing
    dims(1,10,L)
    L#cache.max
    numgens L#cache.lieRing
    

    
///
document {
     Key => {
	     (standardForm, RingElement, LieAlgebra),
     	     (standardForm, List, LieAlgebra),
	  },
     Headline => "get a Lie element in standard form",
     "The set of linear polynomials in " , TT "L#cache.mbRing", " gives a
    representation of Lie elements. The function " , TT "standardForm", " gives 
    back the standard output and " , TT "indexForm",  " goes in the other direction.",
     SeeAlso => {"indexForm","mbRing","normalForm"},
     SYNOPSIS {
	  Usage => "standardForm(r,L)",
     	  Inputs => {
	       "r" => RingElement =>  {"an element in ", TT "L#cache.mbRing"},
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       LieElement => "the corresponding Lie element"
	       }
	  },
     SYNOPSIS {
	  Usage => "standardForm(x,L)",
     	  Inputs => {
	       "x" => List => {"a list of elements in ", TT "L#cache.mbRing"},
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       List => "the list of the corresponding Lie elements"
	       }
	  },
      EXAMPLE{
        "L = lieAlgebra{a,b}",
    	"b3 = basis(3,L)",
    	"Q = L#cache.mbRing",
    	"gens Q",
    	"c3 = {indexForm a a b,indexForm b a b}",
        "standardForm(c3,L)", 
    	"standardForm(mb_{3,0}+2*mb_{3,1},L)", 
    	"indexForm oo"  
	},
     
     }


doc ///
Key
  normalForm
     (normalForm, LieElement)  
Headline
  compute the normal form of a LieElement
Usage
  c = normalForm(x)  
Inputs
  x: LieElement
     $x$ is of type $L$, where $L$ is of type LieAlgebra
SeeAlso
     "standardForm(RingElement,LieAlgebra)"
     indexForm
     (symbol @,LieElement,LieElement)
     
Outputs
  c: LieElement
     the normal form of $x$  
Description
  Text
    A Lie element obtained as a linear combination of iterated Lie products
    of generators has an output that is of 
    normal form. If the Lie element is defined by the "formal" operators,
    then the output may be of non-normal form, and in this case, the 
    normal form is obtained using {\tt normalForm}. 
    
  Example
    L = lieAlgebra{a,b,c}
    x = a b c - 3 c b a +(1/3) b a c
    y = a@b@c/3@c@b@a++(1/3)@b@a@c
    normalForm y
///
doc ///
Key
  innerDerivation
     (innerDerivation, LieElement)
    
Headline
  make the derivation defined by right Lie multiplication by a Lie element 
Usage
  d=innerDerivation(u)
Inputs
  u: LieElement     
Outputs
  d: LieDerivation
     the inner derivation defined by $x$ \ \to\  [$x$,$u$]
SeeAlso
   lieDerivation
   LieDerivation
Description
   
 Example
    L = lieAlgebra{a,b}
    d=innerDerivation a a b
    describe d
    a (a a b) 
    b (a a b)
    
///
doc ///
Key
  (isIsomorphism,LieAlgebraMap)
         
Headline
  whether a Lie map is an isomorphism
SeeAlso
  "trace(ZZ,LieSubSpace,LieAlgebraMap)"
  "isWellDefined(ZZ,LieAlgebraMap)"
  "Holonomy Lie algebras and symmetries"
    
Usage
  b=isIsomorphism(f)
  
Inputs
  f: LieAlgebraMap
      {\tt source f == target f}       
Outputs  
  b: Boolean
     {\tt true} if $f$ is an isomorphism, {\tt false} otherwise            
Description
  Text
    It is checked that $f$ is surjective and well defined 
    (and commutes with the differential).
    It follows from this that $f$ is also injective, 
    since the dimensions of {\tt source f} and {\tt target f} are equal in 
    each degree.
        
  Example
     L=holonomy{{a0,a1,a2},{a0,a3,a4},{a1,a3,a5},{a2,a4,a5}}
     f=map(L,L,{a5,a2,a4,a1,a3,a0})
     isIsomorphism f
     g=map(L,L,{a5,a0,a1,a2,a3,a4})
     isIsomorphism g 
     
     
///
doc ///
Key
  (isSurjective,LieAlgebraMap)
         
Headline
  whether a Lie map is surjective 
SeeAlso
  "isIsomorphism(LieAlgebraMap)"
  "isWellDefined(ZZ,LieAlgebraMap)"
  "image(LieAlgebraMap,LieSubSpace)"
 
  
   
Usage
  f=isSurjective(f)  
  
Inputs
  f: LieAlgebraMap       
Outputs  
  b: Boolean
     {\tt true} if $f$ is surjective, {\tt false} otherwise      
Description
  Text
    The map may not be well defined. 
    Use @TO "isWellDefined(ZZ,LieAlgebraMap)"@ to test 
    if the map is well defined.
    
    
  Example
     F=lieAlgebra{a,b,c}
     L=F/{a b,a b c}
     M=minimalModel(3,L)
     isSurjective map M
     f=map(F,L)
     isSurjective f
     isWellDefined(3,f)
     g=map(L,F)
     isSurjective g
     
     
///
doc ///
Key
  (isWellDefined,ZZ,LieAlgebraMap)
         
Headline
  whether a Lie map is well defined
SeeAlso
    "isWellDefined(ZZ,LieDerivation)"
    "isIsomorphism(LieAlgebraMap)"
    "map(LieAlgebra,LieAlgebra,List)"
    "Homomorphisms and derivations"
        
Usage
  b=isWellDefined(n,f)
  
Inputs
  n: ZZ
      the degree
  f: LieAlgebraMap       
Outputs  
  b: Boolean
     {\tt true} if $f$ is well defined and commutes with 
     the differentials up to degree $n$, 
     {\tt false} otherwise      
Description
  Text
    It is checked that the map $f: M \ \to\  L$ maps the relations in $M$ to 0 up 
    to degree $n$ and
    that $f$ commutes with the differentials in $M$ and $L$. 
    If $n$ is big enough and
    {\tt ideal(M)} is of type {\tt List}, then it is possible to get that $f$ 
    maps all relations
    to 0, which is noted as the message "the map is well defined for all degrees".
    This may happen even if the map does not commute with the differential 
    (see {\tt g} in the example below).   
    
    
  Example
      L=lieAlgebra({a,b},Signs=>1,LastWeightHomological=>true,
	  Weights=>{{1,0},{2,1}})
      F=lieAlgebra({a,b,c},
	  Weights=>{{1,0},{2,1},{5,2}},Signs=>1,LastWeightHomological=>true)
      D=differentialLieAlgebra{0_F,a a,a a a b}
      Q1=D/{a a a a b,a b a b + a c}
      use F
      Q2=F/{a a a a b,a b a b + a c}
      f=map(D,Q1)
      isWellDefined(6,f)
      g=map(Q1,Q2)
      isWellDefined(6,g)
      h=map(Q1,D)
      isWellDefined(6,h)
      
     
///
doc ///
Key
  (isWellDefined,ZZ,LieDerivation)
         
Headline
  whether a Lie derivation is well defined
SeeAlso
    "isIsomorphism(LieAlgebraMap)"
    "isWellDefined(ZZ,LieAlgebraMap)"
     lieDerivation
     "Homomorphisms and derivations"
        
Usage
  b=isWellDefined(n,d)
  
Inputs
  n: ZZ
      the degree
  d: LieDerivation       
Outputs  
  b: Boolean
     {\tt true} if $d$ is well defined up to degree $n$, 
     {\tt false} otherwise            
Description
  Text
    It is checked that the derivation $(d,f): M \ \to\  L$ maps the ideal of 
    relations 
    in $M$ to 0 up 
    to degree $n$. More precisely, if $M=F/I$ where $F$ is free, and $p$ 
    is the projection $F$ \ \to\  $M$,
    then the derivation $(d*p,f*p): F \ \to\  L$ maps 
    $I$ to 0 in degrees $\le\ n$. If $n$ is big enough and $I$ is a list, then
    it is possible to get the information "the derivation is 
    well defined for all degrees".
      
    
    
  Example
      F=lieAlgebra{a,b}
      L=F/{a a a b,b b b a}
      e=euler L
      isWellDefined(4,e)
      d4=lieDerivation{0_L,a b a b a}
      isWellDefined(4,d4)
      d5=lieDerivation{0_L,b a b a b a}
      isWellDefined(4,d5)
      di=innerDerivation(a b a b a)
      isWellDefined(4,di)
      di===d5
           
///
doc ///
Key
  koszulDual
     (koszulDual, QuotientRing)
     (koszulDual, PolynomialRing)

Headline
  compute the Lie algebra whose enveloping algebra is the Koszul dual of a quadratic algebra
Usage
  L = koszulDual(Q)
Inputs
  Q: QuotientRing
       the input type may also be @TO PolynomialRing@ 
    
Outputs
  L: LieAlgebra
SeeAlso
 minimalModel
 extAlgebra
Description
  Text
    The input $Q$ is a quotient of a polynomial algebra 
    by a quadratic ideal (which might be 0). 
    Some of the variables 
    may be declared as {\tt SkewCommutative}. Moreover, the variables may have multi-degrees where the 
    first degree is equal to $1$. The quadratic ideal must be homogeneous
    with respect to the multi-degree and the "skew-degree". The output is the Lie algebra 
    whose enveloping algebra is the Koszul dual of $Q$. 
    
  Example
    R1=QQ[x,y,z, SkewCommutative=>{y,z}]
    I1={x^2,y*z}
    L1=koszulDual(R1/ideal I1)
    describe L1
    E1=extAlgebra(3,L1)
    dims(3,E1)
  
  Text
    Here is an example of a non-Koszul algebra. The table for 
    the Ext-algebra has a non-zero occurrence off the diagonal.
      
  Example
    R2=QQ[x,y,z, SkewCommutative=>{},Degrees=>{{1,1},{1,2},{1,3}}]
    I2=ideal{y^2+x*z,x*y,z^2}
    L2=koszulDual(R2/I2)
    describe L2
    E2=extAlgebra(4,L2)
    dims(4,E2)
     
///

doc ///
Key
  listMultiply
Headline 
  multiplication of lists    
SeeAlso
  (symbol SPACE, LieElement, LieElement)
  (symbol SPACE, ExtElement, ExtElement)
Usage
  m=listMultiply(x,y)
Inputs
  x: List
  y: List
      $x$ and $y$ are lists of Lie elements from the same Lie algebra
      or lists of Ext-elements from the same Ext-algebra
Outputs
  m: List
      of lists       
Description
  Text
    The first list in the output consists of the first element in $x$ multiplied 
    by all elements in $y$. 
  Example
    K=lieAlgebra({a,b,c},Signs=>1)/{a a,b b- 2 c a}
    E=extAlgebra(3,K)
    dims(3,E)
    listMultiply(basis(1,E),basis(2,E))
    basis(2,K)
    listMultiply(basis(1,K),oo)    
///

doc ///
Key
  minimalModel
     (minimalModel, ZZ, LieAlgebra)
Headline
  compute the minimal model 
Usage
  M = minimalModel(d,L)
Inputs
  d: ZZ
     the maximal degree
  L: LieAlgebra
Outputs
  M: LieAlgebra
SeeAlso
 "minimalPresentation(ZZ,LieAlgebra)"
 extAlgebra
 "map(LieAlgebra)"
 (symbol SPACE,ExtElement,ExtElement)
 "Differential Lie algebra tutorial"
 "Minimal models, Ext-algebras and Koszul duals"

Description
  Text
    That $M$ is a minimal model of a Lie algebra $L$ up to degree $d$ means 
    that there exists a differential Lie algebra 
    homomorphism $f: M \ \to\  L$ such that $H(f)$ is an isomorphism up to degree $d$, 
    $M$ is free as a Lie algebra, and the linear part of the differential 
    on $M$ is zero. The homomorphism $f$ may be obtained using @TO "map(LieAlgebra)"@ 
    applied to $M$.
        
    The generators of $M$ yield a basis for 
    the cohomology of $L$, i.e., $Ext_{UL}(k,k)$, where $k$ is the coefficient 
    field of $L$. 
    This skewcommutative algebra may be obtained using @TO extAlgebra@.  
    Multiplication of elements
    in $Ext_{UL}(k,k)$ is obtained using @TO (symbol SPACE,ExtElement,ExtElement)@.
    
    Observe that the homological weight in the cohomology algebra 
    is one higher than the homological weight in the minimal model.
    
  Text
    Since $R$ in the following example is a Koszul algebra it follows that 
    the cohomology algebra of $L$ is equal to $R$. This means that 
    the minimal model of $L$ has generators in each degree $(d,d-1)$.
     
    
  Example 
    R=QQ[x]
    L=koszulDual R
    describe L
    E=extAlgebra(5,L)
    dims(5,E)
    describe minimalModel(5,L)
    
  Text
    In the following example the enveloping algebra of $L1$ has global dimension $2$, 
    which means that the computed
    minimal model is in fact the full minimal model of $L1$.
      
 
  Example
    L1=lieAlgebra{a,b,c}/{a b,a b c}
    M1= minimalModel(3,L1)
    describe M1
    H=lieHomology M1
    dims(6,L1)===dims(6,H)
    
 
  
    
		  
    
/// 
doc ///
Key
  (minimalPresentation,ZZ,LieAlgebra)

Headline
 compute a minimal presentation  
SeeAlso
  minimalModel
  "Minimal models, Ext-algebras and Koszul duals"
   
Usage
  M = minimalPresentation(n,L)
  
Inputs
  n: ZZ
    the maximal  degree
  L: LieAlgebra       
Outputs
  M: LieAlgebra  
Description
  Text
    The optional input given above is not relevant for Lie algebras.
    A minimal set of generators and relations for the Lie algebra $L$ (without
    differential) is given. In general the presentation applies to $H_0(L)$. The example $L$
    below is the Lie algebra of strictly upper triangular $4\times 4$-matrices given by
    its multiplication table on the natural basis.
  
  Example
   L=lieAlgebra({e12,e23,e34,e13,e24,e14},Weights=>{1,1,1,2,2,3})/
    {e12 e34,e12 e13,e12 e14,
     e23 e13,e23 e24,e23 e14,
     e34 e24,e34 e14,e13 e24,
     e13 e14,e24 e14,
     e12 e23 - e13,
     e12 e24 - e14,
     e13 e34 - e14,
     e23 e34 - e24}    
   M=minimalPresentation(3,L)
   describe M
   dims(1,4,M)
       
/// 

document {
     Key => {
	    (random, ZZ, LieAlgebra), 
     	    (random, List, LieAlgebra),  
     	    (random, ZZ, ZZ, LieAlgebra)  
	  },
     Headline => "get a random element of a Lie algebra", 
     TEX "The optional inputs given 
     above are not relevant for Lie algebras. 
     Below is an example of a periodic Lie algebra (a periodization of $sl_3$) with
     five generators and seven random quadratic relations.",
     SYNOPSIS {
	  Usage => "random(d,L)",
     	  Inputs => {
	       "d" => ZZ => "the degree",
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       LieElement => TEX "a random Lie element of degree $d$"
	       }
	  },
    SYNOPSIS {
	  Usage => "random(m,L)",
     	  Inputs => {
	       "m" => List => "the multi-degree",
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       LieElement => TEX "a random Lie element of multi-degree $m$"
	       }
	  },
     SYNOPSIS {
	  Usage => "random(m,n,L)",
     	  Inputs => {
	       "m" => ZZ => "the degree",
	       "n" => ZZ => "the homological degree",
	       "L" => LieAlgebra
	       },
	  Outputs => {
	       LieElement => TEX "a random Lie element of degree $m$ and 
	                     homological degree $n$"
	       }
	  },
      EXAMPLE{
       "L = lieAlgebra({a,b,c,d,e}, Field=>ZZ/7)",
       "Q = L/apply(7,i->random(2,L))",
       "dims(1,8,Q)",
       "random({4,0},Q)"  
	},
     
     }
doc ///
Key
     (source,LieAlgebraMap)
Headline
  get the source of a map
Usage
  S=source(f)
Inputs
  f: LieAlgebraMap        
Outputs
  S: LieAlgebra 
     the source of $f$     
SeeAlso
  "source(LieDerivation)"
  "target(LieAlgebraMap)"
      
 ///
doc ///
Key
     (source,LieDerivation)
Headline
  get the source of a map
Usage
  S=source(d)
Inputs
  d: LieDerivation        
Outputs
  S: LieAlgebra 
     the source of $d$     
SeeAlso
  "source(LieAlgebraMap)"
  "target(LieDerivation)"
      
 ///
doc ///
Key
     (target,LieAlgebraMap)
Headline
  get the target of a map
Usage
  T=target(f)
Inputs
  f: LieAlgebraMap        
Outputs
  T: LieAlgebra 
     the target of $f$     
SeeAlso
  "target(LieDerivation)"
  "source(LieAlgebraMap)"
      
 ///
doc ///
Key
     (target,LieDerivation)
Headline
  get the target of a map
Usage
  T=target(d)
Inputs
  d: LieDerivation        
Outputs
  T: LieAlgebra 
     the target of $d$     
SeeAlso
  "target(LieAlgebraMap)"
  "source(LieDerivation)"
      
 ///

doc ///
Key
     (generators,LieAlgebra)
Headline
  get the generators
Usage
  g=generators(L)
Inputs
  L: LieAlgebra        
Outputs
  g: List 
     a list of generators for $L$ as a Lie algebra      
SeeAlso
  "generators(LieSubSpace)"
  "generators(ExtAlgebra)"
Description
  Text
    The optional input given 
     above is not relevant for Lie algebras. Instead of {\tt generators}
     one may use the abbreviation {\tt gens}. 
  
  Example
      F=lieAlgebra{a,b,c}
      I=lieIdeal{a a b,a a c}
      L=F/I
      gens L
      
 ///
doc ///
Key
     (generators,ExtAlgebra)
Headline
  get the generators
Usage
  g=generators(E)
Inputs
  E: ExtAlgebra        
Outputs
  g: List 
     a basis for $E$ as a vector space      
SeeAlso
  "generators(LieAlgebra)"
  "generators(LieSubSpace)"
Description
  Text
    The optional input given 
     above is not relevant for Lie algebras. Instead of {\tt generators}
     one may use {\tt gens}. Observe that the output 
     is a basis for the Ext-algebra as a vector space, and not as an algebra.
     
  
  Example
      F=lieAlgebra{a,b,c}
      I=lieIdeal{a a b,a a c}
      L=F/I
      E=extAlgebra(3,L)
      gens E
      
 ///
doc ///
Key
     (generators,LieSubSpace)
Headline
  get the generators
Usage
  g=generators(S)
Inputs
  S: LieSubSpace
     an instance of type {\tt LieSubSpace}        
Outputs
  g: List 
     a list of generators for $S$ as a Lie ideal, or as a Lie subalgebra,
     or as a Lie subspace, or $g$ is undefined   
SeeAlso
  "generators(LieAlgebra)"
  "generators(ExtAlgebra)"
     

Description
  Text
    The optional input given 
     above is not relevant for Lie algebras. Instead of {\tt generators}
     one may use the abbreviation {\tt gens}. If $S$ is of 
     type {\tt FGLieIdeal}, then the generators of $S$ are
     the generators of $S$ as an ideal. If $S$ is of 
     type {\tt FGLieSubAlgebra}, then the generators of $S$ are
     the generators of $S$ as a Lie subalgebra. If $S$ is of 
     type {\tt LieSubSpace} given by a finite set of generators,
     then the generators of $S$ are
     the generators of $S$ as a Lie subspace. In all other cases, if
      $S$ is of 
     type {\tt LieSubSpace}, then the function {\tt generators} 
     applied to  $S$ is
     not defined.
  
  Example
      F=lieAlgebra{a,b,c}
      I=lieIdeal{a a b,a a c}
      L=F/I
      gens I
      J=kernel map(L,F)
      gens J
      
 ///

document {
     Key => {
	    (use,LieAlgebra)
  	    	  },
     Headline => "set the generators", 
     "The generator names of the Lie algebra (and Ext-algebra) will get the value
     of the true generators. This is needed when several Lie algebras are used.",
     SYNOPSIS {
	  Usage => "use(L)",
     	  Inputs => {
	       "L" => LieAlgebra
	       }
	  }
     }
doc ///
Key
     (degreeLength,LieAlgebra)
Headline
  get the length of the weight of a generator
Usage
  n=degreeLength(L)

Inputs
  L: LieAlgebra        
Outputs
  n: ZZ 
     the length of an arbitrary weight     
SeeAlso
  "firstDegree(LieElement)"
  weight
  
Description
  Text
    Observe that the weights of the generators in a Lie algebra will
    have an extra component 0 if the option {\tt LastWeightHomological} is {\tt false}. 
  
  Example
      L=lieAlgebra{a,b}
      M=lieAlgebra({a,b},Weights=>{{1,0},{2,1}},LastWeightHomological=>true)
      degreeLength L
      weight\gens L
      degreeLength M
      
 ///
 doc ///
Key
     (diff,LieAlgebra)
Headline
  get the differential of the generators
Usage
  x=diff(L)
  
Inputs
  L: LieAlgebra        
Outputs 
  x: List 
    the list of the differential of the generators     
SeeAlso
  differentialLieAlgebra
  "describe(LieAlgebra)"
 
Description
  
 
  Example     
      L=lieAlgebra({a,b},Signs=>1,Weights=>{{1,0},{2,1}},LastWeightHomological=>true)
      D=differentialLieAlgebra{0_L,a a}
      diff D
      describe D     
 /// 
doc ///
Key
     (ideal,LieAlgebra)
Headline
  get the relations in a Lie algebra 
Usage
  x=ideal(L)

Inputs
  L: LieAlgebra       
Outputs
  x: List
      $x$ may also be of type {\tt LieIdeal} 
SeeAlso
  lieIdeal
  "describe(LieAlgebra)"

Description
  Text
    The output is either
    the list of generators of the ideal or the ideal itself of type
    {\tt LieIdeal}.
  
  Example     
      L=lieAlgebra{a,b}/{a a a b,b b b a}      
      ideal L
      describe L
      F=lieAlgebra{a,b} 
      f=map(L,F)
      J=kernel f
      N=F/J
      ideal N   
 ///
  doc ///
Key
     (numgens,LieAlgebra)
Headline
  get the number of generators
Usage
  x=numgens(L)
  
Inputs
  L: LieAlgebra        
Outputs
  x: ZZ 
     the number of generators of $L$     
SeeAlso
  "describe(LieAlgebra)"
 
Description
  
 
  Example     
      L=lieAlgebra{a,b,c}
      numgens L   
 /// 
doc ///
Key
  zeroDerivation
     (zeroDerivation, LieAlgebra)
  
Headline
  make a derivation from the zero map
Usage
  d=zeroDerivation(L)  
 
SeeAlso
  lieDerivation
  
Inputs
  L: LieAlgebra          
Outputs
   d: LieDerivation   
Description
  Text
    The zero map from $L$ to $L$ is a derivation.
  Example
    L = lieAlgebra{a,b}
    dL=differential L
    d=zeroDerivation L
    dL===d
    
///
doc ///
Key
  zeroMap
     (zeroMap, LieAlgebra,LieAlgebra)
  
Headline
  make the zero map
Usage
  f=zeroMap(M,L)  
 
SeeAlso
  zeroDerivation
  
Inputs
  M: LieAlgebra 
  L: LieAlgebra         
Outputs
   f: LieAlgebraMap   
Description
  Text
    The zero map from $L$ to $M$ is a Lie homomorphism
  Example
    L = lieAlgebra{a,b}
    M = lieAlgebra{a,b}
    f=zeroMap(M,L)
    describe f
    
///
doc ///
Key 
  zeroIdeal
     (zeroIdeal, LieAlgebra)
   
Headline
  make the zero ideal
Usage
  I=zeroIdeal(L)  
 
SeeAlso
  lieIdeal
  
Inputs
  L: LieAlgebra           
Outputs
   I: LieIdeal   
Description
  Text
    The set \{$0_L$\}\  is an ideal in $L$.
  Example
    L = lieAlgebra{a,b}
    I=zeroIdeal L
    J=lieIdeal{0_L,a a}
    I===J
    
///



end  

