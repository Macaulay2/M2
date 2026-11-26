doc ///
   Key
      SAInfAlg
   Headline
      The class of shifted A-infinity algebras
   Description
      Text
         A shifted A-infinity algebra is assumed to be strictly unital. It consists of a graded $Q$-module $B$ concentrated in degrees $\geq 2$, maps $\mu_n \colon B^{\otimes n} \to B$ of degree $-1$ and a map $B_2 \to Q^1$ of degree $-2$.
      Text
         Shifted A-infinity algebras can be obtained by lifting the multiplication of an algebra to its free resolution.
   SeeAlso
      AInfAlg
///

doc ///
   Key
      sAInfAlg
      (sAInfAlg,Ring)
      (sAInfAlg,QuotientRing)
      (sAInfAlg,Ring,Complex,HashTable,Matrix)
      (sAInfAlg,SAInfAlg)
      (sAInfAlg,AInfAlg)
   Headline
      Constructor for shifted A-infinity algebras
   Usage
      sAInfAlg(Q)
      sAInfAlg(R)
   Inputs
      Q:Ring
      R:QuotientRing
   Outputs
      :SAInfAlg
         viewing a ring as a shifted A-infinity algebra
   Description
      Text
         When the input is a ring it returns the ring viewed as a shifted A-infinity algebra over itself.
      Example
         Q = QQ[x,y]
         sAInfAlg Q
      Text
         When the input is a quotient ring $Q/I$ it returns the ring viewed as a shifted A-infinity algebra over $Q$.
      Example
         Q = QQ[x,y]
         R = Q/ideal(x^2,x*y)
         sAInfAlg Q
         sAInfAlg R
   SeeAlso
      aInfAlg
///

doc ///
   Key
      AInfAlg
   Headline
      The class of A-infinity algebras
   Description
      Text
         An A-infinity algebra is assumed to be strictly unital. It consists of a graded $Q$-module $A$ concentrated in degrees $\geq 1$, maps $\mu_n \colon A^{\otimes n} \to A$ of degree $n-2$ and a map $B_1 \to Q^1$ of degree $-1$.
      Text
         A-infinity algebras can be obtained by lifting the multiplication of an algebra to its free resolution.
   SeeAlso
      SAInfAlg
///

doc ///
   Key
      aInfAlg
      (aInfAlg,Ring)
      (aInfAlg,QuotientRing)
      (aInfAlg,Ring,Complex,HashTable,Matrix)
      (aInfAlg,AInfAlg)
      (aInfAlg,SAInfAlg)
   Headline
      Constructor for A-infinity algebras
   Usage
      aInfAlg(Q)
      aInfAlg(R)
   Inputs
      Q:Ring
      R:QuotientRing
   Outputs
      :AInfAlg
         viewing a ring as an A-infinity algebra
   Description
      Text
         When the input is a ring it returns the ring viewed as an A-infinity algebra over itself.
      Example
         Q = QQ[x,y]
         aInfAlg Q
      Text
         When the input is a quotient ring $Q/I$ it returns the ring viewed as an A-infinity algebra over $Q$.
      Example
         Q = QQ[x,y]
         R = Q/ideal(x^2,x*y)
         aInfAlg Q
         aInfAlg R
   SeeAlso
      sAInfAlg
///

doc ///
   Key
      (aInfAlg,Ring,HashTable)
   Headline
      Constructor for A-infinity algebras from maps given on generators
   Usage
      aInfAlg(Q,mm)
   Inputs
      Q:Ring
      mm:HashTable
   Outputs
      :AInfAlg
         A-infinity algebra over Q with higher multiplications given by mm
   Description
      Text
         This constructs the A-infinity algebra where $\mu_n \colon A^{\otimes n} \to A$ is given by the maps on the generators $e_{i,j}$ of $A_i$.
      Example
         Q = QQ[x,y,z]
         M = hashTable {
            {{1,0}} => {x^2},
            {{1,1}} => {-y*z},
            {{1,2}} => {x*y+z^2},
            {{1,3}} => {-x*z},
            {{1,4}} => {y^2},
            {{2,0}} => {0, -y, 0, 0, -z},
            {{2,1}} => {y, 0, -x, -z, 0},
            {{2,2}} => {0, x, 0, -y, 0},
            {{2,3}} => {0, z, y, 0, -x},
            {{2,4}} => {z, 0, 0, x, 0},
            {{3,0}} => {x^2, -y*z, x*y+z^2, -x*z, y^2},
            {{1,0},{1,1}} => {0, 0, x, 0, y},
            {{1,0},{1,2}} => {0, -x, 0, 0, -z},
            {{1,0},{1,3}} => {0, 0, 0, 0, x},
            {{1,0},{1,4}} => {0, -y, z, -x, 0},
            {{1,1},{1,0}} => {0, 0, -x, 0, -y},
            {{1,1},{1,2}} => {x, 0, 0, -z, 0},
            {{1,1},{1,3}} => {0, 0, z, 0, 0},
            {{1,1},{1,4}} => {y, 0, 0, 0, 0},
            {{1,2},{1,0}} => {0, x, 0, 0, z},
            {{1,2},{1,1}} => {-x, 0, 0, z, 0},
            {{1,2},{1,3}} => {0, -z, 0, 0, y},
            {{1,2},{1,4}} => {-z, 0, 0, -y, 0},
            {{1,3},{1,0}} => {0, 0, 0, 0, -x},
            {{1,3},{1,1}} => {0, 0, -z, 0, 0},
            {{1,3},{1,2}} => {0, z, 0, 0, -y},
            {{1,3},{1,4}} => {x, 0, y, 0, 0},
            {{1,4},{1,0}} => {0, y, -z, x, 0},
            {{1,4},{1,1}} => {-y, 0, 0, 0, 0},
            {{1,4},{1,2}} => {z, 0, 0, y, 0},
            {{1,4},{1,3}} => {-x, 0, -y, 0, 0},
            {{1,0},{2,0}} => {1_Q},
            {{1,1},{2,1}} => {1_Q},
            {{1,2},{2,2}} => {1_Q},
            {{1,3},{2,3}} => {1_Q},
            {{1,4},{2,4}} => {1_Q},
            {{2,0},{1,0}} => {1_Q},
            {{2,1},{1,1}} => {1_Q},
            {{2,2},{1,2}} => {1_Q},
            {{2,3},{1,3}} => {1_Q},
            {{2,4},{1,4}} => {1_Q}
         };
         aInfAlg(Q,M)
   SeeAlso
      aInfAlg
///

doc ///
   Key
      SAInfMod
   Headline
      The class of shifted A-infinity modules
   Description
      Text
         A shifted A-infinity module of a shifted A-infinity algebra $B$ is assumed to be strictly unital. It consists of a graded $Q$-module $G$ and maps $\mu_n \colon B^{\otimes (n-1)} \otimes G \to G$ of degree $-1$.
   SeeAlso
      SAInfAlg
      AInfMod
///

doc ///
   Key
      sAInfMod
      (sAInfMod,SAInfAlg,Complex,HashTable)
      (sAInfMod,SAInfMod)
      (sAInfMod,SAInfAlg)
      (sAInfMod,AInfMod)
   Headline
      Constructor for shifted A-infinity modules
   Usage
      sAInfMod(B,Gnat,mm)
      sAInfMod(G)
      sAInfMod(B)
   Inputs
      B:SAInfAlg
      Gnat:Complex
      mm:HashTable
      G:SAInfMod
   Outputs
      :SAInfMod
   Description
      Text
         When the input is a shifted A-infinity algebra, this method returns the shifted A-infinity algebra viewed as a shifted A-infinity module over itself.
      Example
         kk = ZZ/101
         S = kk[a,b,c]
         R = S/((ideal a^2)*ideal(a,b,c)) -- a simple 3 variable Golod ring
         B = sAInfAlgModel R
         sAInfMod B
   SeeAlso
      aInfMod
///

doc ///
   Key
      AInfMod
   Headline
      The class of A-infinity modules
   Description
      Text
         An A-infinity module of an A-infinity algebra $A$ is assumed to be strictly unital. It consists of a graded $Q$-module $F$ and maps $\mu_n \colon A^{\otimes (n-1)} \otimes F \to F$ of degree $n-2$.
   SeeAlso
      AInfAlg
      SAInfMod
///

doc ///
   Key
      aInfMod
      (aInfMod,AInfAlg,Complex,HashTable)
      (aInfMod,AInfMod)
      (aInfMod,AInfAlg)
      (aInfMod,SAInfMod)
   Headline
      Constructor for A-infinity modules
   Usage
      sAInfMod(A,Fnat,mm)
      sAInfMod(F)
      sAInfMod(A)
   Inputs
      A:AInfAlg
      Fnat:Complex
      mm:HashTable
      F:AInfMod
   Outputs
      :AInfMod
   Description
      Text
         When the input is an A-infinity algebra, this method returns the A-infinity algebra viewed as an A-infinity module over itself.
      Example
         kk = ZZ/101
         S = kk[a,b,c]
         R = S/((ideal a^2)*ideal(a,b,c)) -- a simple 3 variable Golod ring
         A = aInfAlg sAInfAlgModel R
         aInfMod A
   SeeAlso
      sAInfMod
///

doc ///
   Key
      (symbol ==,SAInfAlg,SAInfAlg)
   Headline
      Checks whether two shifted A-infinity algebras are equal
   Usage
      B1 == B2
   Inputs
      B1:SAInfAlg
      B2:SAInfAlg
   Outputs
      :Boolean
         true when {\tt B1} and {\tt B2} are equal
   Description
      Text
         Two shifted A-infinity algebras are equal when the graded modules, maps to the ground ring, and higher multiplications agree. Missing components represent zero within the known range. Equality also compares the precision of the structures.
   SeeAlso
///

doc ///
   Key
      (symbol ==,AInfAlg,AInfAlg)
   Headline
      Checks whether two A-infinity algebras are equal
   Usage
      A1 == A2
   Inputs
      A1:AInfAlg
      A2:AInfAlg
   Outputs
      :Boolean
         true when {\tt A1} and {\tt A2} are equal
   Description
      Text
         Two A-infinity algebras are equal when the graded modules, maps to the ground ring, and higher multiplications agree. Missing components represent zero within the known range. Equality also compares the precision of the structures.
   SeeAlso
///

doc ///
   Key
      (symbol ==,SAInfMod,SAInfMod)
   Headline
      Checks whether two shifted A-infinity modules are equal
   Usage
      G1 == G2
   Inputs
      G1:SAInfMod
      G2:SAInfMod
   Outputs
      :Boolean
         true when {\tt G1} and {\tt G2} are equal
   Description
      Text
         Two shifted A-infinity modules are equal when they are defined over the same A-infinity algebra, the underlying graded modules are equal and they have the same higher multiplication maps. Missing components represent zero within the known range. Equality also compares the precision of the structures.
   SeeAlso
///

doc ///
   Key
      (symbol ==,AInfMod,AInfMod)
   Headline
      Checks whether two A-infinity modules are equal
   Usage
      F1 == F2
   Inputs
      F1:AInfMod
      F2:AInfMod
   Outputs
      :Boolean
         true when {\tt F1} and {\tt F2} are equal
   Description
      Text
         Two A-infinity modules are equal when they are defined over the same A-infinity algebra, the underlying graded modules are equal and they have the same higher multiplication maps. Missing components represent zero within the known range. Equality also compares the precision of the structures.
   SeeAlso
///

doc ///
   Key
      (complex,SAInfAlg)
      (complex,AInfAlg)
      (complex,SAInfMod)
      (complex,AInfMod)
   Headline
      Returns the complex underlying the (shifted) A-infinity structure
   Usage
      complex(B)
   Inputs
      B:SAInfAlg
   Outputs
      :Complex
   Description
   SeeAlso
///

doc ///
   Key
      (symbol SPACE,SAInfMod,Array)
      (symbol SPACE,AInfMod,Array)
   Headline
      Shift a (shifted) A-infinity module
   Usage
      G[i]
   Inputs
      G:SAInfMod
      :Array
   Outputs
      :SAInfMod
   Description
   SeeAlso
///

doc ///
    Key
        TensorOrder
        [sAInfAlg,TensorOrder]
        [aInfAlg,TensorOrder]
        [sAInfMod,TensorOrder]
        [aInfMod,TensorOrder]
    Headline
        Restrict the number of computed higher multiplications
    Usage
        sAInfAlg(B, TensorOrder => n)
        aInfAlg(A, TensorOrder => n)
    Description
        Text

        Example
            kk = ZZ/101
            S = kk[x,y,z,u]
            I = ideal(u^3, x*y^2, (x+y)*z^2, x^2*u+z*u^2, y^2*u+x*z*u, y^2*z+y*z^2)
            R = S/I
            B = sAInfAlgModel R
            sAInfAlg(B,TensorOrder => 2)
    SeeAlso
        sAInfAlg
        aInfAlg
///

doc ///
   Key
      checkStasheff
      (checkStasheff,SAInfAlg,ZZ)
      (checkStasheff,SAInfAlg)
      (checkStasheff,AInfAlg,ZZ)
      (checkStasheff,AInfAlg)
      (checkStasheff,SAInfMod,ZZ)
      (checkStasheff,SAInfMod)
      (checkStasheff,AInfMod,ZZ)
      (checkStasheff,AInfMod)
   Headline
      Checks whether the (shifted) A-infinity structure satisfies the Stasheff identities
   Usage
      checkStasheff(B,n)
      checkStasheff(B)
   Inputs
      B:SAInfAlg
      n:ZZ
   Outputs
      :Boolean
   Description
      Text
         An explicit sparse structure treats omitted components as zero. For a partial model the checker tests identities through the recorded arity and target-degree bounds. Asking for an arity above the recorded precision produces an error. Passing the available checks certifies exactly that range. A module check tests its identities relative to its algebra. Check the algebra separately when both objects have been supplied.
   SeeAlso
      sAInfAlgModel
///

doc ///
Key
 sAInfAlgModel
 sAInfModModel
 (sAInfAlgModel, Complex)
 (sAInfAlgModel, PolynomialRing)
 (sAInfAlgModel, Ring, QuotientRing)
 (sAInfAlgModel, QuotientRing)
 (sAInfModModel, Module)
 (sAInfModModel, SAInfAlg, Module)
 (sAInfModModel, SAInfAlg, Module, Complex)
 [sAInfAlg, LengthLimit]
 [aInfAlg, LengthLimit]
 [sAInfAlgModel, LengthLimit]
 [sAInfAlgModel, Check]
 [sAInfAlgModel, TensorOrder]
 [sAInfModModel, LengthLimit]
 [sAInfModModel, Check]
 [sAInfModModel, TensorOrder]
Headline
 A-Infinity algebra and module structures on free resolutions
Usage
 mR = sAInfAlgModel R
 mX = sAInfModModel(mR, X)
Inputs
 R:Ring
  of the form S/I, where S is a polynomial ring
 mR:SAInfAlg
  Shifted A-infinity algebra structure on resolution of R
 LengthLimit => ZZ
  Construct A-infinity structure to specified homological degree
 Check => Boolean
  Verifies that the lifts in the construction were successful
 TensorOrder => ZZ
  Restrict the arity of A-infinity structures produced
Outputs
 mR:SAInfAlg
  A-infinity algebra structure on res coker presentation R
 mX:SAInfMod
  A-infinity module structure over mR on res pushForward(map(R,S),M)
Description
 Text
   Given an S-free resolution of  R = S/I, set B = A_+[-1] (so that B_m = A_(m-1) for m >= 2, B_i = 0 for i<2),
   and differentials have changed sign.

   The A-infinity algebra structure
   is a sequence of degree -1 maps

   mR#"mm"#u: B_(u_1)**..**B_(u_t) -> B_(sum u -1), for sum u <= 2 + (pd_S R), and thus,
   since each u_i>= 2, for t <= 1 + (pd_S R)//2.

   where u is a List of integers \geq 2, such that

   mR#"mm"#{v}: B_v -> B_(v-1) is the differential of B,

   mR#"mm"#{v_1,v_2} is the multiplication (which is a homotopy B**B \to B lifting the degree -2 map
   d**1 - 1**d: B_2**B_2 \to B_1 (which induces 0 in homology)

   mR#"mm"#u for n>2 is a homotopy for the negative of the sum of degree -2 maps of the form
   (+/-) mR(1**...** 1 ** mR ** 1 **..**),
   inserting m into each possible (consecutive) sub product, and i = 2...n-1.
   Here m_1 represents the differential both of B and of B^(**n).

   Given mR, a similar description holds for the A-infinity module structure mX on the
   S-free resolution of an R-module X.

   LengthLimit bounds the target degree of the computed higher multiplications. TensorOrder bounds their arity. The unary maps retain the differential of the available resolution. A computed prefix records when later terms or operations are unknown.

   The overload sAInfAlgModel(C) uses a supplied free resolution of a cyclic quotient. The overload sAInfModModel(B,X,C) uses a supplied ambient free resolution of X. The caller supplies the identification of its bottom homology with X. Check verifies the lifting equations; definite homology and generator mismatches are also rejected for checked, supplied resolutions.

   For a relative quotient, sAInfAlgModel(Q,R) computes over the chosen ambient ring Q. A finite LengthLimit selects a prefix when the relative resolution continues indefinitely.
 Example
   S = ZZ/101[a,b,c]
   R = S/(ideal(a)*ideal(a,b,c))
   mR = sAInfAlgModel R;
   res coker presentation R
   mR#"natural"
   mR#"mm"#{2,2}
   X = coker map(R^2,R^{2:-1},matrix{{a,b},{b,c}})
   mX = sAInfModModel(mR,X)
 Text
   Jesse Burke showed how to use mR,mX to make an R-free resolution
 Example
   betti burkeResolution(X,8)
   betti res(X, LengthLimit =>8)
   Y = image presentation X
   burkeResolution(Y,8)
SeeAlso
 sAInfAlgModel
References
 Jesse Burke, Higher Homotopies and Golod Rings. arXiv:1508.03782v2, October 2015.
Caveat
 Inhomogeneous model construction uses local resolutions at the point selected with setMaxIdeal on the ambient polynomial ring. Local computations certify exactness only at that point. A local model may contain contractible summands.
///

doc ///
Key
 burkeModule
 (burkeModule, Ring, Complex, Complex, ZZ)
 (burkeModule, Ring, SAInfAlg, SAInfMod, ZZ)
Headline
 construct a single term of the Burke resolution
Usage
 burkeModule(R,B,G,i)
Description
 Text
  Returns the direct sum in homological degree i over R. Each summand is labeled by the homological degrees of its algebra factors followed by the degree of its module factor.
SeeAlso
 burkeDifferential
///


doc ///
Key
 (burkeDifferential,Ring,SAInfAlg,SAInfMod,ZZ)
Headline
 assemble one differential from typed algebra and module operations
Usage
 burkeDifferential(T,B,G,i)
Inputs
 T:Ring
 B:SAInfAlg
 G:SAInfMod
 i:ZZ
Outputs
 :Matrix
Description
 Text
  Uses the operations of $B$ and its module $G$ to form the map from Burke term $i$ to term $i-1$ over $T$.
  The ring $T$ can be the ambient ring or its quotient. A missing operation beyond the recorded bounds gives an error.
SeeAlso
 burkeModule
 burkeDifferential
///

doc ///
Key
 (picture,ChainComplex)
Headline
 display labeled blocks in the differentials of a chain complex
Usage
 picture C
Inputs
 C:ChainComplex
Outputs
 :Net
SeeAlso
 picture
///
