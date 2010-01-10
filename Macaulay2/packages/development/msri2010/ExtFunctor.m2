newPackage(
	  "ExtFunctor",
	  Version => "1.0",
	  Date => "January 8, 2010",
	  Authors => {{Name => "Chris Cunningham",
          Email => "cjc258@cornell.edu"}},
	  Headline => "Connecting Homomorphism",
	  DebuggingMode => true
	  )

needsPackage("Functoriality")
export {extension,yonedaExt,toExt,isYonedaEquivalent,yonedaProduct}

mapsAndModules = method()
mapsAndModules(ChainComplex,ZZ,ZZ) := (C,a,b) -> (
    -- This grabs all the maps and modules between
    -- spots a and b.
    D := new ChainComplex;
    D.ring = ring C;
    for i from a to b do (
	 D#i = C_i;
	 if i != a then D.dd#i = C.dd_i; 
	 );
    D
    )

mapsAndModules(ChainComplex) := C -> (
    -- This function just grabs all the maps and modules.

    -- Here we find the highest nonzero entry...
    top := max select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);
    -- and the lowest nonzero entry...
    bottom := min select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);     
    -- Then get everything from bottom to top.
    mapsAndModules(C,bottom,top)
    )

extension = method()
extension(Matrix) := f -> (
     -- This takes an element of Ext^r(M,N)
     -- and gives an exact sequence 
     -- that is an r-extension of N by M.
     
     -- To figure out what r, M and N are,
     -- we look in the cache table for Ext...
     if (target f).cache.?Hom then return chainComplex homomorphism f;
     if not (target f).cache.?Ext then error "extension(Matrix) takes a map R^1 -> Ext^r(M,N); you may want extension(ZZ,Module,Module,Matrix)?";
     EE := (target f).cache.Ext;
     E := target f;
     r := EE#0;
     M := EE#1;
     N := EE#2; 
     if r < 0 then (C:=new ChainComplex;C.ring = M.ring;return C);
     -- Now f:R^1 --> Ext^r(M,N).
     -- If we use yonedaExt, we get a map
     -- FM_r --> N, and then extension is
     -- already written for that.
     extension(r,M,N,yonedaExt(f))
     )

extension(ZZ,Module,Module,Matrix) := (r,M,N,f) -> (
     -- This takes a map FM_r --> N and gives
     -- an exact sequence that is an r-extension
     -- of N by M.
     -- that is an r-extension of N by M.
     if target f != N then error "extension(r,M,N,f) requires f to have target N.";

     if r < 0 then (CC := new ChainComplex;CC.ring = ring M;return CC);
     if r == 0 then return chainComplex map(N,M,f);

     -- If you modify a ChainComplex that was
     -- originally created using "res M", M.cache.resolution
     -- will actually change to reflect your modifications!
     -- So, instead we use mapsAndModules here to only get
     -- some maps and modules from the resolution.
     A  := res(M,LengthLimit => (r+1));
     C  := mapsAndModules(A,0,r-2);
     FM := mapsAndModules(A,0,r);   
     -- Now we put M at the end of both. We intend
     -- to return C eventually, but for now we will
     -- have FM = FM_r --> FM_r-1 --> FM_r-2 --> ... --> FM_0 --> M
     --      C  =  N   -->   0    --> FM_r_2 --> ... --> FM_0 --> M.
     -- We'll fill in C's missing spot soon. 
     if source f != FM_r then error "extension(r,M,N,f) requires f to have source C_r, where C = res M.";

     -- One thing -- we can't make an extension out of it
     -- unless it is in the kernel of the map that comes
     -- from the resolution's differential:
     if f * A.dd_(r+1) != 0 then error "to represent an element of Ext^r(M,N), the map must be zero when composed with (res M).dd_(r+1)";
     
     FM_(-1) = M;
     FM.dd_0 = inducedMap(FM_(-1),FM_0);
     C_(-1) = M;
     if r > 1 then C.dd_0 = inducedMap(C_(-1),C_0);
     C_r = N;
     
     -- The thing we put in C's missing spot
     -- is a quotient of N ++ FM_(r-1) [the push-forward].
     C_(r-1)    = cokernel map(N ++ FM_(r-1),FM_r, f || FM.dd_r);
     C.dd_r     = map(C_(r-1),C_r,    id_N || map(FM_(r-1),C_r,0));
     C.dd_(r-1) = map(C_(r-2),C_(r-1),map(C_(r-2),N,0) | FM.dd_(r-1));
        
     C
     )

yonedaExt = method()
yonedaExt(ChainComplex) := C -> (
     -- This takes an exact sequence that
     -- is an r-extension of M by N, and 
     -- returns a map FM_r --> N.

     -- We are given 0 -> N --> ... --> M -> 0.
     R := C.ring;

     -- Let's see what r, M, and N are
     -- in Ext^r(M,N).
     
     -- This picks out the highest nonzero entry of the complex..
     top := max select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);
     -- and the lowest nonzero entry...
     bottom := min select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);     
     r := top - bottom - 1;
     M := C_bottom;
     N := C_top;

     if r < 0  then return map(N,R^0,0);
     if r == 0 then return inducedMap(M,cover M) // C.dd_top;

     -- We'd like C to work up from 0...
     Cshifted = C[bottom];
     
     -- The plan here is to take part of a 
     -- resolution of M, and get a map from
     -- M_r to N that is a representative
     -- for the element of Ext^r(M,N) associated
     -- to this extension.
     
     -- MM will have a resolution of M, but
     -- shifted up by one so that we can actually
     -- put M at the end of it.
     MM := mapsAndModules(resolution(M,LengthLimit => r),0,r)[-1];
     MM_0 = M;
     MM.dd_1 = inducedMap(MM_0,MM_1);
     
     -- Now when we extend the identity map M --> M,
     -- we get the appropriate thing in slot r+1.
     F := extend(Cshifted,MM,id_M);
     F_(r+1)
     )  

yonedaExt(Matrix) := f -> (
     -- This takes an element of Ext^r,
     -- and gives a map FM_r --> N.

     -- First we check the cache to make
     -- sure f is a map R^1 --> Ext^r(M,N)
     if not (target f).cache.?Ext then error "yonedaExt may only be determined for maps R -> Ext^r(M,N)";
     EE := (target f).cache.Ext;
     E := target f;
     r := EE#0;
     M := EE#1;
     N := EE#2; 
     -- Now f:R^1 --> Ext^r(M,N).
     FM := mapsAndModules(resolution(M,LengthLimit => r),0,r);   
     
     -- Ext prunes everything, so undo that.
     L := target E.cache.pruningMap; -- L is actually ker/im

     -- This function isn't even well-defined!
     -- The map FM_r --> N is only well-defined
     -- up to stuff in the image of a d. Which
     -- are we supposed to return? 
     -- Really any one would work. So: how do we go
     -- from an element of A/B to an element of A,
     -- when we don't care which representative we
     -- choose?
     -- Well, given a map to A/B, it is really given
     -- as a map to the generators of A/B, and those
     -- are the same as the generators of A. So, we
     -- can use the matrix of our map.
     
     -- In short, gens L is the same as gens of the relevant kernel.
     -- gens L == gens kernel Hom((res M).dd_(r+1),N)
     K := image gens L;
     phi := map(K,source f,   E.cache.pruningMap * f);
     
     -- This is now a map R --> K, so we
     -- are almost back to Hom(FM_1,N) 
     Hr := Hom(FM_r, N);
     phir := inducedMap(Hr, K) * phi;
     
     -- Now phir:R^1 -> Hom(FM_r,N), so
     -- homomorphism gives us the map FM_r --> N.
     homomorphism phir      
     )

toExt = method();
toExt(ChainComplex) := C -> (
     -- This takes an r-extension of N by M
     -- and gives an element of Ext^r(M,N).
     -- We'll use the pre-written toExt(ZZ,Module,Module,Matrix)
     -- after we use yonedaExt on C.
     
     -- We are given 0 -> N --> ... --> M -> 0.
     R := C.ring;

     -- Let's see what r, M, and N are
     -- in Ext^r(M,N).
     
     -- This picks out the highest nonzero entry of the complex..
     top := max select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);
     -- and the lowest nonzero entry...
     bottom := min select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);     
     r := top - bottom - 1;
     M := C_bottom;
     N := C_top;
     if (r < 0) then return map(Ext^r(M,N),R^1,0);
     if (r ==0) then return toHom C.dd_top;
           
     toExt(r,M,N,yonedaExt(C))
     )

toExt(ZZ,Module,Module,Matrix) := (r,M,N,f) -> (
     -- This takes a map FM_r --> N and
     -- gives an element of Ext^r(M,N).

     -- The thing is that Ext^r(M,N) has representatives
     -- that are just maps FM_r --> N. So mathematically
     -- this is easy. We just need to find our way to
     -- the actual Ext module.
     FM := mapsAndModules(resolution(M,LengthLimit=>r+1),0,r);
     if source f != FM_r then error "toExt(r,M,N,f) requires f: (res M)_r --> N.";
     if target f != N then error "toExt(r,M,N,f) requires f: (res M)_r --> N.";
     if f * (res M).dd_(r+1) != 0 then error "to represent an element of Ext^r(M,N), the map must be zero when composed with (res M).dd_(r+1)";
      
     -- Here's the proper Ext.
     E := Ext^r(M,N);
     -- The pruningMap in E's cache is what will let us
     -- go between the kernel/image representing Ext and
     -- the actual Ext module.
     L := target E.cache.pruningMap;     
    
     -- First get a map R^1 --> Hom(FM_r,N).
     g := toHom(f);
     
     -- Now we'd like to tack on a map that 
     -- takes Hom(FM_r,N) to the kernel/image 
     -- that is Ext^r(M,N). But there is no
     -- such well-defined map from Hom(FM_r,N) to L.

     -- Instead, as long as g actually maps
     -- into the kernel, we can lift along the
     -- inclusion map to get to kernel, and then
     -- use the actually well-defined map to
     -- kernel/image. 

     -- So we do g // inclusion  to get the map 
     -- from R^1 to kernel d, then inducedMap to get to L.
     d := Hom((res M).dd_(r+1),N);
     inclusion := inducedMap(Hom((res M)_r,N),kernel d);
     h1 := g // inclusion;             -- now h:R^1 --> kernel d
     h := inducedMap(L,kernel d) * h1; -- now h:R^1 --> kernel d / image d
     
     -- Now we go back by the pruningMap, and we are all set.
     (inverse E.cache.pruningMap) * h
     )

isYonedaEquivalent = method()
isYonedaEquivalent(ChainComplex,ChainComplex) := (C,D) -> (
     -- To determine whether two r-extensions of
     -- M by N are equivalent, see if they correspond
     -- to the same element of Ext^r(M,N).

     -- First let's make sure that they are both r-extensions
     -- of M by N (that the r's, M's, and N's match)
     -- This picks out the highest nonzero entry of the complex..
     topC := max select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);
     topD := max select(keys D, i -> if (class i === ZZ) then D_i != 0 else false);
     -- and the lowest nonzero entry...
     bottomC := min select(keys C, i -> if (class i === ZZ) then C_i != 0 else false);     
     bottomD := min select(keys D, i -> if (class i === ZZ) then D_i != 0 else false);     
     if topC - bottomC != topD - bottomD then false;
     if C_topC != D_topD then false;
     if C_bottomC != D_bottomD then false;  
     toExt(C) == toExt(D))    

yonedaProduct = method()
yonedaProduct(Matrix,Matrix) := (f,g) -> (
     -- This takes two elements of Ext
     -- modules and multiplies them.
     R := ring f;
     -- if not source f == R^1 then error "Expected maps R^1 --> Ext^i(M,N)";
     -- if not source g == R^1 then error "Expected maps R^1 --> Ext^i(M,N)";
     if not (target f).cache.?Ext then error "Expected maps R^1 --> Ext^i(M,N)";
     if not (target g).cache.?Ext then error "Expected maps R^1 --> Ext^i(M,N)";
     E1 := target f;
     E2 := target g;
     i := E1.cache.Ext#0;
     L := E1.cache.Ext#1;
     M := E1.cache.Ext#2;
     j := E2.cache.Ext#0;
     N := E2.cache.Ext#2;
     if E2.cache.Ext#1 != M then error "Expected elements of Ext^i(L,M) and Ext^j(M,N).";
     LL := res (L,LengthLimit => i+j) [i];
     MM := res (M,LengthLimit => j);
     
     q := map(MM_0,LL_0,yonedaExt(f));

     F := extend(MM,LL,q);
     toExt(i+j,L,N,map(N,LL_(j),yonedaExt(g) * F_j))  
     );



beginDocumentation()
needsPackage "SimpleDoc";

doc ///
  Key
    ExtFunctor
  Headline
    A package allowing manipulation of elements of Ext modules.
  Description
    Text
       This package allows elements of an Ext module to be represented in three distinct ways:
       
       1. Literally as an element, i.e. a map Ext^r(M,N) $\leftarrow$ R^1, or
       
       2. As an ``r-extension,'' an exact sequence of length r starting with N and ending with M, or
       
       3. As a map from the r^{th} module of a free resolution of M to N which can be extended to a degree i map of chain complexes res N $\leftarrow$ res M.
  
       Corresponding to these three types are three functions:
       
       1. The function @TO toExt@ returns an element of Ext given either of the other representations.
       
       2. The function @TO extension@ returns an r-extension given either of the other representations (analogous to @TO homomorphism@). 
       
       3. The function @TO yonedaExt@ returns the third type, given either of the first two. 
              
       Note that the package does not have a preferred representation for an element of Ext; instead the three above functions each accept any of the representations.
       
       Once these three representations are available, some manipulations are possible. The first is that two different r-extensions sometimes correspond to the same element of Ext: the function @TO isYonedaEquivalent@ checks this. The second is that elements of Ext^i(L,M) and Ext^j(M,N) can compose to form elements of Ext^{i+j}(L,N). The function @TO yonedaProduct@ accomplishes this composition.
  SeeAlso
    Functoriality
///

doc ///
  Key
    extension
    (extension,Matrix)
  Headline
    Get an r-extension corresponding to an element of Ext^r(M,N).
  Usage
    C = extension(f)
  Inputs
    f:Matrix
      an element of Ext^r(M,N), in the form of a map Ext^r(M,N) $\leftarrow$ R^1
  Outputs
    C:ChainComplex
      an exact sequence, an r-extension of N by M corresponding to f.
  Description
   Text
     Enough information is stored in Ext^r(M,N).cache.Ext to recover r, M, and N.   
     
     Here we look at some different extensions of k by k.
   Example
     R = QQ[x,y];
     k = comodule ideal vars R;
   Text
     In this case Ext^1(k,k) is isomorphic to the vector space k^2:
   Example
     E = Ext^1(k,k)
   Text
     So we can look at the first and second generators of the Ext module and get two essentially different extensions:
   Example
     extension E_{0}
     extension E_{1}
   Text
     Since the elements of Ext are module elements, we can add them and use extension to see the effect:
   Example
     extension (E_{0} - 3*E_{1})
   Text

     Ext^2(k,k) is a single copy of k:
   Example
     E2 = Ext^2(k,k)
   Text
     So we can use extension to see a 2-extension of k by k:
   Example
     (extension E2_{0}).dd
  SeeAlso
    (yonedaExt,ChainComplex)
    (yonedaExt,Matrix)
    (toExt,ZZ,Module,Module,Matrix)
    (toExt,ChainComplex)
    (extension,ZZ,Module,Module,Matrix)
///    
doc ///
  Key
    (extension,ZZ,Module,Module,Matrix)
  Headline
    Get an r-extension corresponding to a map N <- (res M)_r.
  Usage
    C = extension(r,M,N,f)
  Inputs
    r:ZZ
    M:Module
    N:Module
    f:Matrix
      a map N $\leftarrow$ (res M)_r representing an element of Ext^r(M,N).
  Outputs
    C:ChainComplex
      an exact sequence; an r-extension of N by M represented by f.
  Description
   Text
     A map N $\leftarrow$ (res M)_r does not contain enough information
     to link itself to Ext^r(M,N), so r, M, and N are all inputs.
     
     In the following example, we get an extension of M by k and demonstrate the module structure on extensions.
   Example
     R = ZZ/101[x,y];
     N = comodule ideal"x2-y3";
     k = comodule ideal"x,y";
   Text
   
     This map from (res k)_1 to N represents an extension:
   Example
     F = map(N,(res k)_1,{{y^2,x}});
     e1 = extension(1,k,N,F)
   Text
   
     We can also find the extension ``y*F'' and the extension ``y*F + F:''
   Example
     G = y*F;
     e2 = extension(1,k,N,G)
     H = F + G;
     e3 = extension(1,k,N,H)
   Text
   
     We can make sure that these different extensions are nonequivalent using @TO isYonedaEquivalent@:
   Example
     isYonedaEquivalent(e1,e2)
  SeeAlso
    (yonedaExt,ChainComplex)
    (yonedaExt,Matrix)
    (toExt,ZZ,Module,Module,Matrix)
    (toExt,ChainComplex)
    (extension,Matrix)
    isYonedaEquivalent
/// 
doc ///
  Key
    yonedaExt
    (yonedaExt,ChainComplex)
  Headline
    Get a map N <- (res M)_r corresponding to an r-extension.  
  Usage
    f = yonedaExt(C)
  Inputs
    C:ChainComplex
      an exact sequence, an r-extension of N by M.
  Outputs
    f:Matrix
      a map N $\leftarrow$ (res M)_r corresponding to C.
  Description
   Text
     Note that such a map will always be zero when composed with the map (res M).dd_{r+1}, since it represents an element of the Ext module.

     Here we construct an extension of k by N by hand, in the form of a chain complex C:
   Example
     R = ZZ/101[x,y];
     N = comodule ideal"x";
     k = comodule ideal"x,y";  
     E1 = cokernel matrix {{0,1,x},{x,y,0}};
     C = chainComplex(map(k,E1,{{0,1}}),map(E1,N,{{1},{0}}))
   Text
     
     Now that we have an extension, we can go up and find the relevant map N $\leftarrow$ (res k)_1.
   Example
     f = yonedaExt C
     source f == ((res k)_1)
   Text
   
     Ext elements are representatives in homology, so composing with (res k).dd_2 will give zero:
   Example  
     f * (res k).dd_2
  SeeAlso
    (yonedaExt,Matrix)
    (toExt,ZZ,Module,Module,Matrix)
    (toExt,ChainComplex)
    (extension,Matrix)
    (extension,ZZ,Module,Module,Matrix)
///
doc ///
  Key
    (yonedaExt,Matrix)
  Headline
    Get a map N <- (res M)_r corresponding to an element of Ext^r(M,N)
  Usage
    f = yonedaExt(g)
  Inputs
    g:Matrix
      an element of Ext^r(M,N), in the form of a map Ext^r(M,N) $\leftarrow$ R^1.
  Outputs
    f:Matrix
      the map N $\leftarrow$ (res M)_r corresponding to g.
  Description
   Text
     Enough information is stored in Ext^r(M,N).cache.Ext to recover r, M, and N.

     In the following example, we show that an element of Ext^1(k,k) corresponds to a particular map from (res k)_1 to k. We start by finding Ext^1(k,k):
   Example
     R = ZZ/101[x,y];
     k = comodule ideal"x,y";
     E = Ext^1(k,k)
   Text
   
     It has two generators, which we will call e1 and e2:
   Example
     e1 = map(E,R^1,{{1},{0}});
     e2 = map(E,R^1,{{0},{1}});
   Text
   
     These will give two different maps from (res k)_1 to k:
   Example
     yonedaExt e1
     yonedaExt e2
   Text
   
     We can even see soome of the module structure of the Ext group in these maps:
   Example
     yonedaExt(3*e1+e2)
  SeeAlso
    (yonedaExt,ChainComplex)
    (toExt,ZZ,Module,Module,Matrix)
    (toExt,ChainComplex)
    (extension,Matrix)
    (extension,ZZ,Module,Module,Matrix)
///
doc ///
  Key
    toExt
    (toExt,ChainComplex)
  Headline
    Get the element of Ext^r(M,N) corresponding to an r-extension.
  Usage
    f = toExt(C)
  Inputs
    C:ChainComplex
      an exact sequence, an r-extension of N by M.
  Outputs
    f:Matrix
      an element of Ext^r(M,N), in the form of a map Ext^r(M,N) $\leftarrow$ R^1.     
  Description
   Text
     The function toExt gives the well-defined element of Ext^r corresponding to an extension,
     so that it can return the same module element when given two different extensions. Below we find two equivalent extensions of k by k.
   Example
     R = QQ[x];
     k = comodule ideal x;
   Text
     Here is one extension:
   Example
     E1 = comodule ideal x^2;
     f = map(E1,k,3*x);
     g = map(k,E1,1);
     C = chainComplex(g,f);
     C.dd
   Text
     Its element of Ext is as follows:
   Example
     e1 = toExt C
     target e1 == Ext^1(k,k) 
   Text

     Here is a second extension, which is equivalent although this is not clear from inspection:
   Example
     D = extension e1;
     D.dd  
     e2 = toExt D
   Text
     We can check equivalence by checking equality in the Ext module:
   Example
     e1 == e2
   Text
   
     Note: the function isYonedaEquivalent accomplishes the above tasks for two extensions.
   Example
     isYonedaEquivalent(C,D)     
  SeeAlso
    (yonedaExt,ChainComplex)
    (yonedaExt,Matrix)
    (toExt,ZZ,Module,Module,Matrix)
    (extension,Matrix)
    (extension,ZZ,Module,Module,Matrix)    
    isYonedaEquivalent
///
doc ///
  Key
    (toExt,ZZ,Module,Module,Matrix)
  Headline
    Get the element of Ext^r(M,N) corresponding to a map N <- (res M)_r.
  Usage
    f = toExt(r,M,N,g)
  Inputs
    r:ZZ
    M:Module
    N:Module
    g:Matrix
      a map N $\leftarrow$ (res M)_r corresponding to an element of Ext^r(M,N).
  Outputs
    f:Matrix
      an element of Ext^r(M,N), in the form of a map Ext^r(M,N) $\leftarrow$ R^1.     
  Description
   Text
     A map N $\leftarrow$ (res M)_r does not contain enough information
     to link itself to Ext^r(M,N), so r, M, and N are all inputs.

     Different maps can give the same element of the Ext module. Below we start by constructing two different maps that will represent elements of Ext^1(k,N):
   Example
     R = ZZ/101[x,y];
     N = comodule ideal"x";
     k = comodule ideal"x,y";  
     f1 = map(N,(res k)_1,{{0,1}})
     f2 = map(N,(res k)_1,{{0,1+x}})
   Text
     If we look at their corresponding module elements of Ext^1(k,N), we get identical results:
   Example
     toExt(1,k,N,f1)
     toExt(1,k,N,f2)
   Text

     The fact that they correspond to the same element of Ext means their corresponding extensions are equivalent:
   Example
     isYonedaEquivalent(extension(1,k,N,f1),extension(1,k,N,f2))
  SeeAlso
    (yonedaExt,ChainComplex)
    (yonedaExt,Matrix)
    (toExt,ChainComplex)
    (extension,Matrix)
    (extension,ZZ,Module,Module,Matrix) 
    (isYonedaEquivalent,ChainComplex,ChainComplex)
///
doc ///
  Key
    isYonedaEquivalent
    (isYonedaEquivalent,ChainComplex,ChainComplex)
  Headline
    Determine whether two extensions of N by M are Yoneda equivalent.
  Usage
    isYonedaEquivalent(C,D)
  Inputs
    C:ChainComplex
      an r-extension of N by M
    D:ChainComplex
      another r-extension of N by M
  Outputs
    :Boolean
  Description
   Text
     Elements of Ext^r(M,N) correspond to r-extensions of N by M, but this correspondence is not one-to-one. 
     To check whether two different r-extensions corrrespond to the same element of Ext^r(M,N), one could use
     @TO toExt@ and check equality in the Ext module, but a shorter way is to use this function. 
     
     The following example verifies the equivalence of two extensions of k by k. 
   Example
     R = QQ[x];
     k = comodule ideal x;
     E1 = comodule ideal x^2;
     f = map(E1,k,3*x);
     g = map(k,E1,1);
   Text
     The chain complex C is an exact sequence that is an extension of k by k:
   Example
     C = chainComplex(g,f);
     C.dd
   Text
     We can get another extension by composing @TO toExt@ with @TO extension@, but this composition is not the identity:
   Example   
     extension toExt C == C
   Text
     The result, however, is an equivalent extension.
   Example
     isYonedaEquivalent(C,extension toExt(C))
  SeeAlso
    (yonedaExt,ChainComplex)
    (yonedaExt,Matrix)
    (toExt,ZZ,Module,Module,Matrix)
    (toExt,ChainComplex)
    (extension,Matrix)
    (extension,ZZ,Module,Module,Matrix)
///
doc ///
  Key
    yonedaProduct
    (yonedaProduct,Matrix,Matrix)
  Headline
    Compose two elements of (possibly different) Ext modules.
  Usage
    h = yonedaProduct(f,g)
  Inputs
    f:Matrix
      an element of Ext^r(L,M), in the form of a map Ext^r(L,M) $\leftarrow$ R^1.
    g:Matrix
      an element of Ext^s(M,N), in the form of a map Ext^s(M,N) $\leftarrow$ R^1.    
  Outputs
    h:Matrix
      an element of Ext^{(r+s)}(L,N), in the form of a map Ext^{(r+s)}(L,N) $\leftarrow$ R^1.
  Description
   Text
     Here we experimentally find that over this particular ring, k's self-Ext algebra in degree 2 
     is generated by the elements of Ext^1(k,k).
     
     We start by creating Ext^1 and Ext^2.
   Example
     R = QQ[a,b,c];
     k = cokernel vars R;
     E1 = Ext^1(k,k)
     E2 = Ext^2(k,k)
   Text
     
     The generators of Ext^1(k,k) will be called f, g, and h:
   Example
     f = E1_{0};
     g = E1_{1};
     h = E1_{2};
   Text
   
     Then we can find what elements of Ext^2(k,k) we get on multiplying the generators of Ext^1(k,k):  
   Example
     yonedaProduct(f,f)
     yonedaProduct(f,g)
     yonedaProduct(g,f)
     yonedaProduct(f,h)
     yonedaProduct(g,h)
   Text
     This covers all of Ext^2, as desired.

     In the following example, the degree 2 part of the self-Ext algebra is not generated by the degree 1 elements:
     We start by creating Ext^1 and Ext^2.
   Example
     R = QQ[a,b,c]/(a^3,b^3,c^3);
     k = cokernel vars R;
     E1 = Ext^1(k,k)
     E2 = Ext^2(k,k)
   Text
     
     The generators of Ext^1(k,k) will again be called f, g, and h:
   Example
     f = E1_{0};
     g = E1_{1};
     h = E1_{2};
   Text
   
     Then only three dimensions of Ext^2(k,k) are spanned by products of elements of Ext^1(k,k):  
   Example
     yonedaProduct(f,f)
     yonedaProduct(f,g)
     yonedaProduct(g,f)
     yonedaProduct(f,h)
     yonedaProduct(g,h)
  Caveat     
    Note that yonedaExt does not return the complete algebra structure on Ext(k,k) - but it does allow the multiplication of any individual elements of the Ext algebra.
  SeeAlso
    (yonedaExt,ChainComplex)
    (yonedaExt,Matrix)
    (toExt,ZZ,Module,Module,Matrix)
    (toExt,ChainComplex)
    (extension,Matrix)
    (extension,ZZ,Module,Module,Matrix)
///