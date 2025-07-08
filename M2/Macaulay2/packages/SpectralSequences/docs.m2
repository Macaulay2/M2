undocumented {
    (NewMethod, Page),
    (degree, Page),
    (describe, Page),
    (describe, PageMap),
    (describe, SpectralSequence),
    (expression, SpectralSequence),
    (net, FilteredComplex),
    (net, Page),
    (net, PageMap),
    (net, SpectralSequence),
    (net, SpectralSequencePage),
    (page, SpectralSequencePage),
    (ring, Page),
    (spots, PageMap),
    (support, FilteredComplex),
    (support, PageMap),
    (support, SpectralSequencePage),
    (symbol _, Page, List),
    (symbol _, PageMap, List),
    pageMap,

    -- (ring, Page),
    -- (spectralSequencePageMap, FilteredComplex, ZZ),
    -- (spots, PageMap),
    -- (support, FilteredComplex)
    -- (support, PageMap),
    -- (support, SpectralSequencePage),
    -- (symbol _, Page, List),
    -- (symbol _, PageMap, List),
    -- ReducedHomology,
    -- Shift,
    -- sourcePruningMap, targetPruningMap,
    -- spectralSequencePageMap,
    }

document {
    Key => SpectralSequences,
    Headline => "a package for working with filtered complexes and spectral sequences",
    "Spectral sequences, although notoriously technical, are very useful in applications---especially when they degenerate quickly.
    By contrast, little is known about their general structure when they fail to degenerate quickly.
    Even in cases when the terms in the spectral sequences are well understood, the maps remain mysterious.
    One of the motivations behind this package is to shed light on spectral sequences through examples.
    Its purpose is to allow for effective calculations of particular kinds of spectral sequences.
    As one general situation, which illustrates some capabilities of this package,
    let k be a computable field, S a k-algebra of finite type, C a bounded chain complex of
    finitely generated S-modules, and FC a bounded ascending filtration of C.  This package is
    capable of computing, under these assumptions, the spectral sequence---especially the differentials on each page---determined by FC.",
    -- SUBSECTION "Contributors",
    -- "The following people have generously contributed code or worked on our code.",
    -- UL {
    -- {HREF("","")},
    -- {HREF("","")},
    -- {HREF("","")},
    -- {HREF("","")},
    -- {HREF("","")},},
    SUBSECTION "Constructors used in this package",
    UL {
        TO "How to make filtered complexes from chain complex maps", --"How to work with filtered complexes", --"Making filtered chain complexes from chain complex maps",
        TO "Filtrations and tensor product complexes",
        TO "Filtrations and homomorphism complexes",
        TO "Filtered complexes and simplicial complexes",
        TO "I-adic filtrations of chain complexes and their spectral sequences",
        --  TO "Spectral sequences from filtered chain complexes",
        },
    SUBSECTION "Other examples which illustrate this package",
    UL {
        TO "Computing the Serre Spectral Sequence associated to a Hopf Fibration",
        TO "Balancing Tor",
        TO "Spectral sequences and hypercohomology calculations",
        TO "Spectral sequences and connecting morphisms",
        TO "Spectral sequences and non-Koszul syzygies",
        TO "A spectral sequence which fails to degenerate quickly",
        TO "Seeing Cancellations",
        TO "Edge homomorphisms",
        TO "Examples of change of rings Spectral Sequences",
        },
    SUBSECTION "More easy topological examples",
    UL {
        TO "Identifying anti-podal points of the two sphere", --"The quotient map SS ^2 --> RR PP ^2",--"More topological examples",
        TO "The fibration of the Klein Bottle over the sphere with fibers the sphere", --"The fibration SS^1 --> Klein Bottle --> SS^1",
        TO "The trivial fibration over the sphere with fibers the sphere",
        }, -- SS^1 --> SS^1 x SS^1 --> SS^1"},
    }

doc ///
  Key
    "Examples of filtered complexes and spectral sequences"
  Headline
    How to use this package
  Description
    Text
      Here is a list of some examples which illustrate various parts of this package.

      {\bf First examples which show how to use this package}

      $\bullet$ @TO"How to make filtered complexes from chain complex maps"@

      $\bullet$ @TO"Filtrations and tensor product complexes"@

      $\bullet$ @TO"Filtrations and homomorphism complexes"@

      $\bullet$ @TO"Filtered complexes and simplicial complexes"@

      $\bullet$ @TO"I-adic filtrations of chain complexes and their spectral sequences"@

      {\bf More elaborate examples which illustrate this package}

      $\bullet$ @TO"Computing the Serre Spectral Sequence associated to a Hopf Fibration"@

      $\bullet$ @TO"Balancing Tor"@

      $\bullet$ @TO"Spectral sequences and hypercohomology calculations"@

      $\bullet$ @TO"Spectral sequences and connecting morphisms"@

      $\bullet$ @TO"Spectral sequences and non-Koszul syzygies"@

      $\bullet$ @TO"Seeing Cancellations"@

      $\bullet$ @TO"A spectral sequence which fails to degenerate quickly"@

      $\bullet$ @TO"Edge homomorphisms"@

      $\bullet$ @TO"Examples of change of rings Spectral Sequences"@

      {\bf More easy topological examples}

      $\bullet$ @TO"Identifying anti-podal points of the two sphere"@

      $\bullet$ @TO"The fibration of the Klein Bottle over the sphere with fibers the sphere"@

      $\bullet$ @TO"The trivial fibration over the sphere with fibers the sphere"@
///

doc ///
  Key
    "I-adic filtrations of chain complexes and their spectral sequences"
  Description
    Text
      By multiplying a chain complex by successive powers of an ideal we obtain a filtered complex.
    Example
      B = QQ[a..d]
      J = ideal vars B
      C = res monomialCurveIdeal(B,{1,3,4})
      K = filteredComplex(J,C,4)
    Text
      Here are some higher pages of the associated spectral sequence:
    Example
      E = prune spectralSequence K
      -- E^2
      -- E^3
      -- E^3 .dd
      E^4
      E^4 .dd
///

doc ///
  Key
    "Filtered complexes and simplicial complexes"
  Description
    Text
      We can make a filtered complex from a nested list of simplicial
      complexes:
    Example
      A = QQ[x,y,z,w];
      F2D = simplicialComplex {x*y*z, w*z};
      F1D = simplicialComplex {x*y, w};
      F0D = simplicialComplex {x,w};
      K = filteredComplex{F2D, F1D, F0D}
    Text
      The resulting spectral sequence takes the form:
    Example
      E = prune spectralSequence K;
      E^0
      E^0 .dd
      E^1
      E^1 .dd
      E^2
      E^2 .dd
      E^infinity
    Text
      If we want the homology of the complex to be the non-reduced homology
      of the simplicial complex we set the ReducedHomology option to false:
    Example
      k = filteredComplex({F2D, F1D, F0D}, ReducedHomology => false)
    Text
      The resulting spectral sequence takes the form:
    Example
      e = prune spectralSequence k;
      e^0
      e^0 .dd
      e^1 .dd
      e^2
      e^2 .dd
      e^infinity
  SeeAlso
    "How to make filtered complexes from chain complex maps"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
///

doc ///
  Key
    "Filtrations and homomorphism complexes"
  Description
    Text
      Let $S$ be a commutative ring and let
      $B : \dots \rightarrow B_{i} \rightarrow B_{i - 1} \rightarrow \cdots $ and
      $C : \dots \rightarrow C_{i} \rightarrow C_{i - 1} \rightarrow \cdots $ be chain complexes.

      For all integers $p$ and $q$ let $K_{p,q} := Hom_S(B_{-p}, C_q)$,
      let $d'_{p,q} : K_{p,q} \rightarrow K_{p - 1, q}$ denote the homorphism
      $ \phi \mapsto \partial^B_{-p + 1}  \phi$, and let
      $d^{''}_{p,q} : K_{p,q} \rightarrow K_{p, q - 1} $ denote the homorphism
      $\phi \mapsto (-1)^p \partial^C_q  \phi$.

      The chain complex $Hom(B, C)$ is given by
      $ Hom(B, C)_k := \prod_{p + q = k} Hom_S(B_{-p}, C_q) $
      and the differentials
      by $ \partial := d^{'} + d^{''} $;
      it carries two natural ascending filtrations $F' ( Hom(B, C) )$ and $F''( Hom(B, C))$.

      The first is obtained by
      letting $F'_n (Hom(B, C))$ be the chain complex determined by setting
      $F'_n (Hom(B, C))_k := \prod_{p + q = k , p \leq n} Hom_S(B_{-p}, C_q)$
      and the differentials $\partial := d' + d''$.

      The second is obtained by letting $F''_n (Hom(B, C)) := \prod_{p + q = k , q \leq n} Hom_S(B_{-p}, C_q)$
      and the differentials $\partial := d' + d''$.

      In {\it Macaulay2}, using this package, $F'$ and $F''$ as defined above are
      computed as illustrated in the following example, by using
      Hom(filteredComplex B, C) or Hom(B,filteredComplex C).

    Example
      A = QQ[x,y,z,w];
      B = res monomialCurveIdeal(A, {1,2,3});
      C = res monomialCurveIdeal(A, {1,3,4});
      F' = Hom(filteredComplex B, C)
      F'' = Hom(B,filteredComplex C)
    Text
      Notice that the display above shows that these are different filtered complexes.
      The resulting spectral sequences take the form:
    Example
      E' = prune spectralSequence F';
      E'' = prune spectralSequence F'' ;
      E' ^0
      E' ^ 0 .dd
      E'' ^0
      E'' ^1
///

doc ///
  Key
    "Filtrations and tensor product complexes"
  Description
    Text
      Let $S$ be a commutative ring and let
      $B : \dots \rightarrow B_{i} \rightarrow B_{i - 1} \rightarrow \dots $ and
      $C : \dots \rightarrow C_{i} \rightarrow C_{i - 1} \rightarrow \dots $ be chain complexes.

      For all integers $p$ and $q$ let $K_{p,q} := B_p \otimes_S C_q$, let $d'_{p,q} : K_{p,q} \rightarrow K_{p - 1, q}$
      denote the homorphism
      $\partial^B_{p} \otimes 1$, and let $d''_{p,q} : K_{p,q} \rightarrow K_{p, q - 1} $ denote the
      homorphism $(-1)^p \otimes \partial_q^C $.

      The chain complex $B \otimes_S C$ is given by
      $ (B \otimes_S C)_k := \oplus_{p + q = k} B_p \otimes_S C_q$
      and the differentials by $\partial := d' + d''$. It carries two natural ascending filtrations
      $F'B \otimes_S C$ and $F'' B \otimes_S C$.

      The first is obtained by letting
      $F'_n (B \otimes_S C)$ be the chain complex determined by setting
      $F'_n (B \otimes_S C)_k := \oplus_{p + q = k , p \leq n} B_{p} \otimes_S C_q$
      and the differentials $\partial := d' + d''$.

      The second is obtained by letting
      $F''_n (B \otimes_S C)$ be the chain complex determined by setting
      $F''_n (B \otimes_S C)_k := \oplus_{p + q = k , q \leq n} B_{p} \otimes_S C_q$
      and the differentials $\partial := d' + d''$.

      In Macaulay2 we can compute these filtered complexes as follows.
      --To obtain the chain complex $F' B \otimes_S C$ we use the syntax
      --$(filteredComplex B)\otimes C$.
      --To obtain the chain complex $ F'' B \otimes_S C$ we use the syntax
      --$ B\otimes(filteredComplex C)$.
    Example
      A = QQ[x,y,z,w];
      B = res monomialCurveIdeal(A,{1,2,3});
      C = res monomialCurveIdeal(A,{1,3,4});
      F' = (filteredComplex B) ** C
      F'' = B ** (filteredComplex C)
    Text
      The pages of the resulting spectral sequences take the form:
    Example
      E' = prune spectralSequence F';
      E'' = prune spectralSequence F'';
      E' ^0
      E' ^ 1
      E'' ^0
      E'' ^1
  SeeAlso
    "Balancing Tor"
///

doc ///
  Key
    "How to make filtered complexes from chain complex maps"
  --  Headline
  --    the most primitive way to make filtered complexes
  Description
    Text
      We describe the most primitive way to create filtered complexes.

      Let $C$ be a chain complex and consider a list of
      chain complex maps $\{\phi_n, \phi_{n - 1}, \dots, \phi_0  \}$
      with properties that $C$ is the target of $\phi_i$, for $0 \leq i \leq n$, and the
      image of $\phi_{i-1}$ is a subchain complex of the image of $\phi_i$, for $1 \leq i \leq n$.
      Given this input data we produce an ascending filtered chain complex $FC$
      with the properties that $F_k C = C$ for $k \geq n + 1$ and $F_k C = image \phi_k$, for $k = 0, \dots, n$.

      We now illustrate how this is done in two easy examples.
      We first make three chain complexes $C$, $D$, and $E$,
      two chain complex maps, $d : D \rightarrow C$
      and $e : E \rightarrow C$, and then
      compute the resulting filtration of $C$.
--      When then consider a boundary case by considering the filtered complex obtained
--      from a single chain complex map, that is the identity of $C$.
    Text
       Let's make our chain complexes $C$, $D$, and $E$.
    Example
      R = QQ[x,y,z,w] ;
      c2 = matrix(R,{{1},{0}}) ;
      c1 = matrix(R,{{0,1}}) ;
      C = complex({c1,c2})
      D_2 = image matrix(R,{{1}});
      D_1 = image matrix(R,{{1,0},{0,0}});
      D_0 = image matrix(R,{{1}});
      D = complex({inducedMap(D_0,D_1,C.dd_1),inducedMap(D_1,D_2,C.dd_2)})
      E_2 = image matrix(R,{{0}});
      E_1 = image matrix(R,{{1,0},{0,0}});
      E_0 = image matrix(R,{{1}});
      E = complex({inducedMap(E_0,E_1,C.dd_1),inducedMap(E_1,E_2,C.dd_2)})
    Text
      We now make our chain complex maps.
    Example
      d = map(C,D,apply(spots C, i-> inducedMap(C_i,D_i,id_C _i)))
      e = map(C,E,apply(spots C, i->inducedMap(C_i,E_i, id_C _i)))
    Text
      We can check that these are indeed chain complex maps:
    Example
      isWellDefined d
      isWellDefined e
    Text
      Now, given the list of chain complex maps $\{d, e\}$, we obtain
      a filtration of $C$ by:
    Example
      K = filteredComplex({d,e})
    Text
       If we want to specify a minimum filtration degree we can use the Shift option.
    Example
      L = filteredComplex({d,e},Shift =>1)
      M = filteredComplex({d,e},Shift =>-1)
--    Text
--       We now explain a boundary case in which the list consists of a single map $\{\phi_0\}$.
--    Example
--        P = filteredComplex {id_C}
--        P_1
///

---
-- Examples
---

doc ///
  Key
    "A spectral sequence which fails to degenerate quickly"
  -- Headline
  --   nonzero maps on higher page numbers
  Description
    Text
      The following example is taken from p. 127, Fig 7.2 of
      Zomorodian's {\it Topology for computing}.  In that figure, a filtration of a suitable
      simplicial complex is pictured.  Here we compute the associated spectral sequence.
      As we will see below, the spectral sequences has nonzero maps on higher page numbers.
    Example
       A = ZZ [s,t,u,v,w] ;
       D0 = simplicialComplex {s} ;
       D1 = simplicialComplex {s,t} ;
       D2 = simplicialComplex {s,t,u} ;
       D3 = simplicialComplex {s*t, u} ;
       D4 = simplicialComplex {s*t, u, v} ;
       D5 = simplicialComplex {s*t, u, v, w} ;
       D6 = simplicialComplex {s*t, s*w ,u, v} ;
       D7 = simplicialComplex {s*t, s*w ,t * w, u, v} ;
       D8 = simplicialComplex {s*t, s*w ,t * w, u * v} ;
       D9 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v} ;
       D10 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u} ;
       D11 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w} ;
       D12 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u} ;
       D13 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w} ;
       D14 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w} ;
       D15 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u} ;
       D16 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v} ;
       D17 = simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w} ;
       L = reverse {D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15, D16, D17} ;
       K = filteredComplex (L, ReducedHomology => false) ;
       E = prune spectralSequence K ;
       E^0
       E^1 .dd
       E^8
       E^8 .dd
       E^9
       E^9 .dd
       E^infinity
       prune HH K_infinity
///

doc ///
  Key
    "Seeing Cancellations"
  -- Headline
  --  nonzero maps on higher page numbers
  Description
    Text
      Here we give an example of a spectral sequence that takes n+2 steps to degenerate, where
      n is the embedding dimension of the ring.  We present this when n = 2 but the user with
      computational power can easily do a bigger case.
    Example
      S = ZZ/101[x,y];
      I = ideal(x^2,x*y,y^2);
      R = S/I;
      kR = coker vars R;
      kS = coker vars S;
      CS = res kS;
      CR = res(kR,LengthLimit=>6);
      CS' = CS**R;
      E = prune spectralSequence (CS' ** filteredComplex CR);
    Text
      Since this spectral sequence only consists of $k$ vector spaces, and all are generated
      in a single degree, for ease of presentation we may as well just look at the rank and degree
      which we can easily encode in a matrix with $rt^d$ encoding the rank $r$ and degree $d$ of each
      vector space $E_{i,j}$.
    Example
      use ZZ[t]
      easyPresentation = (P,n,m) -> (
	  transpose matrix apply(n,
	      i -> apply(m,
		  j -> (length (P_{i,j}))*t^(
		      if (L = unique flatten degrees P_{i,j})!= {} then first L else 0)
		  )
	      ));
    Text
      To see what we're going for, we compute the E_{infinity} page and also some earlier pages.
      Notice that it's clear that all terms except those in the top row of the matrix must eventually
      disappear, but for this to happen, there must a map of the right degree mapping to them.
    Example
      easyPresentation(E_infinity,6,3)
      easyPresentation(E_1,6,3)
      easyPresentation(E_2,6,3)
      easyPresentation(E_3,6,3)
      length image ((E_2).dd_{3,0})
      length image (E_3).dd_{3,0}
    Text
      The final two computations are meant to explain that the copy of $k^8$ in degree 3 that
      appears on the $E_1$ cancels in two steps via an $E_2$ map with $k^6$ and via an $E_3$ map with a $k^2$.
///

doc ///
  Key
    "Identifying anti-podal points of the two sphere"
  Description
    Text
        In this example we compute the spectral sequence arising from
        the quotient map
        $\mathbb{S}^2 \rightarrow \mathbb{R} \mathbb{P}^2$,
        given by identifying anti-podal points.
        This map can be realized by a simplicial map along the lines of Exercise 27, Section 6.5 of Armstrong's
        book {\it Basic Topology}.
        In order to give a combinatorial picture of the quotient map
        $\mathbb{S}^2 \rightarrow \mathbb{R} \mathbb{P}^2$,
        given by identifying anti-podal points, we
        first make an appropriate simplicial realization of $\mathbb{S}^2$.
        Note that we have added a few barycentric coordinates.
    Example
        S = ZZ[v1,v2,v3,v4,v5,v6,v15,v12,v36,v34,v46,v25];
        twoSphere = simplicialComplex {v3*v4*v5, v5*v4*v15, v15*v34*v4, v15*v34*v1, v34*v1*v6, v34*v46*v6, v36*v46*v6, v3*v4*v46, v4*v46*v34, v3*v46*v36, v1*v6*v2, v6*v2*v36, v2*v36*v12,v36*v12*v3, v12*v3*v5, v12*v5*v25, v25*v5*v15, v2*v12*v25, v1*v2*v25, v1*v25*v15};
    Text
       We can check that the homology of the simplicial complex twoSphere agrees with that of $\mathbb{S}^2$.
    Example
        C = naiveTruncation(complex twoSphere, 1)
        prune HH C
    Text
        We now write down our simplicial complex whose topological realization
        is $\mathbb{R} \mathbb{P}^2$.
    Example
        R = ZZ[a,b,c,d,e,f];
        realProjectivePlane = simplicialComplex {a*b*c, b*c*d, c*d*e, a*e*d, e*b*a, e*f*b, d*f*b, a*f*d, c*f*e,a*f*c};
    Text
        Again we can check that we've entered a simplicial complex
        whose homology agrees with that of the real projective plane.
    Example
        B = naiveTruncation(complex realProjectivePlane, 1)
        prune HH B
    Text
        We now compute the fibers of the anti-podal quotient map
        $\mathbb{S}^2 \rightarrow  \mathbb{R} \mathbb{P}^2$.
        The way this works for example is:
        $a = v3 ~ v1, b = v6 ~ v5, d = v36 ~ v15, c = v4 ~ v2,
        e = v34 ~ v12, f = v46 ~ v25$

        The fibers over the vertices of $\mathbb{R} \mathbb{P}^2$ are:
      Example
        F0twoSphere = simplicialComplex {v1,v3,v5,v6, v4,v2, v36,v15, v34,v12, v46,v25}
      Text
        The fibers over the edges of $\mathbb{R}\mathbb{P}^2$ are:
      Example
        F1twoSphere = simplicialComplex {v3*v4, v1*v2,v3*v5, v1*v6,v4*v5, v2*v6, v5*v15, v6*v36, v4*v34, v2*v12, v15*v34, v36*v12, v1*v15, v3*v36, v46*v34, v25*v12, v6*v34, v5*v12, v6*v46, v5*v25, v36*v46, v15*v25, v3*v46, v1*v25, v4*v15, v2*v36, v1*v34, v3*v12, v4*v46, v25*v2}
      Text
        The fibers over the faces is all of $\mathbb{S}^2$.
      Example
        F2twoSphere = twoSphere
      Text
        The resulting filtered complex is:
      Example
        K = filteredComplex({F2twoSphere, F1twoSphere, F0twoSphere}, ReducedHomology => false)
      Text
        We now compute the resulting spectral sequence.
      Example
        E = prune spectralSequence K
        E^0
        E^1
        E^0 .dd
        E^1 .dd
        E^2
        E^2 .dd
///

doc///
  Key
    "The fibration of the Klein Bottle over the sphere with fibers the sphere"
  Description
      Text
        In this example we give a simplicial realization of the fibration
        $\mathbb{S}^1 \rightarrow {\rm Klein Bottle} \rightarrow \mathbb{S}^1$.
        To give a simplicial realization of this fibration we first make a simplicial
        complex which gives a triangulation of the Klein Bottle.
        The triangulation of the Klein Bottle that we use has 18 facets and is, up to relabling, the triangulation of the Klein bottle given
        in Figure 6.14 of Armstrong's book {\it Basic Topology}.
      Example
        S = ZZ[a00,a10,a20,a01,a11,a21,a02,a12,a22];
        -- there will be 18 facets of Klein Bottle
        Delta = simplicialComplex {a00*a10*a02, a02*a12*a10, a01*a02*a12, a01*a12*a11, a00*a01*a11, a00*a11*a10, a10*a12*a20, a12*a20*a22, a11*a12*a22, a11*a22*a21, a10*a11*a21, a10*a21*a20, a20*a22*a00, a22*a00*a01, a21*a22*a01, a21*a02*a01, a20*a21*a02, a20*a02*a00}
      Text
        We can check that the homology of this simplicial complex agrees with that
        of the Klein Bottle:
      Example
        C = naiveTruncation(complex Delta, 1)
        prune HH C
      Text
        Let $S$ be the simplicial complex with facets $\{A_0 A_1, A_0 A_2, A_1 A_2\}$.  Then $S$ is a triangulation of $S^1$.  The simplicial map
        $\pi : \Delta \rightarrow S$ given by $\pi(a_{i,j}) = A_i$ is a combinatorial realization of the fibration
        $S^1 \rightarrow {\rm Klein Bottle} \rightarrow S^1$.
        The subsimplicial complexes of $\Delta$, which arise from the
        the inverse images of the simplicies of $S$, are described below.
      Example
        F1Delta = Delta
        F0Delta = simplicialComplex {a00*a01,a01*a02,a00*a02,a10*a11,a10*a12,a11*a12,a21*a20,a20*a22,a21*a22}
      Text
        The resulting filtered chain complex is:
      Example
        K = filteredComplex({F1Delta, F0Delta}, ReducedHomology => false)
     Text
        The resulting spectral sequence is:
     Example
        E = prune spectralSequence K
        E^0
        E^0 .dd
        E^1
        E^1 .dd
        E^2
     Text
        Note that the spectral sequence is abutting to what it should --- the integral
        homology of the Klein bottle
///

doc ///
  Key
    "The trivial fibration over the sphere with fibers the sphere"--"The trivial fibration over the sphere with fiber the sphere"
  Description
    Text
      In this example we compute the spectral sequence associated to the
      trivial fibration $\mathbb{S}^1 \rightarrow  \mathbb{S}^1 x \mathbb{S}^1 \rightarrow  \mathbb{S}^1$,
      where the map is given by one of the projections.  To give a simplicial realization of this fibration we first make a simplicial complex
      which gives a triangulation of $\mathbb{S}^1 \times \mathbb{S}^1$.  The simplicial complex that we construct
      is the triangulation of the torus given in Figure 6.4 of Armstrong's book
      {\it Basic Topology} and has 18 facets.
    Example
      S = ZZ/101[a00,a10,a20,a01,a11,a21,a02,a12,a22];
      --S = ZZ[a00,a10,a20,a01,a11,a21,a02,a12,a22]; for some reason get an error
      -- if use ZZ coefs...
      -- there will be 18 facets of SS^1 x SS^1
      Delta = simplicialComplex {a00*a02*a10, a02*a12*a10, a01*a02*a12, a01*a11*a12, a00*a01*a11, a00*a10*a11, a12*a10*a20, a12*a20*a22, a11*a12*a22, a11*a22*a21, a10*a11*a21, a10*a21*a20, a20*a22*a00, a22*a00*a02, a21*a22*a02, a21*a02*a01, a20*a21*a01, a20*a01*a00}
    Text
      We can check that the homology of the simplicial complex
      $\Delta$ agrees with that of the torus
      $\mathbb{S}^1 \times \mathbb{S}^1 $
    Example
      C = naiveTruncation(complex Delta, 1)
      prune HH C
    Text
      Let $S$ be the simplicial complex with facets $\{A_0 A_1, A_0 A_2, A_1 A_2\}$.  Then $S$ is a triangulation of $S^1$.  The simplicial map
      $\pi : \Delta \rightarrow S$ given by $\pi(a_{i,j}) = A_i$ is a combinatorial realization of the trivial fibration
      $\mathbb{S}^1 \rightarrow \mathbb{S}^1 \times \mathbb{S}^1 \rightarrow \mathbb{S}^1$.
      We now make subsimplicial complexes arising from the filtrations of the
      inverse images of the simplicies.
    Example
      F1Delta = Delta;
      F0Delta = simplicialComplex {a00*a01, a01*a02, a00*a02, a10*a11,a11*a12,a10*a12, a21*a20,a21*a22,a20*a22};
      K = filteredComplex({F1Delta, F0Delta}, ReducedHomology => false) ;
    Text
      The resulting spectral sequence is:
    Example
      E = prune spectralSequence K
      E^0
      E^0 .dd
      E^1
      E^1 .dd
      E^2
///

doc ///
  Key
    "Spectral sequences and non-Koszul syzygies"
  Description
    Text
      We illustrate some aspects of the paper
      "A case study in bigraded commutative algebra" by Cox-Dickenstein-Schenck.
      In that paper, an appropriate term on the E_2 page of a suitable
      spectral sequence corresponds to non-koszul syzygies.

      Using our indexing conventions, the E^2_{3,-1} term will be what the
      $E^{0,1}_2$ term is in their paper.

      We illustrate an instance of the non-generic case for non-Koszul syzygies.
      To do this we look at the three polynomials used in their Example 4.3.
      The behaviour that we expect to exhibit is predicted by their Proposition 5.2.
    Example
       R = QQ[x,y,z,w, Degrees => {{1,0},{1,0},{0,1},{0,1}}];
       B = ideal(x*z, x*w, y*z, y*w);
       p_0 = x^2*z;
       p_1 = y^2*w;
       p_2 = y^2*z+x^2*w;
       I = ideal(p_0,p_1,p_2);
       -- make the frobenious power of the irrelevant ideal
       B = B_*/(x -> x^2)//ideal;
       -- need to take a large enough power.
       -- it turns out that 2 is large enough for this example
       G = res image gens B;
       F = koszulComplex gens I;
       K = Hom(G, filteredComplex(F));
       E = prune spectralSequence K;
       E^1
       E^2
    Text
       The degree zero piece of the module $E^2_{3,-1}$ twisted by $R((2,3))$ below
       shows that there is a $1$-dimensional space of non-Koszul syzygies
       of bi-degree $(2,3)$.  This is what is predicted by the paper.
    Example
       E^2_{3,-1}
       basis({0,0}, E^2_{3, -1} ** R^{{2, 3}})
       E^2 .dd_{3, -1}
--       E^2 .dd
       basis({0,0}, image E^2 .dd_{3,-1} ** R^{{2,3}})
       basis({0,0}, E^2_{1,0} ** R^{{2,3}})
       -- this shows that there is a 1 dimensional space of non-Koszul syzygies of bi-degree (2,3)
       -- which is also what is predicted by the paper.
    Text
       The degree zero piece of the module $E^2_{3,-1}$ twisted by $R((6,1))$ below
       shows that there is a $1$-dimensional space of non-Koszul syzygies of bi-degree
       $(6,1)$.  This is also what is predicted by the paper.
    Example
       basis({0,0}, E^2 _{3, -1} ** R^{{6,1}})
       -- this shows that there is a 1 dimensional space of non-Koszul syzygies of bi-degree (6,1)
       -- this is what is predicted by the paper.
       isIsomorphism(E^2 .dd_{3, -1})
///

doc ///
  Key
    "Spectral sequences and connecting morphisms"
  Description
    Text
      If $0 \rightarrow A \rightarrow B \rightarrow C \rightarrow 0$ is a
      short exact sequence of chain complexes then the connecting morphism
      $H_i(C) \rightarrow H_{i - 1}(A)$ can realized as a suitable map
      on the $E^1$ of a spectral sequence determined by a suitably defined
      two step filtration of $B$.

      Here we illustrate this realization in a concrete situation:  we
      compute the connecting morphism $H^i(X, F) \rightarrow H^{i + 1}(X, G)$
      arising from a short exact sequence
      $0 \rightarrow G \rightarrow H \rightarrow F \rightarrow 0$ of sheaves
      on a smooth toric variety $X$.

      More specifically we let $X = \mathbb{P}^1 \times \mathbb{P}^1$ and use multigraded commutative algebra
      together with spectral sequences to compute the connecting
      morphism $H^1(C, OO_C(1,0)) \rightarrow H^2(X, OO_X(-2,-3))$ where
      $C$ is a general divisor of type $(3,3)$ on $X$.  This connecting morphism is an
      isomorphism.
    Example
       R = ZZ/101[a_0..b_1, Degrees=>{2:{1,0},2:{0,1}}]; -- PP^1 x PP^1
       M = intersect(ideal(a_0,a_1),ideal(b_0,b_1)) ; -- irrelevant ideal
       M = M_*/(x -> x^5)//ideal ; -- Suitably high Frobenius power of M
       G = res image gens M ;
       b = complex R^{{1,0}} -- make line bundle a chain complex
       a = complex R^{{-2,-3}}
       -- make the map OO(-2, -3) --> OO(1,0)
       f = randomComplexMap(b, a, Degree => 0)
       K = filteredComplex ({Hom(G,f)}) ; -- the two step filtered complex we want
       E = prune spectralSequence K ;
    Text
      The degree zero piece of the map $E^1 .dd_{1, -2}$ below is the desired connecting
      morphism $H^1(C, OO_C(1,0)) \rightarrow H^2(X, OO_X(-2,-3))$.
    Example
       E^1 .dd_{1,-2} -- the connecting map HH^1(C, OO_C(1,0)) --> HH^2(X, OO_X(-2,-3))
       basis({0,0}, image E^1 .dd_{1,-2})  -- image 2-dimensional
       basis({0,0}, ker E^1 .dd_{1,-2}) -- map is injective
       basis({0,0}, target E^1 .dd_{1,-2}) -- target 2-dimensional
       basis({0,0}, source E^1 .dd_{1,-2}) -- source 2 dimensional
    Text
      An alternative way to compute the connecting morphism is
    Example
       prune connectingMorphism(Hom(G, f), - 2) ;
       prune connectingMorphism(Hom(G, f), - 2) == E^1 .dd_{1, -2}
///

doc ///
  Key
    "Spectral sequences and hypercohomology calculations"
  --  Headline
  --    using spectral sequences to compute hypercohomology
  Description
    Text
      If $\mathcal{F}$ is a coherent sheaf on a smooth toric variety $X$
      then multigraded commutative algebra can be used to compute
      the cohomology groups $H^i(X, \mathcal{F})$.

      Indeed if $B$ is the irrelevant ideal of $X$ then the cohomology group
      $H^i(X, \mathcal{F})$ can be realized as the degree zero piece of the multigraded
      module
      $Ext^i(B^{[l]}, F)$ for sufficiently large $l$; here $B^{[l]}$ denotes
      the $l$th Frobenius power of $B$ and $F$ is any multigraded module whose
      corresponding sheaf on $X$ is $\mathcal{F}$.

      Given the fan of
      $X$ and $F$ a sufficiently large power of $l$ can be determined effectively.
      We refer to sections 2 and 3 of the paper
      "Cohomology on Toric Varieties and Local Cohomology with Monomial Supports"
      for more details.

      In this example, we consider
      the case that $X = \mathbb{P}^1 \times \mathbb{P}^1$ and
      $F = \mathcal{O}_C(1,0)$ where
      $C$ is a general divisor of type $(3,3)$ on $X$.
      In this setting, $H^0(C,F)$ and $H^1(C, F)$ are both $2$-dimensional
      vector spaces.

      We first make the multi-graded coordinate ring of
      $\mathbb{P}^1 \times \mathbb{P}^1$, the
      irrelevant ideal, and a sufficentily high Frobenus power of the
      irrelevant ideal needed for our calculations.  Also the complex $G$
      below is a resolution of the irrelevant ideal.
    Example
        -- C \subseteq PP^1 x PP^1 type (3,3)
        -- Use hypercohomology to compute HH OO_C(1,0)
        R = ZZ/101[a_0..b_1, Degrees=>{2:{1,0},2:{0,1}}]; -- PP^1 x PP^1
        B = intersect(ideal(a_0,a_1),ideal(b_0,b_1)) ; -- irrelevant ideal
        B = B_*/(x -> x^5)//ideal ; -- Sufficentily high Frobenius power
        G = res image gens B ;
    Text
      We next make the ideal, denoted by $I$ below, of a general divisor of type $(3,3)$
      on $\mathbb{P}^1 \times \mathbb{P}^1$.  Also the chain complex
      $F$ below is a resolution of this ideal.
    Example
        I = ideal random(R^1, R^{{-3,-3}}) ; -- ideal of C
        F = res comodule I
    Text
      To use hypercohomology to compute the cohomology groups of the
      line bundle $\mathcal{O}_C(1,0)$ on $C$ we twist the
      complex $F$ above by a line of ruling and then
      make a filtered complex whose associated spectral
      sequence abuts to the desired cohomology groups.
    Example
        K = Hom(G , filteredComplex (F ** R^{{1,0}})) ; -- Twist F by a line of ruling and make filtered complex whose ss abuts to HH OO_C(1,0)
        E = prune spectralSequence K ; --the spectral sequence degenerates on the second page
        E^1
        E^2 ; -- output is a mess
    Text
      The cohomology groups we want are obtained as follows.
    Example
        basis({0,0}, E^2_{0,0}) --  == HH^0 OO_C(1,0)
        basis({0,0}, E^2_{1,-2}) --  == HH^1 OO_C(1,0)
  SeeAlso
    "Spectral sequences and connecting morphisms"
    "Spectral sequences and non-Koszul syzygies"
///

doc ///
  Key
    "Computing the Serre Spectral Sequence associated to a Hopf Fibration"
  Description
    Text
      We compute the Serre Spectral Sequence associated to the Hopf Fibration
      $S^1 \rightarrow S^3 \rightarrow S^2$.
      This example is made possible by the minimal triangulation of this fibration given in the paper
      "A minimal triangulation of the Hopf map and its application"
      by K.V. Madahar and K.S Sarkaria. Geom Dedicata, 2000.
    Text
      We first make the relevant simplicial complexes described on page 110 of the paper.
      The simplicial complex $S3$ below is a triangulation of $S^3$.
    Example
      B = QQ[a_0..a_2,b_0..b_2,c_0..c_2,d_0..d_2];
      l1 = {a_0*b_0*b_1*c_1,a_0*b_0*c_0*c_1,a_0*a_1*b_1*c_1,b_0*b_1*c_1*d_1,b_0*c_0*c_1*d_2,a_0*a_1*c_1*d_2,a_0*c_0*c_1*d_2,b_0*c_1*d_1*d_2};
      l2 = {b_1*c_1*c_2*a_2,b_1*c_1*a_1*a_2,b_1*b_2*c_2*a_2,c_1*c_2*a_2*d_1,c_1*a_1*a_2*d_2,b_1*b_2*a_2*d_2,b_1*a_1*a_2*d_2,c_1*a_2*d_1*d_2};
      l3 = {c_2*a_2*a_0*b_0,c_2*a_2*b_2*b_0,c_2*c_0*a_0*b_0,a_2*a_0*b_0*d_1,a_2*b_2*b_0*d_2,c_2*c_0*b_0*d_2,c_2*b_2*b_0*d_2,a_2*b_0*d_1*d_2};
      l4 = {a_0*b_0*b_1*d_1,a_0*b_1*d_0*d_1,b_1*c_1*c_2*d_1,b_1*c_2*d_0*d_1,a_0*a_2*c_2*d_1,a_0*c_2*d_0*d_1};
      l5 = {a_0*b_1*d_0*d_2,a_0*a_1*b_1*d_2,b_1*c_2*d_0*d_2,b_1*b_2*c_2*d_2,a_0*c_2*d_0*d_2,a_0*c_0*c_2*d_2};
      S3 = simplicialComplex(join(l1,l2,l3,l4,l5));
    Text
      We identify the two sphere $S^2$ with the simplicial complex $S2$ defined by the facets $\{abc, abd, bcd, acd \}$.
      The Hopf fibration $S^1 \rightarrow S^3 \rightarrow S^2$ is then realized by the simplicial map
      $p: S3 \rightarrow S2$ defined by $a_i \mapsto a$, $b_i \mapsto b$, $c_i \mapsto c$, and $d_i \mapsto d$.

      We now explain how to construct the filtration of $S3$ obtained by
      considering the $k$-skeletons of this fibration.

      The simplicial complex $F1S3$ below is the subsimplicial complex of $S3$ obtained by considering the
      inverse images of the $1$-dimensional faces of the simplicial complex $S2$
      We first describe the simplicial complex $F1S3$ in pieces.

      For example, to compute $f1l1$ below, we observe that the inverse image of $ab$ under $p$ is
      $a_0b_0b_1, a_0a_1b_1$ etc. All of these inverse images have been computed by hand previously.
    Example
      f1l1 = {a_0*b_0*b_1,a_0*a_1*b_1,a_0*c_0*c_1,a_0*a_1*c_1,a_0*a_1*d_2,d_1*d_2,b_0*b_1*c_1,b_0*c_0*c_1,b_0*b_1*d_1,b_0*d_1*d_2,c_1*d_1*d_2,c_0*c_1*d_2};
      f1l2 = {b_1*a_1*a_2,b_1*b_2*a_2,c_1*c_2*a_2,c_1*a_1*a_2,a_1*a_2*d_2,a_2*d_1*d_2,b_1*c_1*c_2,b_1*b_2*c_2,b_1*b_2*d_2,d_1*d_2,c_1*d_1*d_2,c_1*c_2*d_1};
      f1l3 = {a_2*a_0*b_0,a_2*b_2*b_0, c_2*a_2*a_0,c_2*c_0*a_0,a_2*a_0*d_1,a_2*d_1*d_2,b_2*b_0*c_2,c_2*c_0*b_0,b_2*b_0*d_2,b_0*d_1*d_2,c_2*c_0*d_2,d_1*d_2};
      f1l4 = {a_0*b_0*b_1,a_0*a_2,a_0*a_2*c_2,c_1*c_2,a_0*d_0*d_1,a_0*a_2*d_1,b_1*c_1*c_2,b_0*b_1,b_0*b_1*d_1,b_1*d_0*d_1,c_1*c_2*d_1,c_2*d_0*d_1}
      f1l5 = {a_0*a_1*b_1,b_1*b_2,a_0*c_0*c_2,a_0*a_1,a_0*d_0*d_2,a_0*a_1*d_2,b_1*b_2*c_2,c_0*c_2,b_1*d_0*d_2,b_1*b_2*d_2,c_2*d_0*d_2,c_0*c_2*d_2};
      F1S3 = simplicialComplex join(f1l1, f1l2, f1l3, f1l4, f1l5);
    Text
      The simplicial complex $F0S3$ below is the subsimplicial complex of $F1S3$ obtained by
      considering the inverse images of the $0$-dimensional faces of the simplicial complex $S2$.
      Again we describe this simplicial complex in pieces.
    Example
      f0l1 = {a_0*a_1,b_0*b_1,c_0*c_1,d_1*d_2};
      f0l2 = {a_1*a_2,b_1*b_2,c_1*c_2,d_1*d_2};
      f0l3 = {a_0*a_2,b_0*b_2,c_0*c_2,d_1*d_2};
      f0l4 = {a_0*a_2,b_0*b_1,c_1*c_2,d_0*d_1};
      f0l5 = {a_0*a_1,b_1*b_2,c_0*c_2,d_0*d_2};
      F0S3 = simplicialComplex(join(f0l1,f0l2,f0l3,f0l4,f0l5));
    Text
      The simplicial complex $S3$ is obtained by considering the
      inverse images of the $2$ dimensional faces of $S2$.

      To compute a simplicial version of
      the Serre spectral sequence for the
      $S^1 \rightarrow S^3 \rightarrow S^2$
      correctly, meaning that the spectral sequence takes the form
      $E^2_{p,q} = H_p(S^2,H_q(S^1,QQ))$, we need to
      use non-reduced homology.
    Example
      K = filteredComplex({S3,F1S3,F0S3}, ReducedHomology => false);
    Text
      We now compute the various pages of the spectral sequence.
      To make the output
      intelligible we prune the spectral sequence.
    Example
      E = prune spectralSequence K;
    Example
      E0 = E^0
    Text
      Here are the maps.
    Example
      E0.dd
    Text
      Now try the $E^1$ page.
    Example
      E1 = E^1
    Text
      Here are the maps.
    Example
      E1.dd
    Text
      Now try the $E^2$ page.
    Example
      E2 = E^2
    Text
      Here are the maps.
    Example
      E2.dd
    Text
      Note that the modules on the $E^2$ page appear to have been computed correctly.
      The statement of the Serre spectral sequence, see for example Theorem 1.3 p. 8 of
      Hatcher's Spectral Sequence book, asserts that $E^2_{p,q} = H_p(S^2,H_q(S^1,QQ))$.
      This is exactly what we obtained above.  Also the maps on the $E^2$ page also seem
      to be computed correctly as the spectral sequence will abut to the homology of $S^3$.
    Example
      E3 = E^3
      E3.dd
    Text
      Thus the E^3 page appears to have been computed correctly.
///

doc ///
  Key
    "Balancing Tor"
  Description
    Text
      To balance Tor we first need to make some modules over a ring.
    Example
      A = QQ[x,y,z,w];
      M = monomialCurveIdeal(A,{1,2,3});
      N = monomialCurveIdeal(A,{1,3,4});
    Text
      To compute $Tor^A_i(M,N)$ we resolve the modules, tensor appropriately,
      and then take homology.
    Example
      K = res M
      J = res N
    Text
      The spectral sequence that computes $Tor^A_i(M,N)$ by tensoring
      $K$ with $N$ and taking homology is given by
    Example
      E = prune spectralSequence((filteredComplex K) ** J)
    Text
      The spectral sequence that computes $Tor^A_i(M,N)$ by tensoring
      $J$ with $M$ and taking homology is given by
    Example
      F = prune spectralSequence((K ** (filteredComplex J)))
    Text
      Let's compute some pages and maps of these spectral sequences.
      The zeroth pages takes the form:
    Example
      E^0
      E^0 .dd
      F^0
      -- F^0 .dd
    Text
      The first pages take the form:
    Example
      E^1
      -- E^1 .dd
      F^1
      -- F^1 .dd
    Text
      The second pages take the form:
    Example
      E^2
      -- E^2 .dd
      F^2
     -- F^2 .dd
    Text
      Observe that $E^2$ and $F^2$ are equal as they should.
  SeeAlso
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
///

doc ///
  Key
    "Examples of change of rings Spectral Sequences"
  Description
    Text
      Here are some examples of change of rings spectral sequences.
    Text
      Given a ring map f: R -> S, an R-module M and an R-module S,
      there is a spectral sequence E with E^2_{p,q} = Tor^S_p(Tor^R_q(M,S),N)
      that abuts to Tor^R_{p+q}(M,N).
    Example
--      First example
      k=QQ;
      R=k[a,b,c];
      S=k[s,t];
      f = map(S,R,{s^2,s*t,t^2});
      N = coker vars S;
      M = coker vars R --;
      F := res N;
      pushFwdF := pushFwd(f,F);
      G := res M;
      E := spectralSequence(filteredComplex(G) ** pushFwdF);
      EE := spectralSequence(G ** (filteredComplex pushFwdF));
      e = prune E;
      ee = prune EE;
      e^0
      e^1
      e^2
      e^infinity
      ee^0
  SeeAlso
    "Filtrations and tensor product complexes"
///

--------------------------------------------
-- Documentation of methods and functions --
--------------------------------------------

--
-- Types
--

doc ///
  Key
    FilteredComplex
  Headline
    the type of all filtered complexes
  Description
    Text
       An ascending filtration of a bounded (homological, lower index, or degree $-1$) chain complex
       $C : \dots \rightarrow C_i \rightarrow C_{i - 1} \rightarrow \dots$
       is an ordered family of chain subcomplexes
       $FC : \dots \subseteq F_{n - 1} C \subseteq F_n C \subseteq \dots $.
       Such a filtration is said to be bounded if $F_s C = C$ for all sufficiently
       large $s$ and $F_t C = 0$ for all sufficiently small $t$.

       Alternatively, a descending filtration of a bounded (cohomological, or upper index, or degree $1$) chain complex
       $C : \dots  \rightarrow C^i \rightarrow C^{i + 1} \rightarrow \dots $
       is an ordered family of subchain complexes
       $FC : \dots \subseteq F^{n + 1} C \subseteq F^n C \subseteq \dots$.
       Such a filtration is said to be bounded if $F^s C = 0$ for all sufficiently
       large $s$ and $F^t C = C$ for all sufficiently small $t$.

       The type {\tt FilteredComplex} is a data type for working with bounded filtrations of bounded chain complexes.
  Caveat
    By assumption all filtered complexes arise from bounded filtrations of bounded chain complexes.  Filtrations on degree $-1$
    chain complexes are ascending.  Filtrations on degree $1$ chain complexes are
    descending.
  SeeAlso
    "How to make filtered complexes from chain complex maps"
    "Filtered complexes and simplicial complexes"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
///

doc ///
  Key
    SpectralSequence
  Headline
    the type of all spectral sequences
  Description
    Text
      A (homological, or lower index) spectral sequence consists of:

      1. A sequence of modules $\{E^r_{p,q}\}$ for $p,q \in \mathbb{Z}$ and $r \geq 0$;

      2. A collection of homomorphisms $\{d^r_{p,q}: E^r_{p,q} \rightarrow E^r_{p-r,q+r-1} \}$, for $p,q \in \mathbb{Z}$ and $ r \geq 0$, such that
      $d^r_{p,q} d^r_{p+r,q-r+1} = 0$ ;

      3. A collection of isomorphisms $E^{r+1}_{p,q}  \rightarrow  ker d^r_{p,q} / image d^r_{p+r,q-r+1}$.

      Alternatively a (cohomological, or upper index) spectral sequence consists of:

      1'. A sequence of modules $\{E_r^{p,q}\}$ for $p,q \in \mathbb{Z}$, and $r \geq 0$;

      2'. A collection of homomorphisms $\{d_r^{p,q}: E_r^{p,q} \rightarrow E_{r}^{p+r,q-r+1}\}$ for $p,q \in \mathbb{Z}, r \geq 0$ such that
      $d_r^{p,q} d_r^{p-r,q+r-1} = 0$ ;

      3'. A collection of isomorphisms $E_{r+1}^{p,q}  $\rightarrow$ ker d_r^{p,q} / image d_r^{p-r,q+r-1}$.

      The type {\tt SpectralSequence} is a data type for working with spectral sequences.
      In this package, a spectral sequence is represented by a sequence of spectral sequence pages.
  Caveat
    All spectral sequences arise from bounded filtrations of bounded chain complexes.  Ascending filtrations of degree $-1$ chain complexes
    determine spectral sequences of the first type.  Descending filtrations of degree $1$ chain complex determine spectral sequences of the second type.
  SeeAlso
    SpectralSequencePage
    SpectralSequencePageMap
    "Filtered complexes and simplicial complexes"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
///

doc ///
  Key
    SpectralSequencePage
  Headline
    the type of all spectral sequence pages
  Description
    Text
      A (homological, or lower index) spectral sequence page consists of:

      1. A fixed integer $r \geq 0$, the page number;

      2. A sequence of modules $\{E^r_{p,q}\}$ for $p,q \in \mathbb{Z}$;

      3. A collection of homomorphisms $\{d^r_{p,q}: E^r_{p,q} \rightarrow E^r_{p-r,q+r-1}\}$ for
      $p,q \in \mathbb{Z}, r \geq 0$ such that
      $d^r_{p,q} d^r_{p+r,q-r+1} = 0$ ;

      4. A collection of isomorphisms $E^{r+1}_{p,q}  \rightarrow ker d^r_{p,q} / image d^r_{p+r,q-r+1}$.

      Alternatively a (cohomological, or upper index) spectral sequence page consists of:

      1'.  A fixed integer $r \geq 0$, the page number;

      2'. A sequence of modules $\{E_r^{p,q}\}$ for $p,q \in \mathbb{Z}$;

      3'. A collection of homomorphisms $\{d_r^{p,q}: E_r^{p,q} \rightarrow E_r^{p+r,q-r+1}\}$ for
      $ p,q \in \mathbb{Z}, r \geq 0$ such that
      $d_r^{p,q} d_r^{p-r,q+r-1} = 0$ ;

      4'. A collection of isomorphisms $E_{r+1}^{p,q}  \rightarrow ker d_r^{p,q} / image d_r^{p-r,q+r-1}$.

      The type {\tt SpectralSequencePage} is a data type for working with spectral sequences
      and spectral sequence pages.
  Caveat
      The isomorphisms $4$ and $4$' are not explicitly
      part of the data type, although they can be obtained by using the command @TO"homologyIsomorphism"@.
  SeeAlso
    SpectralSequence
    SpectralSequencePageMap
    Page
///

doc ///
  Key
    spectralSequencePageMap
   (spectralSequencePageMap, FilteredComplex, ZZ)
   [spectralSequencePageMap, Prune]
  Headline
    compute the maps on a spectral sequence page
  Usage
      d = spectralSequencePageMap(FilteredComplex, ZZ)
  Inputs
      K:FilteredComplex
      n:ZZ
  Outputs
      D:SpectralSequencePageMap
  Description
    Text
      Returns the differentials of a spectral sequence page.
///

doc ///
  Key
    (spectralSequencePage, FilteredComplex, ZZ)
  Headline
    construct a spectral sequence page from a filtered complex
  Usage
    E = spectralSequencePage(K,r)
  Inputs
    K:FilteredComplex
    r:ZZ
  Outputs
    E:SpectralSequencePage
  Description
    Text
      Returns the rth page of the spectral sequence determined by K.

      Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);

    Text
      Let $E$ be the spectral sequence determined by $K$.
    Example
      E = spectralSequence K;
    Text
      We now compute some pages.
    Example
      E^0
      E^1
      E^infinity
  SeeAlso
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    page
   (page, List, List, Page)
   [page, Prune]
  Description
    Text
      Adds keys to a page.
  SeeAlso
    Page
///

doc ///
  Key
    SpectralSequencePageMap
  Headline
    the type of all spectral sequence page maps
  Description
    Text
      A (homological, or lower index) spectral sequence page map consists of:

      1.  A fixed integer $r \geq 0 $, the page number;

      2. A collection of homomorphisms $\{d^r_{p,q}: E^r_{p,q} \rightarrow E^r_{p-r,q+r-1}\}$ for $p,q \in \mathbb{Z}, r \geq 0$ such that
      $d^r_{p,q} d^r_{p+r,q-r+1} = 0$.

      Alternatively a (cohomological, or upper index) spectral sequence page consists of:

      1'.  A fixed integer $r \geq 0$, the page number;

      2'.   A collection of homomorphisms $\{d_r^{p,q}: E_r^{p,q} \rightarrow E_r^{p+r,q-r+1}\}$ for $p,q \in \mathbb{Z}, r \geq 0$ such that
      $d_r^{p,q} d_r^{p-r,q+r-1} = 0$.

      The type {\tt SpectralSequencePageMap} is a data type for working with spectral sequences and the differentials
      on the pages of a spectral sequence.
  SeeAlso
    SpectralSequence
    SpectralSequencePage
    PageMap
///

doc ///
  Key
    Page
  Headline
    the type of all pages
  Description
    Text
      A page is a collection of modules which are indexed by lists of integers.  This is a parent class for the type @TO"SpectralSequencePage"@.  The infinity page of a spectral sequence
      is an example of a page which is not a spectral sequence page.

        As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
        the rational quartic space curve by successive powers of the irrelevant ideal.
      Example
        B = QQ[a..d];
        J = ideal vars B;
        C = res monomialCurveIdeal(B,{1,3,4});
        K = filteredComplex(J,C,4);
      Text
        The infinity page of the resulting spectral sequence is computed below.
      Example
      E = prune spectralSequence K;
      E^infinity
  SeeAlso
    SpectralSequencePage
    (symbol ^, SpectralSequence, InfiniteNumber)
    (symbol _, SpectralSequence, InfiniteNumber)
///

doc ///
  Key
    spots
   (spots, Page)
  Headline
    which spots does the given page has a module.
  Usage
    s = spots P
  Inputs
    P:Page
  Outputs
    s:List
  Description
    Text
      Returns a list of all the spots where the given page has a module.
///

doc ///
  Key
   (support, Page)
  Headline
    which non-zero modules appear in the given page.
  Usage
    l = support P
  Inputs
    P:Page
  Outputs
    l:List
  Description
    Text
      Returns a list of all the non-zero modules appearing in the given page has a module.
///

doc ///
  Key
    PageMap
  Headline
    the type of all page maps
  Description
    Text
      A page map is a collection of homomorphisms which are indexed by lists of integers.  This is a parent class for the type @TO"SpectralSequencePageMap"@.  The output of the
      method {\tt pruningMaps(SpectralSequencePage)} is an example of a {\tt Page} which is not a {\tt SpectralSequencePage}.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute an example of a pruning map below.
    Example
      E = prune spectralSequence K;
      pruningMaps E^2
  SeeAlso
    (pruningMaps,SpectralSequencePage)
    prune
///

--- functions and methods ---

-- TODO: merge
doc ///
  Key
    filteredComplex
  Headline
    make a filtered complex
  Usage
    K = filteredComplex L
  Inputs
    L:{List,Complex,SpectralSequence}
    ReducedHomology => Boolean
    Shift => ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
      This is the primitive filtered complex constructor.
  SeeAlso
    FilteredComplex
    "How to make filtered complexes from chain complex maps"
    "Filtrations and tensor product complexes"
    "Filtrations and homomorphism complexes"
    "Filtered complexes and simplicial complexes"
///

doc ///
  Key
    (filteredComplex, List)
    [filteredComplex, ReducedHomology]
    [filteredComplex, Shift]
    Shift
    ReducedHomology
  Headline
    obtain a filtered complex from a list of chain complex maps or a nested list of simplicial complexes
  Usage
    K = filteredComplex L
  Inputs
    L: List
    ReducedHomology => Boolean
    Shift => ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
      We can make a filtered complex from a list of chain complex maps as follows.
      We first need to load the relevant packages.
    Example
      needsPackage "SpectralSequences"
    Text
      We then make a chain complex.
    Example
      R = QQ[x,y,z,w]
      d2 = matrix(R,{{1},{0}})
      d1 = matrix(R,{{0,1}})
      C = complex({d1,d2})
    Text
        We now make the modules of the another chain complex which we will label D.
    Example
      D_2 = image matrix(R,{{1}})
      D_1 = image matrix(R,{{1,0},{0,0}})
      D_0 = image matrix(R,{{1}})
      D = complex({inducedMap(D_0,D_1,C.dd_1),inducedMap(D_1,D_2,C.dd_2)})
    Text
      Now make a chain complex map.
    Example
      d = map(C,D,{inducedMap(C_0,D_0,id_(C_0)),inducedMap(C_1,D_1,id_(C_1)),inducedMap(C_2,D_2,id_(C_2))})
      isWellDefined d
    Text
      We now make the modules of another chain complex which we will label E.
    Example
      E_2 = image matrix(R,{{0}})
      E_1 = image matrix(R,{{1,0},{0,0}})
      E_0 = image matrix(R,{{1}})
      E = complex({inducedMap(E_0,E_1,C.dd_1),inducedMap(E_1,E_2,C.dd_2)})
    Text
      Now make a chain complex map.
    Example
      e = map(C,E,{inducedMap(C_0,E_0,id_(C_0)),inducedMap(C_1,E_1,id_(C_1)),inducedMap(C_2,E_2,id_(C_2))})
      isWellDefined e
    Text
      Now make a filtered complex from a list of chain complex maps.
    Example
      K = filteredComplex({d,e})
    Text
       We can make a filtered complex, with a specified minimum filtration degree
       from a list of ComplexMaps by using the Shift option.
    Example
      L = filteredComplex({d,e},Shift => 1)
      M = filteredComplex({d,e},Shift => -1)
    Text
      We can make a filtered complex from a nested list of simplicial
      complexes as follows
    Example
        D = simplicialComplex {x*y*z, x*y, y*z, w*z}
        E = simplicialComplex {x*y, w}
        F = simplicialComplex {x,w}
        K = filteredComplex{D,E,F}
    Text
       If we want the resulting complexes to correspond to the non-reduced homology
       of the simplicial complexes we can do the following.
    Example
       filteredComplex({D,E,F}, ReducedHomology => false)
  SeeAlso
    "Complexes :: Making maps between chain complexes"
///

doc ///
  Key
    (filteredComplex, Complex)
  Headline
      obtain a filtered complex from a chain complex
  Usage
      K = filteredComplex C
  Inputs
    C: Complex
-- these options don't do anything for this constructor.
    ReducedHomology => Boolean
    Shift => ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
       Produces the filtered complex obtained by successively truncating the complex.
    Example
      A = QQ[x,y]
      C = koszulComplex vars A
      K = filteredComplex C
  SeeAlso
     (naiveTruncation, Complex, ZZ, ZZ)
///

doc ///
  Key
    (filteredComplex, SpectralSequence)
  Headline
      obtain the filtered complex associated to the spectral sequence
  Usage
      K = filteredComplex E
  Inputs
    E: SpectralSequence
-- these options don't do anything for this constructor.
    ReducedHomology => Boolean
    Shift => ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
       Produces the filtered complex which determined the spectral sequence.
       Consider the spectral sequence $E$ which arises from a nested list of simplicial
       complexes.
    Example
      A = QQ[a,b,c,d];
      D = simplicialComplex {a*d*c, a*b, a*c, b*c};
      F2D = D;
      F1D = simplicialComplex {a*c, d};
      F0D = simplicialComplex {a,d};
      K = filteredComplex {F2D, F1D, F0D};
      E = spectralSequence(K) ;
    Text
      The underlying filtered chain complex
      can be recovered from the
      spectral sequence by:
    Example
      C = filteredComplex E
  SeeAlso
     (symbol _, FilteredComplex, InfiniteNumber)
     (symbol ^, FilteredComplex, InfiniteNumber)
///

doc ///
  Key
    (filteredComplex, Ideal, Complex, ZZ)
  Headline
    I-adic filtrations of chain complexes
  Usage
      K = filteredComplex(I,C,n)
  Inputs
    I: Ideal
    C: Complex
    n: ZZ
  Outputs
    K: FilteredComplex
  Description
    Text
      By multiplying a chain complex by successive powers of an ideal we obtain a filtered complex.
    Example
      B = QQ[a..d]
      J = ideal vars B
      C = res monomialCurveIdeal(B,{1,3,4})
      K = filteredComplex(J,C,4)
    Text
      Here are higher some pages of the associated spectral sequence:
    Example
      e = prune spectralSequence K
      e^2
      -- e^3
      -- e^3 .dd
      -- e^4
      -- e^4 .dd
      assert(all(keys support e^0, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,0)))
      assert(all(keys support e^1, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,1)))
      assert(all(keys support e^2, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,2)))
      assert(all(keys support e^3, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,3)))
      assert(all(keys support e^4, j -> isIsomorphism homologyIsomorphism(e,j#0,j#1,4)))
///

doc ///
  Key
   (spots, FilteredComplex)
  Headline
    which spots does the given filtered complex has a module.
  Usage
    s = spots L
  Inputs
    L:FilteredComplex
  Outputs
    s:List
  Description
    Text
      Returns a list of all the spots where the given filtered complex has a module.
///

doc ///
  Key
   (max, FilteredComplex)
  Headline
    maximum spot where the given filtered complex has a module.
  Usage
    m = max L
  Inputs
    L:FilteredComplex
  Outputs
    m:ZZ
  Description
    Text
      Returns the maximum spot where the given filtered complex has a module.
///

doc ///
  Key
   (min, FilteredComplex)
  Headline
    minimum spot where the given filtered complex has a module.
  Usage
    m = min L
  Inputs
    L:FilteredComplex
  Outputs
    m:ZZ
  Description
    Text
      Returns the minimum spot where the given filtered complex has a module.
///

doc ///
  Key
    spectralSequence
  Headline
    construct a spectral sequence
  Usage
    E = spectralSequence K
  Inputs
    K:FilteredComplex
  Outputs
    E:SpectralSequence
  Description
    Text
      This is the primitive spectral sequence constructor.

      In the example below we construct a spectral sequence
      $E$ from the filtered complex $K$.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
      E = spectralSequence K
    Text
      To view pages and or maps we proceed, for example, as follows (note we suppress the output of the E^0.dd command to prevent excessive output)
    Example
      E^0
      E^0 .dd;
      E^infinity
  SeeAlso
    SpectralSequence
    SpectralSequencePage
    (symbol ^,SpectralSequence,ZZ)
    (spectralSequence, FilteredComplex)
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
   (spectralSequence, FilteredComplex)
   [spectralSequence, Prune]
  Headline
    construct a spectral sequence from a filtered complex
  Usage
    E = spectralSequence K
  Inputs
    K:FilteredComplex
  Outputs
    E:SpectralSequence
  Description
    Text
      Returns the spectral sequence associated to the filtered complex.
    Example
      A = QQ[x,y];
      C = koszulComplex vars A
      K = filteredComplex C;
      E = spectralSequence K
///

doc ///
  Key
    spectralSequencePage
   [spectralSequencePage, Prune]
  Headline
    construct a spectral sequence page from a filtered complex
  Usage
    E = spectralSequencePage(K,r)
  Inputs
    K:FilteredComplex
    r:ZZ
  Outputs
    E:SpectralSequencePage
  Description
    Text
      This is the primitive spectral sequence page constructor.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
      E = spectralSequence K
    Text
      To view pages and or maps we proceed, for example, as follows
    Example
      E^0
  SeeAlso
    spectralSequence
    (spectralSequence, FilteredComplex)
    SpectralSequencePageMap
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    pruningMaps
  Headline
    compute the pruning maps on a spectral sequence page
  Usage
    d = pruningMaps E
  Inputs
    E:SpectralSequencePage
  Outputs
    d:PageMap
  Description
    Text
      Returns the pruning maps which are cached in the process of pruning the spectral sequence page.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute an example of a pruning map below.
    Example
      E = prune spectralSequence K;
      pruningMaps E^2
  SeeAlso
    (prune, SpectralSequence)
    SpectralSequencePage
    PageMap
///

doc ///
  Key
    (pruningMaps, SpectralSequencePage)
  Headline
    compute the pruning maps on a spectral sequence page
  Description
    Text
      Returns the pruning maps which are cached in the process of pruning the spectral sequence page.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute an example of a pruning map below.
    Example
      E = prune spectralSequence K;
      pruningMaps E^2
  SeeAlso
    (prune, SpectralSequence)
    SpectralSequencePage
    PageMap
///

doc ///
  Key
    (basis, List, SpectralSequencePage)
    (basis, ZZ, SpectralSequencePage)
  Headline
    generators of a particular degree
  Usage
    B = basis(L, E)
  Inputs
    L:List
    E:SpectralSequencePage
  Outputs
      B:Matrix --Note!!  The output should actually be a page!!
  Description
    Text
      Returns generators for the requested (multi)degree of the spectral sequence page.  It is designed to extend
      the function @TO"basis"@ which can be applied to modules, for instance.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute the degree $0$ piece of the $E^3$ page below.
    Example
      E = prune spectralSequence K;
      E^3
      basis(0,E^3)
  SeeAlso
      basis
///

doc ///
  Key
    (hilbertPolynomial, SpectralSequencePage)
  Headline
    the Hilbert polynomial of a spectral sequence page
  Usage
    H = hilbertPolynomial(E)
  Inputs
    E:SpectralSequencePage
  Outputs
      H:Page
  Description
    Text
      Returns the Hilbert polynomials of all modules of the spectral sequence page

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute the degree $0$ piece of the $E^3$ page below.
    Example
      E = prune spectralSequence K;
      hilbertPolynomial(E^3)
///

doc ///
  Key
    (complex, FilteredComplex)
  Headline
    the ambient chain complex of a filtered complex
  Usage
    C = complex K
  Inputs
    K:FilteredComplex
  Outputs
    C:Complex
  Description
    Text
      Returns the ambient chain complex of the filtered complex.
    Example
        A = QQ[x,y];
        C = koszulComplex vars A
        K = filteredComplex C;
        complex K
        K_infinity
  SeeAlso
    (symbol _, FilteredComplex, ZZ)
    (symbol _, FilteredComplex, InfiniteNumber)
    (symbol ^, FilteredComplex, ZZ)
    (symbol ^, FilteredComplex, InfiniteNumber)
///

doc ///
  Key
    (minimalPresentation, SpectralSequence)
    (prune, SpectralSequence)
  Headline
    a minimal presentation of a spectral sequence
  Usage
    E = minimalPresentation e
  Inputs
    e:SpectralSequence
  Outputs
    E:SpectralSequence
  Description
    Text
      Returns the minimal presentation of a spectral sequence.

      If we fail to prune a spectral sequence then the out-put can be highly
      unintelligible.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      Compare some pages of the non-pruned version of the spectral sequence
      with that of the pruned version.
    Example
      E = prune spectralSequence K;
      e = spectralSequence K;
      e^3
      E^3
  SeeAlso
    (minimalPresentation, SpectralSequencePage)
    (prune, SpectralSequencePage)
    minimalPresentation
    prune
    pruningMaps
///

doc ///
  Key
    (minimalPresentation, SpectralSequencePage)
    (prune, SpectralSequencePage)
  Headline
    a minimal presentation of a spectral sequence page
  Usage
    E = minimalPresentation e
  Inputs
    e:SpectralSequencePage
  Outputs
    E:SpectralSequencePage
  Description
    Text
      Returns a minimal presentation of the spectral sequence page.

      If we fail to prune a spectral sequence then the out-put can be highly
      unintelligible.

      As a specific example consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      Compare some pruned and non-prunded pages the spectral sequence $E$ below.
    Example
      E = spectralSequence K;
      E^3
      prune E^3
  SeeAlso
    (minimalPresentation, SpectralSequence)
    (prune, SpectralSequence)
    minimalPresentation
    prune
///

doc ///
  Key
     (Hom, FilteredComplex, Complex)
     (Hom, Complex, FilteredComplex)
  Headline
    the filtered Hom complex
  Usage
    f = Hom(K,C)
  Inputs
    K:FilteredComplex
    C:Complex
  Outputs
    f:FilteredComplex
  Description
    Text
        Returns the filtrations of the Hom complex determined by the double complex.  Here is
        an example which illustrates the syntax.
    Example
       A = QQ[x,y,z,w];
       B = res monomialCurveIdeal(A, {1,2,3});
       C = res monomialCurveIdeal(A, {1,3,4});
       F' = Hom(filteredComplex B, C)
       F'' = Hom(B,filteredComplex C)
  SeeAlso
      "Filtrations and tensor product complexes"
///

doc ///
  Key
     (complex, SpectralSequence)
  Headline
    the underlying chain complex of a Spectral Sequence
  Usage
    K = complex E
  Inputs
    E:SpectralSequence
  Outputs
    K:Complex
  Description
    Text
      Returns the underlying chain complex of a spectral sequence.
    Example
      A = QQ[x,y];
      C = koszulComplex vars A
      K = filteredComplex C;
      E = spectralSequence K
      complex E
///

doc ///
  Key
    netPage
   (netPage,Page, List, List)
  Headline
    display a small portion of a given Spectral Sequence page
  Usage
    E' = netPage(E,L1,L2)
  Inputs
    E: Page
    L1: List
    -- A list {minP,minQ}, the bottom left corner coordinates to display
    L2: List
     -- A list {maxP,maxQ}, the top right corner coordinates to display
  Outputs
    E': Net
  Description
    Text
      Produces the portion of a given spectral sequence page that lies in the square
      with given bottom right and top left coordinates.
    Example
      R = QQ[x];
      S = R/ideal"x2";
      N = S^1/ideal"x";
      M = R^1/R_0;
      C = res M;
      C' = C ** S;
      D = res(N,LengthLimit => 10);
      E0 = C' ** (filteredComplex D);
      E = prune spectralSequence E0;
    Text
      The E_2 page has nonzero E_2^{p,q} when 0 <= p <= 10 and 0 <= q <= 1,
      so we may ask to restrict the display to 2 <= p <= 6 and 0 <= q <= 1.
    Example
      netPage(E_2,{2,0},{6,1})
    Text
      If we ask for a square that is too large, only the relevant portion of the page will be displayed.
    Example
      R = QQ[x];
      S = R/ideal"x2";
      N = S^1/ideal"x";
      M = R^1/R_0;
      C = res M;
      C' = C ** S;
      D = res(N,LengthLimit => 10);
      E0 = C' ** (filteredComplex D);
      E = prune spectralSequence E0;
      netPage(E_2,{-5,0},{7,1})
///

doc ///
  Key
    (symbol _, SpectralSequence, ZZ)
  Headline
    retrieve the k-th page of a spectral sequence
  Usage
    P = E_k
  Inputs
    E:SpectralSequence
    k:ZZ
  Outputs
    P: SpectralSequencePage
  Description
    Text
      Returns the kth page of the spectral sequence determined by K.

      Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      Let $E$ be the spectral sequence determined by $K$.
    Example
      E = spectralSequence K;
    Text
      We now compute some pages.
    Example
      E_0
      E_1
      E_infinity
  SeeAlso
    (symbol ^,SpectralSequence,ZZ)
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    (symbol _, SpectralSequencePageMap, List)
  Headline
    retrieve the map from the {p,q} term of a spectral sequence page
  Usage
    d = D _L
  Inputs
    D:SpectralSequencePageMap
    L:List
        A list L = \{p,q\} \ of integers.
  Outputs
    d: Matrix
  Description
    Text
        Returns the p,q th map on a (lower index) spectral sequence page.
	The relationship $D_{p,q} = D^{-p,-q}$ holds.

        Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
        the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute a map on the third page of the spectral sequence associated to $K$.
    Example
      E = spectralSequence K
      E^3 .dd_{-1,2}
  SeeAlso
    (symbol ^, SpectralSequencePageMap, List)
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    (symbol ^, SpectralSequencePageMap, List)
  Headline
    retrieve the map from the {p,q} term of a spectral sequence page
  Usage
    d = D ^L
  Inputs
    D:SpectralSequencePageMap
    L:List
        A list L = \{p,q\} \ of integers.
  Outputs
    d: Matrix
  Description
    Text
        Returns the p,q th map on an (upper index) spectral sequence page.  The relationship $D^{p,q} = D_{-p,-q}$ holds.

      Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
      the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);
    Text
      We compute a map on the third page of the spectral sequence associated to $K$.
    Example
      E = spectralSequence K
      E_3 .dd^{1,-2}
  SeeAlso
    (symbol _, SpectralSequencePageMap, List)
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    (symbol ^, SpectralSequence, ZZ)
  Headline
    retrieve the k-th page of a spectral sequence
  Usage
    P = E^k
  Inputs
    E:SpectralSequence
    k:ZZ
  Outputs
    P:SpectralSequencePage
  Description
    Text
        Returns the kth page of the spectral sequence.

        Consider the filtered complex $K$ below, obtained by multiplying the minimal free resolution of
        the rational quartic space curve by successive powers of the irrelevant ideal.
    Example
      B = QQ[a..d];
      J = ideal vars B;
      C = res monomialCurveIdeal(B,{1,3,4});
      K = filteredComplex(J,C,4);

    Text
      Let $E$ be the spectral sequence determined by $K$.
    Example
      E = spectralSequence K;
    Text
      We now compute some pages.
    Example
      E^0
      E^1
      E^infinity
  SeeAlso
    (symbol _, SpectralSequence, ZZ)
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    (symbol ^, SpectralSequence, InfiniteNumber)
    (symbol _, SpectralSequence, InfiniteNumber)
  Headline
    retrieve the infinity page of a spectral sequence
  Usage
    P = E^k
  Inputs
    E:SpectralSequence
    k:InfiniteNumber
  Outputs
    P:SpectralSequencePage
  Description
    Text
        Returns the infinity page a spectral sequence.
///

doc ///
  Key
    (symbol ^, SpectralSequencePage, List)
  Headline
    retrieve the module in the {i,j} position on the page
  Usage
    M = P^L
  Inputs
    P:SpectralSequencePage
    L:List
      A list L = \{i,j\} of integers
  Outputs
    M:Module
  Description
    Text
      Returns the module in the \{i,j\}  \ position in the spectral sequence page.
      (Using cohomological or upper indexing conventions.)  The relationship $E^{-i,-j} = E_{i,j}$ holds.
    Example
        A = QQ[x,y]
        C = koszulComplex vars A;
        K = filteredComplex C;
        E = spectralSequence K
        E_0
        E_0 ^{-1,0}
        E^0 _{1,0}
  SeeAlso
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    (symbol _, SpectralSequencePage, List)
  Headline
    retrieve the module in the {i,j} position on the page
  Usage
    M = P_L
  Inputs
    P:SpectralSequencePage
    L:List
      A list L = \{i,j\} \ of integers
  Outputs
    M:Module
  Description
    Text
      Returns the module in the \{i,j\} \ position in the spectral sequence page.
      (Using homological or lower indexing conventions.)  The relationship $E_{i,j} = E^{-i,-j}$ holds.
    Example
        A = QQ[x,y]
        C = koszulComplex vars A;
        K = filteredComplex C;
        E = spectralSequence K
        E^0
        E^0 _{1,0}
        E_0 ^{-1,0}
  SeeAlso
    "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
     (symbol **, Complex, FilteredComplex)
     (symbol **, FilteredComplex, Complex)
  Headline
    filtered tensor product of complexes
  Usage
    KK = C ** K
    KK = K ** C
  Inputs
    C:Complex
    K:FilteredComplex
  Outputs
    KK:FilteredComplex
  Description
    Text
      Returns the two filtrations of the tensor product complex determined by
      the double complex.
      The following example illustrates the syntax.
    Example
        A = QQ[x,y];
        B = koszulComplex vars A;
        C = koszulComplex vars A;
        F' = (filteredComplex B) ** C
        F'' = B ** (filteredComplex C)
  SeeAlso
    "Filtrations and tensor product complexes"
///

doc ///
  Key
    (tensor, RingMap, Complex)
  Headline
    tensor product of a chain complex by a ring map
  Usage
    D = tensor(f,C)
  Inputs
    f:RingMap
    C:Complex
  Outputs
    D:Complex
  Description
    Text
      Given a ring map R -> S and a chain complex over R,
      returns the tensor product of the given chain complex.
    Example
        R = QQ[x];
        M = R^1/(x^2);
        S = R/(x^4);
        C = res M
        f = map(S,R,{1});
        tensor(f,C)
  SeeAlso
    "Filtrations and tensor product complexes"
///

doc ///
  Key
    (inducedMap, FilteredComplex, ZZ)
  Headline
    the i th inclusion map in a filtered complex
  Usage
    f = inducedMap(K,i)
  Inputs
    K:FilteredComplex
    i:ZZ
  Outputs
    f:ComplexMap
  Description
    Text
      Returns the chain complex map specifying the inclusion of the i piece
      of the filtered
      complex to the ambient chain complex.
    Example
        A = QQ[x,y];
        C = koszulComplex vars A;
        K = filteredComplex C
        inducedMap(K,1)
///

doc ///
  Key
    (symbol _, FilteredComplex, ZZ)
    (symbol _, FilteredComplex, InfiniteNumber)
  Headline
    retrieve the filtered pieces of a filtered complex
  Usage
    C = K _ j
  Inputs
    K:FilteredComplex
    j:ZZ
      an integer, infinity, or -infinity
  Outputs
    C:Complex
  Description
    Text
      Returns the chain complex in (homological) filtration degree j.
      The relationship     $K _ j = K ^{(-j)}$ holds.
     Example
      A = QQ[x,y];
      C = koszulComplex vars A;
      K = filteredComplex C
      K_0
      K_1
      K_2
      K^(-1)
      K^(-2)
      K_infinity
      K_(-infinity)
      K^(- infinity)
      K^infinity
  SeeAlso
    (symbol ^, FilteredComplex, ZZ)
    (symbol ^, FilteredComplex, InfiniteNumber)
///

doc ///
  Key
    (symbol ^, FilteredComplex, ZZ)
    (symbol ^, FilteredComplex, InfiniteNumber)
  Headline
    retrieve the filtered pieces of a filtered complex
  Usage
    C = K ^  j
  Inputs
    K:FilteredComplex
    j:ZZ
      an integer, infinity, or -infinity
  Outputs
    C:Complex
  Description
    Text
      Returns the chain complex in (cohomological) filtration degree j.
      The relationship $K ^ j = K _{(-j)}$ holds.
    Example
      A = QQ[x,y];
      C = koszulComplex vars A;
      K = filteredComplex C
      K_0
      K_1
      K_2
      K^(-1)
      K^(-2)
      K_infinity
      K_(-infinity)
      K^(-infinity)
      K^infinity
  SeeAlso
    (symbol _, FilteredComplex, ZZ)
    (symbol _, FilteredComplex, InfiniteNumber)
///

doc ///
  Key
    connectingMorphism
  Headline
    use spectral sequences to compute connecting morphisms
  Usage
      g = connectingMorphism(f, n)
  Inputs
      f:ComplexMap
      n:ZZ
  Outputs
      g:Matrix
  Description
    Text
      Given a morphism $f: A \rightarrow B$ of chain complexes
      returns the connecting map $H_{n+1}(coker f) \rightarrow H_n (im f)$.
///

doc ///
  Key
    (connectingMorphism, ComplexMap,ZZ)
  Headline
    use spectral sequences to compute connecting morphisms
  Usage
      g = connectingMorphism(f, n)
  Inputs
      f:ComplexMap
      n:ZZ
  Outputs
      g:Matrix
  Description
    Text
      Given a morphism $f: A \rightarrow B$ of chain complexes
      returns the connecting map $H_{n+1}(coker f) \rightarrow H_n (im f)$.
///

doc ///
    Key
      homologyIsomorphism
    Headline
      compute the homology isomorphism
    Description
      Text
        Computes the isomorphism $ker d^r_{p,q} / image d^r_{p + r, q - r + 1} \rightarrow E^{r+1}_{p,q}$
    SeeAlso
     (homologyIsomorphism, SpectralSequence, ZZ, ZZ, ZZ)
///

doc ///
  Key
    (homologyIsomorphism, SpectralSequence, ZZ, ZZ, ZZ)
  Headline
    the homology isomorphism
  Usage
      g = homologyIsomorphism(SpectralSequence, ZZ, ZZ, ZZ)
  Inputs
      E:SpectralSequence
      p:ZZ
      q:ZZ
      r:ZZ
  Outputs
      g:Matrix
  Description
    Text
      Computes the isomorphism $ker d^r_{p,q} / image d^r_{p + r, q - r + 1} \rightarrow E^{r+1}_{p,q}$
    Example
      A = ZZ [s,t,u,v,w] ;
      K = filteredComplex(reverse {
	      simplicialComplex {s},
	      simplicialComplex {s,t},
	      simplicialComplex {s,t,u},
	      simplicialComplex {s*t, u},
	      simplicialComplex {s*t, u, v},
	      simplicialComplex {s*t, u, v, w},
	      simplicialComplex {s*t, s*w ,u, v},
	      simplicialComplex {s*t, s*w ,t * w, u, v},
	      simplicialComplex {s*t, s*w ,t * w, u * v},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v},
	      simplicialComplex {s*t, s*w ,t * w, u * v, s * v, s*u, u * w, t* u, t*u*w, s*u*w,s*t*u, s*u*v, s*t*w}}, ReducedHomology => false);
      E = prune spectralSequence K
      e = spectralSequence K
      apply(keys support E^11, i -> homologyIsomorphism(E, i#0, i#1, 11))
      apply(keys support e^11, i -> homologyIsomorphism(e, i#0, i#1, 11))
  SeeAlso
      homologyIsomorphism
///

doc ///
  Key
    "filtered complexes and spectral sequences from simplicial complexes"
  Description
    Text
      To make a filtered complex from a list of simplicial
      complexes we first need to make some simplicial complexes.
    Example
        R = QQ[x,y,z,w];
        a = simplicialComplex {x*y*z, x*y, y*z, w*z}
        b = simplicialComplex {x*y, w}
        c = simplicialComplex {x,w}
    Text
       Note that $b$ is a simplicial subcomplex of $a$ and that
       $c$ is a simplicial subcomplex of $b$.
       Let's now create a filtered complex.
    Example
        K = filteredComplex{a,b,c}
    Text
        The associated spectral sequence takes the form:
    Example
      E = spectralSequence K
    Text
      Let's view some pages and maps of these pages.
    Example
      E^0
      F0 = minimalPresentation(E^0)
      E^0 .dd
      F0.dd
      E^1
      F1 = minimalPresentation(E^1)
      E^1 .dd
      F1.dd
      E^2
      F2 = minimalPresentation(E^2)
      E^2 .dd
      F2.dd
      E^infinity
      (prune E) ^infinity
    Text
       If we want the resulting complexes to correspond to the non-reduced homology
       of the simplicial complexes we set the ReducedHomology option
       to false.
    Example
       J = filteredComplex({a,b,c}, ReducedHomology => false)
    Text
      The resulting spectral sequence looks like
    Example
      D = spectralSequence J
      D^0
      G0 = minimalPresentation(D^0)
      G0.dd
      D^1
      G1 = minimalPresentation(D^1)
      G1.dd
      D^2
      G2 = minimalPresentation(D^2)
      G2.dd
      D^infinity
///

doc ///
  Key
    filteredHomologyObject
  Headline
    compute the filtered homology object
  SeeAlso
     (filteredHomologyObject, ZZ, ZZ, FilteredComplex)
     (associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)
///

doc ///
  Key
    (filteredHomologyObject, ZZ, ZZ, FilteredComplex)
  Headline
    compute the filtered homology object
  Usage
      M = filteredHomologyObject(ZZ, ZZ, FilteredComplex)
  Inputs
      p:ZZ
      n:ZZ
      K:FilteredComplex
  Outputs
      M:Module
  Description
    Text
      Computes the filtered homology object determined by the filtered chain complex
  SeeAlso
     (associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)
///

doc ///
  Key
    associatedGradedHomologyObject
   (associatedGradedHomologyObject, ZZ, ZZ, FilteredComplex)
  Headline
    compute the associated graded homology object
  Usage
      M = associatedGradedHomologyObject(ZZ, ZZ, FilteredComplex)
  Inputs
      p:ZZ
      n:ZZ
      K:FilteredComplex
  Outputs
      M:Module
  Description
    Text
      Computes the associated graded homology object determined by the filtered chain complex
///

doc ///
  Key
    "Edge homomorphisms"
  Description
    Text
      Suppose that $E$ is a spectral sequence with the properties that:

      1. $E^2_{p,q} = 0$ for all $p < l$ and all $q \in \mathbb{Z}$;

      2. $E^2_{p,q} = 0 $ for all $q < m$ and all $p \in \mathbb{Z}$;

      3.  $E$ converges to the graded module $\{H_n\}$ for $n \in \mathbb{Z}$.

      Then $E$ determines a $5$-term exact sequence
      $H_{l+m+2} \rightarrow E^2_{l+2,m} \rightarrow E^2_{l,m+1} \rightarrow H_{l+m+1} \rightarrow E^2_{l+1,m} \rightarrow 0$ which we refer to as the
      {\it edge complex}.

      Note that the above properties are satisfied if $E$ is the spectral sequence determined by a bounded filtration of a bounded chain complex.

      The following is an easy example, of a spectral sequence which arises from a nested chain of simplicial complexes, which illustrates this concept.

    Example
      A = QQ[a,b,c,d];
      D = simplicialComplex {a*d*c, a*b, a*c, b*c};
      F2D = D;
      F1D = simplicialComplex {a*c, d};
      F0D = simplicialComplex {a,d};
      K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
      C = K_infinity;
      prune HH C
    Text
      The second page of the corresponding spectral sequences take the form:
    Example
      E = spectralSequence(K);
      e = prune E;
      E^2
      e^2
    Text
      The acyclic edge complex for this example has the form
      $H_1(C) \rightarrow E^2_{2,-1} \rightarrow E^2_{0,0} \rightarrow H_0(C)  \rightarrow E^2_{1, -1} \rightarrow 0$
      and is given by
    Example
      edgeComplex E
      prune edgeComplex E
    Text
      To see that it is acyclic we can compute
    Example
      prune HH edgeComplex E
  Caveat
    The method currently does not support pruned spectral sequences.
  SeeAlso
     "Examples of filtered complexes and spectral sequences"
///

doc ///
  Key
    edgeComplex
   (edgeComplex, SpectralSequence)
  Headline
    the edge homomorphisms
  Usage
      C = edgeComplex E
  Inputs
    E: SpectralSequence
  Outputs
    C: Complex
  Description
    Text
      Suppose that $E$ is a spectral sequence with the properties that:

      1. $E^2_{p,q} = 0$ for all $p < l$ and all $q \in \mathbb{Z}$;

      2. $E^2_{p,q} = 0 $ for all $q < m$ and all $p \in \mathbb{Z}$;

      3.  $E$ converges to the graded module $\{H_n\}$ for $n \in \mathbb{Z}$.

      Then $E$ determines a $5$-term exact sequence
      $H_{l+m+2} \rightarrow E^2_{l+2,m} \rightarrow E^2_{l,m+1} \rightarrow H_{l+m+1} \rightarrow E^2_{l+1,m} \rightarrow 0$ which we refer to as the
      {\it edge complex}.

      Note that the above properties are satisfied if $E$ is the spectral sequence determined by a bounded filtration of a bounded chain complex.

      The following is an easy example, of a spectral sequence which arises from a nested chain of simplicial complexes, which illustrates this concept.

    Example
      A = QQ[a,b,c,d];
      D = simplicialComplex {a*d*c, a*b, a*c, b*c};
      F2D = D;
      F1D = simplicialComplex {a*c, d};
      F0D = simplicialComplex {a,d};
      K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false);
      C = K_infinity;
      prune HH C
    Text
      The second page of the corresponding spectral sequences take the form:
    Example
      E = spectralSequence(K);
      e = prune E;
      E^2
      e^2
    Text
      The acyclic edge complex for this example has the form
      $H_1(C) \rightarrow E^2_{2,-1} \rightarrow E^2_{0,0} \rightarrow H_0(C)  \rightarrow E^2_{1, -1} \rightarrow 0$
      and is given by
    Example
      edgeComplex E
      prune edgeComplex E
    Text
      To see that it is acyclic we can compute
    Example
      prune HH edgeComplex E
  Caveat
    The method currently does not support pruned spectral sequences.
///

doc ///
  Key
    "Example 1"
  Headline
    Easy example of a filtered simplicial complex
  Description
    Text
      Here we provide an easy example of a filtered simplicial complex and
      the resulting spectral sequence.  This example is small enough
      that all aspects of it can be explicitly computed by hand.
    Example
      A = QQ[a,b,c,d];
      D = simplicialComplex {a*d*c, a*b, a*c, b*c};
      F2D = D
      F1D = simplicialComplex {a*c, d}
      F0D = simplicialComplex {a,d}
      K = filteredComplex({F2D, F1D, F0D},ReducedHomology => false)
      E = prune spectralSequence(K)
      E^0
      E^1
      E^2
      E^3
      E^infinity
      C = K_infinity
      prune HH C
      E^2 .dd
    Text
      Considering the $E^2$ and $E^3$ pages of the spectral sequence
      we conclude that the map $d^2_{2,-1}$ must have a $1$-dimensional
      image and a $1$-dimensional kernel.  This can be verified easily:
    Example
      rank ker E^2 .dd_{2,-1}
      rank image E^2 .dd_{2,-1}
///

-- We might want to not include this next example
doc ///
  Key
    "Example 2"
  Headline
    Easy example of a filtered simplicial complex
  Description
    Text
      We provide an easy example of a filtered simplicial complex and
      the resulting spectral sequence.  This example is small enough that
      all aspects of it can be explicitly computed by hand.
    Example
      A = QQ[a,b,c];
      D = simplicialComplex({a*b*c})
      F3D = D;
      F2D = simplicialComplex({a*b,a*c,b*c})
      F1D = simplicialComplex({a*b,c})
      F0D = simplicialComplex({a,b})
      K = filteredComplex({F3D,F2D,F1D,F0D}, ReducedHomology => false)
      E = prune spectralSequence K
      E^0
      E^0 .dd
      E^0
      E^1
      E^0 .dd_{1,0}
      E^1 .dd
      E^1
      E^0
      E^2
      prune HH K_infinity
      E^infinity
///

-- We might want to not include this next example
doc ///
  Key
    "Example 3"
  Headline
    Easy example of a filtered simplicial complex
  Description
    Text
      We provide an easy example of a filtered simplicial complex
      and the resulting spectral sequence.  This example is small enough that
      all aspects of it can be explicitly computed by hand.
    Example
      A = QQ[a,b,c]
      D = simplicialComplex {a*b*c}
      F2D = D
      F1D = simplicialComplex {a*b,a*c,b*c}
      F0D = simplicialComplex {a,b,c}
      K = filteredComplex({F2D,F1D,F0D}, ReducedHomology => false)
      C = K_infinity
      E = prune spectralSequence K
      E^0
      E^0 .dd
      E^1
      E^1 .dd
      E^2
      E^2 .dd
      E^infinity
      prune HH K_infinity
///

doc ///
  Key
   targetPruningMap
///

doc ///
  Key
   sourcePruningMap
///

--- TODO: Move to Complexes

doc ///
  Key
    (naiveTruncation, Complex, ZZ)
  Headline
    compute the hard truncation of a chain complex
  Usage
    naiveTruncation(C, n)
  Inputs
    C:Complex
    n:ZZ
  Outputs
    :Complex
  Description
    Text
      This method returns the naive truncation of $C$ by truncating
      the low homological degrees if $n$ is positive (i.e. from left)
      and high homological degrees if $n$ is negative (i.e. from right).
    Example
      B = QQ[a..d];
      C = (koszulComplex vars B)[2]
      naiveTruncation(C, 10)
      naiveTruncation(C, 2)
      naiveTruncation(C, 1)
      naiveTruncation(C, 0)
      naiveTruncation(C,-1)
      naiveTruncation(C,-2)
      naiveTruncation(C,-10)
  SeeAlso
    (naiveTruncation, Complex, ZZ, ZZ)
///

doc ///
  Key
    (spots, Complex)
  Headline
    which spots does the given chain complex has a module.
  Usage
    s = spots L
  Inputs
    L:Complex
  Outputs
    s:List
  Description
    Text
      Returns a list of all the spots where the given chain complex has a module.
  SeeAlso
    (concentration, Complex)
    (support, Complex)
///

doc ///
  Key
    (support, Complex)
  Headline
    nonzero parts of a chain complex
  Description
    Text
      Computes the homological degrees in which the chain complex admits a nonzero module
    Example
      A = QQ[x,y];
      C = koszulComplex vars A
      support C
      D = naiveTruncation(C, 1)
      spots D
      support D
  SeeAlso
    (concentration, Complex)
    (spots, Complex)
///
