doc ///
   Key 
      SchurVeronese
   Headline 
      syzygy data for Veronese embeddings of P^1 and P^2
   Description
    Text
      The authors of this package used a combination of high-throughput and
      high-performance computing and sparse numerical linear algebra to compute 
      the syzygies of $\mathbb{P}^{2}$ under the $d$-fold Veronese embedding
      for various values of $d$. See the paper ``Conjectures and 
      Computations about Veronese Syzygies'' by Bruce, Erman, Goldstein and Yang, which
      we refer to as [BEGY] (see @{HREF("http://arxiv.org/abs/1711.03513","arXiv:1711.03513")}@) throughout the documentation for this package.
      In addition, much of the data generated from
      these computations (graded Betti numbers, multigraded Betti numbers, 
      Schur functor decompositions, etc.) is currently available online via syzygydata.com.
      The goal of this package is to make this data more accessible and easy to 
      use by providing a way to access it via Macaulay2.

      Most functions have been implemented with three parameters $(d,n,b)$, where
      the goal is to compute the syzygies of the pushforward of
      the line bundle $\mathcal{O}(b)$ under the $d$-fold
      embedding.  However, we have produced data for $n=1$ and $n=2$, for $b$ between $0$ and $d$ and
      for a limited range of values of $d$.  Other inputs will produce an error message.  Our hope is
      that as we (or others) are able to compute new data, we will be able to update the package.

      
      One of the main functions is @ TO totalBettiTally @, which produces the standard graded
      Betti table of the corresponding standard graded Veronese module.
      Other main functions refine the data in the Betti table
      by providing the multigraded Betti number or the Schur functor decompositions,
      or by computing statistics related to the Betti table (e.g., the BoijSoederberg
      coefficients) or related to the SchurFunctor decomposition.

      A number of functions in this package produce individual entries of a Betti table.
      There are two common notations for referring to Betti numbers in the literature,
      and it will be useful to reference these notations throughout the documentation,
      similar to how they are referenced in the corresponding paper.
      For a graded module $M$ we will write $\beta_{i,j}(M)$ for $\dim Tor_i(M,k)_j$
      and we write $K_{p,q}(M)$ for the vector space $Tor_p(M,k)_{p+q}$. 
///


doc ///
   Key 
    makeBettiTally
    (makeBettiTally,HashTable)
   Headline
    converts a hash table representing a Betti table to a Betti tally
   Usage
    makeBettiTally(H)
   Inputs
    H: HashTable
   Outputs
    : BettiTally
   Description
    Text
      Given a hash table $H$ whose keys are pairs of integers $(p,q)$ 
      this function presents the data in the Betti tally format.
      For instance, combining this with the @ TO totalBetti @ function reproduces
      the standard Betti table.  By contrast, combining this with the
      numRepsBetti function produces a table where the entry in position
      $(p,q)$ is the number of Schur functors in the representation corresponding
      to that Betti table entry.
    Example
      H = totalBetti(3,2,0);
      makeBettiTally H
      makeBettiTally numRepsBetti(3,2,0)

///

doc ///
   Key 
    totalBetti
    (totalBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table containing the graded Betti numbers of a Veronese embedding
   Usage
    totalBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This is a hash table for the total numbers of $\mathcal{O}(b)$
      under the embedding by $\mathcal{O}(d)$. The keys of the hash
      table $H$ are pairs $(p,q)$ where $H#(p,q)$ gives the rank of 
      $K_{p,q}(\mathbb{P}^n, d;b)$. This equals the Betti number
      $\beta_{p,p+q}(d,\mathbb{P}^n,b)$.Some tables are incomplete and 
      we mark unknown entries with infinity.
      
      Note that totalBetti differs from @ TO totalBettiTally @ only in that the output is 
      a hash table instead of a Betti tally. One can convert the output of
      totalBetti into a Betti tally via the @ TO makeBettiTally @ function.

      
      In example below we generate a hash table showing the total graded Betti numbers
      of $\mathbb{P}^{2}$ embedded by $\mathcal{O}(3)$. 
    Example
      B = totalBetti(3,2,0)
    Text  
      If we wish to view these graded Betti numbers in the usual fashion, we can use
      makeBettiTally to convert the hash table above to a Betti tally.
    Example
      makeBettiTally B  
///

doc ///
   Key 
    totalBettiTally
    (totalBettiTally,ZZ,ZZ,ZZ)
   Headline
    a Betti tally containing the graded Betti numbers of a Veronese embedding
   Usage
    totalBettiTally(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : BettiTally
   Description
    Text
      This function returns a Betti tally for the total graded Betti numbers 
      of $\mathcal{O}(b)$ under the embedding by 
      $\mathcal{O}(d)$. Some tables are incomplete and we mark
      unknown entries with infinity.
      
      Note that totalBettiTally differs from @ TO totalBetti @ only in that the output is 
      a Betti tally instead of a hash table. 
      
      In example below we generate a hash table showing the total graded Betti numbers
      of $\mathbb{P}^{2}$ embedded by $\mathcal{O}(3)$. 
    Example
      totalBettiTally(3,2,0)
    Text
      We can also produce the Betti tables of the pushforwards of line bundles.  For instance,
      the following example computes the Betti table of the
      pushforward of $\mathcal{O}(1)$ under
      the 3-uple embedding.
    Example
      totalBettiTally(3,2,1)  
///

doc ///
   Key 
    schurBetti
    (schurBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table for Schur module decomposition of Veronese Betti tables
   Usage
    schurBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This function returns a hash table with the Schur functor decompositions 
      of the syzygies of $\mathcal{O}(b)$ under the embedding by
      $\mathcal{O}(d)$. The keys of the hash
      table $H$ are pairs $(p,q)$ where $H#(p,q)$ gives the Schur functor 
      decomposition of $K_{p,q}(\mathbb{P}^n, d;b)$. $\mathcal{O}(b)$ the 
      Schur functor decomposition as a list of tuples $(\{a_1,a_2,a_3\},b)$ where
      $\{a_1,a_2,a_3\}$ specifies the weight of the Schur functor and $m$ the multiplicity
      with which that particular Schur functor appears in the decomposition
      of $K_{p,q}(\mathbb{P}^n, d;b)$. 
      
      Some tables are incomplete and we mark unknown entries with 
      ({0,0,0},infinity).
      
    Example
      schurBetti(3,2,0)
///
doc ///
   Key
    multiBetti
    (multiBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table containing the multigraded Betti numbers of a Veronese embedding 
   Usage
    multiBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This function returns a hash table $H$ containing the multigraded Betti numbers
      for $\mathcal{O}(b)$ on $\mathbb{P}^{n}$ under the embedding by
      $d$-fold Veronese embedding given by $\mathcal{O}(d)$. The keys 
      of the returned hash table $H$ are pairs $(p,q)$ where $H#(p,q)$ gives the the
      multigraded Betti decomposition of $K_{p,q}(\mathbb{P}^n, d;b)$. We record the 
      multigraded Betti numbers via a multigraded Hilbert series. See Section 1.1 of 
      [BEGY] for a more precise description of the multigraded Betti numbers of a 
      Veronese embedding.
      
      Note that the output of this function is sometimes enormous and so
      can take a long time to print on the screen.
    Example
      totalBettiTally(3,2,0)
      multiBetti(3,2,0)
///


doc ///
   Key 
    dominantWeightsBetti
    (dominantWeightsBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table containing the dominant Schur functors of a Veronese embedding 
   Usage
    dominantWeightsBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This function returns a hash table $H$ whose keys are pairs $(p,q)$ such that
      the corresponding value $H#(p,q)$ is a list of the dominant weights appearing
      in the Schur functor decomposition of $K_{p,q}(\mathbb{P}^n, d;b)$. The Schur
      functors are recorded via the corresponding partitions. See Section 1.3 of [BEGY].
      
    Example
      totalBettiTally(3,2,0)
      D = dominantWeightsBetti(3,2,0);
      D#(1,1)
      D#(5,1)
      D#(7,2)
      
///

doc ///
   Key 
    lexWeightsBetti
    (lexWeightsBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table containing the lex-leading weight Schur functors of a Veronese embedding 
   Usage
    lexWeightsBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This function returns a hash table $H$ whose keys are pairs $(p,q)$ such that
      the corresponding value $H#(p,q)$ is a list of the lexicographically maximal weights of the Schur
      functors appearing in the Schur functor decomposition of 
      $K_{p,q}(\mathbb{P}^n, d;b)$.
      See Section 1.3 of [BEGY].
      
    Example
      totalBettiTally(3,2,0)
      lexWeightsBetti(3,2,0)
   
///

doc ///
   Key 
    numDistinctRepsBetti
    (numDistinctRepsBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table containing the number of distinct Schur functors of a Veronese embedding 
   Usage
    numDistinctRepsBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This function returns a hash table $H$ whose keys are pairs $(p,q)$ such that
      the corresponding value $H#(p,q)$ is the number of distinct Schur functors
      appearing in the Schur functor decomposition of $K_{p,q}(\mathbb{P}^n, d;b)$.
      Note that the function @ TO numRepsBetti @ is similar, though it counts Schur functors
      appearing with multiplicity.
      
    Example
      totalBettiTally(3,2,0)
      numDistinctRepsBetti(3,2,0)
      makeBettiTally oo
    Text
      This example shows that the $\beta_{3,4}(3,2,0)$ entry, which is $189$,
      consists of $7$ distinct Schur functors.  The specific
      Schur functors that appear can be computed using @ TO schurBetti @.  Here is a
      more complicated example:
    Example
      totalBettiTally(5,2,1)
      makeBettiTally numDistinctRepsBetti(5,2,1)
      
///

doc ///
   Key 
    numRepsBetti
    (numRepsBetti,ZZ,ZZ,ZZ)
   Headline
    a hash table containing the number of Schur functors of a Veronese embedding 
   Usage
    numRepsBetti(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : HashTable
   Description
    Text
      This function returns a hash table $H$ whose keys are pairs $(p,q)$ such that
      the corresponding value $H#(p,q)$ is the number of Schur functors appearing
      in the Schur functor decomposition of $K_{p,q}(\mathbb{P}^n, d;b)$ counted
      with multiplicity. Note that the function @ TO numDistinctRepsBetti @ is similar, 
      though that ignores the multiplicities of the Schur functors. 
      
    Example
      totalBettiTally(3,2,0)
      numRepsBetti(3,2,0)
      makeBettiTally oo
    Text
      This example shows that the $\beta_{3,4}(3,2,0)$ entry, which is $189$,
      consists of $7$ Schur functors.  (In this case, the Schur functors all appear with multiplicity 1.)
      The specific Schur functors that appear can be computed using @ TO schurBetti @.  Here is an example where some Schur 
      functors appear with higher multiplicity. 
    Example
      totalBettiTally(5,2,1)
      makeBettiTally numRepsBetti(5,2,1)
     
///

doc ///
   Key 
    bsCoeffs
    (bsCoeffs,ZZ,ZZ,ZZ)
   Headline
    a list of the Boij-Soederberg coefficients of a Veronese embedding 
   Usage
    bsCoeffs(d,n,b)
   Inputs
    d: ZZ
    n: ZZ
    b: ZZ 
   Outputs
    : List
   Description
    Text
      This function returns a list of the Boij-Soederberg coefficients for the 
      decomposition of the Betti table of $\mathcal{O}(b)$ on 
      $\mathbb{P}^{n}$ under the $d$-fold Veronese embedding. See Section 6.3 of [BEGY].  Of course,
      these coefficients and the corresponding pure diagrams are easily computed
      by applying @ TO "BoijSoederberg::decompose(BettiTally)" @ from the BoijSoederberg
      package to the corresponding totalBettiTally.
      
    Example
      totalBettiTally(3,2,0)
      bsCoeffs(3,2,0)
      totalBettiTally(4,2,2)
      bsCoeffs(4,2,2)
///