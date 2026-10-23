------------------------------------------------------------------------
-- HookPieri/doc.m2
-- The documentation of the package HookPieri, loaded by HookPieri.m2 after
-- beginDocumentation().
------------------------------------------------------------------------

doc ///
    Key
        HookPieri
    Headline
        Pieri maps, straightening, and finite hook algebras
    Description
        Text
            @TT "HookPieri"@ computes the straightening relations of products
            of hook tableaux, the Pieri maps between hook Schur functors that
            produce them, the finite hook algebras presented by them, and the
            determinantal Poincaré series of the accompanying paper
            Hook algebras and determinantal varieties by
            Steven V Sam, Keller VandeBogert, and Jerzy Weyman.
        Text
            @BOLD "Tabloids and straightening."@  A hook of type $(a,b)$
            is the partition $(a+1,1^b)$: $a$ boxes to the right of its
            corner and $b$ below it.  Semistandard fillings of this hook on
            $1,\ldots,n$ form a basis of the hook Schur functor
            $\mathbf S_{(a+1,1^b)}(\mathbf k^n)$.  A hook tabloid displays a
            product of hook tableaux by nesting them along the diagonal of
            one Young diagram.
        Text
            A @TO HookTabloid@ is
            entered by its rows, top row first, through @TO tabloid@; every
            function that takes a tabloid also accepts the list of rows.  The
            main command, @TO hookStraighten@, writes a tabloid in the
            semistandard hook basis (weakly increasing rows and strictly
            increasing columns) and returns a hash table from semistandard
            tabloids to coefficients.  Here the two diagonal
            hooks of $T$ are semistandard separately but their product is
            not.  @TO straighteningStep@ performs one rewrite of the
            algorithm of Section 6 of the paper; here both terms it produces
            still need straightening.  @TO hookStraighten@ gives the full
            normal form, either from $T$ or from this intermediate result.
        Example
            T = tabloid {{1,2,2},{2,1,1},{3,3}}
            isHookSemistandard T
            firstStep = straighteningStep T
            apply(sort keys firstStep, isHookSemistandard)
            nf = hookStraighten T
            hookStraighten firstStep === nf
        Text
            In the hook algebra, the table @TT "firstStep"@ is the relation
            $$
            \begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
            = -2\,\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
            + \begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}.
            $$
            Neither tableau on the right is semistandard.  The table
            @TT "nf"@ gives the completed straightening relation
            $$
            \begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
            = \begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}.
            $$
            The coefficient of the final tableau is $1$; the last
            @TT "true"@ checks that straightening @TT "firstStep"@ gives
            this same result.
        Text
            @BOLD "Pieri maps."@  The relations come from the crossed
            hook Pieri maps of Section 4 of the paper.  For $V=\mathbf k^n$
            in characteristic zero and nested hook types $(a,b)$ and
            $(c,d)$, with $a>c$ and $b>d$, these are the injections
            $$
            \mathbf S_{(c+1,1^b)}(V)\otimes\mathbf S_{(a+1,1^d)}(V)
            \xrightarrow{\ \varphi\ }
            \mathbf S_{(a+1,1^b)}(V)\otimes\mathbf S_{(c+1,1^d)}(V).
            $$
            Their images are the linear combinations of hook products
            that become zero in the hook algebra.  @TO pieriMatrix@
            builds the matrix of the map for two hook types on $n$ letters,
            with its source and target bases of pairs of tabloids stored on
            the matrix, and @TO pieriImage@ reads a column as a combination
            of target pairs.
        Example
            M = pieriMatrix({2,2},{0,0},3);
            (numRows M, numColumns M, rank M)
            first pieriSourceBasis M
            pieriImage(M, 0)
        Text
            The types @TT "{2,2}"@ and @TT "{0,0}"@ mean the shapes
            $(3,1,1)$ and $(1)$, so the matrix above represents
            $$
            \bigwedge^3 V\otimes\operatorname{Sym}^3(V)
            \xrightarrow{\ \varphi\ }
            \mathbf S_{(3,1,1)}(V)\otimes V,
            \qquad V=\mathbf k^3.
            $$
            The output $(18,10,10)$ says there are
            $18$ target basis vectors and $10$ independent relations; the
            cokernel is the $8$-dimensional Schur functor
            $\mathbf S_{(3,2,1)}(\mathbf k^3)$.  The last table describes
            the image of the first source basis vector: three times the
            displayed target pair.  As an element of the relation space,
            this combination is zero in the hook algebra.
        Text
            @BOLD "Hook rings and hook algebras."@  In characteristic
            zero, the universal hook algebra has the decomposition
            $$
            \mathcal H(V)\cong
            \bigoplus_{\ell(\lambda)\le\dim V}\mathbf S_\lambda(V),
            \qquad
            \deg\mathbf S_\lambda(V)=(r(\lambda),|\lambda|)
            $$
            as a bigraded $\mathrm{GL}(V)$-representation.  Here $r(\lambda)$
            is the number of diagonal boxes and $|\lambda|$ is the total
            number of boxes.  Its generators are the hook summands
            $\mathbf S_{(a+1,1^b)}(V)$, of bidegree $(1,a+b+1)$.
            Multiplying
            strictly nested hooks (with both arms and legs decreasing)
            projects onto the Schur functor of their
            assembled shape; products of nonnested hook types are zero.
        Text
            In @TT "hookRing(n, types)"@ and @TT "hookAlgebra(n, types)"@,
            $n=\dim V$ and @TT "types"@ is a list of pairs
            @TT "{arm, leg}"@ specifying a finite set $\mathcal T$ of hook
            types.  The two commands construct, respectively,
            $$
            R_{\mathcal T}=\operatorname{Sym}\!\left(
            \bigoplus_{(a,b)\in\mathcal T}\mathbf S_{(a+1,1^b)}(V)\right),
            \qquad A_{\mathcal T}=R_{\mathcal T}/I_{\mathcal T},
            $$
            where $I_{\mathcal T}$ is the ideal returned by
            @TO straighteningIdeal@.  Thus $A_{\mathcal T}$ is the
            subalgebra of $\mathcal H(V)$ generated by the chosen hook
            summands.  The variables @TT "z0, z1, ..."@ index their
            semistandard basis vectors, and @TO tabloid@ recovers the
            tableau of a variable.  These are ordinary polynomial and
            quotient rings of Macaulay2.
        Text
            In the call below, @TT "3"@ specifies $V=\mathbb Q^3$ and
            @TT "{{0,0},{1,1},{2,2}}"@ selects the hook shapes
            $(1)$, $(2,1)$, and $(3,1,1)$.  The resulting algebra is
            graded by the number of hook factors, the first of the two
            gradings above; in particular, every ring variable has degree
            $1$, even though these three hook shapes have different sizes.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}})
            I = ideal A;
            numgens A, numgens I
            tabloid A_4
            hookRelation(A, 4, 14)
            hookStraighten(T, A)
            reduceHilbert hilbertSeries A
            hasQuadraticGroebnerBasis A
        Text
            The rank-graded pieces in this example are
            $$
            \begin{aligned}
            A_0&=\mathbb Q,\\
            A_1&\cong V\oplus\mathbf S_{(2,1)}(V)
                \oplus\mathbf S_{(3,1,1)}(V),\\
            A_2&\cong\mathbf S_{(2,2)}(V)\oplus\mathbf S_{(3,2,1)}(V)
                \oplus\mathbf S_{(3,3,2)}(V),\\
            A_3&\cong\mathbf S_{(3,3,3)}(V),\qquad A_i=0\ (i>3).
            \end{aligned}
            $$
            Thus $\dim A_1=3+8+6=17$,
            $\dim A_2=6+8+3=17$, and $\dim A_3=1$, explaining the
            Hilbert series $1+17q+17q^2+q^3$.  The other number in
            $(17,136)$ counts the defining quadrics.  The
            displayed relation means
            $z_4z_{14}=-z_2z_{15}-z_1z_{16}$ in $A$, while $z_0z_{12}$ is
            the normal form of the earlier tabloid $T$ in $A$.  The final
            @TT "true"@ verifies that the quadratic relations form a
            Gröbner basis in the chosen order.
        Text
            @BOLD "Fine Segre products."@  Let $S$ be the polynomial ring
            on the entries of a generic matrix and $I_r$ its ideal of
            $r\times r$ minors.  The groups
            $\operatorname{Tor}^S_i(S/I_r,\mathbf k)$ record the generators
            in the minimal $S$-free resolution of $S/I_r$ and together
            carry an algebra structure.  For the generic determinantal
            ring the paper degenerates the Tor algebra
            $\operatorname{Tor}^S(S/I_r,\mathbf k)$ to a Zhang twist of the
            fine Segre product $A_V \boxtimes A_W$ of two finite hook
            retracts (Section 7.4.3).  @TO fineSegreAlgebra@ builds that
            fine Segre product as a quotient ring, exactly as
            @TO hookAlgebra@ builds a hook algebra, and @TO fineSegreRing@
            is its ambient polynomial ring.  Every variable is a pair of
            hook tableaux, one from each factor, and @TO tabloid@ of a
            variable returns that pair; @TO hookTypes@ lists the triples of
            a base hook type with its left and right types, and the
            variables come in blocks of decreasing base type.  For the
            $2 \times 2$ minors of a $3 \times 3$ matrix the blocks have
            $9, 8, 8, 9$ variables, indexed by the same pairs of tableaux as
            $\operatorname{Tor}^S_i(S/I_2,\mathbf k)$ for $i = 3, 2, 2, 1$,
            whose Betti numbers are $9, 16, 9$, and the single standard
            quadratic monomial spans $\operatorname{Tor}_4$.
            @TO relationKind@ classifies the quadratic monomials: a pair is
            standard when both factor pairs are semistandard, bad on each
            side whose factor pair is not, and zero when the base types are
            not strictly nested; @TO hookRelation@ is the straightening
            relation of a bad pair, which reduces to a multiple of the
            standard monomial or to zero, and in the algebra the products
            are already reduced.
        Example
            B = fineSegreAlgebra(3,3,2)
            hookTypes B
            numgens B, numgens ideal B
            tabloid B_27
            tally flatten for i from 0 to numgens B - 1 list for j from i to numgens B - 1 list relationKind(B, i, j)
            relationKind(B, 0, 33), relationKind(B, 1, 32), relationKind(B, 0, 27)
            W = ambient B;
            hookRelation(B, 1, 32)
            B_0 * B_33, B_1 * B_32, B_0 * B_27
            reduceHilbert hilbertSeries B
        Text
            There are $34$ generators and $594$ defining quadrics.  The
            relation $w_1w_{32}+w_0w_{33}$ explains the negative sign in the
            second product; the third product is zero.  The Hilbert
            variables $T_0,\ldots,T_3$ mark the four listed base hook
            types.  Their coefficients $9,8,8,9$ count the generators in
            these types, while $T_0T_3$ records the one-dimensional space
            of quadratic products, spanned by $w_0w_{33}$.
        Text
            @BOLD "Poincaré series: cubic generic minors."@  The command
            @TT "genericDetPoincare(n, m, r)"@ concerns
            $R=S/I_r$, where $S=\operatorname{Sym}(V\otimes W)$,
            $\dim V=n$, $\dim W=m$, and $I_r$ is generated by the
            $r\times r$ minors of a generic $n\times m$ matrix.  Matrix
            entries have internal degree one.  It returns
            $$P_R(t,s)=\sum_{i,j}
            [\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j]t^i s^j.$$
            Brackets denote characters, $t$ records homological degree,
            and $s$ records internal degree.  The examples below have
            cubic defining equations and are neither Koszul nor Golod;
            the formula determines their entire residue-field Poincaré
            series from finite equivariant data over $S$.
        Text
            The output uses Schur polynomials in two sets of variables:
            $$\begin{aligned}
            x_\lambda&=s_\lambda(x_1,\ldots,x_n)=[\mathbf S_\lambda(V)],\\
            y_\mu&=s_\mu(y_1,\ldots,y_m)=[\mathbf S_\mu(W)].
            \end{aligned}$$
            Thus $x_\lambda y_\mu$ denotes
            $\mathbf S_\lambda(V)\otimes\mathbf S_\mu(W)$; sums with
            nonnegative coefficients denote direct sums with those
            multiplicities.  The empty partition denotes the trivial
            character.  These alphabets encode representations, not
            matrix entries.  We take the $3\times3$ minors of a generic
            $5\times5$ matrix: the first two arguments give the matrix
            dimensions and the third gives the minor size.
        Example
            P = genericDetPoincare(5,5,3);
            L = dimensionSpecialization P;
            factor L#0
            L#1
        Text
            The two outputs are the numerator and denominator of the
            numerical series (characters replaced by dimensions and $s=1$):
            $$P_R(t,1)=\frac{(1+t)^{25}}{D(t)},$$
            $$\begin{aligned}
            D(t)={}&1-100t^2-450t^3-828t^4-700t^5\\
                   &+700t^7+828t^8+450t^9+100t^{10}-t^{12}.
            \end{aligned}$$
            The Betti numbers are coefficients of the quotient,
            not coefficients of $D(t)$.
        Example
            betti(P, 4)
            expandPoincare(P, 2)
        Text
            In @TT "betti(P, 4)"@, column $i$ and row $j-i$ give
            $\dim\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j$.
            The column totals $1,25,400,5250,64728$ stop at homological
            degree four; the resolution continues.  The equivariant
            table explains the first off-diagonal entry explicitly:
            $$\begin{aligned}
            \operatorname{Tor}^R_1(\mathbf k,\mathbf k)_1
              &\cong V\otimes W,\\
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_2
              &\cong\bigl(\operatorname{Sym}^2V\otimes\bigwedge^2W\bigr)
                \oplus\bigl(\bigwedge^2V\otimes\operatorname{Sym}^2W\bigr),\\
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_3
              &\cong\bigwedge^3V\otimes\bigwedge^3W.
            \end{aligned}$$
            Here $\dim V=\dim W=5$.  The two internal degrees in
            homological degree two have dimensions
            $15\cdot10+10\cdot15=300$ and $10\cdot10=100$, respectively.
            Thus $400=300+100$; the cubic minors supply the second
            summand, printed in row $1$.  Its presence rules out Koszulity.
        Text
            The finite resolution supplying the formula is a different
            one: that of $R$ over the polynomial ring $S$.
        Example
            strandBettiTable P
            isGolodPoincare P
        Text
            This table lists $\operatorname{Tor}^S_i(R,\mathbf k)_j$.
            For these cubic equations, a strand $d=(j-i)/2$ contributes
            $$(-1)^d[\operatorname{Tor}^S_i(R,\mathbf k)_{i+2d}]
              s^{i+2d}t^{i+d}$$
            to the equivariant denominator.  In particular, column $4$,
            row $4$ is
            $$\operatorname{Tor}^S_4(R,\mathbf k)_8
              \cong\mathbf S_{(2,2,2,2)}(V)
                    \otimes\mathbf S_{(2,2,2,2)}(W),$$
            of dimension $15\cdot15=225$.  Its second-strand contribution
            is $+225s^8t^6$, beyond the first-strand terms of a Golod
            formula; the test accordingly returns @TT "false"@.
            Setting $s=1$ hides this particular contribution, since it
            cancels the first-strand term $-225s^7t^6$.
        Text
            @BOLD "Cubic symmetric minors."@  In
            @TT "symDetPoincare(n, r)"@, $r$ is again the minor size,
            but the matrix is symmetric.  Take
            $$R_{\mathrm{sym}}=
              \operatorname{Sym}(\operatorname{Sym}^2V)/I_3,
              \qquad V=\mathbf k^5.$$
            There are $15$ degree-one matrix entries.  Now only the
            alphabet $x_\lambda=[\mathbf S_\lambda(V)]$ occurs.
        Example
            Ps = symDetPoincare(5,3);
            Ls = dimensionSpecialization Ps;
            factor Ls#0
            Ls#1
            betti(Ps, 4)
        Text
            The numerical fraction is
            $$P_{R_{\mathrm{sym}}}(t,1)=
            \frac{(1+t)^{15}}
            {1-50t^2-175t^3-252t^4-175t^5-50t^6+t^8}.$$
            The table gives total Betti numbers
            $1,15,155,1380,11992$.  Its column labeled $2$ separates the
            $105$ internal-degree-two classes from the $50$
            internal-degree-three classes.  Keeping their representations:
        Example
            expandPoincare(Ps, 2)
            isGolodPoincare Ps
        Text
            The characters $x_{(3,1)}$ in row $0$ and $x_{(2,2,2)}$
            in row $1$ mean
            $$\begin{aligned}
            \operatorname{Tor}^{R_{\mathrm{sym}}}_2(\mathbf k,\mathbf k)_2
              &\cong\mathbf S_{(3,1)}(V),\\
            \operatorname{Tor}^{R_{\mathrm{sym}}}_2(\mathbf k,\mathbf k)_3
              &\cong\mathbf S_{(2,2,2)}(V).
            \end{aligned}$$
            The latter is the $50$-dimensional space of cubic equations.
            A later off-diagonal entry is
            $$\operatorname{Tor}^{R_{\mathrm{sym}}}_4(\mathbf k,\mathbf k)_6
              \cong\mathbf S_{(2,2,2)}(V)^{\otimes2},
              \qquad\dim=2500.$$
            This identifies the representation, not its multiplication
            in Tor.  The positive denominator term $t^8$ comes from
            $$\operatorname{Tor}^{S}_6(R_{\mathrm{sym}},\mathbf k)_{10}
              \cong(\det V)^4,\qquad
              S=\operatorname{Sym}(\operatorname{Sym}^2V).$$
            It has strand index $d=(10-6)/2=2$ and contributes
            $+s^{10}t^8$ after taking dimensions.  Again the Golod test
            returns @TT "false"@.
        Text
            @BOLD "Cubic Pfaffians and a higher-strand correction."@
            In @TT "pfaffianPoincare(n, r)"@, $n$ is the size of the
            skew-symmetric matrix and $r$ is @EM "half"@ the Pfaffian
            order.  Thus @TT "(8,3)"@ means the $6\times6$ Pfaffians
            of an $8\times8$ matrix, not $3\times3$ Pfaffians.  Put
            $$R_{\mathrm{pf}}=S/I_6,\qquad
              S=\operatorname{Sym}(\bigwedge^2V),\quad V=\mathbf k^8.$$
            There are $28$ degree-one matrix entries and $28$ cubic
            Pfaffians.  We retain internal degree in the numerical
            numerator and denominator:
        Example
            Pf = pfaffianPoincare(8,3);
            Lf = dimensionSpecialization(Pf, KeepInternalDegree => true);
            factor Lf#0
            Lf#1
        Text
            These outputs give $P_{R_{\mathrm{pf}}}(t,s)=(1+st)^{28}/D(t,s)$
            after taking dimensions, where
            $$\begin{aligned}
            D(t,s)={}&1-28s^3t^2-63s^4t^3-36s^5t^4\\
                    &+36s^7t^5+63s^8t^6+28s^9t^7-s^{12}t^9.
            \end{aligned}$$
        Example
            betti(Pf, 5)
            expandPoincare(Pf, 2)
        Text
            The column totals are $1,28,406,4123,33643,240274$.
            The equivariant output splits $406=378+28$ as
            $$\begin{aligned}
            \operatorname{Tor}^{R_{\mathrm{pf}}}_2(\mathbf k,\mathbf k)_2
              &\cong\mathbf S_{(2,1,1)}(V),\\
            \operatorname{Tor}^{R_{\mathrm{pf}}}_2(\mathbf k,\mathbf k)_3
              &\cong\bigwedge^6V.
            \end{aligned}$$
            These are respectively $x_{(2,1,1)}$ in row $0$ and
            $x_{(1,1,1,1,1,1)}$ in row $1$; the second is the space
            of cubic Pfaffians.  To see information beyond these initial
            classes, compare with the ambient-ring resolution:
        Example
            strandBettiTable Pf
            isGolodPoincare Pf
        Text
            Column $3$, row $4$ now means
            $$\operatorname{Tor}^{S}_3(R_{\mathrm{pf}},\mathbf k)_7
              \cong\mathbf S_{(2,2,2,2,2,2,2)}(V),\qquad\dim=36.$$
            This second-strand term contributes $+36s^7t^5$ to $D$.
            It is not itself a residue-field Betti number: the denominator
            must first be inverted.  For example, the entry $25444$ in
            column $5$, row $2$ of @TT "betti(Pf, 5)"@ is
            $$\begin{aligned}
            \dim\operatorname{Tor}^{R_{\mathrm{pf}}}_5(\mathbf k,\mathbf k)_7
              &=[s^7t^5]\frac{(1+st)^{28}}{D(t,s)}\\
              &=28\cdot28^2+2\cdot28\cdot63-36=25444.
            \end{aligned}$$
            The $-36$ is precisely the contribution of that higher strand
            upon inversion.
            The Golod test returns @TT "false"@, and Koszulity cannot
            recover this series from the Hilbert series: the displayed
            residue-field resolution is not linear.
        Text
            @BOLD "Verifications."@  The checks that examine a whole shape
            or ring return a @TO VerificationReport@ with the verdict at the
            key @TT "\"passed\""@ and the counts of the check and the
            cases it found.
        Example
            R = verifyTriangularity({3,2}, 3)
            R#"passed"
        Text
            This check examines the $45$ hook tabloids of shape $(3,2)$
            whose individual hooks are semistandard on three letters.
            Of these, $15$ are already semistandard as whole diagrams.
            The verdict says every term produced by a rewrite of one of
            the remaining $30$ strictly decreases the running order;
            this verifies termination on the tested set, not the theorem
            for all shapes.
        Text
            @UL {
                TO "Hook algebra quick start",
                TO HookTabloid,
                TO hookStraighten,
                TO straighteningStep,
                TO verifyTriangularity,
                TO pieriMatrix,
                TO hookRing,
                TO hookAlgebra,
                TO fineSegreRing,
                TO genericDetPoincare,
                TO symDetPoincare,
                TO pfaffianPoincare,
                TO EquivariantBettiTally,
                TO VerificationReport,
                TO "Examples from the paper"
            }@
        Text
            In characteristic zero the straightening relations form a
            quadratic Gröbner basis, the semistandard hook tabloids are the
            standard monomials, and the universal hook algebra
            $\mathcal H(V)$ is Koszul; as a $\mathrm{GL}(V)$-representation
            it contains every irreducible polynomial representation exactly
            once.  The hook-ring constructors and straightening and
            Pieri routines accept @TT "CoefficientRing => K"@, with
            @TO QQ@ the default.  They also support prime fields when
            the required denominators and elimination matrices remain
            invertible; see the caveat on @TO hookAlgebra@.  The
            Poincaré-series constructors and Frobenius-union routines
            use rational coefficients.
///

doc ///
    Key
        "Hook algebra quick start"
    Headline
        from a hook tabloid to its Pieri relation
    Description
        Text
            @BOLD "1. Straighten a tabloid."@  Enter its rows from top to
            bottom.  Semistandard means weakly increasing along rows and
            strictly increasing down columns.  A diagonal hook consists
            of a diagonal box and the boxes to its right and below it.
            In this example the individual diagonal hooks are
            semistandard, but the assembled tableau is not.
        Example
            T = tabloid {{1,2,3},{2,1},{3}}
            hookDecomposition T
            isHookSemistandard T
            nf = hookStraighten T
        Text
            The hash table gives the following equality in the hook
            algebra (each bracket denotes the product of the displayed
            tableau's diagonal hooks):
            $$\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
            =-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
             -\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right].$$
            Thus each key of @TT "nf"@ is a semistandard @TO HookTabloid@
            and its value is its coefficient in this sum.  Read a
            coefficient with @TT "nf#U"@ when @TT "U"@ is a key of the
            table; absent keys have coefficient zero.  Use @TO sort@ to
            order the keys in the total order of tabloids.
        Example
            nf#(tabloid {{1,1,2},{2,3},{3}})
            longNF = hookStraighten {{4,5,6},{7,1,2},{8}};
            #longNF
            first sort keys longNF
        Text
            The default strategy is the local algorithm of Section 6 of the
            paper; @TT "Strategy => \"Cokernel\""@ computes the same normal
            form by linear algebra on the Pieri cokernel, and the two agree.
        Example
            hookStraighten(T, Strategy => "Cokernel") === nf
        Text
            @BOLD "2. Build a finite hook algebra."@  Choose the hook types
            that generate it.  The type $(a,b)$ denotes the hook
            $(a+1,1^b)$, and the first argument is $n=\dim V$, with
            $V=\mathbb Q^n$ by default.  The algebra is the quotient of the @TO hookRing@,
            whose variables are indexed by the semistandard tableaux of the
            chosen types, by the @TO straighteningIdeal@.  Here the construction is
            $$A=\operatorname{Sym}\!\left(V\oplus\mathbf S_{(2,1)}(V)
            \oplus\mathbf S_{(3,1,1)}(V)\right)/I,\qquad V=\mathbb Q^3,$$
            where $I$ imposes the hook multiplication relations.  Each
            generator has degree one: this grading counts hooks, not boxes.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}})
            numgens A, numgens ideal A
            hookBasis(A, {2,2})
        Text
            The output $(17,136)$ counts the $3+8+6=17$ basis generators
            and $136$ quadratic relations.  The six tableaux returned by
            @TT "hookBasis(A, {2,2})"@ form a basis of
            $\mathbf S_{(3,1,1)}(\mathbb Q^3)$, the third generating summand.
            Generator indices are zero based.  The variables are named
            @TT "z0, z1, ..."@; refer to generator $i$ as @TT "A_i"@.
            For example, after @TT "use A"@, @TT "z5"@ names @TT "A_5"@;
            @TT "z_5"@ is a different indexed symbol.
            The ring @TT "ambient A"@ is the @TO hookRing@ the algebra was built
            from.  @TO hookVariable@ returns the variable of a
            given tableau and @TO tabloid@ inverts it; @TO hookRelation@
            returns the stored relation of a pair of generators, which is
            the algebraic version of the first straightening calculation,
            and @TO relationKind@ names its kind.
        Example
            z = hookVariable(A, {{1,2,3},{2},{3}})
            tabloid z
            hookRelation(A, 4, 14)
            relationKind(A, 4, 14)
        Text
            The normal form of a tabloid in the algebra is
            @TO (hookStraighten, Thing, Ring)@, and @TO tabloidTerms@ reads an
            element of the algebra back as a combination of tabloids.
        Example
            f = hookStraighten(T, A)
            tabloidTerms f
            use A; z4*z14 == f
        Text
            These outputs express the same tableau identity as above:
            $$z_4z_{14}=-z_2z_{15}-z_1z_{16}\quad\text{in }A.$$
            The polynomial returned by @TO hookRelation@ is
            $z_4z_{14}+z_2z_{15}+z_1z_{16}\in I$; @TT "tabloidTerms f"@
            reads the normal form of the product $z_4z_{14}$ as tableau
            coefficients.
        Text
            @BOLD "3. Inspect the Pieri map."@  The quadratic relations
            come from crossed hook Pieri maps.  For the hook types $(2,2)$
            and $(0,0)$ on three letters, the relevant map is
            $$\varphi:\bigwedge^3 V\otimes\operatorname{Sym}^3 V
            \longrightarrow\mathbf S_{(3,1,1)}(V)\otimes V,
            \qquad V=\mathbb Q^3.$$
        Example
            M = pieriMatrix({2,2},{0,0},3);
            (numRows M, numColumns M, rank M)
            first pieriSourceBasis M
            pieriImage(M, 0)
        Text
            Thus $M$ has $18$ rows, $10$ columns, and rank $10$:
            $\varphi$ is injective and its cokernel is
            $\mathbf S_{(3,2,1)}(V)$ of dimension $8$.  The first column says
            $$\varphi\!\left(
            \left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
            \otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]\right)
            =3\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
            \otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right].$$
            @TO pieriSourceBasis@ and @TO pieriTargetBasis@ list the pairs of
            tabloids indexing the columns and the rows, @TO pieriImage@
            expands a column, and @TO pieriPreimage@ solves for the
            coordinates of a target pair modulo the complement spanned by
            the semistandard nested pairs, which is how the relations of
            Section 4 of the paper are read off the matrix.
        Example
            pieriPreimage(M, first pieriTargetBasis M)
        Text
            The coefficient $1/3$ is the preimage of the target pair in
            this equation: applying $\varphi$ to one third of the source
            gives that target exactly.
        Text
            @BOLD "4. Check the algorithm."@  @TO straighteningStep@
            performs one rewrite of the algorithm, @TO verifyTriangularity@
            checks on the specified finite alphabet that every step decreases
            the running order (so the algorithm terminates on this set), and
            @TO verifyStraighteningAgainstCokernel@ compares every normal
            form with the Pieri-cokernel computation.  The two
            verification functions return a @TO VerificationReport@.
        Example
            straighteningStep T
            V = verifyTriangularity({3,2}, 3)
            V#"passed"
            (verifyStraighteningAgainstCokernel({3,2}, 3))#"passed"
        Text
            For shape $(3,2)$ on the letters $1,2,3$, the report checks
            $45$ fillings whose individual hooks are semistandard.  Of
            these, $15$ are already semistandard as whole tableaux and
            $30$ require a step.  The two @TT "true"@ values certify the
            order and cokernel checks on this finite set, not a proof for
            all shapes and alphabets.
        Text
            @BOLD "5. Poincaré series."@  The determinantal Poincaré series
            are built by @TO genericDetPoincare@ and its
            relatives.  Here $S=\operatorname{Sym}(V\otimes W)$,
            $V=W=\mathbb Q^3$, and $R=S/I_2$, where $I_2$ is generated by
            the $2\times2$ minors.  The equivariant series is
            $$P_R(t,s)=\sum_{i,j}[\operatorname{Tor}^R_i(k,k)_j]t^i s^j.$$
            In the output $x_\lambda=[\mathbf S_\lambda(V)]$ and
            $y_\mu=[\mathbf S_\mu(W)]$; multiplication means tensor
            product and addition means direct sum.  The variables $t,s$
            record homological and internal degree.  @TO totalBettiNumbers@,
            @TO (betti, PoincareSeries, ZZ)@ and @TO strandBettiTable@
            expand them, and @TO determinantalRing@ is the ring they
            describe.
        Example
            P = genericDetPoincare(3,3,2)
            totalBettiNumbers(P, 4)
            betti(P, 4)
            strandBettiTable P
        Text
            The first two nonconstant equivariant coefficients are
            $$\begin{aligned}
            \operatorname{Tor}^R_1(k,k)_1&=V\otimes W,\\
            \operatorname{Tor}^R_2(k,k)_2&=
            \operatorname{Sym}^2V\otimes\bigwedge^2W
            \oplus\bigwedge^2V\otimes\operatorname{Sym}^2W
            \oplus\bigwedge^2V\otimes\bigwedge^2W.
            \end{aligned}$$
            Their dimensions are $9$ and $18+18+9=45$.  The list
            $1,9,45,181,684$ gives $\dim\operatorname{Tor}^R_i(k,k)$
            only for $0\le i\le4$; the resolution continues.
            In contrast, @TT "strandBettiTable P"@ gives
            $\dim\operatorname{Tor}^S_i(R,k)_j$: its totals
            $1,9,16,9,1$ describe the finite resolution of $R$ over $S$,
            not that of $k$ over $R$.  In either table, column $i$ and row
            $j-i$ specify the homological and internal degrees.
        Text
            Continue with @TO weightTruncationIdeal@ and the @TO Weight@
            option of @TO hookRing@ for content bounds, @TO fineSegreRing@
            for the generic determinantal models, @TO symDetPoincare@
            and @TO pfaffianPoincare@ for the other matrix families, or
            @TO "Examples from the paper"@ for the computations in the
            article.
    SeeAlso
        HookTabloid
        isHookSemistandard
        hookStraighten
        hookAlgebra
        hookRing
        hookBasis
        hookRelation
        hookVariable
        pieriMatrix
        pieriImage
        pieriPreimage
        straighteningStep
        verifyTriangularity
        genericDetPoincare
        "Examples from the paper"
///

doc ///
    Key
        "Examples from the paper"
    Headline
        computations from Hook algebras and determinantal varieties
    Description
        Text
            This node reproduces, in the order in which they appear, the
            worked examples of the paper
            Hook algebras and determinantal varieties by
            Steven V Sam, Keller VandeBogert, and Jerzy Weyman that can be
            computed with the package.  Section and example numbers refer to
            that paper.  Direct computations of Koszul cycles and their
            products in the determinantal Tor algebras are outside the
            scope of this tutorial.

        Text
            @BOLD "Example 1.1.2: the Poincaré series of cubic generic minors."@
            Let $S=\mathbb Q[x_{ij}:1\le i,j\le5]$ with each entry of
            degree one, and $R=S/I_3$, where $I_3$ is generated by the
            $3\times3$ minors.  This example computes
            $P_R(t)=\sum_i\dim\operatorname{Tor}^R_i(\mathbb Q,\mathbb Q)t^i$.
            @TO dimensionSpecialization@ returns its numerator and
            denominator as an ordered pair.  The theorem's formula is
            $(1+t)^{25}/(1-100t^2-450t^3-828t^4-700t^5+700t^7+828t^8+450t^9+100t^{10}-t^{12})$:
        Example
            L = dimensionSpecialization genericDetPoincare(5,5,3);
            factor L#0
            L#1
        Text
            The expansion $1+25t+400t^2+5250t^3+64728t^4+779530t^5+9361100t^6+\cdots$
            of the introduction, and the Betti table of the residue field:
        Example
            poincareSeriesTruncation(genericDetPoincare(5,5,3), 6)
            betti(genericDetPoincare(5,5,3), 4)

        Text
            @BOLD "Example 1.1.3: the strand-graded Betti table."@
            This is the resolution of $R$ over the polynomial ring $S$,
            not the residue-field resolution in the preceding example:
            its entries are $\beta^S_{i,j}(R)=
            \dim\operatorname{Tor}^S_i(R,\mathbb Q)_j$.
            Rows are indexed by $j-i$, columns by homological degree $i$; the
            first strand (row $j-i=2$) has total dimension $2303$, the number
            of minimal generators of the Koszul homology algebra by
            Theorem 1.1.4.  The keys of the Betti table are the triples
            $(i,\{j\},j)$ of Macaulay2.
        Example
            B = strandBettiTable genericDetPoincare(5,5,3)
            sum apply(select(keys B, k -> k#2 - k#0 == 2), k -> B#k)

        Text
            @BOLD "Section 1.2: the Frobenius union of two hooks."@
            The hook Pieri injection for the types $(1,1)$ and $(0,0)$ on
            three letters is the first map in the exact sequence
            $$0\longrightarrow\bigwedge^2V\otimes\operatorname{Sym}^2V
            \longrightarrow\mathbf S_{(2,1)}(V)\otimes V
            \longrightarrow\mathbf S_{(2,2)}(V)\longrightarrow0,
            \qquad V=\mathbb Q^3.$$
            It has cokernel of dimension
            $6=\dim\mathbf S_{(2,2)}(\mathbf k^3)$, and the projection onto the
            Frobenius union has rank $6$:
        Example
            D = pieriMatrix({1,1},{0,0},3);
            (numRows D, numColumns D, rank D)
            rank frobeniusUnionMatrix({1,1},{0,0},3)
        Text
            Thus the first matrix has size $24\times18$ and rank $18$;
            the second map has rank $6$, accounting for all $24$ target
            dimensions.  A hook type $(a,b)$ specifies the arm and leg
            lengths, hence the partition $(a+1,1^b)$.
        Text
            @BOLD "Example 1.2.2: a finite hook algebra."@
            Take $V=\mathbb Q^3$ and the hook types $(0,0)$, $(1,1)$,
            and $(2,2)$.
            The algebra $A$ is a quotient of a polynomial ring in $17$
            variables, one for each semistandard hook tableau of the three
            types ($3+8+6$), by $136$ quadratic relations, and its Hilbert
            series is $1+17q+17q^2+q^3$:
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            numgens A, numgens ideal A
            hookBasis A
            reduceHilbert hilbertSeries A
        Text
            The relations form a quadratic Gröbner basis, with the
            semistandard hook tabloids as standard monomials, and the unique
            rank three standard monomial spans the degree three component:
        Example
            hasQuadraticGroebnerBasis A
            hilbertFunction(3, A)
        Text
            The straightening relation displayed in the paper, with the
            $(2,2)$-type hook in blue and the $(0,0)$-type hook in green,
            $$\begin{array}{l}\fcolorbox{black}{#b8c8f0}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{#a8e8a8}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\end{array} = -\begin{array}{l}\fcolorbox{black}{#b8c8f0}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{#b8c8f0}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{#a8e8a8}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\end{array} - \begin{array}{l}\fcolorbox{black}{#b8c8f0}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{#b8c8f0}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{#a8e8a8}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\end{array},$$
            is the product of the $(2,2)$-type generator $z_4$ with the
            $(0,0)$-type generator $z_{14}$: in the quotient
            ring this nonstandard product is rewritten as minus the sum of the
            two standard products $z_2z_{15}$ and $z_1z_{16}$, which are
            themselves already in normal form.  @TO hookRelation@ returns
            the stored relation of the pair as an element of the hook ring.
        Example
            (tabloid A_4, tabloid A_14)
            use A;
            z4*z14
            z2*z15
            z4*z14 + z2*z15 + z1*z16 == 0
            hookRelation(A, 4, 14)
        Text
            The same relation, read off by the straightening algorithm on
            the tabloid rather than in the algebra, and the normal form of
            the tabloid as an element of the algebra:
        Example
            T = tabloid {{1,2,3},{2,1},{3}}
            hookStraighten T
            hookStraighten(T, A)

        Text
            @BOLD "Section 2.1: Frobenius coordinates."@
            If $d$ is the number of diagonal boxes of $\lambda$, its
            Frobenius coordinates are $(a_1,\ldots,a_d\mid b_1,\ldots,b_d)$,
            where $a_i=\lambda_i-i$ and $b_i=\lambda'_i-i$ count the arm
            and leg boxes of its $i$th diagonal hook.  The partition
            $(4,3,2)$ has rank $2$ and Frobenius coordinates
            $(3,1\mid2,1)$:
        Example
            lam = {4,3,2}; d = frobeniusRank lam;
            colLen = i -> #select(lam, x -> x > i);
            (apply(d, i -> lam#i-i-1), apply(d, i -> colLen(i)-i-1))
        Text
            The partitions $(2)$ and $(2,1)$ are not mergeable (their arm
            lengths coincide), so the hook algebra generated by their two
            types has no elements of rank two:
        Example
            reduceHilbert hilbertSeries hookAlgebra(3, {{1,0},{1,1}})

        Text
            @BOLD "Section 3.1: semistandard hook tabloids."@
            Semistandard means rows weakly increase and columns strictly
            increase.  This is a condition on the whole diagram, not just
            on each diagonal hook separately.
            The tabloids $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}$ and $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}$ are semistandard, and $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$ is not:
        Example
            (isHookSemistandard {{1,1,2},{2,3},{4}}, isHookSemistandard {{1,3},{2,4}}, isHookSemistandard {{1,3},{2,2}})
            hookStraighten {{1,3},{2,2}}

        Text
            @BOLD "Section 3.2: running vectors and the running partial order."@
            For $T=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$ and $U=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$ of type $((2,1),(0,0))$, the running
            vectors in the coordinates $q=1,2,3$ are those of the paper's table,
            indexed by the running indices $(t,s)$ of the shape.  Each
            coordinate counts entries at most $q$ in the corresponding
            running region, defined on the @TO runningVectors@ page.
            Their last disagreement is at the index $(1,2)$ where
            $V_{1,2}(U)\le V_{1,2}(T)$, and hence $T\preceq_{\mathrm{run}}U$:
        Example
            runningIndices {3,2}
            runningVectors {{1,1,2},{2,3}}
            runningVectors {{1,1,3},{2,2}}
            runningCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}})
            runningCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}}, Verbose => true)
        Text
            Every step of the straightening algorithm decreases this order;
            @TO verifyTriangularity@ checks it on the hookwise semistandard
            fillings of the specified shape and finite alphabet:
        Example
            (verifyTriangularity({3,2}, 3))#"passed"
        Text
            The paper's term order refines this partial order; the
            verification of the compiled order against the running vectors
            on the finite hook algebra of Example 1.2.2 reports no
            mismatches:
        Example
            verifyPaperOrder A

        Text
            @BOLD "Remark 3.2.2: the naive Garnir lift fails."@
            For the hook types $(2,1)>(0,0)$ with $\dim V=3$, the tabloid
            $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$ is a counterexample: the true projection sends
            $z_T\otimes e_2$ to $-\tfrac12\,v_{T'}$ with $T=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$ and
            $T'=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$ (in the package's normalization, $-2/9$ divided by the
            universal scalar $4/9$), whereas the types $(1,1)>(0,0)$ are
            consistent.  @TO verifyGarnirLift@ examines every tabloid of the
            nest shape and records a counterexample:
        Example
            G = verifyGarnirLift({2,1},{0,0},3);
            G#"passed"
            G#"universalScalar"
            m = G#"minimalCounterexample";
            (m#"tabloid", m#"T1", m#"T2")
            m#"actualMu"
            m#"straightening"
            (verifyGarnirLift({1,1},{0,0},3))#"passed"
        Text
            @TO verifyGarnirLift@ uses the letters $0,\ldots,n-1$: the
            tabloid in @TT "m#\"tabloid\""@ is the filling displayed
            above after subtracting $1$ from every entry.  The empty
            @TT "straightening"@ table records that this tabloid is zero in
            the Schur functor.

        Text
            @BOLD "Section 4: a hook Schur functor basis element."@
            In $T_I^\alpha$, the subscript $I$ lists the full column,
            including the corner, and the exponent vector $\alpha$
            specifies the multiplicities in the row tail.  In the later
            examples a word in the superscript instead lists the row-tail
            entries themselves.
            $T^{(0,1,0,1,0)}_{\{1,3,5\}}$ is the semistandard hook tableau
            $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\end{array}$ in $\mathbf S_{(3,1,1)}(\mathbf k^5)$, and the single-hook
            straightening $T_{132}^{\,3}=-T_{123}^{\,3}$, $T_{131}^{\,3}=0$.
            A hook is entered by its row tail and its full column, as in
            @TO (tabloid, List, List)@:
        Example
            member(tabloid({2,4},{1,3,5}), hookBasis({2,2},5))
            hookStraighten({3},{1,3,2})
            hookStraighten({3},{1,3,1})

        Text
            @BOLD "Example 5.2.2: a row elimination matrix."@
            With $V=\mathbf k^4$, the row-removal map is
            $$P_2:\mathbf S_{(4,1,1)}(V)
            \longrightarrow\operatorname{Sym}^2V\otimes\mathbf S_{(2,1,1)}(V).$$
            Here $a=4$ is the first-row length, $b=2$ the leg length, and
            $m=2$ the number of removed row entries.  The displayed
            $3\times3$ submatrix has columns indexed by @TT "srcs"@ and
            rows by @TT "tgts"@; it is not the entire matrix of $P_2$.
            It has
            determinant $9/8$ and eigenvalues $3/2,3/2,1/2$.  The row
            Pieri map @TO pieriRowRemove@ returns a hash table keyed by the
            pairs (removed letters, remaining hook):
        Example
            srcs = {tabloid({1,2,2},{1,3,4}), tabloid({1,2,3},{1,2,4}), tabloid({1,2,4},{1,2,3})};
            tgts = {{{2,2}, tabloid({1},{1,3,4})}, {{2,3}, tabloid({1},{1,2,4})}, {{2,4}, tabloid({1},{1,2,3})}};
            M = matrix for tg in tgts list for T in srcs list (h := pieriRowRemove(T,2); if h#?tg then h#tg else 0_QQ)
            det M
            R = QQ[x]; factor det(x*id_(R^3) - sub(M,R))

        Text
            @BOLD "Example 5.3.2: a column elimination matrix."@
            With $V=\mathbf k^5$, the column-addition map is
            $$Q_2:\mathbf S_{(3,1)}(V)\otimes\bigwedge^2V
            \longrightarrow\mathbf S_{(3,1,1,1)}(V).$$
            Here $a=3$ is the first-row length, $b=1$ the original leg
            length, and $m=2$ the number of added column entries.
            The selected $3\times3$ submatrix, with columns @TT "srcs"@
            and rows @TT "tgts"@, has determinant $1/9$
            and eigenvalues $1/3,1/3,1$:
        Example
            srcs = {{tabloid({5,5},{1,2}),{3,4}}, {tabloid({4,5},{1,2}),{3,5}}, {tabloid({3,5},{1,2}),{4,5}}};
            tgts = {tabloid({5,5},{1,2,3,4}), tabloid({4,5},{1,2,3,5}), tabloid({3,5},{1,2,4,5})};
            M = matrix for tg in tgts list for T in srcs list (h := pieriColumnAdd(T#0,T#1); if h#?tg then h#tg else 0_QQ)
            det M
            factor det(x*id_(R^3) - sub(M,R))

        Text
            @BOLD "Example 6.1.5: straightening a column obstruction."@
            For $T=(T^{123}_{123},T^{11}_{14})$ the row swap set has the
            elimination matrix displayed in the paper, with determinant $5/2$
            and solution $c=(4,-1,-1)/5$; the full normal form of the
            assembled tabloid then follows from the straightening algorithm:
        Example
            srcs = {tabloid({1,2,3},{1,4}), tabloid({1,3,4},{1,2}), tabloid({1,2,4},{1,3})};
            tgts = {{{1,2,3}, tabloid({},{1,4})}, {{1,3,4}, tabloid({},{1,2})}, {{1,2,4}, tabloid({},{1,3})}};
            M = matrix for tg in tgts list for T in srcs list (h := pieriRowRemove(T,3); if h#?tg then h#tg else 0_QQ)
            det M
            transpose solve(M, matrix{{1_QQ},{0},{0}})
        Text
            The returned row $(4/5,-1/5,-1/5)$ is the solution of
            $Mc=(1,0,0)^{\mathsf T}$: the corresponding linear combination
            of the three source hooks has coefficient one on the first
            selected target and zero on the other two.  This is the
            elimination used to rewrite the offending pair of hooks.
            The same step, performed by the algorithm on the tabloid
            (@TO straighteningStep@); the paper's example with both a row and
            a column obstruction, $T=(T^{123}_1,T^{111}_{2,4})$, is resolved
            by the algorithm at its row obstruction, while the displayed
            column relation is obtained with @TT "Obstruction => \"column\""@:
        Example
            straighteningStep {{1,2,3},{2,1},{3}}
            straighteningStep({{1,1,2,3},{2,1,1,1},{3,4}}, Obstruction => "column")
        Text
            @BOLD "Example 6.2.6: straightening a row obstruction."@
            For $T=(T^{123}_{134},T^{34}_{23})$ the two column elimination
            systems have the matrices $M_\alpha=(1/2)$ and
            $M_\beta=\begin{pmatrix}1/2&-1/2\\-1/4&3/4\end{pmatrix}$ with
            solutions $2$ and $(3,1)$.  These solve the two systems with
            right-hand side the first basis vector.  The last two commands
            compare one elimination step with the full normal form:
        Example
            pieriColumnAdd(tabloid({1,2,3},{1}), {3,4})
            srcs = {{tabloid({3,4},{2}),{3}}, {tabloid({3,3},{2}),{4}}};
            tgts = {tabloid({3,4},{2,3}), tabloid({3,3},{2,4})};
            Mb = matrix for tg in tgts list for T in srcs list (h := pieriColumnAdd(T#0,T#1); if h#?tg then h#tg else 0_QQ)
            transpose solve(Mb, matrix{{1_QQ},{0}})
            straighteningStep {{1,1,2,3},{3,2,3,4},{4,3}}
            hookStraighten {{1,1,2,3},{3,2,3,4},{4,3}}
        Text
            Writing brackets for hook-algebra classes, the one-step output is
            $$\begin{aligned}
            \left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
            ={}&\tfrac23\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}\right]
            +\tfrac13\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\\
            &-\tfrac13\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right].
            \end{aligned}$$
            The last two tableaux still need straightening.  The full
            output is instead
            $$\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
            =\tfrac12\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}\right]
            +\tfrac14\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right].$$
        Text
            @BOLD "Example 6.2.8: hook versus Schur versus Weyl straightening."@
            The tableau $T=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\end{array}$ is straightened in three models.  In the
            hook model (this package) the coefficients are rational:
            $$T=-\tfrac12\,\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}-\tfrac12\,\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}+\tfrac12\,\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}+\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}.$$
        Example
            hookStraighten {{1,2,6},{4,3,5}}
        Text
            In the Schur and Weyl models the coefficients are integers.  These
            are computed by @TO "SchurFunctors :: straighten"@ and
            @TO "SchurFunctors :: weylStraighten"@ of the
            @TO SchurFunctors@ package, whose @TO Filling@ is stored
            column-wise (so $T$ is entered as @TT "{{1,4},{2,3},{6,5}}"@) and
            whose @TO WeylFilling@ is stored row-wise (so $T$ is entered as
            @TT "{{1,2,6},{4,3,5}}"@).  The Schur model gives
            $T=-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}+\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}$ and the Weyl model gives $T=-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}$:
        Example
            needsPackage "SchurFunctors";
            F = new Filling from {{1,4},{2,3},{6,5}}
            straighten F
            W = new WeylFilling from {{1,2,6},{4,3,5}}
            weylStraighten W

        Text
            @BOLD "Section 7.1: maximal and submaximal minors."@
            For maximal minors the denominator is Golod's
            $1-\sum_{i\ge1}\beta_i t^{i+1}$, where
            $\beta_i=\dim\operatorname{Tor}^S_i(R,k)$ for the determinantal
            quotient $R$ of the polynomial ring $S$.  For the submaximal minors of a
            $4\times4$ matrix one obtains the Gulliksen--Negard denominator
            $1-16t^2-30t^3-16t^4+t^6$:
        Example
            (dimensionSpecialization genericDetPoincare(2,4,2))#1
            (dimensionSpecialization genericDetPoincare(3,5,3))#1
            (dimensionSpecialization genericDetPoincare(4,4,3))#1
            (isGolodPoincare genericDetPoincare(2,4,2), isGolodPoincare genericDetPoincare(4,4,3))
        Text
            For the symmetric cover, the ring is
            $M_r=\operatorname{Sym}(\mathbf k^{r-1}\otimes V)^{\mathrm{SO}_{r-1}}$,
            with the group acting on $\mathbf k^{r-1}$.  Its grading comes
            from the polynomial degree before taking invariants.  The
            next call uses $\dim V=3$ and $r=3$, so it gives the
            $3\times3$ Segre ring with generators in degree two; see
            @TO symDetCoverPoincare@ for the grading conventions.
            The output is again the numerical numerator-denominator pair:
        Example
            dimensionSpecialization symDetCoverPoincare(3,3)

        Text
            @BOLD "Example 7.1.7: a Pfaffian ring defined by cubics."@
            The $6\times6$ Pfaffians of a generic $8\times8$ alternating
            matrix have denominator
            $1-28t^2-63t^3-36t^4+36t^5+63t^6+28t^7-t^9$ (the example passes
            to a linear section, which divides the numerator by $1+t$):
        Example
            (dimensionSpecialization pfaffianPoincare(8,3))#1

        Text
            @BOLD "Section 7.4.3: the fine Segre model of the generic case."@
            For $r=2$ on $3\times3$ matrices, each base hook $(a\mid b)$ with
            $0\le a,b\le1$ receives the left type $(a,b+1)$ and right type
            $(b,a+1)$ (@TO hookTypes@ lists the triples base, left, right).
            The fine Segre product is the quotient ring @TO fineSegreAlgebra@
            returns; the quadratic monomial classification of
            @TO relationKind@ has exactly one standard pair, the two
            variables of the base types $(1\mid1)$ and $(0\mid0)$ whose
            tableaux stack to the semistandard filling of $(2,2,2)$ on each
            side.  Since that filling is the only semistandard filling of
            $(2,2,2)$ with entries at most $3$, the left and the right
            tableau of each of these two variables coincide.  Every other
            nonzero product is a multiple of their product:
        Example
            B = fineSegreAlgebra(3,3,2)
            hookTypes B
            N = numgens B
            tally flatten for i to N-1 list for j from i to N-1 list relationKind(B, i, j)
            relationKind(B, 0, 33)
            tabloid B_0
            tabloid B_33
            B_0 * B_33
            B_1 * B_32
        Text
            In the displayed generators the last two outputs say
            $$w_1w_{32}=-w_0w_{33},\qquad w_0w_{33}\ne0.$$
            The generator pairs are tensors of the two tableaux returned
            by @TO tabloid@; the fine degree records their base hook
            type.  This is the fine Segre algebra model, not a direct
            computation of multiplication in the determinantal Tor algebra.
    SeeAlso
        HookPieri
        dimensionSpecialization
        strandBettiTable
        hookStraighten
        straighteningStep
        verifyTriangularity
        runningVectors
        runningCompare
        verifyGarnirLift
        hookAlgebra
        fineSegreRing
///

doc ///
    Key
        VerificationReport
        (net, VerificationReport)
        (toString, VerificationReport)
    Headline
        results of a finite verification
    Description
        Text
            The verifications that examine a whole set of tabloids or
            relations, @TO verifyTriangularity@,
            @TO verifyStraighteningAgainstCokernel@, @TO verifyGarnirLift@
            and @TO verifyPaperOrder@, return a @TT "VerificationReport"@:
            a hash table with string keys that records the verdict together
            with the counts of the check and the cases it found.  Every report
            has the keys @TT "kind"@ (a string naming the verification),
            @TT "passed"@ (the verdict, @TO true@ or @TO false@) and
            @TT "keyOrder"@ (the list of its rows in display order); the
            other keys are the data of that kind of verification, and the
            documentation of each verification lists them.  The verdict is
            read as @TT "R#\"passed\""@.
        Text
            A report prints as a two column table of its rows in the stored
            order, with the @TT "passed"@ row and, when present, the
            @TT "conclusion"@ row (a sentence stating what the verdict
            means) last.  A row whose value is a long list of tabloids or of
            other compound data prints as a count, and the value itself is
            read from the report by its key.  @TO toString@ gives the short
            form naming the kind and the verdict, and @TO peek@ shows every
            key with its value.
        Example
            R = verifyTriangularity({3,2}, 3)
            R#"passed"
            toString R
        Text
            Here @TT "true"@ means that all tested rewrites on the letters
            $1,2,3$ and shape $(3,2)$ decrease the running order.  The
            report distinguishes the $45$ hookwise semistandard inputs
            from the $15$ already semistandard whole tableaux.  It is a
            finite verification with the displayed parameters; the report
            itself is not a Boolean, and it does not assert the theorem
            for every alphabet or shape.
    SeeAlso
        verifyTriangularity
        verifyStraighteningAgainstCokernel
        verifyGarnirLift
        verifyPaperOrder
///

doc ///
    Key
        HookTabloid
        (expression, HookTabloid)
        (net, HookTabloid)
        (toString, HookTabloid)
        (toExternalString, HookTabloid)
        (tex, HookTabloid)
        (symbol ==, HookTabloid, HookTabloid)
        (symbol ?, HookTabloid, HookTabloid)
        (NewFromMethod, Filling, HookTabloid)
    Headline
        a filling represented by its diagonal hooks
    Description
        Text
            A hook tabloid is a filling of the Young diagram of a partition
            $\lambda$ read through the diagonal hooks of $\lambda$: the
            $k$th diagonal hook consists of the box $(k,k)$, the boxes to
            its right in row $k$ (the arm) and the boxes below it in column
            $k$ (the leg), and the tabloid is the product of the hook
            tabloids on these hooks (Section 3 of the paper).  More
            precisely, the filling labels a tensor in the hook Schur
            functors $\bigotimes_k\mathbf S_{(a_k+1,1^{b_k})}(V)$,
            where $a_k=\lambda_k-k$ and $b_k=\lambda'_k-k$; multiplication
            gives its class in the hook algebra.  Entries $1,\ldots,n$
            refer to a fixed basis of $V=k^n$.  A stored filling need not
            be semistandard or represent a nonzero class.  Hook tabloids
            are the elements straightened in Section 6 and the basis
            elements on which the Pieri maps of Section 4 are computed.
        Text
            A @TT "HookTabloid"@ stores its rows, top row first, and
            @TO toList@ recovers them.  The constructor is @TO tabloid@,
            which accepts the rows, a @TO Filling@ (the column-stored type
            of @TO SchurFunctors@), one hook as the pair of its row tail
            and its full column, or a nested pair of hook tabloids.  Every
            function of the package that takes a tabloid accepts a
            @TT "HookTabloid"@, a list of rows or a @TO Filling@.
        Example
            T = tabloid {{1,2,3},{2,1},{3}}
            toList T
            hookDecomposition T
        Text
            The console drawing is the boxed tableau.  @TO toString@ and
            @TO toExternalString@ give the form
            @TT "tabloid {rows}"@, @TO expression@ is that function
            application.  Evaluating this expression reconstructs the
            tabloid.  The output of @TO tex@ is the KaTeX source of @TO youngTex@,
            which renders in the HTML documentation as
            $$\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$$
        Example
            toString T
            value toExternalString T == T
            expression T
            tex T
        Text
            Two hook tabloids are equal under @TO symbol ==@ when their rows
            agree, and @TO symbol ?@ is a total order: the shapes are
            compared first, as lists, and tabloids of the same shape are
            compared by their rows, so @TO sort@ works on any list of
            tabloids.  A tabloid hashes by its rows, so an equal copy finds
            the same entry of a hash table; the normal forms and Pieri
            images of the package are hash tables from tabloids to
            coefficients and are read this way.
        Example
            T == tabloid {{1,2,3},{2,1},{3}}
            tabloid {{1,2},{3}} ? tabloid {{1,3},{2}}
            sort {tabloid {{2,1}}, tabloid {{1,2},{3}}, tabloid {{1,1}}}
            H = hashTable {T => 1};
            H#?(tabloid {{1,2,3},{2,1},{3}})
        Text
            @TT "new Filling from T"@ converts a tabloid to a @TO Filling@
            of @TO SchurFunctors@, which stores the columns; @TO tabloid@
            of a @TO Filling@ is the inverse conversion.
        Example
            needsPackage "SchurFunctors";
            F = new Filling from T
            toList F
            tabloid F == T
    SeeAlso
        tabloid
        hookDecomposition
        youngTex
///

doc ///
    Key
        tabloid
        (tabloid, List)
        (tabloid, HookTabloid)
        (tabloid, Filling)
        (tabloid, List, List)
        (tabloid, HookTabloid, HookTabloid)
    Headline
        construct a hook tabloid
    Usage
        tabloid rows
        tabloid T
        tabloid F
        tabloid(rowTail, colFull)
        tabloid(outer, inner)
    Inputs
        rows:List
            the rows of the tabloid, top row first; the rows are nonempty
            and their lengths form a partition
        T:HookTabloid
            returned unchanged
        F:Filling
            a filling of @TO SchurFunctors@, which stores the columns
        rowTail:List
            the entries of the arm of one hook, left to right
        colFull:List
            the entries of the column of that hook, the corner first
        outer:HookTabloid
            a hook shaped tabloid, one row and one column
        inner:HookTabloid
            a hook shaped tabloid whose arm and leg are both shorter than
            those of @TT "outer"@
    Outputs
        :HookTabloid
    Description
        Text
            The rows form gives the tabloid directly, and a @TO Filling@ is
            converted from column storage to row storage, preserving the
            diagram and its entries (not transposing the represented
            tableau).
        Example
            tabloid {{1,2,3},{2,1},{3}}
            needsPackage "SchurFunctors";
            tabloid new Filling from {{1,2},{3}}
        Text
            A single hook of type $(a,b)$, with an arm of length $a$ and a
            leg of length $b$, is described by its row tail (the arm) and
            its full column (the corner followed by the leg).  This pair is
            used to index the generators of a hook ring; the pair form of
            @TO tabloid@ builds the hook-shaped tabloid from it.
        Example
            tabloid({2,3}, {1,2,3})
            tabloid({}, {2})
        Text
            A tabloid of Frobenius rank two is the nesting of an outer hook
            of type $(a,b)$ around an inner hook of type $(c,d)$ with
            $a > c$ and $b > d$: the outer hook fills the first row and the
            first column, and the inner hook the second row and the second
            column, of the shape $(a+1, c+2, 2^{d}, 1^{b-d-1})$.  These
            are the tabloids whose Pieri relations are computed in Section
            4 of the paper; @TO hookDecomposition@ recovers the two hooks.
        Example
            outer = tabloid({2,3}, {1,2,3})
            inner = tabloid({}, {2})
            tabloid(outer, inner)
        Text
            In the last example the two generators have shapes $(3,1,1)$
            and $(1)$ and nest to shape $(3,2,1)$:
            $$\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
            \cdot\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]
            =\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right].$$
            Brackets here denote hook-algebra classes; the constructor
            itself only assembles the filling and performs no straightening.
    Caveat
        Only the shape is validated.  The entries may be any objects
        (@TO verifyGarnirLift@ uses the alphabet $0, \dots, n-1$), and
        neither the rows nor the hooks are required to be semistandard;
        @TO isHookSemistandard@ tests that.
    SeeAlso
        HookTabloid
        hookDecomposition
        youngTex
        isHookSemistandard
///

doc ///
    Key
        hookDecomposition
        (hookDecomposition, HookTabloid)
        (hookDecomposition, List)
        (hookDecomposition, Filling)
    Headline
        the diagonal hooks of a hook tabloid
    Usage
        hookDecomposition T
    Inputs
        T:HookTabloid
            or a list of rows, or a @TO Filling@
    Outputs
        :List
            the diagonal hooks of @TT "T"@, top left first, each a hook
            shaped @TO HookTabloid@
    Description
        Text
            The $k$th diagonal hook of a tabloid of shape $\lambda$ is the
            box $(k,k)$ together with the rest of row $k$ to its right and
            the rest of column $k$ below it.  There are as many diagonal
            hooks as the Frobenius rank of $\lambda$ (see
            @TO frobeniusRank@), the tabloid is the product of the hook
            tabloids on them, and the straightening algorithm rewrites a
            tabloid one pair of consecutive diagonal hooks at a time
            (Section 6 of the paper).
        Example
            T = tabloid {{1,3,3},{2,2},{3}}
            hookDecomposition T
            hookDecomposition {{1,2,3},{4,5,6},{7,8,9}}
        Text
            For a tabloid of Frobenius rank two, @TO tabloid@ applied to the
            outer and the inner hook recovers the tabloid.  The empty
            tabloid has no hooks.
        Example
            D = hookDecomposition T;
            tabloid(D#0, D#1) == T
            hookDecomposition tabloid {}
    SeeAlso
        tabloid
        HookTabloid
        frobeniusRank
///

doc ///
    Key
        youngTex
        (youngTex, List)
        (youngTex, Filling)
        (youngTex, HookTabloid)
        [youngTex, Colors]
        Colors
    Headline
        KaTeX source for a Young tableau, optionally with colored cells
    Usage
        youngTex T
        youngTex(T, Colors => fills)
    Inputs
        T:BasicList
            a tabloid as a list of rows, a @TO Filling@, or a
            @TO HookTabloid@
        Colors => List
            a list of rows of color strings (any CSS color), one per cell,
            giving the background of each box; the default is white
    Outputs
        :String
            KaTeX source drawing the tableau as framed boxes; it renders in
            the Macaulay2 HTML documentation and in any KaTeX enabled page,
            and it is what @TO tex@ returns for a @TO HookTabloid@
    Description
        Text
            The paper colors the diagonal hooks of a tabloid; passing the
            fills reproduces that.  For instance, the tabloid of
            Example 1.2.2 with its hook of type $(2,2)$ in blue and its
            hook of type $(0,0)$ in green is
            $$\begin{array}{l}\fcolorbox{black}{#b8c8f0}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{#b8c8f0}{\phantom{|}2\phantom{|}}\!
            \fcolorbox{black}{#a8e8a8}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{#b8c8f0}{\phantom{|}3\phantom{|}}\end{array}$$
        Example
            youngTex {{1,3},{2,2}}
            youngTex tabloid({3},{1,2})
            youngTex({{1,2,3},{2,1},{3}}, Colors => {{"#b8c8f0","#b8c8f0","#b8c8f0"},{"#b8c8f0","#a8e8a8"},{"#b8c8f0"}})
    SeeAlso
        tabloid
        HookTabloid
///

doc ///
    Key
        frobeniusUnion
        (frobeniusUnion, HookTabloid, HookTabloid, ZZ)
        (frobeniusUnion, List, List, ZZ)
        (frobeniusUnion, Filling, Filling, ZZ)
    Headline
        the image under the projection onto the Schur functor of the Frobenius union
    Usage
        frobeniusUnion(T1, T2, n)
    Inputs
        T1:HookTabloid
            a hook shaped tabloid of shape $(a, 1^b)$ with entries in
            $\{0, \dots, n-1\}$, also accepted as a list of rows or as a
            @TO Filling@
        T2:HookTabloid
            a hook shaped tabloid of shape $(c, 1^d)$ with $a > c$ and
            $b > d$
        n:ZZ
            the dimension of $V$
    Outputs
        :HashTable
            the image of $z_{T_1} \otimes z_{T_2}$ in the semistandard
            basis of the nest shape: @TO HookTabloid@ keys, rational
            coefficients; the empty table is the zero image
    Description
        Text
            This computes the surjection
            $\mu : \mathbf S_{(a,1^b)}(V) \otimes \mathbf S_{(c,1^d)}(V) \to \mathbf S_{(a,1^b) \sqcup (c,1^d)}(V)$
            onto the Schur functor of the Frobenius union
            $(a, c+1, 2^d, 1^{b-d-1})$ of the two hooks, whose kernel is
            the space of quadratic relations of the two hook generators
            (Section 4 of the paper).  It is realized as the composite of a
            column Pieri map, a dual Pieri map and a dual column Pieri map
            of the package @TO PieriMaps@ and inherits their
            normalization: another equivariant identification of the
            target rescales the map by a nonzero scalar.  The computation
            is over $\mathbb Q$, and the entries are the letters
            $0, \dots, n-1$ of that package, whereas the Pieri maps of
            @TO pieriMatrix@ use the letters $1, \dots, n$.  The element
            $z_T$ of a hook tabloid $T$ with a repeated entry in its full
            column is zero, so a pair containing such a tabloid has the
            empty table as image: in the second and third pairs below the
            outer tabloid has the column $(0,0)$.  The last pair has two
            terms.
        Example
            frobeniusUnion(tabloid {{0,1},{1}}, tabloid {{0}}, 2)
            frobeniusUnion(tabloid {{0,1},{0}}, tabloid {{1}}, 2)
            frobeniusUnion(tabloid {{0,1},{0}}, tabloid {{0}}, 2)
            frobeniusUnion(tabloid {{0,0,1},{1}}, tabloid {{2}}, 3)
        Text
            With the source tableaux denoting hook basis elements and
            the target tableaux denoting Schur basis elements, the first
            output means
            $$\mu\!\left(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]
            \otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\end{array}\right]\right)
            =\tfrac18\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right].$$
            The last output is the two-term identity
            $$\mu\!\left(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]
            \otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\right)
            =\tfrac49\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]
            -\tfrac29\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right].$$
            An entry outside $\{0, \dots, n-1\}$ gives the zero image.
            The matrix of the map on the whole tensor product is
            @TO frobeniusUnionMatrix@.
    SeeAlso
        frobeniusUnionMatrix
        verifyGarnirLift
        pieriMatrix
        tabloid
///

doc ///
    Key
        frobeniusUnionMatrix
        (frobeniusUnionMatrix, List, List, ZZ)
    Headline
        the matrix of the projection onto the Schur functor of the Frobenius union
    Usage
        N = frobeniusUnionMatrix(outerType, innerType, n)
    Inputs
        outerType:List
            the type @TT "{arm, leg}"@ $= (a-1, b)$ of the outer hook
            $(a, 1^b)$
        innerType:List
            the type @TT "{arm, leg}"@ $= (c-1, d)$ of the inner hook
            $(c, 1^d)$; both entries are smaller than those of
            @TT "outerType"@
        n:ZZ
            the dimension of $V$
    Outputs
        N:Matrix
            the matrix over $\mathbb Q$ of
            $\mathbf S_{(a,1^b)}(\mathbf k^n) \otimes \mathbf S_{(c,1^d)}(\mathbf k^n) \to \mathbf S_{(a,1^b) \sqcup (c,1^d)}(\mathbf k^n)$
            in the semistandard bases of @TO PieriMaps@; the rows are
            labeled by the semistandard tabloids of the nest shape and the
            columns by pairs (outer tabloid, inner tabloid), with the outer
            index varying slowest; these bases are available through
            @TO pieriTargetBasis@ and @TO pieriSourceBasis@
    Description
        Text
            Each column is computed by @TO frobeniusUnion@.  The matrix
            is surjective, its row count is the dimension of the target
            Schur functor, and its kernel is the image of the hook Pieri
            injection @TO pieriMatrix@ of the same two types: for the
            types $(1,1)$ and $(0,0)$ on three letters the injection is
            $24 \times 18$ of rank $18$, this map is $6 \times 24$ of rank
            $6$, and $18 + 6 = 24$.  These are the maps in
            $$0\longrightarrow\bigwedge^2V\otimes\operatorname{Sym}^2V
            \xrightarrow{\varphi}\mathbf S_{(2,1)}(V)\otimes V
            \xrightarrow{\mu}\mathbf S_{(2,2)}(V)\longrightarrow0.$$
        Example
            frobeniusUnionMatrix({1,1},{0,0},2)
            N = frobeniusUnionMatrix({1,1},{0,0},3);
            (numRows N, numColumns N, rank N)
            (pieriSourceBasis N)#0
            (pieriTargetBasis N)#0
            pieriImage(N, 0)
        Text
            The source pairs of @TT "N"@ and the target pairs of the Pieri
            matrix are the same semistandard pairs, written with the
            letters $0, \dots, n-1$ here and $1, \dots, n$ there and in
            different orders.  After matching them the product of the two
            matrices is zero.
        Example
            M = pieriMatrix({1,1},{0,0},3);
            shift = T -> tabloid apply(toList T, r -> apply(r, x -> x + 1));
            pos = hashTable apply(#pieriTargetBasis M, i -> (pieriTargetBasis M)#i => i);
            perm = apply(pieriSourceBasis N, P -> pos#(apply(P, shift)));
            N_perm * M == 0
        Text
            The output @TT "true"@ is the matrix identity
            $\mu\circ\varphi=0$ after identifying the middle bases;
            together with the ranks $18$ and $6$ it verifies exactness in
            the middle in this example.  The permutation and shift of
            letter labels are essential: the raw matrices use different
            basis orderings and alphabets.
    SeeAlso
        frobeniusUnion
        pieriMatrix
        pieriImage
        pieriSourceBasis
        pieriTargetBasis
///

doc ///
    Key
        verifyGarnirLift
        (verifyGarnirLift, List, List, ZZ)
    Headline
        test the naive Garnir lift against the projection onto the Frobenius union
    Usage
        R = verifyGarnirLift(outerType, innerType, n)
    Inputs
        outerType:List
            the type @TT "{arm, leg}"@ $= (a-1, b)$ of the outer hook
            $(a, 1^b)$
        innerType:List
            the type @TT "{arm, leg}"@ $= (c-1, d)$ of the inner hook
            $(c, 1^d)$; both entries are smaller than those of
            @TT "outerType"@
        n:ZZ
            the size of the alphabet; the tabloids have entries in
            $\{0, \dots, n-1\}$
    Outputs
        R:VerificationReport
            of kind @TT "\"GarnirLift\""@, with the rows
            @TT "parameters"@, @TT "status"@, @TT "universalScalar"@,
            @TT "tabloidsTested"@, @TT "counterexamples"@,
            @TT "minimalCounterexample"@, @TT "counterexampleData"@,
            @TT "message"@, @TT "passed"@ and @TT "conclusion"@
    Description
        Text
            A tabloid $T$ of the nest shape $(a, c+1, 2^d, 1^{b-d-1})$
            decomposes into its two diagonal hooks, the outer hook $T_1$ of
            shape $(a, 1^b)$ and the inner hook $T_2$ of shape $(c, 1^d)$
            (see @TO hookDecomposition@); the naive Garnir lift sends $T$
            to $z_{T_1} \otimes z_{T_2}$.  This verification enumerates
            every tabloid $T$ of the nest shape with entries in
            $\{0, \dots, n-1\}$ and tests whether the projection
            @TO frobeniusUnion@ of its lift equals $c$ times the
            straightening of $T$ in the semistandard basis of the target,
            for one scalar $c$ read off a reference semistandard tableau.
            Explicitly, if $g(T)=z_{T_1}\otimes z_{T_2}$ is the proposed
            lift and $\operatorname{st}_{\mathrm{Schur}}(T)$ is ordinary
            Schur straightening, the test is
            $$\mu(g(T))\stackrel{?}{=}c\,\operatorname{st}_{\mathrm{Schur}}(T)
            \quad\text{for every enumerated }T.$$
            This is not a test of @TO hookStraighten@: the Schur and hook
            models can assign different classes to the same filling.
            The lift is consistent when the arm step $a - c$ is $1$ and
            fails in general when it is larger.
        Text
            The report has @TT "status"@ @TT "\"consistent\""@ or
            @TT "\"counterexamples\""@ after a complete test, the verdict
            at @TT "passed"@, the scalar at @TT "universalScalar"@ and the
            number of tabloids at @TT "tabloidsTested"@.
        Example
            R = verifyGarnirLift({1,1},{0,0},3)
            R#"passed"
            R#"universalScalar"
        Text
            When the lift fails, @TT "counterexamples"@ lists the tabloids
            on which it does, in enumeration order, and
            @TT "counterexampleData"@ the corresponding records, each with
            the keys @TT "tabloid"@, @TT "T1"@, @TT "T2"@ (the two hooks),
            @TT "straightening"@ (the straightening of the tabloid),
            @TT "actualMu"@ (the projection of the lift) and
            @TT "expected"@ ($c$ times the straightening), all tableaux as
            @TO HookTabloid@s and all expansions as hash tables with
            rational values.  The row @TT "minimalCounterexample"@ holds
            the record of the simplest counterexample whose only failure
            of column strictness is a repeated entry in the second column
            with weakly increasing rows, chosen by the number of distinct
            entries, the largest entry and the sum of the entries, or
            @TO null@ when there is none.
        Example
            R2 = verifyGarnirLift({2,1},{0,0},3)
            R2#"passed"
            #R2#"counterexamples"
            G = R2#"minimalCounterexample";
            G#"tabloid"
            G#"straightening"
            G#"actualMu"
        Text
            Here the filling $T=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]$
            gives a concrete failure:
            $$\operatorname{st}_{\mathrm{Schur}}(T)=0,
            \qquad\mu(g(T))=-\tfrac29
            \left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}0\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]
            \fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\ne0.$$
            Thus no nonzero rescaling can make this naive lift compatible
            with Schur straightening.  The report counts $108$ failures
            among the $3^5=243$ fillings of shape $(3,2)$.
            An empty @TT "straightening"@ means that the tabloid is zero
            in the Schur functor, which is why the minimal counterexample
            has a repeated entry in its second column.  When no verdict is
            possible the status explains why: @TT "\"no-ssyt\""@ when the
            alphabet is too small for the nest shape,
            @TT "\"reference-zero\""@ and @TT "\"reference-mismatch\""@
            when the reference semistandard tableau does not determine a
            scalar; @TT "passed"@ is then @TO false@.
        Example
            R0 = verifyGarnirLift({1,1},{0,0},1);
            R0#"status"
            R0#"tabloidsTested"
    Caveat
        The number of tabloids enumerated is $n$ to the power of the
        number of boxes of the nest shape, and each one is projected
        with the Pieri maps of @TO PieriMaps@, so keep $n$ and the
        shapes small.
    SeeAlso
        VerificationReport
        frobeniusUnion
        frobeniusUnionMatrix
        hookDecomposition
///

doc ///
    Key
        PoincareSeries
        (expression, PoincareSeries)
        (net, PoincareSeries)
        (toString, PoincareSeries)
        (texMath, PoincareSeries)
        (describe, PoincareSeries)
        (toExternalString, PoincareSeries)
        (numerator, PoincareSeries)
        (denominator, PoincareSeries)
        (ring, PoincareSeries)
        (coefficientRing, PoincareSeries)
    Headline
        the equivariant Poincare series of the residue field over a determinantal ring
    Description
        Text
            A @TT "PoincareSeries"@ is the equivariant Poincare series
            $P(t,s)=\sum_{i,j}[\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j]\,t^i s^j$
            of the residue field over one of the rings of Corollary 7.1.3
            of the paper: a generic determinantal ring
            (@TO genericDetPoincare@), a symmetric determinantal ring
            (@TO symDetPoincare@) or its cover $M_r$
            (@TO symDetCoverPoincare@), or a Pfaffian ring
            (@TO pfaffianPoincare@).  The series is the quotient of two
            polynomials in $s$ and $t$ whose coefficients are characters,
            elements of a Schur ring of the package @TT "SchurRings"@;
            $t$ records the homological degree and $s$ the internal degree
            (the variable $u$ of the paper).  In the generic case the Schur
            ring is nested, the factor @TT "x"@ indexing $\mathrm{GL}(V)$
            with $\dim V=n$ (the rows) and the factor @TT "y"@ indexing
            $\mathrm{GL}(W)$ with $\dim W=m$ (the columns); the other
            families use a single factor @TT "x"@ for $\mathrm{GL}(V)$.
        Text
            The brackets denote the character of a representation, not its
            dimension.  For partitions $\lambda,\mu$, the printed symbols
            $x_\lambda$ and $y_\mu$ are Schur polynomials: $x_\lambda y_\mu$
            is the character of
            $\mathbf S_\lambda(V)\otimes\mathbf S_\mu(W)$, and the empty
            partition denotes the trivial representation.  Sums with
            nonnegative integer coefficients record direct sums with those
            multiplicities.  The Tor characters are the coefficients after
            expanding the fraction; negative signs in its denominator do
            not mean negative multiplicities.  Replacing characters by
            dimensions gives the numerical Poincare series.
        Text
            The object is a hash table with the keys @TT "numerator"@,
            @TT "denominator"@ and @TT "ring"@, read by the methods
            @TO numerator@, @TO denominator@ and @TO ring@; the string keys
            @TT "\"family\""@ (one of @TT "\"generic\""@,
            @TT "\"symmetric\""@, @TT "\"pfaffian\""@ and
            @TT "\"symmetricCover\""@), @TT "\"parameters\""@ (the
            arguments of the constructor), @TT "\"generatorDegree\""@ (the
            degree of the defining minors or Pfaffians; stored as $2$ for
            the symmetric cover) and
            @TT "\"denominatorTerms\""@.
            @TO coefficientRing@ gives the base ring $\mathbb Q[s,t]$ under
            the Schur ring; @TO ring@ gives the Schur ring containing the
            numerator and denominator.
        Example
            P = genericDetPoincare(2,2,2)
            numerator P
            denominator P
            ring P
            coefficientRing P
            P#"family", P#"parameters", P#"generatorDegree"
        Text
            Here $R=\mathbf k[z_{11},z_{12},z_{21},z_{22}]/(\det z)$.
            Its single quadratic equation transforms as
            $\bigwedge^2V\otimes\bigwedge^2W$, so the denominator is
            $1-s^2t^2x_{(1,1)}y_{(1,1)}$.  Both vector spaces have
            dimension $2$, and this character has dimension $1$.
        Text
            For the generic, symmetric and Pfaffian families, every
            nonconstant term of the denominator is
            $(-1)^d\,\chi\,t^{i+d}s^{i+(r-1)d}$ for a character $\chi$ of
            $\operatorname{Tor}^S_i(R,\mathbf k)_{i+(r-1)d}$, where $i$ is
            the homological degree over the polynomial ring $S$ and
            $d=(j-i)/(r-1)$ is the strand index.  Here $r$ is the degree
            of the defining equations, so for Pfaffians their order is
            $2r$.  These groups describe the resolution of $R$ over $S$,
            not the residue-field groups encoded by the expanded fraction.
            For the symmetric cover with $r\ge3$, $S=\operatorname{Sym}(S^2V)$
            has generators of degree $2$, and the corresponding internal
            degree is $j=2i+(r-1)d$.
            The list @TT "P#\"denominatorTerms\""@ has one record per such
            term, a hash table with the keys @TT "\"lambda\""@ (the partition
            of the sum in Corollary 7.1.3), @TT "\"mu\""@ (the index of the
            Schur functor, a pair of partitions in the generic case),
            @TT "\"i\""@, @TT "\"d\""@, @TT "\"j\""@ (the internal
            degree), @TT "\"sign\""@, @TT "\"character\""@ and
            @TT "\"term\""@ (the term itself).  These records are the source
            of @TO strandBettiTable@, @TO equivariantStrandBettiTable@ and
            @TO isGolodPoincare@.
        Example
            rec = first P#"denominatorTerms";
            rec#"lambda", rec#"mu", rec#"i", rec#"d", rec#"j", rec#"sign"
            rec#"character"
            rec#"term"
        Text
            The record has $(i,d,j)=(1,1,2)$: one quadratic equation in the
            first step of the $S$-free resolution.  It contributes
            $-s^2t^2x_{(1,1)}y_{(1,1)}$ to the denominator, whose exponent
            of $t$ is $i+d=2$, not $i$.
        Text
            A series is displayed as the fraction
            @TT "numerator/denominator"@.  @TO expression@ returns this
            fraction as a @TO Divide@, @TO describe@ gives its constructor
            call, and @TO toExternalString@ gives a string that reconstructs
            it.  Repeated constructor calls with the same parameters
            return the same object.
        Example
            expression P
            describe P
            toExternalString P
            value toExternalString P === P
            genericDetPoincare(2,2,2) === P
        Text
            The variables of the Schur ring and of its base ring are not
            bound to the global symbols @TT "s"@, @TT "t"@, @TT "x"@ and
            @TT "y"@.  A character is written as @TT "R_lambda"@ with
            @TT "R = ring P"@, the factor @TT "y"@ of the generic case
            being promoted from @TT "coefficientRing R"@, and $s$ and $t$
            are the generators of @TT "coefficientRing P"@.
        Example
            R = ring P; Ry = coefficientRing R; S = coefficientRing P;
            c = R_{1,1} * promote(Ry_{1,1}, R)
            denominator P == 1 - c * S_0^2 * S_1^2
        Text
            Here @TT "S_0"@ and @TT "S_1"@ are $s$ and $t$;
            @TT "c"@ is the character of the determinant equation.  The
            final equality verifies its coefficient in the denominator.
        Text
            The expansion of the series is @TO expandPoincare@ (the
            characters) or @TO (betti, PoincareSeries, ZZ)@ (their
            dimensions); @TO dimensionSpecialization@ and
            @TO ordinaryPoincareSeries@ forget the group action;
            @TO determinantalRing@ constructs the ring represented by the
            series, and
            @TO verifyPoincareSeries@, @TO verifyStrandBettiTable@ and
            @TO koszulPoincareCheck@ compare the formula with Macaulay2's
            resolutions and Hilbert series.
    SeeAlso
        genericDetPoincare
        symDetPoincare
        pfaffianPoincare
        symDetCoverPoincare
        expandPoincare
        (betti, PoincareSeries, ZZ)
        dimensionSpecialization
        ordinaryPoincareSeries
        strandBettiTable
        equivariantStrandBettiTable
        determinantalRing
///

doc ///
    Key
        genericDetPoincare
        (genericDetPoincare, ZZ, ZZ, ZZ)
    Headline
        Poincare series of the residue field over a generic determinantal ring
    Usage
        P = genericDetPoincare(n, m, r)
    Inputs
        n:ZZ
            the number of rows of the generic matrix; $n\ge1$
        m:ZZ
            the number of columns of the generic matrix; $m\ge1$
        r:ZZ
            the minor size; $2\le r\le\min(n,m)$ is the range of Corollary
            7.1.3 of the paper, and for $r>\min(n,m)$ the ideal $I_r$ is
            zero and the series is that of $\mathbf k$ over the polynomial
            ring $S$ itself (denominator $1$)
    Outputs
        P:PoincareSeries
            the $\mathrm{GL}(V)\times\mathrm{GL}(W)$-equivariant Poincare
            series
            $P(t,s)=\sum_{i,j}[\operatorname{Tor}^{S/I_r}_i(\mathbf k,\mathbf k)_j]\,t^i s^j$
    Description
        Text
            Here $S=\operatorname{Sym}(V\otimes W)$ with $\dim V=n$ and
            $\dim W=m$, and $I_r$ is the ideal of $r\times r$ minors of the
            generic $n\times m$ matrix.  The series is the formula of
            Corollary 7.1.3(i) of the paper; its coefficients are elements
            of the Schur ring @TT "schurRing(schurRing(QQ[s,t], y, m), x, n)"@,
            where @TT "x"@ indexes $\mathrm{GL}(V)$ (the rows) and @TT "y"@
            indexes $\mathrm{GL}(W)$ (the columns), and $s$ is the variable
            $u$ of the paper.  Matrix entries have internal degree $1$,
            $t$ records homological degree, and the coefficient of $t^is^j$
            in the expanded fraction is the character of
            $\operatorname{Tor}^{S/I_r}_i(\mathbf k,\mathbf k)_j$.
            The symbols $x_\lambda,y_\mu$ denote the characters of
            $\mathbf S_\lambda(V),\mathbf S_\mu(W)$; their product denotes
            the tensor product representation.
        Text
            The hypersurface of a generic $2\times2$ matrix:
        Example
            P = genericDetPoincare(2,2,2)
            ordinaryPoincareSeries P
        Text
            There are four degree-one variables and one quadratic
            determinant equation.  The numerical fraction is
            $(1+t)^4/(1-t^2)=(1+t)^3/(1-t)$; thus the residue-field
            Betti numbers begin $1,4,7,8,8,\ldots$.
        Text
            For the $3\times3$ minors of a generic $5\times5$ matrix the
            specialization to dimensions gives the series in the
            introduction of the paper.  Its denominator and first total
            Betti numbers are:
        Example
            P = genericDetPoincare(5,5,3);
            (dimensionSpecialization P)#1
            totalBettiNumbers(P, 5)
        Text
            The list reads
            $\dim\operatorname{Tor}^{S/I_3}_i(\mathbf k,\mathbf k)
            =1,25,400,5250,64728,779530$ for $i=0,\ldots,5$.
            These are coefficients of the quotient with numerator
            $(1+t)^{25}$, not coefficients of the displayed denominator.
        Text
            The submaximal minors of a $4\times4$ matrix give the
            Gulliksen--Negard denominator $1-16t^2-30t^3-16t^4+t^6$, and
            maximal minors give Golod's, $1-\sum_{i\ge1}\beta_it^{i+1}$.
            For these generic determinantal rings, Golodness is equivalent
            to every denominator term having strand index $1$
            (see @TO isGolodPoincare@).
        Example
            (dimensionSpecialization genericDetPoincare(4,4,3))#1
            (dimensionSpecialization genericDetPoincare(2,4,2))#1
            isGolodPoincare genericDetPoincare(2,4,2)
        Text
            In the maximal-minor example the denominator is
            $1-6t^2-8t^3-3t^4$: the numbers $6,8,3$ are the ranks of the
            positive steps in the resolution of the determinantal ring
            over its eight-variable polynomial ring.  The verdict
            @TT "true"@ identifies this as the Golod form.
        Text
            When $r$ exceeds $\min(n,m)$ the ideal is zero and one recovers
            $(1+t)^{nm}$:
        Example
            dimensionSpecialization genericDetPoincare(2,2,3)
        Text
            A $2\times2$ matrix has no $3\times3$ minors.  The pair is
            therefore $\{(1+t)^4,1\}$, the Poincare series of the residue
            field over a polynomial ring in four variables.
    SeeAlso
        PoincareSeries
        symDetPoincare
        pfaffianPoincare
        symDetCoverPoincare
        expandPoincare
        (betti, PoincareSeries, ZZ)
        strandBettiTable
        dimensionSpecialization
        ordinaryPoincareSeries
        isGolodPoincare
        verifyPoincareSeries
///

doc ///
    Key
        symDetPoincare
        (symDetPoincare, ZZ, ZZ)
    Headline
        Poincare series of the residue field over a symmetric determinantal ring
    Usage
        P = symDetPoincare(n, r)
    Inputs
        n:ZZ
            the size of the generic symmetric matrix; $n\ge1$
        r:ZZ
            the minor size; $2\le r\le n$ is the range of Corollary 7.1.3 of
            the paper, and for $r>n$ the ideal is zero and the denominator
            is $1$
    Outputs
        P:PoincareSeries
            the $\mathrm{GL}(V)$-equivariant Poincare series of the residue
            field over $\operatorname{Sym}(S^2V)/I_r$
    Description
        Text
            Over a field $\mathbf k$ of characteristic zero, let
            $S=\operatorname{Sym}(S^2V)$ with $\dim V=n$; $I_r$ is
            the ideal of $r\times r$ minors of the generic symmetric matrix.
            The coefficients are characters of $\mathrm{GL}(V)$ (the factor
            @TT "x"@), elements of @TT "schurRing(QQ[s,t], x, n)"@, and $s$
            is the variable $u$ of the paper.  This is the first formula of
            Corollary 7.1.3(ii) of the paper; the series of the cover $M_r$
            is @TO symDetCoverPoincare@.
            The entries of the symmetric matrix have internal degree $1$.
            In the expansion, $t^is^jx_\lambda$ records a copy of
            $\mathbf S_\lambda(V)$ in
            $\operatorname{Tor}^{S/I_r}_i(\mathbf k,\mathbf k)_j$;
            $x_{()}=1$ is the trivial character.
        Example
            P = symDetPoincare(3,2)
            ordinaryPoincareSeries P
            expandPoincare(P, 3)
        Text
            Here $V=\mathbf k^3$ and $R=S/I_2$.  The numerical series is
            $(1+t)^3/(1-3t)$, with coefficients $1,6,21,64,\ldots$.
            The first two positive columns say
            $$
            \operatorname{Tor}^R_1(\mathbf k,\mathbf k)_1
                \cong \mathbf S_{(2)}(V),\qquad
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_2
                \cong \mathbf S_{(3,1)}(V)\oplus\mathbf S_{(2,2)}(V).
            $$
            Their dimensions are $6$ and $15+6=21$.  The row label $0$
            means $j-i=0$, not internal degree zero.
        Text
            For a nonquadratic example take $V=\mathbf k^5$ and
            $R=\operatorname{Sym}(S^2V)/I_3$.  The $3\times3$ minors are
            cubic equations in the $15$ independent matrix entries:
        Example
            P3 = symDetPoincare(5,3);
            betti(P3, 4)
            isGolodPoincare P3
        Text
            Column $2$ has $105$ classes in internal degree $2$ and $50$
            in internal degree $3$ (the row labels are $j-i$).  More precisely,
            $$
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_2
                \cong\mathbf S_{(3,1)}(V),\qquad
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_3
                \cong\mathbf S_{(2,2,2)}(V).
            $$
            The degree-three group records the cubic equations; its nonzero
            off-diagonal position shows that $R$ is not Koszul.
            The @TT "false"@ output says that $R$ is not Golod either.
            Indeed, $\operatorname{Tor}^S_6(R,\mathbf k)_{10}
            \cong(\det V)^4$ has strand index
            $d=(10-6)/(3-1)=2$ and contributes
            $+s^{10}t^8x_{(4,4,4,4,4)}$ to the denominator.  This is a
            syzygy of $R$ over $S$, not an entry in the displayed
            residue-field table.
    SeeAlso
        PoincareSeries
        symDetCoverPoincare
        genericDetPoincare
        pfaffianPoincare
        expandPoincare
        ordinaryPoincareSeries
        strandBettiTable
        isGolodPoincare
///

doc ///
    Key
        pfaffianPoincare
        (pfaffianPoincare, ZZ, ZZ)
    Headline
        Poincare series of the residue field over a Pfaffian ring
    Usage
        P = pfaffianPoincare(n, r)
    Inputs
        n:ZZ
            the size of the generic skew symmetric matrix; $n\ge1$
        r:ZZ
            half the Pfaffian order (the ideal of $2r\times2r$ Pfaffians);
            $2\le r\le\lfloor n/2\rfloor$ is the range of Corollary 7.1.3 of
            the paper, and for $2r>n$ the ideal is zero and the denominator
            is $1$
    Outputs
        P:PoincareSeries
            the $\mathrm{GL}(V)$-equivariant Poincare series of the residue
            field over $\operatorname{Sym}(\bigwedge^2V)/I_{2r}$
    Description
        Text
            Over a field $\mathbf k$ of characteristic zero, let
            $S=\operatorname{Sym}(\bigwedge^2V)$ with $\dim V=n$;
            $I_{2r}$ is the ideal of $2r\times2r$ Pfaffians of the generic
            skew symmetric matrix.  The coefficients are characters of
            $\mathrm{GL}(V)$ (the factor @TT "x"@), elements of
            @TT "schurRing(QQ[s,t], x, n)"@, and $s$ is the variable $u$ of
            the paper.  This is Corollary 7.1.3(iii) of the paper.
            The skew-matrix entries have internal degree $1$, and each
            order-$2r$ Pfaffian has degree $r$.  In the expanded fraction,
            $t^is^jx_\lambda$ records $\mathbf S_\lambda(V)$ in
            $\operatorname{Tor}^{S/I_{2r}}_i(\mathbf k,\mathbf k)_j$.
        Example
            P = pfaffianPoincare(4,2)
            ordinaryPoincareSeries P
            strandBettiTable P
        Text
            For $n=4,r=2$ there is one quadratic Pfaffian in six
            variables.  It spans $\bigwedge^4V$, whose character is
            $x_{(1,1,1,1)}$.  The numerical residue-field series is
            $(1+t)^6/(1-t^2)=(1+t)^5/(1-t)$; the separate strand table
            describes $0\longrightarrow S(-2)\longrightarrow S
            \longrightarrow S/I_4\longrightarrow0$.
        Text
            The $6\times6$ Pfaffians of an $8\times8$ skew symmetric matrix
            are Example 7.1.7 of the paper:
        Example
            (dimensionSpecialization pfaffianPoincare(8,3))#1
        Text
            The parameter $3$ means order $6$, hence cubic equations.
            The displayed polynomial is the denominator of the numerical
            Poincare series, whose numerator is $(1+t)^{28}$ because an
            $8\times8$ skew symmetric matrix has $28$ independent entries.
        Example
            P3 = pfaffianPoincare(8,3);
            betti(P3, 5)
            isGolodPoincare P3
        Text
            Here $V=\mathbf k^8$ and $R=S/I_6$.  Row $j-i=1$ in column
            $2$ records the $28$ cubic equations, while row $0$ in that
            column has dimension $378$.  Their representations are
            $$
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_2
                \cong\mathbf S_{(2,1,1)}(V),\qquad
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_3
                \cong\bigwedge^6V.
            $$
            The off-diagonal group shows that $R$ is not Koszul, and
            @TT "false"@ says that it is not Golod.  The first positive
            nonconstant denominator term, $+36t^5$, comes from
            $\operatorname{Tor}^S_3(R,\mathbf k)_7
            \cong\mathbf S_{(2,2,2,2,2,2,2)}(V)$ of dimension $36$.
            Its strand index is $d=(7-3)/(3-1)=2$, so its full term is
            $+s^7t^5x_{(2,2,2,2,2,2,2)}$.  The displayed table instead
            concerns $\operatorname{Tor}^R(\mathbf k,\mathbf k)$.
    SeeAlso
        PoincareSeries
        genericDetPoincare
        symDetPoincare
        expandPoincare
        ordinaryPoincareSeries
        strandBettiTable
        isGolodPoincare
///

doc ///
    Key
        symDetCoverPoincare
        (symDetCoverPoincare, ZZ, ZZ)
    Headline
        Poincare series of the residue field over the cover M_r of a symmetric determinantal ring
    Usage
        P = symDetCoverPoincare(n, r)
    Inputs
        n:ZZ
            the dimension of $V$; $n\ge1$
        r:ZZ
            the minor size; $r\ge2$
    Outputs
        P:PoincareSeries
            the $\mathrm{GL}(V)$-equivariant Poincare series of the residue
            field over $M_r$, in the format of @TO symDetPoincare@
    Description
        Text
            $M_r=\operatorname{Sym}(\mathbf k^{r-1}\otimes V)^{\mathrm{SO}_{r-1}}$
            is the finite cover of the symmetric determinantal ring
            $\operatorname{Sym}(S^2V)/I_r$.  The group acts on the
            $\mathbf k^{r-1}$ factor, and the internal grading is inherited
            from the polynomial ring on $\mathbf k^{r-1}\otimes V$.
            Thus quadratic invariants have degree $2$ and determinant
            invariants have degree $r-1$; the recorded value
            @TT "P#\"generatorDegree\""@ is $2$ for this family, not a
            claim that every generator has that degree.
            This is the second formula of Corollary 7.1.3(ii) of the paper;
            for $r=2$ the algebra is $\operatorname{Sym}(V)$.
            As in the symmetric case, $x_\lambda$ denotes the character of
            $\mathbf S_\lambda(V)$, and the coefficient of $t^is^j$ in
            the expansion is $[\operatorname{Tor}^{M_r}_i(\mathbf k,\mathbf k)_j]$.
        Example
            P = symDetCoverPoincare(3,3)
            dimensionSpecialization P
            totalBettiNumbers(P, 4)
            dimensionSpecialization symDetCoverPoincare(3,2)
        Text
            For $n=r=3$ the numerical fraction is
            $(1+t)^6/(1-3t-3t^2+t^3)$, whose expansion begins
            $1+9t+45t^2+181t^3+684t^4$.  Its nine algebra generators have
            internal degree $2$.  By contrast, the last pair is
            $\{(1+t)^3,1\}$: when $r=2$, $M_2$ is a polynomial ring on
            three degree-one generators.
        Text
            For $r=3$ the group $\mathrm{SO}_2$ is a torus and $M_3$ is the
            Segre ring of an $n\times n$ matrix.  After regrading its
            generators to degree $1$, this ring is Koszul.
            @TO determinantalRing@ constructs the ring and
            @TO verifyPoincareSeries@ compares the formula with a
            computed resolution.
        Example
            determinantalRing P
            verifyPoincareSeries(P, 3)
            koszulPoincareCheck P
        Text
            The quotient displayed is the $3\times3$ Segre ring, defined
            by its $2\times2$ minors with each matrix variable in degree
            $2$.  The first @TT "true"@ checks the residue-field Betti
            table through homological degree $3$; the second checks
            $P_{M_3}(t)=1/H_{M_3}(-t)$ after regrading its generators to
            degree $1$.
    SeeAlso
        PoincareSeries
        symDetPoincare
        determinantalRing
        verifyPoincareSeries
        koszulPoincareCheck
///

doc ///
    Key
        expandPoincare
        (expandPoincare, PoincareSeries, ZZ)
    Headline
        the characters of the Tor groups of the residue field, as a Betti table
    Usage
        B = expandPoincare(P, N)
        B = expandPoincare(P, N, Truncation => "TotalDegree")
    Inputs
        P:PoincareSeries
            a series from @TO genericDetPoincare@, @TO symDetPoincare@,
            @TO pfaffianPoincare@ or @TO symDetCoverPoincare@
        N:ZZ
            the last homological degree to include (with the default
            truncation), or a bound on the total degree in $s$ and $t$ (see
            @TO Truncation@)
    Outputs
        B:EquivariantBettiTally
            the table whose entry at $(i,\{j\},j)$ is the character of
            $\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j$, an element of
            @TT "ring P"@; with the default truncation every nonzero
            bidegree with $i\le N$ appears, so each listed homological
            degree is complete
    Description
        Text
            Let $R$ be the ring specified by @TT "P"@.  This table concerns
            the resolution of its residue field $\mathbf k=R/R_+$ over
            $R$, not the resolution of $R$ over a polynomial ring.
            Column $i$ and row $j-i$ contain the character of
            $\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j$.
            For a generic matrix, $x_\lambda y_\mu$ denotes
            $\mathbf S_\lambda(V)\otimes\mathbf S_\mu(W)$, where $V,W$
            are its row and column spaces.  For the symmetric, Pfaffian
            and cover families, $x_\lambda$ denotes $\mathbf S_\lambda(V)$.
        Text
            The series @TT "numerator/denominator"@ is expanded as a power
            series in $s$ and $t$ by inverting the denominator modulo
            $t^{N+1}$, which is a finite computation because every
            nonconstant denominator term has homological degree at least
            one.  The entry at $(0,\{0\},0)$ is the trivial character (that
            is $\operatorname{Tor}_0(\mathbf k,\mathbf k)=\mathbf k$).
            The dimensions of the
            characters are the Betti numbers of
            @TO (betti, PoincareSeries, ZZ)@ and @TO totalBettiNumbers@.
        Text
            The table is an @TO EquivariantBettiTally@, a @TO BettiTally@
            whose entries are Schur ring elements rather than integers: it
            prints in the layout of @TT "betti res"@ with a vertical rule
            between the columns and the row of totals summing the
            characters, it can be compared with @TO symbol ==@, and its
            entries are read with the keys $(i,\{j\},j)$.  The entries live
            in @TT "ring P"@, which in the generic case is the Schur ring
            @TT "schurRing(schurRing(QQ[s,t], y, m), x, n)"@.  Every
            product of characters is expanded by the
            Littlewood--Richardson rule; in the generic case an entry is a
            combination of the products $y_\mu x_\lambda$ of one Schur
            polynomial in each alphabet, printed with the terms grouped by their $x$
            factor.  The functions @TO poincare@ and @TO regularity@ of Core
            expect integer entries and are not meaningful on it; apply
            @TO dimensionSpecialization@ to an entry to get its dimension.
        Text
            For the hypersurface $R=\mathbf k[x_{ij}]/(\det)$ of a generic
            $2\times2$ matrix the residue field has Tor groups in every
            homological degree, concentrated on the diagonal $j=i$ because
            the ring is Koszul; the square of the character of
            $\operatorname{Tor}_1$ minus the character of
            $\operatorname{Tor}_2$ is the character of the degree two part
            of the ring:
        Example
            P = genericDetPoincare(2,2,2);
            B = expandPoincare(P, 4)
            sort keys B
            B#(2,{2},2)
            (B#(1,{1},1))^2
            (B#(1,{1},1))^2 - B#(2,{2},2)
            dimensionSpecialization B#(2,{2},2)
            betti(P, 4)
        Text
            Here $\dim V=\dim W=2$.  The degree-two entry means
            $$
            \begin{aligned}
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_2\cong{}&
            (\mathbf S_{(2)}(V)\otimes\mathbf S_{(1,1)}(W))\\
            &\oplus(\mathbf S_{(1,1)}(V)\otimes\mathbf S_{(2)}(W))\\
            &\oplus(\mathbf S_{(1,1)}(V)\otimes\mathbf S_{(1,1)}(W)).
            \end{aligned}
            $$
            Its dimension is $3+3+1=7$.  Subtracting this character from
            that of $(V\otimes W)^{\otimes2}$ leaves $x_{(2)}y_{(2)}$,
            the character of $R_2=\operatorname{Sym}^2V\otimes
            \operatorname{Sym}^2W$.  The numerical table has column totals
            $1,4,7,8,8$; the resolution continues beyond degree $4$.
        Text
            The quotient by the determinant of a $3\times3$ matrix is not
            Koszul.  Its Tor groups through homological degree $3$ occupy
            two rows: homological degree $2$ has characters in internal
            degrees $2$ and $3$, the second being the
            character of the determinant.  The dimensions of the characters
            are the entries of the integer table, so the two agree row by
            row.
        Example
            E = expandPoincare(genericDetPoincare(3,3,3), 3)
            E#(2,{3},3)
            betti(genericDetPoincare(3,3,3), 3)
        Text
            The selected entry says
            $\operatorname{Tor}^R_2(\mathbf k,\mathbf k)_3
            \cong\bigwedge^3V\otimes\bigwedge^3W$, of dimension $1$.
            The same column has a $36$-dimensional component in internal
            degree $2$, so its total is $37$.  The two rows separate these
            components rather than mixing their internal degrees.
    SeeAlso
        PoincareSeries
        Truncation
        (betti, PoincareSeries, ZZ)
        totalBettiNumbers
        equivariantStrandBettiTable
        dimensionSpecialization
///

doc ///
    Key
        Truncation
        [expandPoincare, Truncation]
    Headline
        truncate a Poincare series by homological or total degree
    Usage
        expandPoincare(P, N, Truncation => "TotalDegree")
    Description
        Text
            By default (@TT "\"Homological\""@) @TO expandPoincare@ returns
            every nonzero bidegree of homological degree at most $N$,
            complete in the internal degree, which is possible because every
            denominator term other than $1$ has homological degree at least
            one.  With @TT "\"TotalDegree\""@ it returns the bidegrees with
            $i+j\le N$ instead; in that mode a homological degree $i$ is
            complete only when $N$ is at least $i$ plus the largest internal
            shift occurring in $\operatorname{Tor}_i$, which the output does
            not reveal.
        Example
            P = genericDetPoincare(3,3,3);
            sort keys expandPoincare(P, 4)
            sort keys expandPoincare(P, 4, Truncation => "TotalDegree")
        Text
            The default includes $(2,\{3\},3)$, the determinant character
            in $\operatorname{Tor}_2$ of internal degree $3$.
            @TT "\"TotalDegree\""@ drops it because $2+3>4$, retaining
            only $(0,\{0\},0)$, $(1,\{1\},1)$ and $(2,\{2\},2)$.
            Thus a missing entry in that second output does not imply
            that the corresponding Tor group is zero.
    SeeAlso
        expandPoincare
///

doc ///
    Key
        (betti, PoincareSeries, ZZ)
    Headline
        the Betti table of the residue field over a determinantal ring
    Usage
        B = betti(P, N)
    Inputs
        P:PoincareSeries
            a series from one of the constructors
        N:ZZ
            the last homological degree to include
        Weights=>List
            accepted but ignored by this method
        Minimize=>Boolean
            accepted but ignored by this method
    Outputs
        B:BettiTally
            the table of $\dim\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j$
            for $i\le N$, complete in $j$, in the format of @TT "betti res"@
    Description
        Text
            For the ring $R$ specified by @TT "P"@, write
            $\beta^R_{i,j}(\mathbf k)=
            \dim\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j$.
            It appears in column $i$ and row $j-i$; the row of totals
            sums over $j$.  These are the ranks and shifts in the minimal
            $R$-free resolution of $\mathbf k$, whereas
            @TO strandBettiTable@ describes the determinantal ring over
            its polynomial ring.
        Text
            This is the expansion of the series with the characters of
            @TO expandPoincare@ replaced by their dimensions.  The
            @TO2 {"Macaulay2Doc :: betti(...,Weights=>...)", "Weights"}@ and
            @TO2 {"OldChainComplexes :: betti(...,Minimize=>...)", "Minimize"}@
            options of @TO betti@ have no effect on this method.
            @TO totalBettiNumbers@ returns the column sums, and
            @TO poincareSeriesTruncation@ records these sums as coefficients
            of a polynomial in $t$.  For the
            $3\times3$ minors of a generic $5\times5$ matrix:
        Example
            betti(genericDetPoincare(5,5,3), 4)
        Text
            Column $2$ contains $300$ in row $0$ and $100$ in row $1$:
            $\beta^R_{2,2}(\mathbf k)=300$ and
            $\beta^R_{2,3}(\mathbf k)=100$, with total $400$.
            The displayed column $4$ is a truncation of an infinite
            resolution, not its final step.
        Text
            The table of a Koszul ring has one row:
        Example
            betti(genericDetPoincare(2,3,2), 4)
        Text
            Here all internal degrees satisfy $j=i$, so row $0$ contains
            the totals $1,6,18,40,81$.  This is the diagonal behavior
            defining a Koszul algebra.
        Text
            @TO verifyPoincareSeries@ compares this table with the Betti
            table of a computed resolution of the residue field.
    SeeAlso
        PoincareSeries
        expandPoincare
        totalBettiNumbers
        poincareSeriesTruncation
        verifyPoincareSeries
///

doc ///
    Key
        totalBettiNumbers
        (totalBettiNumbers, PoincareSeries, ZZ)
    Headline
        the total Betti numbers of the residue field over a determinantal ring
    Usage
        L = totalBettiNumbers(P, N)
    Inputs
        P:PoincareSeries
            a series from one of the constructors
        N:ZZ
            the last homological degree to include
    Outputs
        L:List
            the list $\{\dim\operatorname{Tor}^R_i(\mathbf k,\mathbf k)\}_{0\le i\le N}$
    Description
        Text
            These are the coefficients of the ordinary Poincare series, the
            column sums of @TO (betti, PoincareSeries, ZZ)@.  For the
            $3\times3$ minors of a generic $5\times5$ matrix they are the
            coefficients of the series displayed in the introduction of the
            paper:
        Example
            totalBettiNumbers(genericDetPoincare(5,5,3), 5)
            totalBettiNumbers(symDetCoverPoincare(3,3), 4)
        Text
            The first list starts with the dimension $1$ of
            $\operatorname{Tor}_0$, then $25$ in homological degree $1$
            and $400$ in degree $2$, summing over all internal degrees.
            The second list is $1,9,45,181,684$ for the cover $M_3$.
            Each bound includes degree zero, so the lists have lengths
            $6$ and $5$, respectively.
    SeeAlso
        (betti, PoincareSeries, ZZ)
        poincareSeriesTruncation
        ordinaryPoincareSeries
///

doc ///
    Key
        poincareSeriesTruncation
        (poincareSeriesTruncation, PoincareSeries, ZZ)
    Headline
        the Poincare series expanded to a given homological degree
    Usage
        f = poincareSeriesTruncation(P, N)
        f = poincareSeriesTruncation(P, N, KeepInternalDegree => true)
    Inputs
        P:PoincareSeries
            a series from one of the constructors
        N:ZZ
            the last homological degree to include
        KeepInternalDegree=>Boolean
            when @TT "true"@, keep the internal degree as a power of $s$
    Outputs
        f:RingElement
            the polynomial
            $\sum_{i\le N}\dim\operatorname{Tor}_i^R(\mathbf k,\mathbf k)\,t^i$
            in the base ring @TT "coefficientRing P"@, that is
            $\mathbb Q[s,t]$, or the bigraded polynomial in $s$ and $t$
    Description
        Text
            The coefficient of $t^i$ is the total rank of step $i$ in the
            minimal resolution of the residue field over the ring
            specified by @TT "P"@.  With @TT "KeepInternalDegree => true"@,
            that coefficient is instead
            $\sum_j\dim\operatorname{Tor}^R_i(\mathbf k,\mathbf k)_j\,s^j$:
            the exponent of $s$ retains the internal degree.
        Example
            poincareSeriesTruncation(genericDetPoincare(5,5,3), 5)
            poincareSeriesTruncation(genericDetPoincare(2,2,2), 3, KeepInternalDegree => true)
        Text
            The first answer begins $1+25t+400t^2$ and ends with
            $779530t^5$; terms of higher homological degree are omitted.
            The second is $1+4st+7s^2t^2+8s^3t^3$, so, for example,
            $\operatorname{Tor}_3$ has dimension $8$ and internal degree
            $3$.
    SeeAlso
        (betti, PoincareSeries, ZZ)
        totalBettiNumbers
        ordinaryPoincareSeries
        KeepInternalDegree
///

doc ///
    Key
        dimensionSpecialization
        (dimensionSpecialization, PoincareSeries)
        (dimensionSpecialization, RingElement)
    Headline
        specialize an equivariant Poincare series to dimensions
    Usage
        L = dimensionSpecialization P
        L = dimensionSpecialization(P, KeepInternalDegree => true)
        f = dimensionSpecialization e
    Inputs
        P:PoincareSeries
            a series from one of the constructors
        e:RingElement
            an element of a (nested) Schur ring
    Outputs
        L:List
            for @TT "P"@: the specialized numerator and denominator,
            evaluated at $s=1$, as elements of @TT "coefficientRing P"@,
            that is $\mathbb Q[s,t]$ (or with $s$ kept, see
            @TO KeepInternalDegree@)
        f:RingElement
            for @TT "e"@: the element with every Schur functor replaced by
            its dimension, in the underlying coefficient ring
            ($\mathbb Q[s,t]$ for elements obtained from a @TO PoincareSeries@)
    Description
        Text
            Applied to a Schur ring element, this replaces each Schur
            functor by its dimension: $x_\lambda y_\mu$ maps to
            $\dim\mathbf S_\lambda(V)\,\dim\mathbf S_\mu(W)$.
            The dimensions of $V,W$ are those of the Schur rings, not
            extra arguments.  For a ring element, the powers of $s$ and
            $t$ are preserved.  Applied to a series, it also sets
            $s=1$ and returns the numerator and denominator separately, in
            the form displayed in the paper: the numerator factors as
            $(1+t)^e$, with $e=nm$ in the generic case,
            $e=n(n+1)/2$ in the symmetric case, and $e=n(n-1)/2$ in the
            Pfaffian case.  For the symmetric cover, $e=n(n+1)/2$ when
            $r\ge3$ and $e=n$ when $r=2$.  The denominator is
            $\sum_d(-1)^d\sum_i\beta_{i,i+(r-1)d}\,t^{i+d}$
            for the generic, symmetric and Pfaffian families; here
            $\beta_{i,j}=\dim\operatorname{Tor}^S_i(R,\mathbf k)_j$.
            For the symmetric cover, the internal shifts are those
            recorded in its denominator terms.  Use
            @TO ordinaryPoincareSeries@ for their quotient in lowest terms.
            For the $3\times3$ minors of a generic $5\times5$ matrix:
        Example
            L = dimensionSpecialization genericDetPoincare(5,5,3);
            factor L#0
            L#1
        Text
            The answer is a pair $\{N,D\}$, not a quotient: the numerical
            Poincare series is $N/D$.  In this example $N=(1+t)^{25}$,
            and @TT "L#1"@ is the displayed polynomial $D$.
            Negative coefficients in $D$ are not negative Betti numbers;
            those numbers are coefficients of the expansion of $N/D$.
        Text
            The submaximal minors of a $4\times4$ matrix (the case settled by
            Avramov via the Gulliksen--Negard resolution) and the $6\times6$
            Pfaffians of an $8\times8$ skew symmetric matrix (Example 7.1.7
            of the paper):
        Example
            (dimensionSpecialization genericDetPoincare(4,4,3))#1
            (dimensionSpecialization pfaffianPoincare(8,3))#1
        Text
            On a single character:
        Example
            P = genericDetPoincare(2,3,2);
            denominator P
            dimensionSpecialization denominator P
        Text
            The two nontrivial characters in this denominator have
            dimensions $3$ and $2$, giving $1-3s^2t^2-2s^3t^3$.
            The variable $s$ remains because the input was a ring element;
            @TT "dimensionSpecialization P"@ would additionally set $s=1$.
    SeeAlso
        PoincareSeries
        KeepInternalDegree
        ordinaryPoincareSeries
        strandBettiTable
        expandPoincare
///

doc ///
    Key
        KeepInternalDegree
        [dimensionSpecialization, KeepInternalDegree]
        [poincareSeriesTruncation, KeepInternalDegree]
    Headline
        keep the internal degree when specializing a Poincare series
    Usage
        dimensionSpecialization(P, KeepInternalDegree => true)
        poincareSeriesTruncation(P, N, KeepInternalDegree => true)
    Description
        Text
            With @TT "KeepInternalDegree => true"@,
            @TO dimensionSpecialization@ applied to a series returns the
            numerator and denominator with every Schur functor replaced by
            its dimension but with $s$ kept, so the pair represents the
            bigraded numerical series, and @TO poincareSeriesTruncation@
            returns the bigraded polynomial in $s$ and $t$.
        Example
            P = genericDetPoincare(2,2,2);
            dimensionSpecialization(P, KeepInternalDegree => true)
            poincareSeriesTruncation(P, 3, KeepInternalDegree => true)
        Text
            The pair is $\{(1+st)^4,1-s^2t^2\}$.  Dividing and expanding
            through homological degree $3$ gives
            $1+4st+7s^2t^2+8s^3t^3$, the second answer.  Each term
            $c\,t^is^j$ records a Tor group of dimension $c$ in degrees
            $(i,j)$.
    SeeAlso
        dimensionSpecialization
        poincareSeriesTruncation
///

doc ///
    Key
        ordinaryPoincareSeries
        (ordinaryPoincareSeries, PoincareSeries)
    Headline
        the ordinary Poincare series as a rational function of t
    Usage
        f = ordinaryPoincareSeries P
    Inputs
        P:PoincareSeries
            a series from one of the constructors
    Outputs
        f:RingElement
            the non-equivariant series, an element of the fraction field of
            the base ring @TT "coefficientRing P"@, that is $\mathbb Q[s,t]$,
            with $s$ already set to $1$, so a rational function of $t$ alone
    Description
        Text
            This applies @TO dimensionSpecialization@ and returns the
            quotient of the resulting polynomials: the ordinary Poincare
            series, with the internal grading forgotten.  The quotient is in
            lowest terms, so a common factor $(1+t)^k$ may be canceled and
            the numerator and denominator may both change sign; to see them
            before cancellation use @TO dimensionSpecialization@, whose
            first entry factors as $(1+t)^e$.  To move the result to a
            one-variable ring use @TT "sub(f, frac(QQ[t]))"@; note that
            defining a ring @TT "QQ[t]"@ binds the global symbol @TT "t"@.
            The expansion of the series is given by
            @TO poincareSeriesTruncation@ and @TO totalBettiNumbers@.
        Example
            ordinaryPoincareSeries genericDetPoincare(2,2,2)
            ordinaryPoincareSeries pfaffianPoincare(4,2)
            ordinaryPoincareSeries genericDetPoincare(5,5,3)
        Text
            The first two answers are $(1+t)^3/(1-t)$ and
            $(1+t)^5/(1-t)$.  Before cancellation these are
            $(1+t)^4/(1-t^2)$ and $(1+t)^6/(1-t^2)$, the residue-field
            series of quadratic hypersurfaces in four and six variables.
            Their coefficients, not the signs used to print the
            fractions, are the nonnegative total Betti numbers.
    SeeAlso
        dimensionSpecialization
        poincareSeriesTruncation
        totalBettiNumbers
///

doc ///
    Key
        strandBettiTable
        (strandBettiTable, PoincareSeries)
    Headline
        the graded Betti numbers of a determinantal ring read off from its Poincare denominator
    Usage
        B = strandBettiTable P
    Inputs
        P:PoincareSeries
            a series from one of the constructors
    Outputs
        B:BettiTally
            the table of $\beta^S_{i,j}(R)=\dim\operatorname{Tor}_i^S(R,\mathbf k)_j$,
            in the format of @TT "betti res"@
    Description
        Text
            For a determinantal quotient $R=S/I$, this table describes
            its minimal $S$-free resolution:
            $F_i=\bigoplus_j S(-j)^{\beta^S_{i,j}(R)}$.
            Column $i$ and row $j-i$ contain $\beta^S_{i,j}(R)$.
            This is not the table of the residue field over $R$, which
            is returned by @TO (betti, PoincareSeries, ZZ)@.
        Text
            For the generic, symmetric and Pfaffian families, each
            nonconstant denominator term is
            $(-1)^d\chi\,t^{i+d}s^{i+(r-1)d}$, with $\chi$ the
            character of a summand of $\operatorname{Tor}^S_i(R,\mathbf k)$.
            The records @TT "P#\"denominatorTerms\""@ carry
            $i$ and $d$ for every term, so the graded Betti numbers of $R$
            over $S$ are read off for every $r\ge2$, including the Koszul
            case $r=2$ where the exponents alone would not separate the
            strands (Corollary 7.1.3 of the paper).  The table is a
            @TO BettiTally@, comparable with @TT "betti res"@ under
            @TO symbol ==@; @TO equivariantStrandBettiTable@ keeps the
            characters and @TO verifyStrandBettiTable@ compares the table
            with a computed resolution of $R$ over $S$.
            For the symmetric cover $M_r$ with $r\ge3$, the base is
            $S=\operatorname{Sym}(S^2V)$ with its generators in degree $2$:
            the table describes $M_r$ as an $S$-module, not its resolution
            over the ambient ring of its Segre presentation.  For $r=2$,
            the table instead describes $M_2=\operatorname{Sym}(V)$ over
            itself, with only $\mathbf k$ in $\operatorname{Tor}_0$.
        Text
            For the $3\times3$ minors of a generic $5\times5$ matrix the
            first strand (the row $j-i=2$) has total dimension $2303$, the
            number of minimal generators of the Koszul homology algebra:
        Example
            B = strandBettiTable genericDetPoincare(5,5,3)
            sum apply(select(keys B, k -> k#2 - k#0 == 2), k -> B#k)
        Text
            For example, the entry $100$ in column $1$, row $2$ counts
            cubic generators, while $450$ in column $2$, row $2$ counts
            syzygies in internal degree $4$.  The row sum is
            $100+450+828+700+225=2303$; it is not a column total.
        Text
            Maximal minors give the Eagon--Northcott numbers, and the
            $2\times2$ minors of a $3\times3$ matrix the Betti numbers
            $1,9,16,9,1$:
        Example
            strandBettiTable genericDetPoincare(2,4,2)
            strandBettiTable genericDetPoincare(3,3,2)
            S = QQ[a_1..a_9];
            strandBettiTable genericDetPoincare(3,3,2) == betti freeResolution(S^1/minors(2, genericMatrix(S,3,3)))
        Text
            The second table gives the resolution
            $$
            0\longrightarrow S(-6)\longrightarrow S(-4)^9
            \longrightarrow S(-3)^{16}\longrightarrow S(-2)^9
            \longrightarrow S\longrightarrow R\longrightarrow0.
            $$
            Its last entry lies in row $2$ because $6-4=2$.
            The final @TT "true"@ compares all ranks and shifts with a
            resolution computed independently by Macaulay2.
    SeeAlso
        PoincareSeries
        equivariantStrandBettiTable
        verifyStrandBettiTable
        dimensionSpecialization
        isGolodPoincare
///

doc ///
    Key
        equivariantStrandBettiTable
        (equivariantStrandBettiTable, PoincareSeries)
    Headline
        the equivariant Betti table of a determinantal ring over its polynomial ring
    Usage
        B = equivariantStrandBettiTable P
    Inputs
        P:PoincareSeries
            a series from one of the constructors
    Outputs
        B:EquivariantBettiTally
            the table whose entry at $(i,\{j\},j)$ is the character of
            $\operatorname{Tor}^S_i(R,\mathbf k)_j$, an element of
            @TT "ring P"@
    Description
        Text
            The terms stored with the denominator record the equivariant
            Betti numbers of the determinantal ring $R$ over its
            polynomial ring $S$ (Corollary 7.1.3 of the paper);
            this table lists them with their Schur functors, for every
            $r\ge2$.  Like @TO expandPoincare@ it is an
            @TO EquivariantBettiTally@, a @TO BettiTally@ with Schur ring
            entries: it prints in the layout of @TT "betti res"@ with a
            vertical rule between the columns and the characters summed in
            the row of totals, and its entries are read with the keys
            $(i,\{j\},j)$; @TO strandBettiTable@ is the same table with the
            dimensions of the characters.
            Column $i$ and row $j-i$ describe the representations of
            generators of internal degree $j$ in the $i$th free module.
            For a generic matrix, $x_\lambda y_\mu$ denotes the character
            of $\mathbf S_\lambda(V)\otimes\mathbf S_\mu(W)$ on the row
            and column spaces.  In the other families only $x_\lambda$
            occurs.  For a symmetric cover with $r\ge3$, the base ring is
            $\operatorname{Sym}(S^2V)$; for $r=2$ it is $M_2$ itself,
            as explained in @TO strandBettiTable@.
        Example
            equivariantStrandBettiTable genericDetPoincare(3,3,2)
            B = equivariantStrandBettiTable pfaffianPoincare(5,2)
            B#(1,{2},2)
            dimensionSpecialization B#(1,{2},2)
        Text
            In the generic $3\times3$ example, column $1$ records
            $\bigwedge^2V\otimes\bigwedge^2W$, the nine quadratic minors.
            The next column is
            $(\mathbf S_{(2,1)}(V)\otimes\bigwedge^3W)
            \oplus(\bigwedge^3V\otimes\mathbf S_{(2,1)}(W))$,
            of dimension $8+8=16$ and internal degree $3$.
            In the Pfaffian example, $n=5,r=2$ means order $4$;
            the selected entry $x_{(1,1,1,1)}$ says that the five quadratic
            Pfaffians form $\bigwedge^4\mathbf k^5$.
    SeeAlso
        PoincareSeries
        strandBettiTable
        expandPoincare
        dimensionSpecialization
///

doc ///
    Key
        determinantalRing
        (determinantalRing, PoincareSeries)
    Headline
        construct the ring represented by a Poincare series
    Usage
        A = determinantalRing P
    Inputs
        P:PoincareSeries
            a series from one of the constructors
    Outputs
        A:QuotientRing
            the ring $S/I$ over $\mathbb Q$ of the recorded family and
            size: the polynomial ring on a generic, generic symmetric or
            generic skew symmetric matrix modulo its minors or Pfaffians,
            or the cover $M_r$
    Description
        Text
            The constructor arguments stored in @TT "P"@ determine the
            presentation: @TT "genericDetPoincare(n,m,r)"@ gives the
            $r$-minors of an $n\times m$ matrix;
            @TT "symDetPoincare(n,r)"@ gives the $r$-minors of a symmetric
            $n\times n$ matrix; @TT "pfaffianPoincare(n,r)"@ gives the
            order-$2r$ Pfaffians of a skew symmetric $n\times n$ matrix.
            In these three cases the independent matrix entries have
            degree $1$, and the defining equations have degree $r$.
        Text
            The variables of $S$ are the default @TT "p_0, p_1, ..."@ of
            Macaulay2.  Repeated calls with the same series return the same
            ring.  For the cover $M_r$ of @TO symDetCoverPoincare@ the ring
            is $\operatorname{Sym}(V)$ when $r=2$ and, when $r=3$, the Segre
            ring of an $n\times n$ matrix with its generators in degree $2$
            (the grading inherited from the invariant-ring description);
            for $r>3$ the cover is not
            constructed and the function raises an error.  The checks
            @TO verifyPoincareSeries@, @TO verifyStrandBettiTable@ and
            @TO koszulPoincareCheck@ use this presentation.
        Example
            A = determinantalRing genericDetPoincare(2,3,2)
            ideal A
            hilbertSeries A
            determinantalRing pfaffianPoincare(4,2)
            determinantalRing symDetCoverPoincare(2,3)
        Text
            The first ring has six variables and three quadratic minors;
            its Hilbert series is $(1-3T^2+2T^3)/(1-T)^6$.
            The next ring is a hypersurface defined by the single
            $4\times4$ Pfaffian.  The last is the $2\times2$ Segre
            hypersurface, but its four variables have degree $2$, so its
            determinant equation has internal degree $4$.
    SeeAlso
        PoincareSeries
        verifyPoincareSeries
        verifyStrandBettiTable
        koszulPoincareCheck
///

doc ///
    Key
        isGolodPoincare
        (isGolodPoincare, PoincareSeries)
    Headline
        test whether a determinantal ring is Golod
    Usage
        isGolodPoincare P
    Inputs
        P:PoincareSeries
            a series from one of the constructors
    Outputs
        :Boolean
            whether every denominator term has strand index $d=1$
    Description
        Text
            A ring $R=S/I$ is Golod exactly when the Poincare series of its
            residue field is
            $(1+t)^{\dim S_1}/(1-\sum_{i\ge1}\beta_i t^{i+1})$,
            where $S$ is the polynomial ring on a minimal set of
            degree-one generators and
            $\beta_i=\dim\operatorname{Tor}^S_i(R,\mathbf k)$;
            for the determinantal rings this means that only the first
            strand contributes to the denominator, that is, every record of
            @TT "P#\"denominatorTerms\""@ has $d=1$.  Generic maximal-minor
            rings are Golod; generic submaximal-minor rings are not.
        Example
            isGolodPoincare genericDetPoincare(2,4,2)
            isGolodPoincare genericDetPoincare(3,3,2)
            isGolodPoincare genericDetPoincare(4,4,3)
        Text
            The outputs are @TT "true"@, @TT "false"@, @TT "false"@.
            The first example has denominator $1-6t^2-8t^3-3t^4$,
            with all terms on the first strand.  The others have
            higher-strand contributions.  This tests the records of the
            Poincare formula; it does not compute products or higher
            operations in a Koszul complex.
    SeeAlso
        PoincareSeries
        genericDetPoincare
        strandBettiTable
        koszulPoincareCheck
///

doc ///
    Key
        koszulPoincareCheck
        (koszulPoincareCheck, PoincareSeries)
    Headline
        check the Poincare series of a quadratic determinantal ring against its Hilbert series
    Usage
        koszulPoincareCheck P
    Inputs
        P:PoincareSeries
            a generic, symmetric or Pfaffian series with $r=2$, or a
            symmetric cover with $r=2$ or $3$
    Outputs
        :Boolean
            whether $P_R(t)=1/H_R(-t)$
    Description
        Text
            For $r=2$ the ideal is quadratic and the ring is Koszul, so the
            Poincare series of the residue field is the reciprocal of the
            Hilbert series at $-t$.  The function builds the ring with
            @TO determinantalRing@, computes its Hilbert series (with the
            generators in degree one, so the cover $M_3$ is regraded), and
            compares it with @TO ordinaryPoincareSeries@.  When
            @TO "debugLevel"@ is positive the Hilbert series and the
            predicted series are printed.  Here
            $H_R(q)=\sum_j\dim R_j\,q^j$ uses the degree-one grading, and
            $P_R(t)=\sum_i\dim\operatorname{Tor}^R_i(\mathbf k,\mathbf k)t^i$
            forgets internal degrees.  A generic, symmetric or Pfaffian
            series with $r\ne2$ is refused; covers with $r>3$ cannot be
            constructed by @TO determinantalRing@.
        Example
            koszulPoincareCheck genericDetPoincare(2,3,2)
            koszulPoincareCheck pfaffianPoincare(5,2)
            koszulPoincareCheck symDetCoverPoincare(3,3)
            debugLevel = 1;
            koszulPoincareCheck genericDetPoincare(2,2,2)
        Text
            In the last example $H_R(q)=(1+q)/(1-q)^3$, hence
            $1/H_R(-t)=(1+t)^3/(1-t)$, exactly the printed Poincare
            series.  The verdict compares these rational functions, not
            just finitely many coefficients.  It checks the formula for
            this ring; equality alone is not a general test for Koszulness.
    SeeAlso
        PoincareSeries
        determinantalRing
        verifyPoincareSeries
        ordinaryPoincareSeries
///

doc ///
    Key
        verifyPoincareSeries
        (verifyPoincareSeries, PoincareSeries, ZZ)
    Headline
        compare a Poincare series with a computed resolution of the residue field
    Usage
        verifyPoincareSeries(P, N)
    Inputs
        P:PoincareSeries
            a series from one of the constructors
        N:ZZ
            the length of the resolution
    Outputs
        :Boolean
            whether the Betti table of the resolution of $\mathbf k$ over
            $R$ to length $N$ equals @TO (betti, PoincareSeries, ZZ)@
    Description
        Text
            The ring $R=S/I$ of the recorded family is built over
            $\mathbb Q$ by @TO determinantalRing@, the residue field is
            resolved to length $N$ with @TO freeResolution@, and the graded
            Betti numbers are compared with the table predicted by the
            formula.  This is feasible for small sizes and small $N$ only.
            When @TO "debugLevel"@ is positive both tables and both lists of
            total Betti numbers are printed.
            The comparison is numerical, in every internal degree through
            homological degree $N$; it does not compare Schur characters
            or certify degrees beyond that bound.
        Example
            verifyPoincareSeries(genericDetPoincare(3,3,3), 3)
            debugLevel = 1;
            verifyPoincareSeries(pfaffianPoincare(5,2), 2)
        Text
            The second @TT "true"@ says that the computed and predicted
            tables agree for homological degrees $0,1,2$.  Their totals
            $1,10,50$ occur in internal degrees $0,1,2$, respectively.
            The argument $2$ bounds the residue-field resolution; the
            Pfaffian order is already fixed as $4$ by
            @TT "pfaffianPoincare(5,2)"@.
    SeeAlso
        PoincareSeries
        (betti, PoincareSeries, ZZ)
        determinantalRing
        verifyStrandBettiTable
        koszulPoincareCheck
///

doc ///
    Key
        verifyStrandBettiTable
        (verifyStrandBettiTable, PoincareSeries)
    Headline
        compare the strand Betti table with a computed resolution of the ring
    Usage
        verifyStrandBettiTable P
    Inputs
        P:PoincareSeries
            a series from one of the constructors
    Outputs
        :Boolean
            whether the Betti table of $R$ over $S$ computed by Macaulay2
            equals @TO strandBettiTable@
    Description
        Text
            The ring $R=S/I$ is built over $\mathbb Q$ by
            @TO determinantalRing@, $R$ is resolved over $S$ with
            @TO freeResolution@, and the Betti table of the resolution is
            compared with the table read off the denominator.  When
            @TO "debugLevel"@ is positive both tables are printed.
            For the cover $M_3$, the function first regards the Segre
            presentation as a module over $\operatorname{Sym}(S^2V)$,
            whose generators have degree $2$: that is the base ring of
            the denominator formula, not the larger polynomial ring
            presenting the Segre algebra.
        Example
            verifyStrandBettiTable genericDetPoincare(2,4,2)
            verifyStrandBettiTable symDetPoincare(4,3)
            debugLevel = 1;
            verifyStrandBettiTable genericDetPoincare(3,3,2)
        Text
            The printed tables agree in every rank and internal shift:
            $1,9,16,9,1$ occur in homological degrees $0,\ldots,4$
            and internal degrees $0,2,3,4,6$.  Unlike a bounded
            residue-field check, this compares the complete finite
            resolution over the polynomial ring.  It verifies numerical
            Betti numbers, not the equivariant representation labels.
    SeeAlso
        PoincareSeries
        strandBettiTable
        determinantalRing
        verifyPoincareSeries
///

doc ///
    Key
        EquivariantBettiTally
    Headline
        a Betti table whose entries are characters
    Description
        Text
            An entry records a representation rather than its dimension.
            In the generic case, the character $x_\lambda y_\mu$ means
            $\mathbf S_\lambda(V)\otimes\mathbf S_\mu(W)$, where $V,W$
            are the row and column spaces; in the other families
            $x_\lambda$ means $\mathbf S_\lambda(V)$.  The empty partition
            denotes $\mathbf k$, addition denotes direct sum, and an
            integer coefficient records multiplicity.
        Text
            The tables of @TO expandPoincare@ and
            @TO equivariantStrandBettiTable@ record the characters of Tor
            groups, elements of the Schur ring of a @TO PoincareSeries@,
            rather than their dimensions.  They are instances of this type,
            a subtype of @TO BettiTally@: the keys are the triples
            $(i,\{j\},j)$, an entry is read as @TT "B#(i,{j},j)"@, the row
            of totals sums the characters of a column, and two tables are
            compared with @TO symbol ==@.  Column $i$ is homological degree
            and row $j-i$ records the internal shift.  Core functions
            requiring integer entries, such as @TO poincare@ and
            @TO regularity@, are not applicable to these character entries.
            Entries are displayed as combinations of Schur polynomials,
            with vertical rules separating the columns.  Wide tables are
            split into panels, each with its own row labels.
        Example
            P = genericDetPoincare(3,3,2);
            B = expandPoincare(P, 2)
            class B, instance(B, BettiTally)
            B#(2,{2},2)
            equivariantStrandBettiTable P
        Text
            The selected entry in @TT "B"@ represents
            $$
            \begin{aligned}
            \operatorname{Tor}^R_2(\mathbf k,\mathbf k)_2\cong{}&
            (\mathbf S_{(2)}(V)\otimes\mathbf S_{(1,1)}(W))\\
            &\oplus(\mathbf S_{(1,1)}(V)\otimes\mathbf S_{(2)}(W))\\
            &\oplus(\mathbf S_{(1,1)}(V)\otimes\mathbf S_{(1,1)}(W)),
            \end{aligned}
            $$
            where $R=\operatorname{Sym}(V\otimes W)/I_2$ and
            $\dim V=\dim W=3$; its dimension is $18+18+9=45$.
            The final table instead describes
            $\operatorname{Tor}^S_i(R,\mathbf k)$ over
            $S=\operatorname{Sym}(V\otimes W)$, with total dimensions
            $1,9,16,9,1$.
        Text
            Every product of characters that arises in expanding the series
            is computed in the Schur ring using the
            Littlewood--Richardson rule; in this example the square of the
            character of $\operatorname{Tor}_1$ is the character of
            $(V\otimes W)^{\otimes 2}$, and its difference from
            $\operatorname{Tor}_2$ is the character of the degree two part
            of the ring:
        Example
            c = B#(1,{1},1)
            c^2
            c^2 - B#(2,{2},2)
        Text
            The final character $x_{(2)}y_{(2)}$ identifies
            $R_2\cong\operatorname{Sym}^2V\otimes\operatorname{Sym}^2W$.
            Its dimension is $6\cdot6=36=9^2-45$, matching the degree-two
            relation between the Hilbert and residue-field Poincare
            series of this Koszul ring.
    SeeAlso
        expandPoincare
        equivariantStrandBettiTable
        PoincareSeries
        dimensionSpecialization
///

doc ///
    Key
        hookBasis
        (hookBasis, List, ZZ)
    Headline
        the semistandard hook tabloids of one hook type
    Usage
        hookBasis(type, n)
    Inputs
        type:List
            a hook type @TT "{a, b}"@ of nonnegative integers: arm length
            $a$ and leg length $b$, neither including the corner
        n:ZZ
            the size of the alphabet
    Outputs
        :List
            the semistandard tabloids of hook shape $(a+1, 1^b)$ with
            entries in $\{1, \dots, n\}$, as @TO HookTabloid@s
    Description
        Text
            A hook of type $(a, b)$ has an arm of $a$ boxes to the right
            of its corner and a leg of $b$ boxes below it, so its shape is
            the partition $(a+1, 1^b)$.  The semistandard tabloids of that
            shape, the first column strictly increasing and the row weakly
            increasing from the corner, form the standard basis of the hook
            Schur functor $\mathbf S_{(a+1,1^b)}(\mathbf k^n)$ and index
            the generators of hook type $(a, b)$ of a hook ring.  The list
            is ordered by the column and then by the row tail, the order in
            which the generators of a hook ring are numbered.
        Example
            hookBasis({1,1}, 3)
            #hookBasis({1,1}, 3) == 8
        Text
            Thus the eight tableaux displayed above are a basis of
            $\mathbf S_{(2,1)}(\mathbf k^3)$, not of a space with partition
            $(1,1)$.  More generally the hook-content dimension formula is
            $$\#\operatorname{hookBasis}((a,b),n)
              =\prod_{(i,j)\in(a+1,1^b)}\frac{n+j-i}{h_{ij}},$$
            where $h_{ij}$ is the hook length of the cell $(i,j)$.
            For example, three useful generator spaces are
            $V\oplus\mathbf S_{(2,1)}V\oplus\mathbf S_{(3,1,1)}V$.
        Example
            apply({{0,0},{1,1},{2,2}}, h -> #hookBasis(h, 3))
        Text
            Their dimensions are $3$, $8$ and $6$, respectively, so a hook
            ring with these types on three letters has $17$ variables.
            When $n < b+1$ the column cannot be filled and the list is
            empty.  The type is a pair of integers, not a partition; the
            tabloids of an arbitrary shape are enumerated by
            @TO hookTabloids@.
        Example
            hookBasis({2,3}, 3)
    SeeAlso
        HookTabloid
        tabloid
        hookTabloids
        isHookSemistandard
        pieriMatrix
///

doc ///
    Key
        pieriRowRemove
        (pieriRowRemove, HookTabloid, ZZ)
        (pieriRowRemove, List, ZZ)
        (pieriRowRemove, Filling, ZZ)
        [pieriRowRemove, CoefficientRing]
    Headline
        the row Pieri map P_m on a hook tabloid
    Usage
        pieriRowRemove(T, m)
        pieriRowRemove(T, m, CoefficientRing => K)
    Inputs
        T:HookTabloid
            a hook shaped tabloid with positive integer entries, also
            accepted as a list of rows or as a @TO Filling@
        m:ZZ
            the number of boxes removed from the first row, between $1$
            and the arm length
        CoefficientRing => Ring
            the field of the coefficients, @TO QQ@ or a field of positive
            characteristic
    Outputs
        :HashTable
            the image of $T$ under
            $P_m : \mathbf S_{(a,1^b)}(V) \to \operatorname{Sym}^m(V) \otimes \mathbf S_{(a-m,1^b)}(V)$
            in the semistandard bases: the keys are pairs
            @TT "{beta, U}"@ with @TT "beta"@ the sorted list of the $m$
            letters of a symmetric monomial and @TT "U"@ a semistandard
            hook shaped @TO HookTabloid@, the values are the coefficients
    Description
        Text
            Here $V=\mathbf k^n$, $a$ is the length of the first row
            including its corner, and $b$ is the leg length.  Thus the
            public hook type is $(a-1,b)$ and $1\le m\le a-1$.
            The map is $\mathrm{GL}(V)$-equivariant; its normalization is
            fixed by the following formula.  Write $T_I^\alpha$ for a
            hook with row-tail multiplicities $\alpha$ and ordered column
            $I=(i_0,\ldots,i_b)$, and put
            $C(\alpha,\beta)=\prod_q\binom{\alpha_q}{\beta_q}$.
            Then
            $$\begin{aligned}
              P_m(T_I^\alpha)
              ={}&\sum_{\substack{\beta\le\alpha\\|\beta|=m}}
                 C(\alpha,\beta)e^\beta\otimes T_I^{\alpha-\beta}\\
              &+\frac{1}{a+b-m}
                \sum_{\substack{\beta\le\alpha\\|\beta|=m}}
                \sum_{p=0}^{b}\sum_{q:\beta_q>0}
                (-1)^p\beta_q C(\alpha,\beta)
                e^{\beta-\epsilon_q+\epsilon_{i_p}}
                \otimes T_{(q,I\setminus i_p)}^{\alpha-\beta}.
            \end{aligned}$$
            Here $\epsilon_q$ is the $q$th unit vector and the remaining
            column entries retain their order.  Column permutations carry
            their wedge signs; the resulting hooks are then straightened.
        Example
            pieriRowRemove(tabloid {{1,1,1}}, 1)
            pieriRowRemove(tabloid {{1,2},{3}}, 1)
            pieriRowRemove(tabloid {{1,2,3},{1}}, 2)
        Text
            Brackets denote the Schur-module classes of the boxed
            tableaux.  For example, the first two outputs express
            $$P_1(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right])=3e_1\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right],$$
            $$P_1\!\left(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\right)
              =e_2\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
              +\frac12 e_1\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
              +\frac12 e_3\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right].$$
            A key @TT "{beta,U}"@ represents $e^\beta\otimes U$; the
            list @TT "beta"@ records repeated letters, not an exponent
            vector.  The third output is zero because its column repeats
            the letter $1$.
            Every output hook is straightened to the semistandard basis,
            so a nonsemistandard hook never appears among the keys; the
            input hook need not be semistandard, and a repeated letter in
            its column gives the zero map.
        Example
            pieriRowRemove(tabloid {{2,1,3},{1}}, 1)
            pieriRowRemove(tabloid {{1,2},{1}}, 1)
        Text
            The coefficients lie in the field given by
            @TT "CoefficientRing"@.  The formula divides by $a+b-m$, so a
            characteristic dividing that integer is refused with an error
            naming it.
        Example
            pieriRowRemove(tabloid {{1,1,1}}, 1, CoefficientRing => ZZ/7)
    SeeAlso
        juxtaposeSymToRow
        pieriColumnAdd
        pieriImage
        pieriMatrix
///

doc ///
    Key
        juxtaposeSymToRow
        (juxtaposeSymToRow, HookTabloid, List)
        (juxtaposeSymToRow, List, List)
        (juxtaposeSymToRow, Filling, List)
        [juxtaposeSymToRow, CoefficientRing]
    Headline
        multiply a symmetric monomial into the first row of a hook
    Usage
        juxtaposeSymToRow(T, alpha)
        juxtaposeSymToRow(T, alpha, CoefficientRing => K)
    Inputs
        T:HookTabloid
            a hook shaped tabloid of shape $(c, 1^b)$ with positive
            integer entries, also accepted as a list of rows or as a
            @TO Filling@
        alpha:List
            the letters of a monomial of $\operatorname{Sym}^m(V)$, in any
            order
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        :HashTable
            the product in $\mathbf S_{(c+m,1^b)}(V)$ in the semistandard
            basis: @TO HookTabloid@ keys, coefficient values
    Description
        Text
            With $c$ the first-row length and $b$ the leg length, this is
            the equivariant multiplication
            $$Q_m:\mathbf S_{(c,1^b)}V\otimes\operatorname{Sym}^m V
                    \longrightarrow\mathbf S_{(c+m,1^b)}V,
              \qquad T_I^\gamma\otimes e^\alpha\longmapsto
                    T_I^{\gamma+\alpha}.$$
            The superscripts record row-tail multiplicities; the ordered
            column $I$ is unchanged.  The letters of @TT "alpha"@ are
            appended to the row tail and the resulting hook is
            straightened.  Together with
            @TO pieriRowRemove@ it assembles the hook Pieri injection of
            @TO pieriMatrix@.
        Example
            juxtaposeSymToRow(tabloid {{2},{3}}, {1})
            juxtaposeSymToRow(tabloid {{1,2},{3}}, {1})
            juxtaposeSymToRow(tabloid {{2},{3}}, {1,1})
        Text
            The first output is the identity in $\mathbf S_{(2,1)}V$
            $$Q_1\!\left(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\otimes e_1\right)
              =\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
               -\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right].$$
            In the second output the appended $1$ is already at least
            the corner, so $Q_1(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\otimes e_1)=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$;
            repeating the appended letter
            gives $Q_2(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\otimes e_1^2)=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$.
            A letter smaller than the corner is moved into the column by
            the straightening relation of a single hook, with the signs of
            the wedge.
        Example
            juxtaposeSymToRow(tabloid {{3},{4}}, {1,2})
        Text
            This last output is $Q_2(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}\right]\otimes e_1e_2)
            =\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}\right]-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$.  No choice of alphabet size is needed:
            the map is evaluated on the letters supplied.
    SeeAlso
        pieriRowRemove
        pieriColumnAdd
        pieriMatrix
///

doc ///
    Key
        pieriColumnAdd
        (pieriColumnAdd, HookTabloid, List)
        (pieriColumnAdd, List, List)
        (pieriColumnAdd, Filling, List)
        [pieriColumnAdd, CoefficientRing]
    Headline
        the column Pieri map Q'_m adjoining a wedge to the first column of a hook
    Usage
        pieriColumnAdd(T, K)
        pieriColumnAdd(T, K, CoefficientRing => kk)
    Inputs
        T:HookTabloid
            a hook shaped tabloid of shape $(a, 1^b)$ with positive
            integer entries, also accepted as a list of rows or as a
            @TO Filling@
        K:List
            the letters of a wedge $e_K \in \bigwedge^m V$, in order; a
            repeated letter gives zero
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        :HashTable
            the image in $\mathbf S_{(a,1^{b+m})}(V)$ in the semistandard
            basis: @TO HookTabloid@ keys, coefficient values
    Description
        Text
            Here $a$ includes the corner, $b$ is the leg length and
            $m=\#K$.  This is the equivariant map
            $$Q'_m:\mathbf S_{(a,1^b)}V\otimes\bigwedge^m V
                    \longrightarrow\mathbf S_{(a,1^{b+m})}V.$$
            Its normalization can be defined using the two operators on
            symmetric-exterior tensors
            $$\kappa(e^\alpha\otimes e_I)
               =\sum_{p=0}^{b}(-1)^p e^{\alpha+\epsilon_{i_p}}
                    \otimes e_{I\setminus i_p},\qquad
              d(e^\beta\otimes e_J)
               =\sum_q\beta_q e^{\beta-\epsilon_q}
                    \otimes(e_q\wedge e_J),$$
            where $I=(i_0,\ldots,i_b)$ is the full column and $\alpha$
            records the row tail.  In hook-tableau notation the map is
            $$Q'_m(T\otimes e_K)=\frac{1}{a+b+m}
                    d\bigl(\kappa(T)\wedge e_K\bigr).$$
            The wedge $e_K$ is appended on the right of the exterior
            factor; every resulting hook is expressed in the semistandard
            basis.  This specifies both the scalar and the signs.
        Example
            pieriColumnAdd(tabloid {{1,1},{2}}, {3})
            pieriColumnAdd(tabloid {{1,1,2,3}}, {3,4})
            pieriColumnAdd(tabloid {{2,3,4}}, {3})
        Text
            The first output says
            $$Q'_1\!\left(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes e_3\right)
              =\frac34\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
              \quad\text{in }\mathbf S_{(2,1,1)}V.$$
            In particular, adjoining a wedge is not simply concatenating
            a column with coefficient $1$.
            The formula divides by $a+b+m$, and the evaluator behind it
            also by $a+b$; a characteristic dividing either integer is
            refused with an error naming it.  In characteristic $7$ the
            coefficient $3/4$ above becomes $6$.
        Example
            pieriColumnAdd(tabloid {{1,1},{2}}, {3}, CoefficientRing => ZZ/7)
    SeeAlso
        pieriRowRemove
        juxtaposeSymToRow
        pieriMatrix
///

doc ///
    Key
        pieriMatrix
        (pieriMatrix, List, List, ZZ)
        [pieriMatrix, Content]
        [pieriMatrix, CoefficientRing]
    Headline
        the matrix of the hook Pieri injection in the semistandard bases
    Usage
        M = pieriMatrix(outerType, innerType, n)
        M = pieriMatrix(outerType, innerType, n, Content => w)
    Inputs
        outerType:List
            the type @TT "{arm, leg}"@ $= (a-1, b)$ of the outer hook
            $(a, 1^b)$ of the codomain
        innerType:List
            the type @TT "{arm, leg}"@ $= (c-1, d)$ of the inner hook
            $(c, 1^d)$; both entries are smaller than those of
            @TT "outerType"@
        n:ZZ
            the size of the alphabet
        Content => List
            a content vector, to compute the block of one content only
            (see @TO Content@)
        CoefficientRing => Ring
            the field of the entries, @TO QQ@ or a field of positive
            characteristic
    Outputs
        M:Matrix
            the matrix of the injection
            $\mathbf S_{(c,1^b)} \otimes \mathbf S_{(a,1^d)} \to \mathbf S_{(a,1^b)} \otimes \mathbf S_{(c,1^d)}$
            over the field, with columns indexed by the semistandard pairs
            of the source and rows by those of the target; the two lists
            of pairs @TT "{T1, T2}"@ of @TO HookTabloid@s are stored at
            @TT "M.cache#\"sourceBasis\""@ and
            @TT "M.cache#\"targetBasis\""@
    Description
        Text
            The two types specify the nested hooks $(a,1^b)\supset(c,1^d)$
            of the codomain: $a,c$ are first-row lengths, $b,d$ are leg
            lengths, and $a>c$, $b>d$.  All functors below are evaluated
            on $V=\mathbf k^n$.  With
            $t = a - c$ the injection factors as
            $$
                \mathbf S_{(c,1^b)} \otimes \mathbf S_{(a,1^d)}
                \xrightarrow{\ \mathrm{id} \otimes P_t\ }
                \mathbf S_{(c,1^b)} \otimes \operatorname{Sym}^t(V) \otimes \mathbf S_{(c,1^d)}
                \xrightarrow{\ Q_t \otimes \mathrm{id}\ }
                \mathbf S_{(a,1^b)} \otimes \mathbf S_{(c,1^d)},
            $$
            where the row Pieri map $P_t$ (@TO pieriRowRemove@) removes
            $t$ boxes from the first row of the second factor and $Q_t$
            (@TO juxtaposeSymToRow@) appends the symmetric monomial to the
            first row of the first factor.  The
            map is injective in characteristic zero; its image is the space of quadratic relations
            of this pair of hook generators, and its cokernel is the Schur
            functor of the Frobenius union of the two hooks, onto which
            @TO frobeniusUnionMatrix@ projects.
        Text
            For types $(1,1)$ and $(0,0)$ the first-row lengths are
            $a=2$, $c=1$ and the leg lengths are $b=1$, $d=0$.
            Thus the crossed source is $\bigwedge^2V\otimes
            \operatorname{Sym}^2V$ and the target is
            $\mathbf S_{(2,1)}V\otimes V$.
        Example
            M = pieriMatrix({1,1},{0,0},3);
            (numRows M, numColumns M, rank M)
            rank coker M
        Text
            Over the default field $\mathbb Q$, the output gives the exact
            sequence for $V=\mathbb Q^3$:
            $$0\longrightarrow
              \underbrace{\mathbf S_{(1,1)}V\otimes\mathbf S_{(2)}V}_{\dim=18}
              \xrightarrow{\ M\ }
              \underbrace{\mathbf S_{(2,1)}V\otimes V}_{\dim=24}
              \longrightarrow\underbrace{\mathbf S_{(2,2)}V}_{\dim=6}
              \longrightarrow0.$$
            The Frobenius union nests the one-box hook at cell $(2,2)$
            inside the outer hook; it is not concatenation of partitions.
            The rows and columns are labeled by pairs of tabloids, read
            with @TO pieriSourceBasis@ and @TO pieriTargetBasis@; a column
            is read as a hash table with @TO pieriImage@, and the preimage
            of a target pair with @TO pieriPreimage@.
        Example
            first pieriSourceBasis M
            first pieriTargetBasis M
            pieriImage(M, 0)
        Text
            In particular, the first column is the tensor identity
            $$M\!\left(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]\right)
              =2\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right].$$
            Its right-hand side is zero in the cokernel; the nonzero
            matrix column records a relation, not a semistandard product.
            The injection preserves content, so the matrix is block
            diagonal over the contents; @TT "Content => w"@ builds the
            block of the content $w$ alone, with the pairs of that content
            as its labels.
        Example
            Mw = pieriMatrix({2,2},{1,1},4, Content => {3,2,2,1});
            (numRows Mw, numColumns Mw)
            first pieriTargetBasis Mw
        Text
            Here each label contains three $1$'s, two $2$'s, two $3$'s
            and one $4$.  This is the weight $(3,2,2,1)$ in the tensor
            product, whose total polynomial degree is $8$.
            The entries lie in the field of @TT "CoefficientRing"@.
        Example
            M5 = pieriMatrix({1,1},{0,0},3, CoefficientRing => ZZ/5);
            ring M5
            rank M5
    Caveat
        The size grows quickly with the types and with $n$: the types
        $(3,2)$ and $(1,1)$ on five letters give a matrix of size
        $11200 \times 10080$.  When one or two images are needed,
        @TO pieriImage@ applied to a pair of tabloids computes them
        without the matrix, and @TO Content@ restricts to one block.
    SeeAlso
        pieriImage
        pieriPreimage
        pieriSourceBasis
        pieriTargetBasis
        Content
        frobeniusUnionMatrix
        hookBasis
///

doc ///
    Key
        Content
    Headline
        restrict a Pieri matrix to one content
    Usage
        pieriMatrix(outerType, innerType, n, Content => w)
    Description
        Text
            The hook Pieri injection preserves the content of a pair of
            tabloids, so its matrix is block diagonal over the contents.
            With @TT "Content => w"@, @TO pieriMatrix@ computes the block
            of the content $w$ only: its rows and columns are the pairs
            of that content, in the order of the full matrix.  The content
            is an exact count of every letter, a list of length $n$ whose
            sum is the number of boxes of the two hooks; it is not the
            componentwise bound of the option @TO Weight@ of a hook ring.
        Example
            Mw = pieriMatrix({2,2},{1,1},4, Content => {3,2,2,1});
            (numRows Mw, numColumns Mw)
        Text
            If $v=(v_1,\ldots,v_n)$ is a label's content, a diagonal
            matrix $\operatorname{diag}(t_1,\ldots,t_n)$ acts on it by
            $t_1^{v_1}\cdots t_n^{v_n}$.  Equivariance therefore restricts
            the map to $M_w:(\text{source})_w\to(\text{target})_w$;
            @TT "Mw"@ is precisely that restricted linear map.
            The block is the full matrix restricted to the pairs of that
            content.
        Example
            M = pieriMatrix({2,2},{1,1},4);
            letters = P -> sort flatten apply(P, T -> flatten toList T);
            rows = positions(pieriTargetBasis M, P -> letters P == {1,1,1,2,2,3,3,4});
            cols = positions(pieriSourceBasis M, P -> letters P == {1,1,1,2,2,3,3,4});
            M^rows_cols == Mw
        Text
            The squarefree content $(1, \dots, 1)$ gives the block of the
            Specht modules of the two hook shapes.
    SeeAlso
        pieriMatrix
        Weight
///

doc ///
    Key
        pieriSourceBasis
        (pieriSourceBasis, Matrix)
        pieriTargetBasis
        (pieriTargetBasis, Matrix)
    Headline
        the labeled bases of a Pieri or Frobenius union matrix
    Usage
        pieriSourceBasis M
        pieriTargetBasis M
    Inputs
        M:Matrix
            a matrix returned by @TO pieriMatrix@ or
            @TO frobeniusUnionMatrix@
    Outputs
        :List
            the labels of the columns, or of the rows, of @TT "M"@: pairs
            @TT "{T1, T2}"@ of @TO HookTabloid@s, except that the rows of
            a Frobenius union matrix are labeled by single tabloids of the
            nest shape
    Description
        Text
            The two lists are stored at @TT "M.cache#\"sourceBasis\""@
            and @TT "M.cache#\"targetBasis\""@ when the matrix is built,
            the same two keys that the package @TO PieriMaps@ uses for its
            labeled matrices; these accessors read them and refuse a
            matrix without labels.
        Example
            M = pieriMatrix({1,1},{0,0},3);
            #pieriSourceBasis M == numColumns M
            #pieriTargetBasis M == numRows M
            (pieriSourceBasis M)#0
            (pieriTargetBasis M)#5
        Text
            In this example the first source vector is
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]\in\bigwedge^2V\otimes\operatorname{Sym}^2V$,
            whereas target vector $5$ is
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\in\mathbf S_{(2,1)}V\otimes V$;
            these labels refer to tensor
            products, not concatenated rows.
            The source pairs of a Pieri matrix are semistandard hook
            tabloids of the crossed types, here $(0,1)$ and $(1,0)$, and
            the target pairs those of the nested types $(1,1)$ and $(0,0)$.
        Example
            N = frobeniusUnionMatrix({1,1},{0,0},3);
            (pieriSourceBasis N)#0
            (pieriTargetBasis N)#0
    SeeAlso
        pieriMatrix
        frobeniusUnionMatrix
        pieriImage
///

doc ///
    Key
        pieriImage
        (pieriImage, Matrix, ZZ)
        (pieriImage, Matrix, List)
        (pieriImage, HookTabloid, HookTabloid)
        (pieriImage, List, List)
        (pieriImage, Filling, Filling)
        [pieriImage, CoefficientRing]
    Headline
        the image of one basis element under a Pieri map
    Usage
        pieriImage(M, i)
        pieriImage(M, pair)
        pieriImage(T1, T2)
        pieriImage(T1, T2, CoefficientRing => K)
    Inputs
        M:Matrix
            a matrix returned by @TO pieriMatrix@ or
            @TO frobeniusUnionMatrix@
        i:ZZ
            the index of a column of @TT "M"@, from $0$
        pair:List
            a source label @TT "{T1, T2}"@ of @TT "M"@, each entry a
            @TO HookTabloid@ or a list of rows
        T1:HookTabloid
            the hook tabloid with the longer column, of shape $(c, 1^b)$,
            also accepted as a list of rows or a @TO Filling@
        T2:HookTabloid
            the hook tabloid with the longer row, of shape $(a, 1^d)$ with
            $a > c$ and $b > d$
        CoefficientRing => Ring
            the field of the coefficients for the two tabloid form; the
            matrix forms take the field of the matrix
    Outputs
        :HashTable
            the image expanded in the target basis: the keys are the
            target labels with a nonzero coefficient, pairs
            @TT "{U1, U2}"@ of @TO HookTabloid@s for a Pieri map, and the
            values are the coefficients
    Description
        Text
            The matrix forms read one column of a labeled matrix as a hash
            table, by its index or by its source label.
        Example
            M = pieriMatrix({1,1},{0,0},3);
            b = (pieriSourceBasis M)#4
            pieriImage(M, 4)
            pieriImage(M, b) === pieriImage(M, 4)
        Text
            In the tableau bases, this column is
            $$M(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right])
                =\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]+\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right].$$
            The two keys specify the two target tensors and both
            coefficients are $1$.
            The two tabloid form computes the image of $z_{T_1} \otimes z_{T_2}$
            under the hook Pieri injection
            $\mathbf S_{(c,1^b)} \otimes \mathbf S_{(a,1^d)} \to \mathbf S_{(a,1^b)} \otimes \mathbf S_{(c,1^d)}$
            directly: the row Pieri map
            $P_{a-c}$ (@TO pieriRowRemove@) is applied to $T_2$ and each
            symmetric monomial it produces is multiplied into the row of
            $T_1$ (@TO juxtaposeSymToRow@).  No matrix is built, so this
            is the way to compute a few images of a large Pieri map.  The
            result agrees with the corresponding column of the matrix.
        Example
            pieriImage(b#0, b#1)
            pieriImage(b#0, b#1) === pieriImage(M, b)
            pieriImage(tabloid {{2},{3}}, tabloid {{1,2}})
        Text
            The last output is
            $$M(\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right])=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]
                 -\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]+\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right].$$
            This is an equality in the target tensor product; setting the
            image equal to zero gives its quadratic hook-algebra relation.
            The step $a - c$ is read from the two shapes, so the first
            tabloid must have the longer column and the second the longer
            row.  The coefficients of the tabloid form lie in the field
            given by @TT "CoefficientRing"@.
        Example
            pieriImage(tabloid {{2},{3}}, tabloid {{1,1,2}}, CoefficientRing => ZZ/7)
        Text
            On a Frobenius union matrix the keys are the single tabloids
            of the nest shape that label its rows.
        Example
            N = frobeniusUnionMatrix({1,1},{0,0},3);
            pieriImage(N, 1)
    SeeAlso
        pieriMatrix
        pieriPreimage
        pieriSourceBasis
        pieriTargetBasis
        frobeniusUnionMatrix
        pieriRowRemove
        juxtaposeSymToRow
///

doc ///
    Key
        pieriPreimage
        (pieriPreimage, Matrix, List)
    Headline
        the Pieri preimage of a target basis pair
    Usage
        pieriPreimage(M, pair)
    Inputs
        M:Matrix
            a matrix returned by @TO pieriMatrix@
        pair:List
            a target label @TT "{T1, T2}"@ of @TT "M"@, each entry a
            @TO HookTabloid@ or a list of rows
    Outputs
        :HashTable
            the coefficients $x$ on the source pairs with
            $v = M x + w$, where $v$ is the unit vector of the target pair
            and $w$ lies in the span of the nested semistandard target
            pairs: the keys are pairs @TT "{S1, S2}"@ of
            @TO HookTabloid@s, the values the coefficients
    Description
        Text
            The target pairs $(T_1, T_2)$ whose nested tabloid
            @TT "tabloid(T1, T2)"@ is semistandard span a complement of
            the image of the Pieri injection.  A
            target pair therefore decomposes uniquely as
            $v = M x + w$ with $w$ in that complement; $w$ is the
            straightening of the nested tabloid in the cokernel and $x$ is
            its Pieri preimage, the value returned here.  The
            complement matrix is inverted once and kept with @TT "M"@, so
            further preimages from the same matrix are read off directly.
        Example
            M = pieriMatrix({1,1},{0,0},3);
            src = pieriSourceBasis M; tgt = pieriTargetBasis M;
            t = tgt#3
            isHookSemistandard tabloid(t#0, t#1)
            x = pieriPreimage(M, t)
        Text
            Here $v=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]$ and the returned source vector is
            $x=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$.  Since
            $$Mx=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]+\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right],\qquad
              v=Mx-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right],$$
            the semistandard remainder is $w=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$.
            Equivalently, the nested tabloids satisfy
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$ in the hook algebra.  The function returns
            $x$, not $w$; this is a preimage modulo the chosen complement,
            even when $v$ itself is not in $\operatorname{im}M$.
            The remainder $v - M x$ is supported on nested semistandard
            pairs.
        Example
            xvec = matrix apply(src, s -> {if x#?s then x#s else 0_QQ});
            w = (id_(target M))_{3} - M * xvec;
            supp = positions(flatten entries w, c -> c != 0)
            all(supp, j -> isHookSemistandard tabloid(tgt#j#0, tgt#j#1))
        Text
            A nested semistandard pair lies in the complement, so its
            preimage is zero.
        Example
            u = tgt#1
            isHookSemistandard tabloid(u#0, u#1)
            pieriPreimage(M, u)
        Text
            Now $u=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$ nests to $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$, so its
            decomposition is $u=M0+u$ and the returned hash table is empty.
    SeeAlso
        pieriMatrix
        pieriImage
        pieriTargetBasis
        isHookSemistandard
///

doc ///
    Key
        hookSymbolicForm
        (hookSymbolicForm, Matrix)
    Headline
        a Pieri matrix as a hash table from source pairs to their images
    Usage
        S = hookSymbolicForm M
    Inputs
        M:Matrix
            a labeled Pieri matrix from @TO pieriMatrix@
    Outputs
        S:HashTable
            from each source pair $\{T_1,T_2\}$ of tabloids to the hash
            table from target pairs to coefficients that @TO pieriImage@
            returns for its column
    Description
        Text
            The symbolic form is the whole map read in tableaux: the key set
            is @TO pieriSourceBasis@ of the matrix and the value at a pair is
            @TO pieriImage@ of that pair, so a column is @TT "S#{T1, T2}"@ and
            a coefficient is @TT "S#{T1, T2}#{U1, U2}"@.
        Example
            M = pieriMatrix({1,1},{0,0},3);
            S = hookSymbolicForm M;
            #S == numColumns M
            p = first pieriSourceBasis M
            S#p
            S#p === pieriImage(M, 0)
        Text
            The displayed value of @TT "S#p"@ is the same map as
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]\mapsto2\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]\otimes\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\end{array}\right]$.
            In general, if $M(e_j)=\sum_i m_{ij}f_i$, the outer table is
            indexed by $e_j$ and its value is the inner table
            $\{f_i\mathrel{\Rightarrow}m_{ij}:m_{ij}\ne0\}$.
    SeeAlso
        pieriMatrix
        pieriImage
        pieriSourceBasis
        pieriTargetBasis
///

doc ///
    Key
        hookStraighten
        (hookStraighten, HookTabloid)
        (hookStraighten, List)
        (hookStraighten, Filling)
        (hookStraighten, List, List)
        (hookStraighten, HashTable)
        [hookStraighten, Strategy]
        [hookStraighten, CoefficientRing]
    Headline
        the normal form of a hook tabloid in the semistandard basis
    Usage
        hookStraighten T
        hookStraighten(rowTail, colFull)
        hookStraighten H
        hookStraighten(T, Strategy => s, CoefficientRing => K, Progress => n)
    Inputs
        T:HookTabloid
            or a list of rows, or a @TO Filling@; the entries are positive
            integers, the letters of the alphabet
        rowTail:List
            the arm of a single hook, left to right
        colFull:List
            the column of that hook, the corner first
        H:HashTable
            a linear combination of hook tabloids: the keys are tabloids
            (in any of the three forms) and the values their coefficients
        Strategy => String
            @TT "\"Automatic\""@ (the default) runs the straightening
            algorithm; @TT "\"Rank2\""@ runs the
            rank two engine alone, on a tabloid of Frobenius rank two;
            @TT "\"Cokernel\""@ computes the normal form by linear algebra
            on the Pieri relations of the weight space, with no
            straightening step at all
        CoefficientRing => Ring
            the field of the coefficients, @TO QQ@ or a field of positive
            characteristic such as @TT "ZZ/7"@
        Progress => ZZ
            print worklist counters at intervals of $n$ iterations
            (see @TO Progress@ for the rank-dependent counts)
    Outputs
        :HashTable
            the expansion $T = \sum_U c_U\,U$ in the semistandard hook
            tabloids of the shape of $T$: the keys are the semistandard
            @TO HookTabloid@s $U$ and the values the coefficients $c_U$ in
            the chosen field; a semistandard $T$ gives @TT "{T => 1}"@ and
            a tabloid that is zero in the hook algebra gives an empty table
    Description
        Text
            Let $\lambda$ have diagonal hook shapes
            $H_i=(a_i+1,1^{b_i})$.  A hook tabloid represents a tensor in
            $\bigotimes_i\mathbf S_{H_i}V$, or its product in the hook
            algebra.  Quotienting by the crossed-hook Pieri relations
            gives $\mathbf S_\lambda V$ in characteristic zero.
            The normal form is the unique expansion
            $$\operatorname{NF}_\lambda(T)
              =\sum_{U\in\operatorname{SSYT}(\lambda)}c_U U,
              \qquad T-\operatorname{NF}_\lambda(T)\in\mathcal R_\lambda,$$
            where $\mathcal R_\lambda$ is the span of those relations.
            Here semistandard means weakly increasing along every row
            and strictly increasing down every column of the whole
            diagram, not merely within each diagonal hook.
        Text
            Every diagonal hook is first straightened by the single-hook
            relation; then, as long as some consecutive pair of diagonal
            hooks is not semistandard, the south-east-most such pair is
            rewritten by a row or column Pieri relation and the terms
            are re-embedded in the tabloid.  After the individual hooks
            are normalized, every term of a pair step is strictly smaller
            than its input in the @TO runningCompare@ order, so the
            process terminates: a step preserves
            the shape and the content, only finitely many hook tabloids
            have a given shape and content, and a strictly descending chain
            in a finite poset is finite.  @TO straighteningStep@ performs
            one step, and @TO verifyTriangularity@ checks the descent on
            all tabloids of a shape and alphabet whose diagonal hooks are
            semistandard.  Each worklist rewrites the largest pending
            tabloid first, so a tabloid is rewritten at most once within
            that worklist.
        Text
            The following tabloid has two semistandard diagonal hooks
            whose product is not semistandard; its normal form has two
            terms.
        Example
            T = tabloid {{1,2,3},{2,1},{3}}
            H = hookStraighten T
            keys H
            all(keys H, isHookSemistandard)
        Text
            The hash table is the equality in the hook algebra
            $$\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
              =-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]
               -\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right].$$
            Both right-hand tableaux have weak rows and strict columns.
            The keys are @TO HookTabloid@s, so a coefficient is read with
            @TT "H#U"@, and @TT "H#?U"@ tells whether a tabloid occurs; a
            tabloid given as rows is looked up after @TO tabloid@.
        Example
            U = tabloid {{1,1,3},{2,2},{3}};
            H#U
            H#?(tabloid {{1,2,3},{2,3},{3}})
        Text
            The two answers are $-1$ and @TO false@: the coefficient of
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$ is $-1$, whereas $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$ does not occur.
            Brackets denote the classes of the boxed tableaux modulo
            the crossed-hook Pieri relations.
            A tabloid may be given as its rows, as a @TO Filling@ of
            @TO SchurFunctors@, or, for a single hook, as its row tail
            and its full column.  A tabloid of Frobenius rank one is
            straightened by the single-hook relation alone, and a tabloid
            with a repeated entry in the column of a hook is zero.
        Example
            hookStraighten {{3,1},{2}}
            hookStraighten({1}, {3,2})
            hookStraighten {{1,1},{1,1}}
        Text
            The first two calls describe the same hook and give
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]=\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$.  The third is zero: a repeated letter
            in a hook column is zero by alternation.
            In rank three and higher, the tabloid is rewritten one pair of
            consecutive diagonal hooks at a time.  Here the two hooks of
            the second and third diagonal are exchanged by one signed
            rewrite.
        Example
            hookStraighten {{1,2,3},{4,5,9},{7,8,6}}
            #hookStraighten {{4,5,6},{7,1,2},{8}}
        Text
            The first output is $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}9\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}7\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}8\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}\right]=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}7\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}8\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}9\phantom{|}}\end{array}\right]$.
            The second call counts the $14$ terms of a different normal
            form, without printing all of them.
            The next example illustrates rational coefficients in this
            hook-tableau basis.
        Example
            hookStraighten {{1,2,6},{4,3,5}}
        Text
            Its five entries express
            $$\begin{aligned}\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\end{array}\right]={}&-\tfrac12\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}\right]
              -\tfrac12\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}\right]-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}\right]\\
              &+\tfrac12\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}\right]+\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}5\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}6\phantom{|}}\end{array}\right].\end{aligned}$$
            These are identities in the hook model; the chosen basis
            normalization need not match another realization of the same
            Schur functor.
            A hash table from tabloids to coefficients is straightened by
            linearity; the normal form of a normal form is itself.
        Example
            hookStraighten hashTable {T => 2, tabloid {{1,2},{3}} => 1}
            hookStraighten H === H
        Text
            Thus $\operatorname{NF}(\sum_T a_TT)=\sum_Ta_T
            \operatorname{NF}(T)$ and $\operatorname{NF}^2=
            \operatorname{NF}$; a linear combination may include several
            shapes, each straightened in its own component.
            The three strategies compute the same normal form.  The rank
            two engine is the one the algorithm applies to each pair of
            hooks; the cokernel strategy builds the space spanned by the
            hook tabloids of the shape and content of $T$, the relations
            given by the crossed-hook Pieri injections of every pair of
            diagonal hooks, and reduces $T$
            modulo an echelon basis of the relation space, in which the
            semistandard tabloids are a complement.  It is the reference
            against which the algorithm is verified by
            @TO verifyStraighteningAgainstCokernel@, and it errors when the
            semistandard tabloids are not a complement, which happens in
            small positive characteristic.
        Example
            T2 = {{4,5,6},{7,1,2},{8}};
            hookStraighten(T2, Strategy => "Rank2") === hookStraighten T2
            hookStraighten(T2, Strategy => "Cokernel") === hookStraighten T2
        Text
            Over a field of positive characteristic the coefficients are
            reduced modulo the characteristic.  The steps of the algorithm
            divide by integers bounded by the number of boxes of the shape,
            and the elimination systems used to solve a local relation may
            become singular below that bound; the computation then fails
            with an error naming the integer or the system.
        Example
            H11 = hookStraighten(T2, CoefficientRing => ZZ/11);
            all(keys H11, U -> H11#U == sub((hookStraighten T2)#U, ZZ/11))
    Caveat
        Intermediate expressions can have many more terms than the final
        normal form.  Time and memory use depend on the number and cost of
        the local rewrites, not only on the size of the answer.  Use
        @TO Progress@ to inspect worklist activity during a long run.
        The cokernel strategy builds the relations of the whole weight
        space on the first call for a shape, content and field, and caches
        the resulting linear-algebra data for the session.
    SeeAlso
        straighteningStep
        verifyTriangularity
        verifyStraighteningAgainstCokernel
        isHookSemistandard
        tabloid
        Progress
///

doc ///
    Key
        Progress
        [hookStraighten, Progress]
    Headline
        print the progress of a long straightening
    Usage
        hookStraighten(T, Progress => n)
    Description
        Text
            With @TT "Progress => n"@ for a positive integer $n$, each
            straightening worklist prints counters at intervals of $n$
            iterations.  In rank two, @TT "rewrites"@ counts
            nonsemistandard tabloids selected for a local rewrite.  In
            higher rank, @TT "pair rewrites"@ counts full tabloids
            selected by the outer worklist, including those already
            semistandard; its local rank two computations may also print
            their own @TT "rewrites"@ lines.
        Text
            Counters restart for separate worklists, so they are not a
            cumulative rewrite count for the whole call.  In each line,
            @TT "pending"@ counts queued entries, which may include entries
            whose coefficients have canceled, and @TT "done"@ counts the
            nonzero semistandard terms currently collected in that
            worklist.  These are activity indicators, not estimates of the
            time remaining.
        Text
            The default $0$ prints nothing.  Single-hook normalization and
            @TT "Strategy => \"Cokernel\""@ do not print these worklist
            counters.
        Example
            H = hookStraighten({{4,5,6},{7,1,2},{8,3}}, Progress => 200);
            #H
    SeeAlso
        hookStraighten
///

doc ///
    Key
        straighteningStep
        (straighteningStep, HookTabloid)
        (straighteningStep, List)
        (straighteningStep, Filling)
        [straighteningStep, Obstruction]
        [straighteningStep, EngineStep]
        [straighteningStep, CoefficientRing]
    Headline
        one rewrite of the straightening algorithm on a hook tabloid
    Usage
        straighteningStep T
        straighteningStep(T, Obstruction => kind)
        straighteningStep(T, EngineStep => true)
    Inputs
        T:HookTabloid
            of any Frobenius rank, or a list of rows, or a @TO Filling@
        Obstruction => String
            @TT "\"Automatic\""@ (the default), @TT "\"row\""@ or
            @TT "\"column\""@: which obstruction of the rewritten pair to
            resolve when both are present
        EngineStep => Boolean
            when @TT "true"@, replace the local move on the pair by the
            complete rank two normal form of that pair, which is the step
            the package engine takes in rank three and higher
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        :HashTable
            the relation $T=\sum_U c_U\,U$ after exactly one step, keyed by
            @TO HookTabloid@s; a semistandard $T$ gives @TT "{T => 1}"@ and
            a tabloid that is zero gives an empty table
    Description
        Text
            @TO hookStraighten@ runs the straightening algorithm to the
            end.  This function performs a single step of it, so that
            the local Pieri relations can be
            examined one at a time.  The step is chosen exactly as in the
            algorithm:
        Text
            @UL {
                "if some diagonal hook of the tabloid is not semistandard, the single-hook relation is applied to the leftmost such hook (its column is sorted, and a row tail entry below the corner is exchanged with the column);",
                "otherwise, in rank two, a failed row or column inequality is resolved by a Pieri relation (a row obstruction takes priority when both are present);",
                "in rank three and higher, the same rank two step is applied to the south-east-most consecutive pair of diagonal hooks that is not semistandard, and the result is re-embedded in the tabloid."
            }@
        Text
            The following tabloid is rewritten in one
            step as a combination of two tabloids that are strictly smaller
            in the running order (see @TO verifyTriangularity@); since the
            step lands on semistandard tabloids, it is already the normal
            form.
        Example
            T = tabloid {{1,2,3},{2,1},{3}}
            straighteningStep T
            straighteningStep T === hookStraighten T
        Text
            In the hook algebra this is
            $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$.
            A single step need not land in the semistandard basis:
        Example
            U = tabloid {{1,2,2},{2,1,1},{3,3}};
            oneStep = straighteningStep U
            any(keys oneStep, isHookSemistandard)
            hookStraighten oneStep === hookStraighten U
        Text
            Here the local identity is
            $$\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]=-2\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]+\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right].$$
            Neither term on the right is semistandard.  Further
            straightening gives the single term $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$; the last
            command checks that straightening the displayed linear
            combination gives that same normal form.
            When both a row and a column obstruction are present, the
            option @TT "Obstruction"@ selects the relation.  In the next
            tableau, row $2$ decreases from $2$ to $1$, while column $2$
            has equal entries $1,1$; the explicit inequalities are described under
            @TO straighteningObstruction@.  Both moves represent the same
            class, although their immediate expansions can differ.
        Example
            P = {{1,1,2,3},{2,1,1,1},{3,4}};
            straighteningStep P
            straighteningStep(P, Obstruction => "column")
        Text
            Iterating the step until every key is semistandard reproduces
            the normal form; @TO hookStraighten@ does this with a worklist
            that rewrites the largest pending tabloid first.  In rank three
            the step rewrites one pair of hooks.
        Example
            T3 = {{4,5,6},{1,2,3},{7,8,9}};
            straighteningStep T3
            hookStraighten T3
        Text
            A tabloid whose hooks are not semistandard is first rewritten
            by the single-hook relation; the column of the outer hook below
            is sorted at the cost of a sign.
        Example
            straighteningStep {{2,1,3},{1,2},{3}}
        Text
            The last equality is $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$: sorting the
            outer column $2,1,3$ makes one transposition.  This preliminary
            normalization is different from a two-hook Pieri move.
    SeeAlso
        hookStraighten
        straighteningObstruction
        isStepTriangular
        verifyTriangularity
        Obstruction
        EngineStep
///

doc ///
    Key
        straighteningObstruction
        (straighteningObstruction, HookTabloid)
        (straighteningObstruction, List)
        (straighteningObstruction, Filling)
        [straighteningObstruction, Obstruction]
        [straighteningObstruction, CoefficientRing]
    Headline
        the obstruction that the next straightening step resolves, with the data of the step
    Usage
        D = straighteningObstruction T
        D = straighteningObstruction(T, Obstruction => kind)
    Inputs
        T:HookTabloid
            of Frobenius rank at least two whose diagonal hooks are
            semistandard, or a list of rows, or a @TO Filling@
        Obstruction => String
            @TT "\"Automatic\""@, @TT "\"row\""@ or @TT "\"column\""@, as
            in @TO straighteningStep@
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        D:HashTable
            a diagnostic record with string keys: the obstruction data of
            the pair of hooks that the algorithm rewrites next, and
            everything the rank two step computes on it
    Description
        Text
            In rank two the record concerns the tabloid itself; in higher
            rank it concerns the south-east-most pair of consecutive
            diagonal hooks that is not semistandard, whose index is stored
            under @TT "pairIndex"@ and whose rank two tabloid is stored
            under @TT "subTabloid"@.  The pair index is zero based: $k$
            means diagonal hooks $k+1$ and $k+2$.
            In the extracted rank two tableau, using one-based cell
            indices, a row obstruction is the first $i\ge2$ with
            $T_{i,1}>T_{i,2}$, and a column obstruction is the first
            $j\ge2$ with $T_{1,j}\ge T_{2,j}$; only existing cells are
            compared.  The key @TT "obstructionType"@ is
            @TT "\"row\""@, @TT "\"column\""@ or @TT "\"none\""@, and
            @TT "obstructionIndex"@ is $i-1$ or $j-1$, respectively.
            The name records the failed inequality, not the direction
            in which entries are moved: a row obstruction uses a column
            Pieri move, and a column obstruction uses a row Pieri move.
        Text
            The obstruction data includes short and descriptive names.
            For a row obstruction: @TT "L0"@ (also
            @TT "movedColumnTail"@), @TT "I0"@ (@TT "bumpedColumnSegment"@),
            @TT "R0"@, the @TT "pivot"@, and the triple @TT "T1"@,
            @TT "T2"@, @TT "T3"@ (also @TT "leftHook"@, @TT "middleHook"@,
            @TT "thirdFactor"@, the third factor being a wedge).  For a
            column obstruction: @TT "tau0"@ (@TT "movedRowTail"@),
            @TT "rho"@ (@TT "fixedRightRow"@), @TT "sigma"@
            (@TT "bumpedRowTail"@), and the triple @TT "T1"@, @TT "T2"@,
            @TT "T3"@, the third factor being a row.  A hook in these
            entries is a pair @TT "{rowTail, colFull}"@ of its arm and its
            full column, denoted $T^{\mathrm{rowTail}}_{\mathrm{colFull}}$;
            @TO tabloid@ of the pair draws it.
        Text
            The data of the step follows: @TT "rawImage"@, the image of
            the Pieri map on the triple, which contains the sub-tabloid
            with coefficient @TT "origCoeffInRawImage"@;
            @TT "normalizedImage"@, the same image divided by that
            coefficient; @TT "rewrite"@, the relation on the pair after
            solving for the sub-tabloid; @TT "step"@, the same relation
            re-embedded in the full tabloid, which is the value of
            @TO straighteningStep@; @TT "input"@; @TT "localObstructionData"@,
            the obstruction record of the engine; and @TT "internal"@, the
            elimination matrices and intermediate lists for inspecting
            the local calculation.
            The tables are keyed by @TO HookTabloid@s.
        Text
            Algebraically, if the raw relation is
            $0=c_TT+\sum_{U\ne T}c_UU$, then
            @TT "origCoeffInRawImage"@ is $c_T$,
            @TT "normalizedImage"@ divides every coefficient by $c_T$,
            and @TT "rewrite"@ is
            $T=-\sum_{U\ne T}(c_U/c_T)U$.
            In the next example the failed row inequality is $3>2$.
        Example
            D = straighteningObstruction {{1,1,2,3},{3,2,3,4},{4,3}};
            D#"obstructionType"
            (D#"L0", D#"I0", D#"R0")
            (D#"T1", D#"T2", D#"T3")
            D#"rewrite"
            D#"step" === straighteningStep {{1,1,2,3},{3,2,3,4},{4,3}}
        Text
            The moved tail is $L_0=(3,4)$, its pivot is $3$, the bumped
            inner-column segment is $I_0=(3)$, and $R_0=(2)$ is left.
            Thus the diagnostic describes which letters enter the Pieri
            relation, while @TT "rewrite"@ gives the solved equality.
            The following examples give a column obstruction and a rank
            three tabloid whose second pair of hooks is rewritten.
        Example
            C = straighteningObstruction {{1,3,4},{2,2,5},{3}};
            (C#"obstructionType", C#"tau0", C#"rho", C#"sigma")
            (straighteningObstruction {{1,2,3},{4,5,9},{7,8,6}})#"pairIndex"
        Text
            The column inequality fails at $T_{1,2}=3\ge2=T_{2,2}$.
            Its moved row tail is $\tau_0=(3,4)$, the fixed prefix
            $\rho$ is empty and the bumped tail is $\sigma=(5)$.
            The rank three call returns $1$, meaning hooks $2$ and $3$.
            A semistandard tabloid has no obstruction: the record then has
            @TT "obstructionType"@ equal to @TT "\"none\""@, the keys
            @TT "input"@, @TT "pairIndex"@ (@TT "null"@) and @TT "step"@
            (the identity), and a @TT "message"@.
        Example
            peek straighteningObstruction {{1,2,3},{2,3},{3}}
    SeeAlso
        straighteningStep
        hookStraighten
        Obstruction
///

doc ///
    Key
        isStepTriangular
        (isStepTriangular, HookTabloid)
        (isStepTriangular, List)
        (isStepTriangular, Filling)
        [isStepTriangular, Obstruction]
        [isStepTriangular, EngineStep]
        [isStepTriangular, CoefficientRing]
    Headline
        whether one straightening step decreases the running order
    Usage
        isStepTriangular T
    Inputs
        T:HookTabloid
            or a list of rows, or a @TO Filling@
        Obstruction => String
            passed to @TO straighteningStep@
        EngineStep => Boolean
            passed to @TO straighteningStep@
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        :Boolean
            whether the step preserves content and, for a nontrivial
            two-hook move, every output term is strictly smaller than $T$
            in the @TO runningCompare@ order; a semistandard input or a
            preliminary single-hook normalization is accepted
    Description
        Text
            This is the verdict of the report @TO verifyTriangularity@
            computes on one tabloid.  When a diagonal hook of the input is
            not semistandard, the step is a single-hook normalization.
            This preparatory step is accepted separately, without a
            strict-descent test, because it puts that hook directly into
            its semistandard basis.  Pair moves are tested by
            $$\operatorname{step}(T)=\sum_Uc_UU
               \quad\Longrightarrow\quad
               c_U\ne0\ \Rightarrow\ U\prec T
               \text{ and }\operatorname{content}(U)=\operatorname{content}(T).$$
        Example
            isStepTriangular {{1,3},{2,2}}
            isStepTriangular {{1,5,6},{4,2,3},{7,8,9}}
            isStepTriangular tabloid {{1,2,3},{2,3},{3}}
        Text
            For the first call the relation is $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$,
            and $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]\prec\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]$.
            The last call returns @TO true@ because the input is already
            semistandard, rather than requiring it to be smaller than itself.
    SeeAlso
        verifyTriangularity
        straighteningStep
        runningCompare
///

doc ///
    Key
        verifyTriangularity
        (verifyTriangularity, List, ZZ)
        (verifyTriangularity, List, List)
        (verifyTriangularity, HookTabloid)
        (verifyTriangularity, List)
        (verifyTriangularity, Filling)
        [verifyTriangularity, Obstruction]
        [verifyTriangularity, EngineStep]
        [verifyTriangularity, CoefficientRing]
    Headline
        verify that the straightening algorithm terminates on a shape and alphabet
    Usage
        R = verifyTriangularity(lambda, n)
        R = verifyTriangularity(lambda, content)
        R = verifyTriangularity T
    Inputs
        lambda:List
            a partition
        n:ZZ
            the alphabet size: every tabloid of shape @TT "lambda"@
            with semistandard diagonal hooks and entries in $1,\ldots,n$
            is checked
        content:List
            a content vector (the number of occurrences of each letter):
            only the tabloids of that content are checked
        T:HookTabloid
            one tabloid, or a list of rows, or a @TO Filling@: only the
            step on $T$ is checked
        Obstruction => String
            passed to @TO straighteningStep@
        EngineStep => Boolean
            passed to @TO straighteningStep@
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        R:VerificationReport
            of kind @TT "\"Triangularity\""@ for a shape, with the counts
            of the sweep and the list of violations, or of kind
            @TT "\"StepTriangularity\""@ for one tabloid, with the step and
            the comparison of each of its terms with the input; the verdict
            is @TT "R#\"passed\""@
    Description
        Text
            A step of the straightening algorithm preserves the shape and
            the content of a tabloid.  The tabloids of a fixed shape and
            content whose diagonal hooks are semistandard form a finite
            set $S$.  If every term of the
            step on every nonsemistandard tabloid of $S$ is strictly
            smaller than that tabloid in the running partial order
            (see @TO runningCompare@), then
            every branch of successive rewrites is a strictly descending
            chain in the finite poset $S$.  Each step has finitely many
            terms, so straightening ends in a combination of semistandard
            tabloids.  Thus a passing sweep proves that
            @TO hookStraighten@ terminates on every tabloid of $S$ and on
            every linear combination of them, without running the
            algorithm to the end on any of them; the worklist of the
            engine, which rewrites the largest pending tabloid first, then
            rewrites each tabloid at most once within that worklist.
            A sweep over an alphabet
            covers every content on that alphabet at once.
        Text
            The check applies @TO straighteningStep@ to every tabloid of
            @TO hookTabloids@ that is not semistandard, compares each term
            with the input in the running order, and also records terms of
            a different content or terms that are incomparable with the
            input, since the order is only partial.  The certificate is
            restricted to the specified shape and alphabet or content.
            With @TT "EngineStep => true"@ it certifies the step rule of
            the package engine rather than the single local move.
        Example
            R = verifyTriangularity({3,2}, 3)
            R#"passed"
        Text
            This report checks $45$ products of diagonal-hook basis
            vectors: $15$ are semistandard, and the $30$ other inputs
            produce $40$ nonzero terms in total, all strictly smaller
            than their inputs.  Seven steps are zero.  Thus descent is
            checked on the whole specified finite set, not inferred from
            one successful straightening.
            The rows of the report are @TT "shape"@, @TT "restriction"@
            (@TT "\"alphabet\""@ or @TT "\"content\""@) with
            @TT "restrictionValue"@, @TT "obstruction"@, @TT "engine"@,
            @TT "tabloids"@, @TT "semistandard"@, @TT "steps"@,
            @TT "terms"@, @TT "maxTerms"@, @TT "zeroSteps"@,
            @TT "incomparable"@, @TT "contentViolations"@,
            @TT "violations"@ (the pairs of tabloid and term, as
            @TO HookTabloid@s), @TT "firstViolation"@ (a triple with the
            reason, or @TT "null"@), @TT "seconds"@, @TT "passed"@ and
            @TT "conclusion"@.  A content restricts the sweep to one
            weight space: the shape $(3,3,3)$ on four letters has $2880$
            hook tabloids, of which the content $(3,2,2,2)$ selects $105$.
        Example
            R = verifyTriangularity({3,3,3}, {3,2,2,2});
            (R#"tabloids", R#"steps", R#"terms", R#"passed")
            #hookTabloids({3,3,3}, 4)
        Text
            The restricted report is $(105,104,304,\mathrm{true})$:
            $105$ inputs of content $(3,2,2,2)$, $104$ nonsemistandard
            inputs and $304$ terms checked.  The full alphabet has $2880$
            inputs; the restricted computation checks that entire weight
            space.
            On one tabloid the report shows the step and, for each of its
            terms, the coefficient, the term and its comparison with the
            input (@TT "symbol <"@ when the term is strictly smaller); the
            rows are @TT "input"@, @TT "semistandard"@,
            @TT "hookRelation"@, @TT "step"@, @TT "terms"@,
            @TT "failures"@ (the terms whose comparison is not
            @TT "symbol <"@), @TT "contentPreserved"@, @TT "passed"@ and
            @TT "conclusion"@.  When a diagonal hook of the input is not
            semistandard the step is the single-hook relation; the report
            marks it under @TT "hookRelation"@ and accepts this finite
            preliminary normalization separately, without requiring a
            strict running-order comparison.
        Example
            S = verifyTriangularity {{1,3},{2,2}}
            S#"terms"
            (verifyTriangularity {{4,5,6},{1,2,3},{7,8,9}})#"hookRelation"
        Text
            The first report records the coefficient, tableau and order
            relation for $\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}\right]=-\left[\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}\right]$.  A @TT "<"@ in its
            @TT "terms"@ row means the output tableau is smaller than
            the input, not that its coefficient is negative.
    SeeAlso
        isStepTriangular
        straighteningStep
        hookTabloids
        runningCompare
        hookStraighten
        VerificationReport
///

doc ///
    Key
        verifyStraighteningAgainstCokernel
        (verifyStraighteningAgainstCokernel, List, ZZ)
        (verifyStraighteningAgainstCokernel, List, List)
        [verifyStraighteningAgainstCokernel, CoefficientRing]
    Headline
        compare the straightening algorithm with the Pieri cokernel on a whole shape
    Usage
        R = verifyStraighteningAgainstCokernel(lambda, n)
        R = verifyStraighteningAgainstCokernel(lambda, content)
    Inputs
        lambda:List
            a partition
        n:ZZ
            the alphabet size: every content on $n$ letters is checked
        content:List
            a single content vector
        CoefficientRing => Ring
            the field of the coefficients
    Outputs
        R:VerificationReport
            of kind @TT "\"Cokernel\""@, with the rows @TT "shape"@,
            @TT "restriction"@ and @TT "restrictionValue"@,
            @TT "tabloids"@ and @TT "relations"@ (counts),
            @TT "agreements"@ (a count), @TT "disagreements"@ (a list of
            triples: the tabloid, the normal form of the algorithm and the
            normal form of the cokernel), @TT "seconds"@, @TT "passed"@
            and @TT "conclusion"@
    Description
        Text
            For every hook tabloid of the shape (of every content on the
            alphabet, or of the given content) the normal form produced by
            the straightening algorithm is compared with the normal form
            computed by linear algebra on the Pieri relations of the weight
            space, that is @TO hookStraighten@ with
            @TT "Strategy => \"Cokernel\""@.  Since the cokernel
            computation involves none of the local straightening steps,
            agreement on a set certifies the algorithm on that set
            independently of them.
            More explicitly, let $F_w$ have the hookwise-semistandard
            tabloids of content $w$ as a basis, let $\mathcal R_w$ be the
            span of all crossed-hook Pieri relations, and let $C_w$ be
            the span of the fully semistandard tabloids.  The linear
            algebra computes the projection in
            $F_w=\mathcal R_w\oplus C_w$ and compares it with the
            algorithm's normal form.
        Example
            verifyStraighteningAgainstCokernel({3,2}, 3)
            R = verifyStraighteningAgainstCokernel({3,3,3}, {3,2,2,2});
            (R#"tabloids", R#"relations", R#"passed")
        Text
            The first report agrees on all $45$ tabloids and uses $30$
            relation vectors.  The second checks $105$ tabloids of one
            content using $227$ relation vectors.  Relation counts count
            supplied vectors, not necessarily linearly independent ones;
            neither number by itself is a dimension of the quotient.
    SeeAlso
        hookStraighten
        verifyTriangularity
        VerificationReport
///

doc ///
    Key
        isHookSemistandard
        (isHookSemistandard, Thing)
        (isHookSemistandard, List)
        (isHookSemistandard, HookTabloid)
        (isHookSemistandard, Filling)
    Headline
        whether a hook tabloid is semistandard
    Usage
        isHookSemistandard T
    Inputs
        T:HookTabloid
            or a list of rows, or a @TO Filling@
    Outputs
        :Boolean
            @TO true@ if the whole tableau has weakly increasing rows and
            strictly increasing columns, @TO false@ otherwise
    Description
        Text
            With one-based cell indices, the conditions are
            $$T_{i,j}\le T_{i,j+1}\quad\text{and}\quad
              T_{i,j}<T_{i+1,j}$$
            whenever both cells being compared exist.  They apply across
            diagonal-hook boundaries as well as within each hook.
            Equivalently, each diagonal hook is semistandard and each
            consecutive pair, nested in its rank two diagram, is
            semistandard.  Such tabloids form the standard basis of the
            corresponding component of the hook algebra, and
            @TO hookStraighten@ returns @TT "{T => 1}"@ on them.
        Text
            The tabloid $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}4\phantom{|}}\end{array}$ is semistandard, and $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$ is not: the
            pair fails the column inequality because the outer arm entry
            $3$ is not smaller than the inner corner $2$.
        Example
            isHookSemistandard {{1,3},{2,4}}
            isHookSemistandard {{1,3},{2,2}}
            isHookSemistandard tabloid {{1,2,3},{2,1},{3}}
        Text
            The answers are @TO true@, @TO false@, @TO false@.  The last
            tableau fails the row condition $2\le1$, despite having
            two semistandard diagonal hooks.  Thus this function tests
            the product, not just its individual factors.
            The next calls compare a semistandard rank three tabloid with
            one whose outer hook is not sorted.
        Example
            isHookSemistandard {{1,2,3},{4,5,6},{7,8,9}}
            isHookSemistandard {{4,2,5,6},{1,7,8},{3,9,10},{11}}
        Text
            Anything that is not a hook tabloid with positive integer
            entries (an empty row, row lengths that do not form a
            partition, an entry that is not a positive integer, an object
            of another type) gives @TO false@ rather than an error; the
            empty tabloid is semistandard.  Every term of a normal form is
            semistandard.
        Example
            isHookSemistandard {{1,2},{3,4,5}}
            isHookSemistandard 5
            isHookSemistandard tabloid {}
            all(keys hookStraighten {{4,5,6},{7,1,2},{8}}, isHookSemistandard)
    SeeAlso
        hookStraighten
        hookTabloids
        frobeniusRank
///

doc ///
    Key
        frobeniusRank
        (frobeniusRank, List)
        (frobeniusRank, HookTabloid)
        (frobeniusRank, Filling)
    Headline
        the Frobenius rank of a partition or of the shape of a tabloid
    Usage
        r = frobeniusRank lambda
        r = frobeniusRank T
    Inputs
        lambda:List
            a partition $\lambda=(\lambda_0\ge\lambda_1\ge\cdots)$, as a
            list of positive integers, or a tabloid as a list of rows
        T:HookTabloid
            or a @TO Filling@; the rank of its shape
    Outputs
        r:ZZ
            the Frobenius rank of $\lambda$, the largest $r$ with
            $\lambda_{r-1}\ge r$: the side length of the Durfee square, or
            the number of cells $(i,i)$ on the main diagonal that lie
            inside $\lambda$
    Description
        Text
            The Frobenius rank counts the diagonal hooks in the
            decomposition $\lambda=H_0\sqcup H_1\sqcup\cdots\sqcup H_{r-1}$.
            Each hook $H_k$ has corner cell $(k,k)$, arm length
            $\lambda_k-k-1$ and leg length $\lambda^{\mathsf t}_k-k-1$;
            @TO hookDecomposition@ lists the hooks of a tabloid.
        Example
            frobeniusRank {4,3,2}
            frobeniusRank {3,3,3}
            frobeniusRank {4,1,1,1}
            frobeniusRank tabloid {{1,2,3},{2,1},{3}}
        Text
            These ranks are $2,3,1,2$, respectively.  For example,
            $(4,3,2)$ has diagonal cells $(1,1),(2,2)$ but no $(3,3)$,
            so its tensor has two diagonal-hook factors.  Here is the
            same count for several small hook and nested-hook shapes:
        Example
            apply({{1},{2,1},{3,1,1},{2,2},{3,2,1},{3,3,2},{3,3,3}}, frobeniusRank)
        Text
            The result is $(1,1,1,2,2,2,3)$: Frobenius rank is neither
            the number of rows nor the dimension of a representation.
    SeeAlso
        hookDecomposition
        isHookSemistandard
        hookStraighten
///

doc ///
    Key
        hookTabloids
        (hookTabloids, List, ZZ)
        (hookTabloids, List, List)
    Headline
        all hook tabloids of a shape
    Usage
        hookTabloids(lambda, n)
        hookTabloids(lambda, content)
    Inputs
        lambda:List
            a partition
        n:ZZ
            the alphabet size
        content:List
            a content vector, whose length is the alphabet size and whose
            sum is the size of the shape
    Outputs
        :List
            the hook tabloids of shape @TT "lambda"@ whose diagonal hooks
            are semistandard, as @TO HookTabloid@s; with an alphabet, all
            of them with entries in $1,\ldots,n$; with a content, only
            those of that content
    Description
        Text
            These are the tabloids on which the straightening algorithm
            acts: products of semistandard hook tableaux of the diagonal
            hook shapes, placed in the diagram of $\lambda$.  Only some of
            them are semistandard as whole tableaux (see
            @TO isHookSemistandard@).  The content of all ones gives the
            tabloids in which every letter $1,\ldots,|\lambda|$ occurs
            once.
        Example
            L = hookTabloids({3,2}, 3);
            #L
            #select(L, isHookSemistandard)
            hookTabloids({3,2}, {2,2,1})
            #hookTabloids({3,2}, {1,1,1,1,1})
        Text
            The diagonal hooks of $(3,2)$ have shapes $(3,1)$ and $(1)$.
            Thus the first list is a tensor-product basis of
            $\mathbf S_{(3,1)}(\mathbf k^3)\otimes\mathbf k^3$, with
            $15\cdot3=45$ elements.  Exactly $15$ are fully
            semistandard and give the basis of $\mathbf S_{(3,2)}
            (\mathbf k^3)$ after quotienting by Pieri relations.
            The content $(2,2,1)$ selects five tensor-product basis
            elements; the squarefree content selects $15$ tabloids,
            each using all five letters once.
    Caveat
        Enumeration returns the full list, so large shapes or alphabets
        can require substantial time and memory.  Use the content form
        to restrict the list to a single weight space.
    SeeAlso
        verifyTriangularity
        verifyStraighteningAgainstCokernel
        isHookSemistandard
        tabloid
///

doc ///
    Key
        runningVectors
        (runningVectors, HookTabloid, ZZ)
        (runningVectors, List, ZZ)
        (runningVectors, Filling, ZZ)
        (runningVectors, HookTabloid)
        (runningVectors, List)
        (runningVectors, Filling)
    Headline
        the running vectors of a hook tabloid
    Usage
        runningVectors(T, N)
        runningVectors T
    Inputs
        T:HookTabloid
            or a list of rows, or a @TO Filling@
        N:ZZ
            the number of letters $\dim V$ (the length of each vector);
            when omitted, the largest entry of @TT "T"@
    Outputs
        :List
            the running vectors $V_{t,s}(T)$, one for each running index
            $(t,s)$ of the shape in the order of @TO runningIndices@
    Description
        Text
            Number rows, columns and diagonal hooks from $1$.  For a
            shape $\lambda$, let $H_i$ be its $i$th diagonal hook and
            let $a_t=\lambda_t-t$ be the arm length of $H_t$.  Define
            the running region
            $$B_{t,s}=\bigcup_{i<t}H_i\ \cup
                 \{(i,t):t\le i\le\lambda_t^{\mathsf t}\}
                 \ \cup\ \{(t,j):t<j\le t+s\},
                 \qquad 0\le s\le a_t.$$
            It contains every earlier hook, the full column of $H_t$
            (including its corner), and its first $s$ arm cells.  The
            returned vector is the cumulative letter count
            $$V_{t,s}(T)_q=\#\{b\in B_{t,s}:T(b)\le q\},
                 \qquad 1\le q\le N.$$
            It records a partial filling as cumulative counts, not as a
            list of its entries.  These vectors define the running
            partial order used by @TO runningCompare@.
        Text
            For the tabloids $T=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$ and $U=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$, the diagonal-hook types are $(2,1)$ and $(0,0)$:
        Example
            runningIndices {3,2}
            runningVectors {{1,1,2},{2,3}}
            runningVectors {{1,1,3},{2,2}}
        Text
            The indices are $(1,0),(1,1),(1,2),(2,0)$ and the vector
            lists are
            $$\begin{aligned}
               V(T)&=((1,2,2),(2,3,3),(2,4,4),(2,4,5)),\\
               V(U)&=((1,2,2),(2,3,3),(2,3,4),(2,4,5)).
            \end{aligned}$$
            At $(1,2)$ the region is the complete outer hook: its
            multisets are $\{1,1,2,2\}$ and $\{1,1,2,3\}$.
            At $(2,0)$ the whole tableaux have the same content $(2,2,1)$,
            so both cumulative vectors are $(2,4,5)$.
            The $q$th coordinate counts entries $\le q$, so enlarging $N$
            appends the total counts.
        Example
            runningVectors(tabloid {{1,1,2},{2,3}}, 4)
        Text
            The extra coordinates are $2,3,4,5$, the sizes of the four
            running regions.  Padding both inputs to a common $N$ does
            not change a running-order comparison.
    SeeAlso
        runningIndices
        runningCompare
        verifyTriangularity
///

doc ///
    Key
        runningIndices
        (runningIndices, List)
        (runningIndices, HookTabloid)
        (runningIndices, Filling)
    Headline
        the running indices of a shape
    Usage
        runningIndices lambda
        runningIndices T
    Inputs
        lambda:List
            a partition, or a tabloid as a list of rows
        T:HookTabloid
            or a @TO Filling@; the indices of its shape
    Outputs
        :List
            the pairs @TT "{t, s}"@ with $1\le t\le r$ and
            $0\le s\le a_t$, where $r$ is the Frobenius rank and $a_t$ the
            arm length of the $t$th diagonal hook, in lexicographic order
    Description
        Text
            The running vectors of a tabloid of shape $\lambda$ are
            indexed by these pairs, in this order: $V_{t,s}$ counts the
            entries of the first $t-1$ hooks
            and of the column and the first $s$ arm boxes of the $t$th
            hook.  The running order compares two tabloids at the largest
            index at which their vectors differ.
        Example
            runningIndices {3,2}
            runningIndices {4,4,2}
            runningIndices tabloid {{1,2,3},{2,1},{3}}
        Text
            For $(3,2)$ the index list is
            $((1,0),(1,1),(1,2),(2,0))$: scan the outer column, then its
            two arm cells, then the inner column.  For $(4,4,2)$ the
            list is $((1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2))$.
            In general its length is $\sum_{t=1}^r(a_t+1)$; there is
            one stage for a whole hook column, not one stage per leg cell.
    SeeAlso
        runningVectors
        runningCompare
        frobeniusRank
///

doc ///
    Key
        runningCompare
        (runningCompare, BasicList, BasicList)
        [runningCompare, Verbose]
    Headline
        compare two hook tabloids in the running partial order
    Usage
        runningCompare(T, U)
        runningCompare(T, U, Verbose => true)
    Inputs
        T:BasicList
            a @TO HookTabloid@, a list of rows or a @TO Filling@
        U:BasicList
            another tabloid of the same shape
        Verbose => Boolean
            when @TO true@, print the running index of the last
            disagreement, the two vectors there and the reason for the
            answer
    Outputs
        :Symbol
            @TT "symbol <"@ if $T\prec U$, @TT "symbol >"@ if $U\prec T$,
            @TT "symbol =="@ if all running vectors agree, and
            @TT "incomparable"@ if the two vectors at the last disagreement
            are incomparable
    Description
        Text
            Compare the vector lists of @TO runningVectors@ at the largest
            running index $(t_0,s_0)$ at which they differ.  At that index,
            $$T\prec U\quad\Longleftrightarrow\quad
              V_{t_0,s_0}(U)_q\le V_{t_0,s_0}(T)_q
              \text{ for every }q.$$
            At least one coordinate is strict, since this is a
            disagreement.  Reverse inequalities give $U\prec T$;
            inequalities in both directions among different coordinates
            give @TT "incomparable"@.  If there is no disagreement the
            result is @TT "symbol =="@.  Thus smaller tableau entries
            tend to give larger cumulative counts, which is why the
            vector inequality is reversed.  The order is only partial, so
            incomparable pairs occur.  The default monomial order of a
            hook ring, described by @TO paperOrder@, refines it.
        Text
            The tableaux $T=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$ and
            $U=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$ last disagree at the index $(1,2)$, where
            $V_{1,2}(U)=(2,3,4)\le(2,4,4)=V_{1,2}(T)$, so $T\prec U$.
            The reverse comparison gives @TT ">"@, and a tabloid compared
            with itself gives @TT "=="@; the two arguments need not be
            given in the same input form.
        Example
            runningCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}})
            runningCompare({{1,1,2},{2,3}}, {{1,1,3},{2,2}}, Verbose => true)
            runningCompare(tabloid {{1,1,3},{2,2}}, tabloid {{1,1,2},{2,3}})
            runningCompare(tabloid {{1,1,3},{2,2}}, {{1,1,3},{2,2}})
        Text
            These calls return $<,<,>,==$; the verbose second call
            displays the witness vectors $(2,4,4)$ and $(2,3,4)$.
            Equality here means equality of the running data.  For
            inputs whose individual hooks have not been normalized,
            it need not mean equality of their row lists: permuting a
            column does not change its cumulative counts.
            Once the diagonal hooks are semistandard, straightening a
            nonsemistandard product produces terms that are
            strictly smaller in this order; here every term of the normal
            form of $T$ is smaller than $T$.  @TO verifyTriangularity@
            checks this descent for one step on every tabloid of a shape
            and alphabet.
        Example
            T = tabloid {{4,5,6},{7,1,2},{8}};
            all(keys hookStraighten T, U -> runningCompare(U, T) === symbol <)
        Text
            Among the hook tabloids of shape $(3,2)$ on three letters some
            pairs are incomparable.
        Example
            L = hookTabloids({3,2}, 3);
            P = first select(subsets(L, 2), p -> runningCompare(p#0, p#1) === incomparable)
            runningCompare(P#0, P#1, Verbose => true)
        Text
            The displayed pair is $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$ and $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\end{array}$.  Its last
            vectors are $(3,4,5)$ and $(2,5,5)$: the first has a larger
            first coordinate but a smaller second coordinate.  Neither
            dominates the other, so the answer is @TT "incomparable"@,
            not an arbitrary tie-break.
    SeeAlso
        runningVectors
        runningIndices
        verifyTriangularity
        hookStraighten
///

doc ///
    Key
        Obstruction
        EngineStep
    Headline
        options of the single-step straightening functions
    Description
        Text
            @TT "Obstruction"@ chooses which obstruction of a rank two pair
            is resolved: @TT "\"Automatic\""@ (a row obstruction first, as
            in the algorithm), @TT "\"row\""@ or @TT "\"column\""@; the
            requested obstruction must be present.  @TT "EngineStep"@, when
            @TT "true"@, replaces the local move by the complete rank two
            normal form of the pair, which is the step the package engine
            takes in rank three and higher.  Both are accepted by
            @TO straighteningStep@, @TO isStepTriangular@ and
            @TO verifyTriangularity@; @TO straighteningObstruction@ accepts
            @TT "Obstruction"@.
        Example
            P = {{1,1,2,3},{2,1,1,1},{3,4}};
            straighteningStep(P, Obstruction => "column")
            straighteningStep({{4,5,6},{1,2,3},{7,8,9}}, EngineStep => true)
        Text
            For the first tableau both $T_{2,1}=2>T_{2,2}=1$ and
            $T_{1,2}=1\ge T_{2,2}=1$ fail the semistandard conditions;
            choosing @TT "\"column\""@ selects the latter.  The second
            tableau first needs a single-hook normalization, so that
            preliminary step is performed even with @TT "EngineStep"@
            enabled; the option changes a subsequent two-hook move.
    SeeAlso
        straighteningStep
        straighteningObstruction
        verifyTriangularity
///

doc ///
    Key
        hookRing
        (hookRing, ZZ, List)
        (hookRing, ZZ)
        [hookRing, CoefficientRing]
        [hookRing, MaxArm]
        [hookRing, MaxLeg]
        [hookRing, Weight]
        [hookRing, MonomialOrder]
        [hookRing, VariableLimit]
        [hookRing, Force]
    Headline
        the polynomial ring of a finite hook algebra
    Usage
        R = hookRing(n, types)
        R = hookRing(n, MaxArm => A, MaxLeg => B)
    Inputs
        n:ZZ
            the alphabet is $\{1,\ldots,n\}$, that is, $\dim V = n$
        types:List
            the hook types to retain, each a pair @TT "{a, b}"@ meaning the
            hook shape $(a+1, 1^b)$ with $a$ boxes to the right of the corner
            and $b$ below it; a nonempty list of distinct types with
            $a\ge0$ and $0\le b<n$
        MaxArm => ZZ
            with @TO MaxLeg@, an alternative to the list of types: every
            type @TT "{a, b}"@ with $0 \le a \le A$ and $0 \le b \le B < n$
        MaxLeg => ZZ
            see @TO MaxArm@
        CoefficientRing => Ring
            @TT "QQ"@ (the default) or a field of positive characteristic
            such as @TT "ZZ/7"@; the presentations of the ring are computed
            over this field
        Weight => List
            an optional list of $n$ nonnegative integers bounding the
            content: its $q$th entry (zero based) bounds the multiplicity
            of the letter $q+1$; only the tableaux with content at most the
            bound are retained
        MonomialOrder => String
            @TT "\"Paper\""@ (the default), the term order of Section 3.2
            of the paper compiled as a weight order, or @TT "\"GRevLex\""@
        VariableLimit => ZZ
            stop before making a ring with more than this many variables
            (the default is $500$)
        Force => Boolean
            override @TO VariableLimit@
    Outputs
        R:PolynomialRing
            the polynomial ring with one variable for each semistandard
            hook tableau of the chosen types, named @TT "z0, z1, ..."@, in
            the chosen monomial order
    Description
        Text
            Let $V=\mathbf k^n$ and let $\mathcal T$ be the supplied set of
            hook types.  Write $H_{a,b}=\mathbf S_{(a+1,1^b)}(V)$.  In
            characteristic zero, without a content bound, the command constructs
            $$
            R_{\mathcal T}=\operatorname{Sym}\!\left(
                \bigoplus_{(a,b)\in\mathcal T}H_{a,b}\right).
            $$
            A semistandard hook tableau is a basis vector of $H_{a,b}$,
            hence one polynomial variable.  Every variable has ring degree
            $1$, regardless of the number $a+b+1$ of boxes in its hook.
            Types are ordered by decreasing $(a+b+1,a,b)$; within a type,
            the variable order is the tableau order returned by @TO hookBasis@.
            The finite hook algebra generated by these hook spaces is the quotient by
            @TO straighteningIdeal@, which @TO hookAlgebra@ builds.  The
            ring itself is a polynomial ring; the
            accessors @TO hookTypes@, @TO alphabetSize@, @TO weightBound@
            and @TO hookBasis@ recover the choices it was built from, and
            @TO tabloid@ of a variable gives its tableau.
        Example
            R = hookRing(2, {{0,0},{1,1}})
            hookTypes R
            hookBasis R
            tabloid R_1
        Text
            Here $V=\mathbb Q^2$ and the two types specify
            $H_{0,0}=V$ and $H_{1,1}=\mathbf S_{(2,1)}(V)$, each of
            dimension $2$.  Thus $R=\mathbb Q[z_0,z_1,z_2,z_3]$.
            The first two displayed tableaux label $z_0,z_1$ in
            $\mathbf S_{(2,1)}(V)$; the single boxes label $z_2,z_3$ in
            $V$.  The last output identifies the basis vector called
            @TT "R_1"@, not a relation or a quotient of $R$.
        Text
            The default monomial order is the paper order, a weight order
            whose rows are listed by @TO paperOrder@; @TO verifyPaperOrder@
            checks it against the running order.  Under @TT "\"GRevLex\""@
            the straightening relations are in general not a Groebner
            basis.
        Example
            (options R).MonomialOrder
        Text
            With @TT "Weight => w"@ only the tableaux whose content is at
            most $w$ are retained, and the presentations of the ring add
            the truncation relations of @TO weightTruncationIdeal@.  Content
            counts every box once, including the corner.  The bound
            $(1, \ldots, 1)$ is preserved by the symmetric group; a general
            bound only by the permutations that fix it.
        Example
            Rw = hookRing(3, {{0,0},{1,1}}, Weight => {1,1,1});
            hookBasis Rw
        Text
            The five surviving variables are the two $(2,1)$ tableaux
            using each of $1,2,3$ once and the three single boxes.
            For example, a tableau containing two $1$'s is excluded by
            the first entry of the bound.
        Text
            @TO MaxArm@ and @TO MaxLeg@ select the rectangle of types,
            and @TO CoefficientRing@ chooses the field of the ring and of
            every computation on it.
        Example
            R7 = hookRing(2, MaxArm => 1, MaxLeg => 1, CoefficientRing => ZZ/7);
            hookTypes R7
            numgens R7
            coefficientRing R7
        Text
            The rectangle selects the shapes $(2,1),(2),(1,1),(1)$.
            Their dimensions on a two-dimensional space are $2,3,1,2$,
            explaining the $8$ variables, now over $\mathbb F_7$.
    Caveat
        The variables are @TT "z0, z1, ..."@; refer to generator $i$ as
        @TT "R_i"@ or, after @TT "use R"@, as @TT "z5"@ (not @TT "z_5"@,
        which Macaulay2 reads as an unrelated indexed variable).  Two calls
        make two different rings.
    SeeAlso
        hookAlgebra
        straighteningIdeal
        pieriIdeal
        hookBasis
        tabloid
        paperOrder
        verifyPaperOrder
        weightTruncationIdeal
        fineSegreRing
///

doc ///
    Key
        straighteningIdeal
        (straighteningIdeal, PolynomialRing)
        (straighteningIdeal, QuotientRing)
        [straighteningIdeal, Validate]
        [straighteningIdeal, RelationLimit]
        [straighteningIdeal, Force]
    Headline
        the ideal of the straightening relations of a hook ring
    Usage
        I = straighteningIdeal R
        I = straighteningIdeal A
    Inputs
        R:PolynomialRing
            a hook ring from @TO hookRing@ or a fine Segre ring from
            @TO fineSegreRing@
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one; the
            ideal of its ambient hook ring is returned
        Validate => Boolean
            check that the leading monomial of every relation is the
            nonstandard product it rewrites (the default is @TT "true"@;
            the check applies to the paper order)
        RelationLimit => ZZ
            stop before storing more than this many relations (the default
            is $10000$)
        Force => Boolean
            override @TO RelationLimit@
    Outputs
        I:Ideal
            the ideal of @TT "R"@ generated by the straightening relations
    Description
        Text
            The straightening presentation of a finite hook algebra has one
            relation for each nonstandard product of two generators.  If
            $z_Tz_U$ straightens to $\sum_{P,Q}c_{P,Q}z_Pz_Q$, its relation
            is the polynomial
            $$z_Tz_U-\sum_{P,Q}c_{P,Q}z_Pz_Q\in R.$$
            Thus this polynomial vanishes in $R/I$; the ideal is not the
            list of the normal forms themselves.  A
            product of hook types that are not strictly nested is zero
            (kind @TT "\"nonchain\""@), a nonsemistandard nested pair is
            replaced by its complete rank two normal form, the
            straightening algorithm of Section 6 of the paper (kind
            @TT "\"straightening\""@), and a product whose content exceeds
            the weight bound is zero (kind @TT "\"weight-truncation\""@,
            together with the minimal overweight standard chains of higher
            degree).  @TO hookRelation@ returns the relation of a pair of
            generators and @TO relationKind@ its kind.  With the paper
            order, the leading monomial of every relation is the
            nonstandard product it rewrites, so the standard monomials are
            the semistandard hook tabloids.  When the relations are quadratic,
            @TO hasQuadraticGroebnerBasis@ checks this Groebner basis property.
            The ideal is computed once and memoized on the ring.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            I = straighteningIdeal R
            hookRelation(R, 1, 2)
            relationKind(R, 1, 2)
            A = hookAlgebra R;
            ideal A === I
        Text
            The displayed relation $z_1z_2+z_0z_3$ means
            $z_1z_2=-z_0z_3$ in $A=R/I$.  The monomial generators of
            $I$, such as $z_0^2$ and $z_0z_2$, instead say those products
            are zero.  The final @TT "true"@ checks that @TO hookAlgebra@
            uses this very ideal.
        Text
            With a weight bound, overweight products are zero; the minimal
            overweight standard monomials can have degree greater than two,
            so a weight restricted presentation need not be quadratic.  The
            bound $(3,3,2)$ on the three diagonal hook types has one cubic
            relation.
        Example
            Rc = hookRing(3, {{0,0},{1,1},{2,2}}, Weight => {3,3,2});
            Ic = straighteningIdeal Rc;
            tally apply(degrees Ic, sum)
        Text
            The entries $2\mapsto123$ and $3\mapsto1$ count $123$
            quadratic relations and one cubic relation in the presentation;
            they are not dimensions of graded pieces of the quotient.
        Text
            On a fine Segre ring the relations are those of Section 7 of
            the paper: a fine product is standard only when the base types
            form a strict chain and the induced pair of tableaux is
            semistandard in both factors, and every other product is zero
            or is replaced by the product of the two rank two normal forms.
        Example
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            Iw = straighteningIdeal W;
            numgens Iw
            hookRelation(W, 0, 0), relationKind(W, 0, 0)
            tabloid W_1, tabloid W_6
            hookRelation(W, 1, 6), relationKind(W, 1, 6)
            tabloidTerms(W_1 * W_6 - hookRelation(W, 1, 6))
        Text
            Here $w_0^2=0$ and $w_1w_6=-w_0w_7$ in $W/I_w$.
            The last hash-table entry has coefficient $-1$ and a pair
            of tableaux as its key: it expresses the normal form as minus
            the tensor product of the two displayed tableau basis vectors.
    SeeAlso
        hookRing
        hookAlgebra
        pieriIdeal
        nonstandardMonomialIdeal
        weightTruncationIdeal
        hookRelation
        relationKind
        hasQuadraticGroebnerBasis
        fineSegreRing
///

doc ///
    Key
        pieriIdeal
        (pieriIdeal, PolynomialRing)
        (pieriIdeal, QuotientRing)
        [pieriIdeal, Validate]
        [pieriIdeal, RelationLimit]
        [pieriIdeal, Force]
    Headline
        the ideal of the Pieri relations of a hook ring
    Usage
        P = pieriIdeal R
        P = pieriIdeal A
    Inputs
        R:PolynomialRing
            a hook ring from @TO hookRing@
        A:QuotientRing
            a hook algebra or a quotient of one; the ideal of its ambient
            hook ring is returned
        Validate => Boolean
            check that every Pieri matrix used is injective (the default is
            @TT "true"@)
        RelationLimit => ZZ
            stop before storing more than this many relations (the default
            is $10000$)
        Force => Boolean
            override @TO RelationLimit@
    Outputs
        P:Ideal
            the ideal of @TT "R"@ generated by the Pieri relations
    Description
        Text
            This presents the same algebra without the straightening
            algorithm.  Products of hook types that are not strictly nested
            vanish, and for each nested pair of types the remaining
            relations are the columns of the crossed hook Pieri map.  For
            types $(a,b)$ and $(c,d)$ with $a>c$ and $b>d$, this is
            $$
            \mathbf S_{(c+1,1^b)}(V)\otimes\mathbf S_{(a+1,1^d)}(V)
            \longrightarrow
            \mathbf S_{(a+1,1^b)}(V)\otimes\mathbf S_{(c+1,1^d)}(V)
            \longrightarrow R_2,
            $$
            where the second arrow multiplies the corresponding ring
            generators.  The image consists of quadratic polynomials
            declared zero in the hook algebra (Section 4 of the paper;
            see @TO pieriMatrix@).  With a weight
            bound only the relevant weight blocks of those maps are built,
            and the overweight products and chains are added.  The two
            presentations define the same ideal; their equality can be
            checked with @TO symbol ==@.  The ideal is memoized on the
            ring.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            P = pieriIdeal R
            P == straighteningIdeal R
            numgens pieriIdeal hookAlgebra(3, {{0,0},{1,1},{2,2}})
        Text
            For the first ring the crossed map is
            $\bigwedge^2V\otimes\operatorname{Sym}^2V
            \longrightarrow\mathbf S_{(2,1)}(V)\otimes V$ with
            $V=\mathbb Q^2$.  Together with the nonchain products, its
            image generates the displayed ideal.  The equality test
            says the Pieri and straightening presentations have the same
            quotient, not that their displayed generating lists coincide.
        Text
            A fine Segre ring has no Pieri presentation of its own; the
            kernel of @TO fineSegreMap@ into the tensor product of the Pieri
            quotients of the factors is its Pieri presentation.
    SeeAlso
        straighteningIdeal
        hookRing
        pieriMatrix
        hasQuadraticGroebnerBasis
        fineSegreMap
///

doc ///
    Key
        nonstandardMonomialIdeal
        (nonstandardMonomialIdeal, PolynomialRing)
        (nonstandardMonomialIdeal, QuotientRing)
    Headline
        the monomial ideal of the nonstandard products of a hook ring
    Usage
        J = nonstandardMonomialIdeal R
        J = nonstandardMonomialIdeal A
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one; the
            ideal of its ambient ring is returned
    Outputs
        J:MonomialIdeal
            generated by the products of the pairs of generators that are
            not standard and by the minimal overweight standard chains of
            higher degree
    Description
        Text
            A product of two generators is standard when their types are
            strictly nested and the nested pair of tableaux is semistandard
            (on a fine Segre ring, in both factors); with a weight bound
            the product must also stay within the bound.  The monomial
            ideal of the nonstandard products is the initial ideal that the
            straightening presentation predicts: with the paper order each
            straightening relation has a nonstandard product as its leading
            monomial, and @TO hasQuadraticGroebnerBasis@ checks that the
            initial ideal of @TO straighteningIdeal@ is exactly this
            monomial ideal.  The standard monomials, its complement, are the
            semistandard hook tabloids.  The ideal is memoized on the ring.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            J = nonstandardMonomialIdeal R
            J == monomialIdeal leadTerm gb straighteningIdeal R
            hilbertSeries(R/J) == hilbertSeries(R/straighteningIdeal R)
        Text
            In this example the only surviving quadratic monomial modulo
            $J$ is $z_0z_3$.  Thus $1,z_0,z_1,z_2,z_3,z_0z_3$ is a
            vector-space basis of $R/J$, and its Hilbert series is
            $1+4q+q^2$, the same as that of the hook algebra.  The
            monomial quotient records which products are standard; it
            does not retain the hook algebra's nonmonomial relations.
    SeeAlso
        straighteningIdeal
        hasQuadraticGroebnerBasis
        relationKind
        weightTruncationIdeal
///

doc ///
    Key
        weightTruncationIdeal
        (weightTruncationIdeal, PolynomialRing)
        (weightTruncationIdeal, QuotientRing)
        [weightTruncationIdeal, RelationLimit]
        [weightTruncationIdeal, Force]
    Headline
        the monomial ideal of the monomials exceeding a content bound
    Usage
        K = weightTruncationIdeal R
        K = weightTruncationIdeal A
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one; the
            ideal of its ambient ring is returned
        RelationLimit => ZZ
            stop before storing more than this many candidate monomials
            (the default is $10000$)
        Force => Boolean
            override @TO RelationLimit@
    Outputs
        K:MonomialIdeal
            generated by the divisibility minimal monomials whose total
            content exceeds the weight bound of the ring; the zero ideal
            when the ring has no bound
    Description
        Text
            The content of a monomial in the generators is the sum of the
            contents of its tableaux.  When the ring was built with
            @TT "Weight => w"@ (see @TO hookRing@), the finite hook algebra
            is truncated to the weight spaces at most $w$, which kills
            every monomial whose content exceeds $w$; this is the monomial
            ideal of those monomials, and it is contained in
            @TO straighteningIdeal@.  On a fine Segre ring, each factor's
            specified content bound is enforced separately.  The ideal is
            memoized on the ring.
            Explicitly, if $\operatorname{cont}(z_i)=c_i\in\mathbb N^n$,
            then
            $$K=(z^\alpha:\ \sum_i\alpha_i c_i\not\le w),$$
            where $\le$ is coordinatewise comparison.  It suffices to
            return the divisibility-minimal monomials in this set.
        Example
            R = hookRing(3, {{0,0},{1,1}}, Weight => {1,1,1});
            K = weightTruncationIdeal R
            isSubset(K, straighteningIdeal R)
            weightTruncationIdeal hookRing(2, {{0,0},{1,1}})
        Text
            With $w=(1,1,1)$, any product repeating a letter is killed.
            The @TT "true"@ output checks $K\subseteq I$, and the
            final zero ideal says that without a content bound no such
            additional truncation is imposed.
    SeeAlso
        hookRing
        weightBound
        straighteningIdeal
        nonstandardMonomialIdeal
///

doc ///
    Key
        hasQuadraticGroebnerBasis
        (hasQuadraticGroebnerBasis, Ideal)
        (hasQuadraticGroebnerBasis, PolynomialRing)
        (hasQuadraticGroebnerBasis, QuotientRing)
    Headline
        whether the given relations are a quadratic Groebner basis
    Usage
        hasQuadraticGroebnerBasis I
        hasQuadraticGroebnerBasis R
        hasQuadraticGroebnerBasis A
    Inputs
        I:Ideal
            an ideal of a hook ring or of a fine Segre ring, such as
            @TO straighteningIdeal@ or @TO pieriIdeal@
        R:PolynomialRing
            a hook ring or fine Segre ring; its straightening ideal is
            tested
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one; its
            defining ideal @TT "ideal A"@ is tested
    Outputs
        :Boolean
            whether the generators of the ideal are quadratic and a
            Groebner basis for the monomial order of the ring with initial
            ideal @TO nonstandardMonomialIdeal@
    Description
        Text
            Macaulay2 computes a Groebner basis of the ideal independently
            of the straightening algorithm and compares three things: every
            given generator is quadratic, the initial ideal of the given
            generators is the initial ideal of the Groebner basis (so the
            generators are themselves a Groebner basis), and that initial
            ideal is the monomial ideal of the nonstandard products.  When
            all three hold the standard monomials are the semistandard hook
            tabloids and the algebra is Koszul.  The certificate is
            computed once and cached at @TT "I.cache#\"groebnerCertificate\""@,
            a hash table whose entry @TT "\"passed\""@ is the result.  With
            @TT "debugLevel > 0"@, the first computation also prints any
            failed checks.  The Pieri relations of @TO pieriIdeal@ generate
            the same ideal, but
            a column of a Pieri matrix need not have a nonstandard product
            as its leading monomial, so they are in general not a Groebner
            basis.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            hasQuadraticGroebnerBasis A
            sort keys (ideal A).cache#"groebnerCertificate"
            hasQuadraticGroebnerBasis pieriIdeal A
            pieriIdeal A == ideal A
        Text
            The first @TT "true"@ certifies the straightening generators.
            The later @TT "false"@ for the Pieri generators, followed by
            @TT "true"@ for equality of ideals, distinguishes a property
            of a chosen generating list from a property of its quotient.
            Here Koszul means $\operatorname{Tor}^A_i(\mathbf k,\mathbf k)_j=0$
            for $j\ne i$, using degree $1$ for each hook generator.
        Text
            The squarefree bound $(1,1,1,1)$ on four letters keeps the
            relations a quadratic Groebner basis, so that truncation is
            Koszul; the bound $(3,3,2)$ on three letters has a cubic
            relation, and under @TT "\"GRevLex\""@ the straightening
            relations are not a Groebner basis.
        Example
            hasQuadraticGroebnerBasis hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1})
            hasQuadraticGroebnerBasis hookAlgebra(3, {{0,0},{1,1},{2,2}}, Weight => {3,3,2})
            Rg = hookRing(3, MaxArm => 2, MaxLeg => 2, MonomialOrder => "GRevLex");
            hasQuadraticGroebnerBasis Rg
    SeeAlso
        straighteningIdeal
        pieriIdeal
        nonstandardMonomialIdeal
        hookAlgebra
        fineSegreAlgebra
///

doc ///
    Key
        paperOrder
        (paperOrder, PolynomialRing)
        (paperOrder, QuotientRing)
    Headline
        the rows of the paper's term order on a hook ring
    Usage
        paperOrder R
        paperOrder A
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
    Outputs
        :List
            the rows of the compiled order, each a pair @TT "{label, weights}"@
            of a string naming the meaning of the row and the list of its
            weights, one per variable; the empty list for a ring built with
            @TT "MonomialOrder => \"GRevLex\""@
    Description
        Text
            The term order of Section 3.2 of the paper is realized as a
            weight order refined by @TT "Lex"@.  Its first row is the number
            of boxes, then one indicator row per hook type in the order of
            the types, and then, for each scan stage (a type and a number of
            arm entries read, latest stage first) and each letter $q$, the
            number of scanned entries larger than $q$; the last stage of
            disagreement decides a comparison, which is the running order
            of Definition 3.2.3 read in the orientation that makes the
            nonstandard product the leading monomial.  On a fine Segre ring
            the rows are the boxes, one indicator row per fine degree, and
            then the rows of the left factor and of the right factor.
            The weights are the ones of the ring's monomial order, so that
            @TT "(options R).MonomialOrder"@ shows the same rows without
            labels.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            paperOrder R
            apply(paperOrder R, first)
        Text
            A row $w=(w_0,\ldots,w_3)$ assigns the monomial $z^\alpha$
            the weight $w\cdot\alpha$.  The first row $(3,3,1,1)$ thus
            counts boxes, not polynomial degree: $z_0z_3$ and $z_1z_2$
            both have box weight $4$ and polynomial degree $2$.
            Successive rows break ties, and the final lexicographic
            refinement resolves any ties left by all these rows.
    SeeAlso
        hookRing
        verifyPaperOrder
        runningVectors
///

doc ///
    Key
        verifyPaperOrder
        (verifyPaperOrder, PolynomialRing)
        (verifyPaperOrder, QuotientRing)
        [verifyPaperOrder, ComparisonLimit]
        [verifyPaperOrder, Force]
    Headline
        audit the compiled term order against the running order
    Usage
        V = verifyPaperOrder R
        V = verifyPaperOrder A
    Inputs
        R:PolynomialRing
            a hook ring built with the paper order
        A:QuotientRing
            a hook algebra or a quotient of one
        ComparisonLimit => ZZ
            stop before making more than this many comparisons (the
            default is $100000$)
        Force => Boolean
            override @TO ComparisonLimit@
    Outputs
        V:VerificationReport
            of kind @TT "\"PaperOrder\""@, with the fields
            @TT "\"strictComparisons\""@ (the number of pairs of rank two
            tabloids of the same types that the running order compares
            strictly), @TT "\"paperTies\""@ (the number of pairs it does not
            separate), @TT "\"mismatches\""@ (the number of strict
            comparisons on which Macaulay2's leading monomial disagrees),
            @TT "\"firstMismatch\""@ (the first disagreeing pair as two
            tabloids with the two verdicts, or @TT "null"@), and
            @TT "\"passed\""@, which is @TT "true"@ when there is no
            mismatch
    Description
        Text
            For every pair of hook types that are strictly nested and every
            two rank two chain tabloids of those types, the running vectors
            (see @TO runningVectors@) are compared as in Definition 3.2.3 of
            the paper, and the outcome is checked against the leading
            monomial of the sum of the two corresponding quadratic monomials
            in the ring.  The audit is independent of the compilation of the
            order into weight rows.  On the algebra of Example 1.2.2 of the
            paper this is $1557$ comparisons with no mismatch.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            V = verifyPaperOrder A
            V#"strictComparisons"
            V#"passed"
    SeeAlso
        paperOrder
        runningVectors
        VerificationReport
        hookRing
///

doc ///
    Key
        MaxArm
    Headline
        the largest arm length of a rectangle of hook types
    Description
        Text
            An option of @TO hookRing@ and @TO hookAlgebra@: with
            @TO MaxLeg@, it selects every hook type @TT "{a, b}"@ with
            $0 \le a \le$ @TT "MaxArm"@ and $0 \le b \le$ @TT "MaxLeg"@,
            in place of a list of types.
        Example
            hookTypes hookRing(3, MaxArm => 1, MaxLeg => 1)
    SeeAlso
        MaxLeg
        hookRing
        hookAlgebra
///

doc ///
    Key
        MaxLeg
    Headline
        the largest leg length of a rectangle of hook types
    Description
        Text
            An option of @TO hookRing@ and @TO hookAlgebra@, used with
            @TO MaxArm@; the leg length is at most $n - 1$ on $n$ letters.
        Example
            hookTypes hookRing(3, MaxArm => 0, MaxLeg => 2)
    SeeAlso
        MaxArm
        hookRing
///

doc ///
    Key
        Weight
    Headline
        a content bound on the tableaux of a hook ring
    Description
        Text
            An option of @TO hookRing@ and @TO hookAlgebra@: a list of $n$
            nonnegative integers whose $q$th entry (zero based) bounds the
            multiplicity of the letter $q+1$.  Only the tableaux with
            content at most the bound are retained, and the presentations
            kill every monomial whose total content exceeds it (see
            @TO weightTruncationIdeal@).  The bound is read back by
            @TO weightBound@.  The options @TO LeftWeight@ and
            @TO RightWeight@ of @TO fineSegreRing@ play the same role for
            the two factors of the generic fine Segre model.
        Example
            hookBasis hookRing(3, {{0,0},{1,1}}, Weight => {1,1,1})
    SeeAlso
        hookRing
        weightBound
        weightTruncationIdeal
        LeftWeight
        RightWeight
///

doc ///
    Key
        VariableLimit
    Headline
        a safeguard on the number of variables of a ring
    Description
        Text
            An option of @TO hookRing@, @TO hookAlgebra@, @TO fineSegreRing@
            and @TO fineSegreAlgebra@: the constructor stops with an error
            instead of making a polynomial ring with more than this many
            variables (the default is $500$); @TO Force@ overrides it.
            @TO fineSegreEstimate@ counts the variables of a fine Segre
            ring before it is built.
    SeeAlso
        Force
        hookRing
        fineSegreRing
        fineSegreEstimate
///

doc ///
    Key
        Force
    Headline
        override a size safeguard
    Description
        Text
            An option of the constructors and presentations that carry a
            size guard: @TO VariableLimit@ on @TO hookRing@, @TO hookAlgebra@,
            @TO fineSegreRing@ and @TO fineSegreAlgebra@, @TO RelationLimit@
            on @TO straighteningIdeal@, @TO pieriIdeal@ and
            @TO weightTruncationIdeal@, and @TO ComparisonLimit@ on
            @TO verifyPaperOrder@.  With @TT "Force => true"@ the guard is
            ignored.
    SeeAlso
        VariableLimit
        RelationLimit
        ComparisonLimit
///

doc ///
    Key
        RelationLimit
    Headline
        a safeguard on the number of relations of a presentation
    Description
        Text
            An option of @TO straighteningIdeal@, @TO pieriIdeal@,
            @TO weightTruncationIdeal@, @TO hookAlgebra@ and
            @TO fineSegreAlgebra@: the computation stops with an error
            naming the option instead of storing more than this many
            relations (the default is $10000$); @TO Force@ overrides it.
    SeeAlso
        Force
        straighteningIdeal
        pieriIdeal
///

doc ///
    Key
        Validate
    Headline
        check the leading terms or injectivity of a presentation
    Description
        Text
            An option of @TO straighteningIdeal@, @TO pieriIdeal@,
            @TO hookAlgebra@ and @TO fineSegreAlgebra@, @TT "true"@ by
            default.  For the straightening presentation with the paper
            order it checks that the leading monomial of every relation is
            the nonstandard product it rewrites; for the Pieri presentation
            it checks that every Pieri matrix used is injective.  A failed
            check is an error.
    SeeAlso
        straighteningIdeal
        pieriIdeal
        hasQuadraticGroebnerBasis
///

doc ///
    Key
        ComparisonLimit
    Headline
        a safeguard on the number of comparisons of the term order audit
    Description
        Text
            An option of @TO verifyPaperOrder@: the audit stops with an
            error instead of making more than this many comparisons (the
            default is $100000$); @TO Force@ overrides it.
    SeeAlso
        verifyPaperOrder
        Force
///

doc ///
    Key
        fineSegreRing
        (fineSegreRing, PolynomialRing, PolynomialRing)
        (fineSegreRing, ZZ, ZZ, ZZ)
        [fineSegreRing, TypePairs]
        [fineSegreRing, LeftWeight]
        [fineSegreRing, RightWeight]
        [fineSegreRing, CoefficientRing]
        [fineSegreRing, MonomialOrder]
        [fineSegreRing, VariableLimit]
        [fineSegreRing, Force]
    Headline
        the polynomial ring of a fine Segre product of two hook algebras
    Usage
        W = fineSegreRing(R1, R2, TypePairs => typePairs)
        W = fineSegreRing(n, m, r)
    Inputs
        R1:PolynomialRing
            the left hook ring, from @TO hookRing@
        R2:PolynomialRing
            the right hook ring, over the same coefficient field
        TypePairs => List
            triples @TT "{baseType, leftType, rightType}"@ matching one hook
            type of each factor to a common fine degree; required with two
            hook rings
        n:ZZ
            the dimension of the left vector space of the generic model
        m:ZZ
            the dimension of the right vector space
        r:ZZ
            the size of the minors, with $2 \le r \le \min(n, m)$
        LeftWeight => List
            an optional content bound of length $n$ on the left factor of
            the generic model
        RightWeight => List
            an optional content bound of length $m$ on the right factor of
            the generic model
        CoefficientRing => Ring
            the field of the generic model, @TT "QQ"@ by default; with two
            hook rings the field is theirs
        MonomialOrder => String
            @TT "\"Paper\""@ (the default) or @TT "\"GRevLex\""@; the paper
            order on the fine Segre ring needs factors built with the paper
            order
        VariableLimit => ZZ
            stop before making a ring with more than this many variables
            (the default is $500$)
        Force => Boolean
            override @TO VariableLimit@
    Outputs
        W:PolynomialRing
            the polynomial ring with one variable @TT "w0, w1, ..."@ for each pair
            of a left tableau of type @TT "leftType"@ and a right tableau of
            type @TT "rightType"@, fine degree by fine degree, multigraded
            by the fine degrees
    Description
        Text
            Write $H_{a,b}(V)=\mathbf S_{(a+1,1^b)}(V)$, and similarly
            for the second vector space $U$.  A triple
            @TT "{h, g1, g2}"@ selects the tensor-product generator space
            $H_{g_1}(V)\otimes H_{g_2}(U)$.  In characteristic zero,
            without content bounds, the command constructs
            $$W=\operatorname{Sym}\!\left(\bigoplus_h
                H_{g_1(h)}(V)\otimes H_{g_2(h)}(U)\right).$$
            If there are $s$ triples, the ring is $\mathbb N^s$-graded:
            every variable belonging to the $h$th triple has degree
            $e_h$, the corresponding coordinate vector.  The base hook
            type labels this coordinate; it is not itself the degree
            vector.  The quotient @TO fineSegreAlgebra@ imposes the
            multiplication relations of the hook algebras.
        Text
            For every triple @TT "{h, g1, g2}"@ and every pair of tableaux
            $T$ of type $g_1$ in the left factor and $U$ of type $g_2$ in
            the right factor, the ring has one variable $w^h_{T,U}$; the
            assignments $h \mapsto g_1$ and $h \mapsto g_2$ must be
            injective and preserve and reflect strict nesting, which is what
            lets the straightening algorithms of the two factors run in
            parallel (Section 7 of the paper).  The generators are read with
            @TO hookBasis@ and @TO tabloid@ as pairs of tabloids,
            @TO hookTypes@ returns the type pairs, @TO factors@ the two hook
            rings, and @TO straighteningIdeal@ the fine straightening
            relations; @TO fineSegreAlgebra@ builds the quotient.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}})
            hookTypes W
            hookBasis(W, {1,1})
            tabloid W_0
            degree W_0
            factors W
        Text
            The triple @TT "{{1,1},{1,1},{1,1}}"@ pairs
            $\mathbf S_{(2,1)}(\mathbb Q^2)$ with another copy of that
            two-dimensional space, giving the four displayed tensor
            basis vectors.  The other triple pairs the two copies of
            $\mathbb Q^2$, giving four more variables.  Since the
            $(1,1)$ base type is ordered first, @TT "W_0"@ has degree
            $(1,0)$; variables of base type $(0,0)$ have degree $(0,1)$.
        Text
            Content bounds are inherited from the factors.  In the quotient
            @TO fineSegreAlgebra@, a product vanishes when its total left
            or right content exceeds the corresponding bound, and @TO weightBound@
            returns the two bounds.
        Example
            RL = hookRing(2, {{0,0},{1,1}}, Weight => {2,1});
            Ww = fineSegreRing(RL, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            numgens Ww
            weightBound Ww
        Text
            The form @TT "fineSegreRing(n, m, r)"@ builds the fine Segre
            model of the generic $r$ minors of an $n \times m$ matrix: for
            $0 \le a \le m - r$ and $0 \le b \le n - r$ the base hook
            $(a, b)$ is paired with the left type $(a, b + r - 1)$ on $n$
            letters and the right type $(b, a + r - 1)$ on $m$ letters,
            both hook rings being built on the way.
        Example
            G = fineSegreRing(3, 3, 2);
            hookTypes G
            numgens G
            apply(factors G, alphabetSize)
        Text
            Here $V=U=\mathbb Q^3$.  In the displayed order, the four
            tensor spaces of generators are
            $$
            \begin{aligned}
            &\mathbf S_{(2,1,1)}(V)\otimes\mathbf S_{(2,1,1)}(U),\\
            &\mathbf S_{(2,1)}(V)\otimes\bigwedge^3U,\\
            &\bigwedge^3V\otimes\mathbf S_{(2,1)}(U),\\
            &\bigwedge^2V\otimes\bigwedge^2U.
            \end{aligned}
            $$
            Their dimensions $9,8,8,9$ sum to the output $34$.
            This is the polynomial presentation ring of the fine Segre
            model associated to the minors, not the polynomial ring in
            the nine matrix entries.
    Caveat
        The variables are @TT "w0, w1, ..."@, referred to as @TT "W_i"@.
        @TO fineSegreEstimate@ counts the variables before the ring is
        built.
    SeeAlso
        hookRing
        fineSegreAlgebra
        fineSegreEstimate
        fineSegreMap
        factors
        straighteningIdeal
        TypePairs
///

doc ///
    Key
        fineSegreAlgebra
        (fineSegreAlgebra, PolynomialRing, PolynomialRing)
        (fineSegreAlgebra, ZZ, ZZ, ZZ)
        (fineSegreAlgebra, PolynomialRing)
        [fineSegreAlgebra, TypePairs]
        [fineSegreAlgebra, LeftWeight]
        [fineSegreAlgebra, RightWeight]
        [fineSegreAlgebra, CoefficientRing]
        [fineSegreAlgebra, MonomialOrder]
        [fineSegreAlgebra, VariableLimit]
        [fineSegreAlgebra, RelationLimit]
        [fineSegreAlgebra, Force]
        [fineSegreAlgebra, Validate]
    Headline
        a fine Segre product of two hook algebras
    Usage
        B = fineSegreAlgebra(R1, R2, TypePairs => typePairs)
        B = fineSegreAlgebra(n, m, r)
        B = fineSegreAlgebra W
    Inputs
        R1:PolynomialRing
            the left hook ring
        R2:PolynomialRing
            the right hook ring
        n:ZZ
            the dimension of the left vector space in the generic model
        m:ZZ
            the dimension of the right vector space
        r:ZZ
            the size of the minors, with $2\le r\le\min(n,m)$
        W:PolynomialRing
            a fine Segre ring from @TO fineSegreRing@
        TypePairs => List
            as for @TO fineSegreRing@
        LeftWeight => List
            as for @TO fineSegreRing@
        RightWeight => List
            as for @TO fineSegreRing@
        CoefficientRing => Ring
            as for @TO fineSegreRing@
        MonomialOrder => String
            as for @TO fineSegreRing@
        VariableLimit => ZZ
            as for @TO fineSegreRing@
        RelationLimit => ZZ
            as for @TO straighteningIdeal@
        Force => Boolean
            override the two guards
        Validate => Boolean
            as for @TO straighteningIdeal@
    Outputs
        B:QuotientRing
            the quotient of the fine Segre ring by its straightening ideal,
            @TT "W / straighteningIdeal W"@
    Description
        Text
            Let $A_1,A_2$ be the subalgebras of the two hook algebras
            generated by the paired hook types.  Give the generators of
            types $g_1(h)$ and $g_2(h)$ the same fine degree $e_h$.
            The fine Segre product matches these multidegrees:
            $$B=A_1\mathbin{\#}A_2
              =\bigoplus_{\alpha\in\mathbb N^s}
                (A_1)_\alpha\otimes(A_2)_\alpha
              \subseteq A_1\otimes A_2.$$
            Multiplication is inherited from the tensor product.  Thus
            it matches the number of hooks of every paired type, not
            just the total number of generators.  Without content
            truncations, its
            straightening presentation (see @TO straighteningIdeal@) is a
            quadratic Groebner basis for the block paper order, which
            @TO hasQuadraticGroebnerBasis@ certifies, and the kernel of
            @TO fineSegreMap@ into the tensor product of the two Pieri
            quotients gives the same ideal.  The standard operations
            @TO ideal@, @TO ambient@ and @TO hilbertSeries@, and the
            accessors of the fine Segre ring, apply to this quotient ring.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            B = fineSegreAlgebra(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}})
            numgens B, numgens ideal B
            hasQuadraticGroebnerBasis B
            reduceHilbert hilbertSeries B
            tabloid B_5
            factors B
        Text
            The output $(8,35)$ counts eight generators and $35$
            defining quadrics.  The Hilbert series means
            $$\dim B_{(0,0)}=1,\quad
              \dim B_{(1,0)}=\dim B_{(0,1)}=4,\quad
              \dim B_{(1,1)}=1,$$
            with all other components zero.  The variables $T_0,T_1$
            in this Hilbert series record the two fine degrees.  The
            tableau pair returned for @TT "B_5"@ represents
            $v_1\otimes u_2$ in the degree-$(0,1)$ generator space
            $V\otimes U$, for the bases indexed by the two alphabets.
        Text
            A product of generators is reduced to its normal form by the
            Groebner basis: the generators @TT "B_1"@ and @TT "B_6"@ form
            a pair that is not semistandard in the right factor, and their
            product is minus the standard product @TT "B_0 * B_7"@, the
            only standard pair of this algebra.
        Example
            relationKind(B, 1, 6)
            B_1 * B_6
            tabloidTerms(B_1 * B_6)
        Text
            Explicitly, $w_1w_6=-w_0w_7$.  The hash-table key is the pair
            of standard $(2,2)$ tableaux indexing $w_0w_7$; its
            coefficient $-1$ is the coefficient in this equality in $B$.
        Text
            The generic model of the $2 \times 2$ minors of a $3 \times 3$
            matrix has $9 + 8 + 8 + 9$ generators and exactly one standard
            quadratic monomial, the product of two generators of the base
            types $(1,1)$ and $(0,0)$; a nonstandard pair reduces to a
            multiple of that monomial or to zero.  The third form presents
            a fine Segre ring built separately by @TO fineSegreRing@:
        Example
            B2 = fineSegreAlgebra(3, 3, 2);
            hookTypes B2
            numgens B2, numgens ideal B2
            relationKind(B2, 0, 33), relationKind(B2, 1, 32), relationKind(B2, 0, 27)
            B2_0 * B2_33, B2_1 * B2_32, B2_0 * B2_27
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            fineSegreAlgebra W === W / straighteningIdeal W
        Text
            The first product $w_0w_{33}$ is already standard; the other
            two satisfy $w_1w_{32}=-w_0w_{33}$ and $w_0w_{27}=0$ in
            the $34$-generator algebra.  For $S=\operatorname{Sym}(V\otimes U)$,
            the determinantal Tor algebra
            $\operatorname{Tor}^S(S/I_2,\mathbf k)$ degenerates to a
            Zhang twist of this fine Segre algebra.  This does not
            identify the two multiplications.
    SeeAlso
        fineSegreRing
        hookAlgebra
        straighteningIdeal
        hasQuadraticGroebnerBasis
        fineSegreMap
///

doc ///
    Key
        fineSegreEstimate
        (fineSegreEstimate, PolynomialRing, PolynomialRing, List)
    Headline
        count the generators of a proposed fine Segre product
    Usage
        E = fineSegreEstimate(R1, R2, typePairs)
    Inputs
        R1:PolynomialRing
            the left hook ring
        R2:PolynomialRing
            the right hook ring
        typePairs:List
            triples @TT "{baseType, leftType, rightType}"@, as for the
            option @TO TypePairs@
    Outputs
        E:Tally
            the number of fine generators of each type pair, that is, the
            product of the numbers of retained tableaux in the two paired
            types; the sum of its values is the number of variables of the
            fine Segre ring
    Description
        Text
            A fine generator pairs one left tableau with one right tableau
            of the same fine degree.  This counts the pairs, and validates
            the type pairs, before @TO fineSegreRing@ allocates a polynomial
            ring.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            E = fineSegreEstimate(R, R, {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}})
            sum values E
        Text
            Each entry is $2\cdot2=4$: both $V$ and
            $\mathbf S_{(2,1)}(V)$ have dimension $2$ for
            $V=\mathbb Q^2$.  The polynomial presentation therefore
            has $4+4=8$ variables, before any relations are imposed.
    SeeAlso
        fineSegreRing
        VariableLimit
///

doc ///
    Key
        fineSegreMap
        (fineSegreMap, PolynomialRing, Ring, Ring)
    Headline
        the map of a fine Segre ring into the tensor product of its factors
    Usage
        g = fineSegreMap(W, A1, A2)
    Inputs
        W:PolynomialRing
            a fine Segre ring
        A1:Ring
            the left factor of @TT "W"@ or a quotient of it, such as its
            hook algebra or its quotient by @TO pieriIdeal@
        A2:Ring
            the right factor or a quotient of it
    Outputs
        g:RingMap
            from @TT "W"@ to @TT "A1 ** A2"@, sending each fine variable
            $w^h_{T,U}$ to the product $z_T \otimes z_U$ of the two factor
            generators it pairs
    Description
        Text
            In mathematical notation the returned map is
            $$g:W\longrightarrow A_1\otimes_{\mathbf k}A_2,
              \qquad w^h_{T,U}\longmapsto z_T\otimes z_U.$$
            Macaulay2 prints a ring map with the target to the left and
            the source to the right, so its displayed arrow points left.
            The fine Segre product is $\operatorname{im}g\cong W/\ker g$.
            Its kernel,
            computed by elimination and independently of the straightening
            rules, is a presentation of the fine Segre product of the two
            quotients: for the Pieri quotients of the factors it is the
            Pieri presentation, and it agrees with
            @TO straighteningIdeal@.  With the two hook rings themselves in
            place of quotients the kernel is the ideal of the classical
            Segre relations.  Elimination grows quickly; use it for small
            checks and @TO straighteningIdeal@ for routine work.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            g = fineSegreMap(W, R/pieriIdeal R, R/pieriIdeal R);
            g W_0
            ker g == straighteningIdeal W
            numgens ker fineSegreMap(W, R, R)
        Text
            The output for @TT "g W_0"@ is $z_0\otimes z_0$;
            Macaulay2 adds subscripts $0$ and $1$ to distinguish the
            variables in the two tensor factors.  The equality check
            gives $\ker g=I_{\mathrm{straightening}}$.  Before taking
            the factor quotients there are instead just two Segre
            equations, one $2\times2$ determinant for each of the two
            paired type blocks.
    SeeAlso
        fineSegreRing
        pieriIdeal
        straighteningIdeal
        factors
///

doc ///
    Key
        factors
        (factors, PolynomialRing)
        (factors, QuotientRing)
    Headline
        the two hook rings of a fine Segre ring
    Usage
        (R1, R2) = factors W
        (R1, R2) = factors B
    Inputs
        W:PolynomialRing
            a fine Segre ring
        B:QuotientRing
            a fine Segre algebra or a quotient of one
    Outputs
        :Sequence
            the left and the right hook ring the fine Segre ring was built
            from
    Description
        Text
            A fine Segre ring has two alphabets and two weight bounds, one
            per factor; @TO alphabetSize@ and the other accessors of a hook
            ring are applied to the factors.
        Example
            G = fineSegreRing(2, 3, 2);
            (R1, R2) = factors G;
            alphabetSize R1, alphabetSize R2
            hookTypes R1, hookTypes R2
            factors fineSegreAlgebra G === factors G
    SeeAlso
        fineSegreRing
        fineSegreAlgebra
        alphabetSize
///

doc ///
    Key
        TypePairs
    Headline
        the paired hook types of a fine Segre ring
    Description
        Text
            An option of @TO fineSegreRing@ and @TO fineSegreAlgebra@ and
            the third argument of @TO fineSegreEstimate@: a list of triples
            @TT "{baseType, leftType, rightType}"@.  The base types, the
            labels of the fine degrees, are distinct; the left and right
            types are hook types of the two factors.  The two assignments
            from base types to factor types are injective and preserve
            and reflect strict nesting in the same direction: two base
            types are nested if and only if their assigned types are
            nested, with the same one outermost.  The type pairs
            are read back, in the order of the base types, by
            @TO hookTypes@.
            For example, @TT "{{1,1},{1,2},{1,2}}"@ labels the generator
            space $\mathbf S_{(2,1,1)}(V)\otimes
            \mathbf S_{(2,1,1)}(U)$ by the base type $(1,1)$.
            Its ring degree is the coordinate vector $e_h$ belonging to
            that label, not $(1,1)$; see @TO fineSegreRing@.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            hookTypes fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}})
    SeeAlso
        fineSegreRing
        hookTypes
///

doc ///
    Key
        LeftWeight
    Headline
        a content bound on the left factor of the generic fine Segre model
    Description
        Text
            An option of the forms @TT "fineSegreRing(n, m, r)"@ and
            @TT "fineSegreAlgebra(n, m, r)"@: a list of $n$ nonnegative
            integers passed as the @TO Weight@ of the left hook ring.  With
            two hook rings given, their own bounds are inherited.
        Example
            weightBound fineSegreRing(2, 3, 2, LeftWeight => {2,1})
    SeeAlso
        RightWeight
        Weight
        fineSegreRing
///

doc ///
    Key
        RightWeight
    Headline
        a content bound on the right factor of the generic fine Segre model
    Description
        Text
            An option of the forms @TT "fineSegreRing(n, m, r)"@ and
            @TT "fineSegreAlgebra(n, m, r)"@: a list of $m$ nonnegative
            integers passed as the @TO Weight@ of the right hook ring.
        Example
            weightBound fineSegreRing(2, 3, 2, RightWeight => {1,1,1})
    SeeAlso
        LeftWeight
        Weight
        fineSegreRing
///

doc ///
    Key
        hookAlgebra
        (hookAlgebra, ZZ, List)
        (hookAlgebra, ZZ)
        (hookAlgebra, PolynomialRing)
        [hookAlgebra, CoefficientRing]
        [hookAlgebra, MaxArm]
        [hookAlgebra, MaxLeg]
        [hookAlgebra, Weight]
        [hookAlgebra, MonomialOrder]
        [hookAlgebra, VariableLimit]
        [hookAlgebra, RelationLimit]
        [hookAlgebra, Force]
        [hookAlgebra, Validate]
    Headline
        a finite hook algebra
    Usage
        A = hookAlgebra(n, types)
        A = hookAlgebra(n, MaxArm => a, MaxLeg => b)
        A = hookAlgebra R
    Inputs
        n:ZZ
            the alphabet is $\{1, \ldots, n\}$, that is, $\dim V = n$
        types:List
            the hook types, each @TT "{arm, leg}"@; the hook of type $(a,b)$
            is the shape $(a+1, 1^b)$
        R:PolynomialRing
            a hook ring from @TO hookRing@, or a fine Segre ring
        CoefficientRing => Ring
            @TT "QQ"@ (the default) or a field of positive characteristic
            such as @TT "ZZ/7"@
        MaxArm => ZZ
            with @TO MaxLeg@, the rectangle of types in place of the list
        MaxLeg => ZZ
            see @TO MaxArm@
        Weight => List
            an optional content bound of length $n$ (see @TO hookRing@)
        MonomialOrder => String
            @TT "\"Paper\""@ (the default) or @TT "\"GRevLex\""@
        VariableLimit => ZZ
            a safeguard on the number of generators (the default is $500$)
        RelationLimit => ZZ
            a safeguard on the number of relations (the default is $10000$)
        Force => Boolean
            override the two safeguards
        Validate => Boolean
            run the leading term check of @TO straighteningIdeal@
    Outputs
        A:QuotientRing
            the quotient of the hook ring by its straightening ideal,
            @TT "R / straighteningIdeal R"@
    Description
        Text
            For $V=\mathbf k^n$ and the chosen set $\mathcal T$ of hook
            types, with no content bound this constructs the subalgebra
            $A_{\mathcal T}\subseteq\mathcal H(V)$.  In characteristic zero,
            its representation decomposition is
            $$A_{\mathcal T}\cong\bigoplus_\lambda\mathbf S_\lambda(V),
              \qquad \deg\mathbf S_\lambda(V)=(r(\lambda),|\lambda|),$$
            where the sum is over partitions whose every diagonal hook
            has type in $\mathcal T$ and which have at most $n$ rows.
            The empty partition supplies the unit.  Here $r(\lambda)$ is
            the number of diagonal boxes, and $|\lambda|$ is the number
            of boxes.  The grading used by the returned ring and its
            Hilbert series is $r(\lambda)$: each hook generator has
            degree $1$.  A content bound instead gives a quotient retaining
            only the allowed weight spaces of these summands; it need not
            carry the full $\mathrm{GL}(V)$-action.
        Text
            The algebra is presented as the quotient of the polynomial
            ring on the semistandard hook tableaux (see @TO hookRing@) by the
            straightening relations (see @TO straighteningIdeal@).  With the
            paper order, each nonsemistandard product is the leading term of
            its relation, so the standard monomials are the semistandard
            hook tabloids.  The standard operations @TO ideal@, @TO ambient@,
            @TO hilbertSeries@ and @TO dim@ apply to this quotient ring.
            A product of generators is reduced to its normal form by the
            Groebner basis.  Repeated calls with the same hook ring return
            the same quotient ring.
        Example
            A = hookAlgebra(2, {{0,0},{1,1}})
            ideal A
            reduceHilbert hilbertSeries A
        Text
            This first call chooses $V=\mathbb Q^2$ and the types
            $(0,0),(1,1)$, that is, the shapes $(1),(2,1)$.  Its graded
            pieces are
            $$A_0=\mathbb Q,\qquad
              A_1=V\oplus\mathbf S_{(2,1)}(V),\qquad
              A_2=\mathbf S_{(2,2)}(V),\qquad A_i=0\ (i>2).$$
            Their dimensions are $1,4,1$, explaining $1+4T+T^2$.
            The generator $z_1z_2+z_0z_3$ of the displayed ideal means
            $z_1z_2=-z_0z_3$ in $A$; the displayed monomial generators
            are products that vanish.
        Text
            The algebra of Example 1.2.2 of the paper, on three letters with
            the types $(0,0)$, $(1,1)$, $(2,2)$: $17$ generators, $136$
            relations, Hilbert series $1 + 17q + 17q^2 + q^3$.  The
            generators are read as tabloids with @TO hookBasis@ and
            @TO tabloid@, the relations with @TO hookRelation@ and
            @TO relationKind@, and a product in the algebra is a normal
            form whose tabloids @TO tabloidTerms@ lists.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            numgens A, numgens ideal A
            reduceHilbert hilbertSeries A
            hookTypes A
            tabloid A_4
            A_4 * A_14
            tabloidTerms(A_4 * A_14)
        Text
            The last two outputs express the same identity
            $z_4z_{14}=-z_2z_{15}-z_1z_{16}$, first as a polynomial
            normal form and then in tableau notation.  Interpreting each
            tableau as the product of its diagonal hook generators, the
            latter is
            $$
            \begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
            =-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
             -\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}.
            $$
            The two coefficients $-1$ in the hash table are precisely
            those in this equality; its keys are basis vectors, not
            lists of relations.
        Text
            The option @TT "Weight => w"@ truncates to the weight spaces at
            most $w$.  For the squarefree bound $(1,1,1,1)$ the truncation
            generated by the types $(0,0)$ and $(1,1)$ has Hilbert series
            $1 + 12q + 2q^2$ and a quadratic Groebner basis, so it is
            Koszul; the bound $(3,3,2)$ on the three diagonal types has one
            cubic relation.
        Example
            Aw = hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1});
            reduceHilbert hilbertSeries Aw
            hasQuadraticGroebnerBasis Aw
            tally apply(degrees ideal hookAlgebra(3, {{0,0},{1,1},{2,2}}, Weight => {3,3,2}), sum)
        Text
            Thus the squarefree truncation has dimension $1+12+2=15$.
            The last tally instead describes a different presentation:
            $123$ quadratic generators of its ideal and one cubic.
        Text
            Coefficients lie in the field given by @TO CoefficientRing@.
            The straightening formulas divide by integers determined by
            the hook sizes and solve elimination systems.  In positive
            characteristic these divisions or solves can fail; the error
            identifies the integer or system and the pair of generators
            being straightened.  No such failure occurs when the
            characteristic is $0$ or exceeds $|\lambda|$ for every shape
            $\lambda$ straightened.
        Example
            A7 = hookAlgebra(2, {{0,0},{1,1}}, CoefficientRing => ZZ/7);
            coefficientRing A7
            ideal A7
        Text
            @TT "hookAlgebra R"@ presents a hook ring built earlier, with the
            guards of the presentation as options.
        Example
            R = hookRing(2, MaxArm => 1, MaxLeg => 1);
            numgens ideal hookAlgebra R
    Caveat
        The generators are @TT "A_i"@ or, after @TT "use A"@, @TT "z5"@
        (not @TT "z_5"@).  In the quotient ring a product of generators is
        already reduced; take the monomial in @TT "ambient A"@ when the
        product itself is wanted, for instance for @TO tabloid@.
    SeeAlso
        hookRing
        straighteningIdeal
        pieriIdeal
        hasQuadraticGroebnerBasis
        hookBasis
        tabloid
        tabloidTerms
        hookRelation
        hookStraighten
        fineSegreAlgebra
///

doc ///
    Key
        hookTypes
        (hookTypes, PolynomialRing)
        (hookTypes, QuotientRing)
    Headline
        the hook types of a hook ring or algebra
    Usage
        hookTypes R
        hookTypes A
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
    Outputs
        :List
            the hook types @TT "{arm, leg}"@ the ring was built from, in
            the order of the generators (larger hooks first); on a fine
            Segre ring the type pairs @TT "{baseType, leftType, rightType}"@
            in the order of the fine degrees
    Description
        Text
            A pair $(a,b)$ denotes the Schur-functor generator space
            $\mathbf S_{(a+1,1^b)}(\mathbf k^n)$; it is not a partition
            written in row lengths.  On a fine Segre ring each triple
            gives a base label and the two hook spaces paired at that
            label (see @TO TypePairs@).
        Example
            hookTypes hookAlgebra(3, {{0,0},{1,1},{2,2}})
            hookTypes hookRing(2, MaxArm => 1, MaxLeg => 1)
            hookTypes fineSegreRing(2, 3, 2)
        Text
            In particular, the first output lists $(2,2),(1,1),(0,0)$,
            meaning the shapes $(3,1,1),(2,1),(1)$, in variable order.
    SeeAlso
        hookRing
        alphabetSize
        weightBound
        hookBasis
        TypePairs
///

doc ///
    Key
        alphabetSize
        (alphabetSize, PolynomialRing)
        (alphabetSize, QuotientRing)
    Headline
        the number of letters of a hook ring or algebra
    Usage
        alphabetSize R
        alphabetSize A
    Inputs
        R:PolynomialRing
            a hook ring
        A:QuotientRing
            a hook algebra or a quotient of one
    Outputs
        :ZZ
            the $n$ of the alphabet $\{1, \ldots, n\}$, the dimension of
            $V$
    Description
        Text
            A fine Segre ring has two alphabets; apply @TO alphabetSize@
            to its @TO factors@.
        Example
            alphabetSize hookAlgebra(3, {{0,0},{1,1},{2,2}})
            apply(factors fineSegreRing(2, 3, 2), alphabetSize)
    SeeAlso
        hookTypes
        weightBound
        factors
///

doc ///
    Key
        weightBound
        (weightBound, PolynomialRing)
        (weightBound, QuotientRing)
    Headline
        the content bound of a hook ring or algebra
    Usage
        weightBound R
        weightBound A
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
    Outputs
        :List
            the bound given as @TO Weight@ when the ring was built, or
            @TT "null"@ when there is none; on a fine Segre ring the pair
            of the bounds of the two factors
    Description
        Text
            Write the returned bound as $w=(w_1,\ldots,w_n)$, and let
            $c_i(T)$ count the entries of a tableau $T$ equal to $i$.
            The bound imposes $c_i(T)\le w_i$ for $1\le i\le n$,
            and the same inequalities on the total contents of a
            product.  The value @TT "null"@ means there is no bound;
            it does not mean that only weight zero is allowed.
        Example
            weightBound hookAlgebra(4, {{0,0},{1,1}}, Weight => {1,1,1,1})
            weightBound hookRing(2, {{0,0},{1,1}})
            weightBound fineSegreRing(2, 3, 2, LeftWeight => {1,1})
    SeeAlso
        Weight
        weightTruncationIdeal
        hookTypes
///

doc ///
    Key
        (hookBasis, PolynomialRing)
        (hookBasis, QuotientRing)
    Headline
        the generators of a hook ring or algebra as tabloids
    Usage
        hookBasis X
    Inputs
        X:PolynomialRing
            a hook ring or a fine Segre ring; a hook algebra, a fine Segre
            algebra or a quotient of one is accepted too
    Outputs
        :List
            the semistandard hook tableaux indexing the generators, as hook
            shaped @TO HookTabloid@s, in the order of the variables, so that
            the $i$th entry is @TT "tabloid X_i"@; on a fine Segre ring the
            pairs @TT "{T, U}"@ of tabloids indexing the fine generators
    Description
        Text
            On a hook ring without a content bound, these tableaux form a
            basis of the generator space
            $\bigoplus_{(a,b)\in\mathcal T}\mathbf S_{(a+1,1^b)}(V)$,
            not a basis of the entire algebra.  Products of generators
            supply the higher-degree pieces.  With a content bound, only
            the permitted weight vectors are listed.  In the example the list
            contains two basis vectors of $\mathbf S_{(2,1)}(\mathbb Q^2)$
            followed by two of $\mathbb Q^2$.
            For a further quotient, the list still labels the original
            generators, even if additional relations make their classes
            linearly dependent.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            hookBasis R
            A = hookAlgebra R;
            hookBasis A == hookBasis R
            hookVariable(A, first hookBasis A)
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            hookBasis W
        Text
            A pair in the last list denotes the tensor basis vector
            $[T]\otimes[U]$ in the corresponding fine Segre generator
            space.  @TO hookVariable@ converts a tableau or such a
            pair back to its ring generator.
    SeeAlso
        hookRing
        hookVariable
        tabloid
        hookTypes
///

doc ///
    Key
        (hookBasis, PolynomialRing, List)
        (hookBasis, QuotientRing, List)
    Headline
        the generators of one hook type as tabloids
    Usage
        hookBasis(X, type)
    Inputs
        X:PolynomialRing
            a hook ring or a fine Segre ring; a hook algebra, a fine Segre
            algebra or a quotient of one is accepted too
        type:List
            a hook type @TT "{arm, leg}"@ of the ring, or the base-type
            label of a fine degree of a fine Segre ring
    Outputs
        :List
            the generators of that type, as hook shaped @TO HookTabloid@s
            in the order of the variables; on a fine Segre ring the pairs
            of tabloids of that fine degree
    Description
        Text
            For a hook ring without a content bound the result is a
            tableau basis of
            $\mathbf S_{(a+1,1^b)}(V)$ for the requested type $(a,b)$.
            With a content bound only the permitted weight vectors are
            included.  Thus the first call below returns two basis vectors of
            $\mathbf S_{(2,1)}(\mathbb Q^2)$, and the second returns the
            two basis vectors of $\mathbb Q^2$.  On the fine Segre ring,
            base type $(0,0)$ indexes the four tensor basis vectors of
            $\mathbb Q^2\otimes\mathbb Q^2$.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            hookBasis(R, {1,1})
            hookBasis(hookAlgebra R, {0,0})
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            hookBasis(W, {0,0})
    SeeAlso
        hookTypes
        hookVariable
///
doc ///
    Key
        hookVariable
        (hookVariable, PolynomialRing, HookTabloid)
        (hookVariable, QuotientRing, HookTabloid)
        (hookVariable, PolynomialRing, List)
        (hookVariable, QuotientRing, List)
        (hookVariable, PolynomialRing, Filling)
        (hookVariable, QuotientRing, Filling)
    Headline
        the variable of a hook ring indexed by a hook tableau
    Usage
        hookVariable(R, T)
        hookVariable(A, T)
        hookVariable(W, {T, U})
    Inputs
        R:PolynomialRing
            a hook ring, or a fine Segre ring @TT "W"@
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
        T:HookTabloid
            a hook shaped tabloid (one row and one column), also accepted
            as its list of rows or as a @TO Filling@; on a fine Segre ring
            a pair @TT "{T, U}"@ of hook shaped tabloids
    Outputs
        :RingElement
            the variable of the ring indexed by the tableau, as an element
            of the ring given
    Description
        Text
            On the polynomial ring this is the inverse of @TO tabloid@
            on a generator; on a quotient it returns that generator's class.
            The tableau must be one of the generators: semistandard, of one of
            the hook types of the ring, and within its weight bound.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            z = hookVariable(A, tabloid {{1,3,3},{2},{3}})
            tabloid z
            hookVariable(ambient A, {{1,3,3},{2},{3}})
            hookVariable(A, tabloid({3,3}, {1,2,3})) == z
        Text
            All three input formats designate the same basis vector of
            $\mathbf S_{(3,1,1)}(\mathbb Q^3)$, hence the same generator
            $z_5$.  The call with @TT "ambient A"@ returns $z_5$ in
            the polynomial ring, while the calls with $A$ return its
            residue class in the quotient.
        Text
            On a fine Segre ring the generator is indexed by a pair.
        Example
            R = hookRing(2, {{0,0},{1,1}});
            W = fineSegreRing(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            hookVariable(W, {tabloid {{1,2},{2}}, tabloid {{1,1},{2}}})
    SeeAlso
        tabloid
        hookMonomial
        hookBasis
///

doc ///
    Key
        hookMonomial
        (hookMonomial, PolynomialRing, HookTabloid)
        (hookMonomial, QuotientRing, HookTabloid)
        (hookMonomial, PolynomialRing, List)
        (hookMonomial, QuotientRing, List)
        (hookMonomial, PolynomialRing, Filling)
        (hookMonomial, QuotientRing, Filling)
    Headline
        the monomial of a hook tabloid in a hook ring or algebra
    Usage
        hookMonomial(R, T)
        hookMonomial(A, T)
        hookMonomial(W, {T, U})
    Inputs
        R:PolynomialRing
            a hook ring, or a fine Segre ring @TT "W"@
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
        T:HookTabloid
            a hook tabloid, also accepted as its list of rows or as a
            @TO Filling@; on a fine Segre ring a pair @TT "{T, U}"@ of
            tabloids of the same Frobenius rank, with corresponding hooks
            paired by @TO TypePairs@
    Outputs
        :RingElement
            the product of the variables indexed by the diagonal hooks of
            @TT "T"@ (see @TO hookDecomposition@), as an element of the ring
            given; $1$ for the empty tabloid
    Description
        Text
            If the diagonal hooks of $T$ are $H_1,\ldots,H_r$, the
            operation is
            $$T\longmapsto z_{H_1}\cdots z_{H_r}.$$
            Each diagonal hook must itself be a semistandard tableau
            indexing a generator of the ring.  For an input whose hooks
            also need straightening, use @TO hookStraighten@ instead.
            In the hook ring the value is the monomial itself, and
            @TO tabloid@ inverts it when the hook types form a chain.  In
            the algebra the product is reduced by the Groebner basis of the
            straightening ideal, so for a nonsemistandard tabloid the value
            is its normal form, the same element that @TO hookStraighten@
            computes with the straightening algorithm; @TO tabloidTerms@ reads
            it back as tabloids.  On a fine Segre ring the $k$th hooks of
            the two tabloids are paired.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            R = ambient A;
            T = tabloid {{1,3,3},{2,2},{3}}
            m = hookMonomial(R, T)
            tabloid m == T
            hookMonomial(A, T)
            U = tabloid {{1,2,3},{2,1},{3}};
            hookMonomial(R, U)
            hookMonomial(A, U)
            hookMonomial(A, U) == hookStraighten(U, A)
        Text
            The ambient-ring outputs are the unreduced products $z_5z_{15}$
            and $z_4z_{14}$.  In the quotient, respectively, they satisfy
            $$z_5z_{15}=-2z_4z_{16},\qquad
              z_4z_{14}=-z_2z_{15}-z_1z_{16}.$$
            Thus the first tableau is the product of its two diagonal
            hooks even though the whole tableau is not semistandard;
            the quotient output performs the straightening automatically.
    SeeAlso
        hookVariable
        tabloid
        tabloidTerms
        hookStraighten
        hookDecomposition
///

doc ///
    Key
        (tabloid, RingElement)
        (tabloid, IndexedVariable)
    Headline
        the tabloid of a variable or of a chain monomial of a hook ring
    Usage
        tabloid z
    Inputs
        z:RingElement
            a generator of a hook ring, of a fine Segre ring, or of a
            quotient of one, such as @TT "A_i"@ for a hook algebra
            @TT "A"@; or a monomial with coefficient $1$ in the generators
            whose hook types form a chain
    Outputs
        :Thing
            a @TO HookTabloid@ on a hook ring or algebra: the semistandard
            hook tableau indexing the generator, or the hook tabloid whose
            diagonal hooks are the variables of the monomial.  On a fine
            Segre ring or algebra a @TO List@ instead, the pair
            @TT "{T, U}"@ of the left and the right tabloid
    Description
        Text
            Every variable of a hook ring is indexed by a semistandard hook
            tableau; this reverse lookup also works in the algebra or a
            further quotient.  A monomial whose variables have strictly
            nested hook types is a single tabloid, the product of its hooks
            along the diagonal (see @TO hookDecomposition@); @TO hookMonomial@
            is the inverse.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            R = ambient A;
            tabloid R_4
            tabloid A_4
            T = tabloid(R_4 * R_14)
            hookMonomial(R, T) == R_4 * R_14
            hookDecomposition T
        Text
            The displayed $T=\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$
            represents $z_4z_{14}$ in $R$.  The final list separates its
            $(3,1,1)$ hook from its one-box diagonal hook.  This call
            merely translates the product into a tabloid; it does not
            replace it by its semistandard expansion in the quotient.
        Text
            A variable of a fine Segre ring or algebra is indexed by a pair
            of tableaux, one in each factor, and the pair @TT "{T, U}"@ of
            the left and the right tableau is returned.
        Example
            B = fineSegreAlgebra(2, 3, 2);
            tabloid B_0
            (T, U) = toSequence tabloid B_0;
            class T, class U
    Caveat
        The input must be one generator or a monomial with coefficient $1$
        whose types form a chain; sums, scalar multiples and nonchain
        products are rejected, and so is a variable of an unrelated ring.
        In the algebra a product of generators is already reduced to its
        normal form, so take the monomial in the ambient ring; a polynomial
        is read as tabloids by @TO tabloidTerms@.  An indexed variable such as
        @TT "z_5"@ is rejected with a hint to write @TT "z5"@ or
        @TT "R_5"@.
    SeeAlso
        tabloid
        tabloidTerms
        hookVariable
        hookMonomial
        hookBasis
///

doc ///
    Key
        tabloidTerms
        (tabloidTerms, RingElement)
    Headline
        a polynomial of a hook ring or algebra read as tabloids
    Usage
        tabloidTerms f
    Inputs
        f:RingElement
            an element of a hook ring, of a fine Segre ring, or of a
            quotient of one, every monomial of which is a product of
            generators with strictly nested hook types
    Outputs
        :HashTable
            from the @TO HookTabloid@ of each monomial of @TT "f"@ (see
            @TO tabloid@) to its coefficient; on a fine Segre ring the keys
            are pairs of tabloids
    Description
        Text
            An element of the algebra is a combination of standard
            monomials, that is, of semistandard hook tabloids; this reads
            the combination.  Applied to @TO hookStraighten@ or to a product
            in the algebra it gives the normal form of a tabloid in the
            same form as the straightening functions on tabloids return it.
            More explicitly, a hash table with entries $T\mapsto c_T$
            denotes $\sum_T c_T[T]$, where $[T]$ is the product of the
            diagonal hook generators of $T$.  In the ambient polynomial
            ring this only changes notation; it does not straighten.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            R = ambient A;
            tabloidTerms(R_4 * R_14 - 3 * R_0 * R_12 * R_16)
            tabloidTerms(A_4 * A_14)
            tabloidTerms hookStraighten(tabloid {{1,2,3},{2,1},{3}}, A)
        Text
            The first output means
            $$
            z_4z_{14}-3z_0z_{12}z_{16}
            =\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
             -3\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
            $$
            as a relabeling of monomials in $R$.  The next two outputs
            instead give the same equality in the quotient $A$:
            $$
            \begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
            =-\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}
             -\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}.
            $$
    SeeAlso
        tabloid
        hookStraighten
        hookMonomial
        HookTabloid
///

doc ///
    Key
        (hookStraighten, Thing, Ring)
    Headline
        the normal form of a tabloid as an element of a hook algebra
    Usage
        hookStraighten(T, A)
    Inputs
        T:Thing
            a hook tabloid, as a @TO HookTabloid@, a list of rows or a
            @TO Filling@, whose hook types are among the types of the
            algebra
        A:Ring
            a hook algebra, its hook ring, or a quotient of the algebra
    Outputs
        :RingElement
            the normal form of @TT "T"@ as an element of the algebra
    Description
        Text
            The straightening algorithm of Section 6 of the paper is run on
            @TT "T"@ (the same computation as the tabloid forms of
            @TO hookStraighten@, over the field of the algebra) and the
            resulting combination of semistandard tabloids is read into the
            algebra with @TO hookMonomial@; a tabloid whose content exceeds
            the weight bound of the algebra is zero.  The Groebner basis of
            the straightening ideal reduces the monomial of @TT "T"@ to the
            same element, which is what the product in the algebra
            computes.  For the default paper order, the straightening
            relations themselves are a Groebner basis, and these outputs
            have exactly the same standard-monomial expression (see
            @TO hasQuadraticGroebnerBasis@).
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            T = tabloid {{1,2,3},{2,1},{3}}
            f = hookStraighten(T, A)
            f == hookMonomial(A, T)
            tabloidTerms f
            hookStraighten(tabloid {{1,2,3},{2,3},{3}}, A)
            hookStraighten({{2,1,3},{1,2},{3}}, A)
        Text
            The output @TT "f"@ gives the identity
            $[T]=-z_2z_{15}-z_1z_{16}$ in $A$; the hash table lists
            the two corresponding standard tableaux with coefficients
            $-1$.  The next input is already semistandard and represents
            $z_4z_{16}$.  The last, whose first hook itself needs
            straightening, represents $-z_2z_{15}$.
    SeeAlso
        hookStraighten
        (hookStraighten, RingElement)
        hookMonomial
        tabloidTerms
        straighteningIdeal
        hasQuadraticGroebnerBasis
///

doc ///
    Key
        (hookStraighten, RingElement)
    Headline
        the normal form of a polynomial as an element of a hook algebra
    Usage
        hookStraighten f
    Inputs
        f:RingElement
            an element of a hook ring, of a fine Segre ring, or of a
            quotient of one
    Outputs
        :RingElement
            the normal form of @TT "f"@ as an element of the algebra of its
            ring
    Description
        Text
            An element of the hook ring is reduced modulo the straightening
            ideal by the Groebner basis of the algebra, and returned as an
            element of the algebra; an element of the algebra or of a
            quotient of it is already reduced and is returned as it is.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            R = ambient A;
            hookStraighten(R_4 * R_14)
            hookStraighten(R_4 * R_14 - hookRelation(R, 4, 14))
            hookStraighten A_4
            tabloidTerms hookStraighten(R_4 * R_14)
        Text
            Both of the first two polynomials have residue class
            $-z_2z_{15}-z_1z_{16}$ in $A$.  Their difference is
            @TT "hookRelation(R, 4, 14)"@, so the equality of their
            normal forms expresses precisely that this relation lies
            in the kernel of the quotient map $R\longrightarrow A$.
    SeeAlso
        (hookStraighten, Thing, Ring)
        tabloidTerms
        hookRelation
        straighteningIdeal
///
doc ///
    Key
        hookRelation
        (hookRelation, PolynomialRing, ZZ, ZZ)
        (hookRelation, QuotientRing, ZZ, ZZ)
    Headline
        the straightening relation of a pair of generators
    Usage
        hookRelation(R, i, j)
        hookRelation(A, i, j)
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
        i:ZZ
        j:ZZ
            zero based indices of two generators
    Outputs
        :RingElement
            the relation in @TO straighteningIdeal@ for the product of the
            two generators, as an element of the hook ring: the product
            minus its normal form for a
            straightening pair, the product itself for a nonchain or an
            overweight pair, and $0$ for a standard pair
    Description
        Text
            The relations are read from the record of the straightening
            presentation, which is built on the first call; @TO relationKind@
            names the case.  The normal form of the product is the product
            minus the relation, and @TO tabloidTerms@ reads it as tabloids.
            With the paper order, the product is the leading monomial of
            each nonzero relation; this need not hold for another order.
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            R = ambient A;
            f = hookRelation(R, 4, 14)
            relationKind(R, 4, 14)
            tabloidTerms(R_4 * R_14 - f)
        Text
            Here the polynomial returned is
            $$f=z_4z_{14}+z_2z_{15}+z_1z_{16}\in R.$$
            The equation $f=0$ in $A$ therefore gives
            $z_4z_{14}=-z_2z_{15}-z_1z_{16}$.  Subtracting $f$ from
            the original product extracts exactly this right-hand side;
            the hash table records its two standard tableaux, each with
            coefficient $-1$.
        Text
            For a nonchain pair the relation is the product itself, and for
            a standard pair, whose product is already a semistandard
            tabloid, it is zero:
        Example
            hookRelation(A, 0, 0)
            hookRelation(A, 4, 16)
            tabloid(R_4 * R_16)
    SeeAlso
        relationKind
        straighteningIdeal
        tabloidTerms
        hookStraighten
///

doc ///
    Key
        relationKind
        (relationKind, PolynomialRing, ZZ, ZZ)
        (relationKind, QuotientRing, ZZ, ZZ)
    Headline
        the kind of the product of two generators
    Usage
        relationKind(R, i, j)
        relationKind(A, i, j)
    Inputs
        R:PolynomialRing
            a hook ring or a fine Segre ring
        A:QuotientRing
            a hook algebra, a fine Segre algebra or a quotient of one
        i:ZZ
        j:ZZ
            zero based indices of two generators
    Outputs
        :String
            on a hook ring one of @TT "\"standard\""@ (the product is a
            semistandard nested pair), @TT "\"straightening\""@ (a
            nonsemistandard nested pair, rewritten by its normal form),
            @TT "\"nonchain\""@ (the types are not strictly nested, so the
            product is zero) and @TT "\"weight-truncation\""@ (the content
            exceeds the weight bound); on a fine Segre ring one of
            @TT "\"standard\""@, @TT "\"zero:base-nonchain\""@,
            @TT "\"zero:weight\""@, @TT "\"bad:left\""@,
            @TT "\"bad:right\""@ and @TT "\"bad:both\""@, naming the factor
            in which the pair fails to be semistandard
    Description
        Text
            The kinds are those of the records of the straightening
            presentation (see @TO straighteningIdeal@); a tally over all
            pairs classifies the quadratic products, including the standard
            ones that require no relation.  The pair
            $(4,14)$ is the straightening relation of Example 1.2.2 of the
            paper, the pair $(4,16)$ is standard (its tabloid $\begin{array}{l}\fcolorbox{black}{white}{\phantom{|}1\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}2\phantom{|}}\!\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\\[-2pt]\fcolorbox{black}{white}{\phantom{|}3\phantom{|}}\end{array}$ is
            semistandard), and a generator with itself is a nonchain pair,
            since two hooks of the same type are not strictly nested:
        Example
            A = hookAlgebra(3, {{0,0},{1,1},{2,2}});
            relationKind(A, 4, 14)
            relationKind(A, 4, 16)
            relationKind(A, 0, 0)
            tally flatten for i from 0 to 16 list for j from i to 16 list relationKind(A, i, j)
        Text
            On a fine Segre ring the kinds name the factor in which a pair
            fails to be semistandard.  In the fine Segre product of the
            hook algebra of types $(1,1)$ and $(0,0)$ on two letters with
            itself, each type paired with itself, one pair is standard, the
            bad pairs come in all three kinds, and the pairs whose base
            types are not strictly nested, every square among them, are
            zero:
        Example
            R = hookRing(2, {{0,0},{1,1}});
            B = fineSegreAlgebra(R, R, TypePairs => {{{0,0},{0,0},{0,0}},{{1,1},{1,1},{1,1}}});
            relationKind(B, 0, 7), relationKind(B, 1, 6), relationKind(B, 2, 5), relationKind(B, 3, 4), relationKind(B, 0, 0)
            tally flatten for i from 0 to 7 list for j from i to 7 list relationKind(B, i, j)
    SeeAlso
        hookRelation
        straighteningIdeal
        nonstandardMonomialIdeal
///
