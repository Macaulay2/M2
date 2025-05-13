doc ///
Node
  Key
    "a first Macaulay2 session"
  Description
    Text
      Macaulay2 begins with printing its version number and preloaded packages.
      After that, your first input prompt will be @KBD "i1 : "@.

      In response to the prompt, type @KBD "2+2"@ and press @KBD "Enter"@.
      The expression you entered will be evaluated — no punctuation is required at the end of the line.
    Example
      2+2
    Text
      The answer is displayed to the right of the output label @KBD "o1 ="@.
    Text
      Here is some arithmetic with fractions.
    Example
      3/5 + 7/11
    Text
      Notice the additional line of output labeled with @KBD "o2 :"@.  Output lines labeled with colons
      provide information about the type of output.  In this case, the symbol @TO QQ@ is our notation for
      the class of all rational numbers, and indicates that the answer on the previous line is a rational
      number.
    Text
      Multiplication is indicated with @TO "*"@.
    Example
      1*2*3*4
    Text
      Powers are obtained with @TO "^"@.
    Example
      2^200
    Text
      Factorials are obtained with @TO "!"@.
    Example
      40!
    Text
      Because some answers can be very long, it is a good idea to run the program in a window that does
      not wrap output lines, and allows the user to scroll left horizontally to see the rest of the
      output.  (See @TO "using Macaulay2 with Emacs"@.)
    Example
      100!
    Text
      Multiple expressions may be separated by semicolons.
    Example
      1;2;3*4
    Text
      A semicolon at the end of the line suppresses the printing of the value.
    Example
      4*5;
    Text
      The output from the previous line can be obtained with @TO "oo"@, even if a semicolon prevented it
      from being printed.
    Example
      oo
    Text
      Lines before that can be obtained with @TO "ooo"@ and @TO "oooo"@. Alternatively, the symbol labeling an
      output line can be used to retrieve the value, as in the following example.
    Example
      o5 + 1
    Text
      To enter a string, use quotation marks.
    Example
      "hi there"
    Text
      A value can be assigned to a variable with @TO "="@.
    Example
      s = "hi there"
    Text
      Strings may be concatenated horizontally with @TO "|"@ (see @TO (symbol |, String, String)@).
    Example
      s | " | " | s
    Text
      or vertically with @TO "||"@ (see @TO (symbol ||, Net, Net)@).
    Example
      s || "--------" || s
    Text
      A list of expressions can be formed with braces.
    Example
      {1, 2, s}
    Text
      Lists behave like vectors.
    Example
      10*{1,2,3} + {1,1,1}
    Text
      A function can be created with the arrow operator, @TO "->"@ .
    Example
      f = i -> i^3
    Text
      To evaluate a function, place its argument to the right of the function.
    Example
      f 5
    Text
      Functions of more than one variable take a parenthesized sequence of arguments.
    Example
      g = (x,y) -> x * y
      g(6,9)
    Text
      The function @TO apply@ can be used to apply a function to each element of a list.
    Example
      apply({1,2,3,4}, i -> i^2)
      apply({1,2,3,4}, f)
    Text
      The operator @TO ".."@ may be used to generate sequences of consecutive numbers.
    Example
      apply(1 .. 4, f)
    Text
      If the first argument to @TT "apply"@ is an integer @TT "n"@ then it stands for the list @TT "{0, 1,
      ..., n-1}"@.
    Example
      apply(5, f)
    Text
      The function @TO scan@ is analogous to @TT "apply"@ except that no value is returned.  It may be used
      to implement loops in programs.
    Example
      scan(5, i -> print (i, i^3))
      j=1; scan(10, i -> j = 2*j); j
    Text
      Most computations with polynomials take place in rings that may be specified in usual mathematical
      notation.
    Example
      R = ZZ/5[x,y,z];
    Text
      Caution: we reserve single letter symbols such as @TT "Z"@ for use as variables in rings, hence we must
      use something like @TO ZZ@ to stand for the ring of integers. It may remind you of the "blackboard bold"
      font of $\mathcal{A\kern-.1667em}\raisebox{-.5ex}{$\mathcal M$\kern-.125em}\mathcal{S}\textrm{-}\TeX$.
      If you prefer @TT "Z"@ to @TO ZZ@, you may put @TT "Z=ZZ"@ in your @TO "initialization file"@.
      The symbols @TT "ZZ/5"@ represent the quotient ring $\ZZ/5\ZZ$, and @TT "ZZ/5[x,y,z]"@ represents
      the ring of polynomials in the variables $x$,$y$, and $z$ with coefficients in the ring $\ZZ/5\ZZ$.
    Example
      (x+y)^5
    Text
      Rings and certain other types of things acquire the name of the global variable they are assigned
      to.
    Example
      R
    Text
      To see the original description of a ring, use @TO describe@.
    Example
      describe R
    Text
      A free module can be created as follows.
    Example
      F = R^3
    Text
      The i-th basis element of @TT "F"@ can be obtained as @TT "F_i"@.  In this example, the valid values
      for @TT "i"@ are 0, 1, and 2.
    Example
      F_1
    Text
      Using a list of indices instead will produce the homomorphism corresponding to the basis vectors
      indicated.
    Example
      F_{1,2}
    Text
      Repetitions are allowed.
    Example
      F_{2,1,1}
    Text
      We can create a homomorphism between free modules with @TO matrix@ by providing the list of rows of
      the matrix, each of which is in turn a list of ring elements.
    Example
      f = matrix {{x,y,z}}
    Text
      Use @TO image@ to get the image of f.
    Example
      image f
    Text
      We may use @TO ideal@ to produce the corresponding ideal.
    Example
      ideal (x,y,z)
    Text
      We may use @TO kernel@ to compute the kernel of f.
    Example
      kernel f
    Text
      The answer comes out as a module that is expressed as the image of a homomorphism whose matrix is
      displayed. This is one way of @TO "making modules from matrices"@. Integers inside braces to the
      left of the matrix give the degrees of the basis elements of the target of the matrix; they are
      omitted if the degrees are all zero. In case the matrix itself is desired, it can be obtained
      with @TO generators@, as follows.
    Example
      generators oo
    Text
      We may use @TO poincare@ to compute the Poincare polynomial.
    Example
      poincare kernel f
    Text
      We may use @TO rank@ to compute the rank.
    Example
      rank kernel f
    Text
      A presentation for the kernel can be obtained with @TO presentation@.
    Example
      presentation kernel f
    Text
      We can produce the cokernel with @TO cokernel@; no computation is performed.
    Example
      cokernel f
    Text
      The direct sum is formed with @TO (symbol ++, Module, Module)@.
    Example
      N = kernel f ++ cokernel f
    Text
      The answer is expressed in terms of the @TO subquotient@ function, which produces @TO "subquotient modules"@.
      Each subquotient module is accompanied by its matrix of generators and its matrix of
      relations.  These matrices can be recovered with @TO generators@ and @TO relations@.
    Example
      generators N
      relations N
    Text
      The function @TO prune@ can be used to convert a subquotient module to a quotient module.
    Example
      prune N
    Text
      We can use @TO "OldChainComplexes :: resolution"@ to compute a projective resolution of the cokernel of @TT "f"@.
    Example
      C = resolution cokernel f
    Text
      -- TODO: will change to dd^C soon
      To see the differentials we examine @KBD "C.dd"@.
    Example
      C.dd
    Text
      We can verify that @TT "C"@ is a complex by squaring the differential map.
    Example
      C.dd^2 == 0
    Text
      We can use @TO betti@ to see the degrees of the components of @TT "C"@.
    Example
      betti C
    Text
      Let's try a harder example.  We can use @TO symbol ..@ to create a sequence of variables.
    Example
      R = ZZ/101[a .. r];
    Text
      We use @TO genericMatrix@ to make a 3 by 6 generic matrix whose entries are drawn from the variables
      of the ring @TT "R"@.
    Example
      g = genericMatrix(R,a,3,6)
    Text
      Then we construct its cokernel with @TO cokernel@.
    Example
      M = cokernel g
    Text
      We may use @TO "OldChainComplexes :: resolution"@ to produce a projective resolution of it,
      and @TO "time"@ to report the time required.
    Example
      time C = resolution M
    Text
      As before, we may examine the degrees of its components, or display it.
    Example
      betti C
    Text
      We can make a polynomial ring with 18 @TO2(IndexedVariable, "indexed variables")@.
    Example
      S = ZZ/101[t_1 .. t_9, u_1 .. u_9];
    Text
      We can use @TO genericMatrix@ to pack the variables into 3-by-3 matrices.
    Example
      m = genericMatrix(S, t_1, 3, 3)
      n = genericMatrix(S, u_1, 3, 3)
    Text
      We may look at the matrix product.
    Example
      m*n
    Text
      Let's use @TO flatten@ to produce the equations generated by the equations that assert that $m$ and $n$ commute with each
      other.
    Example
      j = flatten(m*n - n*m)
    Text
      Let's compute a Gröbner basis for the image of @TT "j"@ with @TO gb@.
    Example
      gb j
    Text
      The resulting Gröbner basis contains a lot of information, which may be very long to print,
      so only a summary is printed. We can get the generators of the basis using @TO generators@,
      and even though we call upon @TO gb@ again, the computation will not be repeated.
    Example
      generators gb j;
    Text
      The semicolon prevents the matrix of generators from appearing on the screen, but the class of the
      matrix appears — we see that there are 26 generators.
    Text
      We can use @TO betti@ to see the degrees involved in the Gröbner basis.
    Example
      betti gb j
    Text
      To exit your first session, you can type @KBD "exit"@ or @KBD "quit"@.
  SeeAlso
    "reading the documentation"
    "BeginningMacaulay2::BeginningMacaulay2"
///
