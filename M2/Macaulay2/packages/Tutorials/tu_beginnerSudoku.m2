doc ///
Node
  Key
    "Beginner tutorial: Shidoku"
  Headline
    A first M2 session solving a 4x4 Sudoku
  Description
    Text
      This tutorial is for a new Macaulay2 user who has limited knowledge of commutative algebra.
      The goal is to show how Macaulay2 can turn a system of polynomial equations
      into useful information.

      The mathematical problem is a 4-by-4 Sudoku puzzle,
      sometimes called Shidoku. Instead of solving it by hand,
      we encode the rules as polynomial equations and ask Macaulay2 to solve the resulting system.
    Text
      @BOLD "Starting Macaulay2"@

      After starting Macaulay2, you should see something like this:

    Pre
      i1 : restart

    Text
      Macaulay2 labels each input line with @TT "i1, i2"@ and so on.
      It labels each output line with @TT "o1, o2"@ and so on.

      Try a few basic commands:

    Example
      2+3
      2^10
      factor 360

    Text
	While Macaulay2 can be used as a calculator,
	its main strength is computation with algebraic objects
	like polynomial rings, ideals, modules, maps, complexes, and related structures.
    Text
      @BOLD "Finding help"@

    Text
      A good first habit is to ask Macaulay2 what it knows.

    Example
      help ideal

    Text
      If you do not know the exact name of a command, try @TT "apropos"@ or @TT "about"@.

    Example
      apropos "Groebner"

    Text
      In an interactive session, @TT "viewHelp ideal"@ opens the documentation
      page in a browser, and @TT "examples ideal"@ shows examples attached to
      that documentation entry.

    Text
      @BOLD "The puzzle"@

    Text
      Here is a 4-by-4 Sudoku puzzle we will solve.
      The entries are the numbers 1, 2, 3, and 4.
      The boxes are 2 rows by 2 columns.

    Pre
      +-----+-----+
      | 1 . | . 4 |
      | . 4 | 1 . |
      +-----+-----+
      | . 1 | 4 . |
      | 4 3 | . 1 |
      +-----+-----+

    Text
      Here a dot @TT "."@ means that the entry is not yet known.

    Text
      @BOLD "The rules"@

    Text
      A completed Shidoku board must satisfy the following conditions:

      @UL {
        LI "Each cell contains one of 1, 2, 3, 4.",
        LI "Each row contains each number exactly once.",
        LI "Each column contains each number exactly once.",
        LI "Each 2-by-2 box contains each number exactly once.",
        LI "The given clues must be preserved."
      }@

    Text
      We will translate these rules into polynomial equations.

    Text
      @BOLD "The algebraic idea"@

    Text
      We work over the finite field @TT "ZZ/5"@. Its nonzero elements are
      $1,2,3,4$, which are exactly the four symbols allowed in a Shidoku puzzle.

    Text
      If @TT "x"@ is a variable representing one cell, then the equation

    Pre
      x^4 - 1 = 0

    Text
      forces @TT "x"@ to be one of the nonzero elements of @TT "ZZ/5"@.

    Text
      To say that two cells @TT "x"@ and @TT "y"@ contain different symbols, we use

    Pre
      (x - y)^4 - 1 = 0

    Text
      This works because every nonzero element @TT "a"@ of @TT "ZZ/5"@ satisfies
      $a^4 = 1$. Thus the equation above says that @TT "x-y"@ is nonzero, so
      @TT "x"@ and @TT "y"@ must be different.

    Text
      In this way, a Shidoku puzzle becomes a system of polynomial equations whose
      solutions are exactly the completed boards satisfying all the rules.

    Text
      @BOLD "Variables for the cells"@

    Text
      We represent the unknown board by a 4-by-4 matrix of variables
      $X = (x_(i,j))$ for $1 \le i,j \le 4$. Here @TT "x_(i,j)"@ is the entry
      in row $i$ and column $j$.

    Pre
      | x_(1,1)  x_(1,2)  x_(1,3)  x_(1,4) |
      | x_(2,1)  x_(2,2)  x_(2,3)  x_(2,4) |
      | x_(3,1)  x_(3,2)  x_(3,3)  x_(3,4) |
      | x_(4,1)  x_(4,2)  x_(4,3)  x_(4,4) |

    Text
      @BOLD "Solving with polynomial constraints"@

    Text
      We begin by creating a polynomial ring over @TT "ZZ/5"@ with one variable
      for each cell. The shorthand @TT "x_(1,1)..x_(4,4)"@ creates all 16 variables.

    Example
      R = ZZ/5[x_(1,1)..x_(4,4), MonomialOrder => Lex]
      R_*

    Text
      The list @TT "R_*"@ contains the 16 variables in row-major order:
      first the four variables in row 1, then the four in row 2, and so on.
      This is enough structure for the tutorial, so we do not actually need to
      build a separate matrix @TT "X"@.

      Next we build some helper lists.

      The function @TT "pairwiseDifferent"@ takes a list of cells and writes the
      equations forcing all pairs to be different.
      Then @TT "rows"@, @TT "cols"@, and @TT "boxes"@ collect the four rows,
      four columns, and four 2-by-2 boxes of the puzzle.
      The helper @TT "distinctnessRules"@ applies @TT "pairwiseDifferent"@ to a
      whole family of lists.

    Example
      pairwiseDifferent = L -> toList flatten for i from 0 to #L-1 list
                              for j from i+1 to #L-1 list
                                (L#i - L#j)^4 - 1;
      rows = apply(0..3, i -> R_*_{4*i..4*i+3});
      cols = apply(0..3, j -> apply(0..3, i -> R_*#(4*i+j)));
      boxes = apply({0,2,8,10}, k -> {R_*#k, R_*#(k+1), R_*#(k+4), R_*#(k+5)});
      distinctnessRules = F -> toList flatten apply(F, pairwiseDifferent);

    Text
      Now we translate the Shidoku rules into polynomial equations.

      The list @TT "cellRules"@ forces each variable to be one of
      @TT "1,2,3,4"@. The lists @TT "rowRules"@, @TT "colRules"@, and
      @TT "boxRules"@ enforce the distinctness conditions for rows, columns,
      and boxes.

    Example
      cellRules = apply(R_*, x -> x^4 - 1);
      rowRules = distinctnessRules rows;
      colRules = distinctnessRules cols;
      boxRules = distinctnessRules boxes;

    Text
      Finally we encode the given clues.

      The clue list uses the usual 1-based row and column numbers. Since list
      positions in Macaulay2 start at 0, we subtract 1 when converting each
      clue into an index of @TT "R_*"@.

    Example
      clues = {(1,1,1), (1,4,4),
               (2,2,4), (2,3,1),
               (3,2,1), (3,3,4),
               (4,1,4), (4,2,3), (4,4,1)};
      clueRules = apply(clues, clue -> R_*#(4*(clue#0-1) + (clue#1-1)) - clue#2);
      I = ideal(cellRules | rowRules | colRules | boxRules | clueRules);

    Text
      The ideal @TT "I"@ now contains all the equations describing the puzzle.
      To solve the puzzle, we compute a Gröbner basis for @TT "I"@.

    Example
      transpose gens gb I
      dim I
      degree I

    Text
      The Gröbner basis consists of 16 linear equations, one for each cell.
      For example, @TT "x_(1,2)-2"@ says that the entry in row 1, column 2 is
      2. The equation @TT "x_(1,3)+2"@ says that @TT "x_(1,3)=-2"@, which means
      @TT "x_(1,3)=3"@ in @TT "ZZ/5"@ because @TT "-2 = 3"@ in that field.

      The output @TT "dim I = 0"@ tells us that the ideal defines a
      zero-dimensional solution set, so there are only finitely many solutions.
      The output @TT "degree I = 1"@ then tells us that there is exactly one
      solution, so the Shidoku puzzle has a unique completion.

    Text
      @BOLD "Reading the completed board"@

    Text
      Reading the 16 linear equations from the Gröbner basis gives the unique
      completed Shidoku board:

    Pre
      +-----+-----+
      | 1 2 | 3 4 |
      | 3 4 | 1 2 |
      +-----+-----+
      | 2 1 | 4 3 |
      | 4 3 | 2 1 |
      +-----+-----+

    Text
      @BOLD "E. Directions for further exploration"@

    Text
      Possible follow-up directions for the finished tutorial might include:

      @UL {
        LI "using a full 9x9 encoding",
        LI "comparing symbolic and numerical approaches",
        LI "testing uniqueness of a solution",
        LI "using the tutorial as a first introduction to ideals and Gröbner bases"
      }@
///
