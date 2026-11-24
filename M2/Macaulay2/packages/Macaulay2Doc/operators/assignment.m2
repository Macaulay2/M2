doc ///
  Key
    symbol <-
    (symbol <-, IndexedVariable)
    (symbol <-, Sequence)
  Headline
    assignment with left side evaluated
  Synopsis
    Heading
      assignment to symbols
    Usage
      x <- e
    Inputs
      x:Symbol
        (evaluated)
      e:Thing
    Outputs
      :
        the value of the expression is the value of @TT "e"@
    Consequences
      Item
        assuming the value of @TT "x"@ is a symbol, @TT "t"@, say, the value of
        @TT "e"@ is assigned to @TT "t"@ so that future references to the value
        of @TT "t"@ yield @TT "e"@.
    Description
      Example
        x = t
        x <- 4
        x
        t
      Text
        Initially, the value of a symbol @TT "y"@ is @TT "y"@ itself, and then
        @TT "y = e"@ and @TT "y <- e"@ do the same thing.
      Example
        y <- 44
        y
      Text
        The code to the left of the arrow can be any expression whose value is
        a symbol.
      Example
        f = () -> symbol z
        (f()) <- 44
        z
  Synopsis
    Heading
      installing new assignment methods
    Usage
      installMethod(symbol <-, X, (x,e) -> ...)
    Inputs
      X:Type
      :
        @TT "(x,e) -> ..."@, @ofClass Function@ of two arguments
    Outputs
      :
        the value returned is the function provided
    Consequences
      Item
        the function @TT "(x,e) -> ..."@ is installed as the method for
        assignment to objects of type @TT "X"@. See the next subsection below
        for using it.
    Description
      Example
        installMethod(symbol <-, String, peek)
  Synopsis
    Heading
      using installed assignment methods
    Usage
      x <- e
    Inputs
      x:
        an object of type X
      e:Thing
    Outputs
      :
        the value of the expression is the value returned by the previously
        installed method
    Consequences
      Item
        the previously installed method for assignment to objects of type
        @TT "X"@ is called with arguments @TT "(x,e)"@, unless @TT "x"@ is a
        symbol, in which case the internal assignment method applies, as
        described above. If there is no method for @TT "X"@, then the ancestors
        of @TT "X"@ are consulted, starting with the parent of @TT "X"@ and
        ending with @TO "Thing"@ (see @TO "inheritance"@).
    Description
      Text
        The following example used the method for assignment to strings
        installed above.
      Example
        "foo" <- "bar"
      Text
        As before, the left hand side is evaluated, and in this case, it can be
        any expression whose value is a string.
      Example
        "foo" | "foo" <- "bar"
  Synopsis
    Heading
      assignment to an indexed variable
    Usage
      x <- e
    Inputs
      x:IndexedVariable
      e:Thing
    Outputs
      :
        the value of the expression is the value returned by the previously
        installed method
    Consequences
      Item
        assuming the value of @TT "x"@ is @ofClass IndexedVariable@, the value
        of e is assigned to it, so that future references to @TT "value x"@ or
        to @TT "s_i"@, if that's what the value of @TT "x"@ is, yield @TT "e"@.
        Moreover, the value of @TT "s"@ is set to @TT "s"@.
    Description
      Text
        This assignment method is pre-installed.
      Example
        u = s_4
        s = 3
        u <- 555
        s
        s_4
        u
        value u
  Synopsis
    Heading
      parallel assignment
    Usage
      x <- e
    Inputs
      x:Sequence
      e:Sequence
    Outputs
      :
        the value of the expression is the value of @TT "e"@
    Consequences
      Item
        assuming the values of @TT "x"@ and of @TT "e"@ are @ofClass Sequence@,
        with the same length, each member of @TT "e"@ is assigned to the
        corresponding member of @TT "x"@
    Description
      Text
        This assignment method is pre-installed.
      Example
        (symbol a, symbol b) <- (3,4)
        a
        (symbol r_1 .. symbol r_3) <- (5,6,7)
        r_2
  SeeAlso
    "="
    ":="
    (symbol <-, IndexedVariable)
///

doc ///
  Key
    symbol =
  Headline
    assignment
  Description
    Text
      In this section we'll discuss simple assignment to variables, multiple
      assignment, assignment to parts of objects, assignment covered by various
      other methods, and briefly touch on the possibility of custom
      installation of assignment methods. See also the operator @TO ":="@,
      which handles assignment and declaration of local variables and
      assignment of methods to operators, as well as the operator @TO "<-"@,
      which is an assignment operator that evaluates its left hand side and can
      have assignment methods installed for it by the user.
  Synopsis
    Heading
      simple assignment
    Usage
      x = e
    Inputs
      x:Symbol
        (unevaluated)
      e:Thing
    Outputs
      :Thing
        the value of the expression is @TT "e"@
    Consequences
      Item
        @TT "e"@ is assigned to @TT "x"@, so future references to the value of
        @TT "x"@ yield @TT "e"@
      Item
        if @TT "x"@ is a global variable, then the global assignment hook for
        the class of @TT "e"@, if any, is run (see @TO "GlobalAssignHook"@),
        and the global assignment hook for the symbol itself (see
        @TO "globalAssignmentHooks"@), if any, is run.
    Description
      Example
        x
        x = 4
        x
      Text
        Since the value of the entire expression is @TT "e"@, and since the
        operator @TO "="@ is right-associative (see
        @TO "precedence of operators"@), @TT "e"@ can be easily assigned to
        more than one variable, as in the following example.
      Example
        x = y = 44
        x
        y
  Synopsis
    Heading
      multiple assignment
    Usage
      (x,y,z,...) = (c,d,e,...)
    Inputs
      :
        @TT "(x,y,z,...)"@ a @TO2 {Sequence, "sequence"}@ of
        @TO2 {Symbol, "symbols"}@ (unevaluated)
      :
        @TT "(c,d,e,...)"@ a @TO2 {Sequence, "sequence"}@ of
        @TO2 {Thing, "things"}@
    Outputs
      :
        the value of the expression is @TT "(c,d,e,...)"@
    Consequences
      Item
        the expressions c,d,e,... are assigned to the variables x,y,z,...,
        respectively, as above. Global assignment hooks may be run, as above.
        The number of expressions must match the number of variables.
    Description
      Text
        Multiple assignment makes it easy to switch the values of two
        variables, or to permute the values of several.
      Example
        x = 444
        y = foo
        (y,x) = (x,y)
        x
        y
      Text
        Multiple assignment enables functions to return multiple values
        usefully. See @TO "making functions with multiple return values"@.
      Example
        f = i -> (i,i^2)
        (x,y) = f 9
        x
        y
      Text
        Multiple assignment may also be used for other uses of @CODE "="@ such
        as assigning to elements of mutable lists and hash tables and
        using/installing assignment methods.
  Synopsis
    Heading
      assignment to an element of a mutable list
    Usage
      x#i = e
    Inputs
      x:MutableList
      i:ZZ
      e:Thing
    Outputs
      :
        the value of the expression is @TT "e"@
    Consequences
      Item
        the @TT "i"@-th element of the list @TT "x"@ is replaced by @TT "e"@,
        so that future references to the value of @TT "x#i"@ yield @TT "e"@
    Description
      Example
        x = new MutableList from a .. e
        peek x
        x#3
        x#3 = "foo"
        x#3
        peek x
  Synopsis
    Heading
      assignment to an element of a mutable hash table
    Usage
      x#i = e
    Inputs
      x:MutableList
      i:Thing
      e:Thing
    Outputs
      :
        the value of the expression is @TT "e"@
    Consequences
      Item
        @TT "e"@ is stored in the hash table @TT "x"@ under the key @TT "i"@,
        so that future references to the value of @TT "x#i"@ yield @TT "e"@
      Item
        future references to the value of @TT "x#?i"@ will yield the value
        @TO "true"@, indicating that something has been stored in @TT "x"@
        under the key @TT "i"@. See @TT "#?"@.
    Description
      Example
        x = new MutableHashTable from { "a" => 2, "b" => 3 }
        peek x
        x#?"foo"
        x#"foo" = "bar"
        x#?"foo"
        x#"foo"
        peek x
  Synopsis
    Heading
      installing assignment methods for binary operators
    Usage
      X OP Y = (x,y,e) -> ...
    Inputs
      X:Type
      OP:
        one of the binary operators for which users may install methods,
        namely:
        @between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Binary and operatorAttributes#op#Binary#?Flexible), s -> TO {s})@
        . The operator SPACE, indicating adjacency, may be omitted from the
        usage above.
      Y:Type
      :
        @TT "(x,y,e) -> ..."@, @ofClass Function@
    Outputs
      :
        the value of the expression is the same as the function on the right
        hand side
    Consequences
      Item
        the function on the right hand side is installed as the method for
        assignment to @TT "X OP Y"@. See the next subsection below for using it
    Description
      Text
        The first line of the following example illustrates the syntax above.
      Example
        String * String = peek; -* no-capture-flag *-
        "left" * "right" = "value"
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using assignment methods for binary operators
    Usage
      x OP y = e
    Inputs
      x:
        an object of type @TT "X"@
      OP:
        one of the binary operators for which users may install methods, listed
        above. The operator SPACE, indicating adjacency, may be omitted from
        the usage above.
      y:
        an object of type @TT "Y"@
      e:Thing
    Outputs
      :
        the previously installed method for assignment to @TT "X OP Y"@ is
        called with arguments @TT "(x,y,e)"@, and its return value is returned.
        If no such method has been installed, then Macaulay2 searches for a
        method for assignment to @TT "X' OP Y'"@, where @TT "X'"@ is an
        ancestor of @TT "X"@ and @TT "Y'"@ is an ancestor of @TT "Y"@ (see
        @TO "inheritance"@ for details).
    Description
      Text
        The return value and the consequences depend on the code of the
        installed assignment method. References to currently installed
        assignment methods are given below.

        The second line of the following example illustrates the syntax above.
      Example
        String * String = peek;
        "left" * "right" = "value"
  Synopsis
    Heading
      assignment to indexed variables
    Usage
      x_i = e
    Inputs
      x:Symbol
        (evaluated)
      i:Thing
      e:Thing
    Outputs
      e:
    Consequences
      Item
        The @TO2{IndexedVariable, "indexed variable"}@ @TT {"x", SUB "i"}@ is
        created (if necessary) and is assigned the value @TT "e"@ so that
        future references to @TT "x_i"@ yield the value @TT "e"@. Moreover, the
        value of the symbol @TT "x"@ is set to an object of type
        @TO "IndexedVariableTable"@, which contains the values of the
        expressions @TT "x_i"@.
    Description
      Text
        The method for assignment to indexed variables is pre-installed.
      Example
        s
        s_2
        s_2 = 44
        s_2
        s_(i,j)
        symbol s_2
        value oo
  Synopsis
    Heading
      installing assignment methods for unary prefix operators
    Usage
      OP X = (x,e) -> ...
    Inputs
      OP:
        one of the unary prefix operators for which users may install methods,
        namely:
        @between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Prefix and operatorAttributes#op#Prefix#?Flexible), s -> TO {s})@
      X:Type
      :
        @TT "(x,e) -> ..."@, @ofClass Function@
    Outputs
      :
        the value of the expression is the same as the function on the right
        hand side
    Consequences
      Item
        the function on the right hand side is installed as the method for
        assignment to @TT "OP X"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above.
      Example
        - String = peek;
        - "foo" = "value"
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using assignment methods for unary prefix operators
    Usage
      OP x = e
    Inputs
      OP:
        one of the unary prefix operators for which users may install methods,
        listed above.
      x:
        an object of type @TT "X"@
      e:Thing
    Outputs
      :
        the previously installed method for assignment to @TT "OP X"@ is called
        with arguments @TT "(x,e)"@, and its return value is returned.
    Description
      Text
        The return value and the consequences depend on the code of the
        installed assignment method. References to currently installed
        assignment methods are given below.

        The second line of the following example illustrates the syntax above.
      Example
        - String = peek;
        - "foo" = "value"
  Synopsis
    Heading
      installing assignment methods for unary postfix operators
    Usage
      X OP = (x,e) -> ...
    Inputs
      OP:
        one of the unary postfix operators for which users may install methods,
        namely:
        @between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Postfix and operatorAttributes#op#Postfix#?Flexible), s -> TO {s})@
      X:Type
      :
        @TT "(x,e) -> ..."@, @ofClass Function@
    Outputs
      :
        the value of the expression is the same as the function on the right
        hand side
    Consequences
      Item
        the function on the right hand side is installed as the method for
        assignment to @TT "OP X"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above.
      Example
        String ^~ = peek;
        "foo" ^~ = "value"
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using assignment methods for unary postfix operators
    Usage
      x OP = e
    Inputs
      x:
        an object of type @TT "X"@
      OP:
        one of the unary postfix operators for which users may install methods,
        listed above.
      e:Thing
    Outputs
      :
        the previously installed method for assignment to @TT "X OP"@ is called
        with arguments @TT "(x,e)"@, and its return value is returned.
    Description
      Text
        The return value and the consequences depend on the code of the
        installed assignment method. References to currently installed
        assignment methods are given below.

        The second line of the following example illustrates the syntax above.
      Example
        String ^~ = peek;
        "foo" ^~ = "value"
  SeeAlso
    ":="
    "<-"
    "globalAssignmentHooks"
///

doc ///
  Key
    ":="
  Headline
    assignment of method or new local variable
  Description
    Text
      In this section we'll discuss simple local assignment to variables,
      multiple local assignment, and installation and use of method functions.
      See also the operator @TO "="@, which handles other forms of assignment,
      as well as the operator @TO "<-"@, which is an assignment operator that
      evaluates its left hand side and can have assignment methods installed
      for it by the user.
  Synopsis
    Heading
      simple local assignment
    Usage
      x := e
    Inputs
      x:Symbol
        (unevaluated)
      e:Thing
    Outputs
      :Thing
        the value of the expression is @TT "e"@
    Consequences
      Item
        a new local variable @TT "x"@ is created. The scope of @TT "x"@ is the
        innermost current function body or @TO "for"@ loop, or the current
        file.
      Item
        @TT "e"@ is assigned to @TT "x"@, so future references to the value of
        @TT "x"@ yield @TT "e"@
      Item
        a warning message is issued if a local variable with the same name has
        already been created
    Description
      Example
        x
        x := 4
        x
      Text
        In the next example, we see that the scope of the local variable
        @TT "p"@ is limited to the body of the function.
      Example
        g = () -> ( p := 444; p )
        g()
        p
      Text
        In this example, we see that the scope of the local variable @TT "j"@
        is limited to the body of a @TO "for"@ loop.
      Example
        i="a b c";
        for i to 3 list j := i+1
        i
        j
      Text
        In this example, we see that a function returned by another function
        retains access to the values of local variables in its scope.
      Example
        g = () -> ( p := 444; () -> p )
        g()
        oo ()
      Text
        Functions returned by a function can also modify local variables within
        their scope, thereby communicating with each other.
      Example
        g = () -> ( p := 444; (() -> p, i -> p = i))
        (b,c) = g()
        b()
        c 555
        b()
      Text
        Since the value of the entire expression is @TT "e"@, and since the
        operator @TO "="@ is right-associative (see
        @TO "precedence of operators"@), @TT "e"@ can be easily assigned to
        more than one variable, as in the following example.
      Example
        a := b := 44
        a
        b
      Text
        By the way, there is a difference between a variable (to which values
        can be assigned) and a symbol (which can be used as an indeterminate in
        making a polynomial ring). If you want a local variable to which is
        assigned the corresponding local symbol, then combine the use of
        @TO ":="@ with the use of @TO "local"@, as in the following example,
        which illustrates a good way to do a computation in a temporary ring
        without disturbing the values of any global variables.
      Example
        g = () -> (
             x := local x;
             R := QQ[x];
             (x+2)^10);
        g()
  Synopsis
    Heading
      multiple local assignment
    Usage
      (x,y,z,...) := (c,d,e,...)
    Inputs
      :
        @TT "(x,y,z,...)"@ a @TO2 {Sequence, "sequence"}@ of
        @TO2 {Symbol, "symbols"}@ (unevaluated)
      :
        @TT "(c,d,e,...)"@ a @TO2 {Sequence, "sequence"}@ of
        @TO2 {Thing, "things"}@
    Outputs
      :
        the value of the expression is @TT "(c,d,e,...)"@
    Consequences
      Item
        new local variables @TT "x"@, @TT "y"@, @TT "z"@, ... are created
      Item
        the expressions c,d,e,... are assigned to the variables x,y,z,...,
        respectively, as above.
      Item
        the number of expressions must match the number of variables.
    Description
      Text
        Multiple assignment effectively means that functions can return
        multiple values usefully.
      Example
        f = i -> (i,i^2)
        (r,s) := f 9
        r
        s
      Text
        Multiple assignment may also be used for other uses of @CODE ":="@ such
        as method installation.
  Synopsis
    Heading
      installing methods for binary operators
    Usage
      X OP Y := (x,y) -> ...
    Inputs
      X:Type
      OP:
        one of the binary operators for which users may install methods,
        namely:
        @between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Binary and operatorAttributes#op#Binary#?Flexible), s -> TO {s})@
        . The operator SPACE, indicating adjacency, may be omitted from the
        usage above.
      Y:Type
      :
        @TT "(x,y) -> ..."@, @ofClass Function@
    Outputs
      :
        the value of the expression is the same as the function on the right
        hand side
    Consequences
      Item
        the function on the right hand side is installed as the method for
        @TT "X OP Y"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above.
      Example
        String * String := peek; -* no-capture-flag *-
        "left" * "right"
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using methods for binary operators
    Usage
      x OP y
    Inputs
      x:
        an object of type @TT "X"@
      OP:
        one of the binary operators for which users may install methods, listed
        above. The operator SPACE, indicating adjacency, may be omitted from
        the usage above.
      y:
        an object of type @TT "Y"@
    Outputs
      :
        the previously installed method for @TT "X OP Y"@ is called with
        arguments @TT "(x,y)"@, and its return value is returned. If no such
        method has been installed, then Macaulay2 searches for a method for
        @TT "X' OP Y'"@, where @TT "X'"@ is an ancestor of @TT "X"@ and
        @TT "Y'"@ is an ancestor of @TT "Y"@ (see @TO "inheritance"@ for
        details).
    Description
      Text
        The second line of the following example illustrates the syntax above.
      Example
        String * Number := peek;
        "left" * 33
        "left" * 3.3
      Text
        Some methods for operators are

        @EM "internal"@, and cannot be successfully overridden by the user, as
        we illustrate in the next example, where we try (and fail) to override
        the definition of the sum of two integers.
      Example
        ZZ + ZZ := (x,y) -> x+y+100
        3 + 4
      Text
        By contrast, division with remainder of rational numbers is not
        internal, and can be overridden.
      Example
        QQ // QQ := (w,z) -> (numerator w + numerator z)/(denominator w + denominator z)
        (3/5) // (6/7)
  Synopsis
    Heading
      installing methods for unary prefix operators
    Usage
      OP X = (x) -> ...
    Inputs
      OP:
        one of the unary prefix operators for which users may install methods,
        namely:
        @between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Prefix and operatorAttributes#op#Prefix#?Flexible), s -> TO {s})@
      X:Type
      :
        @TT "(x) -> ..."@, @ofClass Function@
    Outputs
      :
        the value of the expression is the same as the function on the right
        hand side
    Consequences
      Item
        the function on the right hand side is installed as the method for
        @TT "OP X"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above.
      Example
        - String := peek;
        - "foo"
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using methods for unary prefix operators
    Usage
      OP x
    Inputs
      OP:
        one of the unary prefix operators for which users may install methods,
        listed above.
      x:
        an object of type @TT "X"@
    Outputs
      :
        the previously installed method for @TT "OP X"@ is called with argument
        @TT "x"@, and its return value is returned. If no such method has been
        installed, then Macaulay2 searches for a method for @TT "OP X'"@, where
        @TT "X'"@ is an ancestor of @TT "X"@ (see @TO "inheritance"@ for
        details).
    Description
      Text
        The second line of the following example illustrates the syntax above.
      Example
        - String := peek;
        - "foo"
  Synopsis
    Heading
      installing methods for unary postfix operators
    Usage
      X OP = (x) -> ...
    Inputs
      OP:
        one of the unary postfix operators for which users may install methods,
        namely:
        @between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Postfix and operatorAttributes#op#Postfix#?Flexible), s -> TO {s})@
      X:Type
      :
        @TT "(x) -> ..."@, @ofClass Function@
    Outputs
      :
        the value of the expression is the same as the function on the right
        hand side
    Consequences
      Item
        the function on the right hand side is installed as the method for
        @TT "OP X"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above.
      Example
        String ^~ := peek;
        "foo" ^~
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using methods for unary postfix operators
    Usage
      x OP
    Inputs
      x:
        an object of type @TT "X"@
      OP:
        one of the unary postfix operators for which users may install methods,
        listed above.
    Outputs
      :
        the previously installed method for @TT "X OP"@ is called with
        argument= @TT "x"@, and its return value is returned. If no such method
        has been installed, then Macaulay2 searches for a method for to
        @TT "X' OP"@, where @TT "X'"@ is an ancestor of @TT "X"@ (see
        @TO "inheritance"@ for details).
    Description
      Text
        The second line of the following example illustrates the syntax above.
      Example
        String ^~ := peek;
        "foo" ^~
  Synopsis
    Heading
      installing unary methods for method functions
    Usage
      f X := (x) -> ...
      f(X) := (x) -> ...
    Inputs
      f:
        a previously defined method function. A method function may be created
        with the function @TO "method"@.
      X:Type
      :
        @TT "(x) -> ..."@, @ofClass Function@
    Consequences
      Item
        the function on the right hand side is installed as the method for
        assignment to @TT "f X"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above,
        using @TO "source"@, which happens to be a method function.
      Example
        source String := peek;
        source "foo"
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.
  Synopsis
    Heading
      using unary methods for method functions
    Usage
      f x
    Inputs
      f:
        a method function
      x:
        an object of type @TT "X"@
    Outputs
      :
        the previously installed method for @TT "f X"@ is called with argument
        @TT "x"@, and its return value is returned
    Description
      Text
        The second line of the following example illustrates the syntax above,
        using @TO "source"@, which happens to be a method function.
      Example
        source String := peek;
        source "foo"
  Synopsis
    Heading
      installing binary methods for method functions
    Usage
      f(X,Y) := (x,y) -> ...
    Inputs
      f:
        a previously defined method function. A method function may be created
        with the function @TO "method"@.
      X:Type
      Y:Type
      :
        @TT "(x,y) -> ..."@, @ofClass Function@
    Consequences
      Item
        the function on the right hand side is installed as the method for
        @TT "f(X,Y)"@. See the next subsection below for using it.
    Description
      Text
        The first line of the following example illustrates the syntax above,
        using @TO "source"@, which happens to be a method function.
      Example
        source(String,Number) := peek;
        source("foo",33)
        source("foo",3.3)
      Text
        Warning: the installation of new methods may supplant old ones,
        changing the behavior of Macaulay2.

        The same syntax works for 3 or 4 arguments.
  Synopsis
    Heading
      using binary methods for method functions
    Usage
      f(x,y)
    Inputs
      f:
        a method function
      x:
        an object of type @TT "X"@
      y:
        an object of type @TT "Y"@
    Outputs
      :
        the previously installed method for @TT "f(X,Y)"@ is called with
        arguments @TT "(x,y)"@, and the return value is returned. If no such
        method has been installed, then Macaulay2 searches for a method for
        @TT "f(X',Y')"@, where @TT "X'"@ is an ancestor of @TT "X"@ and
        @TT "Y'"@ is an ancestor of @TT "Y"@ (see @TO "inheritance"@ for
        details).
    Description
      Text
        The second line of the following example illustrates the syntax above,
        using @TO "source"@, which happens to be a method function.
      Example
        source(String,String) := peek;
        source("foo","bar")
      Text
        The same syntax works for 3 or 4 arguments.
  Description
    Text
      Another use of the operator @TO ":="@ is for installing methods for
      creation of new objects. For details, see @TO "new"@.
  SeeAlso
    "="
    "<-"
    "new"
///
