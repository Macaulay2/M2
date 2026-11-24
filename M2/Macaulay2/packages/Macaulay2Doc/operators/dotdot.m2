-- -*- coding: utf-8 -*-
--		Copyright 1993-2009 by Daniel R. Grayson

doc ///
  Key
    symbol ..
  Headline
    a binary operator, used for sequences of consecutive items
  Description
    Text
      The most confusing thing about this operator, in all its guises, is that
      it is not a syntactic construction, and so the resulting sequences do not
      splice themselves into enclosing lists, as in each of the following
      examples.
    Example
      {10..10}
      {10..8}
      {3..5,8..10}
    Text
      Use @TO "splice"@ to fix that.
    Example
      splice {3..5,8..10}
    Text
      If a type of list, instead of a sequence, is desired, use @TO "toList"@
      or the operator @TO "new"@.
    Example
      0..5
      toList (0..5)
      new Array from 0..5
      new Sum from 0..5
    Text
      The operator can be used with sequences or lists, whose elements are of
      various types, to produce rectangular intervals.
    Example
      (0,0)..(1,3)
      p_(0,a) .. p_(1,c)
      p_(1,1) .. q_(2,2)
    Text
      Use @TO "..<"@ instead to get a sequence that stops short of the
      endpoint.
  SeeAlso
    "polynomial rings"
    "subscripted variables"
    "..<"
///
doc ///
  Key
    symbol ..<
  Headline
    a binary operator, used for sequences of consecutive items, not including the endpoint
  Description
    Text
      The most confusing thing about this operator, in all its guises, is that
      it is not a syntactic construction, and so the resulting sequences do not
      splice themselves into enclosing lists, as in each of the following
      examples.
    Example
      {10..<11}
      {10..<8}
      {3..<5,8..<10}
    Text
      Use @TO "splice"@ to fix that.
    Example
      splice {3..<5,8..<10}
    Text
      If a type of list, instead of a sequence, is desired, use @TO "toList"@
      or the operator @TO "new"@.
    Example
      0..<5
      toList (0..<5)
      new Array from 0..<5
      new Sum from 0..<5
    Text
      The operator can be used with sequences or lists, whose elements are of
      various types, to produce rectangular intervals.
    Example
      (0,0)..<(2,3)
      p_(0,a) ..< r_(2,c)
    Text
      Use @TO ".."@ instead to get a sequence that does not stop short of the
      endpoint.
  SeeAlso
    "polynomial rings"
    "subscripted variables"
    ".."
///
doc ///
  Key
    (symbol .., List, List)
    (symbol .., Sequence, Sequence)
  Headline
    rectangular sequences of consecutive lists and sequences
  Usage
    s .. t
  Inputs
    s:
    t:
  Outputs
    :
      the rectangular sequence of consecutive lists of the same length, from
      @TT "s"@ to @TT "t"@, in lexicographic order, inclusive, obtained by
      applying the operator @TO ".."@ recursively
  Description
    Example
      {a,1} .. {c,3}
      {1} .. {4}
///
doc ///
  Key
    (symbol ..<, List, List)
    (symbol ..<, Sequence, Sequence)
  Headline
    rectangular sequences of consecutive lists and sequences
  Usage
    s ..< t
  Inputs
    s:
    t:
  Outputs
    :
      the rectangular sequence of consecutive lists of the same length, from
      @TT "s"@ to @TT "t"@, in lexicographic order, not including the endpoint,
      obtained by applying the operator @TO "..<"@ recursively
  Description
    Example
      {a,1} ..< {d,4}
      {1} ..< {5}
///
doc ///
  Key
    (symbol .., IndexedVariable, IndexedVariable)
  Headline
    sequences of consecutive indexed variables
  Usage
    s .. t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of indexed variables with the same base and with consecutive
      subscripts, inclusive
  Description
    Example
      x_1
      x_1 .. x_10
      x_(a,1) .. x_(b,3)
///
doc ///
  Key
    (symbol ..<, IndexedVariable, IndexedVariable)
  Headline
    sequences of consecutive indexed variables
  Usage
    s ..< t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of indexed variables with the same base and with consecutive
      subscripts, not including the endpoint
  Description
    Example
      x_1
      x_(a,1) ..< z_(c,3)
///
doc ///
  Key
    (symbol .., Symbol, Symbol)
  Headline
    sequences of consecutive symbols
  Usage
    s .. t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive symbols useful as variables in polynomial
      rings, from @TT "s"@ to @TT "t"@, inclusive, as determined by the
      @TO (vars, ZZ)@
  Description
    Example
      x .. z
      x .. C
///
doc ///
  Key
    (symbol ..<, Symbol, Symbol)
  Headline
    sequences of consecutive symbols
  Usage
    s ..< t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive symbols useful as variables in polynomial
      rings, from @TT "s"@ to @TT "t"@, not including the endpoint, as
      determined by the @TO (vars, ZZ)@
  Description
    Example
      x ..< z
      x ..< C
///
doc ///
  Key
    (symbol .., ZZ, ZZ)
  Headline
    sequences of consecutive integers
  Usage
    s .. t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive integers from @TT "s"@ to @TT "t"@, inclusive
  Description
    Example
      1 .. 10
///
doc ///
  Key
    (symbol ..<, ZZ, ZZ)
  Headline
    sequences of consecutive integers
  Usage
    s ..< t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive integers from @TT "s"@ to @TT "t"@, not
      including the endpoint
  Description
    Example
      0 ..< 10
    Text
      The indices valid for a list of length @TT "n"@ can be obtained with
      @TT "0 ..< n"@.
    Example
      x = {a,c,e,f}
      0 ..< #x
      apply(0 ..< #x, i -> x#i)
///

doc ///
  Key
    (symbol .., String, String)
  Headline
    a sequence of consecutive strings
  Usage
    s .. t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive strings of the same length, from @TT "s"@ to
      @TT "t"@, in lexicographic order, inclusive
  Description
    Example
      "a" .. "z"
      "aa" .. "ce"
      "aaa" .. "abc"
      "佖" .. "佥"
  SeeAlso
    (symbol ..<, String, String)
///
doc ///
  Key
    (symbol ..<, String, String)
  Headline
    a sequence of consecutive strings, not including the endpoint
  Usage
    s ..< t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive strings of the same length, from @TT "s"@ to
      @TT "t"@, in lexicographic order, inclusive
  Description
    Example
      "a" ..< "z"
      "aa" ..< "ce"
      "aaa" ..< "bcd"
      "佖" ..< "佥"
  SeeAlso
    (symbol .., String, String)
///

doc ///
  Key
    (symbol .., RingElement, RingElement)
  Headline
    a sequence of consecutive generators of a polynomial ring
  Usage
    s .. t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive generators of a polynomial ring, from
      @TT "s"@ to @TT "t"@, inclusive
  Description
    Example
      R = QQ[a..z]
      b .. i
      plus oo
    Text
      Warning: former behavior involved making the names of the generators
      consecutive, so the results in the next example differ from those given
      before.
    Example
      R = QQ[e,d,c,b,a,X_1,y,X_2]
      e .. a
      X_1 .. X_2
    Text
      Warning: since former behavior involved only the names of the generators,
      there was no requirement that @TT "s"@ and @TT "t"@ be in the same ring,
      whereas now there is.
///
doc ///
  Key
    (symbol ..<, RingElement, RingElement)
  Headline
    a sequence of consecutive generators of a polynomial ring
  Usage
    s ..< t
  Inputs
    s:
    t:
  Outputs
    :
      the sequence of consecutive generators of a polynomial ring, from
      @TT "s"@ to @TT "t"@, not including the endpoint
  Description
    Example
      R = QQ[a..z]
      b ..< i
      plus oo
///

-- this should be at the end
undocumented select(makeDocumentTag \ flatten apply({symbol .., symbol ..<}, methods),
    m -> package m === Macaulay2Doc and not isUndocumented m and isMissingDoc m)
