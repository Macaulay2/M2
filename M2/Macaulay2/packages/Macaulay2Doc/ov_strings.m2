--		Copyright 2006 by Daniel R. Grayson

doc ///
  Key
    "strings and nets"
  Headline
    an overview of strings and nets in Macaulay2
  Description
    Text
      In this section we discuss strings and nets. Strings are sequences of
      characters intended to be printed, and are encountered in almost every
      programming language. Nets are two-dimensional arrays of characters
      intended to be printed, and constitute a natural generalization of
      strings that makes ascii printing of complicated objects much easier.

      @SUBSECTION "strings"@

      A string is a sequence of characters. Strings can be manipulated in
      various ways to produce printed output. One enters a string by
      surrounding a sequence of characters with quotation marks.
    Example
      "abcdefghij"
    Text
      Strings may contain newline characters.
    Code
      EXAMPLE ////"abcde
      fghij"////
    Text
      Strings, like anything else, can be assigned to variables.
    Example
      w = "abcdefghij"
    Text
      -- "
      There are escape sequences that make it possible to enter special
      characters, see @TO ////"////@.
    Example
      u = "abc\101\102\n\tstu"
    Text
      We can use @TO "peek"@ to see what characters are in the string.
    Example
      peek u
    Text
      -- ////
      Another way to enter special characters into strings is to use @TO "////"@
      as the string delimiter. That makes it easier to enter the special
      characters into the string, but impossible to enter three consecutive
      slashes.
    Code
      EXAMPLE ("////" | ////a \ n = "c"//// | "////")
    Text
      The function @TO "ascii"@ converts strings to lists of ascii character
      code, and back again.
    Example
      ascii u
      ascii oo
    Text
      We may use the operator @TO "|"@ to concatenate strings.
    Example
      w|w|w
    Text
      The operator @TO "#"@ computes the length of a string.
    Example
      #w
    Text
      We may also use the operator @TO "#"@ to extract single characters from a
      string. Warning: we number the characters starting with 0.
    Example
      w#5
    Text
      The function @TO "substring"@ will extract portions of a string for us.
    Example
      substring(5,w)
      substring(5,2,w)
    Text
      @SUBSECTION "nets"@

      A net is a rectangular two-dimensional array of characters, together with
      an imaginary horizontal baseline that allows nets to be assembled easily
      into lines of text. A string is regarded as a net with one row.

      Nets are used extensively for such things as formatting polynomials for
      display on ascii terminals. Use @TO "net"@ to obtain such nets directly.
    Example
      R = ZZ[x,y];
      (x+y)^2
      n = net oo
    Text
      The net @TT "n"@ above looks exactly the same as the original polynomial
      - that's because a polynomial is printed by printing its net. But the net
      @TT "n"@ is no longer usable as a polynomial; it's just an array of
      characters. We may use @TO "peek"@ to clarify the extent of a net.
    Example
      peek n
    Text
      One way to create nets directly is with the operator @TT "||"@, which
      concatenates strings or nets vertically.
    Example
      x = "a" || "bb" || "ccc"
    Text
      We may use the operator @TO "^"@ to raise or lower a net with respect to
      its baseline. Look carefully to see how the baseline has moved - it is
      aligned with the equal sign.
    Example
      x^2
    Text
      Nets may appear in lists, and elsewhere.
    Example
      {x,x^1,x^2}
    Text
      Nets and strings can be concatenated horizontally with the operator
      @TO "|"@.
    Example
      x^2 | "-------" | x
    Text
      Each net has a width, a depth, and a height. The width is the number of
      characters in each row. The depth is the number of rows below the
      baseline, and the height is the number of rows above the baseline. The
      depth and the height can be negative.
    Example
      width x, height x, depth x
    Text
      We may extract the rows of a net as strings with @TO "unstack"@ and put
      them back together again with @TO "stack"@.
    Example
      v = unstack x
      peek oo
      stack v
  Subnodes
    :functions for handling strings
    "regular expressions"
    concatenate
    format
    lines
    characters
    demark
    ascii
    utf8
    utf8substring
    toLower
    toUpper
    urlEncode
    substring
    (value, String)
    (symbol _, String, ZZ)
    (symbol _, String, Sequence)
    (symbol _, String, List)
    (symbol .., String, String)
    (symbol ..<, String, String)
    (symbol ^, String, Sequence)
    :functions for handling nets
    horizontalJoin
    pad
    wrap
    centerString
    columnate
    stack
    unstack
    :more information
    String
    Net
    "newline"
///

doc ///
  Key
    String
  Headline
    the class of all strings
  Description
    Text
      A string is thing that contains a sequence of characters (bytes). A
      string is normally entered as a sequence of characters surrounded by
      quotation marks.
    Example
      "abcd"
    Text
      Strings involving special characters can be entered by using the
      backslash as an escape character, see @TO "\""@. For an alternate method
      of entering strings with fewer escape sequences, see @TO "////"@.

      A net is a two-dimensional array of characters, and strings are regarded
      as a type of @TO2{Net, "net"}@.
  Subnodes
    (net, String)
    toString
    toExternalString
    "////"
    "\""
///


doc ///
  Key
    Net
  Headline
    the class of all nets and strings
  Description
    Text
      A net is a generalization of a string that is designed to facilitate
      two-dimensional printing on ascii terminals. It consists of a rectangular
      array of characters subdivided horizontally by an imaginary baseline.

      Operations on nets also accept strings by interpreting a string as a
      rectangle of height one with the baseline just below it. In fact, the
      parent of @TO "String"@ is @TO "Net"@.

      Multiple nets per line can be sent to an output file with @TO "<<"@ but
      care must be taken to use @TO "endl"@ to end lines, for nets with new
      line characters embedded in them will be displayed in an unexpected way.

      Warning: if so many characters are written to a file that an internal
      buffer is filled before the line ends or first net is seen, then the
      buffer will be flushed, and writing a net subsequently will produce an
      unexpected result.
  Subnodes
    width
    (width, Net)
    (height, Net)
    (depth, Net)
    (length, Net)
    (symbol ^, Net, ZZ)
    (symbol ||, Net, Net)
    (symbol |, Net, Net)
///

doc ///
  Key
    "////"
    -- ////
  Headline
    delineate a string with slashes
  Description
    Text
      This method for entering a string involves no escape characters, so it
      can be used for easily inserting large chunks of text into a string
      without treating the characters @TT "\\"@ and @TT "\""@ specially. A
      series of more than 3 slashes can be represented before the end of the
      string by doubling all but the last two, and a series of 1 or more
      slashes can be represented at the end of the string by doubling each of
      them; this allows an arbitrary string to be represented.
    Example
      //// \ " ////
      ascii oo
      ////-- ////// -- ////////////////
      ////-- ////////// -- ////////////////////
      //////////////////////////
  SeeAlso
    String
    "\""
    ascii
///

doc ///
  Key
    "\""
  Headline
    delineate a string with quotation marks
  Description
    Text
      This method for entering a string involves the use of backslashes as
      escape characters.
    Pre
      \n             newline
      \f             form feed
      \r             return
      \\             \
      \"             "
      \a             audible bell
      \b             backspace
      \e, \E         escape
      \t             tab
      \v             vertical tab
      \nnn           ascii character with octal value nnn
      \xnn           ascii character with hex value nn
      \unnnn         unicode character with hex value nnnn, encoded with utf-8
    Example
      x = " \" \f \r \\ \a\b\e\E\t\v \013 \x0b \u4f60 ";
      ascii x
      utf8 x
  SeeAlso
    String
    "////"
    ascii
    utf8
///

doc ///
  Key
    concatenate
    (concatenate, ZZ)
    (concatenate, BasicList)
    (concatenate, String)
    (concatenate, Nothing)
    (concatenate, Symbol)
  Headline
    join strings
  Description
    Text
      @TT "concatenate(s,t,...,u)"@ yields the concatenation of the strings
      s,t,...,u.

      The arguments may also be lists or sequences of strings and symbols, in
      which case they are concatenated recursively. Additionally, an integer
      may be used to represent a number of spaces, and @TO "null"@ will be
      represented by the empty string.
    Example
      concatenate {"a",("s",3,"d",),"f"}
  SeeAlso
    "String"
///

undocumented {
    (stack, Net),
    (stack, Nothing),
    (stack, String),
    }

doc ///
  Key
    stack
    (stack, BasicList)
  Headline
    join nets or string vertically
  Description
    Text
      @TT "stack(m,n,...)"@ -- joins nets or strings by concatenating them
      vertically. The baseline of the result is the baseline of the first
      argument.

      Nested sequences among the arguments are first spliced together.

      If there are no arguments, then the net returned has zero height and zero
      depth. This might be unexpected.

      Tab characters in any of the strings are first expanded into spaces,
      assuming tab stops at every eighth column.

      Null arguments are allowed and ignored.
  SeeAlso
    (symbol ||, Net, Net)
///

doc ///
  Key
    unstack
    (unstack, Net)
    (unstack, String)
  Headline
    list the rows of a net
  Description
    Text
      @TT "unstack x"@ -- produces a list of strings, each containing the
      characters in one row of the @TT "Net"@ @TT "x"@.

      The original net, adjusted so its height is 1, may be recovered with
      @TO "stack"@. The individual strings will have all trailing spaces
      removed, unless this would make all of them narrower than the original
      net, in which case the first string retains its trailing spaces.
///

doc ///
  Key
    (height,Net)
    (height,String)
  Headline
    height of a net
  Description
    Text
      @TT "height n"@ -- the height of a net @TT "n"@.

      The height of a net is the number of rows of characters it has above the
      baseline. It may be a negative number, but the depth plus the height is
      always the total number of rows, which is not negative.
  SeeAlso
    "Net"
    "depth"
///

doc ///
  Key
    (width,Net)
    (width,String)
  Headline
    width of a net
  Description
    Text
      @TT "width n"@ -- the width of a net @TT "n"@.

      The width of a net is the length of its longest row, if any, else 0.
    Example
      "a c" || "-" || "adsf"
      width oo
  SeeAlso
    "depth"
    "height"
///

doc ///
  Key
    (height,File)
  Headline
    get window height
  Usage
    height f
  Inputs
    f:
  Outputs
    :ZZ
      the height of the window or terminal attached to the file @TT "f"@, if
      any, else 0
///
doc ///
  Key
    (width,File)
  Headline
    get window width
  Usage
    width f
  Inputs
    f:
  Outputs
    :ZZ
      the width of the window or terminal attached to the file @TT "f"@, if
      any, else 0
///

doc ///
  Key
    (depth, Net)
    (depth, String)
  Headline
    depth of a net
  Description
    Text
      @TT "depth n"@ -- the depth of a net or string @TT "n"@.

      The depth of a net is the number of rows of characters it has below the
      baseline. It may be a negative number, but the depth plus the height is
      always the total number of rows, which is not negative.
  SeeAlso
    "Net"
    "height"
///

doc ///
  Key
    (length, String)
    (length, Net)
  Headline
    length of a string
  Usage
    n = length s
  Inputs
    s:
  Outputs
    n:
      the length of the string @TT "s"@
///

doc ///
  Key
    (symbol ^, Net, ZZ)
    (symbol ^, String, ZZ)
  Headline
    raise a net or string
  Usage
    n^i
  Inputs
    n:
      or a @ofClass String@
    i:
  Outputs
    :Net
      obtained by elevating @TT "n"@ by raising its characters by @TT "i"@ rows
  Description
    Text
      If @TT "n"@ is a string, then @TT "n^0"@ is an easy way to convert it to
      a net.
    Example
      s = "x"
      class(s^0)
      n = s^1|"ij"
      n^-3
  SeeAlso
    "strings and nets"
///

doc ///
  Key
    (symbol ^, String, Sequence)
  Headline
    vertically stacked copies of a string
  Usage
    s^(height,depth)
  Inputs
    s:
    :Nothing
      @TT "(height,depth)"@, a pair of integers
  Outputs
    :Net
      @TT "height + depth"@ copies of @TT "s"@, stacked vertically. There are
      @TT "depth"@ lines below the base line
  Description
    Example
      s = "|"
      s^(4,3)
      n = net(x_0)
      n0 = s^(height n, depth n)
      n0|n|n0
  SeeAlso
    "strings and nets"
    height
    depth
///

doc ///
  Key
    ascii
    (ascii, BasicList)
    (ascii, String)
    (ascii, ZZ)
  Headline
    ASCII character conversion
  Synopsis
    Usage
      ascii v
    Inputs
      v:
        containing small integers
    Outputs
      :
        the string whose characters have the @wikipedia "ASCII"@ codes listed
        in @TT "v"@
  Synopsis
    Usage
      ascii s
    Inputs
      s:
    Outputs
      :
        the list of (small integer) @wikipedia "ASCII"@ codes of the characters
        of @TT "s"@
  Description
    Example
      ascii "abcdef"
      ascii oo
      first ascii "A"
///

-- TODO: utf8check

doc ///
  Key
    utf8
    utf8check
  Headline
    encode and decode unicode utf-8-encoded strings
  Synopsis
    Usage
      utf8 x
    Inputs
      x:List
        a list of small natural numbers to serve as character codes
    Outputs
      :String
        a string obtained by encoding the character codes in @TT "x"@ according
        to the utf-8 encoding standard
    Description
      Example
        s = utf8 {119, 111, 51, 32, 25105}
  Synopsis
    Usage
      utf8 s
    Inputs
      s:String
    Outputs
      :List
        a list of the integer codes obtained by decoding the string @TT "s"@
        according to the utf-8 encoding standard
    Description
      Example
        utf8 s
  Description
    Text
      The two operations described above are inverse to each other.

      The function @TT "utf8check"@ can be used to verify that a string
      contains a valid uft8-encoding of a sequence of unicode characters. It
      returns @TO "null"@ upon success, and signals an error otherwise.
    Example
      try utf8check "你好" else "invalid"
      try utf8check "\200\200" else "invalid"
  SeeAlso
    ascii
///

doc ///
  Key
    toLower
  Headline
    convert to lower case
  Usage
    toLower s
  Inputs
    s:String
  Outputs
    :String
      the string produced from @TT "s"@ by converting its characters to lower
      case
  Description
    Example
      toLower "A b C d E f"
///

doc ///
  Key
    toUpper
  Headline
    convert to upper case
  Usage
    toUpper s
  Inputs
    s:String
  Outputs
    :String
      the string produced from @TT "s"@ by converting its characters to lower
      case
  Description
    Example
      toUpper "A b C d E f"
///

doc ///
  Key
    print
  Headline
    print something
  Usage
    print x
  Description
    Text
      @TT "print x"@ prints @TT "x"@ on the standard output followed by a new
      line.
    Example
      print "Hello world!"
    Text
      The return value is @TO "null"@.
///

doc ///
  Key
    printerr
  Headline
    print something to stderr
  Usage
    printerr x
  Inputs
    x:{String,Net,BasicList}
  Description
    Text
      Print @TT "x"@, each line prepended with @TT "--"@, to @TO
      stderr@.  This is useful for displaying warning messages and
      verbose logs.
    Example
      printerr "Hello, world!"
      printerr("foo" || "bar")
    Text
      If @TT "x"@ is @ofClass BasicList@, then its elements are first
      joined with @TO horizontalJoin@.
    Example
      printerr("foo", "bar")
///

doc ///
  Key
    pad
    (pad, String, ZZ)
    (pad, ZZ, String)
    (pad, Net, ZZ)
    (pad, ZZ, Net)
  Headline
    pad a string or net with spaces
  Usage
    pad(s,n)
    pad(n,s)
  Inputs
    s:Net
    n:ZZ
  Description
    Text
      @TT "pad(s,n)"@ pads the string or net @TT "s"@ to length @TT
      "n"@ with spaces on the right.

      @TT "pad(n,s)"@ pads the string or net @TT "s"@ to length @TT
      "n"@ with spaces on the left.
    Example
      pad(6, "foo")
      pad("foo", 6) | "bar"
///

doc ///
  Key
    columnate
  Headline
    arrange strings in columns
  Description
    Text
      @TT "columnate(w,s)"@ -- arranges the strings in the list @TT "s"@ in
      columns, returning a @TO "Net"@ suitable for output to a terminal with a
      linewidth of @TT "w"@.
    Example
      columnate(12, characters ascii (65 .. 90))
///

doc ///
  Key
    (symbol |, Net, Net)
    (symbol |, String, String)
    (symbol |, String, ZZ)
    (symbol |, ZZ, String)
  Headline
    join strings or nets
  Description
    Text
      @TT "s|t"@ -- concatenates strings or nets horizontally.

      The result is a string if the arguments are all strings, otherwise it is
      a net. The baselines of the nets are aligned.
    Example
      "abc" | "def"
      x = "abc" || "ABC"
      x|"x"|x
    Text
      If one of the two arguments is an integer, it is converted to a string
      first.
    Example
      "t = " | 333
  SeeAlso
    horizontalJoin
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
