--		Copyright 2006 by Daniel R. Grayson

document {
     Key => "strings and nets",
     Headline => "an overview of strings and nets in Macaulay2",
     "In this section we discuss strings and nets.  Strings are sequences of
     characters intended to be printed, and are encountered in almost every programming language.  Nets
     are two-dimensional arrays of characters intended to be printed, and constitute a natural generalization
     of strings that makes ascii printing of complicated objects much easier.",
     SUBSECTION "strings",
     "A string is a sequence of characters.  Strings can
     be manipulated in various ways to produce printed output.
     One enters a string by surrounding a sequence of characters with
     quotation marks.",
     EXAMPLE {
	  ///"abcdefghij"///,
	  },
     "Strings may contain newline characters.",
     EXAMPLE ///"abcde
fghij"///,
     "Strings, like anything else, can be assigned to variables.",
     EXAMPLE ///w = "abcdefghij"///,
     PARA {
     	  "There are escape sequences that make it possible to
     	  enter special characters, see ", TO ///"///  -- "
	  , "."
	  },
     EXAMPLE ///u = "abc\101\102\n\tstu"///,
     PARA {
     	  "We can use ", TO "peek", " to see what characters are in the string."
	  },
     EXAMPLE "peek u",
     "Another way to enter special characters into strings is to use ", TO "///", -- ///
										  "
     as the string delimiter.  That makes it easier to enter the special characters
     into the string, but impossible to enter three consecutive slashes.",
     EXAMPLE ("///" | ///a \ n = "c"/// | "///"),
     "The function ", TO "ascii", " converts strings to lists of
     ascii character code, and back again.",
     EXAMPLE {
      	  "ascii u",
      	  "ascii oo",
	  },
     "We may use the operator ", TO "|", " to concatenate strings.",
     EXAMPLE "w|w|w",
     "The operator ", TO "#", " computes the length of a string.",
     EXAMPLE "#w",
     "We may also use the operator ", TO "#", " to extract single characters from
     a string.  Warning: we number the characters starting with 0.",
     EXAMPLE "w#5",
     "The function ", TO "substring", " will extract portions of a string
     for us.",
     EXAMPLE {
	  "substring(5,w)",
	  "substring(5,2,w)",
	  },
     SUBSECTION "nets",
     "A net is a rectangular two-dimensional array of characters, together
     with an imaginary horizontal baseline that allows nets to be assembled
     easily into lines of text.  A string is regarded as a net with one row.",
     PARA{
     	  "Nets are used extensively for such things as formatting polynomials for
     	  display on ascii terminals.  Use ", TO "net", " to obtain such nets directly."},
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "(x+y)^2",
	  "n = net oo",
	  },
     "The net ", TT "n", " above looks exactly the same as the original polynomial -
     that's because a polynomial is printed by printing its net.  But the net 
     ", TT "n", " is no longer usable as a polynomial; it's just an 
     array of characters.  We may use ", TO "peek", " to clarify the extent of 
     a net.",
     EXAMPLE "peek n",
     "One way to create nets directly is with the operator ", TT "||", ", 
     which concatenates strings or nets vertically.",
     EXAMPLE ///x = "a" || "bb" || "ccc"///,
     "We may use the operator ", TO "^", " to raise or lower a net with 
     respect to its baseline.  Look carefully to see how the baseline has
     moved - it is aligned with the equal sign.",
     EXAMPLE "x^2",
     "Nets may appear in lists, and elsewhere.",
     EXAMPLE {
	  "{x,x^1,x^2}",
	  },
     "Nets and strings can be concatenated horizontally with the operator ", TO "|", ".",
     EXAMPLE ///x^2 | "-------" | x///,
     "Each net has a width, a depth, and a height.  The width is the number
     of characters in each row.  The depth is the number of rows below
     the baseline, and the height is the number of rows above the baseline.
     The depth and the height can be negative.",
     EXAMPLE "width x, height x, depth x",
     "We may extract the rows of a net as strings with ", TO "unstack", " and put
     them back together again with ", TO "stack", ".",
     EXAMPLE {
	  "v = unstack x",
	  "peek oo",
	  "stack v"
	  },
     Subnodes => {
	  "functions for handling strings",
	  TO "regular expressions",
	  TO concatenate,
	  TO format,
	  TO lines,
	  TO characters,
	  TO demark,
	  TO ascii,
	  TO utf8,
	  TO utf8substring,
	  TO toLower,
	  TO toUpper,
	  TO urlEncode,
	  TO substring,
	  TO (value, String),
	  TO (symbol _, String, ZZ),
	  TO (symbol _, String, Sequence),
	  TO (symbol .., String, String),
	  TO (symbol ..<, String, String),
	  TO (symbol ^, String, Sequence),
	  "functions for handling nets",
	  TO horizontalJoin,
	  TO pad,
	  TO wrap,
	  TO centerString,
	  TO columnate,
	  TO stack,
	  TO unstack,
	  "more information",
     	  TO String,
	  TO Net,
	  TO "newline",
	  }
     }

document {
     Key => String,
     Headline => "the class of all strings",
     PARA{
	  "A string is thing that contains a sequence of characters (bytes).
	  A string is normally entered as a sequence of characters surrounded 
	  by quotation marks."
	  },
     EXAMPLE "\"abcd\"",
     PARA{
	  "Strings involving special characters can be entered by using the backslash
	  as an escape character, see ", TO "\"", ".  For an alternate method of entering strings 
	  with fewer escape sequences, see ", TO "///", ".",
	  },
     PARA{
	  "A net is a two-dimensional array of characters, and strings are regarded
	  as a type of ", TO2{ Net, "net" }, "."
	  },
     Subnodes => TO \ {(net, String), toString, toExternalString, "///", "\""}
     }


document {
     Key => Net,
     Headline => "the class of all nets and strings",
     "A net is a generalization of a string that is designed to facilitate
     two-dimensional printing on ascii terminals.  It consists of a rectangular
     array of characters subdivided horizontally by an imaginary baseline.",
     PARA{},
     "Operations on nets also accept strings by interpreting a string as a rectangle
     of height one with the baseline just below it.  In fact, the parent of
     ", TO "String", " is ", TO "Net", ".",
     PARA{},
     "Multiple nets per line can be sent to an output file with ", TO "<<", "
     but care must be taken to use ", TO "endl", " to end lines, for nets with
     new line characters embedded in them will be displayed in an unexpected way.",
     PARA{},
     "Warning: if so many characters are written to a file that an internal buffer
     is filled before the line ends or first net is seen, then the buffer will be 
     flushed, and writing a net subsequently will produce an unexpected result.",
    Subnodes => {
	TO width,
	TO (width, Net),
	TO (height, Net),
	TO (depth, Net),
	TO (length, Net),
        TO (symbol ^, Net, ZZ),
	TO (symbol ||, Net, Net),
        TO (symbol |, Net, Net),
        },
     }

document {
     Key => "///",					    -- ///
     Headline => "delineate a string with slashes",
     "This method for entering a string involves no escape characters, so
     it can be used for easily inserting large chunks of text into a string
     without treating the characters ", TT "\\", " and ", TT "\"", " specially.
     A series of more than 3 slashes can be represented before the end of the string by doubling all but the
     last two, and a series of 1 or more slashes can be represented at the end of the string by doubling
     each of them; this allows an arbitrary string to be represented.",
     EXAMPLE {
	  "/// \\ \" ///",
      	  "ascii oo",
	  "///-- //// -- /////////",
	  "///-- ////// -- ///////////",
	  "//////////////"
	  },
     SeeAlso => {String, "\"", ascii}
     }

document {
     Key => "\"",
     Headline => "delineate a string with quotation marks",
     PARA {
     	  "This method for entering a string involves the use of backslashes as escape characters."
	  },
     PRE "      \\n             newline
      \\f             form feed
      \\r             return
      \\\\             \\ 
      \\\"             \"
      \\a             audible bell
      \\b             backspace
      \\e, \\E         escape
      \\t             tab
      \\v             vertical tab
      \\nnn           ascii character with octal value nnn
      \\xnn           ascii character with hex value nn
      \\unnnn         unicode character with hex value nnnn, encoded with utf-8",
     EXAMPLE lines ///
     x = " \" \f \r \\ \a\b\e\E\t\v \013 \x0b \u4f60 ";
     ascii x
     utf8 x
     ///,
     SeeAlso => {String, "///", ascii, utf8}
     }

document {
     Key => {concatenate,(concatenate, ZZ), (concatenate, BasicList), (concatenate, String), (concatenate, Nothing), (concatenate, Symbol)},
     Headline => "join strings",
     TT "concatenate(s,t,...,u)", " yields the concatenation of the strings s,t,...,u.",
     PARA{},
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces, and ", TO "null", " will be represented by the empty string.",
     EXAMPLE ///concatenate {"a",("s",3,"d",),"f"}///,
     SeeAlso => { "String"} 
     }

undocumented {
    (stack, Net),
    (stack, Nothing),
    (stack, String),
    }

document {
     Key => {stack,(stack, BasicList)},
     Headline => "join nets or string vertically",
     TT "stack(m,n,...)", " -- joins nets or strings by concatenating
     them vertically.  The baseline of the result is the baseline of the
     first argument.",
     PARA{},
     "Nested sequences among the arguments are first spliced together.",
     PARA{},
     "If there are no arguments, then the net returned has zero height and
     zero depth.  This might be unexpected.",
     PARA{},
     "Tab characters in any of the strings are first expanded into spaces,
     assuming tab stops at every eighth column.",
     PARA{},
     "Null arguments are allowed and ignored.",
     SeeAlso => { (symbol ||, Net, Net)}
     }

document {
     Key => {unstack,(unstack, Net),(unstack, String)},
     Headline => "list the rows of a net",
     TT "unstack x", " -- produces a list of strings, each containing the
     characters in one row of the ", TT "Net", " ", TT "x", ".",
     PARA{},
     "The original net, adjusted so its height is 1, may be recovered
     with ", TO "stack", ". The individual strings will have
     all trailing spaces removed, unless this would make all of them
     narrower than the original net, in which case the first string
     retains its trailing spaces."
     }

document {
     Key => {(height,Net),(height,String)},
     Headline => "height of a net",
     TT "height n", " -- the height of a net ", TT "n", ".",
     PARA{},
     "The height of a net is the number of rows of characters it has above
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SeeAlso => {"Net", "depth"}}

document {
     Key => {(width,Net),(width,String)},
     Headline => "width of a net",
     TT "width n", " -- the width of a net ", TT "n", ".",
     PARA{},
     "The width of a net is the length of its longest row, if any, else 0.",
     EXAMPLE lines ///
     	  "a c" || "-" || "adsf"
	  width oo
     ///,
     SeeAlso => {"depth", "height"}
     }

document { Key => (height,File),
     Headline => "get window height",
     Usage => "height f",
     Inputs => { "f" },
     Outputs => { ZZ => {"the height of the window or terminal attached to the file ", TT "f", ", if any, else 0" }}}
document { Key => (width,File),
     Headline => "get window width",
     Usage => "width f",
     Inputs => { "f" },
     Outputs => { ZZ => {"the width of the window or terminal attached to the file ", TT "f", ", if any, else 0" }}}

document {
     Key => {(depth, Net),(depth, String)},
     Headline => "depth of a net",
     TT "depth n", " -- the depth of a net or string ", TT "n", ".",
     PARA{},
     "The depth of a net is the number of rows of characters it has below
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SeeAlso => {"Net", "height"}
     }

document {
    Key => {
	(length, String),
	(length, Net),
    },
     Headline => "length of a string",
     Usage => "n = length s",
     Inputs => { "s" },
     Outputs => { "n" => { "the length of the string ", TT "s" } }
     }

document {
     Key => {(symbol ^, Net, ZZ),
	  (symbol ^, String, ZZ)},
     Headline => "raise a net or string",
     Usage => "n^i",
     Inputs => {"n" => {"or a ", ofClass String}, "i"},
     Outputs => {
     	  Net => {"obtained by elevating ", TT "n", " by raising its characters by ", TT "i", " rows"}
	  },
     PARA{},
     "If ", TT "n", " is a string, then ", TT "n^0", " is an easy way to convert
     it to a net.",
     EXAMPLE lines ///
     	  s = "x"
	  class(s^0)
	  n = s^1|"ij"
	  n^-3
     	  ///,
     SeeAlso => {"strings and nets"}
     }

document {
     Key => {(symbol ^, String, Sequence)},
     Headline => "vertically stacked copies of a string",
     Usage => "s^(height,depth)",
     Inputs => {"s", Nothing => { TT"(height,depth)", ", a pair of integers"}},
     Outputs => {
     	  Net => {TT "height + depth", " copies of ", TT "s", 
	       ", stacked vertically.  There are ", TT "depth", " lines
	       below the base line"}
	  },
     PARA{},
     EXAMPLE lines ///
     	  s = "|"
	  s^(4,3)
	  n = net(x_0)
	  n0 = s^(height n, depth n)
	  n0|n|n0
     	  ///,
     SeeAlso => {"strings and nets", height, depth}
     }

document {
    Key => {ascii, (ascii, BasicList), (ascii, String), (ascii, ZZ)},
    Headline => "ASCII character conversion",
    SYNOPSIS (
     	Usage => "ascii v",
     	Inputs => {"v" => "containing small integers"},
     	Outputs => {{"the string whose characters have the ", wikipedia "ASCII", " codes listed in ", TT "v"}}),
    SYNOPSIS (
     	Usage => "ascii s",
     	Inputs => {"s"},
     	Outputs => {{"the list of (small integer) ", wikipedia "ASCII", " codes of the characters of ", TT "s"}}),
    EXAMPLE {///ascii "abcdef"///, ///ascii oo///, ///first ascii "A"///}
    }

-- TODO: utf8check

document {
     Key => {utf8, utf8check},
     Headline => "encode and decode unicode utf-8-encoded strings",
     SYNOPSIS (
     	  Usage => "utf8 x",
	  Inputs => {
	       "x" => List => {"a list of small natural numbers to serve as character codes"}
	       },
	  Outputs => {
	       String => {"a string obtained by encoding the character codes in ", TT "x", " according to the utf-8 encoding standard"}
	       },
	  EXAMPLE lines ///
	       s = utf8 {119, 111, 51, 32, 25105}
	  ///
	  ),
     SYNOPSIS (
     	  Usage => "utf8 s",
	  Inputs => {
	       "s" => String
	       },
	  Outputs => {
	       List => {"a list of the integer codes obtained by decoding the string ", TT "s", " according to the utf-8 encoding standard"}
	       },
	  EXAMPLE lines ///
	       utf8 s
	  ///
	  ),
     PARA {
	  "The two operations described above are inverse to each other."
	  },
     PARA {
	  "The function ", TT "utf8check", " can be used to verify that a string contains a valid uft8-encoding of a sequence of
	  unicode characters.  It returns ", TO "null", " upon success, and signals an error otherwise."
	  },
     EXAMPLE lines ///
     try utf8check "你好" else "invalid"
     try utf8check "\200\200" else "invalid"
     ///,
     SeeAlso => {ascii},
     }

document { Key => toLower,
     Headline => "convert to lower case",
     Usage => "toLower s",
     Inputs => {"s"=>String},
     Outputs => {String => {"the string produced from ", TT "s", " by converting its characters to lower case"}},
     EXAMPLE lines ///
     	  toLower "A b C d E f"
     ///}

document { Key => toUpper,
     Headline => "convert to upper case",
     Usage => "toUpper s",
     Inputs => {"s"=>String},
     Outputs => {String => {"the string produced from ", TT "s", " by converting its characters to lower case"}},
     EXAMPLE lines ///
     	  toUpper "A b C d E f"
     ///}

document {
     Key => print,
     Headline => "print something",
	Usage => "print x",
     TT "print x", " prints ", TT "x", " on the standard output followed by a 
     new line.",
	EXAMPLE {
		///print "Hello world!"///
		},
     "The return value is ", TO "null", "."
     }

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

document {
     Key => columnate,
     Headline => "arrange strings in columns",
     TT "columnate(w,s)", " -- arranges the strings in the list ", TT "s", " in
     columns, returning a ", TO "Net", " suitable for output to a terminal 
     with a linewidth of ", TT "w", ".",
     PARA{},
     EXAMPLE {
	  "columnate(12, characters ascii (65 .. 90))",
	  }
     }

document {
     Key => {(symbol |, Net, Net),
	  (symbol |, String, String),
	  (symbol |, String, ZZ),
	  (symbol |, ZZ, String)},
     Headline => "join strings or nets",
     TT "s|t", " -- concatenates strings or nets horizontally.", 
     PARA{},
     "The result is a string if the arguments are all strings, otherwise it
     is a net.  The baselines of the nets are aligned.",
     EXAMPLE {
	  ///"abc" | "def"///,
      	  ///x = "abc" || "ABC"///,
      	  ///x|"x"|x///,
	  },
     "If one of the two arguments is an integer, it is converted to a string first.",
     EXAMPLE ///"t = " | 333///,
     SeeAlso => {horizontalJoin}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
