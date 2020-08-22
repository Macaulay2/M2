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
	  TO "///",
	  "functions for handling strings",
	  TO concatenate,
	  TO format,
	  TO lines,
	  TO "regular expressions",
	  "functions for handling nets",
	  TO horizontalJoin,
	  TO stack,
	  TO unstack,
	  "more information",
     	  TO String,
	  TO Net
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
	  }
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
     flushed, and writing a net subsequently will produce an unexpected result."
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
      \\t             tab
      \\xxx           ascii character with octal value xxx
      \\uxxxx         unicode character with hex value xxxx, encoded with utf-8",
     EXAMPLE lines ///
     x = " \" \f \r \\ \t \013 \u4f60 ";
     ascii x
     utf8 x
     ///,
     SeeAlso => {String, "///", ascii, utf8}
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
     Key => width,
     Headline => "width of a file or net"
     }



document {
     Key => {height,(height,Net),(height,String)},
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
     "The width of a net is the length of its longest row, if any, else 0.  For a string, the width and the height are the same.",
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
     Key => {depth,(depth, Net),(depth, String)},
     Headline => "depth of a net",
     TT "depth n", " -- the depth of a net or string ", TT "n", ".",
     PARA{},
     "The depth of a net is the number of rows of characters it has below
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SeeAlso => {"Net", "height"}
     }

document {
     Key => (length, String),
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
     Key => format,
     Headline => "format a string or a real number",
     SYNOPSIS (
     	  Usage => "format(s)",
	  Inputs => {"s"},
	  Outputs => {{"a string obtained from ", TT "s", " by inserting 
		    escape sequences, thereby preparing it for printing in a form suitable
		    for reading in again"}},
	  EXAMPLE lines ///
	  s = "a\"b\"c"
	  t = format s
	  u = value t
	  u == s
	  ///
	  ),
     SYNOPSIS (
     	  Usage => "format(s,k,l,t,e,x)",
	  Inputs => {
	       "s" => ZZ => {"the maximum number of significant decimal digits (default: ", TO "printingPrecision", ").
		    The special value ", TT "0", " imposes no limit." },
	       "k" => ZZ => {"how far to the right of the decimal point to go, at most (default: ", TO "printingAccuracy", ").
		    The special value ", TT "-1", " imposes no limit."},
	       "l" => ZZ => {"maximum number of leading zeroes (default: ", TO "printingLeadLimit", ")"},
	       "t" => ZZ => {"maximum number of trailing zeroes (default: ", TO "printingTrailLimit", ")"},
	       "e" => String => {"the separator between the mantissa and the exponent (default: ", TO "printingSeparator", ")"},
	       "x" => RR => "the number to be converted to a string"
	       },
	  Outputs => {
	       String => {"the decimal representation of the number ", TT "x", ", prepared according to the parameters above"}
	       },
	  EXAMPLE lines ///
	      format(10,1/3000.)
	      format(10,6,1/3000.)
	      format(10,6,2,1/3000.)
	      format(10,300000.)
	      format(10,-1,10,5,300000.)
	      format(10,-1,10,4,300000.)
	      format(10,-1,10,4,"E",300000.)
	  ///
	  ),
     SeeAlso => {toExternalString}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
