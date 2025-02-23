document {
     Key => symbol <<,
     Headline => "a binary operator (file output, ...)",
     }
document {
     Key => {"left shift", (symbol <<, ZZ, ZZ), (symbol <<, RR, ZZ), (symbol <<, CC, ZZ), (symbol <<, RRi, ZZ)},
     Usage => "x << j\nI << j",
     Inputs => { "x", "j", "I" => RRi },
     Outputs => {{ "the number obtained from ", TT "x", " by shifting its binary representation leftward ", TT "j", " places" },
    RRi => {"an interval containing all shifts of the binary representations elements of ", TT "I", " by ", TT "j", " places leftward" }
},
     EXAMPLE {"256 << 5","256. << 555"},
     SeeAlso => {"right shift"}
     }

document {
     Key => {"right shift", (symbol >>, ZZ, ZZ), (symbol >>, RR, ZZ), (symbol >>, CC, ZZ), (symbol >>, RRi, ZZ)},
     Usage => "x >> j\nx >> I",
     Inputs => { "x", "j", "I" => RRi },
     Outputs => {{ "the integer obtained from ", TT "x", " by shifting its binary representation rightward ", TT "j", " places" },
    RRi => {"an interval containing all shifts of the binary representations elements of ", TT "I", " by ", TT "j", " places rightward" }
},
     EXAMPLE {"256 >> 5","256. >> 555"},
     SeeAlso => {"left shift"}
     }

document {
     Key => { (symbol <<, File, Thing),(symbol <<, String, Thing), (symbol <<, File, Manipulator),
	  (symbol <<, Nothing, Thing),(symbol <<, Nothing, Manipulator), (symbol <<, Thing),
	  (symbol <<, File, Symbol),(symbol <<, File, Net),(symbol <<,File,String) },
     Headline => "print to a file",
     Usage => "f << x\n  << x",
     Inputs => {
	  "f" => Nothing => { ofClass {File, String, Nothing} },
	  "x"
	  },
     Outputs => {
	  File => "the output file(s) used"
     	  },
     Consequences => {{
	  "The object ", TT "x", " is prepared for printing (with ", TO "net", ") and printed on the output file(s) ", TT "f", ".
	  If ", TT "f", " is a string, then it is interpreted as a filename and an output file is opened, used, and returned,
	  unless a single open file with the same name already exists, in which case it is used and returned.
	  Filenames starting with ", TT "!", " or with ", TT "$", " are treated specially, see ", TO "openInOut", ".
	  If ", TT "f", " is a list, then the output operation is performed on each one.
	  If ", TT "f", " is ", TO "null", ", then the output is discarded; thus ", TO "null", " is useful as a dummy output file.
	  If ", TT "f", " is omitted, as in the second usage line, then the output is sent to ", TO "stdio", ", and it will appear (usually) on the screen."
	  }},
     PARA {
	  "Parsing of ", TO "<<", " associates leftward, so that several objects  may be displayed with an expression such as ", TT "f<<x<<y<<z", "."
	  },
     EXAMPLE lines ///
     	  stderr << "-- hi there --" << endl
     	  << "-- ho there --" << endl
	  fn = temporaryFileName()
	  fn << "hi there" << endl << close
	  get fn
	  R = QQ[x]
	  f = (x+1)^10
	  << f
	  fn << f << close
     	  get fn
	  fn << toExternalString f << close
     	  get fn
	  value get fn
	  removeFile fn
     ///,
     SeeAlso => { stdio, stderr, endl, close }
     }

document {
    Key => (symbol <<, List, List),
    Headline => "component-wise comparison of lists",
    Usage => "L1 << L2",
    Inputs => { "L1" => List, "L2" => List },
    Outputs => { Boolean => "whether the first list is less than or equal to the second list in each component" },
    SourceCode => (symbol <<, List, List),
    SeeAlso => (symbol ?, List, List)
    }

document {
     Key => symbol >>,
     Headline => "a binary operator, uses include bit shifting, or attaching optional inputs to functions"
     }
