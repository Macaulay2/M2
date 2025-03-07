document {
     Key => symbol <<,
     Headline => "a binary operator, used for bit shifting or file output",
     SeeAlso => { "printing to a file" },
     Subnodes => { TO "left shift", TO (symbol <<, List, List) },
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
     Key => symbol >>,
     Headline => "a binary operator, used for bit shifting or attaching optional inputs to functions",
     SeeAlso => { (symbol >>, OptionTable, Function) },
     Subnodes => { TO "right shift" }
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
