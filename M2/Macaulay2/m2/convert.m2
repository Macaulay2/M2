--		Copyright 1994 by Daniel R. Grayson

ConvertInteger
ConvertMissing
ConvertApply = x -> x			  -- (f,T1,...,Tm)
ConvertJoin = x -> if class x === Sequence then prepend (identity, x) else (identity, x)
ConvertRepeat = T -> seq T
ConvertFixedRepeat = x -> x		  -- (n,T1,...,Tm)
ConvertList  = T -> (elements, ConvertRepeat T)

document { "engine communication protocol",
     "Here is a description of the protocol for communication between the 
     front end and the engine.  At the moment, this protocol is used only
     for transmissions from the engine to the front end.",
     PARA,
     MENU {
	  TO "transmitting a positive integer",
	  TO "transmitting an integer",
	  TO "transmitting an integer mod n",
     	  TO "transmitting a sequence",
	  TO "transmitting a monomial",
	  TO "transmitting a polynomial",
	  TO "transmitting a vector",
	  TO "transmitting a matrix",
	  TO "convert"
	  }
     }


document { quote ConversionFormat,
     TT "ConversionFormat", " -- a method consulted to provide an engine conversion
     format.",
     SEEALSO ("convert", "pop")
     }

document { quote convert,
     TT "convert (fmt,str)", " -- converts a string ", TT "str", " containing data 
     transmitted from the engine in the ", TO "engine communication protocol", ".
     The argument ", TT "fmt", " is a recursive description of the format to
     be used for the conversion.",
     PARA,
     "The method named ", TO "ConversionFormat", " is used to provide
     a conversion format",
     PARA,
     "A format of the form", PRE "          seq f",
     "specifies that a sequence has been transmitted, each element of
     which should be converted with the format ", TT "f", ".  
     See ", TO "transmitting a sequence", ".",
     PARA,
     "A format of the form", PRE "          (g,x,y,z,...)",
     "where ", TT "g", " is a function, specifies that consecutive items are to
     be converted with the formats x,y,z,..., and the results of conversion
     are to be passed as arguments to the functin ", TT "g", " for processing.",
     PARA,
     "A format of the form", PRE "          (n,x,y,z,...)",
     "where ", TT "n", " is an integer, specifies that consecutive items are to
     be converted wtih the formats x,y,z,..., a total of n times.  The
     results are to be placed in a sequence.",
     PARA,
     "A format consisting of the symbol ", TO "ConvertInteger", "specifies
     that an integer has been transmitted.  See ",
     TO "transmitting an integer", ".",
     PARA,
     "Functions which assemble formats.",
     MENU {
	  TO "ConvertApply",
	  TO "ConvertFixedRepeat",
	  TO "ConvertJoin",
	  TO "ConvertList",
	  TO "ConvertMissing",
	  TO "ConvertRepeat"
	  },
     PARA,
     "A format is usually stored under the key ", TO "ConvertToExpression", "
     in the apprpriate class.",
     PARA,
     SEEALSO ("pop")
     }

document { quote ConvertToExpression,
     TT "ConvertToExpression", " -- a key for classes under which a
     conversion format is stored.",
     PARA,
     "See ", TO "convert", "."
     }

document { quote pop,
     TT "pop", " -- used as a key.  If X is a class, then X.pop will contain a
     routine which uses ", TO "convert", " to pop the top item off the
     engine's stack and return it.",
     SEEALSO "engine communication protocol"
     }

document { quote ConvertInteger,
     "A format item for communication with the engine that corresponds to
     an integer.  See ", TO "transmitting an integer", "."
     }


document { quote ConvertApply,
     TT "ConvertApply(f,T1,...,Tm)", " -- a format item for communication with
     the engine that specifies that format items T1, ..., Tm should be 
     applied to the bytes received from the engine, and then the function
     ", TT "f", " should be applied to the sequence of results.",
     PARA,
     "See ", TO "convert", "."
     }


document { quote ConvertList,
     TT "ConvertList T", " -- a format item for converting data received from the
     ", TO "engine", ", which specifies that format item T be applied to each
     element in the array, returning the results as a list.",
     PARA,
     "See ", TO "convert", "."
     }


document { quote ConvertRepeat,
     TT "ConvertRepeat T", " -- a format item for converting data received from the
     ", TO "engine", ", which specifies that format item T be applied to each
     element in the array, returning the results as a sequence.",
     PARA,
     "See ", TO "convert", "."
     }


document { quote ConvertFixedRepeat,
     TT "ConvertFixedRepeat(n,T1,...,Tm)", " -- a format item for converting data
     from the engine that specifies that the format items T1,...Tm be applied
     to the incoming data a total of n times.",
     PARA,
     "See ", TO "convert", "."
     }


document { quote ConvertJoin,
     TT "ConvertJoin(T1,...,Tm)", " -- a format item for converting data
     from the engine that specifies that format items T1,...,Tm be applied
     to the data received, and the sequence of results returned.",
     PARA,
     "If there is just one format item T1, then its result is returned.",
     PARA,
     "See ", TO "convert", "."
     }

document { "transmitting a vector",
     "The method for transmitting a vector depends on the ring involved.",
     PARA,
     "If the ring is a monoid ring (e.g., a polynomial ring), then
     the vector is transmitted as a sequence of triples ", TT "(i,m,a)", ", 
     where ", TT "i", " is the number of the row, ", TT "m", " is the monomial,
     and ", TT "a", " is the coefficient.",
     PARA,
     "If the ring is not a monoid ring, then the vector is transmitted
     as a sequences of pairs ", TT "(i,r)", " where ", TT "i", " is the 
     number of the row, and ", TT "r", " is the entry.",
     PARA,
     "The columns of a matrix are transmitted as vectors.",
     SEEALSO ("transmitting a monomial", "transmitting a matrix")
     }

document { "transmitting a matrix",
     "Most objects in the engine are stored as matrices.  Even single
     polynomials are usually stored as 1 by 1 matrices.",
     PARA,
     "A matrix is transmitted by sending the columns as a sequence of
     vectors.",
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "f = matrix ( R, {{11,0,33},{0,22,34}} )",
     EXAMPLE "ascii sendgg(ggPush f, ggtonet)",
     SEEALSO "transmitting a vector"
     }

document { "transmitting an integer mod n",
     "An integer mod n is sent as an integer.",
     PARA,
     EXAMPLE "ZZ/101[x]",
     EXAMPLE "s = 44 + x - x",
     EXAMPLE "ascii sendgg( ggPush s, ggleadcoeff, ggtonet)"
     }

document { "transmitting a polynomial",
     "A polynomial is transmitted as a sequence of pairs (m,c), where
     m is a monomial and c is a coefficient.",
     PARA,
     EXAMPLE "ZZ/101[x,y,z]",
     EXAMPLE "ascii callgg(ggtonet, 22*x^66+11*y^77)"
     }

document { "transmitting a monomial",
     "A monomial is transmitted as a sequence of pairs (i,e) of integers,
     where i refers to the i-th variable in the ring, and e is the exponent.",
     PARA,
     EXAMPLE "ZZ/3[t,u,x,y,z]",
     EXAMPLE "ascii sendgg(ggPush (t^22 * y^33 * z^55), ggleadmonom, ggtonet)"
     }

document { "transmitting a sequence",
     "Several items of the same type are transmitted as follows.  The
     number of items is transmitted first, as a positive integer of
     28 bits or less.  See ", TO "transmitting a positive integer", ".
     Then the items are transmitted.",
     PARA,
     EXAMPLE "ascii gg {33,44,55}"
     }

document { "transmitting a positive integer",
     "The integer 0 is transmitted as a single zero byte.
     A positive integer of 28 bits or less is sent 7 bits at a time, with
     the high-order nonzero seven bits sent first.  The highest bit of
     each byte except the last one is set to 1.",
     PRE "
     00000000    or
     0xxxxxxx    or
     1xxxxxxx 0xxxxxxx    or
     1xxxxxxx 1xxxxxxx 0xxxxxxx    or
     1xxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx",
     PARA,
     "A positive integer of more than 28 bits is sent as follows.  First
     come four bytes, with seven bits of the number in each one, with 
     the high bit of each byte set to 1.  Then comes the number of succeeding
     bytes, transmitted as described above for a positive integer of 28
     bits or less.  Finally come the succeeding bytes, each containing 8
     bits of the intger.  Format:",
     PRE "
     1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx  (first 28 bits)
     1xxxxxxx 0xxxxxxx                    (number of succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     ...
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     xxxxxxxx xxxxxxxx                    (succeeding bytes)",
     "It may happen that the first byte in the sequence above has the
     form 10000000."
     }

document { "transmitting an integer",
     "The integer 0 is transmitted as a single zero byte.
     Otherwise, the sign of the integer is put into bit 6 of the first byte,
     and the bits of the absolute value of the integer are packed as follows:
     6 bits into the first byte, 7 bits into 1, 2, or 3 more bytes, and 
     8 bits into each of the succeeding bytes.  If 8 bit bytes are needed,
     then the number of them is sent as a positive integer after the
     first four bytes are sent.  See also ", 
     TO "transmitting a positive integer", ".
     In the following illustration, S denotes the sign bit, and x's denote
     the bits of the integer.",
     PRE "
     00000000     or

     0Sxxxxxx     or

     1Sxxxxxx 0xxxxxxx	or

     1Sxxxxxx 1xxxxxxx 0xxxxxxx	 or

     1Sxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx   or

     1Sxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx   (first 27 bits)
     1xxxxxxx 0xxxxxxx    	      	   (number of succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     ...
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     xxxxxxxx xxxxxxxx	      	           (succeeding bytes)",
     EXAMPLE "binary = s -> concatenate between (\" \",
          apply(s,x -> apply(8, i-> string ((x >> 7-i) % 2))));",
     EXAMPLE "<< binary ascii gg 63;",
     EXAMPLE "<< binary ascii gg 64;",
     EXAMPLE "<< binary ascii gg 127;",
     EXAMPLE "<< binary ascii gg 128;",
     EXAMPLE "<< binary ascii gg 2^10;",
     EXAMPLE "<< binary ascii gg 2^20;",
     EXAMPLE "<< binary ascii gg (-2^20);",
     EXAMPLE "<< binary ascii gg 2^30;",
     EXAMPLE "<< binary ascii gg 2^40;",
     EXAMPLE "<< binary ascii gg 2^50;"
     }

document { quote sendToEngine,
     TT "sendToEngine s", " -- sends the string ", TT "s", " to the engine and returns the result.",
     PARA,
     "See also ", TO "engine communication protocol", "."
     }


document { quote ConvertMissing,
     TT "ConvertMissing", " -- a format item for converting data from the engine
     which specifies that the class for which this item has been installed
     has no conversion format specified, presumably because it corresponds
     to a type which the engine doesn't support.",
     PARA,
     "See ", TO "convert", "."
     }

TEST "
     f = (x) -> assert( x == convert(ConvertInteger,gg x) )
     scan(100, i -> (
	       f(2^i);
	       f(2^i-1);
	       f(2^i+1);
	       f(-2^i);
	       f(-2^i+1);
	       ))

     f = x -> assert( x == convert(ConvertRepeat ConvertInteger,gg x) )
     g = i -> f(i .. i+20)
     g(-10)
     g(10)
     g(100)
     g(1000)
     g(1000000)
     g(1000000000000000)
     g(10000000000000000000000000000000000000000000000000)
     "
