--		Copyright 1995 by Daniel R. Grayson

GroebnerBasis = new Type of MutableHashTable
name GroebnerBasis := g -> name new FunctionApplication from { gb, g.matrix }
net GroebnerBasis := g -> net new FunctionApplication from { gb, g.matrix }
document { quote GroebnerBasis,
     TT "GroebnerBasis", " -- the class of all Groebner bases.",
     PARA,
     "A Groebner basis in Macaulay 2 consists of a
      Groebner basis computation, and several associated matrices. 
      Normally you don't need to refer to these objects directly, as many
      operations on matrices and modules create them, and refer to them.
      Nonetheless it is sometimes useful to have control over the
      creation of Groebner bases.",
     PARA,
     "Groebner bases are attached to a matrix using one of the following
      operations. Recomputation is avoided, in that if a Groebner basis
      has already been partially computed, then it will be used whenever
      possible.  Each of these routines takes a matrix as input, together
      with optional ", TT "Degree", " and ", TT "Hilb", ", parameters, and 
      returns a matrix. Eventually, these commands will handle the
      situtation when the ring of m is a quotient ring, or is not graded, or is
      a local ring",
      PARA,
      "Operations which produce Greobner bases:",
      MENU {
	   TO "gb"
	   },
      "Operations on Groebner bases:",
      MENU {
	   TO "getchange",
	   TO "target"
	   },
      "Operations which use Groebner bases to produce matrices:",
      MENU {
	   (TO "mingens", " m"),
	   (TO "kernel", " m"),
	   (TO "modulo", "(m,n)")
       	   },
      "Each of these operations may be
      interrupted or stopped (by typing CNTRL-C).  The computation
      is continued by re-issuing the same command.",
      PARA,
      "To obtain information from a Groebner basis, use one of the
      following routines.  These return matrices which represent the
      current state of the computation.  Further computation of the Groebner
      basis will ", EM "not", " change matrices previously obtained from
      these routines.",
      MENU {
       	   (TO "generators", " G      -- the Groebner basis matrix"),
       	   (TO "mingens", " G   -- a matrix whose columns are minimal generators
         	of the corresponding submodule"
		),
	   (TO "syz", "G        -- the syzygy matrix: the columns are the syzygies on
		'original G'")
       	   },
      "Status of the computation can be determined by the following
      routines",
      MENU {
       	   (TO "stats", " G -- display some statistics about the computation"),
       	   (TO "gbTrace", " -- provide tracing output during Groebner basis computations")
       	   },
      PARA,
      "Keys used:",
      MENU {
	   TO "GBtype",
	   TO "ring",
	   TO "returnCode"
	   },
      PARA,
      SEEALSO "gbTrace"
      }
document { quote returnCode,
     TT "returnCode", " --  a key for a ", TO "GroebnerBasis", " under which is
     stored the return code from the engine for the computation."
     }

stats GroebnerBasis := g -> (
     <<sendgg(ggPush g, ggstats);
     )

gbTrace = (n) -> (
     if class n === ZZ then (
	  sendgg(ggINT, gg n, ggtracing);
	  eePopInt())
     else error "expected an integer" )
document { quote gbTrace,
     TT "gbTrace n", " -- set the tracing level for the ", TO "engine", " to
     level n.  Meaningful values for n are 0, 1, 2, and 3.",
     PARA,
     "The notations used in tracing are :",
     MENU {
	  "z       - a syzygy has been found (a generator or S-pair reduced to zero).",
	  "m       - a new basis element has been found.",
	  "o       - an S-pair or generator reduced to zero, but no new syzygy occurred.",
	  "r       - an S-pair has been removed.",
	  "g       - a minimal generator has been found.",
	  "{2}     - beginning to reduce the S-pairs of multi-degree {2}",
	  "(15,34) - there are 15 S-pairs to do in this degree, and 34 more in 
	  higher degrees.",
	  },
     PARA,
     "The value returned is the old tracing level."
     }

document { quote GBtype,
     TT "GBtype", " -- a key used in a Groebner basis, under which is stored
     a list, recording the type of Groebner basis computation done."
     }

bool := t -> if t then 1 else 0

gbOnly       := {false,  0}
gbWithChg    := {false, -1}
gbWithSyzygy := {true , -1}
--     	    	        ^^ --- number of rows to retain, or -1 for all
--     	         ^^^^ -------- whether to collect syzygies

makeGB := (f,type,strategy) -> (
     -- if not isHomogeneous f then error "expected a homogeneous matrix";
     --   commented out to prevent a crash!
     --     intarray.hh:65: failed assertion entries != NULL
     if f#?type then f#type
     else if (
	  type===gbOnly 
	  and f#?gbWithChg 
	  and f#gbWithChg.?returnCode 
	  and f#gbWithChg.returnCode === 0
	  )
     then f#gbWithChg
     else if (
	  ( type===gbOnly or type===gbWithChg ) 
	  and f#?gbWithSyzygy
	  and f#gbWithSyzygy.?returnCode 
	  and f#gbWithSyzygy.returnCode === 0
	  )
     then f#gbWithSyzygy
     else (
	  g := new GroebnerBasis;
	  g.GBtype = type;
	  g.matrix = f;
	  g.ring = ring f;
	  g.target = target f;
	  g.handle = newHandle (
	       ggPush f,		  -- the matrix
	       ggPush bool type#0,	  -- whether to collect syzygies
	       ggPush type#1,		  -- how many rows of them to keep
	       -- MES: commented out until engine can handle this
	       if f.?cokernel and f.cokernel.?poincare then (
	           ggPush f.cokernel.poincare 
	           ),			  -- the Poincare polynomial
	       ggPush strategy, 	  -- which strategy to use (0=default)
	       gggb);
	  f#type = g;			  -- do this last (interrupts!)
	  g))

runGB := (G,ggcmds) -> (
     t := gbTrace 0;
     if t >= 4 then << "computing gb with type " << G.GBtype << endl;
     gbTrace t;
     sendgg(ggPush G, ggcmds);
     if t >= 5 then (
	  << "stack : ";
	  engineStack();
	  );
     sendgg ggcalc;
     G.returnCode = eePopInt();
     )

protect quote Compute
protect quote DegreeLimit
protect quote GeneratorLimit
protect quote SyzygyLimit
protect quote PairLimit
protect quote CodimensionLimit
protect quote Minimal
protect quote NextExponentSize
protect quote Syzygies
protect quote ChangeMatrix
protect quote SyzygyRows
protect quote Strategy

inf := t -> if t === infinity then -1 else t

cl :=method()
cl ZZ := t -> {t}
cl List := t -> (
     scan(t, i -> if class i =!= ZZ then error "expected list of integers");
     t)

gb = method(
     Options => {
	  Compute => true,
	  DegreeLimit => {},
	  GeneratorLimit => infinity,
	  SyzygyLimit => infinity,
	  PairLimit => infinity,
	  CodimensionLimit => infinity,
          SubringLimit => infinity,
	  Minimal => false,
	  Syzygies => false,
	  ChangeMatrix => false,
	  SyzygyRows => infinity,
	  Strategy => {}
	  }
     )

strategyCodes := new HashTable from {
     LongPolynomial => 8,
     Sort => 16
     }

processStrategy := (v) -> (
     if class v =!= List then v = {v};
     sum(v,s->(
	       if not strategyCodes#?s
	       then error("unknown strategy ", name s, " encountered");
	       strategyCodes#s)))     

gb Ideal := (I,options) -> gb ( module I, options )

gb Module := (M,options) -> (
     if M.?relations 
     then (
	  notImplemented();
	  -- provisional
	  m := generators M;
	  n := relations M;
	  gb (m|n, 
	       options,
	       -- ChangeMatrix => true,
	       -- Syzygies => true,
	       SyzygyRows => numgens source m))
     else gb(generators M, options))

gb Matrix := (f,options) -> (
     if ring source f =!= ring target f 
     then error "expected module map with source and target over the same ring";
     if not isFreeModule target f
     then error "Groebner bases of subquotient modules not yet implemented";
     if not isFreeModule source f
     then f = ambient f * generators source f;   -- sigh
     type := {
	  options.Syzygies,
	  if options.Syzygies or options.ChangeMatrix
	  then inf options.SyzygyRows else 0
	  };
     strat := processStrategy options.Strategy;
     G := makeGB(f, type, strat);
     if options.Compute 
     then runGB(G, (
	       ggPush cl options.DegreeLimit,
	       ggPush {
		    inf options.GeneratorLimit,
		    inf options.SyzygyLimit,
		    inf options.PairLimit,
		    inf options.CodimensionLimit,
		    bool options.Minimal,
		    inf options.SubringLimit,
		    strat
		    }));
     G)
     
document { quote gb,
     TT "gb f", " -- compute the Groebner basis for the image of a ", TO "Matrix", " f.",
     PARA,
     "If the computation is interrupted, then the partially completed
     Groebner basis is available as ", TT "f#{t,i}", ", where ", TT "t", " is true or
     false depending on whether syzygies or the change of basis matrix are 
     to be computed, and ", TT "i", " is the number of rows of the syzygy matrix to 
     be retained.  The computation can be continued by repeating the 
     ", TT "gb", " command with the same options.",
     PARA,
     "Optional arguments and flags:",
     MENU {
	  (TO "Compute", "          -- whether to start the computation"),
	  (TO "DegreeLimit", "      -- compute only up to this degree"),
	  (TO "GeneratorLimit", "   -- stop when this number of generators are
	       obtained"),
	  (TO "SyzygyLimit", "      -- stop when this number of syzygies are obtained"),
	  (TO "PairLimit", "        -- stop when this number of pairs are handled"),
	  (TO "CodimensionLimit", " -- stop when this codimension is reached"),
	  (TO "Minimal", "          -- whether to produce a set of minimal generators"),
	  (TO "NextExponentSize", " -- the number of bits to use for each
	       exponent the next time expansion occurs."),
	  (TO "Syzygies", "         -- whether to collect syzygies"),
	  (TO "ChangeMatrix", "     -- whether to produce the change of basis matrix"),
	  (TO "SyzygyRows", "       -- if syzygies are to be collected, the number
	       rows of the syzygy matrix to collect"),
	  (TO "Strategy", "         -- change the strategy used to compute Groebner bases")

	  },
     SEEALSO "GroebnerBasis"
     }
document { quote Compute,
     TT "Compute", " -- keyword for an optional argument used with
     ", TO "gb", " and ", TO "resolution", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution or Groebner basis contained in
     an interrupted computation."
     }
document { quote DegreeLimit,
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "gb", ", ", TO "resolution", ", and ", TO "quotient", ", which specifies
     that the computation should halt after dealing with degree n.",
     PARA,
     "For resolutions, one might get some matrix entries of slightly higher degree 
     than requested."
     }
document { quote GeneratorLimit,
     TT "GeneratorLimit", " -- keyword for an optional argument used with
     ", TO "gb", "."
     }
document { quote SyzygyLimit,
     TT "SyzygyLimit", " -- keyword for an optional argument used with
     ", TO "gb", " and ", TO "resolution", "."
     }
document { quote PairLimit,
     TT "PairLimit", " -- keyword for an optional argument used with
     ", TO "gb", " and ", TO "resolution", "."
     }
document { quote CodimensionLimit,
     TT "CodimensionLimit", " -- keyword for an optional argument used with
     ", TO "gb", "."
     }
document { quote Minimal,
     TT "Minimal", " -- keyword for an optional argument used with
     ", TO "gb", "."
     }
document { quote NextExponentSize,
     TT "NextExponentSize", " -- keyword for an optional argument used with
     ", TO "gb", " and ", TO "resolution", "."
     }
document { quote Strategy,
     TT "Strategy => v", " -- an optional argument used with various routines 
     to suggest a strategy for efficient computation.",
     PARA,
     "When used with ", TO "gb", " or ", TO "resolution", ", the value v should
     be a suggested strategy or list of strategies drawn from the following
     possibilities.",
     PARA,
     MENU {
	  TO "LongPolynomial",
	  TO "Sort"
	  },
     PARA,
     "When used with ", TO "pushForward1", ", the value v should be one
     of the following.",
     MENU {
	  TO "NonLinear",
     	  TO "Linear"
	  }
     }
document { quote Sort,
     TT "Sort", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA,
     "Indicates that the Groebner basis should be sorted by lead term; usually
     this is a bad idea.  Normally the basis is sorted only by degree. The
     matrix of generators is affected, as is the running time."
     }

document { quote LongPolynomial,
     TT "LongPolynomial", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA,
     "Indicates that during computation of a Groebner basis, the reduction
     routine will be replaced by one which will handle long polynomials more
     efficiently using \"geobuckets\", which accomodate the terms in buckets
     of geometrically increasing length.  This method was first used
     successfully by Thomas Yan, graduate student in CS at Cornell."
     }

document { quote Syzygies,
     TT "Syzygies", " -- keyword for an optional argument used with
     ", TO "gb", "."
     }

document { quote ChangeMatrix,
     TT "ChangeMatrix => p", " -- optional argument for ", TO "forceGB", "
     which specifies that the change of basis matrix is p.",
     BR,NOINDENT,
     TT "ChangeMatrix => true", " -- optional argument for ", TO "gb", "
     which specifies whether to compute the change of basis matrix."
     }

document { quote SyzygyRows,
     TT "SyzygyRows", " -- keyword for an optional argument used with
     ", TO "gb", "."
     }

mingens GroebnerBasis := (g,options) -> (
     sendgg(ggPush g, gggetmingens);
     getMatrix ring g			  -- we're losing information here! MES
     )

syz = method(Options => options gb)

syz GroebnerBasis := (g,options) -> (
     sendgg(ggPush g, gggetsyz);
     getMatrix ring g )

generators GroebnerBasis := (g) -> (
     sendgg(ggPush g, gggetgb);
     getMatrix ring g)

getchange GroebnerBasis := (g) -> (
     sendgg(ggPush g, gggetchange);
     getMatrix ring g)

document { quote getchange,
     TT "getchange G", " -- for a Groebner basis G, return the change of
     basis matrix from the Groebner basis to another generating set, 
     usually a minimal, or original, generating set.",
     PARA,
     "The option ", TO "ChangeMatrix", " can be used with ", TO "gb", " 
     to enable the computation of the change of basis matrix."
     }

forceGB = method(
     Options => {
          MinimalMatrix => null,
	  SyzygyMatrix => null,
	  ChangeMatrix => null
	  }
     )

forceGB Matrix := (f,options) -> (
     minmat := if options.MinimalMatrix === null
               then f
               else options.MinimalMatrix;
     changemat := if options.ChangeMatrix === null
               then map((ring f)^0, source f, 0)
               else options.ChangeMatrix;
     syzmat := if options.SyzygyMatrix === null
               then map(target changemat, 0)
               else options.SyzygyMatrix;
     nsyz := numgens target changemat;
     if nsyz >= numgens source minmat then nsyz = -1;
     type := {
	  options.SyzygyMatrix =!= null,
          nsyz
	  };
     g := new GroebnerBasis;
     g.GBtype = type;
     g.matrix = f;
     g.ring = ring f;
     g.target = target f;
     g.handle = newHandle(
          ggPush minmat,
          ggPush f, 
          ggPush changemat, 
          ggPush syzmat, 
          gggb);
     f#type = g;
     g)


document { quote forceGB,
     TT "forceGB f", " -- declares that the columns of the matrix ", TT "f", "
     constitute a Groebner basis, and returns a Groebner basis object
     encapsulating that assertion.",
     PARA,
     "Optional arguments for providing additional information:",
     MENU {
	  TO "MinimalMatrix",
	  TO "SyzygyMatrix",
	  TO "ChangeMatrix"
	  },
     "We should probably rename this function or incorporate it into
     ", TO "gb", " somehow."
     }

document { quote MinimalMatrix,
     TT "MinimalMatrix => g", " -- an option for ", TO "forceGB", " which
     specifies that the columns of g are minimal generators for the submodule
     generated by the Groebner basis."
     }

document { quote SyzygyMatrix,
     TT "SyzygyMatrix => h", " -- an option for ", TO "forceGB", " which
     specifies that the columns of h are the syzygies for the Groebner basis."
     }
    

TEST "
R = ZZ/103[a..c]
C = resolution cokernel vars R
assert(regularity C === 0)
R = ZZ/101[a .. r]
M = cokernel genericMatrix(R,a,3,6)
time C = resolution M
assert(regularity C === 2)
f = symmetricPower(2,vars R)
assert(f%a + a * (f//a) == f)
"

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (toList (t_1 .. t_9),3)			  -- 3 by 3
n = matrix pack (toList (u_1 .. u_9),3)			  -- 3 by 3
j = flatten (m * n - n * m)
k = flatten (m * n - n * m)
G = gb j
jj = generators G
assert( numgens source jj == 26 )
T = (degreesRing S)_0
assert( poincare cokernel j == 1-8*T^2+2*T^3+31*T^4-32*T^5-25*T^6+58*T^7-32*T^8+4*T^9+T^10 )
v = apply(7, i -> numgens source generators gb(k,DegreeLimit => i) )
assert (v  === {0, 0, 8, 20, 25, 26, 26} )
"

Matrix // GroebnerBasis := (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     if R =!= ring n then error "expected matrix over the same ring";
     sendgg(ggPush g, ggPush n, ggreduce, ggPush 1, ggpick, ggpop);
     getMatrix R)
RingElement // GroebnerBasis := (r,g) -> (r * id_(target g)) // g

Matrix % GroebnerBasis := (n,g) -> (
     R := ring n;
     if R =!= ring g then error "expected matrix over the same ring";
     sendgg(ggPush g, ggPush n, ggreduce, ggpop);
     getMatrix R)
RingElement % GroebnerBasis := (r,g) -> ((r * id_(target g)) % g)_(0,0)

GroebnerBasis == GroebnerBasis := (g,h) -> (
     ring g === ring h
     and (
     	  sendgg(ggPush g, ggPush h, ggisequal);
     	  eePopBool()))
