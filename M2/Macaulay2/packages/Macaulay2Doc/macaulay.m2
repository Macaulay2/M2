-- -*- coding: utf-8 -*-

document {
     Key => "replacements for commands and scripts from Macaulay",
     "Macaulay2 aims to provide all of the functionality of Macaulay, but
     the names of the functions are not the same, and there are
     other differences.  One major difference is the introduction of the
     notion of module, whereas in Macaulay, the pervasive concept was
     the matrix.",
     PARA{},
     "Here is a list of old Macaulay functions, together with pointers to
     functions in Macaulay2 that might be used to replace them.",
     PARA{},
     UL {
	  LI ("Macaulay commands:",
	       UL {
		    LI ("ac -- such destructive changes are not allowed"),
		    LI ("add -- ", TO "+"),
		    LI ("ar -- ", "not documented yet"),
		    LI ("betti -- ", TO "betti"),
		    LI ("calc -- ", "not documented yet"),
		    LI ("cat -- ", "not documented yet"),
		    LI ("ce -- ", "not documented yet"),
		    LI ("characteristic -- ", TO "char"),
		    LI ("chcalc -- ", "not documented yet"),
		    LI ("codim -- ", TO "codim"),
		    LI ("coef -- ", "not documented yet"),
		    LI ("col_degree -- ", "not documented yet"),
		    LI ("col_degs -- ", "not documented yet"),
		    LI ("commands -- ", "not documented yet"),
		    LI ("compress -- ", TO "compress"),
		    LI ("concat -- ", TO "|", ",", TO "||"),
		    LI ("continue -- ", "not documented yet"),
		    LI ("contract -- ", TO "contract"),
		    LI ("copy -- ", "not documented yet"),
		    LI ("degree -- ", TO "degree"),
		    LI ("determinants -- ", TO "minors"),
		    LI ("diag -- ", "The command ", TT "diag m n", " is equivalent to the Macaulay2
			 expression ", BR{}, TT "n = map((ring m)^(numgens source m), source m,(i,j) -> if i === j then m_(0,i) else 0)"),
		    LI ("diag -- ", "not documented yet"),
		    LI ("diff -- ", TO "diff"),
		    LI ("dshift -- ", "not documented yet"),
		    LI ("dsum -- ", TO "++"),
		    LI ("edit -- ", "not documented yet"),
		    LI ("edit_map -- ", "not documented yet"),
		    LI ("elim -- ", TO "selectInSubring"),
		    LI ("ev -- ", TO "substitute", ",", TO "RingMap"),
		    LI ("exit -- ", TO "quit"),
		    LI ("fetch -- ", TO "substitute"),
		    LI ("flatten -- ", TO "flatten"),
		    LI ("forcestd -- ", TO "forceGB"),
		    LI ("help -- ", TO "help"),
		    LI ("help_file -- ", "not documented yet"),
		    LI ("hilb -- ", TO "hilbertSeries", ",",TO "hilbertPolynomial",
			 ",",TO "hilbertFunction", ",",TO "poincare"),
		    LI ("hilb_numer -- ", TO "hilbertSeries"),
		    LI ("homog -- ", TO "homogenize"),
		    LI ("hulb -- ", "not documented yet"),
		    LI ("ideal -- ", TO "ideal", ", ", TO "matrix"),
		    LI ("iden -- ", TO "id"),
		    LI ("if -- ", TO "if"),
		    LI ("imap -- ", TO "map"),
		    LI ("in -- ", TO "leadTerm"),
		    LI ("inpart -- ", "not documented yet"),
		    LI ("int -- ", "not documented yet"),
		    LI ("intersect -- ", TO "intersect"),
		    LI ("is_zero -- ", TO "=="),
		    LI ("jacob -- ", TO "jacobian"),
		    LI ("jump -- ", "not documented yet"),
		    LI ("k-basis -- ", TO "basis"),
		    LI ("k_basis -- ", TO "basis"),
		    LI ("keep -- ", "not documented yet"),
		    LI ("kill -- ", "not documented yet"),
		    LI ("koszul -- ", TO "koszul"),
		    LI ("lift -- ", TO "//"),
		    LI ("lift_std -- ", TO "gb"),
		    LI ("listvars -- ", TO "listUserSymbols"),
		    LI ("mat -- ", TO "matrix"),
		    LI ("max -- ", TO "max"),
		    LI ("mc -- ", "not documented yet"),
		    LI ("min -- ", TO "min"),
		    LI ("modulo -- ", TO "modulo"),
		    LI ("monitor -- ", TO "using Macaulay2 with Emacs"),
		    LI ("monitoring -- ", "not documented yet"),
		    LI ("monoms -- ", "not documented yet"),
		    LI ("monprimes -- ", "not documented yet"),
		    LI ("mr -- ", "not documented yet"),
		    LI ("mult -- ", TO "*"),
		    LI ("ncols -- ", "use numgens source m"),
		    LI ("nres -- ", TO "OldChainComplexes :: resolution"),
		    LI ("nrows -- ", "use numgens target m"),
		    LI ("numinfo -- ", "not documented yet"),
		    LI ("nvars -- ", "use numgens R"),
		    LI ("outer -- ", TO "**"),
		    LI ("path -- ", TO "path"),
		    LI ("pc -- ", "not documented yet"),
		    LI ("pfaff -- ", TO "pfaffians"),
		    LI ("pmap -- ", "not documented yet"),
		    LI ("poly -- ", "not documented yet"),
		    LI ("power -- ", TO "^"),
		    LI ("pr -- ", "not documented yet"),
		    LI (BOLD "pres <C:complex> -- ", "Use ", TT "C.dd", " or ", TT "print C.dd"),
		    LI ("present_ring -- ", "not documented yet"),
		    LI ("pring -- ", "not documented yet"),
		    LI ("prmat -- ", TO "print", " or ", TO "toString"),
		    LI ("putchange -- ", "not documented yet"),
		    LI ("putmat -- ", "not documented yet"),
		    LI ("putstd -- ", "not documented yet"),
		    LI (BOLD "qring <I:ideal> <result A:ring> -- ", "In Macaulay2, if ", TT "I", " is 
			 an ideal, then ", TT "A = (ring I)/I", " is equivalent, except that in Macaulay2,
			 a Gröbner basis of I is computed for you, if it is needed"),
		    LI ("quit -- ", TO "quit"),
		    LI ("quotient -- ", "not documented yet"),
		    LI ("random -- ", "not documented yet"),
		    LI ("reduce -- ", "not documented yet"),
		    LI ("res -- ", "not documented yet"),
		    LI ("reset -- ", TO "restart"),
		    LI ("ring -- ", "not documented yet"),
		    LI ("ring-from-rows -- ", "not documented yet"),
		    LI ("ring_from_cols -- ", "not documented yet"),
		    LI ("ring_from_rows -- ", "not documented yet"),
		    LI ("ring_sum -- ", "not documented yet"),
		    LI ("rmap -- ", "not documented yet"),
		    LI ("row_degree -- ", "not documented yet"),
		    LI ("row_degs -- ", "not documented yet"),
		    LI ("sat -- ", TO "Saturation::saturate"),
		    LI ("set -- ", "not documented yet"),
		    LI ("set_value -- ", "not documented yet"),
		    LI ("setcoldegs -- ", "not documented yet"),
		    LI ("setdegs -- ", "not documented yet"),
		    LI ("setring -- ", TO "use"),
		    LI ("shout -- ", "not documented yet"),
		    LI ("size -- ", "not documented yet"),
		    LI ("smult -- ", TO "*"),
		    LI ("space -- ", 
			 "not documented yet" -- Mike wanted this: TO "engineMemory"
			 ),
		    LI ("spairs -- ", "not documented yet"),
		    LI ("spare -- ", "not documented yet"),
		    LI ("sparse -- ", "not documented yet"),
		    LI ("std -- ", "not documented yet"),
		    LI ("std_minimal -- ", "not documented yet"),
		    LI ("stdpart -- ", "not documented yet"),
		    LI ("submat -- ", TO "submatrix"),
		    LI ("subtract -- ", TO "-"),
		    LI ("syz -- ", TO "syz", ",", TO "kernel"),
		    LI ("tensor <matrix M> <matrix N> <result matrix M.N> -- ", 
			 TT "cokernel M ** cokernel N"),
		    LI ("trace -- ", TO "trace"),
		    LI ("transpose -- ", TO "transpose"),
		    LI ("truncate -- ", TO "Truncations::truncate(ZZ,Module)"),
		    LI ("type -- ", "not documented yet"),
		    LI ("version -- ", TO "version"),
		    LI ("wedge -- ", TO "minors", ",", TO "exteriorPower")
		    }),
	  LI ("Macaulay scripts from 'scriptsde':",
	       UL {
		    LI ("<adj_of_cat -- ", "not documented yet"),
		    LI ("<adjoin_fractions -- ", "not documented yet"),
		    LI ("<adjoint -- ", "not documented yet"),
		    LI ("<analytic_spread -- ", "not documented yet"),
		    LI ("<annihilated -- ", "not documented yet"),
		    LI ("<annihilator -- ", TO "Saturation::annihilator"),
		    LI ("<annihilator1 -- ", TO "Saturation::annihilator"),
		    LI ("<annihilator2 -- ", TO "Saturation::annihilator"),
		    LI ("<binomial -- ", TO "binomial"),
		    LI ("<ceiling -- ", "not documented yet"),
		    LI ("<changelog -- ", "not documented yet"),
		    LI ("<codim -- ", "not documented yet"),
		    LI ("<cohomology -- ", "not documented yet"),
		    LI ("<cohomology1 -- ", "not documented yet"),
		    LI ("<column_vector -- ", "not documented yet"),
		    LI ("<complement -- ", "not documented yet"),
		    LI ("<copyring -- ", "not documented yet"),
		    LI ("<cotan -- ", "not documented yet"),
		    LI ("<cotan_bihom -- ", "not documented yet"),
		    LI ("<curve_on_cubic -- ", "not documented yet"),
		    LI ("<diagonal_submodule -- ", "not documented yet"),
		    LI ("<diff -- ", "not documented yet"),
		    LI ("<double_dual -- ", "not documented yet"),
		    LI ("<double_dual1 -- ", "not documented yet"),
		    LI ("<dual_variety -- ", "not documented yet"),
		    LI ("<empty_array -- ", "not documented yet"),
		    LI ("<equality -- ", "not documented yet"),
		    LI ("<ext -- ", "not documented yet"),
		    LI ("<ext(-,r) -- ", "not documented yet"),
		    LI ("<extend_ring -- ", "not documented yet"),
		    LI ("<from_bigraded -- ", "not documented yet"),
		    LI ("<from_div_powers -- ", "not documented yet"),
		    LI ("<generic_mat -- ", TO "genericMatrix"),
		    LI ("<getvars -- ", "not documented yet"),
		    LI ("<hom -- ", "not documented yet"),
		    LI ("<hom_and_map -- ", "not documented yet"),
		    LI ("<hom_is_0 -- ", "not documented yet"),
		    LI ("<homology -- ", "not documented yet"),
		    LI ("<i_in_j -- ", "not documented yet"),
		    LI ("<ideal -- ", "not documented yet"),
		    LI ("<idempotent -- ", "not documented yet"),
		    LI ("<idencoldegs -- ", "not documented yet"),
		    LI ("<idenrowdegs -- ", "not documented yet"),
		    LI ("<interchange -- ", "not documented yet"),
		    LI ("<interchange_permutation -- ", "not documented yet"),
		    LI ("<inverse -- ", "not documented yet"),
		    LI ("<is_zero -- ", "not documented yet"),
		    LI ("<k3carpet -- ", "not documented yet"),
		    LI ("<kernel -- ", "not documented yet"),
		    LI ("<kernel_and_map -- ", "not documented yet"),
		    LI ("<kosz_hom1 -- ", "not documented yet"),
		    LI ("<kosz_hom2 -- ", "not documented yet"),
		    LI ("<l_i_in_j -- ", "not documented yet"),
		    LI ("<l_intersect -- ", "not documented yet"),
		    LI ("<l_res -- ", "not documented yet"),
		    LI ("<lex_seg_ideal -- ", "not documented yet"),
		    LI ("<macaulayrep -- ", "not documented yet"),
		    LI ("<map_from_col -- ", "not documented yet"),
		    LI ("<minpres -- ", "not documented yet"),
		    LI ("<module_iso -- ", "not documented yet"),
		    LI ("<monomial_curve -- ", "not documented yet"),
		    LI ("<mult_ideals -- ", "not documented yet"),
		    LI ("<nbyn_commuting -- ", "not documented yet"),
		    LI ("<normal_sheaf -- ", "not documented yet"),
		    LI ("<nres -- ", "not documented yet"),
		    LI ("<nzd -- ", "not documented yet"),
		    LI ("<orbit_equations -- ", "not documented yet"),
		    LI ("<permutation -- ", "not documented yet"),
		    LI ("<perp -- ", "not documented yet"),
		    LI ("<points -- ", "not documented yet"),
		    LI ("<powers -- ", "not documented yet"),
		    LI ("<project_from_product -- ", "not documented yet"),
		    LI ("<projective_plane -- ", "not documented yet"),
		    LI ("<prune -- ", TO "prune"),
		    LI ("<prune_and_map -- ", "not documented yet"),
		    LI ("<push_forward -- ", TO "pushForward"),
		    LI ("<push_forward1 -- ", TO "coimage", " (eventually)"),
		    LI ("<quotient1 -- ", TO "quotient"),
		    LI ("<radical -- ", TO "MinimalPrimes::radical"),
		    LI ("<random_element -- ", "not documented yet"),
		    LI ("<random_int -- ", "not documented yet"),
		    LI ("<random_map -- ", "not documented yet"),
		    LI ("<random_mat -- ", "not documented yet"),
		    LI ("<rank_prob -- ", "not documented yet"),
		    LI ("<rat_nor_curve -- ", "not documented yet"),
		    LI ("<rat_nor_osc_locus -- ", "not documented yet"),
		    LI ("<rational_surface -- ", "not documented yet"),
		    LI ("<rational_surface1 -- ", "not documented yet"),
		    LI ("<reduce_syzygy_1 -- ", "not documented yet"),
		    LI ("<regular_sequence -- ", "not documented yet"),
		    LI ("<regular_sequence1 -- ", "not documented yet"),
		    LI ("<regularity -- ", TO "regularity"),
		    LI ("<remove_low_dim -- ", "not documented yet"),
		    LI ("<remove_low_dim_id -- ", "not documented yet"),
		    LI ("<remove_lowest_dim -- ", "not documented yet"),
		    LI ("<representatives -- ", "not documented yet"),
		    LI ("<representatives_old -- ", "not documented yet"),
		    LI ("<res -- ", TO "OldChainComplexes :: resolution"),
		    LI ("<res_and_dim -- ", TO "OldChainComplexes :: resolution", ",", TO "pdim"),
		    LI ("<ring -- ", "not documented yet"),
		    LI ("<sagbi -- ", "not documented yet"),
		    LI ("<sagbi_step -- ", "not documented yet"),
		    LI ("<sat -- ", TO "Saturation::saturate"),
		    LI ("<sat1 -- ", TO "Saturation::saturate"),
		    LI ("<scroll -- ", "not documented yet"),
		    LI ("<select -- ", "not documented yet"),
		    LI ("<shout_list -- ", "not documented yet"),
		    LI ("<sort_by_degree -- ", "not documented yet"),
		    LI ("<stack -- ", TO "matrix", ",",TO "map"),
		    LI ("<submat_by_degs -- ", "not documented yet"),
		    LI ("<subring -- ", TO "kernel"),
		    LI ("<sym -- ", "not documented yet"),
		    LI ("<sym_cokernel -- ", "not documented yet"),
		    LI ("<template_for_scripts -- ", "not documented yet"),
		    LI ("<to_div_powers -- ", "not documented yet"),
		    LI ("<tor -- ", TO "Tor"),
		    LI ("<unmixed_radical -- ", TO "MinimalPrimes::radical"),
		    LI ("<unmixed_radical1 -- ", TO "MinimalPrimes::radical"),
		    LI ("<unmixed_radical2 -- ", TO "MinimalPrimes::radical"),
		    LI ("<wedge_cokernel -- ", "not documented yet"),
		    LI ("<x_to_last -- ", TO "newCoordinateSystem"),
		    LI ("<zeromat -- ", "not documented yet")
		    }),
	  LI ("Macaulay scripts from 'scriptsmj':",
	       UL {
		    LI ("<2BYN -- ", "not documented yet"),
		    LI ("<add_matrix_to_array -- ", "not documented yet"),
		    LI ("<check_complex -- ", "not documented yet"),
		    LI ("<check_exact -- ", "not documented yet"),
		    LI ("<comp_to_array -- ", "not documented yet"),
		    LI ("<eagon_northcott -- ", "not documented yet"),
		    LI ("<extract_matrix -- ", "not documented yet"),
		    LI ("<homology_of_array -- ", "not documented yet"),
		    LI ("<jordan0 -- ", "not documented yet"),
		    LI ("<koszul_complex -- ", "not documented yet"),
		    LI ("<lift -- ", "not documented yet"),
		    LI ("<lift_arrays -- ", "not documented yet"),
		    LI ("<mc -- ", "not documented yet"),
		    LI ("<nilpotent0 -- ", "not documented yet"),
		    LI ("<pres -- ", "not documented yet"),
		    LI ("<smult_complex -- ", "not documented yet"),
		    LI ("<splice_resns -- ", "not documented yet")
		    }),
	  LI ("Macaulay scripts from 'scriptsms':",
	       UL {
		    LI ("<block_map -- ", "not documented yet"),
		    LI ("<blowup -- ", "not documented yet"),
		    LI ("<blowup0 -- ", "not documented yet"),
		    LI ("<generic_skew_mat -- ", TO "genericSkewMatrix"),
		    LI ("<generic_sym_mat -- ", TO "genericSymmetricMatrix"),
		    LI ("<hilb_fcn -- ", "not documented yet"),
		    LI ("<inhomog_std -- ", "not documented yet"),
		    LI ("<l_dual0 -- ", "not documented yet"),
		    LI ("<l_from_dual -- ", TO "InverseSystems::fromDual"),
		    LI ("<l_homog0 -- ", "not documented yet"),
		    LI ("<l_min0 -- ", "not documented yet"),
		    LI ("<l_minimal -- ", "not documented yet"),
		    LI ("<l_minimal0 -- ", "not documented yet"),
		    LI ("<l_tangentcone -- ", "not documented yet"),
		    LI ("<l_to_dual -- ", TO "InverseSystems::toDual"),
		    LI ("<line_bundle_image -- ", "not documented yet"),
		    LI ("<normal_cone -- ", "not documented yet"),
		    LI ("<proj_bundle -- ", "not documented yet"),
		    LI ("<rmap -- ", "not documented yet"),
		    LI ("<symmetric_algebra -- ", "not documented yet")
		    })
	  },
     HEADER3 "add <matrix> <matrix> <matrix>",
     "Add matrices ", TT "m", " and ", TT "n", ", using ",
     TT "result = m + n", ".  In general, arithmetic in Macaulay2 
     is done using natural mathematical expressions.  See ", TO "+", ".",

     HEADER3 "betti <res>",
     "Display the graded betti numbers of a chaincomplex or resolution ", 
     TT "C",
     " using ",  TT "betti C"     , ".  See ", TO "betti", ".",
     
     HEADER3 "calc <computation> [hi degree]",
     HEADER3 "chcalc <computation> [new hi deg]",
     "Compute the Gröbner basis of ", 
     TT "m", " up to a given degree, using ", TT "gb(m, DegreeLimit=>3)", 
     ".  Similarly,
     if a computation in Macaulay2 has been halted by pressing <CTRL>-C, 
     the computation
     can be restarted from where it left off, by giving the original 
     command, e.g., ", TT "gb m", ".",
     
     HEADER3 "cat <first var(0..numvars-1)> <result matrix>",
     "Macaulay1 then also prompts for two lists of integers, say ", 
     TT "rows", 
     " and ", TT "cols", ".  The resulting catalecticant matrix can 
     be obtained in
     Macaulay2 in the following way.",
     EXAMPLE {
	  "R = QQ[a..h]",
     	  "rows = {0,1,2}",
     	  "cols = {0,3}",
     	  "result = map(R^3, 2, (i,j) -> R_(rows_i + cols_j))"},
     --SeeAlso => (genericMatrix, genericSymmetricMatrix, genericSkewMatrix),
     
     HEADER3 "characteristic <ring> [result integer]",
     "To obtain the characteristic of a ring ", TT "R", ", use ",
     TT "char R", ".  See ", TO "char", ".",
     
     HEADER3 "codim <standard basis> [integer result]",
     "When computing the codimension of an ideal or module ", TT "M", 
     ", a Gröbner basis is computed automatically, if needed.  Use ", 
     TT "codim M", ".",
     "One use of this command in Macaulay1 was to compute the codimension 
     of a partially
     computed Gröbner basis.  ",
     -- Mike wanted this: "See ", TO "computing with partial results", " for examples of doing this.",
     HEADER3 "coef <matrix> <result monoms> <result coefs> [variable list]",
     "To obtain the matrices of monomials and their coefficients, use ", 
     TO "coefficients", ".  The following example obtains the coefficients 
     of the variable ", TT "a", " (which is variable number 0)",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
     	  "m = matrix{{a^2+a^2*c+a*b+3*d}}",
     	  "result = coefficients(m, Variables => {a})",
     	  "result_0",
     	  "result_1"},
     HEADER3 "col_degree <matrix> <column> [result integer]",
     "To obtain the degree of the ", TT "i", "-th column of a matrix ", 
     TT "m", ", use ", TT "(degrees source m)_i", ".  See ", TO "degrees",
     ".  Note that in Macaulay2, one can use multi-degrees, so the result
     is a list of integers.  Also all indices in Macaulay2 start at 0.",
     EXAMPLE {
	  "R = QQ[a,b,Degrees=>{{1,0},{1,-1}}];",
     	  "m = matrix{{a*b, b^2}}",
     	  "(degrees source m)_0"},
     HEADER3 "col_degs <matrix> [column degrees]",
     "Compute the list of degrees of the columns of a matrix ", TT "m",
     " using ", TT "degrees source m", ".  The result is a list of degrees.
     Each degree is itself a list of integers, since multi-degrees are 
     allowed.",
     
     HEADER3 "commands",
     "Using Macaulay2 in Emacs, type the first few letters of a command,
     then hit ", KBD "Tab", ". The list of all commands starting with those letters
     will be displayed, or if there is only one, Emacs will complete the typing
     for you.",

     HEADER3 "compress <matrix> <result matrix>",
     "Remove columns of a matrix ", TT "m", " which are entirely zero
     using ", TT "compress m", ". See ", TO "compress", ".",
     
     HEADER3 "concat <matrix> <matrix2> ... <matrix n>",
     "Concatenate matrices ", TT "m1", ", ", TT "m2", ", ..., ", TT "mn",
     " using ", TT "m1 | m2 | ... | mn", ".  See ", TO "|", 
     ".  The main difference is that
     in Macaulay2 this command does not overwrite the first matrix.",
     
     HEADER3 "continue",
     "In Macaulay2, one interrupts a computation using <CTRL>-C, as in 
     Macaulay1.  Restart the computation using the original command.  For 
     Gröbner basis and resolution commands, the computation starts off 
     where it left off.",
     
     HEADER3 "contract <ideal> <ideal> <result matrix>",
     "To contract the matrix ", TT "n", " by the matrix ", TT "m", " use ",
     TT "contract(m,n)", ".  See ", TO "contract", ".  In Macaulay2, the
     arguments are not constrained to be matrices with one row.",
     
     HEADER3 "copy <existing matrix> <copy of matrix>",
     "Since matrices in Macaulay2 are immutable objects, this command is
     no longer necessary.  One can still make a copy of a matrix.  For
     example",
     EXAMPLE {
	  "R = ZZ/101[a..d]",
     	  "m = matrix{{a,b},{c,d}}",
     	  "copym = map(target m, source m, entries m)"},
     
     HEADER3 "degree <standard basis> [integer codim] [integer degree]",
     "When computing the degree of an ideal or module ", TT "M", 
     ", a Gröbner basis is computed automatically, if needed.  Use ", 
     TT "degree M", ".  Note that in Macaulay2, ", TT "degree", " returns
     only the degree.  Use ", TT "codim M", " to obtain the codimension.",
     "One use of this command in Macaulay1 was to compute the degree
     of a partially
     computed Gröbner basis.  ",
     -- Mike wanted this: "See ", TO "computing with partial results", " for examples of doing this.",

     HEADER3 "determinants <matrix> <p> <result>",
     "Use ", TT "minors(p,m)", " to compute the ideal of p by p minors
     of the matrix ", TT "m", ".  See ", TO "minors", ".",
     
     HEADER3 "diag <matrix> <result>",
     "To make a diagonal matrix whose diagonal entries are taken from
     a matrix ", TT "m", ", it is necessary to build the matrix directly, 
     as in the following example.",
     EXAMPLE {
	  "R = ZZ[a..d];",
      	  "m = matrix{{a^2,b^3,c^4,d^5}}",
      	  "map(R^(numgens source m), source m, 
                 (i,j) -> if i === j then m_(0,i) else 0)"},
     
     HEADER3 "diff <ideal> <ideal> <result matrix>",
     "To differentiate the matrix ", TT "n", " by the matrix ", TT "m", 
     " use ",
     TT "diff(m,n)", ".  See ", TO "diff", ".  In Macaulay2, the
     arguments are not constrained to be matrices with one row.",
     
     HEADER3 "dshift <matrix> <degree to shift by>",
     "To shift the degrees of a matrix ", TT "m", " by an integer ", TT "d",
     ", use ", TT "m ** (ring m)^{-d}", ".  See ", TO "**", ".  Note that this
     returns a matrix with the degrees shifted, and does not modify the 
     original matrix ", TT "m", ".  For example",
     EXAMPLE {
	  "R = ZZ[a..d];",
      	  "m = matrix{{a,b^2},{c^2,d^3}}",
      	  "betti m",
      	  "n = m ** R^{-1}",
      	  "betti n"},
     
     HEADER3 "dsum <matrix> ... <matrix> <result>",
     "To form the direct sum of matrices ", TT "m1, m2, ..., mn", ", use ",
     TT "m1 ++ m2 ++ ... ++ mn", ".  See ", TO "++", ".",
     
     HEADER3 "elim <standard basis> <result matrix> [n]",
     "To select the columns of the matrix ", TT "m", " which lie in the
     subring defined by the first ", TT "n", " slots of the monomial order
     being zero, use ", TT "selectInSubring(n,m)", ".  Usually, one uses the
     value of 1 for ", TT "n", ".",
     
     HEADER3 "ev <ideal> <matrix> <changed matrix>",
     "In Macaulay2, one uses ring maps, or ", TO "substitute", " to
     substitute variables.  If ", TT "m", " is a matrix in a ring ", TT "R",
     " having ", TT "n", " variables, and ", TT "f", " is a 1 by n matrix
     over some ring, then use ", TT "substitute(m,f)", " to perform the
     substitution.  For example,",
     EXAMPLE {
	  "R = QQ[a..d]",
      	  "S = QQ[s,t]",
      	  "m = matrix{{a^2-d, b*c}}",
      	  "f = matrix{{s^4,s^3*t,s*t^3,t^4}}",
      	  "substitute(m,f)"},
     "In Macaulay2, one may also create and apply ring maps",
     EXAMPLE {
	  "F = map(R,R,{b,c,d,a})",
      	  "m + F m + F F m + F F F m"},
     "Or one may substitute for only some variables",
     EXAMPLE "substitute(m, {a=>1, b=>3})",
     
     HEADER3 "exit",
     "Use ", TO "exit", " to quit Macaulay2.",
     
     HEADER3 "fetch <matrix> <result matrix> [ones, default=zeros]",
     "In order to bring a matrix ", TT "m", " to a ring ", TT "S", " which
     has ring variables of the same name, use ", TT "substitute(m,S)", ".",
     EXAMPLE {
	  "R = ZZ[s,t]",
      	  "m = s^2+t^2",
      	  "S = R[a..d]",
      	  "substitute(m,S)"},
     
     HEADER3 "flatten <matrix> <result ideal of entries>",
     "In order to form a one row matrix with the entries of a matrix ", 
     TT "m", ", use ", TT "flatten m", ".  See ", TO "flatten", ".",

     HEADER3 "forcestd <matrix> <result standard basis> [change of basis]",
     "In order to inform the system that the columns of the matrix ", TT "m",
     " form a Gröbner basis, use ", TT "forceGB m", ".  See ", TO "forceGB",
     ".",
     
     -*
     -- Mike wanted this: 
     HEADER3 "hilb <standard basis>",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "hilb_numer <standard basis> <ideal> <result>",
     "WRITE THIS",
     *-
     
     HEADER3 "homog <matrix> <homog variable> <new matrix>",
     "To homogenize a matrix ", TT "m", " with respect to a variable ",
     TT "x", ", use ", TT "homogenize(m,x)", ". One may also homogenize
     with respect to a given weight vector.  See ", TO "homogenize", ".",
     
     -*
     -- Mike wanted this: 
     HEADER3 "hulb <standard basis> <deg>",
     "WRITE THIS",
     *-
     
     HEADER3 "ideal <resulting matrix>",
     "To enter a one row matrix, use we may use the following method.",
     EXAMPLE {
	  "R = ZZ[a..d]",
      	  "f = matrix{{a^2-b*c,3*b*c^4-1}}"},
     "Remember that ideals, modules, and matrices are all different in
     Macaulay2.  One can easily change between them, as in:",
     EXAMPLE {
	  "J = ideal f",
      	  "generators J",
      	  "image f",
      	  "cokernel f"},
     
     HEADER3 "iden <size> <result>",
     "To make the identity map on a module ", TT "F", ", use ", 
     TT "id_F", ", as in ",
     EXAMPLE "id_(R^4)",
     "See ", TO "id", ".",
     
     HEADER3 "if <integer> <label1> [<label2>]",
     "For conditional execution, use the if-then or if-then-else statement",
     -- Mike wanted this: "WRITE MORE",
     
     -*
     -- Mike wanted this: 
     HEADER3 "imap <new ring map> <R> <S> [ones, default=zeros]",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "in <standard basis> [optional result matrix] [n]",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "inpart <standard basis> <result matrix> [variable list]",
     "WRITE THIS",
     *-
     
     HEADER3 "int <name> <new value>",
     "To assign a value to a variable in Macaulay2, use ", TO "=", 
     ".  For example,",
     EXAMPLE "myanswer = 2*(numgens R) - 1",
     "Warning: In Macaulay1, names of variables in rings, and user defined 
     variables are completely separate.  In Macaulay2, if you assign something
     to a ring variable, it will assume its new value. ",
     EXAMPLE {
	  "R = ZZ/31991[a..d]",
      	  "a",
      	  "a = 43",
      	  "a",
      	  "use R",
      	  "a"},
     
     HEADER3 "intersect <mat 1> ... <mat n> <result computation>",
     "To intersect ideals, or submodules ", TT "I1, I2, ..., In",
     ", use ", TT "intersect(I1,I2,...,In)", ".  The main difference
     is that these values cannot be matrices, they must be ideals or 
     submodules.  A second difference is that the computation, if interrupted,
     must be restarted at the beginning.  See ", TO "intersect", 
     ".  For example,",
     EXAMPLE {
	  "I = ideal(a^2-b,c-1,d^2-a*b)",
      	  "J = ideal(a*b-1, c*d-2)",
      	  "intersect(I,J)"},

     HEADER3 "is_zero <poly> <result integer: 1 if zero, else 0>",
     "To decide whether a polynomial, matrix, ideal, etc., ", TT "f", 
     " is zero, use ", TT "f == 0", ".  The resulting value is a Boolean: 
     either ", TT "true", ", or ", TT "false", ".  See ", TO "==", ".",
     
     HEADER3 "jacob <ideal> <resulting jacobian> [variable list]",
     "To find the Jacobian matrix of an ideal or one row matrix", 
     TT "f", ", use ", TT "jacobian f", ".  If you only wish to differentiate
     with some of the variables, use ", TO "diff", " instead.  See ",
     TO "jacobian", ".",
     
     HEADER3 "jump <label>",
     "Macaulay2 has no go to statements.  Instead, you should use the
     control structures.", -- Mike wanted this: "WRITE MORE",
     
     -*
     -- Mike wanted this: 
     HEADER3 "k_basis <matrix> <result matrix> [variable list]",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "keep <standard basis> <result matrix> [n]",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "kill <var1> ... <var n>",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "koszul <int n, or matrix> <p> <result matrix>",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "lift <standard basis> <matrix to lift> <result>",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "lift_std <matrix> <computation>",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "listvars",
     "WRITE THIS",
     *-
     
     -*
     -- Mike wanted this: 
     HEADER3 "mat <result matrix> [optional: file name]",
     "WRITE THIS",
     *-
     
     }

///
-- Here we put routines that mimic the Macaulay commands

diag = (m) -> (
     -- input: m : Matrix of size 1 by n
     -- returns a n by n matrix whose diagonal entries are the entries of m.
     F := source m;
     map((ring m)^(numgens F), F, (i,j) -> if i === j then m_(0,i) else 0))

R = ZZ/101[vars(0..4)]
m = matrix{{a,b,c,d,e}}
n = diag m
isHomogeneous n

     EXAMPLE {
	  "R = QQ[a..h]",
      	  "I = ideal(a,b^5,c^7-d^7-a^7)",
      	  "G = gb(I,StopBeforeComputation=>true)",
      	  "gens G"},
     "Note that no Gröbner basis elements have been computed yet",
     EXAMPLE {
	  "gb(I,PairLimit=>2)",
      	  "m = gens G"},
     "To find the codimension of the monomial ideal of lead terms so far computed,
     we use:",
     EXAMPLE "codim cokernel leadTerm m"

	       LI (". -- not available"),
	       LI ("args -- ", "not documented yet"),
	       LI ("cache_mem -- ", "not documented yet"),
	       LI ("cdir -- ", "not documented yet"),
	       LI ("echo -- ", "not documented yet"),
	       LI ("endmon -- ", "not documented yet"),
	       LI ("incoef -- ", "not documented yet"),
	       LI ("incr_set -- ", "not documented yet"),

///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
