
document {
     Key => "replacements for commands and scripts from Macaulay",
     "Macaulay 2 aims to provide all of the functionality of Macaulay, but
     the names of the functions are not the same, and there are
     other differences.  One major difference is the introduction of the
     notion of module, whereas in Macaulay, the pervasive concept was
     the matrix.",
     PARA{},
     "Here is a list of old Macaulay functions, together with pointers to
     functions in Macaulay 2 that might be used to replace them.",
     PARA{},
     UL {
	  SEQ ("Macaulay commands:",
	  UL {
	       SEQ ("ac -- such destructive changes are not allowed"),
	       SEQ ("add -- ", TO "+"),
	       SEQ ("ar -- ", TO "not documented yet"),
	       SEQ ("betti -- ", TO "betti"),
	       SEQ ("calc -- ", TO "not documented yet"),
	       SEQ ("cat -- ", TO "not documented yet"),
	       SEQ ("ce -- ", TO "not documented yet"),
	       SEQ ("characteristic -- ", TO "char"),
	       SEQ ("chcalc -- ", TO "not documented yet"),
	       SEQ ("codim -- ", TO "codim"),
	       SEQ ("coef -- ", TO "not documented yet"),
	       SEQ ("col_degree -- ", TO "not documented yet"),
	       SEQ ("col_degs -- ", TO "not documented yet"),
	       SEQ ("commands -- ", TO "not documented yet"),
	       SEQ ("compress -- ", TO "compress"),
	       SEQ ("concat -- ", TO "|", ",", TO "||"),
	       SEQ ("continue -- ", TO "not documented yet"),
	       SEQ ("contract -- ", TO "contract"),
	       SEQ ("copy -- ", TO "not documented yet"),
	       SEQ ("degree -- ", TO "degree"),
	       SEQ ("determinants -- ", TO "minors"),
	       SEQ ("diag -- ", "The command ", TT "diag m n", " is equivalent to the Macaulay2
		    expression ", BR{}, TT "n = map((ring m)^(numgens source m), source m,(i,j) -> if i === j then m_(0,i) else 0)"),
	       SEQ ("diag -- ", TO "not documented yet"),
	       SEQ ("diff -- ", TO "diff"),
	       SEQ ("dshift -- ", TO "not documented yet"),
	       SEQ ("dsum -- ", TO "++"),
	       SEQ ("edit -- ", TO "not documented yet"),
	       SEQ ("edit_map -- ", TO "not documented yet"),
	       SEQ ("elim -- ", TO "selectInSubring"),
	       SEQ ("ev -- ", TO "substitute", ",", TO "RingMap"),
	       SEQ ("exit -- ", TO "quit"),
	       SEQ ("fetch -- ", TO "substitute"),
	       SEQ ("flatten -- ", TO "flatten"),
	       SEQ ("forcestd -- ", TO "forceGB"),
	       SEQ ("help -- ", TO "help"),
	       SEQ ("help_file -- ", TO "not documented yet"),
	       SEQ ("hilb -- ", TO "hilbertSeries", ",",TO "hilbertPolynomial",
		    ",",TO "hilbertFunction", ",",TO "poincare"),
	       SEQ ("hilb_numer -- ", TO "hilbertSeries"),
	       SEQ ("homog -- ", TO "homogenize"),
	       SEQ ("hulb -- ", TO "not documented yet"),
	       SEQ ("ideal -- ", TO "ideal", ", ", TO "matrix"),
	       SEQ ("iden -- ", TO "id"),
	       SEQ ("if -- ", TO "if"),
	       SEQ ("imap -- ", TO "map"),
	       SEQ ("in -- ", TO "leadTerm"),
	       SEQ ("inpart -- ", TO "not documented yet"),
	       SEQ ("int -- ", TO "not documented yet"),
	       SEQ ("intersect -- ", TO "intersect"),
	       SEQ ("is_zero -- ", TO "=="),
	       SEQ ("jacob -- ", TO "jacobian"),
	       SEQ ("jump -- ", TO "not documented yet"),
	       SEQ ("k-basis -- ", TO "basis"),
	       SEQ ("k_basis -- ", TO "basis"),
	       SEQ ("keep -- ", TO "not documented yet"),
	       SEQ ("kill -- ", TO "not documented yet"),
	       SEQ ("koszul -- ", TO "koszul"),
	       SEQ ("lift -- ", TO "//"),
	       SEQ ("lift_std -- ", TO "gb"),
	       SEQ ("listvars -- ", TO "listUserSymbols"),
	       SEQ ("mat -- ", TO "matrix"),
	       SEQ ("max -- ", TO "max"),
	       SEQ ("mc -- ", TO "not documented yet"),
	       SEQ ("min -- ", TO "min"),
	       SEQ ("modulo -- ", TO "modulo"),
	       SEQ ("monitor -- ", TO "emacs"),
	       SEQ ("monitoring -- ", TO "not documented yet"),
	       SEQ ("monoms -- ", TO "not documented yet"),
	       SEQ ("monprimes -- ", TO "not documented yet"),
	       SEQ ("mr -- ", TO "not documented yet"),
	       SEQ ("mult -- ", TO "*"),
	       SEQ ("ncols -- ", "use numgens source m"),
	       SEQ ("nres -- ", TO "resolution"),
	       SEQ ("nrows -- ", "use numgens target m"),
	       SEQ ("numinfo -- ", TO "not documented yet"),
	       SEQ ("nvars -- ", "use numgens R"),
	       SEQ ("outer -- ", TO "**"),
	       SEQ ("path -- ", TO "path"),
	       SEQ ("pc -- ", TO "not documented yet"),
	       SEQ ("pfaff -- ", TO "pfaffians"),
	       SEQ ("pmap -- ", TO "not documented yet"),
	       SEQ ("poly -- ", TO "not documented yet"),
	       SEQ ("power -- ", TO "^"),
	       SEQ ("pr -- ", TO "not documented yet"),
	       SEQ (BOLD "pres <C:complex> -- ", "Use ", TT "C.dd", " or ", TO "print C.dd"),
	       SEQ ("present_ring -- ", TO "not documented yet"),
	       SEQ ("pring -- ", TO "not documented yet"),
	       SEQ ("prmat -- ", TO "print", " or ", TO "toString"),
	       SEQ ("putchange -- ", TO "not documented yet"),
	       SEQ ("putmat -- ", TO "not documented yet"),
	       SEQ ("putstd -- ", TO "not documented yet"),
	       SEQ (BOLD "qring <I:ideal> <result A:ring> -- ", "In Macaulay2, if ", TT "I", " is 
		    an ideal, then ", TT "A = (ring I)/I", " is equivalent, except that in Macaulay2,
		    a Groebner basis of I is computed for you, if it is needed"),
	       SEQ ("quit -- ", TO "quit"),
	       SEQ ("quotient -- ", TO "not documented yet"),
	       SEQ ("random -- ", TO "not documented yet"),
	       SEQ ("reduce -- ", TO "not documented yet"),
	       SEQ ("res -- ", TO "not documented yet"),
	       SEQ ("reset -- ", TO "restart"),
	       SEQ ("ring -- ", TO "not documented yet"),
	       SEQ ("ring-from-rows -- ", TO "not documented yet"),
	       SEQ ("ring_from_cols -- ", TO "not documented yet"),
	       SEQ ("ring_from_rows -- ", TO "not documented yet"),
	       SEQ ("ring_sum -- ", TO "not documented yet"),
	       SEQ ("rmap -- ", TO "not documented yet"),
	       SEQ ("row_degree -- ", TO "not documented yet"),
	       SEQ ("row_degs -- ", TO "not documented yet"),
	       SEQ ("sat -- ", TO "saturate"),
	       SEQ ("set -- ", TO "not documented yet"),
	       SEQ ("set_value -- ", TO "not documented yet"),
	       SEQ ("setcoldegs -- ", TO "not documented yet"),
	       SEQ ("setdegs -- ", TO "not documented yet"),
	       SEQ ("setring -- ", TO "use"),
	       SEQ ("shout -- ", TO "not documented yet"),
	       SEQ ("size -- ", TO "not documented yet"),
	       SEQ ("smult -- ", TO "*"),
	       SEQ ("space -- ", TO "engineMemory"),
	       SEQ ("spairs -- ", TO "not documented yet"),
	       SEQ ("spare -- ", TO "not documented yet"),
	       SEQ ("sparse -- ", TO "not documented yet"),
	       SEQ ("std -- ", TO "not documented yet"),
	       SEQ ("std_minimal -- ", TO "not documented yet"),
	       SEQ ("stdpart -- ", TO "not documented yet"),
	       SEQ ("submat -- ", TO "submatrix"),
	       SEQ ("subtract -- ", TO "-"),
	       SEQ ("syz -- ", TO "syz", ",", TO "kernel"),
	       SEQ ("tensor <matrix M> <matrix N> <result matrix M.N> -- ", 
		    TT "cokernel M ** cokernel N"),
	       SEQ ("trace -- ", TO "trace"),
	       SEQ ("transpose -- ", TO "transpose"),
	       SEQ ("truncate -- ", TO "truncate"),
	       SEQ ("type -- ", TO "not documented yet"),
	       SEQ ("version -- ", TO "version"),
	       SEQ ("wedge -- ", TO "minors", ",", TO "exteriorPower")
	       }),
	  SEQ ("Macaulay scripts from 'scriptsde':",
	  UL {
	       SEQ ("<adj_of_cat -- ", TO "not documented yet"),
	       SEQ ("<adjoin_fractions -- ", TO "not documented yet"),
	       SEQ ("<adjoint -- ", TO "not documented yet"),
	       SEQ ("<analytic_spread -- ", TO "not documented yet"),
	       SEQ ("<annihilated -- ", TO "not documented yet"),
	       SEQ ("<annihilator -- ", TO "ann"),
	       SEQ ("<annihilator1 -- ", TO "ann"),
	       SEQ ("<annihilator2 -- ", TO "ann"),
	       SEQ ("<binomial -- ", TO "binomial"),
	       SEQ ("<ceiling -- ", TO "not documented yet"),
	       SEQ ("<changelog -- ", TO "not documented yet"),
	       SEQ ("<codim -- ", TO "not documented yet"),
	       SEQ ("<cohomology -- ", TO "not documented yet"),
	       SEQ ("<cohomology1 -- ", TO "not documented yet"),
	       SEQ ("<column_vector -- ", TO "not documented yet"),
	       SEQ ("<complement -- ", TO "not documented yet"),
	       SEQ ("<copyring -- ", TO "not documented yet"),
	       SEQ ("<cotan -- ", TO "not documented yet"),
	       SEQ ("<cotan_bihom -- ", TO "not documented yet"),
	       SEQ ("<curve_on_cubic -- ", TO "not documented yet"),
	       SEQ ("<diagonal_submodule -- ", TO "not documented yet"),
	       SEQ ("<diff -- ", TO "not documented yet"),
	       SEQ ("<double_dual -- ", TO "not documented yet"),
	       SEQ ("<double_dual1 -- ", TO "not documented yet"),
	       SEQ ("<dual_variety -- ", TO "not documented yet"),
	       SEQ ("<empty_array -- ", TO "not documented yet"),
	       SEQ ("<equality -- ", TO "not documented yet"),
	       SEQ ("<ext -- ", TO "not documented yet"),
	       SEQ ("<ext(-,r) -- ", TO "not documented yet"),
	       SEQ ("<extend_ring -- ", TO "not documented yet"),
	       SEQ ("<from_bigraded -- ", TO "not documented yet"),
	       SEQ ("<from_div_powers -- ", TO "not documented yet"),
	       SEQ ("<generic_mat -- ", TO "genericMatrix"),
	       SEQ ("<getvars -- ", TO "not documented yet"),
	       SEQ ("<hom -- ", TO "not documented yet"),
	       SEQ ("<hom_and_map -- ", TO "not documented yet"),
	       SEQ ("<hom_is_0 -- ", TO "not documented yet"),
	       SEQ ("<homology -- ", TO "not documented yet"),
	       SEQ ("<i_in_j -- ", TO "not documented yet"),
	       SEQ ("<ideal -- ", TO "not documented yet"),
	       SEQ ("<idempotent -- ", TO "not documented yet"),
	       SEQ ("<idencoldegs -- ", TO "not documented yet"),
	       SEQ ("<idenrowdegs -- ", TO "not documented yet"),
	       SEQ ("<interchange -- ", TO "not documented yet"),
	       SEQ ("<interchange_permutation -- ", TO "not documented yet"),
	       SEQ ("<inverse -- ", TO "not documented yet"),
	       SEQ ("<is_zero -- ", TO "not documented yet"),
	       SEQ ("<k3carpet -- ", TO "not documented yet"),
	       SEQ ("<kernel -- ", TO "not documented yet"),
	       SEQ ("<kernel_and_map -- ", TO "not documented yet"),
	       SEQ ("<kosz_hom1 -- ", TO "not documented yet"),
	       SEQ ("<kosz_hom2 -- ", TO "not documented yet"),
	       SEQ ("<l_i_in_j -- ", TO "not documented yet"),
	       SEQ ("<l_intersect -- ", TO "not documented yet"),
	       SEQ ("<l_res -- ", TO "not documented yet"),
	       SEQ ("<lex_seg_ideal -- ", TO "not documented yet"),
	       SEQ ("<macaulayrep -- ", TO "not documented yet"),
	       SEQ ("<map_from_col -- ", TO "not documented yet"),
	       SEQ ("<minpres -- ", TO "not documented yet"),
	       SEQ ("<module_iso -- ", TO "not documented yet"),
	       SEQ ("<monomial_curve -- ", TO "not documented yet"),
	       SEQ ("<mult_ideals -- ", TO "not documented yet"),
	       SEQ ("<nbyn_commuting -- ", TO "not documented yet"),
	       SEQ ("<normal_sheaf -- ", TO "not documented yet"),
	       SEQ ("<nres -- ", TO "not documented yet"),
	       SEQ ("<nzd -- ", TO "not documented yet"),
	       SEQ ("<orbit_equations -- ", TO "not documented yet"),
	       SEQ ("<permutation -- ", TO "not documented yet"),
	       SEQ ("<perp -- ", TO "not documented yet"),
	       SEQ ("<points -- ", TO "not documented yet"),
	       SEQ ("<powers -- ", TO "not documented yet"),
	       SEQ ("<project_from_product -- ", TO "not documented yet"),
	       SEQ ("<projective_plane -- ", TO "not documented yet"),
	       SEQ ("<prune -- ", TO "prune"),
	       SEQ ("<prune_and_map -- ", TO "not documented yet"),
	       SEQ ("<push_forward -- ", TO "pushForward"),
	       SEQ ("<push_forward1 -- ", TO "pushForward1"),
	       SEQ ("<quotient1 -- ", TO "quotient"),
	       SEQ ("<radical -- ", TO "radical"),
	       SEQ ("<random_element -- ", TO "not documented yet"),
	       SEQ ("<random_int -- ", TO "not documented yet"),
	       SEQ ("<random_map -- ", TO "not documented yet"),
	       SEQ ("<random_mat -- ", TO "not documented yet"),
	       SEQ ("<rank_prob -- ", TO "not documented yet"),
	       SEQ ("<rat_nor_curve -- ", TO "not documented yet"),
	       SEQ ("<rat_nor_osc_locus -- ", TO "not documented yet"),
	       SEQ ("<rational_surface -- ", TO "not documented yet"),
	       SEQ ("<rational_surface1 -- ", TO "not documented yet"),
	       SEQ ("<reduce_syzygy_1 -- ", TO "not documented yet"),
	       SEQ ("<regular_sequence -- ", TO "not documented yet"),
	       SEQ ("<regular_sequence1 -- ", TO "not documented yet"),
	       SEQ ("<regularity -- ", TO "regularity"),
	       SEQ ("<remove_low_dim -- ", TO "not documented yet"),
	       SEQ ("<remove_low_dim_id -- ", TO "not documented yet"),
	       SEQ ("<remove_lowest_dim -- ", TO "not documented yet"),
	       SEQ ("<representatives -- ", TO "not documented yet"),
	       SEQ ("<representatives_old -- ", TO "not documented yet"),
	       SEQ ("<res -- ", TO "resolution"),
	       SEQ ("<res_and_dim -- ", TO "resolution", ",", TO "pdim"),
	       SEQ ("<ring -- ", TO "not documented yet"),
	       SEQ ("<sagbi -- ", TO "not documented yet"),
	       SEQ ("<sagbi_step -- ", TO "not documented yet"),
	       SEQ ("<sat -- ", TO "saturate"),
	       SEQ ("<sat1 -- ", TO "saturate"),
	       SEQ ("<scroll -- ", TO "not documented yet"),
	       SEQ ("<select -- ", TO "not documented yet"),
	       SEQ ("<shout_list -- ", TO "not documented yet"),
	       SEQ ("<sort_by_degree -- ", TO "not documented yet"),
	       SEQ ("<stack -- ", TO "matrix", ",",TO "map"),
	       SEQ ("<submat_by_degs -- ", TO "not documented yet"),
	       SEQ ("<subring -- ", TO "kernel"),
	       SEQ ("<sym -- ", TO "not documented yet"),
	       SEQ ("<sym_cokernel -- ", TO "not documented yet"),
	       SEQ ("<template_for_scripts -- ", TO "not documented yet"),
	       SEQ ("<to_div_powers -- ", TO "not documented yet"),
	       SEQ ("<tor -- ", TO "Tor"),
	       SEQ ("<unmixed_radical -- ", TO "radical"),
	       SEQ ("<unmixed_radical1 -- ", TO "radical"),
	       SEQ ("<unmixed_radical2 -- ", TO "radical"),
	       SEQ ("<wedge_cokernel -- ", TO "not documented yet"),
	       SEQ ("<x_to_last -- ", TO "newCoordinateSystem"),
	       SEQ ("<zeromat -- ", TO "not documented yet")
	       }),
	  SEQ ("Macaulay scripts from 'scriptsmj':",
	  UL {
	       SEQ ("<2BYN -- ", TO "not documented yet"),
	       SEQ ("<add_matrix_to_array -- ", TO "not documented yet"),
	       SEQ ("<check_complex -- ", TO "not documented yet"),
	       SEQ ("<check_exact -- ", TO "not documented yet"),
	       SEQ ("<comp_to_array -- ", TO "not documented yet"),
	       SEQ ("<eagon_northcott -- ", TO "not documented yet"),
	       SEQ ("<extract_matrix -- ", TO "not documented yet"),
	       SEQ ("<homology_of_array -- ", TO "not documented yet"),
	       SEQ ("<jordan0 -- ", TO "not documented yet"),
	       SEQ ("<koszul_complex -- ", TO "not documented yet"),
	       SEQ ("<lift -- ", TO "not documented yet"),
	       SEQ ("<lift_arrays -- ", TO "not documented yet"),
	       SEQ ("<mc -- ", TO "not documented yet"),
	       SEQ ("<nilpotent0 -- ", TO "not documented yet"),
	       SEQ ("<pres -- ", TO "not documented yet"),
	       SEQ ("<smult_complex -- ", TO "not documented yet"),
	       SEQ ("<splice_resns -- ", TO "not documented yet")
	       }),
	  SEQ ("Macaulay scripts from 'scriptsms':",
	  UL {
	       SEQ ("<block_map -- ", TO "not documented yet"),
	       SEQ ("<blowup -- ", TO "not documented yet"),
	       SEQ ("<blowup0 -- ", TO "not documented yet"),
	       SEQ ("<generic_skew_mat -- ", TO "genericSkewMatrix"),
	       SEQ ("<generic_sym_mat -- ", TO "genericSymmetricMatrix"),
	       SEQ ("<hilb_fcn -- ", TO "not documented yet"),
	       SEQ ("<inhomog_std -- ", TO "not documented yet"),
	       SEQ ("<l_dual0 -- ", TO "not documented yet"),
	       SEQ ("<l_from_dual -- ", TO "fromDual"),
	       SEQ ("<l_homog0 -- ", TO "not documented yet"),
	       SEQ ("<l_min0 -- ", TO "not documented yet"),
	       SEQ ("<l_minimal -- ", TO "not documented yet"),
	       SEQ ("<l_minimal0 -- ", TO "not documented yet"),
	       SEQ ("<l_tangentcone -- ", TO "not documented yet"),
	       SEQ ("<l_to_dual -- ", TO "toDual"),
	       SEQ ("<line_bundle_image -- ", TO "not documented yet"),
	       SEQ ("<normal_cone -- ", TO "not documented yet"),
	       SEQ ("<proj_bundle -- ", TO "not documented yet"),
	       SEQ ("<rmap -- ", TO "not documented yet"),
	       SEQ ("<symmetric_algebra -- ", TO "not documented yet")
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
     "Compute the Groebner basis of ", 
     TT "m", " up to a given degree, using ", TT "gb(m, DegreeLimit=>3)", 
     ".  Similarly,
     if a computation in Macaulay2 has been halted by pressing <CTRL>-C, 
     the computation
     can be restarted from where it left off, by giving the original 
     command, e.g. ", 
     TT "gb m", ".",
     
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
     ", a Groebner basis is computed automatically, if needed.  Use ", 
     TT "codim M", ".",
     "One use of this command in Macaulay1 was to compute the codimension 
     of a partially
     computed Groebner basis.  See ", TO "computing with partial results", 
     " for examples of doing this.",

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
     "Using Macaulay2 in emacs, type the first few letters of a command,
     then hit <TAB>. The list of all commands starting with those letters
     will be displayed, or if there is only one, emacs will complete the typing
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
     Groebner basis and resolution commands, the computation starts off 
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
     ", a Groebner basis is computed automatically, if needed.  Use ", 
     TT "degree M", ".  Note that in Macaulay2, ", TT "degree", " returns
     only the degree.  Use ", TT "codim M", " to obtain the codimension.",
     "One use of this command in Macaulay1 was to compute the degree
     of a partially
     computed Groebner basis.  See ", TO "computing with partial results", 
     " for examples of doing this.",

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
     " form a Groebner basis, use ", TT "forceGB m", ".  See ", TO "forceGB",
     ".",
     
     HEADER3 "hilb <standard basis>",
     "WRITE THIS",
     
     HEADER3 "hilb_numer <standard basis> <ideal> <result>",
     "WRITE THIS",
     
     HEADER3 "homog <matrix> <homog variable> <new matrix>",
     "To homogenize a matrix ", TT "m", " with respect to a variable ",
     TT "x", ", use ", TT "homogenize(m,x)", ". One may also homogenize
     with respect to a given weight vector.  See ", TO "homogenize", ".",
     
     HEADER3 "hulb <standard basis> <deg>",
     "WRITE THIS",
     
     HEADER3 "ideal <resulting matrix>",
     "To enter a one row matrix, use e.g.",
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
     "WRITE MORE",
     
     HEADER3 "imap <new ring map> <R> <S> [ones, default=zeros]",
     "WRITE THIS",
     
     HEADER3 "in <standard basis> [optional result matrix] [n]",
     "WRITE THIS",
     
     HEADER3 "inpart <standard basis> <result matrix> [variable list]",
     "WRITE THIS",
     
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
     control structures.", "WRITE MORE",
     
     HEADER3 "k_basis <matrix> <result matrix> [variable list]",
     "WRITE THIS",
     
     HEADER3 "keep <standard basis> <result matrix> [n]",
     "WRITE THIS",
     
     HEADER3 "kill <var1> ... <var n>",
     "WRITE THIS",
     
     HEADER3 "koszul <int n, or matrix> <p> <result matrix>",
     "WRITE THIS",
     
     HEADER3 "lift <standard basis> <matrix to lift> <result>",
     "WRITE THIS",
     
     HEADER3 "lift_std <matrix> <computation>",
     "WRITE THIS",
     
     HEADER3 "listvars",
     "WRITE THIS",
     
     HEADER3 "mat <result matrix> [optional: file name]",
     "WRITE THIS",
     
     }

document {
     Key => "not documented yet", 
     "We haven't documented yet the way to replace this Macaulay function or
     script."
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
     "Note that no Groebner basis elements have been computed yet",
     EXAMPLE {
	  "gb(I,PairLimit=>2)",
      	  "m = gens G"},
     "To find the codimension of the monomial ideal of lead terms so far computed,
     we use:",
     EXAMPLE "codim cokernel leadTerm m"

	       SEQ (". -- not available"),
	       SEQ ("args -- ", TO "not documented yet"),
	       SEQ ("cache_mem -- ", TO "not documented yet"),
	       SEQ ("cdir -- ", TO "not documented yet"),
	       SEQ ("echo -- ", TO "not documented yet"),
	       SEQ ("endmon -- ", TO "not documented yet"),
	       SEQ ("incoef -- ", TO "not documented yet"),
	       SEQ ("incr_set -- ", TO "not documented yet"),

///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
