
document { "replacements for commands and scripts from Macaulay",
     "Macaulay 2 aims to provide all of the functionality of Macaulay, but
     the names of the functions are not the same, and there are
     other differences.  One major difference is the introduction of the
     notion of module, whereas in Macaulay, the pervasive concept was
     the matrix.",
     PARA,
     "Here is a list of old Macaulay functions, together with pointers to
     functions in Macaulay 2 that might be used to replace them.",
     PARA,
     MENU {
	  ("Macaulay commands:",
	  SHIELD MENU {
	       ("ac -- such destructive changes are not allowed"),
	       ("add -- ", TO "+"),
	       ("ar -- ", TO "not documented yet"),
	       ("betti -- ", TO "betti"),
	       ("calc -- ", TO "not documented yet"),
	       ("cat -- ", TO "not documented yet"),
	       ("ce -- ", TO "not documented yet"),
	       ("characteristic -- ", TO "char"),
	       ("chcalc -- ", TO "not documented yet"),
	       ("codim -- ", TO "codim"),
	       ("coef -- ", TO "not documented yet"),
	       ("col_degree -- ", TO "not documented yet"),
	       ("col_degs -- ", TO "not documented yet"),
	       ("commands -- ", TO "not documented yet"),
	       ("compress -- ", TO "compress"),
	       ("concat -- ", TO "|", ",", TO "||"),
	       ("continue -- ", TO "not documented yet"),
	       ("contract -- ", TO "contract"),
	       ("copy -- ", TO "not documented yet"),
	       ("degree -- ", TO "degree"),
	       ("determinants -- ", TO "minors"),
	       ("diag -- ", "The command ", TT "diag m n", " is equivalent to the Macaulay2
		    expression ", BR, TT "n = map((ring m)^(numgens source m), source m,(i,j) -> if i === j then m_(0,i) else 0)"),
	       ("diag -- ", TO "not documented yet"),
	       ("diff -- ", TO "diff"),
	       ("dshift -- ", TO "not documented yet"),
	       ("dsum -- ", TO "++"),
	       ("edit -- ", TO "not documented yet"),
	       ("edit_map -- ", TO "not documented yet"),
	       ("elim -- ", TO "selectInSubring"),
	       ("ev -- ", TO "substitute", ",", TO "RingMap"),
	       ("exit -- ", TO "quit"),
	       ("fetch -- ", TO "substitute"),
	       ("flatten -- ", TO "flatten"),
	       ("forcestd -- ", TO "forceGB"),
	       ("help -- ", TO "help"),
	       ("help_file -- ", TO "not documented yet"),
	       ("hilb -- ", TO "hilbertSeries", ",",TO "hilbertPolynomial",
		    ",",TO "hilbertFunction", ",",TO "poincare"),
	       ("hilb_numer -- ", TO "hilbertSeries"),
	       ("homog -- ", TO "homogenize"),
	       ("hulb -- ", TO "not documented yet"),
	       ("ideal -- ", TO "ideal", ", ", TO "matrix"),
	       ("iden -- ", TO "id"),
	       ("if -- ", TO "if"),
	       ("imap -- ", TO "map"),
	       ("in -- ", TO "leadTerm"),
	       ("inpart -- ", TO "not documented yet"),
	       ("int -- ", TO "not documented yet"),
	       ("intersect -- ", TO "intersect"),
	       ("is_zero -- ", TO "=="),
	       ("jacob -- ", TO "jacobian"),
	       ("jump -- ", TO "not documented yet"),
	       ("k-basis -- ", TO "basis"),
	       ("k_basis -- ", TO "basis"),
	       ("keep -- ", TO "not documented yet"),
	       ("kill -- ", TO "not documented yet"),
	       ("koszul -- ", TO "koszul"),
	       ("lift -- ", TO "//"),
	       ("lift_std -- ", TO "gb"),
	       ("listvars -- ", TO "listUserSymbols"),
	       ("mat -- ", TO "matrix"),
	       ("max -- ", TO "max"),
	       ("mc -- ", TO "not documented yet"),
	       ("min -- ", TO "min"),
	       ("modulo -- ", TO "modulo"),
	       ("monitor -- ", TO "emacs"),
	       ("monitoring -- ", TO "not documented yet"),
	       ("monoms -- ", TO "not documented yet"),
	       ("monprimes -- ", TO "not documented yet"),
	       ("mr -- ", TO "not documented yet"),
	       ("mult -- ", TO "*"),
	       ("ncols -- ", "use numgens source m"),
	       ("nres -- ", TO "resolution"),
	       ("nrows -- ", "use numgens target m"),
	       ("numinfo -- ", TO "not documented yet"),
	       ("nvars -- ", "use numgens R"),
	       ("outer -- ", TO "**"),
	       ("path -- ", TO "path"),
	       ("pc -- ", TO "not documented yet"),
	       ("pfaff -- ", TO "pfaffians"),
	       ("pmap -- ", TO "not documented yet"),
	       ("poly -- ", TO "not documented yet"),
	       ("power -- ", TO "^"),
	       ("pr -- ", TO "not documented yet"),
	       (BOLD "pres <C:complex> -- ", "Use ", TT "C.dd", " or ", TO "print C.dd"),
	       ("present_ring -- ", TO "not documented yet"),
	       ("pring -- ", TO "not documented yet"),
	       ("prmat -- ", TO "print", " or ", TO "toString"),
	       ("putchange -- ", TO "not documented yet"),
	       ("putmat -- ", TO "not documented yet"),
	       ("putstd -- ", TO "not documented yet"),
	       (BOLD "qring <I:ideal> <result A:ring> -- ", "In Macaulay2, if ", TT "I", " is 
		    an ideal, then ", TT "A = (ring I)/I", " is equivalent, except that in Macaulay2,
		    a Groebner basis of I is computed for you, if it is needed"),
	       ("quit -- ", TO "quit"),
	       ("quotient -- ", TO "not documented yet"),
	       ("random -- ", TO "not documented yet"),
	       ("reduce -- ", TO "not documented yet"),
	       ("res -- ", TO "not documented yet"),
	       ("reset -- ", TO "restart"),
	       ("ring -- ", TO "not documented yet"),
	       ("ring-from-rows -- ", TO "not documented yet"),
	       ("ring_from_cols -- ", TO "not documented yet"),
	       ("ring_from_rows -- ", TO "not documented yet"),
	       ("ring_sum -- ", TO "not documented yet"),
	       ("rmap -- ", TO "not documented yet"),
	       ("row_degree -- ", TO "not documented yet"),
	       ("row_degs -- ", TO "not documented yet"),
	       ("sat -- ", TO "saturate"),
	       ("set -- ", TO "not documented yet"),
	       ("set_value -- ", TO "not documented yet"),
	       ("setcoldegs -- ", TO "not documented yet"),
	       ("setdegs -- ", TO "not documented yet"),
	       ("setring -- ", TO "use"),
	       ("shout -- ", TO "not documented yet"),
	       ("size -- ", TO "not documented yet"),
	       ("smult -- ", TO "*"),
	       ("space -- ", TO "engineMemory"),
	       ("spairs -- ", TO "not documented yet"),
	       ("spare -- ", TO "not documented yet"),
	       ("sparse -- ", TO "not documented yet"),
	       ("std -- ", TO "not documented yet"),
	       ("std_minimal -- ", TO "not documented yet"),
	       ("stdpart -- ", TO "not documented yet"),
	       ("submat -- ", TO "submatrix"),
	       ("subtract -- ", TO "-"),
	       ("syz -- ", TO "syz", ",", TO "kernel"),
	       ("tensor <matrix M> <matrix N> <result matrix M.N> -- ", 
		    TT "cokernel M ** cokernel N"),
	       ("trace -- ", TO "trace"),
	       ("transpose -- ", TO "transpose"),
	       ("truncate -- ", TO "truncate"),
	       ("type -- ", TO "not documented yet"),
	       ("version -- ", TO "version"),
	       ("wedge -- ", TO "minors", ",", TO "exteriorPower")
	       }),
	  ("Macaulay scripts from 'scriptsde':",
	  SHIELD MENU {
	       ("<adj_of_cat -- ", TO "not documented yet"),
	       ("<adjoin_fractions -- ", TO "not documented yet"),
	       ("<adjoint -- ", TO "not documented yet"),
	       ("<analytic_spread -- ", TO "not documented yet"),
	       ("<annihilated -- ", TO "not documented yet"),
	       ("<annihilator -- ", TO "ann"),
	       ("<annihilator1 -- ", TO "ann"),
	       ("<annihilator2 -- ", TO "ann"),
	       ("<binomial -- ", TO "binomial"),
	       ("<ceiling -- ", TO "not documented yet"),
	       ("<changelog -- ", TO "not documented yet"),
	       ("<codim -- ", TO "not documented yet"),
	       ("<cohomology -- ", TO "not documented yet"),
	       ("<cohomology1 -- ", TO "not documented yet"),
	       ("<column_vector -- ", TO "not documented yet"),
	       ("<complement -- ", TO "not documented yet"),
	       ("<copyring -- ", TO "not documented yet"),
	       ("<cotan -- ", TO "not documented yet"),
	       ("<cotan_bihom -- ", TO "not documented yet"),
	       ("<curve_on_cubic -- ", TO "not documented yet"),
	       ("<diagonal_submodule -- ", TO "not documented yet"),
	       ("<diff -- ", TO "not documented yet"),
	       ("<double_dual -- ", TO "not documented yet"),
	       ("<double_dual1 -- ", TO "not documented yet"),
	       ("<dual_variety -- ", TO "not documented yet"),
	       ("<empty_array -- ", TO "not documented yet"),
	       ("<equality -- ", TO "not documented yet"),
	       ("<ext -- ", TO "not documented yet"),
	       ("<ext(-,r) -- ", TO "not documented yet"),
	       ("<extend_ring -- ", TO "not documented yet"),
	       ("<from_bigraded -- ", TO "not documented yet"),
	       ("<from_div_powers -- ", TO "not documented yet"),
	       ("<generic_mat -- ", TO "genericMatrix"),
	       ("<getvars -- ", TO "not documented yet"),
	       ("<hom -- ", TO "not documented yet"),
	       ("<hom_and_map -- ", TO "not documented yet"),
	       ("<hom_is_0 -- ", TO "not documented yet"),
	       ("<homology -- ", TO "not documented yet"),
	       ("<i_in_j -- ", TO "not documented yet"),
	       ("<ideal -- ", TO "not documented yet"),
	       ("<idempotent -- ", TO "not documented yet"),
	       ("<idencoldegs -- ", TO "not documented yet"),
	       ("<idenrowdegs -- ", TO "not documented yet"),
	       ("<interchange -- ", TO "not documented yet"),
	       ("<interchange_permutation -- ", TO "not documented yet"),
	       ("<inverse -- ", TO "not documented yet"),
	       ("<is_zero -- ", TO "not documented yet"),
	       ("<k3carpet -- ", TO "not documented yet"),
	       ("<kernel -- ", TO "not documented yet"),
	       ("<kernel_and_map -- ", TO "not documented yet"),
	       ("<kosz_hom1 -- ", TO "not documented yet"),
	       ("<kosz_hom2 -- ", TO "not documented yet"),
	       ("<l_i_in_j -- ", TO "not documented yet"),
	       ("<l_intersect -- ", TO "not documented yet"),
	       ("<l_res -- ", TO "not documented yet"),
	       ("<lex_seg_ideal -- ", TO "not documented yet"),
	       ("<macaulayrep -- ", TO "not documented yet"),
	       ("<map_from_col -- ", TO "not documented yet"),
	       ("<minpres -- ", TO "not documented yet"),
	       ("<module_iso -- ", TO "not documented yet"),
	       ("<monomial_curve -- ", TO "not documented yet"),
	       ("<mult_ideals -- ", TO "not documented yet"),
	       ("<nbyn_commuting -- ", TO "not documented yet"),
	       ("<normal_sheaf -- ", TO "not documented yet"),
	       ("<nres -- ", TO "not documented yet"),
	       ("<nzd -- ", TO "not documented yet"),
	       ("<orbit_equations -- ", TO "not documented yet"),
	       ("<permutation -- ", TO "not documented yet"),
	       ("<perp -- ", TO "not documented yet"),
	       ("<points -- ", TO "not documented yet"),
	       ("<powers -- ", TO "not documented yet"),
	       ("<project_from_product -- ", TO "not documented yet"),
	       ("<projective_plane -- ", TO "not documented yet"),
	       ("<prune -- ", TO "prune"),
	       ("<prune_and_map -- ", TO "not documented yet"),
	       ("<push_forward -- ", TO "pushForward"),
	       ("<push_forward1 -- ", TO "pushForward1"),
	       ("<quotient1 -- ", TO "quotient"),
	       ("<radical -- ", TO "radical"),
	       ("<random_element -- ", TO "not documented yet"),
	       ("<random_int -- ", TO "not documented yet"),
	       ("<random_map -- ", TO "not documented yet"),
	       ("<random_mat -- ", TO "not documented yet"),
	       ("<rank_prob -- ", TO "not documented yet"),
	       ("<rat_nor_curve -- ", TO "not documented yet"),
	       ("<rat_nor_osc_locus -- ", TO "not documented yet"),
	       ("<rational_surface -- ", TO "not documented yet"),
	       ("<rational_surface1 -- ", TO "not documented yet"),
	       ("<reduce_syzygy_1 -- ", TO "not documented yet"),
	       ("<regular_sequence -- ", TO "not documented yet"),
	       ("<regular_sequence1 -- ", TO "not documented yet"),
	       ("<regularity -- ", TO "regularity"),
	       ("<remove_low_dim -- ", TO "not documented yet"),
	       ("<remove_low_dim_id -- ", TO "not documented yet"),
	       ("<remove_lowest_dim -- ", TO "not documented yet"),
	       ("<representatives -- ", TO "not documented yet"),
	       ("<representatives_old -- ", TO "not documented yet"),
	       ("<res -- ", TO "resolution"),
	       ("<res_and_dim -- ", TO "resolution", ",", TO "pdim"),
	       ("<ring -- ", TO "not documented yet"),
	       ("<sagbi -- ", TO "not documented yet"),
	       ("<sagbi_step -- ", TO "not documented yet"),
	       ("<sat -- ", TO "saturate"),
	       ("<sat1 -- ", TO "saturate"),
	       ("<scroll -- ", TO "not documented yet"),
	       ("<select -- ", TO "not documented yet"),
	       ("<shout_list -- ", TO "not documented yet"),
	       ("<sort_by_degree -- ", TO "not documented yet"),
	       ("<stack -- ", TO "matrix", ",",TO "map"),
	       ("<submat_by_degs -- ", TO "not documented yet"),
	       ("<subring -- ", TO "kernel"),
	       ("<sym -- ", TO "not documented yet"),
	       ("<sym_cokernel -- ", TO "not documented yet"),
	       ("<template_for_scripts -- ", TO "not documented yet"),
	       ("<to_div_powers -- ", TO "not documented yet"),
	       ("<tor -- ", TO "Tor"),
	       ("<unmixed_radical -- ", TO "radical"),
	       ("<unmixed_radical1 -- ", TO "radical"),
	       ("<unmixed_radical2 -- ", TO "radical"),
	       ("<wedge_cokernel -- ", TO "not documented yet"),
	       ("<x_to_last -- ", TO "newCoordinateSystem"),
	       ("<zeromat -- ", TO "not documented yet")
	       }),
	  ("Macaulay scripts from 'scriptsmj':",
	  SHIELD MENU {
	       ("<2BYN -- ", TO "not documented yet"),
	       ("<add_matrix_to_array -- ", TO "not documented yet"),
	       ("<check_complex -- ", TO "not documented yet"),
	       ("<check_exact -- ", TO "not documented yet"),
	       ("<comp_to_array -- ", TO "not documented yet"),
	       ("<eagon_northcott -- ", TO "not documented yet"),
	       ("<extract_matrix -- ", TO "not documented yet"),
	       ("<homology_of_array -- ", TO "not documented yet"),
	       ("<jordan0 -- ", TO "not documented yet"),
	       ("<koszul_complex -- ", TO "not documented yet"),
	       ("<lift -- ", TO "not documented yet"),
	       ("<lift_arrays -- ", TO "not documented yet"),
	       ("<mc -- ", TO "not documented yet"),
	       ("<nilpotent0 -- ", TO "not documented yet"),
	       ("<pres -- ", TO "not documented yet"),
	       ("<smult_complex -- ", TO "not documented yet"),
	       ("<splice_resns -- ", TO "not documented yet")
	       }),
	  ("Macaulay scripts from 'scriptsms':",
	  SHIELD MENU {
	       ("<block_map -- ", TO "not documented yet"),
	       ("<blowup -- ", TO "not documented yet"),
	       ("<blowup0 -- ", TO "not documented yet"),
	       ("<generic_skew_mat -- ", TO "genericSkewMatrix"),
	       ("<generic_sym_mat -- ", TO "genericSymmetricMatrix"),
	       ("<hilb_fcn -- ", TO "not documented yet"),
	       ("<inhomog_std -- ", TO "not documented yet"),
	       ("<l_dual0 -- ", TO "not documented yet"),
	       ("<l_from_dual -- ", TO "fromDual"),
	       ("<l_homog0 -- ", TO "not documented yet"),
	       ("<l_min0 -- ", TO "not documented yet"),
	       ("<l_minimal -- ", TO "not documented yet"),
	       ("<l_minimal0 -- ", TO "not documented yet"),
	       ("<l_tangentcone -- ", TO "not documented yet"),
	       ("<l_to_dual -- ", TO "toDual"),
	       ("<line_bundle_image -- ", TO "not documented yet"),
	       ("<normal_cone -- ", TO "not documented yet"),
	       ("<proj_bundle -- ", TO "not documented yet"),
	       ("<rmap -- ", TO "not documented yet"),
	       ("<symmetric_algebra -- ", TO "not documented yet")
	       })
	  },
     H3 "add <matrix> <matrix> <matrix>",
     "Add matrices ", TT "m", " and ", TT "n", ", using ",
     TT "result = m + n", ".  In general, arithmetic in Macaulay2 
     is done using natural mathematical expressions.  See ", TO "+", ".",

     H3 "betti <res>",
     "Display the graded betti numbers of a chaincomplex or resolution ", 
     TT "C",
     " using ",  TT "betti C"     , ".  See ", TO "betti", ".",
     
     H3 "calc <computation> [hi degree]",
     H3 "chcalc <computation> [new hi deg]",
     "Compute the Groebner basis of ", 
     TT "m", " up to a given degree, using ", TT "gb(m, DegreeLimit=>3)", 
     ".  Similarly,
     if a computation in Macaulay2 has been halted by pressing <CTRL>-C, 
     the computation
     can be restarted from where it left off, by giving the original 
     command, e.g. ", 
     TT "gb m", ".",
     
     H3 "cat <first var(0..numvars-1)> <result matrix>",
     "Macaulay1 then also prompts for two lists of integers, say ", 
     TT "rows", 
     " and ", TT "cols", ".  The resulting catalecticant matrix can 
     be obtained in
     Macaulay2 in the following way.",
     EXAMPLE "R = QQ[a..h]",
     EXAMPLE "rows = {0,1,2}",
     EXAMPLE "cols = {0,3}",
     EXAMPLE "result = map(R^3, 2, (i,j) -> R_(rows_i + cols_j))",
     --SEEALSO (genericMatrix, genericSymmetricMatrix, genericSkewMatrix),
     
     H3 "characteristic <ring> [result integer]",
     "To obtain the characteristic of a ring ", TT "R", ", use ",
     TT "char R", ".  See ", TO "char", ".",
     
     H3 "codim <standard basis> [integer result]",
     "When computing the codimension of an ideal or module ", TT "M", 
     ", a Groebner basis is computed automatically, if needed.  Use ", 
     TT "codim M", ".",
     "One use of this command in Macaulay1 was to compute the codimension 
     of a partially
     computed Groebner basis.  See ", TO "computing with partial results", 
     " for examples of doing this.",

     H3 "coef <matrix> <result monoms> <result coefs> [variable list]",
     "To obtain the matrices of monomials and their coefficients, use ", 
     TO "coefficients", ".  The following example obtains the coefficients 
     of the variable ", TT "a", " (which is variable number 0)",
     EXAMPLE "R = ZZ/101[a..d];",
     EXAMPLE "m = matrix{{a^2+a^2*c+a*b+3*d}}",
     EXAMPLE "result = coefficients({0}, m)",
     EXAMPLE "result_0",
     EXAMPLE "result_1",
     
     H3 "col_degree <matrix> <column> [result integer]",
     "To obtain the degree of the ", TT "i", "th column of a matrix ", 
     TT "m", ", use ", TT "(degrees source m)_i", ".  See ", TO "degrees",
     ".  Note that in Macaulay2, one can use multi-degrees, so the result
     is a list of integers.  Also all indices in Macaulay2 start at 0.",
     EXAMPLE "R = QQ[a,b,Degrees=>{{1,0},{1,-1}}];",
     EXAMPLE "m = matrix{{a*b, b^2}}",
     EXAMPLE "(degrees source m)_0",
     
     H3 "col_degs <matrix> [column degrees]",
     "Compute the list of degrees of the columns of a matrix ", TT "m",
     " using ", TT "degrees source m", ".  The result is a list of degrees.
     Each degree is itself a list of integers, since multi-degrees are 
     allowed.",
     
     H3 "commands",
     "Using Macaulay2 in emacs, type the first few letters of a command,
     then hit <TAB>. The list of all commands starting with those letters
     will be displayed, or if there is only one, emacs will complete the typing
     for you.",

     H3 "compress <matrix> <result matrix>",
     "Remove columns of a matrix ", TT "m", " which are entirely zero
     using ", TT "compress m", ". See ", TO "compress", ".",
     
     H3 "concat <matrix> <matrix2> ... <matrix n>",
     "Concatenate matrices ", TT "m1", ", ", TT "m2", ", ..., ", TT "mn",
     " using ", TT "m1 | m2 | ... | mn", ".  See ", TO "|", 
     ".  The main difference is that
     in Macaulay2 this command does not overwrite the first matrix.",
     
     H3 "continue",
     "In Macaulay2, one interrupts a computation using <CTRL>-C, as in 
     Macaulay1.  Restart the computation using the original command.  For 
     Groebner basis and resolution commands, the computation starts off 
     where it left off.",
     
     H3 "contract <ideal> <ideal> <result matrix>",
     "To contract the matrix ", TT "n", " by the matrix ", TT "m", " use ",
     TT "contract(m,n)", ".  See ", TO "contract", ".  In Macaulay2, the
     arguments are not constrained to be matrices with one row.",
     
     H3 "copy <existing matrix> <copy of matrix>",
     "Since matrices in Macaulay2 are immutable objects, this command is
     no longer necessary.  One can still make a copy of a matrix.  For
     example",
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "m = matrix{{a,b},{c,d}}",
     EXAMPLE "copym = map(target m, source m, entries m)",
     
     H3 "degree <standard basis> [integer codim] [integer degree]",
     "When computing the degree of an ideal or module ", TT "M", 
     ", a Groebner basis is computed automatically, if needed.  Use ", 
     TT "degree M", ".  Note that in Macaulay2, ", TT "degree", " returns
     only the degree.  Use ", TT "codim M", " to obtain the codimension.",
     "One use of this command in Macaulay1 was to compute the degree
     of a partially
     computed Groebner basis.  See ", TO "computing with partial results", 
     " for examples of doing this.",

     H3 "determinants <matrix> <p> <result>",
     "Use ", TT "minors(p,m)", " to compute the ideal of p by p minors
     of the matrix ", TT "m", ".  See ", TO "minors", ".",
     
     H3 "diag <matrix> <result>",
     "To make a diagonal matrix whose diagonal entries are taken from
     a matrix ", TT "m", ", it is necessary to build the matrix directly, 
     as in the following example.",
     EXAMPLE "R = ZZ[a..d];",
     EXAMPLE "m = matrix{{a^2,b^3,c^4,d^5}}",
     EXAMPLE "map(R^(numgens source m), source m, 
                 (i,j) -> if i === j then m_(0,i) else 0)",
     
     H3 "diff <ideal> <ideal> <result matrix>",
     "To differentiate the matrix ", TT "n", " by the matrix ", TT "m", 
     " use ",
     TT "diff(m,n)", ".  See ", TO "diff", ".  In Macaulay2, the
     arguments are not constrained to be matrices with one row.",
     
     H3 "dshift <matrix> <degree to shift by>",
     "To shift the degrees of a matrix ", TT "m", " by an integer ", TT "d",
     ", use ", TT "m ** (ring m)^{-d}", ".  See ", TO "**", ".  Note that this
     returns a matrix with the degrees shifted, and does not modify the 
     original matrix ", TT "m", ".  For example",
     EXAMPLE "R = ZZ[a..d];",
     EXAMPLE "m = matrix{{a,b^2},{c^2,d^3}}",
     EXAMPLE "betti m",
     EXAMPLE "n = m ** R^{-1}",
     EXAMPLE "betti n",
     
     H3 "dsum <matrix> ... <matrix> <result>",
     "To form the direct sum of matrices ", TT "m1, m2, ..., mn", ", use ",
     TT "m1 ++ m2 ++ ... ++ mn", ".  See ", TO "++", ".",
     
     H3 "elim <standard basis> <result matrix> [n]",
     "To select the columns of the matrix ", TT "m", " which lie in the
     subring defined by the first ", TT "n", " slots of the monomial order
     being zero, use ", TT "selectInSubring(n,m)", ".  Usually, one uses the
     value of 1 for ", TT "n", ".",
     
     H3 "ev <ideal> <matrix> <changed matrix>",
     "In Macaulay2, one uses ring maps, or ", TO "substitute", " to
     substitute variables.  If ", TT "m", " is a matrix in a ring ", TT "R",
     " having ", TT "n", " variables, and ", TT "f", " is a 1 by n matrix
     over some ring, then use ", TT "substitute(m,f)", " to perform the
     substitution.  For example,",
     EXAMPLE "R = QQ[a..d]",
     EXAMPLE "S = QQ[s,t]",
     EXAMPLE "m = matrix{{a^2-d, b*c}}",
     EXAMPLE "f = matrix{{s^4,s^3*t,s*t^3,t^4}}",
     EXAMPLE "substitute(m,f)",
     "In Macaulay2, one may also create and apply ring maps",
     EXAMPLE "F = map(R,R,{b,c,d,a})",
     EXAMPLE "m + F m + F F m + F F F m",
     "Or one may substitute for only some variables",
     EXAMPLE "substitute(m, {a=>1, b=>3})",
     
     H3 "exit",
     "Use ", TO "exit", " to quit Macaulay2.",
     
     H3 "fetch <matrix> <result matrix> [ones, default=zeros]",
     "In order to bring a matrix ", TT "m", " to a ring ", TT "S", " which
     has ring variables of the same name, use ", TT "substitute(m,S)", ".",
     EXAMPLE "R = ZZ[s,t]",
     EXAMPLE "m = s^2+t^2",
     EXAMPLE "S = R[a..d]",
     EXAMPLE "substitute(m,S)",
     
     H3 "flatten <matrix> <result ideal of entries>",
     "In order to form a one row matrix with the entries of a matrix ", 
     TT "m", ", use ", TT "flatten m", ".  See ", TO "flatten", ".",

     H3 "forcestd <matrix> <result standard basis> [change of basis]",
     "In order to inform the system that the columns of the matrix ", TT "m",
     " form a Groebner basis, use ", TT "forceGB m", ".  See ", TO "forceGB",
     ".",
     
     H3 "hilb <standard basis>",
     "WRITE THIS",
     
     H3 "hilb_numer <standard basis> <ideal> <result>",
     "WRITE THIS",
     
     H3 "homog <matrix> <homog variable> <new matrix>",
     "To homogenize a matrix ", TT "m", " with respect to a variable ",
     TT "x", ", use ", TT "homogenize(m,x)", ". One may also homogenize
     with respect to a given weight vector.  See ", TO "homogenize", ".",
     
     H3 "hulb <standard basis> <deg>",
     "WRITE THIS",
     
     H3 "ideal <resulting matrix>",
     "To enter a one row matrix, use e.g.",
     EXAMPLE "R = ZZ[a..d]",
     EXAMPLE "f = matrix{{a^2-b*c,3*b*c^4-1}}",
     "Remember that ideals, modules, and matrices are all different in
     Macaulay2.  One can easily change between them, as in:",
     EXAMPLE "J = ideal f",
     EXAMPLE "generators J",
     EXAMPLE "image f",
     EXAMPLE "cokernel f",
     
     H3 "iden <size> <result>",
     "To make the identity map on a module ", TT "F", ", use ", 
     TT "id_F", ", as in ",
     EXAMPLE "id_(R^4)",
     "See ", TO "id", ".",
     
     H3 "if <integer> <label1> [<label2>]",
     "For conditional execution, use the if-then or if-then-else statement",
     "WRITE MORE",
     
     H3 "imap <new ring map> <R> <S> [ones, default=zeros]",
     "WRITE THIS",
     
     H3 "in <standard basis> [optional result matrix] [n]",
     "WRITE THIS",
     
     H3 "inpart <standard basis> <result matrix> [variable list]",
     "WRITE THIS",
     
     H3 "int <name> <new value>",
     "To assign a value to a variable in Macaulay2, use ", TO "=", 
     ".  For example,",
     EXAMPLE "myanswer = 2*(numgens R) - 1",
     "Warning: In Macaulay1, names of variables in rings, and user defined 
     variables are completely separate.  In Macaulay2, if you assign something
     to a ring variable, it will assume its new value. ",
     EXAMPLE "R = ZZ/31991[a..d]",
     EXAMPLE "a",
     EXAMPLE "a = 43",
     EXAMPLE "a",
     EXAMPLE "use R",
     EXAMPLE "a",
     
     H3 "intersect <mat 1> ... <mat n> <result computation>",
     "To intersect ideals, or submodules ", TT "I1, I2, ..., In",
     ", use ", TT "intersect(I1,I2,...,In)", ".  The main difference
     is that these values cannot be matrices, they must be ideals or 
     submodules.  A second difference is that the computation, if interrupted,
     must be restarted at the beginning.  See ", TO "intersect", 
     ".  For example,",
     EXAMPLE "I = ideal(a^2-b,c-1,d^2-a*b)",
     EXAMPLE "J = ideal(a*b-1, c*d-2)",
     EXAMPLE "intersect(I,J)",

     H3 "is_zero <poly> <result integer: 1 if zero, else 0>",
     "To decide whether a polynomial, matrix, ideal, etc., ", TT "f", 
     " is zero, use ", TT "f == 0", ".  The resulting value is a Boolean: 
     either ", TT "true", ", or ", TT "false", ".  See ", TO "==", ".",
     
     H3 "jacob <ideal> <resulting jacobian> [variable list]",
     "To find the Jacobian matrix of an ideal or one row matrix", 
     TT "f", ", use ", TT "jacobian f", ".  If you only wish to differentiate
     with some of the variables, use ", TO "diff", " instead.  See ",
     TO "jacobian", ".",
     
     H3 "jump <label>",
     "Macaulay2 has no go to statements.  Instead, you should use the
     control structures.", "WRITE MORE",
     
     H3 "k_basis <matrix> <result matrix> [variable list]",
     "WRITE THIS",
     
     H3 "keep <standard basis> <result matrix> [n]",
     "WRITE THIS",
     
     H3 "kill <var1> ... <var n>",
     "WRITE THIS",
     
     H3 "koszul <int n, or matrix> <p> <result matrix>",
     "WRITE THIS",
     
     H3 "lift <standard basis> <matrix to lift> <result>",
     "WRITE THIS",
     
     H3 "lift_std <matrix> <computation>",
     "WRITE THIS",
     
     H3 "listvars",
     "WRITE THIS",
     
     H3 "mat <result matrix> [optional: file name]",
     "WRITE THIS",
     
     }

document { "not documented yet", 
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

     EXAMPLE "R = QQ[a..h]",
     EXAMPLE "I = ideal(a,b^5,c^7-d^7-a^7)",
     EXAMPLE "G = gb(I,StopBeforeComputation=>true)",
     EXAMPLE "gens G",
     "Note that no Groebner basis elements have been computed yet",
     EXAMPLE "gb(I,PairLimit=>2)",
     EXAMPLE "m = gens G",
     "To find the codimension of the monomial ideal of lead terms so far computed,
     we use:",
     EXAMPLE "codim cokernel leadTerm m"

	       (". -- not available"),
	       ("args -- ", TO "not documented yet"),
	       ("cache_mem -- ", TO "not documented yet"),
	       ("cdir -- ", TO "not documented yet"),
	       ("echo -- ", TO "not documented yet"),
	       ("endmon -- ", TO "not documented yet"),
	       ("incoef -- ", TO "not documented yet"),
	       ("incr_set -- ", TO "not documented yet"),

///
