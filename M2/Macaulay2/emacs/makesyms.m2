upper := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
f := openOut "M2-symbols.el"

f << "(defvar M2-symbols '(" << endl
scan(
     sort select (keys symbolTable(), n -> #n > 1 and upper#?(n#0) ),
     s -> f << "    " << format s << endl)
f << "   )" << endl
f << "  " << format ///A list of the symbols available in Macaulay 2, for use with dynamic completion./// << endl
f << "  )" << endl

f << ///
(defvar M2-mode-font-lock-keywords '(
///


add := (face,words) -> (
     f
     << "    (" 
     << format concatenate(///\<\(///, between(///\|///, words), ///\)\>///) 
     <<  " . " << face << ")" << endl
     )

add( "font-lock-keyword-face",
     { "if", "do", "else", "then", "or", "and", "not", "try", "new", "while", "list",
	  "for", "from", "to", "when", "symbol", "global", "local", "of", "from" })

add( "font-lock-type-face",
     first \ select(pairs symbolTable(), (str,sym) -> instance(value sym,Type)))

 -- add( "font-lock-type-face",
 --      { 
 -- 	"AffineVariety", "Array", "BasicList", "Boolean", "CC",
 -- 	"ChainComplex", "ChainComplexMap", "CoherentSheaf", "Database",
 -- 	"Field", "File", "FractionField", "Function", "GaloisField",
 -- 	"GradedModule", "GradedModuleMap", "GroebnerBasis", "HashTable",
 -- 	"Ideal", "List", "Matrix", "Module", "ModuleMap", "Monoid",
 -- 	"MonomialIdeal", "MutableHashTable", "MutableList", "Net", "Option",
 -- 	"OrderedMonoid", "PolynomialRing", "ProjectiveVariety", "QQ",
 -- 	"QuotientRing", "RR", "Ring", "RingMap", "SchurRing", "Sequence",
 -- 	"Set", "String", "Symbol", "Tally", "Thing", "Type", "Variety",
 -- 	"Vector", "VisibleList"
 -- 	}
 --    )

add( "font-lock-function-name-face",
     first \ select(pairs symbolTable(), (str,sym) -> instance(value sym,Function)))

alphabet := new MutableHashTable
scan( characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", c -> alphabet#c = true)

add( "font-lock-constant-face",
     first \ select(pairs symbolTable(), (str,sym) -> (
	       not instance(value sym,Function)
	       and 
	       not instance(value sym,Type)
	       and
	       not mutable sym
	       and
	       alphabet#?((toString sym)#0)
	       )
	  ))

add( "font-lock-builtin-face", { "shield", "timing", "time" })

f << "    (" << format "///\\([^/]\\|//?[^/]\\)*/?/?/?" << " . (0 font-lock-string-face t))" << endl
f << "   )" << endl
f << "  )" << endl

f << "(font-lock-add-keywords 'M2-mode M2-mode-font-lock-keywords)" << endl

f << "(provide 'M2-symbols)" << endl

f << close
