n := lineNumber()-1
F := openOut "test.oldvalues"
F << "{" << endl
f := s -> F << format s << " => " << format value s << "," << endl
f ///version#"VERSION"///
f ///version#"compile time"///
f ///get "!date"///
f ///get "!uname -a"///
scan(1 .. n, i -> (
	  osym := value concatenate("symbol o",toString i);
	  oval := value osym;
	  f concatenate("toString net ",osym);
	  f concatenate("toString ",osym);
	  f concatenate("toExternalString ",osym);
	  ))
F << "}" << endl
close F
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/ComputationsBook capture"
-- End:
