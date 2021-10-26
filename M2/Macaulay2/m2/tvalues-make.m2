-- This file makes the file "tvalues.m2" from the files ../d/*.d
-- The file "tvalues.m2" should be distributed with binary distributions

-- egrep -e "-- . typical value: " $^ | sed 's/.*-- . typical value: \(.*\)/typval(\1)/' >$@

make = (srcdir) -> (
     outfile = openOut "tvalues.m2";
     srcdir = minimizeFilename srcdir;
     scan(sort select(readDirectory srcdir, dir -> match("\\.dd?$",dir)), file -> (
	       file = srcdir | file;
	       txt := get file;
	       if match("-- . typical value: ",txt) then (
		    txt = select(lines txt, s -> match("-- . typical value: ",s));
		    txt = apply(txt, s -> replace(///.*-- . typical value: [[:space:]]*(.*[^[:space:]])[[:space:]]*///, ///typval(\1)///, s));
		    outfile << "-- typical values extracted from source file " << file << endl << stack txt << endl;
		    )));
     close outfile;
     )

 -- Local Variables:
 -- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 tvalues.m2 "
 -- End:
