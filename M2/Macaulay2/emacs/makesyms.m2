upper := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
f := openOut "M2-symbols.el"
f << "(defvar M2-symbols '(" << endl
scan(
     sort select (keys symbolTable(), n -> #n > 1 and upper#?(n#0) ),
     s -> f << "    " << format s << endl)
f << "  )" << endl
f << "  \"A list of the symbols available in Macaulay 2, for use with" << endl
f << "  dynamic completion.\")" << endl
close f
