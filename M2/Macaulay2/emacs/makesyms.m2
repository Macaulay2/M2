upper := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
f := openOut "M2-symbols.el"
f << "(setq M2-symbols '(" << endl
scan(
     sort select (keys symbolTable(), n -> #n > 1 and upper#?(n#0) ),
     s -> f << "    " << format s << endl)
f << "  ))" << endl << close
