-* Copyright 1995 by Daniel R. Grayson *-
-* Copyright 2020 by Mahrud Sayrafi    *-

-- get the raw documentation
pkg = loadPackage("Macaulay2Doc", Reload => true, LoadDocumentation => true)
load(currentFileDirectory | "booktex.m2")

-- TODO: change this in CMakeLists.txt for now
bookname := pkg#"pkgname" | "-" | pkg.Options.Version | "-manual"
banner := "Auto-generated for Macaulay2-%M2VERSION%. Do not modify this file manually."

packageBook = (bookFile, template) -> (
    output := concatenate("%% ", banner, newline, newline,     template);
    output = replace("%M2VERSION%",      version#"VERSION",      output);
    output = replace("%M2COMPILETIME%",  version#"compile time", output);
    (preamble, biblio) := toSequence separate("%M2BOOKCONTENT%", output);
    bookFile << preamble;
    scan(pairs getNameFromNumber, (i, node) -> (
	    bookFile                                                            << endl << endl
	    << "\\hypertarget{" << (n := sectionNumberTable#i) << "}{}"                 << endl
	    << sectionType n << "{" << tex format node << "}" << "\\label{" << n << "}" << endl
	    << concatenate booktex documentationMemo node                               << endl;
	    ));
    bookFile << biblio << close)

-----------------------------------------------------------------------------

generateBook := (bookFile, bookFunction) -> (
    template := currentFileDirectory | bookFile | ".in";
    if fileExists template then (
        stdio << "-- Generating " << bookFile << endl;
        directory := replace("/[^/].*$", "", bookFile);
        if directory =!= bookFile and not isDirectory directory then makeDirectory directory;
        bookFunction(bookFile, get(template)))
    else stderr << "Skipping generation of " << bookFile << " as it does not exist." << endl)

generateBook("M2book.tex", packageBook)
