-- https://github.com/Macaulay2/M2/wiki/Internals:-Release-Checklist

pkglist = sort separate_" " version#"packages";
pkgopts = hashTable apply(pkglist, pkg -> (pkg, readPackage pkg));

pkgs = select(pkgopts, opts -> isMember("Uncategorized", opts.Keywords))
if #pkgs > 0 then error("packages without a Keyword: ", demark_", " keys pkgs)

pkgs = select(pkgopts, opts -> opts.DebuggingMode);
if #pkgs > 0 then error("packages with 'DebuggingMode' turned on: ", demark_", " keys pkgs)

pkgs = select(pkgopts, opts -> opts.Reload);
if #pkgs > 0 then error("packages with 'Reload' turned on: ", demark_", " keys pkgs)

-- c.f. commit 7a7e6f96
pkgs = select(pkgopts, opts -> 100 < #opts.Headline or match({
--	    "^[A-Z]", -- headlines should not be capitalized, but hard to check
	    "\\.$",   -- headlines should not end with a period
	    "\\n",    -- headlines should not contain line breaks
	    -- redundant clauses for package headlines:
	    "(T|t)his (P|p)ackage",
	    "(P|p)ackage (for|on|to)"}, opts.Headline));
if #pkgs > 0 then error("packages whose headlines doesn't follow guidelines:", newline,
    toString netList(toList \ pairs applyValues(pkgs, opts -> opts.Headline)))

importFrom(Core, "fetchRawDocumentation")
elapsedTime pkgs = select(pkglist, pkg -> (
	tag := makeDocumentTag(pkg|"::"|pkg);
	rawdoc := fetchRawDocumentation tag;
	rawdoc === null or rawdoc.Description === {}));
if #pkgs > 0 then error("packages with no top level node:", newline,
    toString stack sort pkgs)

end--
restart
load "release-checklist.m2"
