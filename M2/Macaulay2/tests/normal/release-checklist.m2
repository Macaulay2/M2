-- https://github.com/Macaulay2/M2/wiki/Internals:-Release-Checklist

pkgs = select(separate_" " version#"packages",
    p -> (readPackage p)#Keywords === {"Uncategorized"});
if #pkgs > 0 then error("uncategorized packages:", newline,
    toString stack sort pkgs)

pkgs = select(separate_" " version#"packages",
	p -> (readPackage p)#DebuggingMode);
if #pkgs > 0 then error("packages with debugging mode turned on:", newline,
    toString stack sort pkgs)

importFrom(Core, "fetchRawDocumentation")
elapsedTime pkgs = select(separate_" " version#"packages",
    p -> (
	try (fetchRawDocumentation makeDocumentTag p).Description === {}
	else false));
if #pkgs > 0 then error("packages with no top level node:", newline,
    toString stack sort pkgs)
