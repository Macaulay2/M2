restart
importFrom_Core {"markdown"}

ver = version#"VERSION"
page = "changes, " | ver
addr = "https://www.macaulay2.com/doc/Macaulay2-" | ver | "/share"

changelog = markdown help page;
changelog = replace("/home/.*?/share", addr, changelog);
changelog = replace("common/share", addr, changelog);
("changelog-" | ver | ".md") << changelog << close

restart
importFrom_Core { "markdown", "TO", "TT", "IMG", "HREF" };
X = help "packages provided with Macaulay2";
markdown TO := x -> markdown HREF{format first x, TT format first x};
<< replace("common/share/Macaulay2/Style", "../static", markdown drop(X#1, 1))

restart
debug Core
X = help "Macaulay2";
markdown TO := x -> markdown HREF{format first x, TT format first x};
markdown TO2 := x -> markdown HREF{format first x, last x};
markdown TOH := x -> markdown HREF{format first x, format first x};
markdown MENU := x -> apply(x, markdown)
markdown X
<< replace("common/share/Macaulay2/Style", "../static", markdown drop(X#1, 1))
