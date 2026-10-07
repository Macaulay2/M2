
------------------------------------------------------------------------
----------------------------- LaTeX tables -----------------------------
------------------------------------------------------------------------

texTables = method(Options => {Verbose => false, KeepFiles => false, FileName => null});

texTables List := o -> L -> (
    (F1,F2) := (null,null);
    if o.FileName =!= null then (F1,F2) = (o.FileName | "All", o.FileName | "K3");
    L1 := texTableRaw(L,Verbose=>o.Verbose,KeepFiles=>o.KeepFiles,FileName=>F1,"IncludeK3Info"=>false);
    L2 := texTableRaw(L,Verbose=>o.Verbose,KeepFiles=>o.KeepFiles,FileName=>F2,"IncludeK3Info"=>true);
    (L1,L2)
);

texTableRaw = method(Options => {Verbose => true,
                                KeepFiles => false,
                                FileName => null,
                                "SortRows" => true,
                                "OpenPDF" => false,
                                "IncludeK3Info" => true,
                                "RowColor" => null,
                                "ExampleID" => null,
                                "NoetherLefschetzLocus" => null,
                                "PolarizationDataOnK3" => null});
texTableRaw List := o -> L -> (
    if #L == 0 then error "expected a non-empty list";
    if not all(L, X -> instance(X,HodgeSpecialFourfold)) then error "expected a list of Hodge-special fourfolds";
    DSCFcase := all(L, X -> instance(X,DoublySpecialCubicFourfold));
    if (not DSCFcase) and any(L, X -> instance(X,DoublySpecialCubicFourfold)) then error "expected a list of Hodge-special fourfolds of the same type; either all entries must be DSCFs or none of them";
    for X in L do describe X;
    if o#"IncludeK3Info" then (
        if o.Verbose then << "-- selecting fourfolds with an associated K3 surface..." << endl;
        hasK3 := X -> (
            if instance(X,DoublySpecialCubicFourfold) then return hasRationalSection X;
            if instance(X,CubicFourfold) then return isAdmissible X;
            if instance(X,GushelMukaiFourfold) then return isAdmissibleGM X;
            if instance(X,IntersectionOfThreeQuadricsInP7) then return true; -- forced
            false
        );
        L = select(L,hasK3);
        if #L == 0 then error "none of the fourfolds in the list has an associated K3 surface";
    );
    if o#"SortRows" then (
        if o.Verbose then << "-- sorting example list..." << endl;
        L' := sort L;
        if L' === L then (
            if o.Verbose then << "-- example list already sorted" << endl;
        ) else (
            if o.Verbose then << "-- example list order changed" << endl;
            L = L';
        );
    );
    polarizationDataOnK3 := o#"PolarizationDataOnK3";
    if polarizationDataOnK3 === null then polarizationDataOnK3 = (X -> (
        if computationStatus X < 2 then return (null," - ");
        polDat := knownDataForPolarizedK3AssociatedToDSCF X;
        if polDat === null then return (null," - ");
        if instance(last polDat,String) then return (null,last polDat);
        (matrix first polDat,if last polDat then "virt." else "yes")
    ));
    rowColor := o#"RowColor";
    if rowColor === null then rowColor = (X -> (
        if o#"IncludeK3Info" and member(last polarizationDataOnK3 X,{"heavy","no"}) then return ///\rowcolor{yellow!10} ///;
        if instance(X,DoublySpecialCubicFourfold) and eulerCharacteristic surface X =!= 3 + (sum toList drop(first (surface X).cache#"ConstructionParameters",1)) - 6*(numberNodes surface X) then return ///\rowcolor{blue!10} ///;
        if instance(X,DoublySpecialCubicFourfold) and X.cache#?(append(surfaces X,"parameterCount")) and first X.cache#(append(surfaces X,"parameterCount")) === 1 then return ///\rowcolor{green!7} ///;
        if (not instance(X,DoublySpecialCubicFourfold)) and X.cache#?(surface X,"parameterCount") and first X.cache#(surface X,"parameterCount") === 1 then return ///\rowcolor{green!7} ///;
        ""
    ));
    noetherLefschetzLocus := o#"NoetherLefschetzLocus";
    if noetherLefschetzLocus === null then noetherLefschetzLocus = (X -> (
        local disc;
        if instance(X,DoublySpecialCubicFourfold) then (
            disc = discriminant X;
            Cd1Cd2 := /// $\mathcal C_{/// | (toString disc) | "}";
            if disc != 8 then Cd1Cd2 = Cd1Cd2 | ///\cap \mathcal C_8 ///;
            return(Cd1Cd2 | "$ ");
        );
        if instance(X,CubicFourfold) then return(/// $\mathcal C_{/// | (toString discriminant X) | "}$ ");
        if instance(X,GushelMukaiFourfold) then (
            disc = discriminant X;
            D := /// $\mathcal{GM}_{/// | (toString disc) | "}";
            (a,b) := last cycleClass X;
            if disc % 8 == 2 then (
                if even(a+b) and odd(b)
                then D = D|"'"
                else if odd(a+b) and even(b)
                then D = D|"''"
                else error "internal error encountered";
            );
            return(D | "$ ");
        );
        " - "
    ));
    exampleID := o#"ExampleID";
    if DSCFcase and exampleID === null then exampleID = (X -> (
        if substring(0,8,recognizeDSCF X) =!= "DSCF-V1-" then return " - ";
        i := value substring(8,recognizeDSCF X);
        if not(instance(i,ZZ) and i >= 1 and i <= 40) then return " - ";
        tex i
    ));
    tableRows := "";
    local X;
    for i from 1 to #L do (
        X = L_(i-1);
        tableRows = tableRows | ///\\/// | newline | ///\hline/// | newline | (rowColor X) | (toString i) | " & ";
        if DSCFcase then tableRows = tableRows | (exampleID X) | " & ";
        tableRows = tableRows | (tableRowFourfoldInfoTex X) | " & " | (noetherLefschetzLocus X) | " & " | (parameterCountTex X);
        if o#"IncludeK3Info" then (
            tableRows = tableRows | " & " | (associatedK3Tex X) | " & " | minimalK3Tex(X,"PolarizationDataOnK3"=>polarizationDataOnK3);
            if DSCFcase then tableRows = tableRows | " & " | (exampleID X);
            tableRows = tableRows | " & " | (toString i);
        ) else (
            if DSCFcase then tableRows = tableRows | " & " | (tex X.cache#(append(surfaces X,"numberOfResidualPointsInGenericQuadricFiber")));
        );
    );
    tableName := o.FileName;
    if tableName === null then (if o#"IncludeK3Info" then tableName = "tableK3" else tableName = "tableAll");
    if fileExists(tableName | ".tex") then removeFile(tableName | ".tex");
    if fileExists(tableName | ".pdf") then removeFile(tableName | ".pdf");
    (tableName | ".tex") << latexTablePreamble(#L, if o#"IncludeK3Info" then 50 else 30) << (if o#"IncludeK3Info" then latexTableHeaderK3(first L) else latexTableHeaderAll(first L)) << tableRows << latexTableEnding(first L) << close;
    pdflatex := findProgram("pdflatex",RaiseError=>true);
    runProgram(pdflatex, "\"" | tableName | ".tex\" > /dev/null 2>&1", RaiseError=>true, Verbose=>false);
    -- a := run("pdflatex " | tableName | " > /dev/null 2>&1");
    -- if a =!= 0 then error "LaTeX compilation error";
    try removeFile(tableName | ".log");
    try removeFile(tableName | ".aux");
    if not o.KeepFiles then removeFile(tableName | ".tex");
    if fileExists(tableName | ".pdf") then (
        if o.Verbose then << "-- file " << tableName << ".pdf successfully created in " << currentDirectory() << endl;
        if o#"OpenPDF" then run("open \"" | tableName | ".pdf\" &");
    );
    L
);

degreesVarTex = method();
degreesVarTex MultiprojectiveVariety := X -> concatenate for l in degrees X list ("{" | (toString unsequence toSequence first l) | "}^{" | (toString last l) | "}\\ ");

tableRowFourfoldInfoTex = method();
tableRowFourfoldInfoTex DoublySpecialCubicFourfold := X -> (
    (S,P) := surfaces X;
    C := S * P;
    if not S.cache#?"ConstructionParameters" then error "not implemented yet: texTables for doubly-special cubic fourfolds not constructed via specialFourfold(surface((...),(...)))";
    (ai1i2i3,dj1j2j3) := take(S.cache#"ConstructionParameters",2);
    row := ///$\mathfrak{S}/// | (texMath matrix {toList ai1i2i3, toList dj1j2j3}) | /// $ & $\begin{array}{c} \deg(S)=/// | (toString degree S) | " , g(S)= " | (toString sectionalGenus S) | /// \\ \operatorname{gens\,id.}: /// | (degreesVarTex S) | /// \\ \deg(C)= /// | (toString degree C) | " , n(C)= " | (toString numberNodes S) | /// \end{array}$ ///;
    A := latticeIntersectionMatrix3x3 X;
    q := quotientRemainder(det A,8);
    -- z := if last q == 5 then " = " | (toString first q) | /// \cdot 8 + /// | (toString last q) | " = " else " = ";
    z := " = " | (toString first q) | /// \cdot 8 + /// | (toString last q) | " = ";
    row | " & $" | toString(det A) | z | /// \det /// | (texMath A) | "$ "
);
tableRowFourfoldInfoTex HodgeSpecialFourfold := X -> (
    S := surface X;
    local row;
    if S.cache#?"linear system on PP^2" then (
        l := toSequence S.cache#"linear system on PP^2";
        L := if #l == 1 then "(" | (toString l) | ")" else toString l;
        row = ///$\mathfrak{S}/// | L | ///\subset \mathbb{P}^{/// | (toString dim ambient S) | "} $ ";
    ) else (
        row = ///$S \subset \mathbb{P}^{/// | (toString dim ambient S) | "} $ ";
    );
    row = row | /// & $\begin{array}{c} \deg(S)=/// | (toString degree S) | " , g(S)= " | (toString sectionalGenus S) | /// \\ \operatorname{gens\,id.}: /// | (degreesVarTex S) | /// \\ n(S)= /// | (toString numberNodes S) | /// \end{array}$ ///;
    disc := discriminant X;
    row = row | " & $" | toString(disc);
    if X.cache#?(S,"LatticeIntersectionMatrix") then (
        A := X.cache#(S,"LatticeIntersectionMatrix");
        assert(disc == det A);
        row = row | " = " | /// \det /// | (texMath A);
    );
    row | "$ "
);

parameterCountTex = method();
parameterCountTex DoublySpecialCubicFourfold := X -> (
    (S,P) := surfaces X;
    errLog := "internal error encountered: invalid cached parameterCount values";
    (w,x,y,z) := 4 : " - ";
    w0 := w;
    if X.cache#?(S,P,"parameterCount") then (
        w = first X.cache#(S,P,"parameterCount");
        (x,y,z) = last X.cache#(S,P,"parameterCount");
        if w =!= 54 - ((x-1) + y - z) then error errLog;
        w0 = " $" | (toString w) | " = 54 - (" | (toString y) | " + " | (toString(x-1)) | " - " | (toString z) | ")$ ";
    ) else (
        x = 1 + dim target rationalMap(S+P,3);
    );
    (w',x',y',z') := 4 : " - ";
    w1 := w';
    if X.cache#?(S,"parameterCount") then (
        w' = first X.cache#(S,"parameterCount");
        (x',y',z') = last X.cache#(S,"parameterCount");
        if w' =!= 55 - ((x'-1) + y' - z') then error errLog;
        if z === " - " then z = z';
        if z =!= z' then error errLog;
        w1 = tex w';
    ) else (
        x' = 1 + dim target rationalMap(S,3);
        if X.cache#?(S,"CustomParameterCount") then (
            (x'',y'',z'') := X.cache#(S,"CustomParameterCount");
            if x'' =!= x' then error errLog;
            if z === " - " then z = z'';
            if z =!= z'' then error errLog;
            if y'' =!= null then (
                y' = y'';
                w1 = tex(55 - ((x'-1) + y' - z)) | "(!) ";
            );
        );
    );
    w0 | "& $" | (toString y) | ", " | (toString y') | "$ & $" | (toString x) | "," | (toString x') | "$ & $" | (toString z) | "$ & " | w1
);

parameterCountTex HodgeSpecialFourfold := X -> (
    (w,x,y,z) := 4 : " - ";
    w0 := w;
    if X.cache#?(surface X,"parameterCount") then (
        w = first X.cache#(surface X,"parameterCount");
        (x,y,z) = last X.cache#(surface X,"parameterCount");
        w0 = " $" | (toString w) | " = " | toString(w + (x-1) + y - z) | " - (" | (toString y) | " + " | (toString(x-1)) | " - " | (toString z) | ")$ ";
    ) else (
        x = 1 + dim target multirationalMap map X;
    );
    w0 | "& $" | (toString y) | "$ & $" | (toString x) | "$ & $" | (toString z) | "$ "
);

associatedK3Tex = method();
associatedK3Tex HodgeSpecialFourfold := X -> (
    if computationStatus X == -1 then return " - & - & - & - ";
    mu := if instance(X,DoublySpecialCubicFourfold) then fanoMapDSCF X else multirationalMap fanoMap X;
    W := target mu;
    row := ///$ d_1(\mu) = /// | (toString degreeOfDefiningForms mu) | " $ & ";
    row = row | (if codim W == 0 then /// $W=\mathbb{P}^4$ /// else (/// $\begin{array}{c} W\subset\mathbb{P}^{/// | (toString dim ambient W) | ///} \\ \deg(W)=/// | (toString degree W) | ", g(W)=" | (toString sectionalGenus W) | /// \\ \operatorname{gens\,id.}: /// | (degreesVarTex W) | /// \end{array} $ ///));
    if computationStatus X <= 0 then return (row | " & - & - ");
    U := surfaceDeterminingInverseOfFanoMap X;
    row = row | " & " | /// $\begin{array}{c} \deg(U)=/// | (toString degree U) | ///, g(U)=/// | (toString sectionalGenus U) | /// \\ \chi({\mathcal O}_U)= /// | (toString euler hilbertPolynomial U) | /// \\ \operatorname{gens\,id.}: /// | (degreesVarTex U) | /// \end{array} $ ///;
    if computationStatus X <= 1 then return (row | " & - ");
    (L,C) := exceptionalCurves X;
    row = row | " & " | /// $\begin{array}{ll} \operatorname{num\,lines}: & /// | (toString degree L) | /// \\ \deg\operatorname{others}: & /// | (toString degree C) | /// \end{array}$ ///;
    row
);

minimalK3Tex = method(Options => {"PolarizationDataOnK3" => null});
minimalK3Tex DoublySpecialCubicFourfold := o -> X -> (
    (M,s) := (o#"PolarizationDataOnK3") X;
    g := null;
    row := " - & - & " | s;
    if member(s,{" - ","heavy","no"}) then return row;
    if M =!= null then g = lift((M_(0,0) + 2)/2,ZZ);
    if computationStatus X >= 3 then (
        Utilde := projectiveVariety (fanoMapDSCF X).cache#("K3SurfaceFromDoublySpecialCubicFourfold",X);
        if g === null then g = sectionalGenus Utilde;
        if g =!= sectionalGenus Utilde then error "inconsistent sectional genus values encountered while constructing the table";
    );
    if g === null then return row;
    row = /// $\begin{array}{c} g(\widetilde{U}) = /// | (toString g) | /// \\ \deg(\widetilde{U})= /// | (toString(2*g-2)) | /// \end{array} $ ///;
    detMatr := A -> (
        str := /// $\det /// | (texMath A) | /// = /// | (toString det A) | /// $ ///;
        if det A + det latticeIntersectionMatrix3x3 X == 0 then str = str | /// \checkmark ///;
        str
    );
    if computationStatus X >= 4 then (
        row = row | " & " | (detMatr latticeMatrix polarizedK3surface X) | " & yes ";
        return row;
    );
    if M =!= null then (
        row = row | " & " | detMatr M;
    ) else (
        row = row | " & - ";
    );
    row | " & " | s
);
minimalK3Tex HodgeSpecialFourfold := o -> X -> (
    row := " - ";
    if computationStatus X < 3 then return row;
    U := X.cache#"AssociatedSurfaceCompleteData";
    if not instance(U,EmbeddedProjectiveVariety) then return row;
    assert(dim U == 2);
    /// $\begin{array}{c} g(\widetilde{U}) = /// | (toString sectionalGenus U) | /// \\ \deg(\widetilde{U})= /// | (toString degree U) | /// \end{array} $ ///
);

latexTablePreamble = method();
latexTablePreamble (ZZ,ZZ) := (n,m) -> ///
\documentclass[10pt]{amsart}
\usepackage[utf8]{inputenc}
\usepackage[
    paperwidth=/// | (toString m) | ///cm,
    paperheight=/// | (toString max(10, ceiling(1.11 * n))) | ///cm,
    margin=1cm
]{geometry}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{color,colortbl}
\usepackage{xcolor}
\usepackage[most]{tcolorbox}
\usepackage{adjustbox}
\usepackage{multirow}
\usepackage{url}
\usepackage{lscape}
\usepackage{verbatim}
\begin{document}
\thispagestyle{empty}
///;

latexTableHeaderK3 = method();
-- X is used only for method dispatch
latexTableHeaderK3 DoublySpecialCubicFourfold := X -> ///
\begin{table}[htbp]
%\renewcommand{\arraystretch}{1.5}
\centering
%\tabcolsep=1.5pt
\footnotesize
\begin{tabular}{|c|c||c|c|l|c||l|c|c|c|c||c|c|c|c|c|r|c||c|c|}
\hline
$i$ & ID & surface $S\subset X\subset \mathbb{P}^5$ & $S, C=S\cap P$ & & &  codim. in ${\mathcal C}_8$ &(\textdagger) & (\textdaggerdbl) & (\textsection) & (\textasteriskcentered) & $\mu:\mathbb{P}^5\dashrightarrow W$ & $W$ & $U$ & exc. curves & $\widetilde{U}$ & & K3 & ID & $i$
 \\
\hline
///;
latexTableHeaderK3 HodgeSpecialFourfold := X -> ///
\begin{table}[htbp]
%\renewcommand{\arraystretch}{1.5}
\centering
%\tabcolsep=1.5pt
\footnotesize
\begin{tabular}{|c||c|c|l|c||l|c|c|c||c|c|c|c|c||c|}
\hline
$i$ & surface $S\subset X$ & & & & parameter count &(\textdagger) & (\textdaggerdbl) & (\textsection) & $\mu$ & $W$ & $U$ & exc. curves & $\widetilde{U}$ & $i$
 \\
\hline
///;

latexTableHeaderAll = method();
-- X is used only for method dispatch
latexTableHeaderAll DoublySpecialCubicFourfold := X -> ///
\begin{table}[htbp]
%\renewcommand{\arraystretch}{1.5}
\centering
%\tabcolsep=1.5pt
\footnotesize
\begin{tabular}{|c|c||c|c|c|c|l|c|c|c|c||c|}
\hline
$i$ & ID & surface $S\subset X\subset \mathbb{P}^5$ & $S, C=S\cap P$ & & & codim. in ${\mathcal C}_8$ &(\textdagger) & (\textdaggerdbl) & (\textsection) & (\textasteriskcentered) & $\scriptstyle (Q\cap S)\setminus P$
 \\
\hline
///;
latexTableHeaderAll HodgeSpecialFourfold := X -> ///
\begin{table}[htbp]
%\renewcommand{\arraystretch}{1.5}
\centering
%\tabcolsep=1.5pt
\footnotesize
\begin{tabular}{|c||c|c|c|c|l|c|c|c|}
\hline
$i$ & surface $S\subset X$ & & & & parameter count &(\textdagger) & (\textdaggerdbl) & (\textsection)
 \\
\hline
///;

latexTableEnding = method();
-- X is used only for method dispatch
latexTableEnding DoublySpecialCubicFourfold := X -> ///
\\ \hline
\end{tabular}
\end{table}

\begin{comment}
\clearpage
\newpage
\begin{center}
{\footnotesize \today. (\textdagger): $\left(\dim\{S\cup P: S\cup P \subset \mathbb{P}^5\}, h^0(N_{S,\mathbb{P}^5})\right)$;
(\textdaggerdbl): $\left( h^0({\mathcal I}_{S\cup P,\mathbb{P}^5}(3)),h^0({\mathcal I}_{S,\mathbb{P}^5}(3)) \right)$;
(\textsection): $h^0(N_{S,X})$; (\textasteriskcentered) $\mathrm{codim}_{\mathcal C}\{[X]: S\subset X\}$, (!) denotes that the minimality conditions are not met.
}
\end{center}
\end{comment}
\end{document}
///;
latexTableEnding HodgeSpecialFourfold := X -> ///
\\ \hline
\end{tabular}
\end{table}

\begin{comment}
\clearpage
\newpage
\begin{center}
{\footnotesize \today. (\textdagger): $h^0(N_{S,\mathbb{P}^n})$;
(\textdaggerdbl): $h^0({\mathcal I}_{S,\mathbb{P}^n}(e))$;
(\textsection): $h^0(N_{S,X})$.
}
\end{center}
\end{comment}
\end{document}
///;

knownDataForPolarizedK3AssociatedToDSCF = X -> (
    if not instance(X,DoublySpecialCubicFourfold) then return;
    if substring(0,8,recognizeDSCF X) =!= "DSCF-V1-" then return;
    i := value substring(8,recognizeDSCF X);
    if not instance(i,ZZ) then return;
    if i == 1 then return({{26,9},{9,2}},true);
    if i == 2 then return({{26,9},{9,2}},false);
    if i == 3 then return({{30,9},{9,2}},false);
    if i == 4 then return({{26,9},{9,2}},false);
    if i == 5 then return({{50,11},{11,2}},"no");
    if i == 6 then return({{6,7},{7,2}},false);
    if i == 7 then return({{22,9},{9,2}},false);
    if i == 8 then return({{18,9},{9,2}},true);
    if i == 9 then return({{22,9},{9,2}},false);
    if i == 10 then return({{22,9},{9,2}},true);
    if i == 11 then return({{22,9},{9,2}},true);
    if i == 12 then return({{26,9},{9,2}},false);
    if i == 13 then return({{34,11},{11,2}},true);
    if i == 14 then return({{46,11},{11,2}},"no");
    if i == 15 then return({{14,9},{9,2}},true);
    if i == 16 then return({{34,11},{11,2}},"no");
    if i == 17 then return({{18,9},{9,2}},false);
    if i == 18 then return({{26,11},{11,2}},true);
    if i == 19 then return({{38,11},{11,2}},true);
    if i == 20 then return({{26,11},{11,2}},true);
    if i == 21 then return({{18,9},{9,2}},false);
    if i == 22 then return({{38,11},{11,2}},false);
    if i == 23 then return({{18,9},{9,2}},true);
    if i == 24 then return({{30,11},{11,2}},true);
    if i == 25 then return({{30,11},{11,2}},true);
    if i == 26 then return({{22,9},{9,2}},false);
    if i == 27 then return(null,"heavy");
    if i == 28 then return({{30,11},{11,2}},true);
    if i == 29 then return({{22,11},{11,2}},true);
    if i == 30 then return({{14,9},{9,2}},false);
    if i == 31 then return({{34,23},{23,14}},true);
    if i == 32 then return({{26,11},{11,2}},true);
    if i == 33 then return({{14,9},{9,2}},false);
    if i == 34 then return({{34,11},{11,2}},false);
    if i == 35 then return({{34,11},{11,2}},true);
    if i == 36 then return({{18,9},{9,2}},false);
    if i == 37 then return({{26,11},{11,2}},true);
    if i == 38 then return({{30,11},{11,2}},true);
    if i == 39 then return({{10,9},{9,2}},false);
    if i == 40 then return({{34,11},{11,2}},false);
);
