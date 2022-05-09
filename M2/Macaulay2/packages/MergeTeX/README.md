# MergeTeX
A simple M2 script to parse M2 code inside a LaTeX file, run it and insert its output.

Just load it in M2 with `needsPackage "MergeTeX"`
then run it with e.g.
```
"output.tex" << mergeTeX get "input.tex" << close
```
or more simply
```
mergeTeXFile("input.tex","output.tex")
```

The M2 code must be enclosed in `\begin{lstlisting}[language=Macaulay2] \end{lstlisting}` or equivalent, see the ex.tex file.
(This uses the listings LaTeX package)
