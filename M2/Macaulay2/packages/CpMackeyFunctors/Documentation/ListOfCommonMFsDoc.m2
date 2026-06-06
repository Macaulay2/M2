doc ///
    Key
        "list of common Mackey functors"
    Headline
        a list of common Cp-Mackey functors
    Description
        Text
            Here we include a table of common Mackey functors for the group $C_p$. The Lewis diagrams have to be formatted in @TT{"\\substack"}@ in order to compile, apologies for their appearance.

            @TABLE{
                ((BOLD "  Ravenel symbol  "),(BOLD "  Lewis diagram  "), (BOLD "  Description  "), (BOLD "  Implementation  ")),
                ((TEX "$\\square$"), (TEX "$$\\substack{\\ZZ \\\\ p\\uparrow\\downarrow 1\\\\ \\ZZ \\\\ \\circlearrowleft \\\\ 1}$$"),("fixed point Mackey functor of ", TEX"$\\ZZ$"," as a trivial ", TEX"$C_p$", "-module"), (TT "makeFixedPointMackeyFunctor(p,id_(ZZ^1))")),
                ((TEX "$\\circ$"), (TEX "$$\\substack{\\ZZ/p \\\\ 0\\uparrow\\downarrow 0\\\\ 0 \\\\ \\circlearrowleft \\\\ 1}$$"), ("the ", EM "zero-on-bottom", " Mackey functor associated to ", TEX"$\\ZZ/p$"), (TT"makeZeroOnUnderlyingMackeyFunctor(p,coker(matrix{{p}}))")),
                (("⧄"), (TEX "$$\\substack{\\ZZ \\\\ 1\\uparrow\\downarrow p\\\\ \\ZZ \\\\ \\circlearrowleft \\\\ 1}$$"), ("the orbit Mackey functor of ", TEX"$\\Z$"), (TT"makeOrbitMackeyFunctor(p,id_(ZZ^1))")),
                ((TEX "$\\ominus^n$"), (TEX "$$\\substack{\\ZZ / (q^n-1) \\\\ 1\\uparrow\\downarrow \\sum_{j=0}^{p-1} q^{nj} \\\\ \\ZZ/ (q^{np} - 1) \\\\ \\circlearrowleft \\\\ q^n}$$"), ("the fixed point Mackey functor of the algebraic ", TEX"$K$", "-group ", TEX"$K_{2n-1}(\\mathbb{F}_{q^p})$", " for a prime power ", TEX"$q$"), (TT"makeKGroupMackeyFunctor(p,q,n)"))
            }@
///