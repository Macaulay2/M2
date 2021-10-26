needs "monideal.m2"
needs "expressions.m2"
needs "reals.m2"
needs "varieties.m2"

lastprompt := "";

ZZ#{Jupyter,InputPrompt} = lineno -> concatenate(concatenate("[INP]",newline,"[INP]"), lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "));
ZZ#{Jupyter,InputContinuationPrompt} = lineno -> concatenate("[INP]",#lastprompt);

Thing#{Jupyter,Print}   = x -> ( << "[VAL]" << endl; Thing#{Standard,Print}(x) )
Nothing#{Jupyter,Print} = identity

for cls in {Thing, Nothing, Boolean, ZZ, InexactNumber, Expression, Net, Describe,
            Ideal, MonomialIdeal, Matrix, Module, RingMap, Sequence, CoherentSheaf}
    do (cls)#{Jupyter,AfterPrint} = (cls -> x -> ( << "[CLS]" << endl; (cls)#{Standard,AfterPrint} x )) cls;
