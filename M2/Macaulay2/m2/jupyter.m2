lastprompt := "";

ZZ#{Jupyter,InputPrompt} = lineno -> concatenate(concatenate("[INP]",newline,"[INP]"), lastprompt = concatenate(interpreterDepth:"i", toString lineno, " : "));
ZZ#{Jupyter,InputContinuationPrompt} = lineno -> concatenate("[INP]",#lastprompt);
setJupyter = () -> ( ZZ#{Standard,InputPrompt} = ZZ#{Jupyter,InputPrompt}; ZZ#{Jupyter,InputPrompt} = x->print("hi") )

Thing#{Jupyter,Print}   = x -> ( << "[VAL]" << endl; Thing#{Standard,Print}(x) )
Nothing#{Jupyter,Print} = identity

----------------------
-- why doesn't the following work!?
-- for cls in {Thing, Nothing, ...} do (cls)#{Jupyter,AfterPrint} = (cls)#{Standard,AfterPrint}
----------------------
Thing#{Jupyter,AfterPrint}         = x -> ( << "[CLS]" << endl; Thing#{Standard,AfterPrint} x )
Nothing#{Jupyter,AfterPrint}       = x -> ( << "[CLS]" << endl; Nothing#{Standard,AfterPrint} x )
Boolean#{Jupyter,AfterPrint}       = x -> ( << "[CLS]" << endl; Boolean#{Standard,AfterPrint} x )
ZZ#{Jupyter,AfterPrint}            = x -> ( << "[CLS]" << endl; ZZ#{Standard,AfterPrint} x )
InexactNumber#{Jupyter,AfterPrint} = x -> ( << "[CLS]" << endl; InexactNumber#{Standard,AfterPrint} x )
Expression#{Jupyter,AfterPrint}    = x -> ( << "[CLS]" << endl; Expression#{Standard,AfterPrint} x )
Net#{Jupyter,AfterPrint}           = x -> ( << "[CLS]" << endl; Net#{Standard,AfterPrint} x )
--Describe#{Jupyter,AfterPrint}      = x -> ( << "[CLS]" << endl; Describe#{Standard,AfterPrint} x ) --??
Ideal#{Jupyter,AfterPrint}         = x -> ( << "[CLS]" << endl; Ideal#{Standard,AfterPrint} x )
MonomialIdeal#{Jupyter,AfterPrint} = x -> ( << "[CLS]" << endl; MonomialIdeal#{Standard,AfterPrint} x )
Matrix#{Jupyter,AfterPrint}        = x -> ( << "[CLS]" << endl; Matrix#{Standard,AfterPrint} x )
Module#{Jupyter,AfterPrint}        = x -> ( << "[CLS]" << endl; Module#{Standard,AfterPrint} x )
RingMap#{Jupyter,AfterPrint}       = x -> ( << "[CLS]" << endl; RingMap#{Standard,AfterPrint} x )
Sequence#{Jupyter,AfterPrint}      = x -> ( << "[CLS]" << endl; Sequence#{Standard,AfterPrint} x )
CoherentSheaf#{Jupyter,AfterPrint} = x -> ( << "[CLS]" << endl; CoherentSheaf#{Standard,AfterPrint} x )
