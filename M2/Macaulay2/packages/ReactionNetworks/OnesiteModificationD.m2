beginDocumentation()
doc ///
    Key
    	"OnesiteModificationD"
    Headline
    	an example for Motifs
    Description
      Text
        This is a one-site modification cycle with two competing kinases and two competing phosphatases.
      Example
        CRN = oneSiteModificationA {"S_0", "E_1", "X_1", "S_1", "F_1", "Y_1", "E_2", "X_2", "F_2", "Y_2"}
        OnesiteModificationD = {"S_0 + E_1 <--> X_1", "X_1 --> S_1+E_1", "S_1 + F_1 <--> Y_1", "Y_1 --> dS_0 + F_1", "S_0 + E_2 <--> X_2", "X_2 --> S_1 + E_2", "S_1 + F_2 <--> Y_2", "Y_2 --> S_0 + F_2"}
    ///