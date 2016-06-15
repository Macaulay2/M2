beginDocumentation()
doc ///
    Key
    	"TwositeModificationF"
    Headline
    	an example for Motifs
    Description
      Text
        A two-site phosphorylation system where phosphorylation is catalyzed by the same kniase at both sites but dephosphorylation is catalyzed by different phosphateases. It assumes sequential (de)phosphorylation.
      Example
        CRN = oneSiteModificationA {"S_0", "E", "X_1", "S_1", "X_2", "S_2",  "F_1", "Y_1", "F_2", "Y_2"}
        TwositeModificationF = {"S_0 + E <--> X_1", "X_1 --> S_1 +E", "S_1 + E <--> X_2", "X_2 --> S_2 + E", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_2 + F_2 <--> Y_2", "Y_2 --> S_1 + F_2"}
    ///