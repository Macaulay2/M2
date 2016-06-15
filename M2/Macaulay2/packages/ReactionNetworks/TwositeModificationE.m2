beginDocumentation()
doc ///
    Key
    	"TwositeModificationE"
    Headline
    	an example for Motifs
    Description
      Text
        A two-site phosphorylation system in which modifications are carried out by different kinases and phosphatases for each phosphoform. Simply, it is assumed that both phosphorylation and dephosphorylation occur sequentially.
      Example
        CRN = oneSiteModificationA {"S_0", "E_1", "X_1", "S_1", "E_2", "X_2", "S_2", "F_1",  "Y_1", "F_2", "Y_2"}
        TwositeModificationE = {"S_0 + E_1 <--> X_1", "X_1 --> S_1+E_1", "S_1 + E_2 <--> X_2", "X_2 --> S_2 + E_2", "S_1 + F_1 <--> Y_1", "Y_1 --> S_0 + F_1", "S_2 + F_2 <--> Y_2", "Y_2 --> S_1 + F_2"}
    ///