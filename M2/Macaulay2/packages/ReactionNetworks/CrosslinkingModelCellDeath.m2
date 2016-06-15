beginDocumentation()
doc 
///
    Key
        "CrosslinkingModelCellDeath"
    Headline
        an example for mathematical cell death model. 
    Description
      Text
        The crosslinking model against the prevailing crosslinking model.	
      Example
        CRN = oneSiteModificationA {"L", "R", "C_1", "C_2", "C_3"}
        CrosslinkingModelCellDeath = {"L + R <--> C_1","C_1 + R <--> C_2", "C_2 + R <--> C_3"}
///