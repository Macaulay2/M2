-- Documentation for chemical reaction motifs


doc ///
-- enter each motif as a list of strings describing each reaction
    Key
    	"Two-site Modification (g)"
    Headline
    	"an example chemical reaction motif"
    Description
    	Text
	    "A two-site phosphorylation cycle, where phosphorylation is
	    catalyzed by the same kinase at both sites and dephosphorylation is 
	    catalyzed by the same phosphatase at both sites [ref]"
	
    Example
    
         -- Two-site Modification G
	 twoSiteModificationG = {"S_0+E <--> X_1", "X_1 --> S_1+E",
                                "S_1+E <--> X_2", "X_2 --> S_2+E",
				"S_1+F <--> Y_1", "Y_1 --> S_0+F",
				"S_2+F <--> Y_2", "Y_2 --> S_1+F"}
			    
	 -- apply reactionNetwork 
	 reactionNetwork(twoSiteModificationG)
	 
    ///
    
    		    
doc /// 
    Key
    	"Modification of Two Different Substrates (h)"
    Headline
    	"an example chemical reaction motif"
    Description
    	Text
	    "In this system, two cycles are connected through a joint 
	    catalyzing kinase [ref]."
    Example
        -- Modification of Two Substrates H
	modificationOfTwoSubstratesH = {"S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	    	    	"S_1+F_1 <--> Y_1", "Y_1 --> S_0+F_1",
				"P_0+E <--> X_2", "X_2 --> P_1+E",
				"P_1+F_2 <--> Y_2", "Y_2 --> P_0+F_2"}

        -- apply reactionNetwork 
	reactionNetwork(modificationOfTwoSubstratesH)	

    ///
   

doc /// 
    Key
    	"Modification of Two Different Substrates (i)"
    Headline
    	"an example chemical reaction motif"
    Description
    	Text
	    "In this system, two cycles are connected through a joint 
	    catalyzing kinase and a joint catalyzing phosphotase [ref]."
    Example 
        -- Modification of Two Substrates I
	modificationOfTwoSubstratesI = {"S_0+E <--> X_1", "X_1 --> S_1+E",
                                "S_1+F <--> Y_1", "Y_1 --> S_0+F", 
				"P_0+E <--> X_2", "X_2 --> P_1+E",
				"P_1+F <--> Y_2", "Y_2 --> P_0+F"}
			    
	reactionNetwork(modificationOfTwoSubstratesI)

    ///
    
    
doc /// 
    Key
    	"Two-layer Cascade (j)"
    Headline
    	"an example chemical reaction motif"
    Description
    	Text
	    "A combination of two one-site modification cycles in a
	    cascade motif with a specific phosphotase acting in each layer [ref]."
    Example
        -- Two-layer Cascade J
	twoLayerCascadeJ = {"S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	    "S_1+F_1 <--> Y_1", "Y_1 --> S_0+F_1",
		    "P_0+S_1 <--> X_2", "X_2 --> P_1+S_1",
		    "P_1+F_2 <--> Y_2", "Y_2 --> P_0+F_2"}

        reactionNetwork(twoLayerCascadeJ)		

    ///
    

doc /// 
    Key
    	"Two-layer Cascade (k)"
    Headline
    	"an example chemical reaction motif"
    Description
    	Text
	    "A combination of two one-site modification cycles in a
	    cascade motif where the phosphotase is not layer specific;
	    that is the same phosphotase acts in both layers [ref]."
    Example
        -- Two-layer Cascade K
	twoLayerCascadeK = {"S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	    "S_1+F <--> Y_1", "Y_1 --> S_0+F",
		    "P_0+S_1 <--> X_2", "X_2 --> P_1+S_1",
		    "P_1+F <--> Y_2", "Y_2 --> P_0+F"}

        reactionNetwork(twoLayerCascadeK)
	
    ///
    		

doc ///
    Key
    	"Cluster Model for Cell Death"
    Headline
    	"an example of chemical reaction"
    Description
    	Text
	    "A cluster model for cell death, where a cluster is indexed by a tuple
	    (L, X, Y, Z), where L represents FasL and X, Y, and Z are three posited forms of 
	    Fas, denoting closed, open and unstable, and open and stable, i.e., active
	    and signaling, receptors, respectively [ref]."
    Example
    	clusterModelCellDeath = {"Y+Z --> 2Z", "L+Y+Z --> L+2Z",
	    	    	    	 "2Y --> Y+Z", "L+2Y --> L+Y+Z",
				 "2Y --> 2Z", "L+2Y --> L+2Z",
				 "Y+2Z --> 3Z", "L+Y+2Z --> L+3Z",
				 "2Y+Z --> Y+2Z", "L+2Y+Z --> L+Y+2Z",
				 "2Y+Z --> 3Z", "L+2Y+Z --> L+3Z",
				 "3Y --> 2Y+Z", "L+3Y --> L+2Y+Z",
				 "3Y --> Y+2Z", "L+3Y --> L+Y+Z",
				 "3Y --> 3Z", "L+3Y --> L+3Z"}
	reactionNetwork(clusterModelCellDeath)
    				   		    
 