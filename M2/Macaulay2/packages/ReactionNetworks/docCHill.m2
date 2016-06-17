-- Documentation for chemical reaction motifs


doc ///
-- enter each motif as a list of strings describing each reaction
    Key
    	twoSiteModificationG
	1:(twoSiteModificationG)
    Headline
    	"an example chemical reaction motif"
    Description
    	Text
	    "A two-site phosphorylation cycle, where phosphorylation is
	    catalyzed by the same kinase at both sites and dephosphorylation is 
	    catalyzed by the same phosphatase at both sites [ref]"
    	Example
         -- Two-site Modification G
	    twoSiteModificationG()
	Text
	    "In the motif Two-site Modification (g), the symbols used in the 
	     chemical reactions have the following meaning:"
		
	     To rename species, use @TO (substitute, ReactionNetwork, List) @.		
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
	   needsPackage "ReactionNetworks"
	   modificationOfTwoSubstratesH()
	Text
	    "In the motif Modification of Two Different Substrates (h), 
	    the symbols used in the chemical reactions have the following meaning:
		
       
	    The user may choose to change the symbols for one or more of
	    the species participating in the reaction. This can be done in 
	    the following manner. To rename one of the species:"
	Example	
	    N = modificationOfTwoSubstratesH()
	    sub(N, {"X_2" => "C"})
	Text     
	    "Similarly, more species labels may be replaced."
	Example	
	    sub(N, {"S_0" => "A", "X_1" => "B", "X_2" => "C", "S_1" => "D"})	   	
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
	   needsPackage "ReactionNetworks"
	   modificationOfTwoSubstratesI()
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
	   needsPackage "ReactionNetworks"
	   twoLayerCascadeJ()	
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
	   needsPackage "ReactionNetworks"
	   twoLayerCascadeK()
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
    	-- Cluster Model Cell Death
	   needsPackage "ReactionNetworks"
	   clusterModelCellDeath()
    ///  
    				   		    
 doc ///
    Key
    	(substitute, ReactionNetwork, List)
    Headline
    	substitute species names in reaction network
    Description
    	Text
             "The user may choose to change the symbols for one or more of
	     the species participating in the reaction. This can be done in 
	     the following manner. To rename one of the species:"
	Example	
	     N = twoSiteModificationG()
	     sub(N, {"S_0" => "A"})
	Text     
	     "Similarly, more species labels may be replaced."
	Example	
	     sub(N, {"S_0" => "A", "X_1" => "B", "X_2" => "C", "S_1" => "D"})
 ///