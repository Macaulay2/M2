-- Documentation for chemical reaction motifs


doc ///
-- enter each motif as a list of strings describing each reaction
    Key
    	twoSiteModificationG
	1:(twoSiteModificationG)
    Headline
    	an example chemical reaction motif
    Description
    	Text
	    A two-site phosphorylation cycle, where phosphorylation is
	    catalyzed by the same kinase at both sites and dephosphorylation is 
	    catalyzed by the same phosphatase at both sites [Feliu & Wiuf].
    	Example
         -- Two-site Modification G
	    twoSiteModificationG()
	Text
	    In the motif Two-site Modification G, the symbols used in the 
	    chemical reactions have the following meaning:
	    
	    S_i and P_i are substrates with i = 0, 1, 2 phosphorylated sites,
	    E is a kinase and F is a phosphotase,
	    X and Y are the intermediate complexes formed by the 
	    enzymes E and F, respectively.
		
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.		
    /// 
    		    
doc /// 
    Key
    	modificationOfTwoSubstratesH
	1:(modificationOfTwoSubstratesH)
    Headline
    	an example chemical reaction motif
    Description
    	Text
	    In this system, two cycles are connected through a joint 
	    catalyzing kinase [Feliu & Wiuf].
    	Example
        -- Modification of Two Substrates H
	    modificationOfTwoSubstratesH()
	Text
	    In the motif Modification of Two Different Substrates H, 
	    the symbols used in the chemical reactions have the following meaning:
	    
	    S_i and P_i are substrates with i = 0, 1, 2 phosphorylated sites,
	    E is a kinase and F is a phosphotase,
	    X and Y are the intermediate complexes formed by the 
	    enzymes E and F, respectively.
		
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.
	 	
    ///
   

doc /// 
    Key
    	modificationOfTwoSubstratesI
	1:(modificationOfTwoSubstratesI)
    Headline
    	an example chemical reaction motif
    Description
    	Text
	    In this system, two cycles are connected through a joint 
	    catalyzing kinase and a joint catalyzing phosphotase [Feliu & Wiuf].
        Example 
        -- Modification of Two Substrates I
	    modificationOfTwoSubstratesI()
	Text
	    In the motif Modification of Two Different Substrates I, 
	    the symbols used in the chemical reactions have the following meaning:
	    
	    S_i and P_i are substrates with i = 0, 1, 2 phosphorylated sites,
	    E is a kinase and F is a phosphotase,
	    X and Y are the intermediate complexes formed by the 
	    enzymes E and F, respectively.
		
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.   
    ///
    
    
doc /// 
    Key
    	twoLayerCascadeJ
	1:(twoLayerCascadeJ)
    Headline
    	an example chemical reaction motif
    Description
    	Text
	    A combination of two one-site modification cycles in a
	    cascade motif with a specific phosphotase acting in each 
	    layer [Feliu & Wiuf].
        Example
        -- Two-layer Cascade J
	    twoLayerCascadeJ()	
	Text
	    In the motif Modification of Two-layer Cascade J, 
	    the symbols used in the chemical reactions have the following meaning:
	    
	    S_i and P_i are substrates with i = 0, 1, 2 phosphorylated sites,
	    E is a kinase and F is a phosphotase,
	    X and Y are the intermediate complexes formed by the 
	    enzymes E and F, respectively.
		
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.
    ///
    

doc /// 
    Key
    	twoLayerCascadeK
	1:(twoLayerCascadeK)
    Headline
    	an example chemical reaction motif
    Description
    	Text
	    A combination of two one-site modification cycles in a
	    cascade motif where the phosphotase is not layer specific;
	    that is the same phosphotase acts in both layers [Feliu & Wiuf].
        Example
        -- Two-layer Cascade K
	    twoLayerCascadeK()
	Text
	    In the motif Modification of Two-layer Cascade K, 
	    the symbols used in the chemical reactions have the following meaning:
	    
	    S_i and P_i are substrates with i = 0, 1, 2 phosphorylated sites,
	    E is a kinase and F is a phosphotase,
	    X and Y are the intermediate complexes formed by the 
	    enzymes E and F, respectively.
		
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.
    ///
    		

doc ///
    Key
    	clusterModelCellDeath
	1:(clusterModelCellDeath)
    Headline
    	an example of a chemical reaction
    Description
    	Text
	    A cluster model for cell death, where a cluster is indexed by a tuple
	    (L, X, Y, Z), where L represents FasL and X, Y, and Z are three 
	    posited forms of Fas, denoting closed, open and unstable, 
	    and open and stable, i.e., active and signaling, receptors, 
	    respectively [Feliu & Wiuf].
        Example
    	-- Cluster Model Cell Death
	   clusterModelCellDeath()
	Text
	    In this example n=m=3.
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.
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