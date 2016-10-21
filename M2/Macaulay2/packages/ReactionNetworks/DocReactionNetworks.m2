-- Documentation for chemical reaction motifs

doc ///
    Key
        ReactionNetworks
    Headline
     	create chemical reaction networks and corresponding steady-state and conservation equations
    Description
     	Text
	    The @EM "ReactionNetworks" @ package provides functions for creating the 
	    steady-state and conservation equations corresponding to a given 
            reaction network. Included are some basic building-block motifs,
            which can be joined together to create specific reaction network.  
	    Examples are provided illustrating elimination and degeneration with
	    removal of a species or a reaction and the corresponding effect
            on the solutions of the system. 
	Text
	    {\bf Basic Functions:}
	       
		$\bullet$ @TO "reactionNetwork"@
		   
		$\bullet$ @TO "isDeficient"@
		   
		$\bullet$ @TO "isWeaklyReversible"@
	        
		$\bullet$ @TO "steadyStateEquations"@
		   
        	$\bullet$ @TO "conservationEquations"@
		
		$\bullet$ @TO "injectivityTest"@
		   
		$\bullet$ @TO "glue"@

	    {\bf Motifs:}

		$\bullet$ @TO "oneSiteModificationA"@
		   
	        $\bullet$ @TO "oneSiteModificationB"@
       	   
        	$\bullet$ @TO "oneSiteModificationC"@
		   
		$\bullet$ @TO "oneSiteModificationD"@
		   
	        $\bullet$ @TO "twoSiteModificationE"@
		   
		$\bullet$ @TO "twoSiteModificationF"@
		   
		$\bullet$ @TO "twoSiteModificationG"@
		   
		$\bullet$ @TO "modificationOfTwoSubstratesH"@
		   
		$\bullet$ @TO "modificationOfTwoSubstratesI"@
		   
		$\bullet$ @TO "twoLayerCascadeJ"@
		   
		$\bullet$ @TO "twoLayerCascadeK"@
		   
		$\bullet$ @TO "twoLayerCascadeL"@
		   
		$\bullet$ @TO "crosslinkingModelCellDeath"@
		   
		$\bullet$ @TO "clusterModelCellDeath"@
		   
		$\bullet$ @TO "wnt"@
		
	    {\bf Examples}
	       
	        The following example demonstrates how to compute
		the degree and dimension of the ideal cut out
		by the steady-state and conservation equations.  
	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    R = createRing(N, QQ)
	    F' = steadyStateEquations N
	    F'' = conservationEquations N
	    F = join(F',F'')
	    I = ideal F
	    -- Assign random values for parameters kk. How to do this?
	    -- dimension and degree are wrt to R = QQ[cc][kk][xx], incorrect
///

doc /// 
    Key
    	ReactionNetwork
    Headline
    	a mutable hash table, stores information about a reaction network
    Description
    	Text
	    @TO ReactionNetwork @ is a @TO Type @. 
    SeeAlso
	"reactionNetwork"	    	 	
    ///


doc /// 
    Key
    	reactionNetwork
	(reactionNetwork, String)
	(reactionNetwork, List)
    Headline
    	creates a reaction network
    Description
    	Text
	    Create a reaction network from a string.
    	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	Text
	    Create a reaction network from a list.
	Example
	    N = reactionNetwork {"S_0+E <--> X_1", "X_1 --> S_1+E",
	                         "S_1+E <--> X_2", "X_2 --> S_2+E",
				 "S_1+F <--> Y_1", "Y_1 --> S_0+F", 
				 "S_2+F <--> Y_2", "Y_2 --> S_1+F"}
	Text
    	    Create a reaction network including the empty set.
	Example
	    N = reactionNetwork ({"A --> 0", "0 --> A"}, NullSymbol => "0")
	Text
	    The user may view specific information stored in the reaction network, 
	    such as species, complexes, etc.
	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    N.Species
	    N.Complexes
	    N.ReactionGraph
        Text
	    Or the user may view all stored information about a reaction network:
	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    peek N
    SeeAlso
	"glue"
	"steadyStateEquations" 
	"conservationEquations"
	    	 	
    ///


doc /// 
    Key
    	steadyStateEquations
	(steadyStateEquations, ReactionNetwork)
	(steadyStateEquations, ReactionNetwork, Ring)
    Headline
    	creates steady-state equations of a reaction network
    Description
    	Text
	    Obtain the steady-state equations for a given network. 
	    Before any equations can be created, you must invoke the @TO createRing @ 
	    function, which creates the reaction network ring. If you do not create the ring,
	    you will receive an error message.
    	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    R = createRing(N, QQ)
	    steadyStateEquations N
	Text
	    Obtain the steady-state equations for a motif and display equations.
	Example
	    N = modificationOfTwoSubstratesI()
	    R = createRing(N, QQ)
	    steadyStateEquations N
	Text
	    Generate the steady-state equations in a specific ring.
	Example
	    N = twoLayerCascadeL()
	    R = createRing(N, ZZ/2)
	    F = steadyStateEquations(N, ZZ/2)
    SeeAlso
	"reactionNetwork"
	"conservationEquations"
	    	 	
    ///
    
    
doc /// 
    Key
    	conservationEquations
	(conservationEquations, ReactionNetwork)
	(conservationEquations, ReactionNetwork, Ring)
    Headline
    	creates the conservation equations of a reaction network
    Description
    	Text
	    Generate the conservation equations using the stoichiometric subspace.
	    Before any equations can be created, you must invoke the @TO createRing @ 
	    function, which creates the reaction network ring. If you do not create the ring,
	    you will receive an error message.
    	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    R = createRing(N, QQ)
	    conservationEquations N
	Text
	    Obtain the conservation equations for a motif and display equations.
	Example
	    N = modificationOfTwoSubstratesI()
	    createRing(N,QQ)
	    netList conservationEquations N
	Text
	    Generate the conservation equations in a specific ring.
	Example
	    N = twoLayerCascadeL()
	    R = createRing(N, ZZ/2)
	    G = conservationEquations(N, ZZ/2)
	Text
	    The conservation equations describe a linear subspace going through
	    the origin.  To translate the subspace, the user may choose to use
	    random values for the parameters, or enter specific values, such as 
	    initial conditions.  
	    --The examples below illustrate these options. Show both options.
	Example
	    N = twoLayerCascadeL()
	    N.Species
	    R = createRing(N,QQ)
	    G = conservationEquations(N, QQ)
--    	    S1 = netList(G - flatten entries random(QQ^1, QQ^(#G)))
	    --S2 = sub(G, {xx_(S_0) => (xx_(S_0)-1), xx_E => (xx_E-6/13), xx_(F_1) => (xx_(F_1)-7/10)})	    
	    --need to cache parameter ring first	    
    SeeAlso
	"reactionNetwork"
	"steadyStateEquations"
	    	 	
    ///


doc /// 
    Key
    	glue
	(glue, ReactionNetwork, ReactionNetwork)
	(glue, ReactionNetwork, List)
	(glue, List, ReactionNetwork)
    Headline
    	combine two networks 
    Description
    	Text
	    This function takes two reaction networks, or a reaction network and a 
	    list and creates a new, combined network.   
    	Example
	    N1 = reactionNetwork("A <-- 2B, A + C <-- D, B + E --> A + C")
	    glue(N1, {"A --> 2B", "A + C --> D", "D --> B+E"})
    SeeAlso
	"reactionNetwork"
	    	 	
    ///


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
    	wnt
	1:(wnt)
    Headline
    	shuttle model for Wnt signaling pathway
    Description
    	Text
	    The cannonical Wnt/beta-catenin signaling pathway is important for essential
	    cellular functions such as development, homeostasis, and is implicated in many
	    diseases [MacLean, Rosen, Byrne, & Harrington]. The Wnt shuttle model includes
	    an abstraction of the signal transduction pathway 
	    [Gross, Harrington, Rosen, & Sturmfels]. 
	Example
	    wnt()
	Text
	    In the Wnt shuttle model, the symbols used in the 
	    chemical reactions have the following meaning:
	    
	    X_1 - X_3: dishevelled species;
	    X_4 - X_7: destruction complex (APC/Axin/GSK3 beta);
	    X_8 - X_9: phosphatase;
	    Y_0 - Y_1: beta-catenin;
	    Y_2: transcription factor;
	    Y_3 - Y_9: intermediate complex;
	    0: empty set;
	    
	    For more details see Algebraic Systems Biology: A Case Study for the Wnt Pathway:
	    Gross, Harrington, Rosen, & Sturmfels.
				
	    To rename species, use @TO (substitute, ReactionNetwork, List) @.
    ///    
    				   		    
 doc ///
    Key
    	(substitute, ReactionNetwork, List)
    Headline
    	substitute species names in reaction network
    Description
    	Text
            The user may choose to change the symbols for one or more of
	    the species participating in the reaction. This can be done in 
	    the following manner. To rename one of the species:
	Example	
	    N = twoSiteModificationG()
	    sub(N, {"S_0" => "A"})
	Text     
	    Similarly, more species labels may be replaced.
	Example	
	     sub(N, {"S_0" => "A", "X_1" => "B", "X_2" => "C", "S_1" => "D"})
 ///
 
    --"reactionNetwork", "ReactionNetwork", "Species", "Complexes", "NullSymbol", "NullIndex", "ReactionGraph",
    --"stoichiometricSubspace", 
    --"steadyStateEquations", "conservationEquations", 
    --"laplacian", "FullEdges", "NullEdges" --, "netComplex", "networkToHRF", "glue"
    --"ReactionRing", "createRing", "ParameterRing",
    		    
 
 