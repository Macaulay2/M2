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

	--	$\bullet$ @TO "injectivityTest"@

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

		$\bullet$ @TO "crossLinkingModelCelldeath"@

		$\bullet$ @TO "clusterModelCellDeath"@

		$\bullet$ @TO "wnt"@
		
		$\bullet$ @TO "nSiteProcessiveModification"@
		
		$\bullet$ @TO "nSiteDistributiveModification"@
		
		$\bullet$ @TO "nSiteImmuneReaction"@
		
		$\bullet$ @TO "nSiteDiffusion"@
		
		$\bullet$ @TO "nSitePoreForming"@
		
		$\bullet$ @TO "nSiteSequestration"@
		
		$\bullet$ @TO "nSiteAutocatalytic"@

	    {\bf Examples}

	        The following example demonstrates how to compute
		the degree and dimension of the ideal cut out
		by the steady-state and conservation equations.
	Example
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    R = createRing(N, QQ)
    	Text
	    After creating the reaction network and the corresponding ring,
	    we create the steady state equations and substitute random values
	    for the reaction rates; this will allows us to compute the degree
	    of the ideal.
	Example
	    SS = flatten entries steadyStateEquations N
	    K = toList(apply(0..length N.ReactionRates-1, i-> random(QQ)))
	    Rr = toList(apply(0..length N.ReactionRates-1, i->
		    value(N.ReactionRates#i)))
	    P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
	    F' = toList apply(0..length SS-1, i-> sub(SS#i,P))
	Text
	    Next, we create the conservation equations and assume there is no
	    translation, i.e., the initial conditions are all zero.
	Example
	    C = conservationEquations N
	    L = {0,0,0,0,0}
	    Iv = toList(apply(0..length N.InitialValues-1, i->
		    value(N.InitialValues#i)))
	    S = toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
	    F'' = toList apply(0..length C-1, i-> sub(C#i,S))
	Text
	    Finally, we join the two sets of equations and create an ideal. Thus,
	    the degree and dimension can be computed.
	Example
	    F = join(F',F'')
	    I = ideal F
	    degree I
	    dim I

///

doc ///
    Key
    	ReactionNetwork
	Complexes
	ConcentrationRates
	InitialValues
	NullSymbol
	NullIndex
	ReactionGraph
	ReactionRates
	ReactionRing
	Species
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
       	     (reactionNetwork, Ideal)
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
	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"
	    peek N
  	Text
            Create a reaction network from a negative Laplacian matrix (or adjacency matrix) and the complexes.
        Example
            L = matrix{{-1,1,0,0,0},{1,-1,0,0,0},{0,0,-1,1,0},{0,0,1,-2,1},{0,0,1,0,-1}}
            Comp = transpose matrix{{1,0,0,0,0},{0,2,0,0,0},{1,0,1,0,0},{0,0,0,1,0},{0,1,0,0,1}}
      	    N = reactionNetwork({Comp,L},Input=>"Laplacian")
      	    peek N
  	Text
      	    The input can also be a weighted negative Laplacian matrix (or weighted adjacency matrix) and the complexes.
  	Example
      	    R = QQ[k_1..k_6,x_1..x_5]
      	    A = matrix{{-k_1,k_1,0,0,0},{k_2,-k_2,0,0,0},{0,0,-k_3,k_3,0},{0,0,k_4,-k_4-k_5,k_5},{0,0,k_6,0,-k_6}}
      	    Comp = transpose matrix{{1,0,0,0,0},{0,2,0,0,0},{1,0,1,0,0},{0,0,0,1,0},{0,1,0,0,1}}
      	    N = reactionNetwork({Comp,A},Input=>"Laplacian")
      	    peek N
  	Text
      	    Create a reaction network from a stoichiometric matrix and a flux vector.
	Example
      	    R = QQ[k_1..k_6,x_1..x_5]
      	    StoichM = matrix{{-1,1,-1,1,0,1},{2,-2,0,0,1,-1},{0,0,-1,1,0,1},{0,0,1,-1,-1,0},{0,0,0,0,1,-1}}
      	    FluxV = transpose matrix{{k_1*x_1,k_2*x_2^2,k_3*x_1*x_3,k_4*x_4,k_5*x_4,k_6*x_2*x_5}}
      	    N = reactionNetwork({StoichM,FluxV},Input=>"Stoichiometric")
            peek N
        Text
            Create a reaction network from a steady state ideal in R =QQ[k_1..k_m,x_1..x_n].
    	    Note that the ideal needs to be unreduced with generator coming from a mass action dynamical system.
        Example
    	    R = QQ[k_1..k_6,x_1..x_5]
    	    I = ideal(k_2*x_2^2-k_3*x_1*x_3+k_6*x_2*x_5-k_1*x_1+k_4*x_4,
      	        -2*k_2*x_2^2-k_6*x_2*x_5+2*k_1*x_1+k_5*x_4,
       	        -k_3*x_1*x_3+k_6*x_2*x_5+k_4*x_4,
       	        k_3*x_1*x_3-k_4*x_4-k_5*x_4,
       	        -k_6*x_2*x_5+k_5*x_4)
    	    N = reactionNetwork I
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
        Text
	    {\bf Substitute ReactionRates}

	    The example below demonstrates how to substitute randomly generated values,
	    for the reaction rates, directly into the steady state equations. For specific
	    user-input values create a list with the corresponding values for each
	    reaction rate; to view the order: N.ReactionRates, where N is the name of
	    the reaction network.
	Example
	    M = reactionNetwork "A <--> 2B, A+C <--> D, D --> B+E, B+E --> A+C"
	    R = createRing M
	    K = toList(apply(0..length M.ReactionRates-1, i-> random(QQ)))
	    Rr = toList(apply(0..length M.ReactionRates-1, i-> value(M.ReactionRates#i)))
	    P = toList(apply(0..length Rr-1, i-> Rr#i=>sub(K#i,R)))
	    SSE = flatten entries steadyStateEquations M
	    toList apply(0..length SSE-1, i-> sub(SSE#i,P))
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
	Text
	    {\bf Substitute InitialValues}

	    The example below demonstrates how to substitute specific values for
	    the initial values in a reaction network. The list of desired values
	    must be input in the order of the initial values; for that order use
	    N.InitialValues, where N is the name of the reaction network.
	    To substitute random values refer to the example at the end of
	    @TO "steadyStateEquations" @.
	Example
	    N = reactionNetwork("A --> 2B, A + C --> D, D --> 0", NullSymbol => "0")
	    R = createRing N
	    CE = conservationEquations N
	    L = {1,2,3,4}
	    Iv = toList(apply(0..length N.InitialValues-1, i-> value(N.InitialValues#i)))
	    S=toList(apply(0..length Iv-1, i-> Iv#i=>L#i))
	    toList apply(0..length CE-1, i-> sub(CE#i,S))
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
	    catalyzed by the same phosphatase at both sites [Feliu, Wiuf].
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
	    catalyzing kinase [Feliu, Wiuf].
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
	    catalyzing kinase and a joint catalyzing phosphotase [Feliu, Wiuf].
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
	    layer [Feliu, Wiuf].
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
	    that is the same phosphotase acts in both layers [Feliu, Wiuf].
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
	    respectively [Feliu, Wiuf].
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
	    The canonical Wnt/beta-catenin signaling pathway is important for essential
	    cellular functions such as development, homeostasis, and is implicated in many
	    diseases [MacLean, Rosen, Byrne, Harrington]. The Wnt shuttle model includes
	    an abstraction of the signal transduction pathway
	    [Gross, Harrington, Rosen, Sturmfels].
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
	    Gross, Harrington, Rosen, Sturmfels.

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

doc ///
    Key
    	negativeLaplacian
	(negativeLaplacian,ReactionNetwork)
    Headline
    	Computes the negative of the Laplacian matrix of a Reaction Network.
    Usage
    	L = negativeLaplacian N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	L:Matrix
	  the negative of the unweighted Laplacian matrix of N.
    Description
        Text
	    Computes the negative of the (unweighted) Laplacian matrix of
    	    the directed graph associated with a Reaction Network.
    	Example
	    N = reactionNetwork "A <--> B"
	    L = negativeLaplacian N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    L = negativeLaplacian N
    SeeAlso
    	negativeUndirectedLaplacian
        negativeWeightedLaplacian
	stoichiometricMatrix
///

doc ///
    Key
    	negativeUndirectedLaplacian
	(negativeUndirectedLaplacian,ReactionNetwork)
    Headline
    	Computes the negative of the Laplacian matrix of an undirected graph associated with a Reaction Network.
    Usage
    	L = negativeUndirectedLaplacian N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	L:Matrix
	  the negative of the unweighted, undirected Laplacian matrix of N.
    Description
        Text
	    Computes the negative of the (unweighted) Laplacian matrix of
    	    the undirected graph associated with a Reaction Network.
    	Example
	    N = reactionNetwork "A <--> B"
	    L = negativeUndirectedLaplacian N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    L = negativeUndirectedLaplacian N
    SeeAlso
        negativeLaplacian
        negativeWeightedLaplacian
	stoichiometricMatrix
///

doc ///
    Key
    	negativeWeightedLaplacian
	(negativeWeightedLaplacian,ReactionNetwork)
    Headline
    	Computes the negative of the weighted Laplacian matrix of a Reaction Network.
    Usage
    	L = negativeWeightedLaplacian N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	L:Matrix
	  the negative of the weighted Laplacian matrix of N.
    Description
        Text
	    Computes the negative of the weighted Laplacian matrix of
    	    a Reaction Network.
    	Example
	    N = reactionNetwork "A <--> B"
	    L = negativeWeightedLaplacian N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    L = negativeWeightedLaplacian N
    SeeAlso
    	negativeUndirectedLaplacian
        negativeLaplacian
	stoichiometricMatrix
///

doc ///
    Key
        stoichiometricMatrix
	(stoichiometricMatrix,ReactionNetwork)
    Headline
    	Computes the stoichiometric matrix of a Reaction Network.
    Usage
    	S = stoichiometricMatrix N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	S:Matrix
	  the stoichiometric matrix of N.
    Description
        Text
	    Computes the stoichiometric matrix of
    	    a Reaction Network.
    	Example
	    N = reactionNetwork "A <--> B"
	    S = stoichiometricMatrix N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    S = stoichiometricMatrix N
    SeeAlso
        stoichiometricSubspace
	stoichSubspaceKer
	negativeLaplacian
	negativeWeightedLaplacian
///

doc ///
    Key
        reactantMatrix
	(reactantMatrix,ReactionNetwork)
    Headline
    	Computes the reactants matrix of a Reaction Network.
    Usage
    	RM = reactantMatrix N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	RM:Matrix
	   whose rows encode the educt complexes of N.
    Description
    	Example
	    N = reactionNetwork "A + 2B <--> 3A + 4B"
	    RM = reactantMatrix N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    RM = reactantMatrix N
    SeeAlso
    	stoichiometricMatrix
        stoichiometricSubspace
	stoichSubspaceKer
	negativeLaplacian
	negativeWeightedLaplacian
///

doc ///
    Key
        stoichiometricSubspace
	(stoichiometricSubspace,ReactionNetwork)
    Headline
    	Computes the stoichiometric space of a Reaction Network.
    Usage
    	S = stoichiometricSubspace N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	S:Matrix
	  whose columns span the stoichiometric space.
    Description
        Text
	    Computes the stoichiometric space of
    	    a Reaction Network.
    	Example
	    N = reactionNetwork "A <--> B"
	    S = stoichiometricSubspace N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    S = stoichiometricSubspace N
    SeeAlso
        stoichiometricMatrix
	stoichSubspaceKer
///

doc ///
    Key
        stoichSubspaceKer
	(stoichSubspaceKer,ReactionNetwork)
    Headline
    	Computes the kernel of the stoichiometric matrix of a Reaction Network.
    Usage
    	Z = stoichSubspaceKer N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	Z:Matrix
	  whose columns span the left kernel of the stoichiometric matrix.
    Description
        Text
	    Computes the left kernel of the stoichiometric matrix of
    	    a Reaction Network.
    	Example
	    N = reactionNetwork "A <--> B"
	    Z = stoichSubspaceKer N
	    S = stoichiometricMatrix N
	    K = ker transpose S
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    Z = stoichSubspaceKer N
	    S = stoichiometricMatrix N
	    K = ker transpose S
    SeeAlso
        stoichiometricMatrix
	stoichiometricSubspace
///

doc ///
    Key
    	createRing
	(createRing, ReactionNetwork)
	(createRing, ReactionNetwork, Ring)
    Headline
        creates a ring with generators species concentrations, reaction rates, initial values
///

doc ///
    Key
    	createConcentrationRates
    Headline
    	used in creating reaction network ring///

doc ///
    Key
    	createReactionRates
    Headline
    	used in creating reaction network ring///

doc ///
    Key
    	createInitialValues
    Headline
    	used in creating reaction network ring///

doc ///
    Key
    	isDeficient
    Headline
    	determines deficiency of a network
    Description
    	Text
	    Example
    	Example
    	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    isDeficient N
	    W = wnt()
	    isDeficient W
///

doc ///
    Key
    	isWeaklyReversible
    Headline
    	determines if a network is weakly reversible
    Description
    	Text
	    Example
        Example
    	    N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
	    isWeaklyReversible N

	    isWeaklyReversible wnt()
	///

-- here we have the documentation for the functions for the isolation property


doc ///
    Key
        stoichiometricConeKer
	(stoichiometricConeKer,ReactionNetwork)
    Headline
    	Computes a matrix whose columns are the rays of the cone non-negative kernel of the stoichiometric matrix of a Reaction Network.
    Usage
    	E = stoichiometricConeKer N
    Inputs
        N:ReactionNetwork
	  a chemical reaction network.
    Outputs
    	E:Matrix
	  whose columns are the non negative kernel of the stoichiometric matrix of N.
    Description
        Text
	    Computes the cone non-negative kernel of the stoichiometric matrix of N and puts its rays as columns of a matrix.
    	Example
	    N = reactionNetwork "A <--> B"
	    S = stoichiometricMatrix N
	    E = stoichiometricConeKer N
	Text
	    A bigger example:
	Example
	    N = oneSiteModificationA()
	    S = stoichiometricMatrix N
	    E = stoichiometricConeKer N
    SeeAlso
    	stoichiometricMatrix
        stoichiometricSubspace
	stoichSubspaceKer
	negativeLaplacian
	negativeWeightedLaplacian
///





    --"reactionNetwork", "ReactionNetwork", "Species", "Complexes", "NullSymbol", "NullIndex", "ReactionGraph",
    --"stoichiometricSubspace",
    --"steadyStateEquations", "conservationEquations",
    --"laplacian", "FullEdges", "NullEdges" --, "netComplex", "networkToHRF", "glue"
    --"ReactionRing", "ParameterRing",
