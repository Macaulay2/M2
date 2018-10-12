export{"twoSiteModificationG", "modificationOfTwoSubstratesH", "modificationOfTwoSubstratesI",
    	"twoLayerCascadeJ", "twoLayerCascadeK", "clusterModelCellDeath", "wnt"}

-- Two-site Modification G
twoSiteModificationG = method()
installMethod(twoSiteModificationG, () -> reactionNetwork {"S_0+E <--> X_1", "X_1 --> S_1+E",
	                                                   "S_1+E <--> X_2", "X_2 --> S_2+E",
							   "S_1+F <--> Y_1", "Y_1 --> S_0+F", 
							   "S_2+F <--> Y_2", "Y_2 --> S_1+F"}
				                           )

-- Modification of Two Substrates H
modificationOfTwoSubstratesH = method()
installMethod(modificationOfTwoSubstratesH, () -> reactionNetwork { 
                                                   "S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	    	    	                   "S_1+F_1 <--> Y_1", "Y_1 --> S_0+F_1",
				                   "P_0+E <--> X_2", "X_2 --> P_1+E",
				                   "P_1+F_2 <--> Y_2", "Y_2 --> P_0+F_2"}
					       )

-- Modification of Two Substrates I
modificationOfTwoSubstratesI = method()
installMethod(modificationOfTwoSubstratesI, () -> reactionNetwork {
	    	    	    	    	    	   "S_0+E <--> X_1", "X_1 --> S_1+E",
                                		   "S_1+F <--> Y_1", "Y_1 --> S_0+F", 
						   "P_0+E <--> X_2", "X_2 --> P_1+E",
						   "P_1+F <--> Y_2", "Y_2 --> P_0+F"}
					       )

-- Two Layer Cascade J
twoLayerCascadeJ = method()
installMethod(twoLayerCascadeJ, () -> reactionNetwork {
	                                           "S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	                                   "S_1+F_1 <--> Y_1", "Y_1 --> S_0+F_1",
		                                   "P_0+S_1 <--> X_2", "X_2 --> P_1+S_1",
		                                   "P_1+F_2 <--> Y_2", "Y_2 --> P_0+F_2"}
					       )

-- Two Layer Cascade K
twoLayerCascadeK = method()
installMethod(twoLayerCascadeK, () -> reactionNetwork {
	    	    	    	    	    	    "S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	                                    "S_1+F <--> Y_1", "Y_1 --> S_0+F",
		                                    "P_0+S_1 <--> X_2", "X_2 --> P_1+S_1",
		                                    "P_1+F <--> Y_2", "Y_2 --> P_0+F"}  
						)

-- Cluster Model for Cell Death
clusterModelCellDeath = method()
installMethod(clusterModelCellDeath, () -> reactionNetwork {
	                                            "Y+Z --> 2Z", "L+Y+Z --> L+2Z",
	    	    	    	                    "2Y --> Y+Z", "L+2Y --> L+Y+Z",
				                    "2Y --> 2Z", "L+2Y --> L+2Z",
				                    "Y+2Z --> 3Z", "L+Y+2Z --> L+3Z",
				                    "2Y+Z --> Y+2Z", "L+2Y+Z --> L+Y+2Z",
				                    "2Y+Z --> 3Z", "L+2Y+Z --> L+3Z",
				                    "3Y --> 2Y+Z", "L+3Y --> L+2Y+Z",
				                    "3Y --> Y+2Z", "L+3Y --> L+Y+2Z",
				                    "3Y --> 3Z", "L+3Y --> L+3Z"}
						)

-- Shuttle Model for Wnt Signaling Pathway
wnt = method()
installMethod(wnt, () -> reactionNetwork ({"X_1 <--> X_2", "X_2+X_4 <--> Y_4",
					  "Y_4 --> X_2+X_5", "X_5+X_8 <--> Y_6",
					  "Y_6 --> X_4+X_8", "X_4+Y_0 <--> Y_8",
					  "Y_8 --> X_4", "0 --> Y_0", 
				          "Y_0 --> 0", "X_3+X_6 <--> Y_5",
					  "Y_5 --> X_3+X_7", "X_7+X_9 <--> Y_7",
					  "Y_7 --> X_6+X_9", "X_6+Y_1 <--> Y_9",
					  "Y_9 --> X_6", "Y_1 --> 0", 
					  "Y_1+Y_2 <--> Y_3", "X_2 <--> X_3",
					  "X_5 <--> X_7", "Y_0 <--> Y_1"}, NullSymbol => "0")
						)
TEST ///
restart
needsPackage "ReactionNetworks"
netList steadyStateEquations twoLayerCascadeK()
W = wnt()
#W.Species
#W.Complexes
VerticalList steadyStateEquations wnt()
W.NullIndex
N = clusterModelCellDeath()
sub(N, {"Y" => "A", "L" => "B", "Z" => "C"})
///					    


TEST ///
restart 
needsPackage "ReactionNetworks"
needsPackage "Graphs"
N = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B+E"
S = stoichiometricSubspace N
N.Species
N.Complexes
N.ReactionGraph
SSE = steadyStateEquations N
///
