-- Two-site Modification G
twoSiteModificationG = method()
installMethod(twoSiteModificationG, () -> reactionNetwork {"S_0+E <--> X_1", "X_1 --> S_1+E",
	                                                   "S_1+E <--> X_2", "X_2 --> S_2+E",
							   "S_1+F <--> Y_1", "Y_1 --> S_0+F", 
							   "S_2+F <--> Y_2", "Y_2 --> S_1+F"}
				                           )

twoSiteModificationG List := G -> ( 
    -- Alphabet is listed in the following order:
    -- G = {S_0=>G1, E=>G2, X_1=>G3, S_1=>G4, X_2=>G5, F=>G6, S_2=>G7, Y_1=>G8, Y_2=>G9}
    Re := {"G1+G2 <--> G3", "G3 --> G1+G2",
	   "G4+G2 <--> G5", "G5 --> G7+G2", 
	   "G4+G6 <--> G8", "G8 --> G1+G6", 
	   "G7+G6 <--> G9", "G9 --> G4+G6"};
       for i from 1 to 9 do Re = apply(
	       	       	       	   Re, s-> replace(concatenate("G", toString(i)), G #(i-1),s)
				   );
       RS := reactionNetwork Re;
       RS
       )


-- Modification of Two Substrates H
modificationOfTwoSubstratesH = method()
installMethod(modificationOfTwoSubstratesH, () -> reactionNetwork { 
                                                   "S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	    	    	                   "S_1+F_1 <--> Y_1", "Y_1 --> S_0+F_1",
				                   "P_0+E <--> X_2", "X_2 --> P_1+E",
				                   "P_1+F_2 <--> Y_2", "Y_2 --> P_0+F_2"}
					       )
			    
modificationOfTwoSubstratesH List := H -> (
    -- H = {S_0=>H1, E=>H2, X_1=>H3, S_1=>H4, F_1=>H5, Y_1=>H6, P_0=>H7, 
    --      X_2=>H8, P_1=>H9, F_2=>H10, Y_2=>H11}
    Re := {"H1+H2 <--> H3", "H3 --> H4+H2",
	   "H4+H5 <--> H6", "H6 --> H1+H5",
	   "H7+H2 <--> H8", "H8 --> H9+H2",
	   "H9+H10 <--> H11", "H11 --> H7+H10"};
       for i from 1 to 11 do Re = apply(
	       	       	       	   Re, s-> replace(concatenate("H", toString(12-i)), H #(11-i),s)
				   );
       RS := reactionNetwork Re;
       RS
       )


-- Modification of Two Substrates I
modificationOfTwoSubstratesI = method()
installMethod(modificationOfTwoSubstratesI, () -> reactionNetwork {
	    	    	    	    	    	   "S_0+E <--> X_1", "X_1 --> S_1+E",
                                		   "S_1+F <--> Y_1", "Y_1 --> S_0+F", 
						   "P_0+E <--> X_2", "X_2 --> P_1+E",
						   "P_1+F <--> Y_2", "Y_2 --> P_0+F"}
					       )
modificationOfTwoSubstratesI List := I -> (
    -- I = {S_0=>I1, E=>I2, X_1=>I3, S_1=>I4, F=>I5, Y_1=>I6, P_0=>I7, 
    --      X_2=>I8, P_1=>I9, Y_2=>I10}
    Re := {"I1+I2 <--> I3", "I3 --> I4+I2",
           "I4+I5 <--> I6", "I6 --> I1+I5", 
	   "I7+I2 <--> I8", "I8 --> I9+I2",
	   "I9+I5 <--> I10", "I10 --> I7+I5"};
       for i from 1 to 10 do Re = apply(
	       	       	       	   Re, s-> replace(concatenate("I", toString(11-i)), I#(10-i),s)
				   );
       RS := reactionNetwork Re;
       RS
       )


-- Two Layer Cascade J
twoLayerCascadeJ = method()
installMethod(twoLayerCascadeJ, () -> reactionNetwork {
	                                           "S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	                                   "S_1+F_1 <--> Y_1", "Y_1 --> S_0+F_1",
		                                   "P_0+S_1 <--> X_2", "X_2 --> P_1+S_1",
		                                   "P_1+F_2 <--> Y_2", "Y_2 --> P_0+F_2"}
					       )
twoLayerCascadeJ List := J -> (
    -- J = {S_0=>J1, E=>J2, X_1=>J3, S_1=>J4, F_1=>J5, Y_1=>J6, P_0=>J7, X_2=>J8, 
    -- P_1=>J9, F_2=>J10, Y_2=>J11}
    Re := {"J1+J2 <--> J3", "J3 --> J4+J2",
    	   "J4+J5 <--> J6", "J6 --> J1+J5",
	   "J7+J4 <--> J8", "J8 --> J9+J4",
	   "J9+J10 <--> J11", "J11 --> J7+J10"};
       for i from 1 to 11 do Re = apply(
	                          Re, s-> replace(concatenate("J", toString(12-i)), J#(11-i),s)
				   );
       RS := reactionNetwork Re;
       RS
       )


-- Two Layer Cascade K
twoLayerCascadeK = method()
installMethod(twoLayerCascadeK, () -> reactionNetwork {
	    	    	    	    	    	    "S_0+E <--> X_1", "X_1 --> S_1+E",
    	    	                                    "S_1+F <--> Y_1", "Y_1 --> S_0+F",
		                                    "P_0+S_1 <--> X_2", "X_2 --> P_1+S_1",
		                                    "P_1+F <--> Y_2", "Y_2 --> P_0+F"}  
						)
twoLayerCascadeK List := K -> (
    -- K = {S_0=>K1, E=>K2, X_1=>K3, S_1=>K4, F=>K5, Y_1=>K6, P_0=>K7, X_2=>K8, 
    -- P_1=>K9, Y_2=>K10}
    Re := {"K1+K2 <--> K3", "K3 --> K4+K2",
	   "K4+K5 <--> K6", "K6 --> K1+K5",
	   "K7+K4 <--> K8", "K8 --> K9+K4",
	   "K9+K5 <--> K10", "K10 --> K7+K5"};
       for i from 1 to 10 do Re = apply(
	       	       	       	  Re, s -> replace(concatenate("K", toString(11-i)), K#(10-i),s)
				  );
       RS := reactionNetwork Re;
       RS
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
clusterModelCellDeath List := M -> (
    -- M = {Y=>M1, Z=>M2, L=>M3}
    Re := {"M1+M2 --> 2M3", "M3+M1+M2 --> M3+2M2",
	   "2M1 --> M3+M2", "M3+2M1 --> M3+M1+M2",
	   "2M1 --> 2M2", "M3+2M1 --> M3+2M2",
	   "M1+2M2 --> 3M2", "M3+M1+2M2 --> M3+3M2",
	   "2M1+M2 --> M1+2M2", "M3+2M1+M2 --> M3+M1+2M2",
	   "2M1+M2 --> 3M2", "M3+2M1+M2 --> M3+3M2",
	   "3M1 --> 2M1+M2", "M3+3M1 --> M3+2M1+M2",
	   "3M1 --> M1+2M2", "M3+3M2 --> M3+M1+2M2",
	   "3M1 --> 3M2", "M3+3M1 --> M3+3M2"};
       for i from 1 to 3 do Re = apply(
	       	       	       	 Re, s -> replace(concatenate("M", toString(4-i)), M#(3-i),s)
				 );
       RS := reactionNetwork Re;
       RS
       )			    



-- Examples

-- A = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"}
-- B = {"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}	
-- C = {"a", "b", "c", "d", "e", "f", "g", "h", "i"}
-- D = {"a", "b", "c"}

-- twoSiteModificationG(C)
-- modificationOfTwoSubstratesH(A)
-- modificationOfTwoSubstratesI(B)
-- twoLayerCascadeJ(A)
-- twoLayerCascadeK(B)
-- clusterModelCellDeath(D)
