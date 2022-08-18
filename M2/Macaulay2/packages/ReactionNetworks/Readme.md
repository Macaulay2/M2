TO DO List

October 2016

*  Currently:  There is a ReactionRing stored in reactionNetwork hash
	table. It is created using createRing and it is of the form
	FF[cc][kk][xx], where cc are parameters used in the stoichiometric
	equations creation, kk are the reaction rates used in the creation of
	the steady-state equations, and xx are the concentrations.

*  We need:  The parameters cc and kk to have options for user input and
random. How to create this and update the ReactionRing?
 * This is not necessary. Let's have a unique master ring; let's also flatten the ring, i.e., have FF[cc,kk,xx]
 * Create an auxiliary function specialize(variables, values), which creates e.g.
 map(FF[cc,xx],FF[cc,kk,xx],...)
 that the user may then apply to anything.

*  (I don't understand how this is different from keeping the kk's as
   unknown parameters:  We also want the option of grouping the parameters kk with the
   variables xx, thus making kk variables as well.)

*  Once the ReactionRing is fixed there should be no more changes that
   will affect the examples we use in MonodromySolver. The rest of
   the functions are auxiliary. 


* Create isInjective function
* Create reactionMatrix, reactantMatrix, stoichiometricMatrix
* Fix laplacian function
* Create A matrix
* Can we have random, user, and parameter kk's and cc's?
* Can we group the kk's with the xx's, i.e. make them variables?
* Create examples that demonstrate how to treat the cc's and kk's
* Create several examples not within documentation but as
stand-alone.
* Update documentation and add new functions



Summer 2016

* Basic Functions
 * Constructors (parsing non-HR formats) (Tim)
 * Print complexes with labels, i.e. 1->A+B, 2->C+2D,...   (Chill)
 * (maybe) Higher level service functions (elimination, etc)
 * Accessors for Laplacian, other features (Tim)


* Examples
 * Add WNT Shuttle Model (from Elizabeth's Talk) (Chill)
 * Are there other models to add?
 * Examples of solving CRN eqns (all)
 * Elimination, degeneration
 * High-dimensional solution varieties
 * Examples using monodromy technique, mixed volumes 

* Operations
 * Removing vs eliminating for rxns/comps/species?
 * Removing species? 
 * Other manips? What do they mean?

* Equations
 * Options for parameters: random values, etc 
 * Observed variables (don't remember what this means, ask E)

* Higher level 
 * Multistationarity
 * Siphons

* Finishing Touches?
 * Documentation
