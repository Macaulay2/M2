#ifndef _NAG_h_
#  define _NAG_h_

#  include "engine-includes.hpp"

// TODO: fix this
#  if defined(__cplusplus)
class M2SLEvaluator;
class M2Homotopy;
class M2SLProgram;
class StraightLineProgram;
class PathTracker;
class M2PointArray;
#  else
typedef struct M2SLEvaluator M2SLEvaluator;
typedef struct M2Homotopy M2Homotopy;
typedef struct M2SLProgram M2SLProgram;
typedef struct StraightLineProgram StraightLineProgram;
typedef struct PathTracker PathTracker;
typedef struct M2PointArray M2PointArray;
#  endif

/**
   Numerical Algebraic Geometry interface routines
 */

#  if defined(__cplusplus)
extern "C" {
#  endif

M2Homotopy /* or null */ *rawHomotopy(M2SLEvaluator *Hx,
                                      M2SLEvaluator *Hxt,
                                      M2SLEvaluator *HxH);
M2SLEvaluator /* or null */ *rawSLEvaluator(M2SLProgram *SLP,
                                            M2_arrayint constsPos,
                                            M2_arrayint varsPos,
                                            const MutableMatrix *consts);
M2SLEvaluator /* or null */ *rawSLEvaluatorSpecialize(
    M2SLEvaluator *H,
    const MutableMatrix *parameters);
M2SLProgram /* or null */ *rawSLProgram(unsigned long nConstantsAndInputs);
M2_string rawSLEvaluatorToString(M2SLEvaluator *); /* connected */
M2_bool rawSLEvaluatorEvaluate(M2SLEvaluator *sle,
                               const MutableMatrix *inputs,
                               MutableMatrix *outputs);
M2_string rawHomotopyToString(M2Homotopy *);      /* connected */
M2_string rawSLProgramToString(M2SLProgram *);    /* connected */
unsigned int rawSLEvaluatorHash(M2SLEvaluator *); /* connected */
unsigned int rawHomotopyHash(M2Homotopy *);       /* connected */
unsigned int rawSLProgramHash(M2SLProgram *);     /* connected */

M2_bool rawHomotopyTrack(M2Homotopy *H,
                         const MutableMatrix *inputs,
                         MutableMatrix *outputs,
                         MutableMatrix *output_extras,
                         gmp_RR init_dt,
                         gmp_RR min_dt,
                         gmp_RR epsilon,  // o.CorrectorTolerance,
                         int max_corr_steps,
                         gmp_RR infinity_threshold,
                         M2_bool checkPrecision);

gmp_ZZ rawSLPInputGate(M2SLProgram *S);
gmp_ZZ rawSLPSumGate(M2SLProgram *S, M2_arrayint a);
gmp_ZZ rawSLPProductGate(M2SLProgram *S, M2_arrayint a);
gmp_ZZ rawSLPDetGate(M2SLProgram *S, M2_arrayint a);
gmp_ZZ rawSLPsetOutputPositions(M2SLProgram *S, M2_arrayint a);
gmp_ZZ rawSLPDivideGate(M2SLProgram *S, M2_arrayint a);

StraightLineProgram /* or null */ *rawSLP(const Matrix *consts,
                                          M2_arrayint program);
const Matrix /* or null */ *rawEvaluateSLP(StraightLineProgram *SLP,
                                           const Matrix *vals);
M2_string rawStraightLineProgramToString(StraightLineProgram *); /* connected */
unsigned int rawStraightLineProgramHash(StraightLineProgram *);  /* connected */
const Matrix /* or null */ *rawTrackPaths(StraightLineProgram *slp_pred,
                                          StraightLineProgram *slp_corr,
                                          const Matrix *start_sols,
                                          M2_bool is_projective,
                                          gmp_RR init_dt,
                                          gmp_RR min_dt,
                                          gmp_RR max_dt,
                                          gmp_RR dt_increase_factor,
                                          gmp_RR dt_decrease_factor,
                                          int num_successes_before_increase,
                                          gmp_RR epsilon,
                                          int max_corr_steps,
                                          int pred_type);

PathTracker /* or null */ *rawPathTrackerPrecookedSLPs(
    StraightLineProgram *slp_pred,
    StraightLineProgram *slp_corr);
PathTracker /* or null */ *rawPathTracker(const Matrix *);
PathTracker /* or null */ *rawPathTrackerProjective(const Matrix *,
                                                    const Matrix *,
                                                    gmp_RR);
M2_string rawPathTrackerToString(PathTracker *); /* connected */
unsigned int rawPathTrackerHash(PathTracker *);  /* connected */
void rawSetParametersPT(PathTracker *PT,
                        M2_bool is_projective,
                        gmp_RR init_dt,
                        gmp_RR min_dt,
                        gmp_RR dt_increase_factor,
                        gmp_RR dt_decrease_factor,
                        int num_successes_before_increase,
                        gmp_RR epsilon,
                        int max_corr_steps,
                        gmp_RR end_zone_factor,
                        gmp_RR infinity_threshold,
                        int pred_type);
void rawLaunchPT(PathTracker *PT, const Matrix *start_sols);
const Matrix /* or null */ *rawGetSolutionPT(PathTracker *PT, int solN);
const Matrix /* or null */ *rawGetAllSolutionsPT(PathTracker *PT);
int rawGetSolutionStatusPT(PathTracker *PT, int solN);
int rawGetSolutionStepsPT(PathTracker *PT, int solN);
gmp_RRorNull rawGetSolutionLastTvaluePT(PathTracker *PT, int solN);
gmp_RRorNull rawGetSolutionRcondPT(PathTracker *PT, int solN);
const Matrix /* or null */ *rawRefinePT(PathTracker *PT,
                                        const Matrix *sols,
                                        gmp_RR tolerance,
                                        int max_corr_steps_refine);

// M2PointArray
unsigned int rawPointArrayHash(M2PointArray *);
M2_string rawPointArrayToString(M2PointArray *);
M2PointArray /* or null */ *rawPointArray(double epsilon, int n);
int rawPointArrayLookup(M2PointArray *pa, const MutableMatrix *M, int col);
int rawPointArrayLookupOrAppend(M2PointArray *pa,
                                const MutableMatrix *M,
                                int col);

#  if defined(__cplusplus)
}
#  endif

#endif /* _NAG_h_ */

// Local Variables:
// indent-tabs-mode: nil
// End:
