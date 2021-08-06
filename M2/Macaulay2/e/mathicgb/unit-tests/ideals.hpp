// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_IDEALS_GUARD
#define MATHICGB_IDEALS_GUARD

#include <string>

namespace mgb {}

// small
std::string smallIdealComponentLastDescending();

extern const char* idealSmallBasis;
extern const char* idealSmallSyzygies;
extern const char* idealSmallInitial;

// liu
std::string liuIdealComponentLastDescending();

extern const char* liu_gb_strat0_free1;
extern const char* liu_syzygies_strat0_free1;
extern const char* liu_initial_strat0_free1;

// weispfennig97
std::string weispfennig97IdealComponentLast(bool componentsAscending);

extern const char* weispfennig97_gb_strat0_free4;
extern const char* weispfennig97_syzygies_strat0_free4;
extern const char* weispfennig97_initial_strat0_free4;

extern const char* weispfennig97_gb_strat0_free5;
extern const char* weispfennig97_syzygies_strat0_free5;
extern const char* weispfennig97_initial_strat0_free5;

// gerdt93
std::string gerdt93IdealComponentFirst(bool componentsAscending);
std::string gerdt93IdealComponentMiddle(bool componentsAscending);
std::string gerdt93IdealComponentLast(bool componentsAscending, bool schreyer);

extern const char* gerdt93_gb_strat0_free1;
extern const char* gerdt93_syzygies_strat0_free1;
extern const char* gerdt93_initial_strat0_free1;

extern const char* gerdt93_gb_strat0_free2;
extern const char* gerdt93_syzygies_strat0_free2;
extern const char* gerdt93_initial_strat0_free2;

extern const char* gerdt93_gb_strat0_free3;
extern const char* gerdt93_syzygies_strat0_free3;
extern const char* gerdt93_initial_strat0_free3;

extern const char* gerdt93_gb_strat0_free4;
extern const char* gerdt93_syzygies_strat0_free4;
extern const char* gerdt93_initial_strat0_free4;

extern const char* gerdt93_gb_strat0_free5;
extern const char* gerdt93_syzygies_strat0_free5;
extern const char* gerdt93_initial_strat0_free5;

extern const char* gerdt93_gb_strat0_free6;
extern const char* gerdt93_syzygies_strat0_free6;
extern const char* gerdt93_initial_strat0_free6;

extern const char* gerdt93_gb_strat0_free7;
extern const char* gerdt93_syzygies_strat0_free7;
extern const char* gerdt93_initial_strat0_free7;

#endif
