#include "interface/monomial.h"

#include "error.h"
#include "exceptions.hpp"

EngineMonomial *rawMakeMonomial(M2_arrayint m)
{
  try {
    return EngineMonomial::make(m);
  } catch (const exc::engine_error& e) {
    ERROR(e.what());
    return nullptr;
  }
}

EngineMonomial *rawMakeMonomialFromExponents(M2_arrayint exp)
{
  try {
    varpower::Vector vp;
    varpower::from_expvector(exp->len, exp->array, vp);
    return EngineMonomial::make(vp.data());
  } catch (const exc::engine_error &e) {
    ERROR(e.what());
    return nullptr;
  }
}
