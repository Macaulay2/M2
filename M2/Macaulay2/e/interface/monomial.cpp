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
