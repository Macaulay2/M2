#include "EEhilbtrack.hpp"
#include "matrix.hpp"
#include "hilb.hpp"
////////////////////////////////
// Hilbert function computing //
////////////////////////////////

HilbertTracker::HilbertTracker(const RingElement &hf_orig, const FreeModule *F)
  : hf_orig(hf_orig),
    lead_terms(Matrix(F))
{
  HRing = F->get_ring()->HilbertRing();
  needs_recomputing = true;
  n_left = -1;
}

HilbertTracker::~HilbertTracker()
{
  // Nothing to do.
}

bool HilbertTracker::update(int deg)
  // Computes the value for _n_left.  If interrupted, false
  // is returned.
{
  if (needs_recomputing) 
    {
      hilb_comp *hf_comp = new hilb_comp(HRing, lead_terms);
      int retval = hf_comp->calc(-1);
      if (retval != COMP_DONE) return false;
      RingElement hf = hf_comp->value();
      delete hf_comp;
      needs_recomputing = false;
      hf_diff = hf_orig - hf;
    }

  n_left = hilb_comp::coeff_of(hf_diff, deg);
  return true;
}

void HilbertTracker::increment(vec v)
  // Places the lead term v into a matrix, and decrements _n_left.
{
  vec lt = lead_terms.rows()->lead_term(v);
  lead_terms.append(lt);
  n_left--;
}

