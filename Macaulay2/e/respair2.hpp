// Copyright 1996.  Michael E. Stillman

#ifndef _respair_hh_
#define _respair_hh_

struct res2term;

// The following are the possible types of res_pairs's
enum
{
  SYZ2_S_PAIR,			// Pre computation: s-pair

  SYZ2_MAYBE_MINIMAL,		// This s-pair was computed before all of the 
				// pairs in the next level were computed, meaning that
				// this element may not correspond to a minimal syz.
				// Instead, it may be a SYZ2_NOT_MINIMAL.
  SYZ2_MINIMAL,			// Post s-pair computation: element is minimal syzygy
  SYZ2_NOT_MINIMAL,		// Post s-pair computation: element is not minimal
  SYZ2_NOT_NEEDED		// S-pair computation for this pair cancelled
};

struct res2_pair : public our_new_delete
{
  res2_pair *next;		// Next pair in the list
  unsigned int me;		// Used for making the matrices, either minimal or not.
  unsigned int pair_num;	// A sequence number for which pair this is.
				// Used in division to determine which pair to reduce by.
  unsigned char syz_type;
  unsigned char level;
  unsigned short degree;
  unsigned int compare_num;	// We don't need a full 32 (or 64) bit number for this...

  res2term *syz;			// The syzygy itself, once computed
				// Before being computed, this is the 1 or 2 term syzygy
				// saying what the syzygy is.

  MonomialIdeal * mi;
#if 0
//   union {
//     MonomialIdeal mi;		// Monomial ideal of total monomials
//     struct {			// Maybe: want a pointer to this...?
//       res2_pair *mi2;		// List of res_pairs having this as lead term
//       res2_pair *next_mi;	// If this is part of a list of mi2, this is the
// 				// next-link.
//       int monomial_mask;	// Do we really want this?  If so: should probably
// 				// refer to the non-total monomial.
//     }
//   } monideal;
#endif
  // The following are used only for minimalization of the resolution
  res2term *pivot_term;		// SYZ2_NOT_MINIMAL: Points into 'syz', to the 
				// term containing the constant.
				// Is this always the last term??
				// If so, we probably don't need this field...
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
