
struct s_degree
    // Collection of Ns_pairs's all of the same degree
{
  s_degree *next;
  int *this_deg;
  s_pair *first;
  s_pair *first_gen;
  int num_pairs;
  int num_left;

  s_degree() : next(NULL), first(NULL), first_gen(NULL), num_pairs(0), num_left(0) {}
  ~s_degree();

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class s_pair_set
{
  const monoid *M;
  const PolynomialRing *R;
  const FreeModule *F;
  int n_degrees;


  array<s_degree *> spairs;
  s_degree *this_degree;
  int next_compare_num;

  int *multi_degree(const s_pair *q);
  s_degree *next_deg(const int *deg);
  int degree_set_ok(const s_degree *p, const int *deg) const;

  int compare_pairs(s_pair *f, s_pair *g) const;
  s_pair *merge_pairs(s_pair *f, s_pair *g) const;
  void sort_pairs(s_pair *&p) const;
  
public:
  s_pair_set();
  ~s_pair_set();

  int next_degree(const int *stop_degree); // Set up to handle the next degree
					   // <= than the given one.
					   // Return 1 if there is such a degree
  void insert(s_pair *p);	  // Handles generators as well
  s_pair *remove();		  // Remove the smallest spair in the current degree.
				  // Return NULL if no more in this degree.
  void flush();			  // Remove (and delete) all pairs in this degree

  void stats() const;
};
//////////////////////////////////////////////
//  Degree control ///////////////////////////
//////////////////////////////////////////////

int * s_pair_set::multi_degree(const s_pair *q) const
{
  int *result = degree_monoid()->make_one();
  M->multi_degree(q->lcm, result);
  if (q->f != NULL)
    degree_monoid()->mult(result, F->degree(q->f->comp), result);
  else if (q->first != NULL && q->first->f != NULL)
    degree_monoid()->mult(result, F->degree(q->first->f->comp), result);
  else
    assert(0);
  return result;
}

const int *s_pair_set::current_degree() const
{
  if (this_degree == NULL) return NULL;
  return this_degree->this_deg;
}

int s_pair_set::degree_set_ok(const s_degree *p, const int * /*deg*/) const
{
  // Check that 'p' has elements in it.
  // Later we must make sure that this element is in the
  // positive cone of possible weights.
  return p->num_left > 0;
}


static int ints_same(int n, const int *a, const int *b)
{
  for (int i=0; i<n; i++)
    if (*a++ != *b++) return 0;
  return 1;
}

s_degree *s_pair_set::get_degree_set(int *&deg)
     // Find the given degree_set, if it exists.
     // If it doesn't, create it, and return it.
     // Consume 'deg'
{
  int d = deg[0] - lowest_degree;
  int e = d - s_pairs.length();
  if (e >= 0)
    for (int i=0; i<e; i++)
      s_pairs.append((s_degree *)NULL);

  if (s_pairs[d] == NULL)
    s_pairs[d] = new s_degree;

  s_degree *p;
  for (p = s_pairs[d]; p->next != NULL; p = p->next)
    if (ints_same(n_degrees, deg, p->next->this_deg))
      {
	degree_monoid()->remove(deg);
	return p->next;
      }

  p->next = new s_degree;
  p->next->this_deg = deg;
  return p->next;
}

//////////////////////////////////////////////
//  Sorting //////////////////////////////////
//////////////////////////////////////////////

int s_pair_set::compare_pairs(s_pair *f, s_pair *g) const
{
  int cmp = M->compare(f->lcm, g->lcm);
  if (cmp != 0) return cmp;
  cmp = f->first->compare_num - g->first->compare_num;
  if (cmp < 0) return 1;
  if (cmp > 0) return -1;
  return 0;
}

s_pair *s_pair_set::merge_pairs(s_pair *f, s_pair *g) const
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  s_pair head;
  s_pair *result = &head;
  while (1)
    switch (compare_pairs(f, g))
      {
      case 1:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case -1:
      case 0:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
      }
}

void s_pair_set::sort_pairs(s_pair *& p) const
{
  if (p == NULL || p->next == NULL) return;
  s_pair *p1 = NULL;
  s_pair *p2 = NULL;
  while (p != NULL)
    {
      s_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_pairs(p1);
  sort_pairs(p2);
  p = merge_pairs(p1, p2);
}

//////////////////////////////////////////////
//  Visible interface ////////////////////////
//////////////////////////////////////////////

s_pair_set::s_pair_set()
{
  n_degrees = degree_monoid()->n_vars();
  next_compare_num = 0;
  // MES
}

s_pair_set::~s_pair_set()
{
  // MES
}

int s_pair_set::next_degree(const int *stop_degree)
{
  // Step 1: what is the lowest degree which has work to do?
  int top = s_pairs.length();
  if (stop_degree != NULL && stop_degree[0] < top+lowest_degree)
    top = stop_degree[0] - lowest_degree;

  for (int i=0; i<top; i++)
    {
      s_degree *p = s_pairs[i];
      if (p == NULL) continue;
      if (p->next != NULL)
	this_degree = p;
    }

  // Step 2: sort the elements in this degree
  sort_pairs(this_degree->first);
  sort_pairs(this_degree->first_gen);

  // Step 3: set the 'compare_num' fields. ?? is this needed anymore ??
  
  // Step 4: check whether resizing is needed.  If so, return XXX.
  return NULL;

#if 0
int s_pair_set::next_degree(const int *stop_degree)
{
  this_degree = next_deg(stop_degree);
  if (
}

  if (comp_printlevel >= 1)
    {
      degree_monoid()->elem_text_out(cerr, current_degree());
      cerr << '(' << this_degree->num_pairs << ')';
    }

  // MES: is this following line the correct condition?
  if (M->max_degree() < current_degree()[0] - lowest_degree)
    return GB_COMP_RESIZE_MONOMIALS;
#endif
}

void s_pair_set::insert(s_pair *p)
{
  // First we must determine the multi-degree,
  // and then find the correct s_degree,
  // and finally place it onto the list.

  int *deg = multi_degree(p);
  int firstdeg = deg[0];
  if (firstdeg < lowest_active_deg)
    lowest_active_deg = firstdeg;
  s_degree *q = get_degree_set(deg);
  degree_monoid()->remove(deg);
  p->next = q->first;

  q->num_pairs++;
  q->num_left++;
  if (p->syz_type == PAIR_GEN)
    q->first_gen = p;
  else
    {
      q->first = p;
      q->is_sorted = 0;
    }
  n_pairs++;
}

s_pair *s_pair_set::remove()
{
  if (this_degree != NULL)
    {
      if (this_degree->first != NULL)
	{
	  s_pair *result = this_degree->first;
	  this_degree->first = result->next;
	  result->next = NULL;
	  return result;
	}
      if (this_degree->first_gen != NULL)
	{
	  s_pair *result = this_degree->first_gen;
	  this_degree->first_gen = result->next;
	  result->next = NULL;
	  return result;
	}
      // now remove this degree set, since it is empty
      remove(this_degree);
      this_degree = NULL;
    }
  return NULL;


  this_degree = next_degree_set(deg);
  if (this_degree == NULL) return NULL;
  return remove(deg);
}

void s_pair_set::flush(const int *deg)
    // Remove (and delete) all pairs in this degree
{
  // MES
}

void s_pair_set::stats()
{
  // MES

  int i;
  int total_pairs = 0;
  int total_left = 0;
  for (i=0; i<s_pairs.length(); i++)
    {
      Ns_degree *p = s_pairs[i];
      if (p == NULL) continue;
      while ((p = p->next) != NULL)
	{
	  degree_monoid()->elem_text_out(cerr, p->this_deg);
	  cerr << '\t'  << p->num_pairs << '\t' << p->num_left << endl;
	  total_pairs += p->num_pairs;
	  total_left += p->num_left;
	}
    }
  cerr << "------------------------------------------" << endl;
  cerr << '\t' << total_pairs << '\t' << total_left << endl << endl;

  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    for (i=0; i<s_pairs.length(); i++)
      {
	Ns_degree *p = s_pairs[i];
	if (p == NULL) continue;
	while ((p = p->next) != NULL)
	  {
	    cerr << "---- degree ";
	    degree_monoid()->elem_text_out(cerr, p->this_deg);
	    cerr << "----"  << p->num_pairs << " ---- " 
	      << p->num_left << " ----" << endl;
	    for (s_pair *q = p->first; q!=NULL; q=q->next)
	      debug_out(q);
	  }
	
      }

}
