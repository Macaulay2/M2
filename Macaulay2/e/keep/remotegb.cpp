
#include "remotegb.hh"

remote_gbde::remote_gbde(const char */*machine_name*/,/* e.g."pauli.ruph.cornell.edu" */
              const char */*username*/,
	      const char */*password*/, /* don't leave that symbol name here */
	      const char */*command*/, // command name to run
	      unsigned int /*ring_characteristic*/,
	      unsigned int /*ring_n_vars*/)
{
}

void remote_gbde::update_io(int /*block*/) // call this "every once in a while"
{
}

int remote_gbde::connection_established() const
{
  return 1;
}

void remote_gbde::send_generator(const char */*buf*/, int /*len*/)
{
}

void remote_gbde::receive_generator(char *&/*buf*/, int &/*len*/)
{
}

void remote_gbde::compute_degree(unsigned int /*degree*/)
{
}

int remote_gbde::working()
{
  return 0;
}

void remote_gbde::pair_degrees(queue<unsigned int> &/*degree_list*/
			       // variable parameter
			       // for returning degrees
			       // with pairs at the
			       // moment
			       )
{
}

int remote_gbde::n_pairs()
{
  return 0;
}

int remote_gbde::n_pairs(unsigned int /*degree*/) 
     // how many pairs are there in this degree?
{
  return 0;
}

void remote_gbde::basis_degrees(queue<unsigned int> &/*degree_list*/
				// variable parameter
				// for returning degrees
				// with basis elements
				)
{
}

void remote_gbde::lead_terms(array<int> &/*result*/)
{
}

int remote_gbde::n_basis_elements()	// how many basis polys are there
{
  return 0;
}

int remote_gbde::n_basis_elements(unsigned int /*degree*/) 
     // how many basis polys are there in this degree?
{
  return 0;
}
