#ifndef __REMOTE_GBDE_HH
#define __REMOTE_GBDE_HH

#include "queue.hh"
#include "array.hh"

class remote_gbde
{
public:
  // constructor contacts the remote machine. May take a few seconds.
  // let me know if you need the code to read the password from the
  // user without it echoing. 
  remote_gbde(const char *machine_name, /* e.g. "pauli.ruph.cornell.edu" */
              const char *username,
	      const char *password, /* don't leave that symbol name here */
	      const char *command, // command name to run
	      unsigned int ring_characteristic,
	      unsigned int ring_n_vars);

  void update_io(int block=0); // call this "every once in a while"

  int connection_established() const; // returns: 0 => Sorry, please call again
                                      //          1 => ready to go
                                      //         -1 => failed (remote nack)
				      // this last should be thrown,
				      // not returned, when you start using
				      // exceptions.

  // for the moment, provide a copied buffer and <I> will delete it 
  // internally when it's been sent. When we make this class more front-end
  // aware, we can use whatever buffer deletion convention is convenient.
  void send_generator(const char *buf, int len);

  void receive_generator(char *&buf, int &len);

  // the degree parameter here will have a more complicated meaning,
  // but still be an int, when we generalize
  void compute_degree(unsigned int degree);

  int working(); // is the remote end still working 


  // here down, feel free to make changes...
  
  void pair_degrees(queue<unsigned int> &degree_list // variable parameter
						     // for returning degrees
						     // with pairs at the
						     // moment
		    );

  int n_pairs();
  int n_pairs(unsigned int degree); // how many pairs are there in this
				    // degree?

  void basis_degrees(queue<unsigned int> &degree_list // variable parameter
						      // for returning degrees
						      // with basis elements
		     );

  void lead_terms(array<int> &result);
  int n_basis_elements();	// how many basis polys are there
  int n_basis_elements(unsigned int degree); // how many basis polys are there
					     // in this degree?

};

#endif
