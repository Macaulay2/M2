extern "C" {
  void M2_init_gmp();
}

/* this function supplants one provided by factory */

int initializeGMP() {		/* called by factory */
  static int done = 0;
  if (done == 0) {
    M2_init_gmp();
    done = 1;
  }
  return 1;
}
