// Copyright 1996 Michael E. Stillman

#include "interp.hh"
#include "remote.hh"

void cmd_remote(object &om, object &omach, object &ouser, object &os, object &ocmd)
{
  Matrix m = om->cast_to_Matrix();
  gStack.insert(new remote_gb_comp(m, omach->string_of(), 
				   ouser->string_of(), 
				   os->string_of(), 
				   ocmd->string_of()));
}

void cmd_remote_send(object &oremote)
{
  remote_gb_comp *p = oremote->cast_to_remote_gb_comp();
  p->send_matrix();
}

void cmd_remote_receive(object &oremote)
{
  remote_gb_comp *p = oremote->cast_to_remote_gb_comp();
  Matrix m = p->gens();
  gStack.insert(m);
}

void cmd_remote_next_degree(object &oremote)
{
  remote_gb_comp *p = oremote->cast_to_remote_gb_comp();
  int result = p->compute_next_degree();
  gStack.insert(make_object_int(result));
}

void cmd_remote_stats(object &oremote)
{
  remote_gb_comp *p = oremote->cast_to_remote_gb_comp();
  p->stats(cout);
}

int i_remote_gb_cmds()
{
  // send(remote_comp)
  // compute_next_degree
  install(ggremote, cmd_remote, TY_MATRIX, TY_STRING, TY_STRING, TY_STRING, TY_STRING);
  install(ggsend, cmd_remote_send, TY_REMOTE_GB_COMP);
  install(ggstats, cmd_remote_stats, TY_REMOTE_GB_COMP);
  install(ggreceive, cmd_remote_receive, TY_REMOTE_GB_COMP);

  // For later...
//  install(ggleadterm, cmd_remote_leadterms, TY_REMOTE_GB);
//  install(ggreceive, cmd_remote_receive, TY_REMOTE_GB, TY_INT);

  return 0;
}

