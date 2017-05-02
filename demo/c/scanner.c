/* -*- Mode C -*- */

#include <stdio.h>
#include "machine.c"

unsigned char block[1024];

void
callback (int group, int start, int end)
{
  fprintf (stdout, "match: group=%3d start=%3d end=%3d <", group, start, end);
  fwrite (block + start, 1, end - start, stdout);
  fprintf (stdout, ">\n");  
}

int
main (int argc, char * argv[]) 
{
  if (argc < 2) {
    fprintf (stderr, "usage: %s <input-path>\n", argv[0]);
  } else {
    FILE * input = fopen (argv[1], "rb");
    if (!input) {
      fprintf (stderr, "failed to open input file: %s\n", argv[1]);
    } else {
      tdfa_state state;
      state.state = 0;
      state.vpos = 0;
      unsigned int start = 0;
      while (1) {
	int bytes = fread (block, 1, 1024, input);
	if (!bytes) {
	  break;
	} else {
	  tdfa_search (&state, block, bytes, &start, callback);
	}
      }
    }
  }
  return 0;
}
      
  
