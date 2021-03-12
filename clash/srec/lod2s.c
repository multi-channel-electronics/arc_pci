/* _GNU_SOURCE needed for portable getline */
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include "srec.h"

#define DSPMEM_P 0x005f50
#define DSPMEM_X 0x005f58
#define DSPMEM_Y 0x005f59

int dspmem_code(char *ss) {
  if (strcmp(ss, "X")==0)
    return DSPMEM_X;
  if (strcmp(ss, "Y")==0)
    return DSPMEM_Y;
  if (strcmp(ss, "P")==0)
    return DSPMEM_P;
  return -1;
}

int srecs_out(FILE *dest, char *data, int count, int address) {
  char *my_data = data;
  int   my_count = count;
  int   my_address = address*3;

  //printf("count=%i\n", my_count);

  struct srec_t srec;

  while (my_count>0) {

    int line_count = SREC_DATA - SREC_DATA % 3;
    if (line_count>my_count) line_count = my_count;
    
    srec_data(&srec, &my_data, &line_count, &my_address);

    srec_fprint(&srec, dest);
    my_count = count + data - my_data;
  }

  return 0;
}


int main(int argc, char **argv) {

  int chksum1 = 0;
  int chksum2 = 0;
  int chksum3 = 0;

  if (argc !=4 && argc!=5) {
  
    printf("Usage:\n"
	   "   %s input.lod output.s title\n"
	   "   %s input.lod output.s title 4\n",
	   argv[0]);
    exit(1);
  }

  FILE *fin  = fopen(argv[1], "r");
  FILE *fout = fopen(argv[2], "w");
  if (fin==NULL || fout==NULL) {
    printf("Could not open files\n");
    exit(1);
  }

  int   hdr_salt = 0;
  if (argc==5) hdr_salt = atoi(argv[4]);

  int   line_size = 1000;
  char *line = (char*)malloc(line_size);
  char word[256];
  if (line==0) {
    printf("Memory error.\n");
    exit(1);
  }


  int in_data = 0;
  struct srec_t srec;

  srec_header_salt(&srec, argv[3], hdr_salt);
  srec_fprint(&srec, fout);

  int data_address = -1;
  int data_count = 0;
  char data_buf[10000];

  int mem_type = 0;
  
  while (!feof(fin)) {

    int line_count = line_size-1;
    getline(&line, &line_count, fin);

    int idx = 0, didx;

    sscanf(line, "%s%n", word, &didx);
    idx += didx;

    if (strcmp(word, "_SYMBOL")==0 ||
	strcmp(word, "_END")==0) {
      break;
    } 

    if (strcmp(word, "_DATA")==0) {

      //Dump existing data, if any
      if (data_count>0) {
	srecs_out(fout, data_buf, data_count, data_address);
	data_count = 0;	
      }

      //Get new address
      sscanf(line+idx, "%s%n", word, &didx);
      idx += didx;
      mem_type = dspmem_code(word);
      if (mem_type!=DSPMEM_P) {
	printf("Can only handle P memory!\n");
	exit(1);
      }
      sscanf(line+idx, "%x", &data_address);
      data_count = 0;      
      in_data = 1;
      //printf("%s = [%s]%x %x\n", line, word, mem_type, mem_ofs);

    } else if (in_data) {
      idx=0;
      while (1) {
	int datum;
	int delta = sscanf(line+idx, "%x%n", &datum, &didx);
	idx+=didx;
	if (delta!=1) break;
	
	if (mem_type == DSPMEM_P) {
	  chksum1 += datum;
	  int i;
	  for (i=0; i<3; i++) {
	    data_buf[data_count++] = (char)( (datum >> 8*i) & 0xff );
	    chksum2 += (datum >> 8*i) & 0xff;
	  }
	}
      }
    }
    
  }

  //Dump existing data, if any
  if (data_count>0) {
    srecs_out(fout, data_buf, data_count, data_address);
    data_count = 0;
  }

  srec_term(&srec);
  srec_fprint(&srec, fout);

  fclose(fin);
  fclose(fout);

  printf("%x %x\n", chksum1, chksum2);

  return 0;


}
