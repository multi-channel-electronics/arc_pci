#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "srec.h"

#define PRINT //printf

/*INPUT*/

char get_hex_char(char *src) {
  int tmp = 0;
  sscanf(src, "%02x", &tmp);
  return (char)( tmp & 0xff );
}

int get_int_swap(char *src, int size) {
  int i;
  int val = 0;
  for (i=0; i<size; i++) {
    val = (val << 8) | (int)src[i];
  }
  return val;
}


int srec_parse(struct srec_t *srec, char *rec_str) {

  int tmp;
  int i;
  char line[SREC_MAXDATA];

  PRINT("srec_parse enter '%s'\n", rec_str);

  //First two characters are S#, with # the type
  
  if (rec_str[0] != 'S') return -1;
  
  srec->type  = (int)(rec_str[1] - '0');
  
  PRINT("srec_parse type=%i\n", srec->type);

  //Remaining bytes are data, first byte is length
  int line_len = 0;
  if (sscanf(rec_str+2, "%02x", &line_len)!=1)
    return -1;

  PRINT("srec_parse length=%i\n", line_len);
  
  int chksum = line_len;
  int datum = 0;
  for (i=0; i<line_len; i++) {
    line[i] = get_hex_char(&rec_str[4+2*i]);
    chksum += line[i];
  }

  PRINT("srec_parse check=%x\n", chksum);

  if ((chksum & 0xff) != 0xff) {
    PRINT("srec_parse chksum error\n");
    return -1;
  }

  switch(srec->type) {

  case 0:
  case 1:
  case 9:
    srec->address_size = 2;
    break;

  default:
    return -1;
  }

  srec->count = (line_len - srec->address_size - 1);

  PRINT("srec_parse count=%i\n", srec->count);

  srec->address = get_int_swap(line, srec->address_size);
  
  PRINT("srec_parse address=%#x\n", srec->address);

  PRINT("srec_parse data= ");

  for (i=0; i < srec->count; i++) {
    srec->data[i] = (int)line[srec->address_size+i];
    PRINT(" %02x", srec->data[i] & 0xff);
  }

  PRINT("\n");

  srec->chksum = (int)line[srec->address_size+srec->count] & 0xff;

  PRINT("srec_parse chksum=%x\n", srec->chksum);

  return 0;
}

/* OUTPUT */

void srec_fdump(struct srec_t *srec, FILE *fout) {
  int i;

  fprintf(fout,
	  "type=%i addr=%08x width=%i chksum=%02x ",
	  srec->type, srec->address, srec->address_size, srec->chksum);

  switch (srec->type) {

  case 0: /* header */
    for (i=0; i<srec->count; i++) 
      fprintf(fout, "%c", srec->data[i]);
    break;
    
  default:
    for (i=0; i<srec->count; i++)
      fprintf(fout, "%02X ", srec->data[i] & 0xff);
  }

  fprintf(fout, "\n");

}

void srec_dump(struct srec_t *srec) {
  srec_fdump(srec, stdout);
}

void srec_fprint(struct srec_t *srec, FILE *fout) {
  int i;

  fprintf(fout, "S%X%02X%04X",
	  srec->type, srec->count+3, srec->address);

  for (i=0; i<srec->count; i++)
    fprintf(fout, "%02X", srec->data[i] & 0xff);

  fprintf(fout, "%02X\r\n", srec->chksum);

}

void srec_print(struct srec_t *srec) {
  srec_fprint(srec, stdout);
}

/* create header line from string */

int srec_header_salt(struct srec_t *srec, char *text, int hdr_salt) {
  int i;

  srec->type = 0;
  srec->address = hdr_salt;
  srec->address_size = 2;
  srec->count = strlen(text);

  srec->chksum = (srec->count + 1 + srec->address_size);
  for (i=0; i<srec->address_size; i++)
    srec->chksum += (srec->address >> 8*i);

  for (i=0; i<srec->count; i++) {
    srec->data[i] = text[i];
    srec->chksum += text[i];
  }

  srec->chksum = (~srec->chksum) & 0xff;

  return 0;
}


int srec_header(struct srec_t *srec, char *text) {
  return srec_header_salt(srec, text, 0);
}


/* create data line from data */

int srec_data(struct srec_t *srec,
	      char **data, int *count, int *address) {

  srec->type = 1;
  srec->address = *address;
  srec->address_size = 2;
  srec->count = (*count < SREC_DATA ? *count : SREC_DATA);

  srec->chksum = 1 + srec->address_size + srec->count
    + srec->address + (srec->address >> 8);

  int i;
  for (i=0; i<srec->count; i++) {
    //printf("%i->%i\n", (int)(*data), (int) (**data));
    srec->data[i] = *((*data)++);
    (*count)--;
    (*address)++;
    srec->chksum += srec->data[i];
  }

  srec->chksum = (~srec->chksum) & 0xff;

  return 0;
}

/* create terminator line */


int srec_term(struct srec_t *srec) {

  srec->type = 9;
  srec->address = 0;
  srec->address_size = 2;
  srec->count = 0;

  srec->chksum = ~( 1 + srec->address_size) & 0xff;

  return 0;
}
