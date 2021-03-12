#ifndef _SREC_H_
#define _SREC_H_

#define SREC_MAXDATA 128
#define SREC_DATA 32

struct srec_t {
  int type;

  int count;
  int address;
  int address_size;

  int chksum;

  char data[SREC_MAXDATA];

};


int  srec_parse(struct srec_t *srec, char *rec_str);

void srec_dump(struct srec_t *srec);
void srec_fdump(struct srec_t *srec, FILE *fout);

void srec_print(struct srec_t *srec);
void srec_fprint(struct srec_t *srec, FILE *fout);

int  srec_header_salt(struct srec_t *srec, char *text, int hdr_salt);
int  srec_header(struct srec_t *srec, char *text);
int  srec_data(struct srec_t *srec,
	      char **data, int *count, int *address);
int  srec_term(struct srec_t *srec);

#endif
