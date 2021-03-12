#include <stdio.h>
#include <stdlib.h>
#include "srec.h"


int main(int argc, char **argv) {
  
  char line[300];

  while (1) {
    
    scanf("%s", line);

    if (feof(stdin)) break;
    
    struct srec_t srec;
    if (srec_parse(&srec, line)) {
      printf("Parse failed\n");
      return 1;
    }
    srec_print(&srec);

  }

  return 0;
}
      
