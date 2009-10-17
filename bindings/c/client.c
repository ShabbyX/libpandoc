#include <stdio.h>
#include "pandoc.h"

int std_reader(char* data, int length) {
  printf("\nREADING\n");
  return fread(data, 1, length, stdin);
}

void std_writer(char* data, int length) {
  printf("\nWRITING\n");
  fwrite(data, 1, length, stdout);
}

int main() {
  pandoc_init();  
  char* error =
    pandoc(PANDOC_FMT_MARKDOWN,
           PANDOC_FMT_HTML,
           PANDOC_FLAG_TOC | PANDOC_FLAG_STANDALONE,
           NULL,
           *std_reader,
           *std_writer);

  if (error != NULL) {
    printf("ERROR: %s\n", error);
    pandoc_exit();
    return -1;
  } else {
    pandoc_exit();
    return 0;
  }
}


