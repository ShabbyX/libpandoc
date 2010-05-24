#include <stdio.h>
#include <pandoc.h>

int reader(char* buf, int offset, int length) {
  printf("READING %i %i\n", offset, length);
  int x = fread(buf, 1, length, stdin);
  printf("READ: %i\n", x);
  return x;
}

void writer(char* buf, int offset, int length) {
  printf("WRITING %i %i\n", offset, length);
  fwrite(buf, 1, length, stdout);
}

int main(int argc, char *argv[]) {
  pandoc_init();
  pandoc("markdown", "html", "<settings/>", (*reader), (*writer));
  pandoc_exit();
  return 0;  
}

