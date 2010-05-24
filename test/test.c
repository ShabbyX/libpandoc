#include <stdio.h>
#include <pandoc.h>

/* void test(void) { */
/*   printf("%s", pandoc(PANDOC_FMT_MARKDOWN, */
/* 		      "null", */
/* 		      PANDOC_FMT_HTML, */
/* 		      "{\"WrapText\": false}", */
/* 		      "[hello](http://hello.com)")); */
/* } */

int main(int argc, char *argv[]) {
  pandoc_init();
  int x = increase(12);
  printf("%i\n", x);
  return 0;
}

