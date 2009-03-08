#include <stdio.h>
#include <pandoc.h>

void test(void) {
  printf("%s", pandoc("markdown", "html", "[hello](http://hello.com)"));
}

int main(int argc, char *argv[]) {
    test();
    return 0;
}

