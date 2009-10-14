#include <stdio.h>
#include <HsFFI.h>

extern void __stginit_LibPandoc(void);

void pandoc_init(void){
  int argc = 1;
  char* args[] = {"libpandoc"};
  char** argv = args;
  hs_init(&argc, &argv);
  hs_add_root(__stginit_LibPandoc);
}

void pandoc_exit(void){
  hs_exit();
}


