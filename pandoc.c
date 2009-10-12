#include <stdio.h>
#include <HsFFI.h>

#ifdef __GLASGOW_HASKELL__
/* # - include "LibPandoc_stub.h" */
extern void __stginit_LibPandoc(void);
#endif

void pandoc_init(void){
  int argc = 0;
  char** argv;
  hs_init(&argc, &argv);
  hs_add_root(__stginit_LibPandoc);
}

void pandoc_end(void){
  hs_exit();
}
