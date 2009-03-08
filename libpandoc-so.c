#include <HsFFI.h>
 
static void library_init(void) __attribute__((constructor));
static void library_init(void)
{  
  static char*  arguments[1] = { "" };
  static char** argv         = arguments;
  static int    argc         = 1;
  hs_init(&argc, &argv);
}
 
static void library_exit(void) __attribute__((destructor));
static void library_exit(void)
{
  hs_exit();
}
