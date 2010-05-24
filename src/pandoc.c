// extern void __stginit_LibPandoc(void);

#ifdef WIN32
#include <windows.h>
#include <Rts.h>

BOOL STDCALL DllMain(HANDLE hModule, DWORD reason, void* reserved) 
{
  static char* args[] = {"libpandoc", NULL};
  if (reason == DLL_PROCESS_ATTACH) {
    startupHaskell(1, args, __stginit_LibPandoc);
  }
  return TRUE;
}

void pandoc_init()
{

}

void pandoc_exit()
{
}

#else
#include <HsFFI.h>

void pandoc_init()
{
  int argc = 1;
  static char* args[] = {"libpandoc"};
  char** argv = args;
  hs_init(&argc, &argv);
  //  hs_add_root(__stginit_LibPandoc);
}

void pandoc_exit()
{
  hs_exit();
}

#endif


