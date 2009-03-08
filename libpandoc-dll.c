#include <windows.h>
#include <Rts.h>

extern void __stginit_LibPandoc(void);

BOOL STDCALL DllMain( HANDLE hModule, DWORD reason, void* reserved) {
  if (reason == DLL_PROCESS_ATTACH) {
    static char*  args[] = { "ghcDll", NULL };
    startupHaskell(1, args, __stginit_LibPandoc);
  }
  return TRUE;
}

