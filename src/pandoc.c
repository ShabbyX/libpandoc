/*
 * Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 * Copyright (C) 2015  Shahbaz Youssefi <ShabbyX@gmail.com>
 *
 * This file is part of libpandoc, providing C bindings to Pandoc.
 *
 * libpandoc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * libpandoc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libpandoc.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef WIN32
#include <windows.h>
#include <Rts.h>

extern void __stginit_LibPandoc(void);

BOOL STDCALL DllMain(HANDLE hModule, DWORD reason, void *reserved)
{
  static char *args[] = {"libpandoc", NULL};
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
#include <stdlib.h>
#include <HsFFI.h>

void pandoc_init()
{
  int argc = 1;
  static char arg0[] = "libpandoc";
  static char *args[] = {arg0, NULL};
  char **argv = args;
  hs_init(&argc, &argv);
}

void pandoc_exit()
{
  hs_exit();
}

#endif
