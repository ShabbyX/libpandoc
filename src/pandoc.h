/*
 * Copyright (C) 2009-2010  Anton Tayanovskyy <name.surname@gmail.com>
 *
 * This file is part of libpandoc, providing C bindings to Pandoc.
 * libpandoc is licensed under BSD 3-clause.  However, note that Pandoc
 * itself is licensed under GPL version 2 or later.
 */

#ifndef _PANDOC_H
#define _PANDOC_H 1

#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Initializes the Haskell runtime. Every call to this function should
 * be matched with exactly one call to `pandoc_exit`.
 */
void pandoc_init();

/* Shuts down the Haskell runtime.  */
void pandoc_exit();

/*
 * Calls `pandoc` with given input and output formats and streams.
 * Returns a `NULL` on success, or a `NULL`-terminated error message
 * on failure.  Settings is an XML string conforming to a schema
 * distributed with `libpandoc`.  Settings can be `NULL`.  All strings
 * should be encoded as UTF-8.  User data is any pointer.
 */
char *pandoc(int buffer_size,
            const char *input_format, const char *output_format, const char *settings,
            int (*reader)(char *, void *), void (*writer)(const char *, int, void *),
            void *user_data);

#ifdef __cplusplus
}
#endif

#endif /* !_PANDOC_H */
