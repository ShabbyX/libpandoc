#ifndef _PANDOC_H
#define _PANDOC_H 1
#include <wchar.h>

/*
 * Initializes the Haskell runtime. Every call to this function should
 * be matched with exactly one call to `pandoc_exit`.
 */
extern void pandoc_init();

/* Shuts down the Haskell runtime.  */
extern void pandoc_exit();

/*
 * Calls `pandoc` with given input and output formats and streams.
 * Returns a `NULL` on success, or a `NULL`-terminated error message
 * on failure.  Settings is an XML string conforming to a schema
 * distributed with `libpandoc`.  Settings can be `NULL`.  All strings
 * should be encoded as UTF-8.
 */
extern char *pandoc(int buffer_size,
                    char *input_format, char *output_format, char *settings,
                    int (*reader)(char *), void (*writer)(char *, int));

#endif /* !_PANDOC_H */
