#ifndef _PANDOC_H
#define _PANDOC_H 1

/* Initializes the Haskell runtime. Every call to this function should
   be matched with exactly one call to `pandoc_end`.  */
extern void pandoc_init();

/* Shuts down the Haskell runtime.  */
extern void pandoc_exit();

extern int increase(int x);

/* Calls `pandoc` with given input and output streams.  Returns a
   `NULL` on success, or a `NULL`-terminated error message on failure.
   Settings is an XML string conforming to a schema distributed with
   `libpandoc`.  */
extern char* pandoc(wchar_t* input_format,
                    wchar_t* output_format,
                    wchar_t* settings,
                    int (*reader)(wchar_t*, int, int),
                    void (*writer)(wchar_t*, int, int));

#endif /* !_PANDOC_H */
