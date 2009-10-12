#ifndef _PANDOC_H
#define _PANDOC_H 1

/* Pandoc format constants  */
#define PANDOC_FMT_CONTEXT      0
#define PANDOC_FMT_DOCBOOK      1
#define PANDOC_FMT_HTML         2
#define PANDOC_FMT_HTML_LHS     3
#define PANDOC_FMT_LATEX        4
#define PANDOC_FMT_LATEX_LHS    5
#define PANDOC_FMT_MAN          6
#define PANDOC_FMT_MARKDOWN     7
#define PANDOC_FMT_MARKDOWN_LHS 8
#define PANDOC_FMT_MEDIAWIKI    9
#define PANDOC_FMT_NATIVE       10
#define PANDOC_FMT_ODT          11
#define PANDOC_FMT_OPENDOCUMENT 12
#define PANDOC_FMT_RST          13
#define PANDOC_FMT_RST_LHS      14
#define PANDOC_FMT_RTF          15
#define PANDOC_FMT_S5           16
#define PANDOC_FMT_TEXINFO      17

/* Pandoc flags  */                  
#define PANDOC_FLAG_EMAIL_OBFUSCATION_NONE       0x00001
#define PANDOC_FLAG_EMAIL_OBFUSCATION_JAVASCRIPT 0x00002
#define PANDOC_FLAG_EMAIL_OBFUSCATION_REFERENCES 0x00004
#define PANDOC_FLAG_INCREMENTAL                  0x00008
#define PANDOC_FLAG_MATH_ASCIIMATHML             0x00010
#define PANDOC_FLAG_MATH_GLADTEX                 0x00020
#define PANDOC_FLAG_MATH_JSMATH                  0x00040
#define PANDOC_FLAG_MATH_LATEXMATHML             0x00080
#define PANDOC_FLAG_MATH_MIMETEX                 0x00100
#define PANDOC_FLAG_NO_WRAP                      0x00200
#define PANDOC_FLAG_NUMBER_SECTIONS              0x00400
#define PANDOC_FLAG_PARSE_RAW                    0x00800
#define PANDOC_FLAG_PRESERVE_TABS                0x01000
#define PANDOC_FLAG_REFERENCE_LINKS              0x02000 
#define PANDOC_FLAG_SANITIZE_HTML                0x04000
#define PANDOC_FLAG_SMART                        0x08000
#define PANDOC_FLAG_STANDALONE                   0x10000
#define PANDOC_FLAG_STRICT                       0x20000
#define PANDOC_FLAG_TOC                          0x40000

/* Initializes the Haskell runtime. Every call to this function should
   be matched with exactly one call to `pandoc_end`.  */
extern void pandoc_init();

/* Shuts down the Haskell runtime.  */
extern void pandoc_end();

/* Calls pandoc with given input and output streams. Returns a NULL on
   success, or a NULL-terminated error message on failure  */
extern char* pandoc(int input_format,
                    int output_format,
                    int flags,
                    char* options,
                    int (*reader)(char*, int),
                    void (*writer)(char*, int));

#endif /* !_PANDOC_H */
