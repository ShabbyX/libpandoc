# `libpandoc`

The purpose of `libpandoc` is to make the [Haskell][haskell] library
[Pandoc][pandoc] available for use from C and other non-Haskell
environments that support C FFI.  Pandoc and `libpandoc` support
text conversion between HTML, Markdown, LaTeX, OpenDocument and other
formats.


## Obtaining

The latest version is available at [GitHub][libpandoc].  `libpandoc`
is licensed under GPL version 2 or later, which is also Pandoc's
license.


## Installation

### Building

Building follows standard Haskell conventions and requires the
[Haskell Platform][haskell-platform]:

    $ cabal configure [--global]
    $ cabal install --only-dependencies
    $ cabal build

Or simply:

    $ make

A successful build creates the shared library file in
`./dist/build/libpandoc.so/libpandoc.so`.  Installation and use of
the library is platform-dependent.

Note: On Windows, you should edit the `libpandoc.cabal` file to rename
the extension of the shared object to dll.

[haskell-platform]: http://hackage.haskell.org/platform/

### UNIX Installation

The accompanying `Makefile` contains `install` and `uninstall` targets
that take care of installing the header and library in `/usr`.

### Windows Installation

For your convenience, an `./install.bat` is provided that installs the
shared library under `%windir%\System32`.

## Using

### C Interface

The C interface is defined in `src/pandoc.h`. Synopsis:

```c
#include <pandoc.h>

pandoc_init();
char* error = pandoc(1024       /* buffer size */,
                     "markdown" /* input format */,
                     "html"     /* output format */,
                     NULL       /* JSON settings */,
                     reader     /* the reader function */,
                     writer     /* the writer function */,
                     user_data  /* private user data */);
pandoc_exit();
```

Compile as:

    gcc [my-file.c] -lpandoc

Haskell runtime has to be started and stopped explicitly via the
`init/exit` functions.  Note that you don't need to restart the Haskell runtime
on every call to `pandoc`.  As a matter of fact, you may not even be able to
initialize the Haskell runtime after stopping it, e.g. due to
[this bug](https://downloads.haskell.org/%7Eghc/7.8.2/docs/html/users_guide/bugs-and-infelicities.html#infelicities-ffi)
of GHC.

The reader function is in the following form:

    int reader(char *buf, void *user_data);

Where `buf` is the buffer to be filled.  The size of this buffer is the same
as provided as the first argument to the `pandoc` function.  `user_data` is
the same pointer passed as the last argument of the `pandoc` function.  The
reader function must fill the buffer with the input to be converted by Pandoc.
The return value is the number of characters read. The reader is no longer called
when this value is zero.

The writer function is in the following form:

    void writer(const char *buf, int len, void *user_data);

Where `buf` is the buffer to be written, `len` is the number of elements in
the buffer and `user_data` is the last argument of the `pandoc` function,
similar to `user_data` of the reader.  The writer function must write the
contents of the buffer as the output of the conversion by Pandoc. `buf` 
is not necessarily `NUL`-terminated. In fact, most of the time it isn't 
because the output could be given to writer() in chunks. That's also why 
there is a `len` argument.

#### Input and Output Formats

Input and output formats depend on Pandoc version the library is built
against.  They are passed as strings.  Reader and writer names are the same
as understood by Pandoc, for example `"html"` or `"markdown"`.


#### JSON Settings

The settings parameter allows to customize the text transformation by
passing Pandoc settings as an JSON-encoded string.  The JSON format is
derived automatically from Pandoc data type declarations by generic
programming.

The file `defaults.json` is a printout of the default settings.  NOTE: it may be outdated
with respect to the current library version.  The custom settings
passed by the user are merged with the default settings, so only the
fields that have non-default values have to be provided.

### C example code

```c
#include <stdlib.h>
#include <string.h>

#include <pandoc.h>

int done = 0;
const char test[] = "$\\textit{K}$ -trivial, $\\textit{K}$ -low and ${{\\mathrm{\\textit{MLR}}}}$ -low Sequences: A Tutorial";
const int BUFFER_LENGTH = 1024;

int reader(char *buf, void *user_data) {
    if (done) {
        return 0;
    }
    strncpy(buf, test, BUFFER_LENGTH);
    done = 1;
    return strlen(test);
}

void writer(const char *buf, int len, void *user_data) {
    fwrite(buf, 1, len, stdout);
}

int main() {
    pandoc_init();
    pandoc(BUFFER_LENGTH, "markdown", "plain", NULL, &reader, &writer, NULL);
    printf("\n");      /* if desired */
    pandoc_exit();
}
```


### Other Interfaces

  * A .NET/C# interface is available as [libpandoc-dotnet][libpandoc-dotnet]
    (currently outdated).


## Changelog

  * 0.8
    - Switched to JSON format for settings instead of XML
    - Completed support for all Pandoc readers and writers
    - Updated to Pandoc version 1.16 and higher
  * 0.7 - Updated to Pandoc version 1.13 and higher
  * 0.6 - Updated to Pandoc version 1.10 and higher
  * 0.5 - Implemented XML generics to support all config settings.


## Authors

Original author is Anton Tayanovskyy <name.surname@gmail.com>.

Shahbaz Youssefi <shabbyx@gmail.com> is the current maintainer.  Bug reports and
feature requests are welcome.

Additional Contributers:

- Katherine Whitlock


[haskell]:          http://www.haskell.org
[pandoc]:           http://johnmacfarlane.net/pandoc/
[libpandoc]:        http://github.com/ShabbyX/libpandoc/
[libpandoc-dotnet]: http://github.com/toyvo/libpandoc-dotnet/
