# `libpandoc`

The purpose of `libpandoc` is to make the [Haskell][haskell] library
[Pandoc][pandoc] available for use from C and other non-Haskell
environments that support C FFI.  Pandoc and `libpandoc` support
text conversion between HTML, Markdown, LaTeX, OpenDocument and other
formats.


## Obtaining

The latest version is available (BSD license) at [GitHub][libpandoc].


## Installation

### Building

Building follows standard Haskell conventions and requires the
[Haskell Platform][haskell-platform]:

    # cabal configure [--global]
    # cabal build

The configure stage may report missing dependencies.  These can be
obtained as follows:

    # cabal install [dependency]

A successful build creates the shared library file in
`./dist/build/libpandoc.dll/libpandoc.dll`.  Installation and use of
the library is platform-dependent.

[haskell-platform]: http://hackage.haskell.org/platform/

### UNIX Installation

For your convenience, an `./install.sh` is provided that installs the
shared object and header files under `/usr/local`.  It has been tested
on Ubuntu Linux.

To install system-wide in the `$LIB` folder, copy `libpandoc.dll` to
`$LIB/libpandoc.so` and run `ldconfig $LIB`.  Also, copy
`src/pandoc.h` to `$INCLUDES/`.

To use the library from C, do:

    #include <pandoc.h>
    pandoc(...)

And compile as:

    gcc [my-file.c] -lpandoc

### Windows Installation

For your convenience, an `./install.bat` is provided that installs the
shared library under `%windir%\System32`.

Deploy the shared library in the same folder as your application, or
put it under `%PATH%`.  Intallation of header files depends on the C
compiler.


## Using

### C Interface

The C interface is defined in `src/pandoc.h`. Synopsis:

    pandoc_init();
    char* error = pandoc(1024       /* buffer size */,
                         "markdown" /* input format */,
                         "html"     /* output format */,
                         NULL       /* XML settings */,
                         reader     /* the reader function */,
                         writer     /* the writer function */,
                         user_data  /* private user data */);
    pandoc_exit();

Haskell runtime has to be started and stopped explicitly via the
`init/exit` functions.

The reader function is in the following form:

    int reader(char *buf, void *user_data);

Where `buf` is the buffer to be filled.  The size of this buffer is the same
as provided as the first argument to the `pandoc` function.  `user_data` is
the same pointer passed as the last argument of the `pandoc` function.  The
reader function must fill the buffer with the input to be converted by pandoc.

The writer function is in the following form:

    void writer(char *buf, int len, void *user_data);

Where `buf` is the buffer to be written, `len` is the number of elements in
the buffer and `user_data` is the last argument of the `pandoc` function,
similar to `user_data` of the reader.  The writer function must write the
contents of the buffer as the output of the conversion by pandoc.

#### Input and Output Formats

Input and output formats depend on Pandoc version the library is built
against.  They are passed as strings.  Possible values include:

- For reader:

  * docbook
  * html
  * latex
  * markdown
  * mediawiki
  * native
  * rst
  * ~~texmath~~
  * textile

- For writer:

  * asciidoc
  * context
  * docbook
  * ~~docx~~
  * ~~epub~~
  * ~~fb2~~
  * html
  * latex
  * man
  * markdown
  * mediawiki
  * native
  * ~~odt~~
  * opendocument
  * org
  * rst
  * rtf
  * texinfo
  * textile

In addition, an automatically derived `xml` format is provided for
both input and output.

Note: Some read and write types supported by pandoc striked above are not yet
supported by libpandoc.

#### XML Settings

The settings parameter allows to customize the text transformation by
passing Pandoc settings as an XML-encoded string.  The XML format is
derived automatically from Pandoc data type declarations by generic
programming.

Below is a printout of the default settings.  NOTE: it may be outdated
with respect to the current library version.  The custom settings
passed by the user are merged with the default settings, so only the
fields that have non-default values have to be provided.

    <record name="LibPandocSettings">
      <field name="writerOptions">
        <record name="WriterOptions">
          <field name="writerStandalone">
            <int>0</int>
          </field>
          <field name="writerTemplate">
            <string></string>
          </field>
          <field name="writerVariables">
            <list />
          </field>
          <field name="writerTabStop">
            <int>4</int>
          </field>
          <field name="writerTableOfContents">
            <int>0</int>
          </field>
          <field name="writerSlideVariant">
            <data name="S5Slides" />
          </field>
          <field name="writerIncremental">
            <int>0</int>
          </field>
          <field name="writerHTMLMathMethod">
            <data name="PlainMath" />
          </field>
          <field name="writerIgnoreNotes">
            <int>0</int>
          </field>
          <field name="writerNumberSections">
            <int>0</int>
          </field>
          <field name="writerNumberOffset">
            <list />
          </field>
          <field name="writerSectionDivs">
            <int>0</int>
          </field>
          <field name="writerExtensions">
            <set />
          </field>
          <field name="writerReferenceLinks">
            <int>0</int>
          </field>
          <field name="writerWrapText">
            <int>1</int>
          </field>
          <field name="writerColumns">
            <int>80</int>       <!-- TODO: unsure -->
          </field>
          <field name="writerLiterateHaskell">
            <int>0</int>
          </field>
          <field name="writerEmailObfuscation">
            <data name="JavascriptObfuscation" />
          </field>
          <field name="writerIdentifierPrefix">
            <string></string>
          </field>
          <field name="writerSourceDirectory">
            <string></string>
          </field>
          <field name="writerUserDataDir">
            <string></string>
          </field>
          <field name="writerCiteMethod">
            <data name="Citeproc" />
          </field>
          <field name="writerBiblioFiles">
            <list />
          </field>
          <field name="writerHtml5">
            <int>0</int>
          </field>
          <field name="writerHtmlQTags">
            <int>0</int>
          </field>
          <field name="writerBeamer">
            <int>0</int>
          </field>
          <field name="writerSlideLevel">
            <int>0</int>
          </field>
          <field name="writerChapters">
            <int>0</int>
          </field>
          <field name="writerListings">
            <int>0</int>
          </field>
          <field name="writerHighlight">
            <int>1</int>
          </field>
          <field name="writerHighlightStyle">
            <data name="pygments" />
          </field>
          <field name="writerSetextHeaders">
            <int>0</int>
          </field>
          <field name="writerTeXLigatures">
            <int>1</int>
          </field>
          <field name="writerEpubVersion">
            <data name="epub2" />
          </field>
          <field name="writerEpubMetadata">
            <string></string>
          </field>
          <field name="writerEpubStylesheet">
            <string></string>
          </field>
          <field name="writerEpubFonts">
            <list />
          </field>
          <field name="writerEpubChapterLevel">
            <int>3</int>        <!-- TODO: unsure -->
          </field>
          <field name="writerTOCDepth">
            <int>3</int>
          </field>
          <field name="writerReferenceODT">
            <string></string>
          </field>
          <field name="writerReferenceDocx">
            <string></string>
          </field>
        </record>
      </field>
      <field name="readerOptions">
        <record name="ReaderOptions">
          <field name="readerExtensions">
            <set />
          </field>
          <field name="readerSmart">
            <int>0</int>
          </field>
          <field name="readerStrict">
            <int>0</int>
          </field>
          <field name="readerStandalone">
            <int>0</int>
          </field>
          <field name="readerParseRaw">
            <int>0</int>
          </field>
          <field name="readerColumns">
            <int>80</int>
          </field>
          <field name="readerTabStop">
            <int>4</int>
          </field>
          <field name="readerOldDashes">
            <int>0</int>
          </field>
          <field name="readerReferences">
            <list />
          </field>
          <field name="readerCitationStyle">
            <string></string>
          </field>
          <field name="readerApplyMacros">
            <int>0</int>
          </field>
          <field name="readerIndentedCodeClasses">
            <list />
          </field>
          <field name="readerDefaultImageExtension">
            <string></string>
          </field>
        </record>
      </field>
    </record>

### Other Interfaces

  * A .NET/C# interface is available as [libpandoc-dotnet][libpandoc-dotnet]
    (probably outdated).


## Changelog

  * 0.7 - Updated to pandoc version 1.13 and higher
  * 0.6 - Updated to pandoc version 1.10 and higher
  * 0.5 - Implemented XML generics to support all config settings.


## Authors

Original author is Anton Tayanovskyy <name.surname@gmail.com>, but he seems to be
inactive regarding libpandoc.

Shahbaz Youssefi <shabbyx@gmail.com> is the current maintainer.  Bug reports and
feature requests are welcome.



[haskell]:          http://www.haskell.org
[pandoc]:           http://johnmacfarlane.net/pandoc/
[libpandoc]:        http://github.com/ShabbyX/libpandoc/
[libpandoc-dotnet]: http://github.com/toyvo/libpandoc-dotnet/
