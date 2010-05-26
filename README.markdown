% `libpandoc`

The purpose of `libpandoc` is to make the [Haskell][1] library
[Pandoc][2] available for use from C and other non-Haskell
environments that support C FFI.  Pandoc and (`libpandoc`) support
text conversion between HTML, Markdown, LaTeX, OpenDocument and other
formats.


# C Interface

The C interface is defined in `src/pandoc.h`. Synopsis:

    pandoc_init()
    char* error = pandoc(1024 /* buffer size */,
                         "markdown" /* input format */,
                         "html", /* output format */,
                         NULL /* XML settings */,
                         *reader,
                         *writer);
    pandoc_exit();

Haskell runtime has to be started and stopped explicitly via the
`init/exit` functions.


## Input and Output Formats

Input and output formats depend on Pandoc version the library is built
against.  They are passed as strings.  Possible values include:

  * html
  * latex
  * markdown
  * rst
  * context
  * docbook
  * man
  * mediawiki
  * opendocument
  * plain
  * rst
  * rtf
  * s5
  * texinfo

In addition, an automatically derived `xml` format is provided for
both input and output.


## XML Settings

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
          <field name="writerIncludeBefore">
            <string></string>
          </field>
          <field name="writerIncludeAfter">
            <string></string>
          </field>
          <field name="writerTabStop">
            <int>4</int>
          </field>
          <field name="writerTableOfContents">
            <int>0</int>
          </field>
          <field name="writerS5">
            <int>0</int>
          </field>
          <field name="writerXeTeX">
            <int>0</int>
          </field>
          <field name="writerHTMLMathMethod">
            <data name="PlainMath" />
          </field>
          <field name="writerIgnoreNotes">
            <int>0</int>
          </field>
          <field name="writerIncremental">
            <int>0</int>
          </field>
          <field name="writerNumberSections">
            <int>0</int>
          </field>
          <field name="writerStrictMarkdown">
            <int>0</int>
          </field>
          <field name="writerReferenceLinks">
            <int>0</int>
          </field>
          <field name="writerWrapText">
            <int>1</int>
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
        </record>
      </field>
      <field name="parserState">
        <record name="ParserState">
          <field name="stateParseRaw">
            <int>0</int>
          </field>
          <field name="stateParserContext">
            <data name="NullState" />
          </field>
          <field name="stateQuoteContext">
            <data name="NoQuote" />
          </field>
          <field name="stateSanitizeHTML">
            <int>0</int>
          </field>
          <field name="stateKeys">
            <list />
          </field>
          <field name="stateNotes">
            <list />
          </field>
          <field name="stateTabStop">
            <int>4</int>
          </field>
          <field name="stateStandalone">
            <int>0</int>
          </field>
          <field name="stateTitle">
            <list />
          </field>
          <field name="stateAuthors">
            <list />
          </field>
          <field name="stateDate">
            <list />
          </field>
          <field name="stateStrict">
            <int>0</int>
          </field>
          <field name="stateSmart">
            <int>0</int>
          </field>
          <field name="stateLiterateHaskell">
            <int>0</int>
          </field>
          <field name="stateColumns">
            <int>80</int>
          </field>
          <field name="stateHeaderTable">
            <list />
          </field>
          <field name="stateIndentedCodeClasses">
            <list />
          </field>
        </record>
      </field>
    </record>


# Other Interfaces

  * [libpandoc-dotnet][3]


# Configuration


[1]: http://www.haskell.org
[2]: http://johnmacfarlane.net/pandoc/
[3]: http://github.com/toyvo/libpandoc-dotnet