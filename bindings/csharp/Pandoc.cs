using System;
using System.Runtime.InteropServices; 
namespace Pandoc {

    public static class Formats
    {
	public const int MARKDOWN  = 0;
	public const int RST       = 1;	
	public const int HTML      = 2;
	public const int LATEX     = 3;
	public const int S5        = 4;
	public const int DOCBOOK   = 5;
	public const int ODT       = 6;
	public const int CONTEXT   = 7;
	public const int TEXINFO   = 8;
	public const int MAN       = 9;
	public const int MEDIAWIKI = 10;
	public const int RTF       = 11;
    }

    public static class Converter {

	public static string Convert(int    inputFormat,
				     string parserStateJSON,
				     int    outputFormat,      
				     string writerOptionsJSON,
				     string text) 
	{
	    return Converter.pandoc(inputFormat,
				    (((parserStateJSON == null)
				      ||
				      (parserStateJSON == ""))
				     ? "null"
				     : parserStateJSON),
				    outputFormat,
				    (((writerOptionsJSON == null)
				      ||
				      (writerOptionsJSON == ""))
				     ? "null"
				     : writerOptionsJSON),
				    text);
	}

	[DllImport ("libpandoc")]
	private static extern string pandoc(int    inputFormat, 
					    string parserStateJSON,
					    int    outputFormat,
					    string writerOptionsJSON,
					    string text);
    }

}
