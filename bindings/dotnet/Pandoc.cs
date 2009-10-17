namespace LibPandoc {

    using System;
    using System.IO;
    using System.Text;
    using System.Runtime.InteropServices;

    public enum Format
    {
        ConTeXt,
        Docbook,
        Html,
        HtmlWithLiterateHaskell,
        LaTeX,
        LaTeXWithLiterateHaskell,
        Man,
        Markdown,
        MarkdownWithLiterateHaskell,
        Mediawiki,
        Native,
        OpenDocumentTemplate,
        OpenDocument,
        ReStructuredText,
        ReStructuredTextWithLiterateHaskell,
        S5,
        Texinfo
    };

    public enum MathMethod
    {
        AsciiMathML = 0x00010,
        GladTeX     = 0x00020,
        JsMath      = 0x00040,
        LaTeXMathML = 0x00080,
        MimeTeX     = 0x00100
    };

    ///
    public enum EmailObfuscation
    {
        None       = 0x00001,
        JavaScript = 0x00002,
        References = 0x00004,
    };

    public static class Flags {
        public const int Incremental     = 0x00008;
        public const int NoWrap          = 0x00200;
        public const int NumberSections  = 0x00400;
        public const int ParseRaw        = 0x00800;
        public const int PreserveTabs    = 0x01000;
        public const int ReferenceLinks  = 0x02000;
        public const int SanitizeHtml    = 0x04000;
        public const int Smart           = 0x08000;
        public const int Standalone      = 0x10000;
        public const int Strict          = 0x20000;
        public const int TableOfContents = 0x40000;
    }

    delegate int Reader
        ([Out]
         [MarshalAs(UnmanagedType.LPArray,SizeParamIndex=1)]
         byte[] data,
         int length);

    delegate void Writer
        ([In]
         [MarshalAs(UnmanagedType.LPArray,SizeParamIndex=1)]
         byte[] data,
         int length);

    class Native
    {

        [DllImport("libpandoc")]
        public static extern void pandoc_init();

        [DllImport("libpandoc")]
        public static extern void pandoc_exit();

        [DllImport("libpandoc")]
        public static extern int pandoc(int inputFormat,
                                        int outputFormat,
                                        int flags,
                                        string options,
                                        Reader reader,
                                        Writer writer);
    }

    class Config
    {
        private int              flags;
        private EmailObfuscation obfuscation;
        private MathMethod       math;

        public Config()
        {
            flags = 0;
            obfuscation = EmailObfuscation.JavaScript;
            math        = MathMethod.AsciiMathML;
        }

        public EmailObfuscation EmailObfuscation
        {
            get { return obfuscation; }
            set { obfuscation = value; }
        }

        public MathMethod MathMethod
        {
            get { return math; }
            set { math = value; }
        }

        public int Flags
        {
            get { return flags; }
            set { flags = value; }
        }

        public int ToInt32() {
            return
                Convert.ToInt32(math) |
                Convert.ToInt32(obfuscation) |
                flags;
        }
    }

    class Pandoc : IDisposable
    {

        Pandoc()
        {
            Native.pandoc_init();
        }

        public void Process(Format source, Format target,
                            TextReader input, TextWriter output)
        {
            Process(source, target, new Config(), input, output);
        }

        public void Process(Format source,
                            Format target,
                            Config config,
                            TextReader input,
                            TextWriter output)
        {

            /// TODO: reduce memory consumption by streaming.
            var d = Encoding.UTF8.GetBytes(input.ReadToEnd());
            using (var i = new MemoryStream(d)) {
                Native.pandoc(Convert.ToInt32(source),
                              Convert.ToInt32(target),
                              config.ToInt32(),
                              /// TODO: options
                              "",
                              delegate (byte[] data, int length) {
                                  return i.Read(data, 0, length);
                              },
                              delegate (byte[] data, int length) {
                                  output.Write(Encoding.UTF8.GetChars(data));
                              });
            }
        }

        public void Dispose()
        {
            Native.pandoc_exit();
        }
    }
}

