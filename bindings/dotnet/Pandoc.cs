namespace LibPandoc {

    using System;
    using System.IO;
    using System.Text;
    using System.Runtime.InteropServices;

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

    class Native {

        [DllImport("/usr/local/lib/libpandoc.so")]
        public static extern void pandoc_init();

        [DllImport("/usr/local/lib/libpandoc.so")]
        public static extern void pandoc_exit();

        [DllImport("/usr/local/lib/libpandoc.so")]
        public static extern int pandoc(int inputFormat,
                                        int outputFormat,
                                        int flags,
                                        string options,
                                        Reader reader,
                                        Writer writer);
    }

    class Pandoc : IDisposable {

        Pandoc() {
            Native.pandoc_init();
        }

        public void Process(int source, int target, int flags, string options,
                            TextReader input, TextWriter output) {
            /// TODO: reduce memory consumption by streaming.
            var d = Encoding.UTF8.GetBytes(input.ReadToEnd());
            using (var i = new MemoryStream(d)) {
                Native.pandoc(source, target, flags, options,
                              delegate (byte[] data, int length) {
                                  return i.Read(data, 0, length);
                              },
                              delegate (byte[] data, int length) {
                                  output.Write(Encoding.UTF8.GetChars(data));
                              });
            }
        }

        public void Dispose() { Native.pandoc_exit(); }


        public static void Main() {
            using (var p = new Pandoc()) {
                using (var r = new StringReader("Hello, [pandoc]()!")) {
                    using (var w = new StreamWriter(Console.OpenStandardOutput())) {
                        p.Process(7, 2, 0, "", r, w);
                    }
                }
            }
        }
    }
}

