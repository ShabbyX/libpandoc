namespace LibPandoc

open System
open System.IO
open System.Text
open System.Runtime.InteropServices

type Format =
    | Html 
    | Markdown 

    static member ToInt32 x = 
        match x with
        | Html     -> 2
        | Markdown -> 7

module private C = 

    type Reader = 
        delegate of
            ([<Out>]
             [<MarshalAs(UnmanagedType.LPArray,SizeParamIndex=1s)>] 
             data : byte[] * length : int) -> int

    type Writer = 
        delegate of 
            [<In>]
            [<MarshalAs(UnmanagedType.LPArray,SizeParamIndex=1s)>] 
            data : byte[] * length : int -> unit
        
    [<DllImport "/usr/local/lib/libpandoc.so">]
    extern void pandoc_init()

    [<DllImport "/usr/local/lib/libpandoc.so">]
    extern void pandoc_exit()

    [<DllImport "/usr/local/lib/libpandoc.so">]
    extern string pandoc(int inputFormat,
                         int outputFormat,
                         int flags,
                         string options,
                         Reader reader,
                         Writer writer)

type Pandoc() = 

    let () = C.pandoc_init()

    interface System.IDisposable with 
        member this.Dispose() = C.pandoc_exit()

    member this.Process(source : Format,
                        target : Format,
                        reader : TextReader,
                        writer : TextWriter) =
        /// TODO: reduce memory consumption by streaming
        let bytes =
            reader.ReadToEnd()
            |> Encoding.UTF8.GetBytes
        use input = new MemoryStream(bytes)        
        let r data length = input.Read(data, 0, length)
        let w data length = Encoding.UTF8.GetChars data |> writer.Write
        C.pandoc (int source, 
                  int target,
                  0, 
                  "", 
                  new C.Reader(r), 
                  new C.Writer(w))
        |> printfn "HASKELL SAID: %s"


module Main = 

    let text = @"
Hello There [google]()

This is *Markdown* `code`.
"

    let main () = 
        use p = new Pandoc()
        p.Process(Markdown, Html, new StringReader(text), stdout)

    do main ()

    // let Convert (source : Format)
    //             (target : Format) 
    //             (reader : TextReader) 
    //             (writer : TextWriter) : unit =
    //     pandoc_init()
    //     printfn "IN-HASKELL %i" (one())
    //     //pandoc_end()

    // do Convert Markdown Html stdin stdout
