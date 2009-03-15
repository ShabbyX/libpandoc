#light
#r "Pandoc.dll"
printfn "%A" (Pandoc.Converter.Convert(Pandoc.Formats.MARKDOWN, null,
                                       Pandoc.Formats.HTML,     "{\"WrapText\": false}", 
                                       "[hello](http://hello.com)"));
