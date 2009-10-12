#!/usr/bin/runhaskell
>
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.BuildPaths
> import Distribution.PackageDescription
> import qualified System.Directory as Directory
> import qualified System.FilePath as FilePath
>
> main = defaultMainWithHooks $ simpleUserHooks { instHook = myInstHook }
>
> myInstHook pkgDesc buildInfo userHooks installFlags = do
>     let dirs = absoluteInstallDirs pkgDesc buildInfo NoCopyDest
>         lib  = dynlibdir dirs
>         inc  = includedir dirs
>         exe  = exeName . head . executables $ pkgDesc
>         dll  = FilePath.replaceExtension exe dllExtension
>         (/)  = FilePath.combine
>         exeP = buildDir buildInfo / exe / exe
>         dllP = lib / dll
>         hdrP = inc / "pandoc.h"
>     putStr $ "Install " ++ dllP ++  " .. "
>     Directory.createDirectoryIfMissing True lib
>     Directory.copyFile exeP dllP
>     putStrLn "OK"
>     putStr $ "Install " ++ hdrP ++ " .. "
>     Directory.createDirectoryIfMissing True inc
>     Directory.copyFile "pandoc.h" hdrP
>     putStrLn "OK\n"
