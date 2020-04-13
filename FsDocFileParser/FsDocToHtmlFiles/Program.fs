open System

open ToHtmlFileSet



[<EntryPoint>]
let main argv =

    FileToTraditionalHTMLFileSet
        @"C:\Users\ukjmak\source\repos\tmp"
        @"C:\Users\ukjmak\source\repos\BlogTool\FsDocFileParser\CSS.txt"

    0 // return an integer exit code
