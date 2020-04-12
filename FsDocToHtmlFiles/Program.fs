open System

open DocTree1  // TODO: sort out modules.
open DocTree2
open DocTree3
open ListSplit


let escaped (s:string) = 

    let Repl (searchChar:string) (replaceStr:string) (str:string) =
        // Ensure we create no garbage when there is nothing to replace:
        if str.Contains(searchChar) then str.Replace(searchChar, replaceStr) else str

    s |> Repl "&" "&amp;" |> Repl "<" "&lt;" |> Repl ">" "&gt;" |> Repl "\"" "&quot;"




let rec PreToHtml lst = 
    let mutable str = ""
    lst |> List.iter (fun (PreformattedString(s)) -> str <- str + (s |> escaped) + "<br/>")
    str


let ToHtml2 treeList3 = 

    let rec ToHtml outerContext treeList3 =

        let raw name content tail =
            if outerContext = "li" then
                content + (tail |> ToHtml outerContext)
            else
                "<" + name + ">" + content + "</" + name + ">" + (tail |> ToHtml outerContext)

        let text name content tail =
            raw name (content |> escaped) tail

        let html name content tail =
            raw name (content |> ToHtml name) tail

        let headingTag level =
                match level with
                    | Heading1 -> "h1"
                    | Heading2 -> "h2"
                    | Heading3 -> "h3"

        match treeList3 with

            | DT3Bullet content::tail            -> html "li" content tail
            | DT3Indent content::tail            -> html "blockquote" content tail
            | DT3Paragraph content::tail         -> text "p" content tail
            | DT3Heading(level, content)::tail   -> text (headingTag level) content tail
            | DT3PageBreak::tail                 -> tail |> ToHtml outerContext
            | DT3SubstitutionDirective dir::tail -> text "p" dir tail
            | DT3Preformatted lst::tail          -> "<blockquote><pre>" + (lst |> PreToHtml) + "</pre></blockquote>" + (tail |> ToHtml outerContext)
            | [] -> ""


    ToHtml "" treeList3



[<EntryPoint>]
let main argv =

    let document = 
        System.IO.File.ReadAllLines(@"C:\Users\ukjmak\source\repos\FsDocFileParser\CSS.txt")
    
    let treeList = 
        document 
            |> DocumentToDocTree1
            |> DocTree1ToDocTree2
            |> DocTree2ToDocTree3

    let splitAtPageBreaks = treeList |> ListSplit ((=) DT3PageBreak)

    let listOfHtml = splitAtPageBreaks |> List.map ToHtml2

    listOfHtml |> List.iteri (fun i page -> 
        let path = sprintf @"C:\Users\ukjmak\source\repos\tmp\file%d.html" i
        System.IO.File.WriteAllText(path, page) )

    0 // return an integer exit code
