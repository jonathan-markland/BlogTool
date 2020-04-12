open System

open DocTree1  // TODO: sort out modules.
open DocTree2
open DocTree3
open ListSplit



let EscapedForHTML (rawString:string) = 

    let replacing (searchChar:string) (replaceStr:string) (str:string) =
        // Ensure we create no garbage when there is nothing to replace:
        if str.Contains(searchChar) then str.Replace(searchChar, replaceStr) else str

    rawString 
        |> replacing "&" "&amp;" 
        |> replacing "<" "&lt;" 
        |> replacing ">" "&gt;" 
        |> replacing "\"" "&quot;"



let ToTraditionalHTML treeList3 = 

    // TODO: This is not absolutely performant with the use of .Net string concatenations.

    let rec preformattedToHTML lst = 
        let mutable str = ""
        lst |> List.iter (fun (PreformattedString(s)) -> str <- str + (s |> EscapedForHTML) + "<br/>")
        str

    let rec recurse outerContext treeList3 =

        let raw name content tail =
            if outerContext = "li" then
                content + (tail |> recurse outerContext)
            else
                "<" + name + ">" + content + "</" + name + ">" + (tail |> recurse outerContext)

        let text name content tail =
            raw name (content |> EscapedForHTML) tail

        let html name content tail =
            raw name (content |> recurse name) tail

        let headingTag level =
            match level with
                | Heading1 -> "h1"
                | Heading2 -> "h2"
                | Heading3 -> "h3"

        match treeList3 with
            | DT3Bullet(content)::tail            -> html "li" content tail
            | DT3Indent(content)::tail            -> html "blockquote" content tail
            | DT3Paragraph(content)::tail         -> text "p" content tail
            | DT3Heading(level, content)::tail    -> text (headingTag level) content tail
            | DT3PageBreak::tail                  -> tail |> recurse outerContext
            | DT3SubstitutionDirective(dir)::tail -> text "p" dir tail
            | DT3Preformatted(lst)::tail          -> "<blockquote><pre>" + (lst |> preformattedToHTML) + "</pre></blockquote>" + (tail |> recurse outerContext)
            | [] -> ""


    recurse "" treeList3



[<EntryPoint>]
let main argv =

    let document = 
        System.IO.File.ReadAllLines(@"C:\Users\ukjmak\source\repos\BlogTool\FsDocFileParser\CSS.txt")
    
    let treeList = 
        document 
            |> DocumentToDocTree1
            |> DocTree1ToDocTree2
            |> DocTree2ToDocTree3

    let splitAtPageBreaks = treeList |> ListSplit ((=) DT3PageBreak)

    let listOfHtml = splitAtPageBreaks |> List.map ToTraditionalHTML

    listOfHtml |> List.iteri (fun i page -> 
        if page.Length > 0 then
            let path = sprintf @"C:\Users\ukjmak\source\repos\tmp\file%d.html" i
            System.IO.File.WriteAllText(path, page) )

    0 // return an integer exit code
