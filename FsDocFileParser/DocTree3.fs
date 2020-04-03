module DocTree3

open DocTree2



type DocTree3 =
    | DT3EmptyLine
    | DT3Content of string
    | DT3PageBreak
    | DT3Indent of DocTree3 list
    | DT3Bullet of DocTree3 list



let rec DocTree2ToDocTree3 treeList2 =

    let translated = DocTree2ToDocTree3

    match treeList2 with
        
        | DT2EmptyLine::tail ->
            DT3EmptyLine::(translated tail)

        | DT2Content(str)::tail ->
            DT3Content(str)::(translated tail)

        | DT2Indent(lst)::tail ->
            DT3Indent(translated lst)::(translated tail)

        | DT2Bullet(lst)::tail ->
            DT3Bullet(translated lst)::(translated tail)

        | DT2PageBreak::tail ->
            DT3PageBreak::(translated tail)

        | [] ->
            []



let EscapedForHtml (s:string) = 

    let Repl (searchChar:string) (replaceStr:string) (str:string) =
        // Ensure we create no garbage when there is nothing to replace:
        if str.Contains(searchChar) then str.Replace(searchChar, replaceStr) else str

    s |> Repl "&" "&amp;" |> Repl "<" "&lt;" |> Repl ">" "&gt;" |> Repl "\"" "&quot;"




let IsStringAll chr str =
    let mutable is = true
    for ch in str do is <- is && (ch=chr) 
    is



let IsHeading1Underline = IsStringAll '='
let IsHeading2Underline = IsStringAll '-'

let IsPunctuation ch = (ch = '.') || (ch = '!') || (ch = '?') || (ch = ':')



let LooksLikeSingleLineTitle (str:string) =
    str.Length > 1
        && str.[0] |> System.Char.IsLetterOrDigit
        && not (str.[str.Length-1] |> IsPunctuation)



let TitleWithUnderline pred treeList3 =
    match treeList3 with
        | DT3Content(title)::DT3Content(underline)::DT3EmptyLine::tail ->
            if pred underline then Some(title,tail) else None
        | DT3Content(title)::DT3Content(underline)::tail ->
            if pred underline then Some(title,tail) else None
        | _ -> None

let (|Title1WithUnderline|_|) = TitleWithUnderline IsHeading1Underline
let (|Title2WithUnderline|_|) = TitleWithUnderline IsHeading2Underline

let (|SingleLineTitle|_|) treeList3 =
    match treeList3 with
        | DT3Content(title)::DT3EmptyLine::tail ->
            if title |> LooksLikeSingleLineTitle then Some(title,tail) else None
        | _ -> None


let rec DocTree3ToHtmlRec nestLevel treeList3 =

    let translated = DocTree3ToHtmlRec nestLevel
    let nested = DocTree3ToHtmlRec (nestLevel + 1)

    match treeList3, nestLevel with

        //
        // Priority patterns:
        //

        | Title1WithUnderline (title,tail), 0 ->
            printfn "<H1>%s</H1>" (title |> EscapedForHtml)
            translated tail

        | Title2WithUnderline (title,tail), 0 ->
            printfn "<H2>%s</H2>" (title |> EscapedForHtml)
            translated tail

        | SingleLineTitle (title,tail), 0 ->
            printfn "<H3>%s</H3>" (title |> EscapedForHtml)
            translated tail

        //
        // Basic fallbacks:
        //

        | DT3EmptyLine::tail, _ ->
            printfn "<br/>"
            translated tail

        | DT3Content(str)::tail, _ ->
            printfn "%s<br/>" (str |> EscapedForHtml)
            translated tail

        | DT3Indent(lst)::tail, _ ->
            printfn "<blockquote>"
            nested lst
            printfn "</blockquote>"
            translated tail

        | DT3Bullet(lst)::tail, _ ->
            printfn "<li>"
            nested lst
            printfn "</li>"
            translated tail

        | DT3PageBreak::tail, _ ->
            printfn "<hr/>"
            translated tail

        | [], _ ->
            ()



let DocTree3ToHtml = DocTree3ToHtmlRec 0


