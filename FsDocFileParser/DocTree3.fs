module DocTree3

open DocTree2



type CodeCategoryName = CodeCategoryName of string
type PreformattedString = PreformattedString of string

type DocTree3 =
    | DT3EmptyLine
    | DT3SubstitutionDirective of string
    | DT3Code of CodeCategoryName * PreformattedString list
    | DT3Content of string
    | DT3PageBreak
    | DT3Indent of DocTree3 list
    | DT3Bullet of DocTree3 list




let LooksLikeSquareBracketed (str:string) =
    let str = str.Trim()
    str.Length > 1
        && str.[0] = '['
        && str.[str.Length-1] = ']'




let (|SquareBracketedLine|_|) treeList2 =
    match treeList2 with
        | DT2Content(line)::tail ->
            if line |> LooksLikeSquareBracketed then Some(line,tail) else None
        | _ -> None




let LookedUpIn lst (line:string) =
    let trimmedLine = line.TrimEnd()
    lst |> List.tryFind ((=) trimmedLine)



let ToPreformattedStringList treeList =

    let tab = "    "
    let bulletListIntroducer = "- "
    let bulletMember = "  "
    let emptyLine = PreformattedString("")

    let mutable accumulator = []

    let rec translate indentString item =
        match item with

            | DT2EmptyLine -> 
                accumulator <- emptyLine::accumulator

            | DT2Content(s) -> 
                accumulator <- PreformattedString((indentString + s))::accumulator

            | DT2Indent(indented) ->
                indented |> List.iter (translate (indentString + tab))

            | DT2Bullet(indented) ->
                let headPrefix = indentString + tab + bulletListIntroducer
                let tailPrefix = indentString + tab + bulletMember
                indented |> List.iteri (fun i item -> 
                    let prefix = (if i=0 then headPrefix else tailPrefix)
                    translate prefix item)

            | DT2PageBreak ->
                accumulator <- emptyLine::emptyLine::emptyLine::accumulator

    treeList |> List.iter (translate "")

    accumulator |> List.rev
    



let rec DocTree2ToDocTree3 codeIntroducersList treeList2 =

    let translated = DocTree2ToDocTree3 codeIntroducersList

    let (|CodeIntroducer|_|) treeList2 =
        match treeList2 with
            | DT2Content(line)::DT2EmptyLine::DT2Indent(codeContent)::tail ->
                match line |> LookedUpIn codeIntroducersList with
                    | Some(categoryName) -> Some(CodeCategoryName(categoryName), codeContent |> ToPreformattedStringList, tail)
                    | None -> None
            | _ -> None

    match treeList2 with

        //
        // Upgraded interpretations:
        //

        | SquareBracketedLine (line,tail) -> 
            let trimmedDirective = line.Trim()
            DT3SubstitutionDirective(trimmedDirective)::(translated tail)

        | CodeIntroducer (name, codeContent, tail) ->
            DT3Code(name, codeContent)::(translated tail)

        //
        // Fallback cases (1:1 translation with DT2):
        //
        
        | DT2EmptyLine::tail    ->  DT3EmptyLine::(translated tail)
        | DT2Content(str)::tail ->  DT3Content(str)::(translated tail)
        | DT2Indent(lst)::tail  ->  DT3Indent(translated lst)::(translated tail)
        | DT2Bullet(lst)::tail  ->  DT3Bullet(translated lst)::(translated tail)
        | DT2PageBreak::tail    ->  DT3PageBreak::(translated tail)

        | [] -> []






let ShowDocTree3List treeList =

    let tab = "--->"
    let bulletListIntroducer = "+ "
    let bulletMember = "> "
    let emptyLine = "~"

    let rec ShowDocTree3Item item indentString =
        match item with

            | DT3EmptyLine -> 
                printfn "EMPT:%s%s" indentString emptyLine

            | DT3Content(s) -> 
                printfn "TEXT:%s%s" indentString s

            | DT3Indent(indented) ->
                let newIndentString = indentString + tab
                indented |> List.iter (fun item ->
                    ShowDocTree3Item item newIndentString
                )

            | DT3Bullet(indented) ->
                printfn "BULL:%s%s%s" indentString tab bulletListIntroducer
                let newIndentString = indentString + tab + bulletMember
                indented |> List.iter (fun item ->
                    ShowDocTree3Item item newIndentString
                )

            | DT3PageBreak ->
                printfn "%sPAGE:--------------------- page break -----------------------" indentString

            | DT3SubstitutionDirective(directive) ->
                printfn "%sSDIR:%s" indentString directive

            | DT3Code(cat, preformattedText) ->
                let (CodeCategoryName(catString)) = cat
                printfn "%sCODE:%s" indentString catString
                preformattedText |> List.iter (fun item ->
                    let (PreformattedString(itemStr)) = item
                    printfn "%s    :%s%s" indentString catString itemStr)


    treeList |> List.iter (fun x -> ShowDocTree3Item x "v3: ")






(*





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

let LooksLikeSomethingNotCollectableIntoParagraph (str:string) =
    (str |> LooksLikeSingleLineTitle) || (str |> LooksLikeSquareBracketed)

let TitleWithUnderline pred treeList3 =
    match treeList3 with
        | DT3Content(title)::DT3Content(underline)::DT3EmptyLine::tail ->
            if pred underline then Some(title,tail) else None
        | DT3Content(title)::DT3Content(underline)::tail ->
            if pred underline then Some(title,tail) else None
        | _ -> None

let AsParagraphStringAndTail treeList3 =
    let mutable result = ""
    let mutable resultTail = treeList3
    let rec recurse lst =
        match lst with
            | DT3Content(line)::tail ->
                if result.Length = 0 then result <- line else result <- result + " " + line
                resultTail <- tail
                recurse tail
            | _ ->
                ()
    recurse treeList3
    match resultTail with
        | DT3EmptyLine::tail ->
            resultTail <- tail
        | _ ->
            ()
    result, resultTail





let (|Title1WithUnderline|_|) = TitleWithUnderline IsHeading1Underline
let (|Title2WithUnderline|_|) = TitleWithUnderline IsHeading2Underline

let (|SingleLineTitle|_|) treeList3 =
    match treeList3 with
        | DT3Content(title)::DT3EmptyLine::tail ->
            if title |> LooksLikeSingleLineTitle then Some(title,tail) else None
        | _ -> None

let (|MultiLineParagraph|_|) treeList3 =
    match treeList3 with
        | DT3Content _::_ -> Some(treeList3 |> AsParagraphStringAndTail)
        | _ -> None

            //if line |> LooksLikeSomethingNotCollectableIntoParagraph then None 
            //else Some(treeList3 |> AsParagraphStringAndTail)





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

        // | SquareBracketedLine (line,tail), _ ->
        //     printfn "<p><em>%s (<- TODO: substitute)</em></p>" (line |> EscapedForHtml)
        //     translated tail

        | MultiLineParagraph (paragraph,tail), 0 ->
            printfn "<p>%s</p>" (paragraph |> EscapedForHtml)
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
            translated lst  // not nested, so top-level presentation rules still apply within the bullet point!
            printfn "</li>"
            translated tail

        | DT3PageBreak::tail, _ ->
            printfn "<hr/>"
            translated tail

        | [], _ ->
            ()



let DocTree3ToHtml = DocTree3ToHtmlRec 0


*)
