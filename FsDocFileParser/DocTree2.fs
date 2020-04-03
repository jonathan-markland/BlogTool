module DocTree2

open DocTree1



/// Document item representation, line-based, with additional
/// indication of indents, bullet points, and page breaks.
/// Note that the document is a *list* of these.
type DocTree2 =

    /// The document line is entirely whitespace.
    | DT2EmptyLine

    /// A line of text extracted from the document, with indentation
    /// whitespace and bullet symbols removed.
    | DT2Content of string

    /// More than one consecutive whitespace line gives
    /// rise to a single page break.
    | DT2PageBreak

    /// An indented section, represented by nesting.
    | DT2Indent of DocTree2 list

    /// An bullet point section, represented by nesting.
    | DT2Bullet of DocTree2 list



let WithBulletRemoved (str:string) =
    str.Substring(2, str.Length-2)



let rec SkippingEmptyLines lst =
    match lst with
        | DT1EmptyLine::tail -> tail |> SkippingEmptyLines
        | _ -> lst



let rec DocTree1ToDocTree2 treeList1 =

    let translated = DocTree1ToDocTree2

    match treeList1 with
        
        | DT1Content(str)::DT1Indent(lst)::tail ->
            if str.StartsWith("- ") then
                DT2Bullet(DT2Content(str |> WithBulletRemoved)::(translated lst))
                    ::(translated tail)
            else
                DT2Content(str)
                    ::DT2Indent(translated lst)
                    ::(translated tail)

        | DT1Content(str)::tail ->
            if str.StartsWith("- ") then
                DT2Bullet([DT2Content(str |> WithBulletRemoved)])::(translated tail)
            else
                DT2Content(str)::(translated tail)

        | DT1Indent(lst)::tail ->
            DT2Indent(translated lst)::(translated tail)

        | DT1EmptyLine::DT1EmptyLine::tail ->
            DT2PageBreak::(tail |> SkippingEmptyLines |> translated)

        | DT1EmptyLine::tail ->
            DT2EmptyLine::(translated tail)

        | [] ->
            []



let ShowDocTree2List treeList =

    let tab = "--->"
    let bulletListIntroducer = "+ "
    let bulletMember = "> "
    let emptyLine = "~"

    let rec ShowDocTree2Item item indentString =
        match item with

            | DT2EmptyLine -> 
                printfn "%s%s" indentString emptyLine

            | DT2Content(s) -> 
                printfn "%s%s" indentString s

            | DT2Indent(indented) ->
                let newIndentString = indentString + tab
                indented |> List.iter (fun item ->
                    ShowDocTree2Item item newIndentString
                )

            | DT2Bullet(indented) ->
                printfn "%s%s%s" indentString tab bulletListIntroducer
                let newIndentString = indentString + tab + bulletMember
                indented |> List.iter (fun item ->
                    ShowDocTree2Item item newIndentString
                )

            | DT2PageBreak ->
                printfn "--------------------- page break -----------------------"

    treeList |> List.iter (fun x -> ShowDocTree2Item x "// ")



