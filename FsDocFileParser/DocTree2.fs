module DocTree2

open DocTree1

// DocTree2
// --------
// Why this stage?
//
// DocTree2 adds the following innovations:
//
//     - Bullets
//     - Preformatted sections  (i.e.: code)
//     - Reduces multiple blank lines to a page break
//
// DocTree2 represents the above in a way that simplifies
// later processing stages by being more explicit, and thereby
// avoiding the potential for misinterpretation accidents.
//
// Bullets
// -------
// In the DT1Tree, bullets are represented by two list items,
// A DT1Content that starts with "- ", followed by a DT1Indent,
// which this section reduces to a DT2Bullet.  Alternatively,
// a DT1Content starting with "- " that has no indent is seen
// as a single-line indent, and also transformed to DT2Bullet.
//
// Preformatted sections
// ---------------------
// A line of text that ends with ':' is said to be an "interoducer"
// and if there is DT1Indent following, or DT1EmptyLine-DT1Indent,
// then the DT1Indent is transformed into a DT2Preformatted.  This
// indicates to later sections that the content should be treated
// more literally, as code, for instance.  Empty lines between
// lines of preformatted text are preserved.
//
// Page Breaks
// -----------
// A run of two or more DT1EmptyLines is replaced by a DT2PageBreak.
// This does NOT apply within a preformatted section.


// [ ] We don't have DT2Preformatted yet


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



let (|BulletFollowedByIndent|_|) treeList1 =
    match treeList1 with
        | DT1Content(thisLine)::DT1Indent(children)::tail ->
            if thisLine.StartsWith("- ") then
                Some(thisLine, children, tail)
            else
                None
        | _ -> 
            None



let (|BulletLine|_|) treeList1 =
    match treeList1 with
        | DT1Content(thisLine)::tail ->
            if thisLine.StartsWith("- ") then
                Some(thisLine, tail)
            else
                None
        | _ ->
            None



let rec DocTree1ToDocTree2 treeList1 =

    let translated = DocTree1ToDocTree2

    match treeList1 with
        
        | BulletFollowedByIndent(thisLine, children, tail) ->
            let bulletContent = DT2Content(thisLine |> WithBulletRemoved)::(translated children)
            DT2Bullet(bulletContent)::(translated tail)

        | BulletLine(thisLine, tail) ->
            DT2Bullet([DT2Content(thisLine |> WithBulletRemoved)])::(translated tail)

        | DT1Content(thisLine)::tail ->
            DT2Content(thisLine)::(translated tail)

        | DT1Indent(children)::tail ->
            DT2Indent(translated children)::(translated tail)

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



