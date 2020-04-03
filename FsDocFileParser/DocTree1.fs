module DocTree1


/// Document item representation, line-based, but 
/// using nesting for indentation.  Note that the document is
/// a *list* of these.
type DocTree1 =

    /// The document line is entirely whitespace.
    | DT1EmptyLine

    /// A line of text extracted from the document, with indentation
    /// whitespace removed.
    | DT1Content of string

    /// An indented section, represented by nesting.
    | DT1Indent of DocTree1 list



let private LeftSideIndentation (str:string) =
    let mutable i = 0
    let mutable x = 0
    let n = str.Length
    while i < n do
        if str.[i] = ' ' || str.[i] = '\t' then
            i <- i + 1
            x <- i
        else
            i <- n
    str.Substring(0, x)

 

let DocumentToDocTree1 (document:string[]) =

    let rowIsAtIncreasedIndent (this:string) (context:string) =
        this.Length > context.Length

    let rowIsAtSameLevelAsContext (this:string) (context:string) =
        this.Length = context.Length

    let rowIsEmpty row =
        row = (row |> LeftSideIndentation)

    let totalRowCount = document.Length

    let rec ScanRec (document:string[]) rowIndex contextWhitespace treeList =

        if rowIndex < totalRowCount then
            
            let thisRow = document.[rowIndex]
            let thisIndent = thisRow |> LeftSideIndentation

            if rowIsEmpty thisRow then
                let newTree = DT1EmptyLine::treeList
                ScanRec document (rowIndex+1) contextWhitespace newTree

            else if rowIsAtSameLevelAsContext thisIndent contextWhitespace then
                let rowTail = thisRow.Substring(thisIndent.Length, thisRow.Length - thisIndent.Length)
                let newTree = DT1Content(rowTail)::treeList
                ScanRec document (rowIndex+1) contextWhitespace newTree

            else if rowIsAtIncreasedIndent thisIndent contextWhitespace then
                let childTreeList, indexAfterChildren =
                    ScanRec document rowIndex thisIndent []  // let recursion case deal with it at the new contextual whitespace level
                let newTree = DT1Indent(childTreeList |> List.rev)::treeList
                ScanRec document indexAfterChildren contextWhitespace newTree

            else
                // Row is at DECREASED indent than our context -- return back to caller, let it handle it.
                treeList, rowIndex

        else
            // No further rows.
            treeList, rowIndex

    ScanRec document 0 "" [] |> fst |> List.rev




let ShowDocTreeList treeList =

    let rec ShowDocTreeItem indentString item =
        match item with

            | DT1EmptyLine -> 
                printfn "%s" indentString

            | DT1Content(s) -> 
                printfn "%s" (indentString + s)

            | DT1Indent(indented) ->
                indented |> List.iter (
                    ShowDocTreeItem (indentString + "    "))

    treeList |> List.iter (ShowDocTreeItem "// ")

