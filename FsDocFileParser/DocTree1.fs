module DocTree1

// DocTree1
// --------
// Why this stage?
//
// This processing pass scans the raw text file, a line at a time,
// building a DT1Tree.
//
// Its primary purpose is to remove whitespace indentation from the equation.
//
// The first purpose is to introduce a uniform representation for 
// EMPTY lines.  An empty line is the empty-string, or a line that has
// tabs and spaces and NOTHING else.  For such lines, we declare
// the spaces and tabs to be untrustworthy, and are therefore
// insignificant regarding determining the indentation.  For 
// reliability, only VISIBLE text content plays a part in determining 
// indentation structure.
//
// The second purpose is to strip left-side whitespace that is indentation.
// DocTree1 will have content rows (DT1Content) that have
// the left-side whitspace stripped, and thus will be raw text, and
// the indentation will be represented recursively by DT1Indent nodes.
//
// A third purpose is the resolve a subtlety of blank line ownership
// when a section "un-indents".  Any blank lines between the indented
// text and the unindented text should be siblings at the unindented level. 
//
//              Child end line
//                                                   | (a)
//                                                   | (b)
//        Parent level is here...
//
// So, blank lines (a) and (b) above should be siblings of the "Parent
// level", and NOT siblings after "Child end line".
//
// It is also intended that this will be relative project-agnostic,
// and could be re-used in future similar text processing projects
// without it including too many functions.


// [ ] TODO: WHen indented, we could do a lookahead for trailing blanks before an 'unindent', and return these to be included at the level of that unindent.



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

