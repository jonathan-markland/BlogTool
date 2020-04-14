module StringLexer



/// Safely index a document and retrieve the next character.
/// We need to return the NUL char for the end of document.
let inline CharAt (n:int) (str:string) =
    if n < str.Length then str.[n] else '\x00'



/// With an index into a document string, return the column and line number.
/// Supports CR-LF, CR and LF line endings, even a complete mix!
/// Takes linear time, in the length of the document.
/// Intended for use only when generating a final error position report.
let LineAndColumnWithinString (document:string) posIndex =

    let inline charAt i = CharAt i document

    // Let 'i' be the index into 'document':
    let mutable i = posIndex - 1

    // The position 'pos' is on the current line, so establish
    // the column number by scanning backward:
    let isLineBreak ch = (ch = '\r') || (ch = '\n') || (ch = '\x00')
    let isScanTerminator ch = (isLineBreak ch) || (ch = '\x00')
    while not (isScanTerminator (charAt i)) do i <- i - 1
    let columnNumber = posIndex - i

    // Make a correction (don't consume the CR or LF when terminated on):
    if isLineBreak (charAt i) then i <- i + 1

    // Now scan back through the entire document counting line ends,
    // until we get to the start.  Support a MIXED document:
    let mutable lineNumber = 1

    let tryCRLF () =
        if charAt (i-1) = '\n' && charAt (i-2) = '\r' then
            i <- i - 2
            lineNumber <- lineNumber + 1
            true
        else false

    let tryLF () =
        if charAt (i-1) = '\n' then
            i <- i - 1
            lineNumber <- lineNumber + 1
            true
        else false
            
    let tryCR () =
        if charAt (i-1) = '\r' then
            i <- i - 1
            lineNumber <- lineNumber + 1
            true
        else false

    let tryReverseOneChar () =
        if i > 0 then
            i <- i - 1
            true
        else false

    // Note: Order of short-circuit tests is essential:
    while tryCRLF () || tryLF () || tryCR () || tryReverseOneChar () do ()

    // Return result as struct-tuple:
    struct (columnNumber, lineNumber)
