module DocSlice

open StringLexer

[<Struct>]
type DocPosition = DocPosition of positionIndex:int * document:string

[<Struct>]
type DocSlice = DocSlice of startIndex:int * endIndex:int * document:string

/// Return the start position of a slice
let inline Start docSlice =
    let (DocSlice(startIndex,_,document)) = docSlice
    DocPosition(startIndex, document)

/// Return the end position of a slice
let inline End docSlice =
    let (DocSlice(_,endIndex,document)) = docSlice
    DocPosition(endIndex, document)

/// Return a DocPosition that lets us start parsing the content of a string.
let inline StartPositionOf (document:string) =
    DocPosition(0, document)

/// Returns an empty slice at the given document position.
let inline ToEmptySlice (DocPosition(index, document)) =
   DocSlice(index, index, document)

/// Returns a DocSlice between two positions.
/// It is assumed (but not checked) that it is the same document.
let inline SliceBetweenPositions (DocPosition(index1, document)) (DocPosition(index2, _)) =
    DocSlice(index1, index2, document)

/// Returns true if the two positions match, accounting for whitespace.
/// To advance past whitespace, the caller must pass a recogniser.
/// The positions must be within the same document string, but this is not checked.
/// This is commutative.
let IsPrettyMuchSamePositionAs (whitespaceRecogniser:DocPosition -> DocSlice) (pos1:DocPosition) (pos2:DocPosition) =
    let (DocSlice(_, i1, _)) = pos1 |> whitespaceRecogniser
    let (DocSlice(_, i2, _)) = pos2 |> whitespaceRecogniser
    i1 = i2

/// Asks if a position is at the precise end of the document.
let IsAtEndOfDocument docPosition =
    let (DocPosition (i, document)) = docPosition
    i >= document.Length  // only really need equality

/// If isFunc returns true for the character at docPosition, this
/// returns a slice of that character, else returns an empty slice
/// at the initial position if failed.
let inline CharWhere isFunc docPosition =
    let (DocPosition (initialPos, document)) = docPosition
    let newPos = if CharAt initialPos document |> isFunc then initialPos + 1 else initialPos
    DocSlice(initialPos, newPos, document)

/// Advance zero or more instances of characters matched by the
/// predicate 'isFunc', returning a slice of that if successful, or
/// an empty slice at the initial position if failed.
let inline ZeroOrMoreCharsWhere isFunc docPosition =
    let (DocPosition (initialPos, document)) = docPosition
    let mutable i = initialPos
    while CharAt i document |> isFunc do i <- i + 1
    DocSlice(initialPos, i, document)

/// Returns true if the DocSlice has content.
let inline HasContent docSlice =
    let (DocSlice (startIndex, endIndex, _)) = docSlice
    startIndex < endIndex

/// Extract the text between two positions as a separate string.
/// The positions must be within the same document instance.
let DocSliceToString docSlice =
    let (DocSlice (startIndex, endIndex, document)) = docSlice
    let extractionLength = endIndex - startIndex
    document.Substring(startIndex, extractionLength)

/// Returns true if the document text between two positions exactly
/// matches the given string.
let DocSliceMatchesText (expected:string) docSlice =
    let (DocSlice (startIndex, endIndex, document)) = docSlice
    if (endIndex - startIndex) = expected.Length then 
        let mutable i = startIndex
        let mutable j = 0
        let mutable matches = true
        while i < endIndex do
            if not (document.[i] = expected.[j]) then 
                matches <- false
                i <- endIndex
            else
                i <- i + 1
                j <- j + 1
        matches
    else false

/// Convert a DocPosition into a column and line number.
/// Supports CR-LF, CR and LF line endings, even a complete mix!
/// Takes linear time, in the length of the document.
/// Intended for use only when generating a final error position report.
let DocPositionLineAndColumn docPosition =
    match docPosition with
        | DocPosition(posIndex, document) ->
            LineAndColumnWithinString document posIndex


