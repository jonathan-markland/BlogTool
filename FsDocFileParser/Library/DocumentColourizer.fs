module DocumentColourizer

open DocSlice


let DocumentFromTokens colourTokens =
    match colourTokens with
        | (_, DocSlice(_,_,document))::_ -> Some(document)
        | [] -> None


/// Given a document (in a string) and its tokens, return a version
/// of the document with colourisation markup applied.
/// The tokens is a list of (token, string-slice) tuples where the 
/// token type is user-defined, but will classify the content of the
/// string slice.  The token type is used to determine the colour.
/// The colourizedBy function will be passed tokens and slice strings,
/// and must determine the colour, and return a mark-up string
/// containing the desired colour, and the text from the slice.
let ColourMarkedUpDocument tokens colourizedBy (document:string) =

    let builder = new System.Text.StringBuilder()
    let mutable copiedUntil = 0

    let flushUntil i =
        if copiedUntil < i then
            builder.Append(document, copiedUntil, i - copiedUntil) |> ignore
            copiedUntil <- i

    tokens |> List.iter (fun (colourIndicator:'colour, slice) ->
        let (DocSlice(startIndex,endIndex,_)) = slice
        flushUntil startIndex
        builder.Append((slice |> DocSliceToString |> colourizedBy colourIndicator):string) |> ignore
        copiedUntil <- endIndex
    )

    flushUntil (document.Length)

    builder.ToString()




