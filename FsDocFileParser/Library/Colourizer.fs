module Colourizer

open ColourizationTokens
open DocumentColourizer
open HtmlEscaper



let ColouredUsingHtmlSpan colourIndicator str =

    let str = str |> EscapedForHTML

    match colourIndicator with
        | Black ->
            str
        | _     -> 
            let cssColour = sprintf "#%06x" (StandardColourFor colourIndicator)
            sprintf "<span style=\"color:%s;\">%s</span>" cssColour str



let ColouredUsingTags colourIndicator str =

    let tagName = 
        match colourIndicator with
            | White   -> Some("w")
            | Black   -> None
            | Grey1   -> Some("g1")
            | Grey2   -> Some("g2")
            | Red1    -> Some("r1")
            | Red2    -> Some("r2")
            | Orange1 -> Some("o1")
            | Orange2 -> Some("o2")
            | Yellow1 -> Some("y1")
            | Yellow2 -> Some("y2")
            | Green1  -> Some("g1")
            | Green2  -> Some("g2")
            | Cyan1   -> Some("c1")
            | Cyan2   -> Some("c2")
            | Blue1   -> Some("b1")
            | Blue2   -> Some("b2")
            | Pink1   -> Some("p1")
            | Pink2   -> Some("p2")

    let str = str |> EscapedForHTML

    match tagName with
        | None -> str
        | Some(tagName) -> sprintf "<%s>%s</%s>" tagName str tagName



/// Apply colourisation to a document contained within a string.
/// The colourisation will use span tags and style color.
let ColouredUpWithHtmlSpans colouredTokens = 
    match colouredTokens |> DocumentFromTokens with
        | None -> ""
        | Some(document) -> document |> ColourMarkedUpDocument colouredTokens ColouredUsingHtmlSpan



/// Apply colourisation to a document contained within a string.
/// The colourisation will use span tags and style color.
let ColouredUpWithUserDefinedTags colouredTokens = 
    match colouredTokens |> DocumentFromTokens with
        | None -> ""
        | Some(document) -> document |> ColourMarkedUpDocument colouredTokens ColouredUsingTags



