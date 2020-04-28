module Colourizer

open StandardColourPalette
open DocumentColourizer
open HtmlGeneration



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
            | White      -> Some("w")
            | Black      -> None
            | Grey       -> Some("g1")
            | DarkGrey   -> Some("g2")
            | Red        -> Some("r1")
            | DarkRed    -> Some("r2")
            | Orange     -> Some("o1")
            | DarkOrange -> Some("o2")
            | Yellow     -> Some("y1")
            | DarkYellow -> Some("y2")
            | Green      -> Some("g1")
            | DarkGreen  -> Some("g2")
            | Cyan       -> Some("c1")
            | DarkCyan   -> Some("c2")
            | Blue       -> Some("b1")
            | DarkBlue   -> Some("b2")
            | Pink       -> Some("p1")
            | DarkPink   -> Some("p2")

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



