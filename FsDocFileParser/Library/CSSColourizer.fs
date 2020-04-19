module CSSColourizer

open ColourizationTokens
open DocumentColourizer
open HtmlEscaper



let ColouredHtmlSpan colourIndicator str =

    let cssColour =
        match colourIndicator with
            | ColourToken.SelectorNamePart -> Some("red")
            | ColourToken.DefinitionPart   -> Some("green")
            | ColourToken.PropertyNamePart -> Some("purple")
            | ColourToken.OpenBrace        -> Some("blue")
            | ColourToken.CloseBrace       -> Some("blue")
            | ColourToken.Semicolon        -> Some("blue")
            | ColourToken.Colon            -> Some("blue")
            | ColourToken.HtmlTag          -> Some("cyan")
            | ColourToken.Text             -> None

    let str = str |> EscapedForHTML

    match cssColour with
        | None -> str
        | Some(cssColour) -> sprintf "<span style=\"color:%s;\">%s</span>" cssColour str



let UserDefinedHtmlTagsForCss colourIndicator str =

    let tagName = 
        match colourIndicator with
            | ColourToken.SelectorNamePart -> Some("sel")
            | ColourToken.DefinitionPart   -> Some("def")
            | ColourToken.PropertyNamePart -> Some("prop")
            | ColourToken.OpenBrace        -> Some("open-brace")
            | ColourToken.CloseBrace       -> Some("close-brace")
            | ColourToken.Semicolon        -> Some("semi")
            | ColourToken.Colon            -> Some("colon")
            | ColourToken.HtmlTag          -> Some("tag")
            | ColourToken.Text             -> None

    let str = str |> EscapedForHTML

    match tagName with
        | None -> str
        | Some(tagName) -> sprintf "<%s>%s</%s>" tagName str tagName



/// Apply CSS colourisation to a document contained within a string.
/// The colourisation will use span tags and style color.
let CssColouredUpWithHtmlSpans cssTokens = 
    match cssTokens |> DocumentFromTokens with
        | None -> ""
        | Some(document) -> document |> ColourMarkedUpDocument cssTokens ColouredHtmlSpan



/// Apply CSS colourisation to a document contained within a string.
/// The colourisation will use span tags and style color.
let CssColouredUpWithUserDefinedTags cssTokens = 
    match cssTokens |> DocumentFromTokens with
        | None -> ""
        | Some(document) -> document |> ColourMarkedUpDocument cssTokens UserDefinedHtmlTagsForCss



