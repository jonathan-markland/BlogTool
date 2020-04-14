module CSSColourizer

open CSSTokens
open CSSLexer
open DocumentColourizer
open HtmlEscaper



let ColouredHtmlSpan colourIndicator str =

    let cssColour =
        match colourIndicator with
            | CSSToken.SelectorNamePart -> Some("red")
            | CSSToken.DefinitionPart   -> Some("green")
            | CSSToken.PropertyNamePart -> Some("purple")
            | CSSToken.OpenBrace        -> Some("blue")
            | CSSToken.CloseBrace       -> Some("blue")
            | CSSToken.Semicolon        -> Some("blue")
            | CSSToken.Colon            -> Some("blue")
            | CSSToken.HtmlTag          -> Some("cyan")
            | CSSToken.Text             -> None

    let str = str |> EscapedForHTML

    match cssColour with
        | None -> str
        | Some(cssColour) -> sprintf "<span style=\"color:%s;\">%s</span>" cssColour str



let UserDefinedHtmlTagsForCss colourIndicator str =

    let tagName = 
        match colourIndicator with
            | CSSToken.SelectorNamePart -> Some("sel")
            | CSSToken.DefinitionPart   -> Some("def")
            | CSSToken.PropertyNamePart -> Some("prop")
            | CSSToken.OpenBrace        -> Some("open-brace")
            | CSSToken.CloseBrace       -> Some("close-brace")
            | CSSToken.Semicolon        -> Some("semi")
            | CSSToken.Colon            -> Some("colon")
            | CSSToken.HtmlTag          -> Some("tag")
            | CSSToken.Text             -> None

    let str = str |> EscapedForHTML

    match tagName with
        | None -> str
        | Some(tagName) -> sprintf "<%s>%s</%s>" tagName str tagName



/// Apply CSS colourisation to a document contained within a string.
/// The colourisation will use span tags and style color.
let CssColouredUpWithHtmlSpans document = 

    let cssTokens = document |> CSSTokensFromString
    document |> ColourMarkedUpDocument cssTokens ColouredHtmlSpan



/// Apply CSS colourisation to a document contained within a string.
/// The colourisation will use span tags and style color.
let CssColouredUpWithUserDefinedTags document = 

    let cssTokens = document |> CSSTokensFromString
    document |> ColourMarkedUpDocument cssTokens UserDefinedHtmlTagsForCss



