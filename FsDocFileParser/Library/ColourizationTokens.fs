module ColourizationTokens

open DocSlice

type ColourToken =

    /// The selector name part
    | SelectorNamePart

    /// The property name part 
    | PropertyNamePart

    /// The definition part 
    | DefinitionPart

    /// The open brace
    | OpenBrace

    /// The close brace
    | CloseBrace

    /// The colon
    | Colon

    /// The semicolon
    | Semicolon

    /// HTML tag part
    | HtmlTag

    /// Ordinary text
    | Text



let DocumentFromTokens colourTokens =
    match colourTokens with
        | (_, DocSlice(_,_,document))::_ -> Some(document)
        | [] -> None
