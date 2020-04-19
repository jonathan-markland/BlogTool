module CSSLexer

open DocSlice
open ColourizationTokens

let IsCSSWhitespaceChar ch = (ch = ' ') || (ch = '\r') || (ch = '\n')
let IsCSSSelectorChar ch   = (System.Char.IsLetter ch) || (ch = '.') || ch = '#' || ch = '-'   // approximation
let IsCSSPropertyChar ch   = (System.Char.IsLetter ch) || ch = '-'   // approximation
let IsCSSDefinitionChar ch = ch <> '\x00' && ch <> ';' && ch <> '{' && ch <> '}'   && ch <> '<' && ch <> '>'

let IsHTMLNameChar ch      = (System.Char.IsLetter ch) || ch = '-'

let Whitespace = 
    ZeroOrMoreCharsWhere IsCSSWhitespaceChar

let (|Selector|_|) pos =
    let name  = pos |> ZeroOrMoreCharsWhere IsCSSSelectorChar
    let optws = name |> End |> Whitespace
    let brace = optws |> End |> CharWhere ((=) '{')
    if name |> HasContent && brace |> HasContent then
        Some(name, brace)
    else
        None

let (|Property|_|) pos =
    let name  = pos |> ZeroOrMoreCharsWhere IsCSSPropertyChar
    let optws = name |> End |> Whitespace
    let colon = optws |> End |> CharWhere ((=) ':')
    let body  = colon |> End |> ZeroOrMoreCharsWhere IsCSSDefinitionChar
    let semi  = body |> End |> CharWhere ((=) ';')
    if name |> HasContent && colon |> HasContent && body |> HasContent && semi |> HasContent then
        Some(name, colon, body, semi)
    else
        None

let (|CloseCurly|_|) pos =
    let slice = pos |> CharWhere ((=) '}')
    if slice |> HasContent then
        Some(slice)
    else
        None

let (|TagStart|_|) pos =
    let lessThan = pos |> CharWhere ((=) '<')
    let slash = lessThan |> End |> CharWhere ((=) '/')
    let name  = slash |> End |> ZeroOrMoreCharsWhere IsHTMLNameChar
    if lessThan |> HasContent && name |> HasContent then  // the slash '/' is optional
        Some(SliceBetweenPositions (lessThan |> Start) (name |> End))
    else
        None

let (|CloseTag|_|) pos =
    let slash = pos |> CharWhere ((=) '/')
    let close = slash |> End |> CharWhere ((=) '>')
    if close |> HasContent then  // slash '/' is optional, close '>' is required
        Some(SliceBetweenPositions (slash |> Start) (close |> End))
    else
        None

let LooksLikeSomethingColourable docPosition =

    // ie: NOT OrdinaryText!

    match docPosition with
        | Selector _   -> true
        | Property _   -> true
        | CloseCurly _ -> true
        | TagStart _   -> true
        | CloseTag _   -> true
        | _ -> false

let (|OrdinaryText|_|) docPosition =

    // TODO: This is an "everything until lexical rule matches" function.

    if docPosition |> IsAtEndOfDocument then
        None
    else if docPosition |> LooksLikeSomethingColourable then
        None
    else

        let Next (DocPosition(index,doc)) = 
            DocPosition(index+1,doc)

        let rec findEndPos pos =
            if pos |> IsAtEndOfDocument then
                pos
            else if pos |> LooksLikeSomethingColourable then
                pos
            else
                pos |> Next |> findEndPos

        let endPos = docPosition |> Next |> findEndPos

        Some( SliceBetweenPositions docPosition endPos )



let ParseNextCSSToken listSoFar docPosition =

    let start = docPosition

    // We just try them all, on the basis only one, or none, will work.

    match start with
        | Selector(name, brace) ->
            Some( (OpenBrace, brace)::(SelectorNamePart, name)::listSoFar, brace |> End )

        | Property(name, colon, body, semi) ->
            Some( (Semicolon, semi)::(DefinitionPart, body)::(Colon, colon)::(PropertyNamePart, name)::listSoFar, semi |> End )

        | CloseCurly(close) ->
            Some( (CloseBrace, close)::listSoFar, close |> End )

        | TagStart(tagIntro) ->
            Some( (HtmlTag, tagIntro)::listSoFar, tagIntro |> End )

        | CloseTag(tagEnd) ->
            Some( (HtmlTag, tagEnd)::listSoFar, tagEnd |> End )

        | OrdinaryText(txt) ->
            Some( (Text, txt)::listSoFar, txt |> End )

        | _ -> None


            
let CSSTokensFromString str =

    let startPosition = StartPositionOf str

    let rec recurse listSoFar currentPos =
        match ParseNextCSSToken listSoFar currentPos with
            | Some(newList, newPos) ->
                recurse newList newPos
            | None ->
                listSoFar, currentPos

    // TODO: Are we at end?

    recurse [] startPosition |> fst |> List.rev
