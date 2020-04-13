module DocTree3

open DocTree2
open StringClassifiers

// DocTree3
// --------
// Why this stage?
//
// DocTree3 is responsible for identifying:
//
//     - Titles with underlines using '=' or '-'.
//     - Titles which are a single line of text without ending punctuation.
//     - Lines surrounded with square brackets [ ] as directives (image/link substitution points).
//     - Concatenating lines into paragraphs, as the line ends
//       are not stylistically significant.
//
// The above subsitutions do NOT apply within DT2Preformatted sections.


// ----------------------------------------------------------------------------------------------
//  Tree types
// ----------------------------------------------------------------------------------------------

type HeadingLevel = Heading1 | Heading2 | Heading3

type DocTree3 =
    | DT3Heading of HeadingLevel * string
    | DT3Paragraph of string
    | DT3Indent of DocTree3 list
    | DT3Bullet of DocTree3 list
    | DT3Preformatted of PreformattedString list
    | DT3SubstitutionDirective of string
    | DT3PageBreak


// ----------------------------------------------------------------------------------------------
//  Square Bracketed Line
// ----------------------------------------------------------------------------------------------

let (|SquareBracketedLine|_|) treeList2 =
    match treeList2 with

        | DT2Content(line)::tail ->
            if line |> LooksLikeSquareBracketed then Some(line,tail) else None

        | _ -> None


// ----------------------------------------------------------------------------------------------
//  Underlined titles
// ----------------------------------------------------------------------------------------------

let TitleWithUnderline pred treeList2 =
    match treeList2 with

        | DT2Content(title)::DT2Content(underline)::DT2EmptyLine::tail ->
            if pred underline then Some(title,tail) else None
    
        | DT2Content(title)::DT2Content(underline)::tail ->
            if pred underline then Some(title,tail) else None

        | _ -> None

let (|Title1WithUnderline|_|) = TitleWithUnderline IsHeading1Underline
let (|Title2WithUnderline|_|) = TitleWithUnderline IsHeading2Underline


// ----------------------------------------------------------------------------------------------
//  Paragraph collection
// ----------------------------------------------------------------------------------------------

let AsParagraphStringAndTail treeList2 =
    let mutable result = ""
    let mutable resultTail = treeList2
    let rec recurse lst =
        match lst with
            | DT2Content(line)::tail ->
                if result.Length = 0 then result <- line.Trim() else result <- result + " " + line.Trim()
                resultTail <- tail
                recurse tail
            | _ ->
                ()
    recurse treeList2
    match resultTail with
        | DT2EmptyLine::tail ->
            resultTail <- tail
        | _ ->
            ()
    result, resultTail


let (|Paragraph|_|) treeList2 =
    match treeList2 with
        | DT2Content _::_ -> Some(treeList2 |> AsParagraphStringAndTail)
        | _ -> None


// ----------------------------------------------------------------------------------------------
//  Translation
// ----------------------------------------------------------------------------------------------

type Level = Outer | Nested

let rec ConvertedMainlyToDocTree3 nestLevel treeList2 =

    let withNested = ConvertedMainlyToDocTree3 Nested
    let withTail   = ConvertedMainlyToDocTree3 nestLevel

    match treeList2, nestLevel with

        //
        // Upgraded interpretations:
        //

        | SquareBracketedLine (line,tail), _ -> 
            let trimmedDirective = line.Trim()
            DT3SubstitutionDirective(trimmedDirective)::(withTail tail)

        | Title1WithUnderline(title,tail), Outer ->
            DT3Heading(Heading1, title)::(withTail tail)

        | Title2WithUnderline(title,tail), Outer ->
            DT3Heading(Heading2, title)::(withTail tail)

        | Paragraph(content,tail), _ ->
            DT3Paragraph(content)::(withTail tail)

        // Reminder:  Single line titles done by caller!

        //
        // Fallback cases (1:1 translation with DT2):
        //
        
        | DT2EmptyLine::tail, _         ->  withTail tail
        | DT2Content(str)::tail, _      ->  DT3Paragraph(str)::(withTail tail)
        | DT2Indent(lst)::tail, _       ->  DT3Indent(withNested lst)::(withTail tail)
        | DT2Preformatted(x)::tail, _   ->  DT3Preformatted(x)::(withTail tail)
        | DT2Bullet(lst)::tail, _       ->  DT3Bullet(withNested lst)::(withTail tail)
        | DT2PageBreak::tail, _         ->  DT3PageBreak::(withTail tail)
        | [], _ -> []



let rec WithSingleLineTitlesRatherThanParagraphs treeList3 =
    
    treeList3 |> List.map (fun item ->
        match item with
            | DT3Paragraph(content) ->
                if content |> LooksLikeSingleLineTitle then
                    DT3Heading(Heading3, content)
                else
                    item
            | DT3Bullet(children) -> DT3Bullet(children)
            | DT3Indent(children) -> DT3Indent(children)
            | _ -> item
        )



let DocTree2ToDocTree3 treeList2 =

    treeList2 
        |> ConvertedMainlyToDocTree3 Outer
        |> WithSingleLineTitlesRatherThanParagraphs

