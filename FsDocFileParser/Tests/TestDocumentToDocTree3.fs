module TestDocumentToDocTree3

open Xunit
open DocTree1
open DocTree2
open DocTree3



let Is expectedOutput document =
    let tree1 = document |> DocumentToDocTree1
    let tree2 = tree1 |> DocTree1ToDocTree2
    let actualResult = tree2 |> DocTree2ToDocTree3
    let t = (actualResult = expectedOutput)
    // if not t then
    //     printfn "Failed with document: %A" documentArray
    //     printfn "Errant result is: %A" actualResult
    //     printfn "Where actual result expected by the test framework is: %A" actualResult
    Assert.True(t)



// ----------------------------------------------------------------------------------------------
//  EMPTY INPUT
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Empty input`` () = 
    [||] |> Is []
    
// ----------------------------------------------------------------------------------------------
//  Paragraph grouping
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Paragraph grouping`` () = 
    [| "This is a" ; "paragraph of two lines." |] 
        |> Is [DT3Paragraph("This is a paragraph of two lines.")]

[<Fact>]
let ``Paragraph grouping trailing space insignificant`` () = 
    [| "This is a    " ; "paragraph of two lines.   " |] 
        |> Is [DT3Paragraph("This is a paragraph of two lines.")]

[<Fact>]
let ``Paragraph grouping three lines`` () = 
    [| "This is a " ; "paragraph of" ; "three lines.   " |] 
        |> Is [DT3Paragraph("This is a paragraph of three lines.")]

// ----------------------------------------------------------------------------------------------
//  Paragraph grouping with indents
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Paragraph grouping with indents`` () = 
    [| 
        "An example."
        "    Indented paragraph"
        "    of two lines."
        "And back."
    |] 
        |> Is 
            [
                DT3Paragraph("An example.")
                DT3Indent([
                    DT3Paragraph("Indented paragraph of two lines.")
                ])
                DT3Paragraph("And back.")
            ]

[<Fact>]
let ``Paragraph grouping with indents with multiple blank lines`` () = 
    [| 
        "An example."
        "    Indented paragraph"
        "    of two lines."
        ""
        ""
        ""   // Reminder:  Multiple blank lines do NOT combine to page breaks in an indented context.
        ""
        ""
        ""
        "    Followed by a second paragraph."
        "And back."
    |] 
        |> Is 
            [
                DT3Paragraph("An example.")
                DT3Indent([
                    DT3Paragraph("Indented paragraph of two lines.")
                    DT3Paragraph("Followed by a second paragraph.")
                ])
                DT3Paragraph("And back.")
            ]

// ----------------------------------------------------------------------------------------------
//  Square bracketed line
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Square bracketed line`` () = 
    [| "[the link]" |] |> Is [ DT3SubstitutionDirective("[the link]") ]

[<Fact>]
let ``Square bracketed line trimming`` () = 
    [| "[the link]    " |] |> Is [ DT3SubstitutionDirective("[the link]") ]

// ----------------------------------------------------------------------------------------------
//  Titles with underlines
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Heading 1`` () = 
    [|
        "Introduction"
        "============"
    |] 
        |> Is [ DT3Heading(Heading1, "Introduction") ]

[<Fact>]
let ``Heading 2`` () = 
    [|
        "Examples"
        "--------"
    |] 
        |> Is [ DT3Heading(Heading2, "Examples") ]

[<Fact>]
let ``Not a Heading 1 when indented`` () = 
    [|
        "    Introduction"
        "    ============"
    |] 
        |> Is [ DT3Paragraph "Introduction" ; DT3Paragraph "============" ]

[<Fact>]
let ``Not a Heading 2 when indented`` () = 
    [|
        "    Examples"
        "    --------"
    |] 
        |> Is [ DT3Paragraph "Examples" ; DT3Paragraph "--------" ]

// ----------------------------------------------------------------------------------------------
//  Single line title
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Heading 3 single line title only applies at top level`` () = 
    [|
        "Examples"
    |] 
        |> Is [ DT3Heading(Heading3, "Examples") ]

[<Fact>]
let ``Not a single line title when indented`` () = 
    [|
        "    Indented single line"
    |] 
        |> Is [ DT3Indent([DT3Paragraph "Indented single line"]) ]

// ----------------------------------------------------------------------------------------------
//  Page breaks
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Page breaks`` () = 
    [| 
        "An example."
        "    Indented paragraph"
        "    of two lines."
        ""   // Reminder:  Multiple blank lines do NOT combine to page breaks in an indented context.
        ""
        "    Followed by a second paragraph."
        "And back."
        ""
        ""
        "On next page."
    |] 
        |> Is 
            [
                DT3Paragraph("An example.")
                DT3Indent([
                    DT3Paragraph("Indented paragraph of two lines.")
                    DT3Paragraph("Followed by a second paragraph.")
                ])
                DT3Paragraph("And back.")
                DT3PageBreak
                DT3Paragraph("On next page.")
            ]

// ----------------------------------------------------------------------------------------------
//  Preformatted
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Preformatted`` () = 
    [| 
        "If-expressions"
        ""
        "An example of if-expressions"
        "in the F# programming language."
        ""
        "F#:"
        ""
        "    if x > y then a else b"
        ""
        "Next paragraph."
    |] 
        |> Is 
            [
                DT3Heading(Heading3, "If-expressions")
                DT3Paragraph "An example of if-expressions in the F# programming language."
                DT3Paragraph "F#:"
                DT3Preformatted([
                    PreformattedString "if x > y then a else b"
                ])
                DT3Paragraph "Next paragraph."
            ]

[<Fact>]
let ``Preformatted 2`` () = 
    [| 
        "Match-expressions"
        ""
        "An example of match-expressions"
        "in the F# programming language."
        ""
        "F#:"
        ""
        "    let myList = if something then list1 else list2"
        ""
        "    match myList with"
        "        | head::tail -> (processed head)::(translated tail)"
        "        | [] -> []"
        ""
        "Next paragraph."
    |] 
        |> Is 
            [
                DT3Heading(Heading3, "Match-expressions")
                DT3Paragraph "An example of match-expressions in the F# programming language."
                DT3Paragraph "F#:"
                DT3Preformatted([
                    PreformattedString "let myList = if something then list1 else list2"
                    PreformattedString ""
                    PreformattedString "match myList with"
                    PreformattedString "    | head::tail -> (processed head)::(translated tail)"
                    PreformattedString "    | [] -> []"
                ])
                DT3Paragraph "Next paragraph."
            ]

// ----------------------------------------------------------------------------------------------
//  Bullets
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Bullets basic`` () = 
    [| 
        "- First case."
        "- Second case."
    |] 
        |> Is 
            [
                DT3Bullet ([DT3Paragraph "First case."])
                DT3Bullet ([DT3Paragraph "Second case."])
            ]

[<Fact>]
let ``Bullets with wrapping paragraph`` () = 
    [| 
        "- First case."
        "- Second case"
        "  which is longer."
        "- Third case."
    |] 
        |> Is 
            [
                DT3Bullet ([DT3Paragraph "First case."])
                DT3Bullet ([DT3Paragraph "Second case which is longer."])
                DT3Bullet ([DT3Paragraph "Third case."])
            ]

[<Fact>]
let ``Bullets with wrapping paragraph and blank lines`` () = 
    [| 
        "- First case."
        ""
        "- Second case"
        "  which is longer."
        ""
        "- Third case."
    |] 
        |> Is 
            [
                DT3Bullet ([DT3Paragraph "First case."])
                DT3Bullet ([DT3Paragraph "Second case which is longer."])
                DT3Bullet ([DT3Paragraph "Third case."])
            ]
