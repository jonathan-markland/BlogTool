module TestDocTree2ToDocTree3

open Xunit
open DocTree2
open DocTree3

let Is expectedOutput docTree2 =
    let actualResult = docTree2 |> DocTree2ToDocTree3
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
    [] |> Is []
    
// ----------------------------------------------------------------------------------------------
//  Square bracketed directive
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Square brackets directive`` () = 
    [
        DT2Content "[directive]   "
    ] |> Is 
        [
            DT3SubstitutionDirective "[directive]"
        ]

// ----------------------------------------------------------------------------------------------
//  Title 1 with underline
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Title 1 with underline`` () = 
    [
        DT2Content "Introduction"
        DT2Content "============"
    ] |> Is 
        [
            DT3Heading(Heading1, "Introduction")
        ]

// ----------------------------------------------------------------------------------------------
//  Title 2 with underline
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Title 2 with underline`` () = 
    [
        DT2Content "Introduction"
        DT2Content "------------"
    ] |> Is 
        [
            DT3Heading(Heading2, "Introduction")
        ]

// ----------------------------------------------------------------------------------------------
//  Paragraphs
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Lines to paragraph joining`` () = 
    [
        DT2Content "First line"
        DT2Content "and second line."
    ] |> Is 
        [
            DT3Paragraph("First line and second line.")
        ]

[<Fact>]
let ``Three lines to paragraph joining`` () = 
    [
        DT2Content "First line"
        DT2Content "and second line"
        DT2Content "and third line."
    ] |> Is 
        [
            DT3Paragraph("First line and second line and third line.")
        ]

// ----------------------------------------------------------------------------------------------
//  Single line titles
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Title 3 single line title`` () = 
    [
        DT2Content "Introduction"
    ] |> Is 
        [
            DT3Heading(Heading3, "Introduction")
        ]

// ----------------------------------------------------------------------------------------------
//  Empty lines get discarded
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Empty lines get discarded`` () = 
    [
        DT2Content ""
    ] |> Is 
        [
        ]

// ----------------------------------------------------------------------------------------------
//  Empty lines and paragraph relationship
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Empty line splits paragraphs`` () = 
    [
        DT2Content "First paragraph"
        DT2Content "goes here."
        DT2Content ""
        DT2Content "Second paragraph"
        DT2Content "goes here."
    ] |> Is 
        [
            DT3Paragraph "First paragraph goes here."
            DT3Paragraph "Second paragraph goes here."
        ]

[<Fact>]
let ``Empty line splits two paragraphs`` () = 
    [
        DT2Content "First paragraph"
        DT2Content "goes here."
        DT2Content ""
        DT2Content "Second paragraph"
        DT2Content "goes here."
        DT2Content ""
        DT2Content "Third paragraph"
        DT2Content "goes here."
    ] |> Is 
        [
            DT3Paragraph "First paragraph goes here."
            DT3Paragraph "Second paragraph goes here."
            DT3Paragraph "Third paragraph goes here."
        ]

(*

        //
        // Fallback cases (1:1 translation with DT2):
        //
        
        | DT2EmptyLine::tail         ->  translated tail
        | DT2Content(str)::tail      ->  DT3Paragraph(str)::(translated tail)
        | DT2Indent(lst)::tail       ->  DT3Indent(translated lst)::(translated tail)
        | DT2Preformatted(x)::tail   ->  DT3Preformatted(x)::(translated tail)
        | DT2Bullet(lst)::tail       ->  DT3Bullet(translated lst)::(translated tail)
        | DT2PageBreak::tail         ->  DT3PageBreak::(translated tail)
*)