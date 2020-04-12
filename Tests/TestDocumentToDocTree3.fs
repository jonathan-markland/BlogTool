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

// ----------------------------------------------------------------------------------------------
//  Single line title
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Heading 3 single line title`` () = 
    [|
        "Examples"
    |] 
        |> Is [ DT3Heading(Heading3, "Examples") ]

// ----------------------------------------------------------------------------------------------
//  1:1 stuff
// ----------------------------------------------------------------------------------------------

// oops move to DocTree2 to DocTree3 test file  [<Fact>]
// oops move to DocTree2 to DocTree3 test file  let ``Empty lines discarded`` () = 
// oops move to DocTree2 to DocTree3 test file      [| "" |] |> Is []
// oops move to DocTree2 to DocTree3 test file  
// oops move to DocTree2 to DocTree3 test file  [<Fact>]
// oops move to DocTree2 to DocTree3 test file  let ``Content becomes Paragraph`` () = 
// oops move to DocTree2 to DocTree3 test file      [| "content" |] |> Is [ DT3Paragraph "content" ]
        
//        | DT2Content(str)::tail      ->  DT3Paragraph(str)::(translated tail)
//        | DT2Indent(lst)::tail       ->  DT3Indent(translated lst)::(translated tail)
//        | DT2Preformatted(x)::tail   ->  DT3Preformatted(x)::(translated tail)
//        | DT2Bullet(lst)::tail       ->  DT3Bullet(translated lst)::(translated tail)
//        | DT2PageBreak::tail         ->  DT3PageBreak::(translated tail)
