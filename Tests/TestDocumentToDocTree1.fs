module TestDocumentToDocTree1

open Xunit
open DocTree1


let Is expectedOutput documentArray =
    let actualResult = documentArray |> DocumentToDocTree1
    let t = (actualResult = expectedOutput)
    // if not t then
    //     printfn "Failed with document: %A" documentArray
    //     printfn "Errant result is: %A" actualResult
    //     printfn "Where actual result expected by the test framework is: %A" actualResult
    Assert.True(t)



let IsWithVariousKindsOfBlank expectedOutput documentArray =
    
    let blankKinds =
        [
            ""
            " "
            "   "
            "\t"
            "\t\t"
            "  \t"
            "\t  "
            "  \t   \t  "
        ]

    let substitutedInto documentArray replacementBlank =
        documentArray |> Array.map (fun str -> if str = "" then replacementBlank else str)

    let runAllBlankKindsOver input =
        blankKinds |> List.iter (fun blank -> blank |> substitutedInto input |> Is expectedOutput)

    runAllBlankKindsOver documentArray



// ----------------------------------------------------------------------------------------------
//  EMPTY INPUT
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Empty input`` () = 
    [||] |> Is []
    
// ----------------------------------------------------------------------------------------------
//  WHITESPACE
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Single line empty`` () = 
    [| "" |] |> Is [ DT1EmptyLine ]

[<Fact>]
let ``Single line with single space`` () = 
    [| " " |] |> Is [ DT1EmptyLine ]

[<Fact>]
let ``Single line with single tab`` () = 
    [| "\t" |] |> Is [ DT1EmptyLine ]

[<Fact>]
let ``Single line with whitespace`` () = 
    [| "  \t \t\t  " |] |> Is [ DT1EmptyLine ]

// ----------------------------------------------------------------------------------------------
//  BASIC TOP LEVEL CONTENT
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Single line of content`` () = 
    [| "Hello" |] |> Is [ DT1Content "Hello" ]

[<Fact>]
let ``Two lines of content`` () = 
    [| "Hello" ; "There" |] |> Is [ DT1Content "Hello" ; DT1Content "There" ]

[<Fact>]
let ``Three lines of content with blank`` () = 
    [| "Hello" ; "" ; "There" |] |> Is [ DT1Content "Hello" ; DT1EmptyLine ; DT1Content "There" ]

// ----------------------------------------------------------------------------------------------
//  TRAILING AND LEADING BLANK LINES
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Leading blank line`` () = 
    [| "" ; "Text" |] |> IsWithVariousKindsOfBlank [ DT1EmptyLine ; DT1Content "Text" ]

[<Fact>]
let ``Trailing blank line`` () = 
    [| "Text" ; "" |] |> IsWithVariousKindsOfBlank [ DT1Content "Text" ; DT1EmptyLine ]

// ----------------------------------------------------------------------------------------------
//  INDENTATION
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Indentation outwards`` () = 
    [| "First" ; "    Second" |] |> Is [ DT1Content "First" ; DT1Indent ([ DT1Content "Second" ]) ]

[<Fact>]
let ``Indentation outwards and back`` () = 
    [| "First" ; "    Second" ; "Third" |] |> Is [ DT1Content "First" ; DT1Indent ([ DT1Content "Second" ]) ; DT1Content "Third" ]

// ----------------------------------------------------------------------------------------------
//  INDENTATION with BLANK LINE OWNERSHIP CONSIDERATIONS
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Indentation with blank within indent`` () = 
    [| 
        "1"
        "    2a"
        ""
        "    2b"
        "3" 
    |] 
        |> IsWithVariousKindsOfBlank
            [ 
                DT1Content "1"
                DT1Indent (
                    [ 
                        DT1Content "2a"
                        DT1EmptyLine
                        DT1Content "2b" 
                    ])
                DT1Content "3" 
            ]

[<Fact>]
let ``Blank before parent belongs to parent level`` () = 
    [| 
        "1"
        ""
        "    2a"
        ""
        "    2b"
        "3" 
    |] 
        |> IsWithVariousKindsOfBlank
            [ 
                DT1Content "1"
                DT1EmptyLine
                DT1Indent (
                    [ 
                        DT1Content "2a"
                        DT1EmptyLine
                        DT1Content "2b" 
                    ])
                DT1Content "3" 
            ]

[<Fact>]
let ``Blank after indent belongs to parent level`` () = 
    [| 
        "1"
        "    2a"
        ""
        "    2b"
        ""
        "3" 
    |] 
        |> IsWithVariousKindsOfBlank
            [ 
                DT1Content "1"
                DT1Indent (
                    [ 
                        DT1Content "2a"
                        DT1EmptyLine
                        DT1Content "2b" 
                    ])
                DT1EmptyLine
                DT1Content "3" 
            ]

[<Fact>]
let ``Blanks before and after indent belongs to parent level`` () = 
    [| 
        "1"
        ""
        ""
        ""
        "    2a"
        ""
        "    2b"
        ""
        ""
        "3" 
    |] 
        |> IsWithVariousKindsOfBlank
            [ 
                DT1Content "1"
                DT1EmptyLine
                DT1EmptyLine
                DT1EmptyLine
                DT1Indent (
                    [ 
                        DT1Content "2a"
                        DT1EmptyLine
                        DT1Content "2b" 
                    ])
                DT1EmptyLine
                DT1EmptyLine
                DT1Content "3" 
            ]

[<Fact>]
let ``Blanks belong to outer levels for multi indented`` () = 
    [| 
        "1" 
        "    2a"
        "    2b"
        ""
        "        3a"
        "        3b"
        ""
        ""
    |] 
        |> IsWithVariousKindsOfBlank
            [ 
                DT1Content "1"
                DT1Indent (
                    [ 
                        DT1Content "2a"
                        DT1Content "2b" 
                        DT1EmptyLine
                        DT1Indent (
                            [ 
                                DT1Content "3a"
                                DT1Content "3b" 
                            ])
                    ])
                DT1EmptyLine
                DT1EmptyLine
            ]

// ----------------------------------------------------------------------------------------------
//  NESTED INDENTATION
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Nested indentation`` () = 
    [| 
        "1"
        ""
        ""
        ""
        "    2a"
        ""
        "    2b"
        "        3a"
        "        3b"
        "        3c"
        ""
        "    2c"
        "        3d"
        "    2d"
        ""
        ""
        "4" 
    |] 
        |> IsWithVariousKindsOfBlank
            [ 
                DT1Content "1"
                DT1EmptyLine
                DT1EmptyLine
                DT1EmptyLine
                DT1Indent (
                    [ 
                        DT1Content "2a"
                        DT1EmptyLine
                        DT1Content "2b" 
                        DT1Indent (
                            [ 
                                DT1Content "3a"
                                DT1Content "3b" 
                                DT1Content "3c" 
                            ])
                        DT1EmptyLine
                        DT1Content "2c" 
                        DT1Indent (
                            [ 
                                DT1Content "3d"
                            ])
                        DT1Content "2d" 
                    ])
                DT1EmptyLine
                DT1EmptyLine
                DT1Content "4" 
            ]

// ----------------------------------------------------------------------------------------------
//  VISUALLY STRANGE INDENTATION / UNINDENTATION CASES
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Single indented line`` () = 
    [| 
        "    1"
    |] 
        |> Is
            [
                DT1Indent([ DT1Content "1" ])
            ]

[<Fact>]
let ``Indented document`` () = 
    [| 
        "    1"
        "    2"
        "    3"
        "    4"
    |] 
        |> Is
            [
                DT1Indent([ 
                    DT1Content "1" 
                    DT1Content "2" 
                    DT1Content "3" 
                    DT1Content "4" 
                ])
            ]

[<Fact>]
let ``Immediate indent then unindent`` () = 
    [| 
        "    1"
        "2"
    |] 
        |> Is
            [
                DT1Indent([ DT1Content "1" ])
                DT1Content "2"
            ]

[<Fact>]
let ``Going back to a nonexistent indentation level starts new indent at parent level`` () = 
    [| 
        "1"
        "    2"
        "  3"
    |] 
        |> Is
            [
                DT1Content "1"
                DT1Indent([ DT1Content "2" ])
                DT1Indent([ DT1Content "3" ])
            ]

