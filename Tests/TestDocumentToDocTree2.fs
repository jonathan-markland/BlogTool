module TestDocumentToDocTree2

open Xunit
open DocTree1
open DocTree2



let Is expectedOutput document =
    let tree1 = document |> DocumentToDocTree1
    let actualResult = tree1 |> DocTree1ToDocTree2
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
//  BASIC CASES
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Empty line 1:1 map`` () = 
    [| "" |] |> Is [ DT2EmptyLine ]

[<Fact>]
let ``Content line 1:1 map`` () = 
    [| "Hello" |] |> Is [ DT2Content "Hello" ]

[<Fact>]
let ``Indentation 1:1 map`` () = 
    [| "    Hello" |] |> Is [ DT2Indent ([DT2Content "Hello"]) ]

// ----------------------------------------------------------------------------------------------
//  PAGE BREAK
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Two or more empty lines translate to PageBreak`` () = 
    [| "" ; "" |] |> Is [ DT2PageBreak ]

[<Fact>]
let ``Two or more empty lines translate to PageBreak 2`` () = 
    [| "" ; "" ; "" |] |> Is [ DT2PageBreak ]

[<Fact>]
let ``Two or more empty lines translate to PageBreak 3`` () = 
    [| "" ; "" ; "" ; "" ; "" ; "" ; "" |] |> Is [ DT2PageBreak ]

// ----------------------------------------------------------------------------------------------
//  INDENT
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Indent line`` () = 
    [|
        "Hello"
        "    Indented"
    |]
        |> Is [ DT2Content "Hello" ; DT2Indent ([ DT2Content "Indented" ]) ]

[<Fact>]
let ``Indented three lines`` () = 
    [|
        "Hello"
        "    Indent 1"
        "    Indent 2"
        "    Indent 3"
    |]
        |> Is
            [ 
                DT2Content "Hello"
                DT2Indent ([ DT2Content "Indent 1" ; DT2Content "Indent 2" ; DT2Content "Indent 3" ])
            ]
    
[<Fact>]
let ``Indented sections`` () = 
    [|
        "Main 1"
        "    Indent 1"
        "    Indent 2"
        "    Indent 3"
        "Main 2"
        "    Indent 4"
        "    Indent 5"
        "Main 3"
        "    Indent 6"
        "    Indent 7"
    |]
        |> Is
            [ 
                DT2Content "Main 1"
                DT2Indent ([ DT2Content "Indent 1" ; DT2Content "Indent 2" ; DT2Content "Indent 3" ])
                DT2Content "Main 2"
                DT2Indent ([ DT2Content "Indent 4" ; DT2Content "Indent 5" ])
                DT2Content "Main 3"
                DT2Indent ([ DT2Content "Indent 6" ; DT2Content "Indent 7" ])
            ]
    
[<Fact>]
let ``Indented sections where empty lines appear at parent level`` () = 
    [|
        "Main 1"
        ""
        "    Indent 1"
        "    Indent 2"
        "    Indent 3"
        "Main 2"
        "    Indent 4"
        "    Indent 5"
        ""
        "Main 3"
        "    Indent 6"
        "    Indent 7"
        ""
        "        Second Indent 8"
        "" // does not appear at parent level
        "        Second Indent 9"
        ""
    |]
        |> Is
            [ 
                DT2Content "Main 1"
                DT2EmptyLine
                DT2Indent ([ DT2Content "Indent 1" ; DT2Content "Indent 2" ; DT2Content "Indent 3" ])
                DT2Content "Main 2"
                DT2Indent ([ DT2Content "Indent 4" ; DT2Content "Indent 5" ])
                DT2EmptyLine
                DT2Content "Main 3"
                DT2Indent (
                    [ 
                        DT2Content "Indent 6"
                        DT2Content "Indent 7"
                        DT2EmptyLine
                        DT2Indent ([ DT2Content "Second Indent 8" ; DT2EmptyLine ; DT2Content "Second Indent 9" ])
                    ])
                DT2EmptyLine
                
            ]
    
// ----------------------------------------------------------------------------------------------
//  BULLET
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Bullet line`` () = 
    [| "- Hello" |]
        |> Is [ DT2Bullet ([ DT2Content "Hello" ]) ]

[<Fact>]
let ``Bullet lines`` () = 
    [| "- Hello" ; "- There" |]
        |> Is [ DT2Bullet ([ DT2Content "Hello" ]) ; DT2Bullet ([ DT2Content "There" ]) ]

[<Fact>]
let ``Bullet line steals following indent`` () = 
    [|
        "- Hello"
        "  This is extra"
    |] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello" ; DT2Content "This is extra" ])
            ]

[<Fact>]
let ``Multi bullets with single lines only`` () = 
    [|
        "- Hello 1"
        "- Hello 2"
    |] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello 1" ])
                DT2Bullet ([ DT2Content "Hello 2" ])
            ]

[<Fact>]
let ``Multi bullets with indents`` () = 
    [|
        "- Hello 1"
        "  This is extra 1"
        "- Hello 2"
        "  This is extra 2"
    |] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello 1" ; DT2Content "This is extra 1" ])
                DT2Bullet ([ DT2Content "Hello 2" ; DT2Content "This is extra 2" ])
            ]

[<Fact>]
let ``Empty line kept after bullet`` () = 
    [|
        "- Hello 1"
        ""
        "Other content"
    |] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello 1" ])
                DT2EmptyLine
                DT2Content "Other content"
            ]

[<Fact>]
let ``Bullets within bullets`` () = 
    [|
        "- Hello 1"
        "  This is extra 1"
        "- Hello 2"
        "  This is extra 2"
        "  - Hello 3"
        "  - Hello 4"   // The bullet '-' characters must be in line with the text above, or else it's an additional indent.
    |] 
        |> Is 
            [ 
                DT2Bullet (
                    [ 
                        DT2Content "Hello 1"
                        DT2Content "This is extra 1"
                    ])
                DT2Bullet (
                    [ 
                        DT2Content "Hello 2"
                        DT2Content "This is extra 2" 
                        DT2Bullet ([ DT2Content "Hello 3" ])
                        DT2Bullet ([ DT2Content "Hello 4" ])
                    ])
            ]

// ----------------------------------------------------------------------------------------------
//  INDENTS and BULLET
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Indented bullets within bullets`` () = 
    [|
        "aaa"
        "    bbb1"
        "    bbb2"
        "    - ccc"
        "    - ddd"
        "      eee"
        "          fff"
        "          fff"
        "    - ggg"
    |] 
        |> Is 
            [ 
                DT2Content "aaa"
                DT2Indent (
                    [ 
                        DT2Content "bbb1"
                        DT2Content "bbb2"
                        DT2Bullet([ DT2Content "ccc" ])
                        DT2Bullet(
                            [ 
                                DT2Content "ddd"
                                DT2Content "eee"
                                DT2Indent(
                                    [ 
                                        DT2Content "fff"
                                        DT2Content "fff"
                                    ])
                            ])
                        DT2Bullet([ DT2Content "ggg" ])
                    ])
            ]

[<Fact>]
let ``Indented bullets within bullets with additional blank lines`` () = 
    [|
        "aaa"
        ""
        "    bbb1"
        "    bbb2"
        ""
        "    - ccc"
        ""
        "    - ddd"
        "      eee"
        "          fff"
        ""
        "          fff"
        "    - ggg"
    |] 
        |> Is 
            [ 
                DT2Content "aaa"
                DT2EmptyLine
                DT2Indent (
                    [ 
                        DT2Content "bbb1"
                        DT2Content "bbb2"
                        DT2EmptyLine
                        DT2Bullet([ DT2Content "ccc" ])
                        DT2EmptyLine
                        DT2Bullet(
                            [ 
                                DT2Content "ddd"
                                DT2Content "eee"
                                DT2Indent(
                                    [ 
                                        DT2Content "fff"
                                        DT2EmptyLine
                                        DT2Content "fff"
                                    ])
                            ])
                        DT2Bullet([ DT2Content "ggg" ])
                    ])
            ]
