module TestDocTree1ToDocTree2

open Xunit
open DocTree1
open DocTree2



let Is expectedOutput docTree1 =
    let actualResult = docTree1 |> DocTree1ToDocTree2
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
//  BASIC CASES
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Empty line 1:1 map`` () = 
    [ DT1EmptyLine ] |> Is [ DT2EmptyLine ]

[<Fact>]
let ``Content line 1:1 map`` () = 
    [ DT1Content "Hello" ] |> Is [ DT2Content "Hello" ]

[<Fact>]
let ``Indentation 1:1 map`` () = 
    [ DT1Indent ([DT1Content "Hello"]) ] |> Is [ DT2Indent ([DT2Content "Hello"]) ]

// ----------------------------------------------------------------------------------------------
//  PAGE BREAK
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Two or more empty lines translate to PageBreak`` () = 
    [ DT1EmptyLine ; DT1EmptyLine ] |> Is [ DT2PageBreak ]

[<Fact>]
let ``Two or more empty lines translate to PageBreak 2`` () = 
    [ DT1EmptyLine ; DT1EmptyLine ; DT1EmptyLine ] |> Is [ DT2PageBreak ]

[<Fact>]
let ``Two or more empty lines translate to PageBreak 3`` () = 
    [ DT1EmptyLine ; DT1EmptyLine ; DT1EmptyLine ; DT1EmptyLine ; DT1EmptyLine ; DT1EmptyLine ; DT1EmptyLine ] |> Is [ DT2PageBreak ]

// ----------------------------------------------------------------------------------------------
//  BULLET
// ----------------------------------------------------------------------------------------------

[<Fact>]
let ``Bullet line`` () = 
    [ DT1Content "- Hello" ]
        |> Is [ DT2Bullet ([ DT2Content "Hello" ]) ]

[<Fact>]
let ``Bullet lines`` () = 
    [ DT1Content "- Hello" ; DT1Content "- There" ]
        |> Is [ DT2Bullet ([ DT2Content "Hello" ]) ; DT2Bullet ([ DT2Content "There" ]) ]

[<Fact>]
let ``Bullet line steals following indent`` () = 
    [
        DT1Content "- Hello"
        DT1Indent ([DT1Content "This is extra"])
    ] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello" ; DT2Content "This is extra" ])
            ]

[<Fact>]
let ``Bullet line steals following indent only`` () = 

    // This is in the realms of trees that the parser "DocumentToDocTree1" 
    // wouldn't generate, so is only theoretical.

    [
        DT1Content "- Hello"
        DT1Indent ([DT1Content "This is extra"])
        DT1Indent ([DT1Content "Other indent"])
    ] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello" ; DT2Content "This is extra" ])
                DT2Indent ([ DT2Content "Other indent" ])
            ]

[<Fact>]
let ``Bullet line steals following indent only 2`` () = 

    // This is a companion for the above test.
    // It shows the parser wouldn't generate the above input anyway, FWIW.

    [|
        "- Hello"
        "  This is extra"
        "  Other indent"
    |] 
        |> TestDocumentToDocTree1.Is 
            [ 
                DT1Content "- Hello"
                DT1Indent ([ DT1Content "This is extra" ; DT1Content "Other indent" ])
            ]

[<Fact>]
let ``Multi bullets with single lines only`` () = 
    [
        DT1Content "- Hello 1"
        DT1Content "- Hello 2"
    ] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello 1" ])
                DT2Bullet ([ DT2Content "Hello 2" ])
            ]

[<Fact>]
let ``Multi bullets with indents`` () = 
    [
        DT1Content "- Hello 1"
        DT1Indent ([DT1Content "This is extra 1"])
        DT1Content "- Hello 2"
        DT1Indent ([DT1Content "This is extra 2"])
    ] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello 1" ; DT2Content "This is extra 1" ])
                DT2Bullet ([ DT2Content "Hello 2" ; DT2Content "This is extra 2" ])
            ]

[<Fact>]
let ``Empty line kept after bullet`` () = 
    [
        DT1Content "- Hello 1"
        DT1EmptyLine
        DT1Content "Other content"
    ] 
        |> Is 
            [ 
                DT2Bullet ([ DT2Content "Hello 1" ])
                DT2EmptyLine
                DT2Content "Other content"
            ]


