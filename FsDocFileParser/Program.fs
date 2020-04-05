open DocTree1
open DocTree2
open DocTree3



let MockDocument =
    [|
        "First line"
        "----------"
        "\tIndented first line"
        "\tIndented second line"
        ""
        "Second paragraph"
        "----------------"
        "\tIndented line 3"
        "\tIndented line 4"
        "\t\tIndented line 5"
        "\t\tIndented line 6"
        "\tIndented line 4a"
        ""
        ""
        ""
        ""
        "Third"
        "-----"
        "    - Four spaces line 1"
        "      Four spaces line 2"
        "      Four spaces line 3"
        "          Four spaces line 4 -- but indented further in"
        "    - Second Four spaces line 1"
        "      Second Four spaces line 2"
        "      Second Four spaces line 3"
        "    A final thing after the two bullet points"
        "  Two spaces line 1"
        "  Two spaces line 2"
        "Back to root"
    |]



let CodeIntroducers =
    [
        "Example:"
    ]



[<EntryPoint>]
let main argv =

    // let x1 = "[]" |> LooksLikeSquareBracketed
    // let x2 = "[adfgadfg]" |> LooksLikeSquareBracketed
    // let x3 = "[adfg" |> LooksLikeSquareBracketed
    // let x4 = "asdf]" |> LooksLikeSquareBracketed
    // let x5 = "" |> LooksLikeSquareBracketed
    // let x6 = "]" |> LooksLikeSquareBracketed
    // let x7 = " \t\t [adfgadfg]   " |> LooksLikeSquareBracketed

    let document = 
        System.IO.File.ReadAllLines(@"C:\Users\ukjmak\source\repos\FsDocFileParser\CSS.txt")

    let treeList = document |> DocumentToDocTree1
    treeList |> ShowDocTreeList

    //let tree2 = treeList |> DocTree1ToDocTree2
    ////tree2 |> ShowDocTree2List

    //let tree3 = tree2 |> DocTree2ToDocTree3 CodeIntroducers
    
    //tree3 |> ShowDocTree3List

    // tree3 |> DocTree3ToHtml

    0 // return an integer exit code
