open System

open DocTree1  // TODO: sort out modules.
open DocTree2
open DocTree3
open ListSplit


[<EntryPoint>]
let main argv =

    let document = 
        System.IO.File.ReadAllLines(@"C:\Users\ukjmak\source\repos\FsDocFileParser\CSS.txt")
    
    let treeList = 
        document 
            |> DocumentToDocTree1
            |> DocTree1ToDocTree2
            |> DocTree2ToDocTree3

    let atPageBreaks docTree3 =
        match docTree3 with
            | DT3PageBreak -> true
            | _ -> false

    let splitAtPageBreaks = treeList |> ListSplit atPageBreaks

    0 // return an integer exit code
