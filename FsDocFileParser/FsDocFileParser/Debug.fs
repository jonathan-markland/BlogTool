module Debug

open DocTree1
open DocTree2


let ShowDocTreeList treeList =

    let rec ShowDocTreeItem indentString item =
        match item with

            | DT1EmptyLine -> 
                printfn "DT1EmptyLine:%s" indentString

            | DT1Content(s) -> 
                printfn "DT1Content  :%s" (indentString + s)

            | DT1Indent(indented) ->
                let newIndent = indentString + "    "
                printfn "-> DT1Indent:%sv" newIndent
                indented |> List.iter (
                    ShowDocTreeItem newIndent)
                printfn "<-"

    treeList |> List.iter (ShowDocTreeItem "")




/// Show a representation of the DocTree2 tree.
/// Intended for debug-examination of the tree.
let ShowDocTree2List treeList =

    let initialIndent = "// "
    let tab = "--->"
    let bulletListIntroducer = "+ "
    let preformattedIntroducer = "# "
    let bulletMember = "> "
    let emptyLine = "~"

    let rec ShowDocTree2Item item indentString =
        match item with

            | DT2EmptyLine -> 
                printfn "%s%s" indentString emptyLine

            | DT2Content(s) -> 
                printfn "%s%s" indentString s

            | DT2Indent(indented) ->
                let newIndentString = indentString + tab
                indented |> List.iter (fun item ->
                    ShowDocTree2Item item newIndentString
                )

            | DT2Bullet(indented) ->
                printfn "%s%s%s" indentString tab bulletListIntroducer
                let newIndentString = indentString + tab + bulletMember
                indented |> List.iter (fun item ->
                    ShowDocTree2Item item newIndentString
                )

            | DT2Preformatted(content) ->
                content 
                    |> List.iter (fun (PreformattedString(item)) -> 
                        printfn "%s%s%s%s" indentString tab preformattedIntroducer item)

            | DT2PageBreak ->
                if indentString = initialIndent then
                    printfn "--------------------- page break -----------------------"
                else
                    printfn "--------------------- page break ** WARNING:  within child sub-tree ** -----------------------"

    treeList |> List.iter (fun x -> ShowDocTree2Item x initialIndent)




