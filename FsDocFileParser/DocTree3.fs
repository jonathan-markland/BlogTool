module DocTree3

open DocTree2



type DocTree3 =
    | DT3EmptyLine
    | DT3Content of string
    | DT3PageBreak
    | DT3Indent of DocTree3 list
    | DT3Bullet of DocTree3 list



let rec DocTree2ToDocTree3 treeList2 =

    let translated = DocTree2ToDocTree3

    match treeList2 with
        
        | DT2EmptyLine::tail ->
            DT3EmptyLine::(translated tail)

        | DT2Content(str)::tail ->
            DT3Content(str)::(translated tail)

        | DT2Indent(lst)::tail ->
            DT3Indent(translated lst)::(translated tail)

        | DT2Bullet(lst)::tail ->
            DT3Bullet(translated lst)::(translated tail)

        | DT2PageBreak::tail ->
            DT3PageBreak::(translated tail)

        | [] ->
            []



let EscapedForHtml s = s



let rec DocTree3ToHtml treeList3 =

    let translated = DocTree3ToHtml

    match treeList3 with

        | DT3EmptyLine::tail ->
            printfn "<br/>"
            translated tail

        | DT3Content(str)::tail ->
            printfn "%s<br/>" (str |> EscapedForHtml)
            translated tail

        | DT3Indent(lst)::tail ->
            printfn "<blockquote>"
            translated lst
            printfn "</blockquote>"
            translated tail

        | DT3Bullet(lst)::tail ->
            printfn "<li>"
            translated lst
            printfn "</li>"
            translated tail

        | DT3PageBreak::tail ->
            printfn "<hr/>"
            translated tail

        | [] ->
            ()


