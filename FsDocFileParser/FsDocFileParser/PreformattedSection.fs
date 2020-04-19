module PreformattedSection

open DocTree2



let PreformattedSectionToDocumentString lst =

    let builder = new System.Text.StringBuilder ()

    lst |> List.iter (fun (PreformattedString(str)) ->
        if builder.Length > 0 then builder.Append('\n') |> ignore
        builder.Append(str) |> ignore
    )

    builder.ToString()


let UnixLineEndsToListOfStrings (str:string) =

    str.Split('\n') |> Array.toList
