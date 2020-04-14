module HtmlEscaper

let EscapedForHTML (rawString:string) = 

    let replacing (searchChar:string) (replaceStr:string) (str:string) =
        // Ensure we create no garbage when there is nothing to replace:
        if str.Contains(searchChar) then str.Replace(searchChar, replaceStr) else str

    rawString 
        |> replacing "&" "&amp;" 
        |> replacing "<" "&lt;" 
        |> replacing ">" "&gt;" 
        |> replacing "\"" "&quot;"



