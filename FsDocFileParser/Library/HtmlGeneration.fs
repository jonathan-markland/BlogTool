module HtmlGeneration

/// The body content of an HTML file as a string.
type HtmlFileBodyString = HtmlFileBodyString of string

/// The full content of an HTML file as a string.
type HtmlFullFile = HtmlFullFile of string



/// Return input string escaped for HTML.
let EscapedForHTML (rawString:string) = 

    let replacing (searchChar:string) (replaceStr:string) (str:string) =
        // Ensure we create no garbage when there is nothing to replace:
        if str.Contains(searchChar) then str.Replace(searchChar, replaceStr) else str

    rawString 
        |> replacing "&" "&amp;" 
        |> replacing "<" "&lt;" 
        |> replacing ">" "&gt;" 
        |> replacing "\"" "&quot;"



/// Return input string as body of HTML file.
let MakeIntoFullHtmlPage pageTitle extraScript (HtmlFileBodyString (pageBodyInnerHtml)) =

    let preamble1 = """
<!DOCTYPE html>
<html style="height:100%; overflow:hidden">
<title>
"""

    let preamble2 = """
</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="jonathans_w3c_css.css">
<body style="height:100%; overflow:hidden">
"""

    let postamble = """
</body>
</html>
"""

    HtmlFullFile (preamble1 + pageTitle + preamble2 + pageBodyInnerHtml + extraScript + postamble)



