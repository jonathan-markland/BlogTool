module ToHtmlFileSet

// TODO: Collect error messages together somehow, and NEVER allow further processing of errant files.
// TODO: Check for image file formats other than .png
// TODO: An unprocessed DT3SubstituationDirective should be an error.
// TODO: Only copy image files to the output folder for SUCCESSFUL files.


open DocTree1
open DocTree2
open DocTree3
open ListSplit
open StringClassifiers
open FileSystemEffects



let ToolErrorString = "**BlogTool ERROR**"



let ErrorInPage (page:string) = 
    page.Contains("**BlogTool ERROR**") || page.Length = 0



let WithErrorPrefixIfErrorIn page leafName =

    if page |> ErrorInPage then
        "ERROR IN FILE - " + leafName
    else
        leafName



let EscapedForHTML (rawString:string) = 

    let replacing (searchChar:string) (replaceStr:string) (str:string) =
        // Ensure we create no garbage when there is nothing to replace:
        if str.Contains(searchChar) then str.Replace(searchChar, replaceStr) else str

    rawString 
        |> replacing "&" "&amp;" 
        |> replacing "<" "&lt;" 
        |> replacing ">" "&gt;" 
        |> replacing "\"" "&quot;"



let InjectedErrorString (message:string) (parameter:string) =
    ToolErrorString + " " + message + ": " + (parameter |> EscapedForHTML)



let WithSquareBracketsRemoved (str:string) =
    if str.StartsWith("[") && str.EndsWith("]") then
        str.Substring(1, str.Length - 2)
    else
        InjectedErrorString "Square brackets are missing from directive" str



let ToSnakeLikeFileName (str:string) =
    str.Replace(' ', '-').ToLower()



let ToHtmlImageTag (leafName:string) =
    "<img src=\"" + leafName + "\"></img>"



let ConsideredForItalics (content:string) =
    if content.StartsWith("...") || content.EndsWith("...") || content.EndsWith("...:") then
        "<em>" + content + "</em>"
    else
        content



let ListOfImageFilesWithin (page:string) =
    let regex = new System.Text.RegularExpressions.Regex("<img src=\"([a-zA-Z0-9\- \.]*)\"></img>")
    let results = regex.Matches(page) //:> System.Text.RegularExpressions.MatchCollection
    results 
        |> Seq.map (fun result -> result.Groups.[1].Value) 
        |> Seq.toList


let (|HyperlinkParagraph|_|) docTree3 =
    match docTree3 with
        | DT3Paragraph(content) ->
            if content.StartsWith("http:") || content.StartsWith("https:") then
                Some(content)
            else
                None
        | _ -> None



let DocTree3ToTraditionalHTML substitutionProvider treeList3 = 

    // TODO: This is not absolutely performant with the use of .Net string concatenations.

    let rec recurse outerContext treeList3 =

        let raw name content tail =
            if outerContext = "li" then
                content + (tail |> recurse outerContext)
            else
                "<" + name + ">" + content + "</" + name + ">" + (tail |> recurse outerContext)   // TODO: Leave tail recursion to caller I think.

        let text name content tail =
            let content = content |> EscapedForHTML |> ConsideredForItalics
            raw name content tail

        let html name content tail =
            raw name (content |> recurse name) tail

        let hyperlink content =
            let escapedContent = content |> EscapedForHTML
            if content = escapedContent then
                "<a href=\"" + content + "\">" + (content |> EscapedForHTML) + "</a>"
            else
                InjectedErrorString "Invalid characters in file path" content

        let substitutedDirective directive =
            directive
                |> WithSquareBracketsRemoved
                |> substitutionProvider
                |> Option.defaultValue (InjectedErrorString "Unrecognised directive" directive)

        let rec preformattedToHTML lst = 

            let mutable str = ""
            lst |> List.iter (fun (PreformattedString(s)) -> 

                let line =
                    if s |> LooksLikeSquareBracketed then
                        substitutedDirective s
                    else
                        s |> EscapedForHTML

                str <- str + line + "<br/>")

            str

        let headingTag level =
            match level with
                | Heading1 -> "h1"
                | Heading2 -> "h2"
                | Heading3 -> "h3"


        match treeList3 with
            | HyperlinkParagraph(content)::tail   -> (hyperlink content) + (tail |> recurse outerContext)
            | DT3Bullet(content)::tail            -> html "li" content tail
            | DT3Indent(content)::tail            -> html "blockquote" content tail
            | DT3Paragraph(content)::tail         -> text "p" content tail
            | DT3Heading(level, content)::tail    -> text (headingTag level) content tail
            | DT3PageBreak::tail                  -> (InjectedErrorString "Unexpected page break" "") + (tail |> recurse outerContext)
            | DT3SubstitutionDirective(dir)::tail -> (substitutedDirective dir) + (tail |> recurse outerContext)
            | DT3Preformatted(lst)::tail          -> "<blockquote><pre>" + (lst |> preformattedToHTML) + "</pre></blockquote>" + (tail |> recurse outerContext)
            | [] -> ""


    recurse "" treeList3






let FileToTraditionalHTMLFileSet (outputPath:string) (pathToTextFile:string) =

    let inputPath =
        System.IO.Path.GetDirectoryName(pathToTextFile)

    let substitutionProvider directive =
        let leafName = (directive |> ToSnakeLikeFileName) + ".png"
        let imagePath = System.IO.Path.Combine(inputPath, leafName)
        if System.IO.File.Exists(imagePath) then
            Some(leafName |> ToHtmlImageTag)
        else
            Some(InjectedErrorString "Cannot find image file" imagePath)

    let document = 
        System.IO.File.ReadAllLines(pathToTextFile)
    
    let treeList = 
        document 
            |> DocumentToDocTree1
            |> DocTree1ToDocTree2
            |> DocTree2ToDocTree3

    let splitAtPageBreaks =
        treeList |> ListSplit ((=) DT3PageBreak)

    let listOfHtmlPages =
        splitAtPageBreaks |> List.map (DocTree3ToTraditionalHTML substitutionProvider)

    let copyFileFromInputFolderToOutputFolder leafName =
        let fileFrom = System.IO.Path.Combine(inputPath, leafName)
        let fileTo   = System.IO.Path.Combine(outputPath, leafName)
        { CopySourceFilePath=fileFrom ; CopyTargetFilePath=fileTo }

    let iterateAllGoodPages op pageList =
        pageList |> List.choose (fun page -> 
            if page |> ErrorInPage then 
                None
            else
                Some(op page))

    let saveJobs =
        listOfHtmlPages
            |> List.mapi (fun i page ->
                let pageLeafName = (sprintf "file%d.html" i) |> WithErrorPrefixIfErrorIn page
                let saveAs = System.IO.Path.Combine(outputPath, pageLeafName)
                { SaveFileContent=page ; SaveAsFilePath=saveAs }
            )

    let copyJobs =
        listOfHtmlPages
            |> iterateAllGoodPages (fun page ->
                ListOfImageFilesWithin page
                    |> List.map copyFileFromInputFolderToOutputFolder)  // TODO: Overwrite warning?  Solve by verifying names first?
        |> List.concat

    //
    // SIDE EFFECTS ON FILE SYSTEM
    //

    saveJobs |> SaveFilesNow
    copyJobs |> CopyFilesNow

