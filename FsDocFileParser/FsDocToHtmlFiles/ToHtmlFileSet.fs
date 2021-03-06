﻿module ToHtmlFileSet

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
open PreformattedSection // HACK
open HtmlGeneration
open CssHtmlTextLexer
open Colourizer



let ToolErrorString = "**BlogTool ERROR**"



let ErrorInPage (page:string) = 
    page.Contains(ToolErrorString) || page.Length = 0



let WithErrorPrefixIfErrorIn page leafName =

    if page |> ErrorInPage then
        "ERROR IN FILE - " + leafName
    else
        leafName



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



let ImageSourceRegex = new System.Text.RegularExpressions.Regex("<img src=\"([a-zA-Z0-9\- \.]*)\"></img>")


type ImageFilePath = ImageFilePath of string

let ListOfImageFilesWithin (page:string) =
    ImageSourceRegex.Matches(page)
        |> Seq.map (fun result -> result.Groups.[1].Value |> ImageFilePath) 
        |> Seq.toList


let (|HyperlinkParagraph|_|) docTree3 =
    match docTree3 with
        | DT3Paragraph(content) ->
            if content.StartsWith("http:") || content.StartsWith("https:") then
                Some(content)
            else
                None
        | _ -> None



type PageTranslationStatus<'htmlString> =
    | SuccessfulTranslation of htmlFile:'htmlString * imageFiles:ImageFilePath list
    | ErrantTranslation of htmlFile:'htmlString



let DocTree3ToTraditionalHTML substitutionProvider (pageWrapped:HtmlFileBodyString -> 'htmlString) treeList3 = 

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
                InjectedErrorString "Characters that require escaping for HTML are not valid for use in this file path" content

        let substitutedDirective directive =
            directive
                |> WithSquareBracketsRemoved
                |> substitutionProvider
                |> Option.defaultValue (InjectedErrorString "Unrecognised directive" directive)

        let rec preformattedToHTML lst = 

            /// vvv HACK

            let html = 
                lst 
                    |> PreformattedSectionToDocumentString
                    |> CssHtmlTextTokensFromString
                    |> ColouredUpWithHtmlSpans
                    |> UnixLineEndsToListOfStrings

            /// ^^^ HACK

            let mutable str = ""
            html |> List.iter (fun s -> 

                let line =
                    if s |> LooksLikeSquareBracketed then
                        substitutedDirective s
                    else
                        s

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


    let htmlPageResult = recurse "" treeList3

    let withWarningPrefix s = "<H1>*** THIS FILE HAS ERRORS ***</H1>" + s

    if htmlPageResult |> ErrorInPage then  // TODO: ErrorInPage: Re-parsing our output is not 100% desireable!
        ErrantTranslation (
            HtmlFileBodyString (htmlPageResult |> withWarningPrefix) |> pageWrapped)
    else
        SuccessfulTranslation(
            HtmlFileBodyString (htmlPageResult) |> pageWrapped, 
            ListOfImageFilesWithin htmlPageResult)  // TODO: ListOfImageFilesWithin: Re-parsing our output is not 100% desireable!




/// Sanitizer for file system file names.
let StringSanitizedForFileName (str:string) =

    let s = str 
                |> Seq.choose (fun ch ->
                    if ch |> System.Char.IsLetterOrDigit then
                        Some(ch)
                    elif ch = ' ' then
                        Some('-')
                    else
                        None)
                |> Seq.map string
                |> String.concat ""

    s.Replace("--", "-").ToLower()



// Try to obtain the first heading.
let TryFirstHeading treeList3 =
    match treeList3 with
        | DT3Heading(_, text)::_ -> Some(text)
        | _ -> None



/// Encapsulates the rule for determine the file name of an output file
/// from the source file and the page content (first heading).
let PageFileName (pathToTextFile:string) fallbackName (treeList3:DocTree3 list) =

    let inputFileNameWithoutExtension =
        System.IO.Path.GetFileNameWithoutExtension(pathToTextFile)

    let heading = treeList3 |> TryFirstHeading |> Option.defaultValue fallbackName

    inputFileNameWithoutExtension + " " + heading
        |> StringSanitizedForFileName

    





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

    
    let translationResults =

        pathToTextFile
            |> System.IO.File.ReadAllLines
            |> DocumentToDocTree1
            |> DocTree1ToDocTree2
            |> DocTree2ToDocTree3
            |> ListSplit ((=) DT3PageBreak)
            |> List.mapi (fun i docTree3 ->
                let fallbackFileName = sprintf "untitled-file%d" i
                let pageLeafFileName = (PageFileName pathToTextFile fallbackFileName docTree3) + ".html"
                let wrappedAsRegularHtmlPage = MakeIntoFullHtmlPage "Title" ""
                let translation = docTree3 |> DocTree3ToTraditionalHTML substitutionProvider wrappedAsRegularHtmlPage
                (pageLeafFileName , translation)
            )

    let saveJobs =

        translationResults
            |> List.map (fun (htmlFileName, translationStatus) -> 
                match translationStatus with
                    | SuccessfulTranslation(HtmlFullFile (htmlFileContent),_)
                    | ErrantTranslation(HtmlFullFile (htmlFileContent))
                        ->  let saveAs = System.IO.Path.Combine(outputPath, htmlFileName)
                            { SaveFileContent=htmlFileContent ; SaveAsFilePath=saveAs }
            )

    let copyJobs =

        let copyFileFromInputFolderToOutputFolder (ImageFilePath(leafName)) =

            let fileFrom = System.IO.Path.Combine(inputPath, leafName)
            let fileTo   = System.IO.Path.Combine(outputPath, leafName)
            { CopySourceFilePath=fileFrom ; CopyTargetFilePath=fileTo }

        translationResults
            |> List.collect (fun (_, translationStatus) -> 
                match translationStatus with
                    | SuccessfulTranslation(_,imagesList) ->
                        imagesList |> List.map copyFileFromInputFolderToOutputFolder
                    | ErrantTranslation _ -> 
                        []
            )

    //
    // SIDE EFFECTS ON FILE SYSTEM
    //

    saveJobs |> SaveFilesNow  // TODO: Overwrite warning?  Solve by verifying names first?
    copyJobs |> CopyFilesNow

