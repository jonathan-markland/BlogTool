module TestCSSLexer

open Xunit
open Colourizer
open CssHtmlTextLexer



[<Fact>]
let ``CSS lex and colourisation test with HTML spans`` () =

    let document = 
        " .thingy {   display: block; }     div { background-color:red;    color: green; }   "

    let colouredDocument = 
        document |> CssHtmlTextTokensFromString |> ColouredUpWithHtmlSpans

    let expectation =
        """ <span style="color:#c00000;">.thingy</span> <span style="color:#88c440;">{</span>   <span style="color:#eb1460;">display</span><span style="color:#88c440;">:</span><span style="color:#ff5505;"> block</span><span style="color:#ff9800;">;</span> <span style="color:#88c440;">}</span>     <span style="color:#c00000;">div</span> <span style="color:#88c440;">{</span> <span style="color:#eb1460;">background-color</span><span style="color:#88c440;">:</span><span style="color:#ff5505;">red</span><span style="color:#ff9800;">;</span>    <span style="color:#eb1460;">color</span><span style="color:#88c440;">:</span><span style="color:#ff5505;"> green</span><span style="color:#ff9800;">;</span> <span style="color:#88c440;">}</span>   """

    Assert.True((colouredDocument = expectation):bool)
    


[<Fact>]
let ``CSS lex and colourisation test with user defined HTML tags`` () =

    let document = 
        " .thingy {   display: block; }     div { background-color:red;    color: green; }   "

    let colouredDocument = 
        document |> CssHtmlTextTokensFromString |> ColouredUpWithUserDefinedTags

    let expectation =
        """ <r2>.thingy</r2> <g1>{</g1>   <p1>display</p1><g1>:</g1><o2> block</o2><o1>;</o1> <g1>}</g1>     <r2>div</r2> <g1>{</g1> <p1>background-color</p1><g1>:</g1><o2>red</o2><o1>;</o1>    <p1>color</p1><g1>:</g1><o2> green</o2><o1>;</o1> <g1>}</g1>   """

    Assert.True((colouredDocument = expectation):bool)



[<Fact>]
let ``Lex and colourisation of CSS with additional text`` () =

    let document = 
        "additional .thingy {   display: block; } additional     div { background-color:red;    color: green; } additional additional   "

    let colouredDocument = 
        document |> CssHtmlTextTokensFromString |> ColouredUpWithHtmlSpans

    let expectation =
        """additional <span style="color:#c00000;">.thingy</span> <span style="color:#88c440;">{</span>   <span style="color:#eb1460;">display</span><span style="color:#88c440;">:</span><span style="color:#ff5505;"> block</span><span style="color:#ff9800;">;</span> <span style="color:#88c440;">}</span> additional     <span style="color:#c00000;">div</span> <span style="color:#88c440;">{</span> <span style="color:#eb1460;">background-color</span><span style="color:#88c440;">:</span><span style="color:#ff5505;">red</span><span style="color:#ff9800;">;</span>    <span style="color:#eb1460;">color</span><span style="color:#88c440;">:</span><span style="color:#ff5505;"> green</span><span style="color:#ff9800;">;</span> <span style="color:#88c440;">}</span> additional additional   """

    Assert.True((colouredDocument = expectation):bool)



[<Fact>]
let ``Lex and colourisation of CSS and HTML and additional text`` () =

    let document = 
        "<html><body style=\"height:100%;\"><style>.thingy { display: block; } div { background-color:red;    color: green; }</style>This is some <br/>additional text</body></html>"

    let colouredDocument = 
        document |> CssHtmlTextTokensFromString |> ColouredUpWithHtmlSpans

    let expectation =
        """<span style="color:#00a7f6;">&lt;html</span><span style="color:#00a7f6;">&gt;</span><span style="color:#00a7f6;">&lt;body</span> style=&quot;<span style="color:#eb1460;">height</span><span style="color:#88c440;">:</span><span style="color:#ff5505;">100%</span><span style="color:#ff9800;">;</span>&quot;<span style="color:#00a7f6;">&gt;</span><span style="color:#00a7f6;">&lt;style</span><span style="color:#00a7f6;">&gt;</span><span style="color:#c00000;">.thingy</span> <span style="color:#88c440;">{</span> <span style="color:#eb1460;">display</span><span style="color:#88c440;">:</span><span style="color:#ff5505;"> block</span><span style="color:#ff9800;">;</span> <span style="color:#88c440;">}</span> <span style="color:#c00000;">div</span> <span style="color:#88c440;">{</span> <span style="color:#eb1460;">background-color</span><span style="color:#88c440;">:</span><span style="color:#ff5505;">red</span><span style="color:#ff9800;">;</span>    <span style="color:#eb1460;">color</span><span style="color:#88c440;">:</span><span style="color:#ff5505;"> green</span><span style="color:#ff9800;">;</span> <span style="color:#88c440;">}</span><span style="color:#00a7f6;">&lt;/style</span><span style="color:#00a7f6;">&gt;</span>This is some <span style="color:#00a7f6;">&lt;br</span><span style="color:#00a7f6;">/&gt;</span>additional text<span style="color:#00a7f6;">&lt;/body</span><span style="color:#00a7f6;">&gt;</span><span style="color:#00a7f6;">&lt;/html</span><span style="color:#00a7f6;">&gt;</span>"""

    Assert.True((colouredDocument = expectation):bool)
