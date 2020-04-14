module TestCSSLexer

open Xunit
open CSSColourizer



[<Fact>]
let ``CSS lex and colourisation test with HTML spans`` () =

    let document = 
        " .thingy {   display: block; }     div { background-color:red;    color: green; }   "

    let colouredDocument = 
        document |> CssColouredUpWithHtmlSpans

    let expectation =
        """ <span style="color:red;">.thingy</span> <span style="color:blue;">{</span>   <span style="color:purple;">display</span><span style="color:blue;">:</span><span style="color:green;"> block</span><span style="color:blue;">;</span> <span style="color:blue;">}</span>     <span style="color:red;">div</span> <span style="color:blue;">{</span> <span style="color:purple;">background-color</span><span style="color:blue;">:</span><span style="color:green;">red</span><span style="color:blue;">;</span>    <span style="color:purple;">color</span><span style="color:blue;">:</span><span style="color:green;"> green</span><span style="color:blue;">;</span> <span style="color:blue;">}</span>   """

    Assert.True((colouredDocument = expectation):bool)
    


[<Fact>]
let ``CSS lex and colourisation test with user defined HTML tags`` () =

    let document = 
        " .thingy {   display: block; }     div { background-color:red;    color: green; }   "

    let colouredDocument = 
        document |> CssColouredUpWithUserDefinedTags

    let expectation =
        """ <sel>.thingy</sel> <open-brace>{</open-brace>   <prop>display</prop><colon>:</colon><def> block</def><semi>;</semi> <close-brace>}</close-brace>     <sel>div</sel> <open-brace>{</open-brace> <prop>background-color</prop><colon>:</colon><def>red</def><semi>;</semi>    <prop>color</prop><colon>:</colon><def> green</def><semi>;</semi> <close-brace>}</close-brace>   """

    Assert.True((colouredDocument = expectation):bool)



[<Fact>]
let ``Lex and colourisation of CSS with additional text`` () =

    let document = 
        "additional .thingy {   display: block; } additional     div { background-color:red;    color: green; } additional additional   "

    let colouredDocument = 
        document |> CssColouredUpWithHtmlSpans

    let expectation =
        """additional <span style="color:red;">.thingy</span> <span style="color:blue;">{</span>   <span style="color:purple;">display</span><span style="color:blue;">:</span><span style="color:green;"> block</span><span style="color:blue;">;</span> <span style="color:blue;">}</span> additional     <span style="color:red;">div</span> <span style="color:blue;">{</span> <span style="color:purple;">background-color</span><span style="color:blue;">:</span><span style="color:green;">red</span><span style="color:blue;">;</span>    <span style="color:purple;">color</span><span style="color:blue;">:</span><span style="color:green;"> green</span><span style="color:blue;">;</span> <span style="color:blue;">}</span> additional additional   """

    Assert.True((colouredDocument = expectation):bool)



[<Fact>]
let ``Lex and colourisation of CSS and HTML and additional text`` () =

    let document = 
        "<html><body style=\"height:100%;\"><style>.thingy { display: block; } div { background-color:red;    color: green; }</style>This is some <br/>additional text</body></html>"

    let colouredDocument = 
        document |> CssColouredUpWithHtmlSpans

    let expectation =
        """<span style="color:cyan;">&lt;html</span><span style="color:cyan;">&gt;</span><span style="color:cyan;">&lt;body</span> style=&quot;<span style="color:purple;">height</span><span style="color:blue;">:</span><span style="color:green;">100%</span><span style="color:blue;">;</span>&quot;<span style="color:cyan;">&gt;</span><span style="color:cyan;">&lt;style</span><span style="color:cyan;">&gt;</span><span style="color:red;">.thingy</span> <span style="color:blue;">{</span> <span style="color:purple;">display</span><span style="color:blue;">:</span><span style="color:green;"> block</span><span style="color:blue;">;</span> <span style="color:blue;">}</span> <span style="color:red;">div</span> <span style="color:blue;">{</span> <span style="color:purple;">background-color</span><span style="color:blue;">:</span><span style="color:green;">red</span><span style="color:blue;">;</span>    <span style="color:purple;">color</span><span style="color:blue;">:</span><span style="color:green;"> green</span><span style="color:blue;">;</span> <span style="color:blue;">}</span><span style="color:cyan;">&lt;/style</span><span style="color:cyan;">&gt;</span>This is some <span style="color:cyan;">&lt;br</span><span style="color:cyan;">/&gt;</span>additional text<span style="color:cyan;">&lt;/body</span><span style="color:cyan;">&gt;</span><span style="color:cyan;">&lt;/html</span><span style="color:cyan;">&gt;</span>"""

    Assert.True((colouredDocument = expectation):bool)
