module ColourizationTokens

open DocSlice

type ColourToken =
    | White
    | Black
    | Grey1
    | Grey2
    | Red1
    | Red2
    | Orange1
    | Orange2
    | Yellow1
    | Yellow2
    | Green1
    | Green2
    | Cyan1
    | Cyan2
    | Blue1
    | Blue2
    | Pink1
    | Pink2



let StandardColourFor colourToken =
    match colourToken with
    | White   -> 0xFFFFFF
    | Black   -> 0x000000
    | Grey1   -> 0xCECECE
    | Grey2   -> 0x9D9D9D
    | Red1    -> 0xF6402C
    | Red2    -> 0xC00000
    | Orange1 -> 0xFF9800
    | Orange2 -> 0xFF5505
    | Yellow1 -> 0xFFEC16
    | Yellow2 -> 0xFFC100
    | Green1  -> 0x88C440
    | Green2  -> 0x46AF4A
    | Cyan1   -> 0x00BBD5
    | Cyan2   -> 0x009687
    | Blue1   -> 0x00A7F6
    | Blue2   -> 0x1093F5
    | Pink1   -> 0xEB1460
    | Pink2   -> 0x9C1AB1



let DocumentFromTokens colourTokens =
    match colourTokens with
        | (_, DocSlice(_,_,document))::_ -> Some(document)
        | [] -> None
