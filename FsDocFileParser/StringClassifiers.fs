module StringClassifiers

// ----------------------------------------------------------------------------------------------
//  Square bracketed
// ----------------------------------------------------------------------------------------------

let LooksLikeSquareBracketed (str:string) =
    let str = str.Trim()
    str.Length > 1
        && str.[0] = '['
        && str.[str.Length-1] = ']'

// ----------------------------------------------------------------------------------------------
//  Underline
// ----------------------------------------------------------------------------------------------

let IsStringAll chr (str:string) =
    let mutable is = true
    for ch in str do is <- is && (ch=chr) 
    is

let IsHeading1Underline = IsStringAll '='
let IsHeading2Underline = IsStringAll '-'

// ----------------------------------------------------------------------------------------------
//  Single line title
// ----------------------------------------------------------------------------------------------

let IsPunctuation ch = (ch = '.') || (ch = '!') || (ch = '?') || (ch = ':')

let LooksLikeSingleLineTitle (str:string) =
    str.Length > 1
        && str.[0] |> System.Char.IsLetterOrDigit
        && not (str.[str.Length-1] |> IsPunctuation)

