module ColourizationTokens

open DocSlice

/// A colour type which is mapped to suggested colours by 'StandardColourFor', which you
/// should use for primary design against a white background.  The intention is these
/// could be wholesale re-mapped by an artist to a different palette, where these
/// original colours hint at rough relationships that should be maintained in the
/// replacement theme.
type StandardColour =

    /// Warning: White is the background colour for this palette.
    | White

    /// Hint: This is the foreground colour.  (Default for text).
    | Black

    /// This colour can be expected to blend the foreground and background colours
    /// in any derived theme.
    | Grey

    /// This colour can be expected to blend the foreground and background colours
    /// in any derived theme.
    | DarkGrey

    /// A red colour in this palette, but artistically re-mapped in other themes.
    | Red

    /// A dark red colour in this palette, but artistically re-mapped in other themes.
    | DarkRed

    /// An orange colour in this palette, but artistically re-mapped in other themes.
    | Orange

    /// A dark orange colour in this palette, but artistically re-mapped in other themes.
    | DarkOrange

    /// A yellow colour in this palette, but artistically re-mapped in other themes.
    | Yellow

    /// An amber colour in this palette, but artistically re-mapped in other themes.
    | DarkYellow

    /// A green colour in this palette, but artistically re-mapped in other themes.
    | Green

    /// A dark green colour in this palette, but artistically re-mapped in other themes.
    | DarkGreen

    /// A cyan colour in this palette, but artistically re-mapped in other themes.
    | Cyan

    /// A teal colour in this palette, but artistically re-mapped in other themes.
    | DarkCyan

    /// A blue colour in this palette, but artistically re-mapped in other themes.
    | Blue

    /// A dark blue colour in this palette, but artistically re-mapped in other themes.
    | DarkBlue

    /// A pink colour in this palette, but artistically re-mapped in other themes.
    | Pink

    /// A purple colour in this palette, but artistically re-mapped in other themes.
    | DarkPink



let StandardColourFor colourToken =
    match colourToken with
    | White      -> 0xFFFFFF
    | Black      -> 0x000000
    | Grey       -> 0xCECECE
    | DarkGrey   -> 0x9D9D9D
    | Red        -> 0xF6402C  // Material RED
    | DarkRed    -> 0xC00000
    | Orange     -> 0xFF9800  // Material ORANGE
    | DarkOrange -> 0xFF5505  // Material DEEP ORANGE
    | Yellow     -> 0xFFEC16  // Material YELLOW
    | DarkYellow -> 0xFFC100  // Material AMBER
    | Green      -> 0x88C440  // Material LIGHT GREEN
    | DarkGreen  -> 0x46AF4A  // Material GREEN
    | Cyan       -> 0x00BBD5  // Material CYAN
    | DarkCyan   -> 0x009687  // Material TEAL
    | Blue       -> 0x00A7F6  // Material LIGHT BLUE
    | DarkBlue   -> 0x1093F5  // Material BLUE
    | Pink       -> 0xEB1460  // Material PINK
    | DarkPink   -> 0x9C1AB1  // Material PURPLE



let DocumentFromTokens colourTokens =
    match colourTokens with
        | (_, DocSlice(_,_,document))::_ -> Some(document)
        | [] -> None
