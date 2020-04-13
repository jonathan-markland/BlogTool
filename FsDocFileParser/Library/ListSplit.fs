module ListSplit



/// Efficiently identifies a sub-section of a list.
[<Struct>]
type ListSlice<'a> =
    { 
        /// A start position in an overall list.
        Start: 'a list
        
        /// Count of the number of significant items 
        /// from the Start position.
        Count: int 
    }



/// Splits the input list where the isSeparator predicate returns true.
/// Returns a reversed list of the sub-lists as ListSlice records.
/// The separators are removed in the result.
let ListSplitToSlices isSeparator theList =

    let counterReset = 0

    let rec splitCore isSeparator count mostRecentSliceStart theList acc =

        let groupEndMet () =
            { Start = mostRecentSliceStart ; Count = count }::acc

        match theList with

            | head::tail ->
                if isSeparator head then
                    let newAcc = groupEndMet ()
                    splitCore isSeparator counterReset tail tail newAcc
                else
                    splitCore isSeparator (count + 1) mostRecentSliceStart tail acc

            | [] -> 
                groupEndMet ()


    splitCore isSeparator counterReset theList theList []



/// Splits the input list where the isSeparator predicate returns true.
/// Returns a reversed list of the sub-lists.  The separators are removed in the result.
let ListSplitReverse isSeparator theList =

    theList 
        |> ListSplitToSlices isSeparator
        |> List.map (fun {Start=lst ; Count=count} -> lst |> List.take count)



/// Splits the input list where the isSeparator predicate returns true.
/// Returns a list of the sub-lists.  The separators are removed in the result.
let ListSplit isSeparator theList =

    theList |> ListSplitReverse isSeparator |> List.rev
