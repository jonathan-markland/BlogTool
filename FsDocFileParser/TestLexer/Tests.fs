module Tests

open System
open Xunit
open StringLexer
open DocSlice
open Hedgehog



// [<Fact>]
// let ``Hedgehog list reversing example`` () =
//     let p = 
//         property {
//             let! xs = Gen.list (Range.linear 0 100) Gen.alpha
//             return xs |> List.rev |> List.rev = xs
//             }
//     Property.check p



[<Fact>]
let ``CharAt beyond end`` () =
    let p = 
        property {
            let document = "This is an example document for this test."
            let! index = Range.constant (document.Length) 1000 |> Gen.int
            return (document |> CharAt index) = '\x00'
            }
    Property.check p



[<Fact>]
let ``CharAt within string`` () =
    let p = 
        property {
            let document = "This is an example document for this test."
            let! index = Range.constant 0 (document.Length-1) |> Gen.int
            return (document |> CharAt index) = document.[index]
            }
    Property.check p




[<Fact>]
let ``StartPositionOf returns start of document`` () =
    let pos = StartPositionOf "hello"
    let (DocPosition(p,d)) = pos
    let truth = (p = 0)
    Assert.True(truth)




[<Fact>]
let ``CharWhere can consume when predicate returns true`` () =
    let pos = StartPositionOf "hello"
    let (DocSlice(s,e,d)) = pos |> CharWhere Char.IsLetter
    let truth = (s = 0 && e = 1)
    Assert.True(truth)



[<Fact>]
let ``CharWhere fails to consume when predicate returns false`` () =
    let pos = StartPositionOf "1234"
    let (DocSlice(s,e,d)) = pos |> CharWhere Char.IsLetter
    let truth = (s = 0 && e = 0)
    Assert.True(truth)



[<Fact>]
let ``ZeroOrMoreCharsWhere can consume while predicate returns true`` () =
    let pos = StartPositionOf "hello"
    let (DocSlice(s,e,d)) = pos |> ZeroOrMoreCharsWhere Char.IsLetter
    let truth = (s = 0 && e = 5)
    Assert.True(truth)



[<Fact>]
let ``ZeroOrMoreCharsWhere fails to consume while predicate returns false`` () =
    let pos = StartPositionOf "1234"
    let (DocSlice(s,e,d)) = pos |> ZeroOrMoreCharsWhere Char.IsLetter
    let truth = (s = 0 && e = 0)
    Assert.True(truth)



[<Fact>]
let ``HasContent reports that empty length slices anywhere in the document have no content`` () =
    let p = 
        property {
            let document = "This is an example document for this test."
            let! index = Range.constant 0 (document.Length) |> Gen.int
            return not (DocSlice(index, index, document) |> HasContent)
            }
    Property.check p



[<Fact>]
let ``DocSliceToString returns expected slices`` () =
    let p = 
        property {
            let document = "This is an example document for this test."
            let maxLength = 10
            let! index = Range.constant 0 (document.Length-maxLength) |> Gen.int
            let! length = Range.constant 0 maxLength |> Gen.int
            let slice = DocSlice(index, index + length, document)
            let expectation = document.Substring(index, length)
            return (slice |> DocSliceToString) = expectation
            }
    Property.check p



[<Fact>]
let ``DocSliceMatchesText is looking at the correct regions`` () =
    let p = 
        property {
            let document = "abcdabcdabcdabcdabcdabcdabcdabcdabcdabcdabcd"
            let maxLength = 6
            let! index = Range.constant 0 (document.Length-maxLength) |> Gen.int
            let! length = Range.constant 0 maxLength |> Gen.int
            let slice = DocSlice(index, index + length, document)
            let truthExpectation = (length = 4) && (index % 4) = 0
            return (slice |> DocSliceMatchesText "abcd") = truthExpectation
            }
    Property.check p



