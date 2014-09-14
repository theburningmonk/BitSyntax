module Extensions

open System
open System.IO
open BitReaderWriterFs
open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``Given a byte`` () =
    [<TestCase(0uy,   Result = [| false; false; false; false; false; false; false; false |])>]
    [<TestCase(1uy,   Result = [| false; false; false; false; false; false; false; true |])>]
    [<TestCase(2uy,   Result = [| false; false; false; false; false; false; true; false |])>]
    [<TestCase(128uy, Result = [| true; false; false; false; false; false; false; false |])>]
    [<TestCase(255uy, Result = [| true; true; true; true; true; true; true; true |])>]    
    member test.``ToBitSequence should return its bit representation as booleans`` (input : byte) =
        input.ToBitSequence() |> Seq.toArray

[<TestFixture>]
type ``Given a stream`` () =
    [<TestCase([||], Result = [||])>]
    [<TestCase(0uy,         Result = [| false; false; false; false; false; false; false; false |])>]
    [<TestCase(0uy, 1uy,    Result = [| false; false; false; false; false; false; false; false;
                                        false; false; false; false; false; false; false; true |])>]
    [<TestCase(0uy, 128uy,  Result = [| false; false; false; false; false; false; false; false;
                                        true; false; false; false; false; false; false; false |])>]
    member test.``ToBitSequence should return bit representation of all its constituent bytes`` ([<ParamArray>] bytes : byte[]) =
        use stream = new MemoryStream(bytes)
        stream.ToBitSequence() |> Seq.toArray

[<TestFixture>]
type ``Given a sequence of integers`` () =
    [<Test>]
    member test.``when it is empty Seq.chunk 3 should return an empty sequence`` () =
        let chunks = Seq.empty<int> |> Seq.chunk 3 |> Seq.toArray
        chunks      |> should haveLength 0

    [<Test>]
    member test.``when it has 12 elements Seq.chunk 3 should return sequence of array of length 3`` () =
        let chunks = { 1..12 } |> Seq.chunk 3 |> Seq.toArray
        chunks      |> should haveLength 4
        chunks      |> Seq.forall (fun arr -> arr.Length = 3)
                    |> should equal true

    [<Test>]
    member test.``when it has 14 elements Seq.chunk 3 should return sequence of 5 arrays with the last having length of 2`` () =
        let chunks = { 1..14 } |> Seq.chunk 3 |> Seq.toArray
        chunks          |> should haveLength 5
        chunks.[0..3]   |> Seq.forall (fun arr -> arr.Length = 3)
                        |> should equal true
        chunks.[4]      |> should haveLength 2
