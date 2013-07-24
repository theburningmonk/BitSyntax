module Tests

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

[<TestFixture>]
type ``Given a sequence of values`` () =
    [<Test>]
    member test.``when it's written to a stream the same values should be read back out`` () =
        let stream = new MemoryStream()

        seq {
            yield BitWriter.WriteInt16(10s, 4)
            yield BitWriter.WriteUint16(10us, 4)
            yield BitWriter.WriteInt32(10, 4)
            yield BitWriter.WriteUint32(10u, 4)
            yield BitWriter.WriteInt64(10L, 4)
            yield BitWriter.WriteUint64(10UL, 4)

            yield BitWriter.WriteFloat(1024.0)
            yield BitWriter.WriteDouble(1024.0)

            yield BitWriter.WriteBool(true)
            yield BitWriter.WriteBool(false)
            yield BitWriter.WriteBytes(6, 10uy)
            yield BitWriter.WriteBytes(13, 3uy, 2uy, 1uy)

            yield BitWriter.WriteChar('Y')
            yield BitWriter.WriteString("Hello World!")
        } |> BitWriter.Flush stream

        stream.Position <- 0L
        let a, b, c, d, e, f, g, h, i, j, k, l, m, n = 
            bitReader stream {
                let! a = BitReader.ReadInt16(4)
                let! b = BitReader.ReadUint16(4)
                let! c = BitReader.ReadInt32(4)
                let! d = BitReader.ReadUint32(4)
                let! e = BitReader.ReadInt64(4)
                let! f = BitReader.ReadUint64(4)
                let! g = BitReader.ReadFloat(4)
                let! h = BitReader.ReadDouble(4)
                let! i = BitReader.ReadBool()
                let! j = BitReader.ReadBool()
                let! k = BitReader.ReadAs(6, (fun arr -> arr.[0]))
                let! l = BitReader.ReadBytes(13)

                let! m = BitReader.ReadChar()
                let! n = BitReader.ReadString(8 * 12)

                return a, b, c, d, e, f, g, h, i, j, k, l, m, n
            }

        a |> should equal 10s
        b |> should equal 10us
        c |> should equal 10
        d |> should equal 10u
        e |> should equal 10L
        f |> should equal 10UL
        g |> should equal 1024.0
        h |> should equal 1024.0
        i |> should equal true
        j |> should equal false
        k |> should equal 10uy
        l |> should equal [| 3uy; 2uy; 1uy |]
        m |> should equal 'Y'
        n |> should equal "Hello World!00011"