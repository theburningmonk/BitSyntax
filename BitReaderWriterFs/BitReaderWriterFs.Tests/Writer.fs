module Writer

open BitReaderWriterFs.BitWriterWorkflow
open FsUnit
open NUnit.Framework
open FsCheck
open System.IO

[<TestFixture>]
type ``BitWriter tests`` () =
    [<Test>]
    member test.``test WriteBool`` () =
        BitWriter.WriteBool true  |> should equal <| Writer(1, [| 1uy |])
        BitWriter.WriteBool false |> should equal <| Writer(1, [| 0uy |])

    [<Test>]
    member test.``test WriteByte with less or equal to 8 bits`` () =
        let property n =
            n <= 8 ==> lazy (BitWriter.WriteByte (255uy, n)  |> should equal <| Writer(n, [| 255uy |]))
        Check.Quick property

    [<Test>]
    member test.``test WriteBytes`` () =
        BitWriter.WriteBytes([| 255uy; 1uy |])    |> should equal <| Writer(16, [| 255uy; 1uy |])
        BitWriter.WriteBytes([| 255uy; 1uy |], 8) |> should equal <| Writer(8, [| 255uy; 1uy |])

    [<Test>]
    member test.``test WriteChar`` () =
        BitWriter.WriteChar('a')    |> should equal <| Writer(8, [| 97uy |])

    [<Test>]
    member test.``test WriteString`` () =
        BitWriter.WriteString("hello world")
        |> should equal <| Writer(88, [| 104uy; 101uy; 108uy; 108uy; 111uy; 32uy; 119uy; 111uy; 114uy; 108uy; 100uy |])

    [<Test>]
    member test.``test WriteInt16`` () =
        BitWriter.WriteInt16(256s) |> should equal <| Writer(16, [| 0uy; 1uy |])
        BitWriter.WriteInt16(256s, 4) |> should equal <| Writer(4, [| 0uy |])

    [<Test>]
    member test.``test WriteUint16`` () =
        BitWriter.WriteUint16(256us) |> should equal <| Writer(16, [| 0uy; 1uy |])
        BitWriter.WriteUint16(256us, 4) |> should equal <| Writer(4, [| 0uy |])

    [<Test>]
    member test.``test WriteInt32`` () =
        BitWriter.WriteInt32(256) |> should equal <| Writer(32, [| 0uy; 1uy; 0uy; 0uy |])
        BitWriter.WriteInt32(256, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])

    [<Test>]
    member test.``test WriteUint32`` () =
        BitWriter.WriteUint32(256u) |> should equal <| Writer(32, [| 0uy; 1uy; 0uy; 0uy |])
        BitWriter.WriteUint32(256u, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])

    [<Test>]
    member test.``test WriteInt64`` () =
        BitWriter.WriteInt64(256L) |> should equal <| Writer(64, [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |])
        BitWriter.WriteInt64(256L, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])

    [<Test>]
    member test.``test WriteUint64`` () =
        BitWriter.WriteUint64(256UL) |> should equal <| Writer(64, [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |])
        BitWriter.WriteUint64(256UL, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])

    [<Test>]
    member test.``test WriteFloat`` () =
        BitWriter.WriteFloat(256.0f) |> should equal <| Writer(32, [| 0uy; 0uy; 128uy; 67uy |])

    [<Test>]
    member test.``test WriteDouble`` () =
        BitWriter.WriteDouble(256.0) |> should equal <| Writer(64, [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 112uy; 64uy |])

[<TestFixture>]
type ``BitWriterBuilder tests`` () =
    let getBytes f input (n : int) = 
        let stream = new MemoryStream()

        do bitWriter stream {
            do! f(input, n)
            do()
        }

        stream.ToArray()

    [<Test>]
    member test.``bitWriter workflow should be able to write Int16 to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteInt16(input, n))

        getBytes 256s 16 |> should equal [| 0uy; 1uy |]
        getBytes 256s 8  |> should equal [| 0uy |]

        // 00001010 > take 4 bits from right > 1010
        // which when padded into a full byte gives 10100000 = 128 + 32 = 160
        getBytes 10s 4   |> should equal [| 160uy |]
        // 00001010 > take 2 bits > 10 > padded into > 10000000 = 128
        getBytes 10s 2   |> should equal [| 128uy |]
        
    [<Test>]
    member test.``bitWriter workflow should be able to write Int32 to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteInt32(input, n))

        getBytes 256 32 |> should equal [| 0uy; 1uy; 0uy; 0uy |]
        getBytes 256 24 |> should equal [| 0uy; 1uy; 0uy |]
        getBytes 256 16 |> should equal [| 0uy; 1uy |]
        getBytes 256 8  |> should equal [| 0uy |]

        getBytes 10 8   |> should equal [| 10uy |]
        
        // see Int16 test for explanations
        getBytes 10 4   |> should equal [| 160uy |]
        getBytes 10 2   |> should equal [| 128uy |]
        
    [<Test>]
    member test.``bitWriter workflow should be able to write Int64 to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteInt64(input, n))

        getBytes 256L 32 |> should equal [| 0uy; 1uy; 0uy; 0uy |]
        getBytes 256L 24 |> should equal [| 0uy; 1uy; 0uy |]
        getBytes 256L 16 |> should equal [| 0uy; 1uy |]
        getBytes 256L 8  |> should equal [| 0uy |]

        getBytes 10L 8   |> should equal [| 10uy |]
        
        // see Int16 test for explanations
        getBytes 10L 4   |> should equal [| 160uy |]
        getBytes 10L 2   |> should equal [| 128uy |]