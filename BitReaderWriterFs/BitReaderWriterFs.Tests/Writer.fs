module Writer

open BitReaderWriterFs
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
    member test.``bitWriter workflow should be able to write Uint16 to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteUint16(input, n))

        getBytes 256us 16 |> should equal [| 0uy; 1uy |]
        getBytes 256us 8  |> should equal [| 0uy |]

        // 00001010 > take 4 bits from right > 1010
        // which when padded into a full byte gives 10100000 = 128 + 32 = 160
        getBytes 10us 4   |> should equal [| 160uy |]
        // 00001010 > take 2 bits > 10 > padded into > 10000000 = 128
        getBytes 10us 2   |> should equal [| 128uy |]
        
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
    member test.``bitWriter workflow should be able to write Uint32 to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteUint32(input, n))

        getBytes 256u 32 |> should equal [| 0uy; 1uy; 0uy; 0uy |]
        getBytes 256u 24 |> should equal [| 0uy; 1uy; 0uy |]
        getBytes 256u 16 |> should equal [| 0uy; 1uy |]
        getBytes 256u 8  |> should equal [| 0uy |]

        getBytes 10u 8   |> should equal [| 10uy |]
        
        // see Int16 test for explanations
        getBytes 10u 4   |> should equal [| 160uy |]
        getBytes 10u 2   |> should equal [| 128uy |]
        
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
        
    [<Test>]
    member test.``bitWriter workflow should be able to write Uint64 to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteUint64(input, n))

        getBytes 256UL 32 |> should equal [| 0uy; 1uy; 0uy; 0uy |]
        getBytes 256UL 24 |> should equal [| 0uy; 1uy; 0uy |]
        getBytes 256UL 16 |> should equal [| 0uy; 1uy |]
        getBytes 256UL 8  |> should equal [| 0uy |]

        getBytes 10UL 8   |> should equal [| 10uy |]
        
        // see Int16 test for explanations
        getBytes 10UL 4   |> should equal [| 160uy |]
        getBytes 10UL 2   |> should equal [| 128uy |]

    [<Test>]
    member test.``bitWriter workflow should be able to write bool to stream`` () =
        let getBytes b = getBytes (fun (input, _n) -> BitWriter.WriteBool(input)) b 1

        getBytes true   |> should equal [| 128uy |]
        getBytes false  |> should equal [| 0uy |]

    [<Test>]
    member test.``bitWriter workflow should be able to write byte to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteByte(input, n))
        
        getBytes 255uy 1  |> should equal [| 128uy |]
        getBytes 255uy 2  |> should equal [| 192uy |]
        getBytes 255uy 3  |> should equal [| 224uy |]
        getBytes 255uy 4  |> should equal [| 240uy |]
        getBytes 255uy 5  |> should equal [| 248uy |]
        getBytes 255uy 6  |> should equal [| 252uy |]
        getBytes 255uy 7  |> should equal [| 254uy |]
        getBytes 255uy 8  |> should equal [| 255uy |]

    [<Test>]
    member test.``bitWriter workflow should be able to write bytes to stream`` () =
        let getBytes = getBytes (fun (input, n) -> BitWriter.WriteBytes(input, n))

        getBytes [| 255uy; 255uy |] 5  |> should equal [| 248uy |]
        getBytes [| 255uy; 255uy |] 9  |> should equal [| 255uy; 128uy |]
        getBytes [| 255uy; 255uy |] 13 |> should equal [| 255uy; 248uy |]

    [<Test>]
    member test.``bitWriter workflow should be able to write char to stream`` () =
        let getBytes char = getBytes (fun (input, _n) -> BitWriter.WriteChar(input)) char 8

        getBytes 'X'    |> should equal [| 88uy |]

    [<Test>]
    member test.``bitWriter workflow should be able to write string to stream`` () =
        let getBytes str = getBytes (fun (input, _n) -> BitWriter.WriteString(input)) str (str.Length * 8)

        getBytes "X"    |> should equal [| 88uy |]
        getBytes "XX"   |> should equal [| 88uy; 88uy |]

    [<Test>]
    member test.``bitWriter workflow should be able to write more than the buffer to stream`` () =
        let getBytes str = getBytes (fun (input, _n) -> BitWriter.WriteString(input)) str (str.Length * 8)

        let bigString = [| 1..1050 |] |> Array.map (fun _ -> 'X')
                        |> (fun arr -> new System.String(arr))

        getBytes bigString |> should equal <| ([| 1..1050 |] |> Array.map (fun _ -> 88uy))

    [<Test>]
    member test.``bitWriter workflow should be able to use for loops`` () =
        let getBytes (bools : bool[]) = 
            let stream = new MemoryStream()

            do bitWriter stream {
                for b in bools do 
                    do! BitWriter.WriteBool(b)
            }

            stream.ToArray()

        // 1 > padded to > 10000000 > 128
        getBytes [| true |]                                             |> should equal [| 128uy |]
        getBytes [| true; true |]                                       |> should equal [| 192uy |]
        getBytes [| true; true; true |]                                 |> should equal [| 224uy |]
        getBytes [| true; true; true; true |]                           |> should equal [| 240uy |]
        getBytes [| true; true; true; true; true |]                     |> should equal [| 248uy |]
        getBytes [| true; true; true; true; true; true |]               |> should equal [| 252uy |]
        getBytes [| true; true; true; true; true; true; true |]         |> should equal [| 254uy |]
        getBytes [| true; true; true; true; true; true; true; true |]   |> should equal [| 255uy |]

        getBytes [| true; true; false; true; false; true; true; true |] |> should equal [| 215uy |]