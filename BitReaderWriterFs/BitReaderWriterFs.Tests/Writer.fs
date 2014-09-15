module Writer

open System
open System.Linq
open BitReaderWriterCs
open BitReaderWriterFs.BitWriterWorkflow
open FsUnit
open NUnit.Framework
open FsCheck

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
        BitWriter.WriteInt16(256s, ?endianness = Some Endianness.Big) |> should equal <| Writer(16, [| 1uy; 0uy |])

        BitWriter.WriteInt16(256s, 4) |> should equal <| Writer(4, [| 0uy |])
        BitWriter.WriteInt16(256s, 4, Endianness.Big) |> should equal <| Writer(4, [| 1uy |])

    [<Test>]
    member test.``test WriteUint16`` () =
        BitWriter.WriteUint16(256us) |> should equal <| Writer(16, [| 0uy; 1uy |])
        BitWriter.WriteUint16(256us, ?endianness = Some Endianness.Big) |> should equal <| Writer(16, [| 1uy; 0uy |])

        BitWriter.WriteUint16(256us, 4) |> should equal <| Writer(4, [| 0uy |])
        BitWriter.WriteUint16(256us, 4, Endianness.Big) |> should equal <| Writer(4, [| 1uy |])

    [<Test>]
    member test.``test WriteInt32`` () =
        BitWriter.WriteInt32(256) |> should equal <| Writer(32, [| 0uy; 1uy; 0uy; 0uy |])
        BitWriter.WriteInt32(256, ?endianness = Some Endianness.Big) |> should equal <| Writer(32, [| 0uy; 0uy; 1uy; 0uy |])

        BitWriter.WriteInt32(256, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])
        BitWriter.WriteInt32(256, 12, Endianness.Big) |> should equal <| Writer(12, [| 1uy; 0uy |])

    [<Test>]
    member test.``test WriteUint32`` () =
        BitWriter.WriteUint32(256u) |> should equal <| Writer(32, [| 0uy; 1uy; 0uy; 0uy |])
        BitWriter.WriteUint32(256u, ?endianness = Some Endianness.Big) |> should equal <| Writer(32, [| 0uy; 0uy; 1uy; 0uy |])

        BitWriter.WriteUint32(256u, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])
        BitWriter.WriteUint32(256u, 12, Endianness.Big) |> should equal <| Writer(12, [| 1uy; 0uy |])

    [<Test>]
    member test.``test WriteInt64`` () =
        BitWriter.WriteInt64(256L) |> should equal <| Writer(64, [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |])
        BitWriter.WriteInt64(256L, ?endianness = Some Endianness.Big) |> should equal <| Writer(64, [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy; 0uy |])

        BitWriter.WriteInt64(256L, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])
        BitWriter.WriteInt64(256L, 12, Endianness.Big) |> should equal <| Writer(12, [| 1uy; 0uy |])

    [<Test>]
    member test.``test WriteUint64`` () =
        BitWriter.WriteUint64(256UL) |> should equal <| Writer(64, [| 0uy; 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |])
        BitWriter.WriteUint64(256UL, ?endianness = Some Endianness.Big) |> should equal <| Writer(64, [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy; 0uy |])

        BitWriter.WriteUint64(256UL, 12) |> should equal <| Writer(12, [| 0uy; 1uy |])
        BitWriter.WriteUint64(256UL, 12, Endianness.Big) |> should equal <| Writer(12, [| 1uy; 0uy |])

    [<Test>]
    member test.``test WriteFloat`` () =
        BitWriter.WriteFloat(256.0f) |> should equal <| Writer(32, [| 0uy; 0uy; 128uy; 67uy |])

    [<Test>]
    member test.``test WriteDouble`` () =
        BitWriter.WriteDouble(256.0) |> should equal <| Writer(64, [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 112uy; 64uy |])