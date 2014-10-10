module Reader

open BitReaderWriterFs
open FsUnit
open NUnit.Framework
open System.IO

[<TestFixture>]
type ``BitReaderBuilder tests`` () =
    [<Test>]
    member test.``bitReader workflow should be able to read Int16 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {
            do! BitWriter.WriteInt16(256s)      // 256
            do! BitWriter.WriteInt16(256s, 9)   // 256
            do! BitWriter.WriteInt16(256s, 7)   // 0
            do! BitWriter.WriteInt16(-256s)     // -256
        }

        stream.Position <- 0L

        let x, y, z, neg = bitReader stream {
            let! x = BitReader.ReadInt16(16)
            let! y = BitReader.ReadInt16(9)
            let! z = BitReader.ReadInt16(7)
            let! neg = BitReader.ReadInt16(16)
            return x, y, z, neg
        }

        x   |> should equal 256s
        y   |> should equal 256s
        z   |> should equal 0s
        neg |> should equal -256s

    [<Test>]
    member test.``bitReader workflow should be able to read UInt16 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {
            do! BitWriter.WriteUint16(256us)      // 256
            do! BitWriter.WriteUint16(256us, 9)   // 256
            do! BitWriter.WriteUint16(256us, 7)   // 0
        }

        stream.Position <- 0L

        let x, y, z = bitReader stream {
            let! x = BitReader.ReadUint16(16)
            let! y = BitReader.ReadUint16(9)
            let! z = BitReader.ReadUint16(7)
            return x, y, z
        }

        x |> should equal 256us
        y |> should equal 256us
        z |> should equal 0us
